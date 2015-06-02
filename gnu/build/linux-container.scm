;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu build linux-container)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-98)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix build syscalls)
  #:use-module ((gnu build file-systems) #:select (mount-file-system))
  #:export (%namespaces
            run-container
            call-with-container
            container-excursion))

(define %namespaces
  '(mnt pid ipc uts user net))

(define (call-with-clean-exit thunk)
  "Apply THUNK, but exit with a status code of 1 if it fails."
  (dynamic-wind
    (const #t)
    thunk
    (lambda ()
      (primitive-exit 1))))

(define (purify-environment)
  "Unset all environment variables."
  (for-each unsetenv
            (match (get-environment-variables)
              (((names . _) ...) names))))

;; The container setup procedure closely resembles that of the Docker
;; specification:
;; https://raw.githubusercontent.com/docker/libcontainer/master/SPEC.md
(define* (mount-file-systems root mounts #:key mount-/sys? mount-/proc?)
  "Mount the essential file systems and the those in the MOUNTS list relative
to ROOT, then make ROOT the new root directory for the process."
  (define (scope dir)
    (string-append root dir))

  (define (bind-mount src dest)
    (mount src dest "none" MS_BIND))

  ;; Like mount, but creates the mount point if it doesn't exist.
  (define* (mount* source target type #:optional (flags 0) options
                   #:key (update-mtab? #f))
    (mkdir-p target)
    (mount source target type flags options #:update-mtab? update-mtab?))

  ;; The container's file system is completely ephemeral, sans directories
  ;; bind-mounted from the host.
  (mount "none" root "tmpfs")

  ;; A proc mount requires a new pid namespace.
  (when mount-/proc?
    (mount* "none" (scope "/proc") "proc"
            (logior MS_NOEXEC MS_NOSUID MS_NODEV)))

  ;; A sysfs mount requires the user to have the CAP_SYS_ADMIN capability in
  ;; the current network namespace.
  (when mount-/sys?
    (mount* "none" (scope "/sys") "sysfs"
            (logior MS_NOEXEC MS_NOSUID MS_NODEV MS_RDONLY)))

  (mount* "none" (scope "/dev") "tmpfs"
          (logior MS_NOEXEC MS_STRICTATIME)
          "mode=755")

  ;; Create essential device nodes via bind-mounting them from the
  ;; host, because a process within a user namespace cannot create
  ;; device nodes.
  (for-each (lambda (device)
              (when (file-exists? device)
                ;; Create the mount point file.
                (call-with-output-file (scope device)
                  (const #t))
                (bind-mount device (scope device))))
            '("/dev/null"
              "/dev/zero"
              "/dev/full"
              "/dev/random"
              "/dev/urandom"
              "/dev/tty"
              "/dev/ptmx"
              "/dev/fuse"))

  ;; Setup standard input/output/error.
  (symlink "/proc/self/fd"   (scope "/dev/fd"))
  (symlink "/proc/self/fd/0" (scope "/dev/stdin"))
  (symlink "/proc/self/fd/1" (scope "/dev/stdout"))
  (symlink "/proc/self/fd/2" (scope "/dev/stderr"))

  ;; Mount user-specified file systems.
  (for-each (lambda (spec)
              (mount-file-system spec #:root root))
            mounts)

  ;; Jail the process inside the container's root file system.
  (let ((put-old (string-append root "/real-root")))
    (mkdir put-old)
    (pivot-root root put-old)
    (chdir "/")
    (umount "real-root" MNT_DETACH)
    (rmdir "real-root")))

(define (initialize-user-namespace pid)
  "Configure the user namespace for PID."
  (define proc-dir
    (string-append "/proc/" (number->string pid)))

  (define (scope file)
    (string-append proc-dir file))

  ;; Only root can map more than a single uid/gid.  A range of 65536 uid/gids
  ;; is used to cover 16 bits worth of users and groups, which is sufficient
  ;; for most cases.
  ;;
  ;; See also: http://www.freedesktop.org/software/systemd/man/systemd-nspawn.html#--private-users=
  (let* ((uid       (getuid))
         (gid       (getgid))
         (uid-range (if (zero? uid) 65536 1))
         (gid-range (if (zero? gid) 65536 1)))

    ;; Only root can write to the gid map without first disabling the
    ;; setgroups syscall.
    (unless (and (zero? uid) (zero? gid))
      (call-with-output-file (scope "/setgroups")
        (lambda (port)
          (display "deny" port))))

    ;; Map the user/group that created the container to the root user
    ;; within the container.
    (call-with-output-file (scope "/uid_map")
      (lambda (port)
        (format port "0 ~d ~d" uid uid-range)))
    (call-with-output-file (scope "/gid_map")
      (lambda (port)
        (format port "0 ~d ~d" gid gid-range)))))

(define (namespaces->bit-mask namespaces)
  "Return the number suitable for the 'flags' argument of 'clone' that
corresponds to the symbols in NAMESPACES."
  (apply logior SIGCHLD
         (map (match-lambda
               ('mnt  CLONE_NEWNS)
               ('uts  CLONE_NEWUTS)
               ('ipc  CLONE_NEWIPC)
               ('user CLONE_NEWUSER)
               ('pid  CLONE_NEWPID)
               ('net  CLONE_NEWNET))
              namespaces)))

(define (run-container root mounts namespaces thunk)
  "Run THUNK in a new container process and return its PID.  ROOT specifies
the root directory for the container.  MOUNTS is a list of file system specs
that specify the mapping of host file systems into the container.  NAMESPACES
is a list of symbols that correspond to the possible Linux namespaces: mnt,
ipc, uts, user, and net."
  ;; The parent process must initialize the user namespace for the child
  ;; before it can boot.  To negotiate this, a pipe is used such that the
  ;; child process blocks until the parent writes to it.
  (match (pipe)
    ((in . out)
     (let ((flags (namespaces->bit-mask namespaces)))
       (match (clone flags)
         (0
          (call-with-clean-exit
           (lambda ()
             (close out)
             ;; Wait for parent to set things up.
             (read in)
             (close in)
             (purify-environment)
             (when (memq 'mnt namespaces)
               (mount-file-systems root mounts
                                   #:mount-/proc? (memq 'pid namespaces)
                                   #:mount-/sys?  (memq 'net namespaces)))
             ;; TODO: Manage capabilities.
             (thunk))))
         (pid
          (when (memq 'user namespaces)
            (initialize-user-namespace pid))
          ;; TODO: Initialize cgroups.
          (close in)
          (write 'ready out)
          (close out)
          pid))))))

(define* (call-with-container mounts thunk #:key (namespaces %namespaces))
  "Run THUNK in a new container process and return its exit status.
MOUNTS is a list of file system specs that specify the mapping of host file
systems into the container.  NAMESPACES is a list of symbols corresponding to
the identifiers for Linux namespaces: mnt, ipc, uts, pid, user, and net.  By
default, all namespaces are used.

Note that if THUNK needs to load any additional Guile modules, the relevant
module files must be present in one of the mappings in MOUNTS and the Guile
load path must be adjusted as needed."
  (call-with-temporary-directory
   (lambda (root)
     (let ((pid (run-container root mounts namespaces thunk)))
       ;; Catch SIGINT and kill the container process.
       (sigaction SIGINT
         (lambda (signum)
           (false-if-exception
            (kill pid SIGKILL))))

       (match (waitpid pid)
         ((_ . status) status))))))

(define (container-excursion pid thunk)
  "Run THUNK as a child process within the namespaces of process PID and
return the exit status."
  (define (namespace-file pid namespace)
    (string-append "/proc/" (number->string pid) "/ns/" namespace))

  (match (primitive-fork)
    (0
     (call-with-clean-exit
      (lambda ()
        (for-each (lambda (ns)
                    (call-with-input-file (namespace-file (getpid) ns)
                      (lambda (current-ns-port)
                        (call-with-input-file (namespace-file pid ns)
                          (lambda (new-ns-port)
                            ;; Joining the namespace that the process
                            ;; already belongs to would throw an error.
                            (unless (= (port->fdes current-ns-port)
                                       (port->fdes new-ns-port))
                              (setns (port->fdes new-ns-port) 0)))))))
                  ;; It's important that the user namespace is joined first,
                  ;; so that the user will have the privileges to join the
                  ;; other namespaces.  Furthermore, it's important that the
                  ;; mount namespace is joined last, otherwise the /proc mount
                  ;; point would no longer be accessible.
                  '("user" "ipc" "uts" "net" "pid" "mnt"))
        (purify-environment)
        (chdir "/")
        (thunk))))
    (pid
     (match (waitpid pid)
       ((_ . status)
        (status:exit-val status))))))
