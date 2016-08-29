;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu build install)
  #:use-module (guix build utils)
  #:use-module (guix build store-copy)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (install-grub
            populate-root-file-system
            reset-timestamps
            register-closure
            populate-single-profile-directory))

;;; Commentary:
;;;
;;; This module supports the installation of the GNU system on a hard disk.
;;; It is meant to be used both in a build environment (in derivations that
;;; build VM images), and on the bare metal (when really installing the
;;; system.)
;;;
;;; Code:

(define* (install-grub grub.cfg device mount-point)
  "Install GRUB with GRUB.CFG on DEVICE, which is assumed to be mounted on
MOUNT-POINT.

Note that the caller must make sure that GRUB.CFG is registered as a GC root
so that the fonts, background images, etc. referred to by GRUB.CFG are not
GC'd."
  (let* ((target (string-append mount-point "/boot/grub/grub.cfg"))
         (pivot  (string-append target ".new")))
    (mkdir-p (dirname target))

    ;; Copy GRUB.CFG instead of just symlinking it, because symlinks won't
    ;; work when /boot is on a separate partition.  Do that atomically.
    (copy-file grub.cfg pivot)
    (rename-file pivot target)

    (unless (zero? (system* "grub-install" "--no-floppy"
                            "--boot-directory"
                            (string-append mount-point "/boot")
                            device))
      (error "failed to install GRUB"))))

(define (evaluate-populate-directive directive target)
  "Evaluate DIRECTIVE, an sexp describing a file or directory to create under
directory TARGET."
  (let loop ((directive directive))
    (catch 'system-error
      (lambda ()
        (match directive
          (('directory name)
           (mkdir-p (string-append target name)))
          (('directory name uid gid)
           (let ((dir (string-append target name)))
             (mkdir-p dir)
             (chown dir uid gid)))
          (('directory name uid gid mode)
           (loop `(directory ,name ,uid ,gid))
           (chmod (string-append target name) mode))
          ((new '-> old)
           (let try ()
             (catch 'system-error
               (lambda ()
                 (symlink old (string-append target new)))
               (lambda args
                 ;; When doing 'guix system init' on the current '/', some
                 ;; symlinks may already exists.  Override them.
                 (if (= EEXIST (system-error-errno args))
                     (begin
                       (delete-file (string-append target new))
                       (try))
                     (apply throw args))))))))
      (lambda args
        ;; Usually we can only get here when installing to an existing root,
        ;; as with 'guix system init foo.scm /'.
        (format (current-error-port)
                "error: failed to evaluate directive: ~s~%"
                directive)
        (apply throw args)))))

(define (directives store)
  "Return a list of directives to populate the root file system that will host
STORE."
  `(;; Note: the store's GID is fixed precisely so we can set it here rather
    ;; than at activation time.
    (directory ,store 0 30000 #o1775)

    (directory "/etc")
    (directory "/var/log")                          ; for shepherd
    (directory "/var/guix/gcroots")
    (directory "/var/empty")                        ; for no-login accounts
    (directory "/var/db")                           ; for dhclient, etc.
    (directory "/var/run")
    (directory "/run")
    (directory "/mnt")
    (directory "/var/guix/profiles/per-user/root" 0 0)

    ;; Link to the initial system generation.
    ("/var/guix/profiles/system" -> "system-1-link")

    ("/var/guix/gcroots/booted-system" -> "/run/booted-system")
    ("/var/guix/gcroots/current-system" -> "/run/current-system")

    ;; XXX: 'guix-register' creates this symlink with a wrong target, so
    ;; create it upfront to be sure.
    ("/var/guix/gcroots/profiles" -> "/var/guix/profiles")

    (directory "/bin")
    (directory "/tmp" 0 0 #o1777)                 ; sticky bit
    (directory "/var/tmp" 0 0 #o1777)
    (directory "/var/lock" 0 0 #o1777)

    (directory "/root" 0 0)                       ; an exception
    (directory "/home" 0 0)))

(define (populate-root-file-system system target)
  "Make the essential non-store files and directories on TARGET.  This
includes /etc, /var, /run, /bin/sh, etc., and all the symlinks to SYSTEM."
  (for-each (cut evaluate-populate-directive <> target)
            (directives (%store-directory)))

  ;; Add system generation 1.
  (let ((generation-1 (string-append target
                                     "/var/guix/profiles/system-1-link")))
    (let try ()
      (catch 'system-error
        (lambda ()
          (symlink system generation-1))
        (lambda args
          ;; If GENERATION-1 already exists, overwrite it.
          (if (= EEXIST (system-error-errno args))
              (begin
                (delete-file generation-1)
                (try))
              (apply throw args)))))))

(define (reset-timestamps directory)
  "Reset the timestamps of all the files under DIRECTORY, so that they appear
as created and modified at the Epoch."
  (display "clearing file timestamps...\n")
  (for-each (lambda (file)
              (let ((s (lstat file)))
                ;; XXX: Guile uses libc's 'utime' function (not 'futime'), so
                ;; the timestamp of symlinks cannot be changed, and there are
                ;; symlinks here pointing to /gnu/store, which is the host,
                ;; read-only store.
                (unless (eq? (stat:type s) 'symlink)
                  (utime file 0 0 0 0))))
            (find-files directory #:directories? #t)))

(define* (register-closure store closure
                           #:key (deduplicate? #t))
  "Register CLOSURE in STORE, where STORE is the directory name of the target
store and CLOSURE is the name of a file containing a reference graph as used
by 'guix-register'.  As a side effect, this resets timestamps on store files
and, if DEDUPLICATE? is true, deduplicates files common to CLOSURE and the
rest of STORE."
  (let ((status (apply system* "guix-register" "--prefix" store
                       (append (if deduplicate? '() '("--no-deduplication"))
                               (list closure)))))
    (unless (zero? status)
      (error "failed to register store items" closure))))

(define* (populate-single-profile-directory directory
                                            #:key profile closure
                                            deduplicate?)
  "Populate DIRECTORY with a store containing PROFILE, whose closure is given
in the file called CLOSURE (as generated by #:references-graphs.)  DIRECTORY
is initialized to contain a single profile under /root pointing to PROFILE.
DEDUPLICATE? determines whether to deduplicate files in the store.

This is used to create the self-contained Guix tarball."
  (define (scope file)
    (string-append directory "/" file))

  (define %root-profile
    "/var/guix/profiles/per-user/root")

  (define (mkdir-p* dir)
    (mkdir-p (scope dir)))

  (define (symlink* old new)
    (symlink old (scope new)))

  ;; Populate the store.
  (populate-store (list closure) directory)
  (register-closure (canonicalize-path directory) closure
                    #:deduplicate? deduplicate?)

  ;; XXX: 'guix-register' registers profiles as GC roots but the symlink
  ;; target uses $TMPDIR.  Fix that.
  (delete-file (scope "/var/guix/gcroots/profiles"))
  (symlink* "/var/guix/profiles"
            "/var/guix/gcroots/profiles")

  ;; Make root's profile, which makes it a GC root.
  (mkdir-p* %root-profile)
  (symlink* profile
            (string-append %root-profile "/guix-profile-1-link"))
  (symlink* (string-append %root-profile "/guix-profile-1-link")
            (string-append %root-profile "/guix-profile"))

  (mkdir-p* "/root")
  (symlink* (string-append %root-profile "/guix-profile")
            "/root/.guix-profile"))

;;; install.scm ends here
