;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu build activation)
  #:use-module (gnu system accounts)
  #:use-module (gnu build accounts)
  #:use-module (gnu build linux-boot)
  #:use-module (guix build utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (activate-users+groups
            activate-user-home
            activate-etc
            activate-setuid-programs
            activate-special-files
            activate-modprobe
            activate-firmware
            activate-ptrace-attach
            activate-current-system))

;;; Commentary:
;;;
;;; This module provides "activation" helpers.  Activation is the process that
;;; consists in setting up system-wide files and directories so that an
;;; 'operating-system' configuration becomes active.
;;;
;;; Code:

(define %skeleton-directory
  ;; Directory containing skeleton files for new accounts.
  ;; Note: keep the trailing '/' so that 'scandir' enters it.
  "/etc/skel/")

(define (dot-or-dot-dot? file)
  (member file '("." "..")))

(define* (copy-account-skeletons home
                                 #:key
                                 (directory %skeleton-directory)
                                 uid gid)
  "Copy the account skeletons from DIRECTORY to HOME.  When UID is an integer,
make it the owner of all the files created; likewise for GID."
  (define (set-owner file)
    (when (or uid gid)
      (chown file (or uid -1) (or gid -1))))

  (let ((files (scandir directory (negate dot-or-dot-dot?)
                        string<?)))
    (mkdir-p home)
    (set-owner home)
    (for-each (lambda (file)
                (let ((target (string-append home "/" file)))
                  (copy-recursively (string-append directory "/" file)
                                    target
                                    #:log (%make-void-port "w"))
                  (for-each set-owner
                            (find-files target (const #t)
                                        #:directories? #t))
                  (make-file-writable target)))
              files)))

(define* (make-skeletons-writable home
                                  #:optional (directory %skeleton-directory))
  "Make sure that the files that have been copied from DIRECTORY to HOME are
owner-writable in HOME."
  (let ((files (scandir directory (negate dot-or-dot-dot?)
                        string<?)))
    (for-each (lambda (file)
                (let ((target (string-append home "/" file)))
                  (when (file-exists? target)
                    (make-file-writable target))))
              files)))

(define (duplicates lst)
  "Return elements from LST present more than once in LST."
  (let loop ((lst lst)
             (seen vlist-null)
             (result '()))
    (match lst
      (()
       (reverse result))
      ((head . tail)
       (loop tail
             (vhash-cons head #t seen)
             (if (vhash-assoc head seen)
                 (cons head result)
                 result))))))

(define (activate-users+groups users groups)
  "Make sure USERS (a list of user account records) and GROUPS (a list of user
group records) are all available."
  (define (make-home-directory user)
    (let ((home (user-account-home-directory user))
          (pwd  (getpwnam (user-account-name user))))
      (mkdir-p home)

      ;; Always set ownership and permissions for home directories of system
      ;; accounts.  If a service needs looser permissions on its home
      ;; directories, it can always chmod it in an activation snippet.
      (chown home (passwd:uid pwd) (passwd:gid pwd))
      (chmod home #o700)))

  (define system-accounts
    (filter (lambda (user)
              (and (user-account-system? user)
                   (user-account-create-home-directory? user)))
            users))

  ;; Allow home directories to be created under /var/lib.
  (mkdir-p "/var/lib")

  (let-values (((groups passwd shadow)
                (user+group-databases users groups)))
    (write-group groups)
    (write-passwd passwd)
    (write-shadow shadow)

    ;; Home directories of non-system accounts are created by
    ;; 'activate-user-home'.
    (for-each make-home-directory system-accounts)

    ;; Turn shared home directories, such as /var/empty, into root-owned,
    ;; read-only places.
    (for-each (lambda (directory)
                (chown directory 0 0)
                (chmod directory #o555))
              (duplicates (map user-account-home-directory system-accounts)))))

(define (activate-user-home users)
  "Create and populate the home directory of USERS, a list of tuples, unless
they already exist."
  (define ensure-user-home
    (lambda (user)
      (let ((name         (user-account-name user))
            (home         (user-account-home-directory user))
            (create-home? (user-account-create-home-directory? user))
            (system?      (user-account-system? user)))
        ;; The home directories of system accounts are created during
        ;; activation, not here.
        (unless (or (not home) (not create-home?) system?
                    (directory-exists? home))
          (let* ((pw  (getpwnam name))
                 (uid (passwd:uid pw))
                 (gid (passwd:gid pw)))
            (mkdir-p home)
            (chown home uid gid)
            (chmod home #o700)
            (copy-account-skeletons home
                                    #:uid uid #:gid gid))))))

  (for-each ensure-user-home users))

(define (activate-etc etc)
  "Install ETC, a directory in the store, as the source of static files for
/etc."

  ;; /etc is a mixture of static and dynamic settings.  Here is where we
  ;; initialize it from the static part.

  (define (rm-f file)
    (false-if-exception (delete-file file)))

  (format #t "populating /etc from ~a...~%" etc)
  (mkdir-p "/etc")

  ;; Create the /etc/ssl -> /run/current-system/profile/etc/ssl symlink.  This
  ;; symlink, to a target outside of the store, probably doesn't belong in the
  ;; static 'etc' store directory.  However, if it were to be put there,
  ;; beware that if /run/current-system/profile/etc/ssl doesn't exist at the
  ;; time of activation (e.g. when installing a fresh system), the call to
  ;; 'file-is-directory?' below will fail because it uses 'stat', not 'lstat'.
  (rm-f "/etc/ssl")
  (symlink "/run/current-system/profile/etc/ssl" "/etc/ssl")

  (rm-f "/etc/static")
  (symlink etc "/etc/static")
  (for-each (lambda (file)
              (let ((target (string-append "/etc/" file))
                    (source (string-append "/etc/static/" file)))
                (rm-f target)

                ;; Things such as /etc/sudoers must be regular files, not
                ;; symlinks; furthermore, they could be modified behind our
                ;; back---e.g., with 'visudo'.  Thus, make a copy instead of
                ;; symlinking them.
                (if (file-is-directory? source)
                    (symlink source target)
                    (copy-file source target))

                ;; XXX: Dirty hack to meet sudo's expectations.
                (when (string=? (basename target) "sudoers")
                  (chmod target #o440))))
            (scandir etc (negate dot-or-dot-dot?)

                     ;; The default is 'string-locale<?', but we don't have
                     ;; it when run from the initrd's statically-linked
                     ;; Guile.
                     string<?)))

(define %setuid-directory
  ;; Place where setuid programs are stored.
  "/run/setuid-programs")

(define (activate-setuid-programs programs)
  "Turn PROGRAMS, a list of file names, into setuid programs stored under
%SETUID-DIRECTORY."
  (define (make-setuid-program prog)
    (let ((target (string-append %setuid-directory
                                 "/" (basename prog))))
      (copy-file prog target)
      (chown target 0 0)
      (chmod target #o6555)))

  (format #t "setting up setuid programs in '~a'...~%"
          %setuid-directory)
  (if (file-exists? %setuid-directory)
      (for-each (compose delete-file
                         (cut string-append %setuid-directory "/" <>))
                (scandir %setuid-directory
                         (lambda (file)
                           (not (member file '("." ".."))))
                         string<?))
      (mkdir-p %setuid-directory))

  (for-each make-setuid-program programs))

(define (activate-special-files special-files)
  "Install the files listed in SPECIAL-FILES.  Each element of SPECIAL-FILES
is a pair where the first element is the name of the special file and the
second element is the name it should appear at, such as:

  ((\"/bin/sh\" \"/gnu/store/…-bash/bin/sh\")
   (\"/usr/bin/env\" \"/gnu/store/…-coreutils/bin/env\"))
"
  (define install-special-file
    (match-lambda
      ((target file)
       (let ((pivot (string-append target ".new")))
         (mkdir-p (dirname target))
         (symlink file pivot)
         (rename-file pivot target)))))

  (for-each install-special-file special-files))

(define (activate-modprobe modprobe)
  "Tell the kernel to use MODPROBE to load modules."
  (call-with-output-file "/proc/sys/kernel/modprobe"
    (lambda (port)
      (display modprobe port))))

(define (activate-firmware directory)
  "Tell the kernel to look for device firmware under DIRECTORY.  This
mechanism bypasses udev: it allows Linux to handle firmware loading directly
by itself, without having to resort to a \"user helper\"."
  (call-with-output-file "/sys/module/firmware_class/parameters/path"
    (lambda (port)
      (display directory port))))

(define (activate-ptrace-attach)
  "Allow users to PTRACE_ATTACH their own processes.

This works around a regression introduced in the default \"security\" policy
found in Linux 3.4 onward that prevents users from attaching to their own
processes--see Yama.txt in the Linux source tree for the rationale.  This
sounds like an unacceptable restriction for little or no security
improvement."
  (let ((file "/proc/sys/kernel/yama/ptrace_scope"))
    (when (file-exists? file)
      (call-with-output-file file
        (lambda (port)
          (display 0 port))))))


(define %current-system
  ;; The system that is current (a symlink.)  This is not necessarily the same
  ;; as the system we booted (aka. /run/booted-system) because we can re-build
  ;; a new system configuration and activate it, without rebooting.
  "/run/current-system")

(define (boot-time-system)
  "Return the '--system' argument passed on the kernel command line."
  (find-long-option "--system" (linux-command-line)))

(define* (activate-current-system
          #:optional (system (or (getenv "GUIX_NEW_SYSTEM")
                                 (boot-time-system))))
  "Atomically make SYSTEM the current system."
  ;; The 'GUIX_NEW_SYSTEM' environment variable is used as a way for 'guix
  ;; system reconfigure' to pass the file name of the new system.

  (format #t "making '~a' the current system...~%" system)

  ;; Atomically make SYSTEM current.
  (let ((new (string-append %current-system ".new")))
    (symlink system new)
    (rename-file new %current-system)))

;;; activation.scm ends here
