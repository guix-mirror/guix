;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build activation)
  #:use-module (guix build utils)
  #:use-module (guix build linux-initrd)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (activate-users+groups
            activate-etc
            activate-setuid-programs
            activate-current-system))

;;; Commentary:
;;;
;;; This module provides "activation" helpers.  Activation is the process that
;;; consists in setting up system-wide files and directories so that an
;;; 'operating-system' configuration becomes active.
;;;
;;; Code:

(define* (add-group name #:key gid password
                    (log-port (current-error-port)))
  "Add NAME as a user group, with the given numeric GID if specified."
  ;; Use 'groupadd' from the Shadow package.
  (format log-port "adding group '~a'...~%" name)
  (let ((args `(,@(if gid `("-g" ,(number->string gid)) '())
                ,@(if password `("-p" ,password) '())
                ,name)))
    (zero? (apply system* "groupadd" args))))

(define* (add-user name group
                   #:key uid comment home shell password
                   (supplementary-groups '())
                   (log-port (current-error-port)))
  "Create an account for user NAME part of GROUP, with the specified
properties.  Return #t on success."
  (format log-port "adding user '~a'...~%" name)

  (if (and uid (zero? uid))

      ;; 'useradd' fails with "Cannot determine your user name" if the root
      ;; account doesn't exist.  Thus, for bootstrapping purposes, create that
      ;; one manually.
      (begin
        (call-with-output-file "/etc/shadow"
          (cut format <> "~a::::::::~%" name))
        (call-with-output-file "/etc/passwd"
          (cut format <> "~a:x:~a:~a:~a:~a:~a~%"
               name "0" "0" comment home shell))
        (chmod "/etc/shadow" #o600)
        #t)

      ;; Use 'useradd' from the Shadow package.
      (let ((args `(,@(if uid `("-u" ,(number->string uid)) '())
                    "-g" ,(if (number? group) (number->string group) group)
                    ,@(if (pair? supplementary-groups)
                          `("-G" ,(string-join supplementary-groups ","))
                          '())
                    ,@(if comment `("-c" ,comment) '())
                    ,@(if home
                          (if (file-exists? home)
                              `("-d" ,home)     ; avoid warning from 'useradd'
                              `("-d" ,home "--create-home"))
                          '())
                    ,@(if shell `("-s" ,shell) '())
                    ,@(if password `("-p" ,password) '())
                    ,name)))
        (zero? (apply system* "useradd" args)))))

(define (activate-users+groups users groups)
  "Make sure the accounts listed in USERS and the user groups listed in GROUPS
are all available.

Each item in USERS is a list of all the characteristics of a user account;
each item in GROUPS is a tuple with the group name, group password or #f, and
numeric gid or #f."
  (define (touch file)
    (call-with-output-file file (const #t)))

  (define activate-user
    (match-lambda
     ((name uid group supplementary-groups comment home shell password)
      (unless (false-if-exception (getpwnam name))
        (let ((profile-dir (string-append "/var/guix/profiles/per-user/"
                                          name)))
          (add-user name group
                    #:uid uid
                    #:supplementary-groups supplementary-groups
                    #:comment comment
                    #:home home
                    #:shell shell
                    #:password password)

          ;; Create the profile directory for the new account.
          (let ((pw (getpwnam name)))
            (mkdir-p profile-dir)
            (chown profile-dir (passwd:uid pw) (passwd:gid pw))))))))

  ;; 'groupadd' aborts if the file doesn't already exist.
  (touch "/etc/group")

  ;; Create the root account so we can use 'useradd' and 'groupadd'.
  (activate-user (find (match-lambda
                        ((name (? zero?) _ ...) #t)
                        (_ #f))
                       users))

  ;; Then create the groups.
  (for-each (match-lambda
             ((name password gid)
              (add-group name #:gid gid #:password password)))
            groups)

  ;; Finally create the other user accounts.
  (for-each activate-user users))

(define (activate-etc etc)
  "Install ETC, a directory in the store, as the source of static files for
/etc."

  ;; /etc is a mixture of static and dynamic settings.  Here is where we
  ;; initialize it from the static part.

  (format #t "populating /etc from ~a...~%" etc)
  (let ((rm-f (lambda (f)
                (false-if-exception (delete-file f)))))
    (rm-f "/etc/static")
    (symlink etc "/etc/static")
    (for-each (lambda (file)
                ;; TODO: Handle 'shadow' specially so that changed
                ;; password aren't lost.
                (let ((target (string-append "/etc/" file))
                      (source (string-append "/etc/static/" file)))
                  (rm-f target)
                  (symlink source target)))
              (scandir etc
                       (lambda (file)
                         (not (member file '("." ".."))))

                       ;; The default is 'string-locale<?', but we don't have
                       ;; it when run from the initrd's statically-linked
                       ;; Guile.
                       string<?))

    ;; Prevent ETC from being GC'd.
    (rm-f "/var/guix/gcroots/etc-directory")
    (symlink etc "/var/guix/gcroots/etc-directory")))

(define %setuid-directory
  ;; Place where setuid programs are stored.
  "/run/setuid-programs")

(define (activate-setuid-programs programs)
  "Turn PROGRAMS, a list of file names, into setuid programs stored under
%SETUID-DIRECTORY."
  (define (make-setuid-program prog)
    (let ((target (string-append %setuid-directory
                                 "/" (basename prog))))
      (catch 'system-error
        (lambda ()
          (link prog target))
        (lambda args
          ;; Perhaps PROG and TARGET live in a different file system, so copy
          ;; PROG.
          (copy-file prog target)))
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

(define %current-system
  ;; The system that is current (a symlink.)  This is not necessarily the same
  ;; as the system we booted (aka. /run/booted-system) because we can re-build
  ;; a new system configuration and activate it, without rebooting.
  "/run/current-system")

(define (boot-time-system)
  "Return the '--system' argument passed on the kernel command line."
  (find-long-option "--system" (linux-command-line)))

(define* (activate-current-system #:optional (system (boot-time-system)))
  "Atomically make SYSTEM the current system."
  (format #t "making '~a' the current system...~%" system)

  ;; Atomically make SYSTEM current.
  (let ((new (string-append %current-system ".new")))
    (symlink system new)
    (rename-file new %current-system)))

;;; activation.scm ends here
