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

(define-module (gnu system shadow)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module ((gnu packages admin)
                #:select (shadow))
  #:use-module (gnu packages bash)
  #:use-module (gnu packages guile-wm)
  #:export (user-account
            user-account?
            user-account-name
            user-account-password
            user-account-uid
            user-account-group
            user-account-supplementary-groups
            user-account-comment
            user-account-home-directory
            user-account-shell

            user-group
            user-group?
            user-group-name
            user-group-password
            user-group-id

            default-skeletons
            skeleton-directory))

;;; Commentary:
;;;
;;; Utilities for configuring the Shadow tool suite ('login', 'passwd', etc.)
;;;
;;; Code:

(define-record-type* <user-account>
  user-account make-user-account
  user-account?
  (name           user-account-name)
  (password       user-account-password (default #f))
  (uid            user-account-uid (default #f))
  (group          user-account-group)             ; number | string
  (supplementary-groups user-account-supplementary-groups
                        (default '()))            ; list of strings
  (comment        user-account-comment (default ""))
  (home-directory user-account-home-directory)
  (shell          user-account-shell              ; gexp
                  (default #~(string-append #$bash "/bin/bash"))))

(define-record-type* <user-group>
  user-group make-user-group
  user-group?
  (name           user-group-name)
  (password       user-group-password (default #f))
  (id             user-group-id (default #f)))

(define (default-skeletons)
  "Return the default skeleton files for /etc/skel.  These files are copied by
'useradd' in the home directory of newly created user accounts."
  (define copy-guile-wm
    #~(begin
        (use-modules (guix build utils))
        (copy-file (car (find-files #$guile-wm "wm-init-sample.scm"))
                   #$output)))

  (mlet %store-monad ((bashrc (text-file "bashrc" "\
# Allow non-login shells such as an xterm to get things right.
test -f /etc/profile && source /etc/profile\n"))
                      (guile-wm (gexp->derivation "guile-wm" copy-guile-wm
                                                  #:modules
                                                  '((guix build utils))))
                      (xdefaults (text-file "Xdefaults" "\
XTerm*utf8: always
XTerm*metaSendsEscape: true\n"))
                      (gdbinit   (text-file "gdbinit" "\
# Tell GDB where to look for separate debugging files.
set debug-file-directory ~/.guix-profile/lib/debug\n")))
    (return `((".bashrc" ,bashrc)
              (".Xdefaults" ,xdefaults)
              (".guile-wm" ,guile-wm)
              (".gdbinit" ,gdbinit)))))

(define (skeleton-directory skeletons)
  "Return a directory containing SKELETONS, a list of name/derivation pairs."
  (gexp->derivation "skel"
                    #~(begin
                        (use-modules (ice-9 match))

                        (mkdir #$output)
                        (chdir #$output)

                        ;; Note: copy the skeletons instead of symlinking
                        ;; them like 'file-union' does, because 'useradd'
                        ;; would just copy the symlinks as is.
                        (for-each (match-lambda
                                   ((target source)
                                    (copy-file source target)))
                                  '#$skeletons)
                        #t)))

;;; shadow.scm ends here
