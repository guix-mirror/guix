;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix store)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix monads)
  #:use-module ((gnu packages system)
                #:select (shadow))
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (user-account
            user-account?
            user-account-name
            user-account-pass
            user-account-uid
            user-account-gid
            user-account-comment
            user-account-home-directory
            user-account-shell

            user-group
            user-group?
            user-group-name
            user-group-password
            user-group-id
            user-group-members

            passwd-file
            group-file
            guix-build-accounts))

;;; Commentary:
;;;
;;; Utilities for configuring the Shadow tool suite ('login', 'passwd', etc.)
;;;
;;; Code:

(define-record-type* <user-account>
  user-account make-user-account
  user-account?
  (name           user-account-name)
  (password       user-account-pass (default ""))
  (uid            user-account-uid)
  (gid            user-account-gid)
  (comment        user-account-comment (default ""))
  (home-directory user-account-home-directory)
  (shell          user-account-shell (default "/bin/sh")))

(define-record-type* <user-group>
  user-group make-user-group
  user-group?
  (name           user-group-name)
  (password       user-group-password (default #f))
  (id             user-group-id)
  (members        user-group-members (default '())))

(define (group-file groups)
  "Return a /etc/group file for GROUPS, a list of <user-group> objects."
  (define contents
    (let loop ((groups groups)
               (result '()))
      (match groups
        ((($ <user-group> name _ gid (users ...)) rest ...)
         ;; XXX: Ignore the group password.
         (loop rest
               (cons (string-append name "::" (number->string gid)
                                    ":" (string-join users ","))
                     result)))
        (()
         (string-join (reverse result) "\n" 'suffix)))))

  (text-file "group" contents))

(define* (passwd-file accounts #:key shadow?)
  "Return a password file for ACCOUNTS, a list of <user-account> objects.  If
SHADOW? is true, then it is a /etc/shadow file, otherwise it is a /etc/passwd
file."
  ;; XXX: The resulting file is world-readable, so beware when SHADOW? is #t!
  (define contents
    (let loop ((accounts accounts)
               (result   '()))
      (match accounts
        ((($ <user-account> name pass uid gid comment home-dir shell)
          rest ...)
         (loop rest
               (cons (if shadow?
                         (string-append name
                                        ":"       ; XXX: use (crypt PASS …)?
                                        ":::::::")
                         (string-append name
                                        ":" "x"
                                        ":" (number->string uid)
                                        ":" (number->string gid)
                                        ":" comment ":" home-dir ":" shell))
                     result)))
        (()
         (string-join (reverse result) "\n" 'suffix)))))

  (text-file (if shadow? "shadow" "passwd") contents))

;;; shadow.scm ends here
