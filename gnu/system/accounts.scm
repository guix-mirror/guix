;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu system accounts)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (user-account
            user-account?
            user-account-name
            user-account-password
            user-account-uid
            user-account-group
            user-account-supplementary-groups
            user-account-comment
            user-account-home-directory
            user-account-create-home-directory?
            user-account-shell
            user-account-system?

            user-group
            user-group?
            user-group-name
            user-group-password
            user-group-id
            user-group-system?

            sexp->user-account
            sexp->user-group

            default-shell))


;;; Commentary:
;;;
;;; Data structures representing user accounts and user groups.  This is meant
;;; to be used both on the host side and at run time--e.g., in activation
;;; snippets.
;;;
;;; Code:

(define default-shell
  ;; Default shell for user accounts (a string or string-valued gexp).
  (make-parameter "/bin/sh"))

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
  (home-directory user-account-home-directory (thunked)
                  (default (default-home-directory this-record)))
  (create-home-directory? user-account-create-home-directory? ;Boolean
                          (default #t))
  (shell          user-account-shell              ; gexp
                  (default (default-shell)))
  (system?        user-account-system?            ; Boolean
                  (default #f)))

(define-record-type* <user-group>
  user-group make-user-group
  user-group?
  (name           user-group-name)
  (password       user-group-password (default #f))
  (id             user-group-id (default #f))
  (system?        user-group-system?              ; Boolean
                  (default #f)))

(define (default-home-directory account)
  "Return the default home directory for ACCOUNT."
  (string-append "/home/" (user-account-name account)))

(define (sexp->user-group sexp)
  "Take SEXP, a tuple as returned by 'user-group->gexp', and turn it into a
user-group record."
  (match sexp
    ((name password id system?)
     (user-group (name name)
                 (password password)
                 (id id)
                 (system? system?)))))

(define (sexp->user-account sexp)
  "Take SEXP, a tuple as returned by 'user-account->gexp', and turn it into a
user-account record."
  (match sexp
    ((name uid group supplementary-groups comment home-directory
           create-home-directory? shell password system?)
     (user-account (name name) (uid uid) (group group)
                   (supplementary-groups supplementary-groups)
                   (comment comment)
                   (home-directory home-directory)
                   (create-home-directory? create-home-directory?)
                   (shell shell) (password password)
                   (system? system?)))))
