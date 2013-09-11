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
  #:use-module (ice-9 match)
  #:export (passwd-file))

;;; Commentary:
;;;
;;; Utilities for configuring the Shadow tool suite ('login', 'passwd', etc.)
;;;
;;; Code:

(define* (passwd-file store accounts #:key shadow?)
  "Return a password file for ACCOUNTS, a list of vectors as returned by
'getpwnam'.  If SHADOW? is true, then it is a /etc/shadow file, otherwise it
is a /etc/passwd file."
  ;; XXX: The resulting file is world-readable, so beware when SHADOW? is #t!
  (define contents
    (let loop ((accounts accounts)
               (result   '()))
      (match accounts
        ((#(name pass uid gid comment home-dir shell) rest ...)
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

  (add-text-to-store store (if shadow? "shadow" "passwd")
                     contents '()))

;;; shadow.scm ends here
