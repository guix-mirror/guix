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

(define-module (test-pki)
  #:use-module (guix pki)
  #:use-module (guix pk-crypto)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-64))

;; Test the (guix pki) module.

(define %public-key
  (call-with-input-file %public-key-file
    (compose string->canonical-sexp
             get-string-all)))

(test-begin "pki")

(test-assert "current-acl"
  (not (not (member (canonical-sexp->sexp %public-key)
                    (map canonical-sexp->sexp
                         (acl->public-keys (current-acl)))))))

(test-assert "authorized-key? public-key current-acl"
  (authorized-key? %public-key))

(test-assert "authorized-key? public-key empty-acl"
  (not (authorized-key? %public-key (public-keys->acl '()))))

(test-assert "authorized-key? public-key singleton"
  (authorized-key? %public-key (public-keys->acl (list %public-key))))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))
