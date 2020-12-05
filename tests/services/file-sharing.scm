;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Simon South <simon@simonsouth.net>
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

(define-module (tests services file-sharing)
  #:use-module (gnu services file-sharing)
  #:use-module (srfi srfi-64))

;;; Tests for the (gnu services file-sharing) module.

(test-begin "file-sharing")


;;;
;;; Transmission Daemon.
;;;

(define %transmission-salt-length 8)

(define (valid-transmission-salt? salt)
    (and (string? salt)
         (eqv? (string-length salt) %transmission-salt-length)))

(test-assert "transmission-random-salt"
  (valid-transmission-salt? (transmission-random-salt)))

(test-equal "transmission-password-hash, typical values"
  "{ef6fba106cdef3aac64d1410090cae353cbecde53ceVVQO2"
  (transmission-password-hash "transmission" "3ceVVQO2"))

(test-equal "transmission-password-hash, empty password"
  "{820f816515d8969d058d07a1de018650619ee7ffCp.I5SWg"
  (transmission-password-hash "" "Cp.I5SWg"))

(test-error "transmission-password-hash, salt value too short"
            (transmission-password-hash
             "transmission"
             (make-string (- %transmission-salt-length 1) #\a)))

(test-error "transmission-password-hash, salt value too long"
            (transmission-password-hash
             "transmission"
             (make-string (+ %transmission-salt-length 1) #\a)))

(test-end "file-sharing")
