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

(define-module (guix tests)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (srfi srfi-34)
  #:use-module (rnrs bytevectors)
  #:export (open-connection-for-tests
            random-text
            random-bytevector))

;;; Commentary:
;;;
;;; This module provide shared infrastructure for the test suite.  For
;;; internal use only.
;;;
;;; Code:

(define (open-connection-for-tests)
  "Open a connection to the build daemon for tests purposes and return it."
  (guard (c ((nix-error? c)
             (format (current-error-port)
                     "warning: build daemon error: ~s~%" c)
             #f))
    (let ((store (open-connection)))
      ;; Make sure we build everything by ourselves.
      (set-build-options store #:use-substitutes? #f)

      ;; Use the bootstrap Guile when running tests, so we don't end up
      ;; building everything in the temporary test store.
      (%guile-for-build (package-derivation store %bootstrap-guile))

      store)))

(define %seed
  (seed->random-state (logxor (getpid) (car (gettimeofday)))))

(define (random-text)
  "Return the hexadecimal representation of a random number."
  (number->string (random (expt 2 256) %seed) 16))

(define (random-bytevector n)
  "Return a random bytevector of N bytes."
  (let ((bv (make-bytevector n)))
    (let loop ((i 0))
      (if (< i n)
          (begin
            (bytevector-u8-set! bv i (random 256 %seed))
            (loop (1+ i)))
          bv))))

;;; tests.scm ends here
