;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.


(define-module (test-derivations)
  #:use-module (guix derivations)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs io ports))

(test-begin "derivations")

(test-assert "parse & export"
  (let* ((b1 (call-with-input-file "test.drv" get-bytevector-all))
         (d1 (read-derivation (open-bytevector-input-port b1)))
         (b2 (call-with-bytevector-output-port (cut write-derivation d1 <>)))
         (d2 (read-derivation (open-bytevector-input-port b2))))
    (and (equal? b1 b2)
         (equal? d1 d2))))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; Local Variables:
;;; eval: (put 'test-assert 'scheme-indent-function 1)
;;; End:
