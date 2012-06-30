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


(define-module (test-packages)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (distro)
  #:use-module (distro base)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

;; Test the high-level packaging layer.

(define %store
  (false-if-exception (open-connection)))

(test-begin "packages")

(test-skip (if (not %store) 1 0))

(test-assert "GNU Hello"
  (and (package? hello)
       (or (location? (package-location hello))
           (not (package-location hello)))
       (let* ((drv (package-derivation %store hello))
              (out (derivation-path->output-path drv)))
         (and (build-derivations %store (list drv))
              (file-exists? (string-append out "/bin/hello"))))))

(test-assert "find-packages-by-name"
  (match (find-packages-by-name "hello")
    (((? (cut eq? hello <>))) #t)
    (wrong (pk 'find-packages-by-name wrong #f))))

(test-assert "find-packages-by-name with version"
  (match (find-packages-by-name "hello" (package-version hello))
    (((? (cut eq? hello <>))) #t)
    (wrong (pk 'find-packages-by-name wrong #f))))

(test-end "packages")


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; Local Variables:
;;; eval: (put 'test-assert 'scheme-indent-function 1)
;;; End:
