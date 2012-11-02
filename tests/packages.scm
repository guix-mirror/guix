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
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system gnu)
  #:use-module (distro)
  #:use-module (distro packages base)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

;; Test the high-level packaging layer.

(define %store
  (false-if-exception (open-connection)))

(when %store
  ;; Make sure we build everything by ourselves.
  (set-build-options %store #:use-substitutes? #f))

(define %bootstrap-inputs
  ;; Use the bootstrap inputs so it doesn't take ages to run these tests.
  ;; This still involves building GNU Make and GNU Diffutils.
  (@@ (distro packages base) %boot0-inputs))

(define %bootstrap-guile
  (@@ (distro packages base) %bootstrap-guile))


(test-begin "packages")

(define-syntax-rule (dummy-package name* extra-fields ...)
  (package (name name*) (version "0") (source #f)
           (build-system gnu-build-system)
           (description #f) (long-description #f)
           (home-page #f)
           extra-fields ...))

(test-assert "package-transitive-inputs"
  (let* ((a (dummy-package "a"))
         (b (dummy-package "b"
              (propagated-inputs `(("a" ,a)))))
         (c (dummy-package "c"
              (inputs `(("a" ,a)))))
         (d (dummy-package "d"
              (propagated-inputs `(("x" "something.drv")))))
         (e (dummy-package "e"
              (inputs `(("b" ,b) ("c" ,c) ("d" ,d))))))
    (and (null? (package-transitive-inputs a))
         (equal? `(("a" ,a)) (package-transitive-inputs b))
         (equal? `(("a" ,a)) (package-transitive-inputs c))
         (equal? (package-propagated-inputs d)
                 (package-transitive-inputs d))
         (equal? `(("b" ,b) ("b/a" ,a) ("c" ,c)
                   ("d" ,d) ("d/x" "something.drv"))
                 (pk 'x (package-transitive-inputs e))))))

(test-skip (if (not %store) 2 0))

(test-assert "trivial"
  (let* ((p (package (inherit (dummy-package "trivial"))
              (build-system trivial-build-system)
              (source #f)
              (arguments
               `(#:guile ,%bootstrap-guile
                 #:builder
                 (begin
                   (mkdir %output)
                   (call-with-output-file (string-append %output "/test")
                     (lambda (p)
                       (display '(hello guix) p))))))))
         (d (package-derivation %store p)))
    (and (build-derivations %store (list d))
         (let ((p (pk 'drv d (derivation-path->output-path d))))
           (equal? '(hello guix)
                   (call-with-input-file (string-append p "/test") read))))))

(test-assert "trivial with system-dependent input"
  (let* ((p (package (inherit (dummy-package "trivial-system-dependent-input"))
              (build-system trivial-build-system)
              (source #f)
              (arguments
               `(#:guile ,%bootstrap-guile
                 #:builder
                 (let ((out  (assoc-ref %outputs "out"))
                       (bash (assoc-ref %build-inputs "bash")))
                   (zero? (system* bash "-c"
                                   (format #f "echo hello > ~a" out))))))
              (inputs `(("bash" ,(lambda (system)
                                   (search-bootstrap-binary "bash"
                                                            system)))))))
         (d (package-derivation %store p)))
    (and (build-derivations %store (list d))
         (let ((p (pk 'drv d (derivation-path->output-path d))))
           (eq? 'hello (call-with-input-file p read))))))

(test-assert "GNU Hello"
  (let ((hello (package-with-explicit-inputs hello %bootstrap-inputs
                                             #:guile %bootstrap-guile)))
    (and (package? hello)
         (or (location? (package-location hello))
             (not (package-location hello)))
         (let* ((drv (package-derivation %store hello))
                (out (derivation-path->output-path drv)))
           (and (build-derivations %store (list drv))
                (file-exists? (string-append out "/bin/hello")))))))

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
;;; eval: (put 'dummy-package 'scheme-indent-function 1)
;;; End:
