;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-utils)
  #:use-module (guix utils)
  #:use-module ((guix store) #:select (%store-prefix store-path-package-name))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match))

(test-begin "utils")

(test-assert "bytevector->base16-string->bytevector"
  (every (lambda (bv)
           (equal? (base16-string->bytevector
                    (bytevector->base16-string bv))
                   bv))
         (map string->utf8 '("" "f" "fo" "foo" "foob" "fooba" "foobar"))))

(test-assert "gnu-triplet->nix-system"
  (let ((samples '(("i586-gnu0.3" "i686-gnu")
                   ("x86_64-unknown-linux-gnu" "x86_64-linux")
                   ("i386-pc-linux-gnu" "i686-linux")
                   ("x86_64-unknown-freebsd8.2" "x86_64-freebsd")
                   ("x86_64-apple-darwin10.8.0" "x86_64-darwin")
                   ("i686-pc-cygwin" "i686-cygwin"))))
    (let-values (((gnu nix) (unzip2 samples)))
      (every (lambda (gnu nix)
               (equal? nix (gnu-triplet->nix-system gnu)))
             gnu nix))))

(test-assert "package-name->name+version"
  (every (match-lambda
          ((name version)
           (let*-values (((full-name)
                          (if version
                              (string-append name "-" version)
                              name))
                         ((name* version*)
                          (package-name->name+version full-name)))
             (and (equal? name* name)
                  (equal? version* version)))))
         '(("foo" "0.9.1b")
           ("foo-bar" "1.0")
           ("foo-bar2" #f)
           ("guile" "2.0.6.65-134c9") ; as produced by `git-version-gen'
           ("nixpkgs" "1.0pre22125_a28fe19")
           ("gtk2" "2.38.0"))))

(test-assert "define-record-type*"
  (begin
    (define-record-type* <foo> foo make-foo
      foo?
      (bar foo-bar)
      (baz foo-baz (default (+ 40 2))))
    (and (match (foo (bar 1) (baz 2))
           (($ <foo> 1 2) #t))
         (match (foo (baz 2) (bar 1))
           (($ <foo> 1 2) #t))
         (match (foo (bar 1))
           (($ <foo> 1 42) #t)))))

(test-assert "define-record-type* with letrec* behavior"
  ;; Make sure field initializers can refer to each other as if they were in
  ;; a `letrec*'.
  (begin
    (define-record-type* <bar> bar make-bar
      foo?
      (x bar-x)
      (y bar-y (default (+ 40 2)))
      (z bar-z))
    (and (match (bar (x 1) (y (+ x 1)) (z (* y 2)))
           (($ <bar> 1 2 4) #t))
         (match (bar (x 7) (z (* x 3)))
           (($ <bar> 7 42 21)))
         (match (bar (z 21) (x (/ z 3)))
           (($ <bar> 7 42 21))))))

(test-assert "define-record-type* & inherit"
  (begin
    (define-record-type* <foo> foo make-foo
      foo?
      (bar foo-bar)
      (baz foo-baz (default (+ 40 2))))
    (let* ((a (foo (bar 1)))
           (b (foo (inherit a) (baz 2)))
           (c (foo (inherit b) (bar -2)))
           (d (foo (inherit c)))
           (e (foo (inherit (foo (bar 42))) (baz 77))))
     (and (match a (($ <foo> 1 42) #t))
          (match b (($ <foo> 1 2) #t))
          (match c (($ <foo> -2 2) #t))
          (equal? c d)
          (match e (($ <foo> 42 77) #t))))))

(test-assert "define-record-type* & inherit & letrec* behavior"
  (begin
    (define-record-type* <foo> foo make-foo
      foo?
      (bar foo-bar)
      (baz foo-baz (default (+ 40 2))))
    (let* ((a (foo (bar 77)))
           (b (foo (inherit a) (bar 1) (baz (+ bar 1))))
           (c (foo (inherit b) (baz 2) (bar (- baz 1)))))
     (and (match a (($ <foo> 77 42) #t))
          (match b (($ <foo> 1 2) #t))
          (equal? b c)))))

;; This is actually in (guix store).
(test-equal "store-path-package-name"
  "bash-4.2-p24"
  (store-path-package-name
   (string-append (%store-prefix)
                  "/qvs2rj2ia5vci3wsdb7qvydrmacig4pg-bash-4.2-p24")))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; Local Variables:
;;; eval: (put 'test-assert 'scheme-indent-function 1)
;;; End:
