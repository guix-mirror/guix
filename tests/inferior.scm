;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-inferior)
  #:use-module (guix tests)
  #:use-module (guix inferior)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define %top-srcdir
  (dirname (search-path %load-path "guix.scm")))

(define %top-builddir
  (dirname (search-path %load-compiled-path "guix.go")))

(define %store
  (open-connection-for-tests))


(test-begin "inferior")

(test-equal "open-inferior"
  '(42 #t)
  (let ((inferior (open-inferior %top-builddir
                                 #:command "scripts/guix")))
    (and (inferior? inferior)
         (let ((a (inferior-eval '(apply * '(6 7)) inferior))
               (b (inferior-eval '(@ (gnu packages base) coreutils)
                                 inferior)))
           (close-inferior inferior)
           (list a (inferior-object? b))))))

(test-equal "inferior-packages"
  (take (sort (fold-packages (lambda (package lst)
                               (cons (list (package-name package)
                                           (package-version package)
                                           (package-home-page package)
                                           (package-location package))
                                     lst))
                             '())
              (lambda (x y)
                (string<? (car x) (car y))))
        10)
  (let* ((inferior (open-inferior %top-builddir
                                  #:command "scripts/guix"))
         (packages (inferior-packages inferior)))
    (and (every string? (map inferior-package-synopsis packages))
         (let ()
           (define result
             (take (sort (map (lambda (package)
                                (list (inferior-package-name package)
                                      (inferior-package-version package)
                                      (inferior-package-home-page package)
                                      (inferior-package-location package)))
                              packages)
                         (lambda (x y)
                           (string<? (car x) (car y))))
                   10))
           (close-inferior inferior)
           result))))

(test-equal "lookup-inferior-packages"
  (let ((->list (lambda (package)
                  (list (package-name package)
                        (package-version package)
                        (package-location package)))))
    (list (map ->list (find-packages-by-name "guile" #f))
          (map ->list (find-packages-by-name "guile" "2.2"))))
  (let* ((inferior (open-inferior %top-builddir
                                  #:command "scripts/guix"))
         (->list   (lambda (package)
                     (list (inferior-package-name package)
                           (inferior-package-version package)
                           (inferior-package-location package))))
         (lst1     (map ->list
                        (lookup-inferior-packages inferior "guile")))
         (lst2     (map ->list
                        (lookup-inferior-packages inferior
                                                  "guile" "2.2"))))
    (close-inferior inferior)
    (list lst1 lst2)))

(test-assert "lookup-inferior-packages and eq?-ness"
  (let* ((inferior (open-inferior %top-builddir
                                  #:command "scripts/guix"))
         (lst1     (lookup-inferior-packages inferior "guile"))
         (lst2     (lookup-inferior-packages inferior "guile")))
    (close-inferior inferior)
    (every eq? lst1 lst2)))

(test-equal "inferior-package-derivation"
  (map derivation-file-name
       (list (package-derivation %store %bootstrap-guile "x86_64-linux")
             (package-derivation %store %bootstrap-guile "armhf-linux")))
  (let* ((inferior (open-inferior %top-builddir
                                  #:command "scripts/guix"))
         (packages (inferior-packages inferior))
         (guile    (find (lambda (package)
                           (string=? (package-name %bootstrap-guile)
                                     (inferior-package-name package)))
                         packages)))
    (map derivation-file-name
         (list (inferior-package-derivation %store guile "x86_64-linux")
               (inferior-package-derivation %store guile "armhf-linux")))))

(test-end "inferior")
