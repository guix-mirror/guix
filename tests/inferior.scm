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
  #:use-module (guix inferior)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define %top-srcdir
  (dirname (search-path %load-path "guix.scm")))

(define %top-builddir
  (dirname (search-path %load-compiled-path "guix.go")))


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

(test-end "inferior")
