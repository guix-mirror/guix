;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-discovery)
  #:use-module (guix discovery)
  #:use-module (guix build-system)
  #:use-module (guix utils)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(define %top-srcdir
  (dirname (search-path %load-path "guix.scm")))

(test-begin "discovery")

(test-assert "scheme-modules"
  (match (map module-name (scheme-modules %top-srcdir "guix/import"))
    ((('guix 'import _ ...) ..1)
     #t)))

(test-assert "scheme-modules recurses in symlinks to directories"
  (call-with-temporary-directory
   (lambda (directory)
     (mkdir (string-append directory "/guix"))
     (symlink (string-append %top-srcdir "/guix/import")
              (string-append directory "/guix/import"))

     ;; DIRECTORY/guix/import is a symlink but we want to make sure
     ;; 'scheme-modules' recurses into it.
     (match (map module-name (scheme-modules directory))
       ((('guix 'import _ ...) ..1)
        #t)))))

(test-equal "scheme-modules, non-existent directory"
  '()
  (scheme-modules "/does/not/exist"))

(test-assert "all-modules"
  (match (map module-name
              (all-modules `((,%top-srcdir . "guix/build-system"))))
    ((('guix 'build-system names) ..1)
     names)))

(test-assert "fold-module-public-variables"
  (let ((modules (all-modules `((,%top-srcdir . "guix/build-system")))))
    (match (fold-module-public-variables (lambda (obj result)
                                           (if (build-system? obj)
                                               (cons obj result)
                                               result))
                                         '()
                                         modules)
      (((? build-system? bs) ..1)
       bs))))

(test-end "discovery")
