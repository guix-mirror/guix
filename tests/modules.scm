;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-modules)
  #:use-module (guix modules)
  #:use-module ((guix build-system gnu) #:select (%gnu-build-system-modules))
  #:use-module ((guix utils) #:select (call-with-temporary-directory))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64))

(test-begin "modules")

(test-assert "closure of (guix build gnu-build-system)"
  (lset= equal?
         (live-module-closure '((guix build gnu-build-system)))
         (source-module-closure '((guix build gnu-build-system)))
         %gnu-build-system-modules
         (source-module-closure %gnu-build-system-modules)
         (live-module-closure %gnu-build-system-modules)))

(test-assert "closure of (gnu build install)"
  (lset= equal?
         (live-module-closure '((gnu build install)))
         (source-module-closure '((gnu build install)))))

(test-assert "closure of (gnu build image)"
  (lset= equal?
         (live-module-closure '((gnu build image)))
         (source-module-closure '((gnu build image)))))

(test-equal "&missing-dependency-error"
  '(something that does not exist)
  (call-with-temporary-directory
   (lambda (directory)
     (call-with-output-file (string-append directory "/foobar.scm")
       (lambda (port)
         (write '(define-module (foobar)
                   #:use-module (something that does not exist))
                port)))

     (call-with-output-file (string-append directory "/baz.scm")
       (lambda (port)
         (write '(define-module (baz)
                   #:use-module (foobar))
                port)))

     (guard (c ((missing-dependency-error? c)
                (missing-dependency-module c)))
       (source-module-closure '((baz)) (list directory)
                              #:select? (const #t))))))

(test-end)
