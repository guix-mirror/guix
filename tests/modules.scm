;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (srfi srfi-1)
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

(test-assert "closure of (gnu build vm)"
  (lset= equal?
         (live-module-closure '((gnu build vm)))
         (source-module-closure '((gnu build vm)))))

(test-end)
