;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
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

(define-module (test-import-utils)
  #:use-module (guix tests)
  #:use-module (guix import utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (srfi srfi-64))

(test-begin "import-utils")

(test-equal "beautify-description: use double spacing"
  "This is a package.  It is great.  Trust me Mr.  Hendrix."
  (beautify-description
   "This is a package. It is great. Trust me Mr. Hendrix."))

(test-equal "beautify-description: transform fragment into sentence"
  "This package provides a function to establish world peace"
  (beautify-description "A function to establish world peace"))

(test-equal "license->symbol"
  'license:lgpl2.0
  (license->symbol license:lgpl2.0))

(test-end "import-utils")
