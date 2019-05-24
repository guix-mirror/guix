;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Li-cheng (Andy) Tai <atai@atai.org>
;;
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

(define-module (gnu packages squirrel)
  #:use-module (gnu packages)
  #:use-module (gnu packages cmake)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public squirrel
  (package
    (name "squirrel")
    (version "3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/squirrel/squirrel3/"
                                  "squirrel " version " stable/squirrel_"
                                  (string-join (string-split version #\.) "_")
                                  "_stable.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jyh1523zrrnh9swanfrda0s14mvwc9431dh07g0nx74hbxsfia8"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (native-inputs
     `(("cmake" ,cmake)))
    (home-page "https://squirrel-lang.org/")
    (synopsis "High level imperative, object-oriented programming language")
    (description
     "Squirrel is a high level imperative, object-oriented programming
language, designed to be a light-weight scripting language that fits in the
size, memory bandwidth, and real-time requirements of applications like video
games.")
    (license license:expat)))
