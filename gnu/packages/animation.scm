;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages animation)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages))

(define-public etl
  (package
    (name "etl")
    (version "0.04.19")
    (source (origin
              (method url-fetch)
              ;; Keep this synchronized with the synfig release version.
              (uri (string-append "mirror://sourceforge/synfig/releases/"
                                  "1.0.2/source/ETL-" version ".tar.gz"))
              (sha256
               (base32
                "070c70slizrklq1gbgja8m49xfmq65wlcd6hz6418cpx0wd4r55s"))))
    (build-system gnu-build-system)
    (home-page "http://www.synfig.org")
    (synopsis "Extended C++ template library")
    (description
     "ETL is a class and template library designed to add new datatypes and
functions which combine well with the existing types and functions from the
C++ @dfn{Standard Template Library} (STL).")
    (license license:gpl3+)))

