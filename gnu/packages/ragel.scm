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

(define-module (gnu packages ragel)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages))

(define-public ragel
  (package
    (name "ragel")
    (version "6.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.colm.net/files/ragel/ragel-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "02k6rwh8cr95f1p5sjjr3wa6dilg06572xz1v71dk8awmc7vw1vf"))))
    (build-system gnu-build-system)
    (home-page "https://www.colm.net/open-source/ragel/")
    (synopsis "State machine compiler")
    (description
     "Ragel compiles executable finite state machines from regular languages.
Ragel targets C, C++, Obj-C, C#, D, Java, Go and Ruby.  Ragel state machines
can not only recognize byte sequences as regular expression machines do, but
can also execute code at arbitrary points in the recognition of a regular
language.  Code embedding is done using inline operators that do not disrupt
the regular language syntax.")
    ;; GPLv2 (or later) with exception for generated code.
    (license license:gpl2+)))

