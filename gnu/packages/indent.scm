;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages indent)
  #:use-module (gnu packages)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public indent
  (package
   (name "indent")
   (version "2.2.10")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/indent/indent-" version
                                ".tar.gz"))
            (sha256 (base32
                     "0f9655vqdvfwbxvs1gpa7py8k1z71aqh8hp73f65vazwbfz436wa"))))
   (build-system gnu-build-system)
   (synopsis "Code reformatter")
   (description
    "Indent is a program that makes source code easier to read by
reformatting it in a consistent style.  It can change the style to one of
several different styles such as GNU, BSD or K&R.  It has some flexibility to
deal with incomplete or malformed syntax.  GNU indent offers several
extensions over the standard utility.")
   (license license:gpl3+)
   (home-page "http://www.gnu.org/software/indent/")))
