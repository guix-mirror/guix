;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
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

(define-module (gnu packages astronomy)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (gnu packages image)
  #:use-module (gnu packages maths)
  #:use-module (guix build-system gnu))

(define-public cfitsio
  (package
    (name "cfitsio")
    (version "3390")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://heasarc.gsfc.nasa.gov/FTP/software/fitsio/c/" name version
             ".tar.gz"))
       (sha256
        (base32 "02gllydm63irwbqqisa3mrskw1fphm5rlplglz3mq9whi3rxilv2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda _
             (substitute* "Makefile.in" (("/bin/") ""))
             #t)))))
    (home-page "http://heasarc.gsfc.nasa.gov/fitsio/fitsio.html")
    (synopsis "Library for reading and writing FITS files")
    (description "CFITSIO provides simple high-level routines for reading and
writing FITS (Flexible Image Transport System) files that insulate the
programmer from the internal complexities of the FITS format. CFITSIO also
provides many advanced features for manipulating and filtering the information
in FITS files.")
    (license (license:non-copyleft "file://License.txt"
                          "See License.txt in the distribution."))))

(define-public wcslib
  (package
    (name "wcslib")
    (version "5.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "ftp://ftp.atnf.csiro.au/pub/software/wcslib/" name "-" version
             ".tar.bz2"))
       (sha256
        (base32 "1s2nig327g4bimd9xshlk11ww09a7mrjmsbpdcd8smsmn2kl1glb"))))
    (inputs
     `(("cfitsio" ,cfitsio)))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'patch-/bin/sh
                    (lambda _
                      (substitute* "makedefs.in"
                        (("/bin/sh") "sh"))
                      #t)))))
    (home-page "http://www.atnf.csiro.au/people/mcalabre/WCS")
    (synopsis "Library which implements the FITS WCS standard")
    (description "The FITS \"World Coordinate System\" (WCS) standard defines
keywords and usage that provide for the description of astronomical coordinate
systems in a FITS image header.")
    (license license:lgpl3+)))

(define-public gnuastro
  (package
    (name "gnuastro")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/gnuastro/gnuastro-"
                           version ".tar.gz"))
       (sha256
        (base32
         "105s007kw8l3jwwhvh8k9lgbpfbf7sqh2wpxmvpv3qdr6nh9lnjg"))))
    (inputs
     `(("cfitsio" ,cfitsio)
       ("gsl" ,gsl)
       ("libjpeg" ,libjpeg-8)
       ("wcslib" ,wcslib)))
    (build-system gnu-build-system)
    (home-page "http://www.gnu.org/software/gnuastro")
    (synopsis "Astronomical data manipulation programs")
    (description "The GNU Astronomy Utilities (Gnuastro) is a suite of
programs for the manipulation and analysis of astronomical data.")
    (license license:gpl3+)))
