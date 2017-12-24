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
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages maths)
  #:use-module (guix build-system cmake)
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
    (version "5.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "ftp://ftp.atnf.csiro.au/pub/software/wcslib/" name "-" version
             ".tar.bz2"))
       (sha256
        (base32 "0v23x1fw01arhmqdrzfd9n593mjglhzfyx4793v065z0dg4bb72w"))))
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
    (home-page "https://www.atnf.csiro.au/people/mcalabre/WCS")
    (synopsis "Library which implements the FITS WCS standard")
    (description "The FITS \"World Coordinate System\" (WCS) standard defines
keywords and usage that provide for the description of astronomical coordinate
systems in a FITS image header.")
    (license license:lgpl3+)))

(define-public gnuastro
  (package
    (name "gnuastro")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/gnuastro/gnuastro-"
                           version ".tar.gz"))
       (sha256
        (base32
         "10lxzxyrf30hj3bqdgprvaj9phzdi816khjmr0vmjf8pmsr8bqqr"))))
    (inputs
     `(("cfitsio" ,cfitsio)
       ("gsl" ,gsl)
       ("libjpeg" ,libjpeg)
       ("wcslib" ,wcslib)))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/gnuastro/")
    (synopsis "Astronomy utilities")
    (description "The GNU Astronomy Utilities (Gnuastro) is a suite of
programs for the manipulation and analysis of astronomical data.")
    (license license:gpl3+)))

(define-public stellarium
  (package
    (name "stellarium")
    (version "0.16.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/stellarium/"
                                 "Stellarium-sources/"
                                 version "/stellarium-" version ".tar.gz"))
             (sha256
              (base32
               "1krxj51lix096xbz64lys166a8zdwhill5vvs7dlxdn14amc8d98"))))
    (build-system cmake-build-system)
    (inputs
     `(("qtbase" ,qtbase)
       ("qtlocation" ,qtlocation)
       ("qtmultimedia" ,qtmultimedia)
       ("qtscript" ,qtscript)
       ("qtserialport" ,qtserialport)
       ("zlib" ,zlib)))
    (native-inputs
     `(("gettext" ,gettext-minimal) ; xgettext is used at compile time
       ("perl" ,perl) ; For pod2man
       ("qtbase" ,qtbase) ; Qt MOC is needed at compile time
       ("qttools" ,qttools)))
    (arguments
      `(#:test-target "tests"
        #:phases (modify-phases %standard-phases
                   (add-after 'unpack 'patch-tests
                     (lambda _
                       (substitute* "src/tests/testStelSphereGeometry.cpp"
                         (("Vec3d v[(]0[)]") "Vec3d v(0.0)"))
                       #t))
                   (add-before 'check 'set-offscreen-display
                     (lambda _
                       ;; make Qt render "offscreen", required for tests
                       (setenv "QT_QPA_PLATFORM" "offscreen")
                       (setenv "HOME" "/tmp")
                       #t)))))
    (home-page "http://www.stellarium.org/")
    (synopsis "3D sky viewer")
    (description "Stellarium is a planetarium.  It shows a realistic sky in
3D, just like what you see with the naked eye, binoculars, or a telescope.  It
can be used to control telescopes over a serial port for tracking celestial
objects.")
    (license license:gpl2+)))
