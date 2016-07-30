;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (gnu packages education)
  #:use-module (ice-9 regex)
  #:use-module (gnu packages)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages perl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix svn-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (srfi srfi-1))

(define-public stellarium
  (package
    (name "stellarium")
    (version "0.14.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/stellarium/"
                                 "Stellarium-sources/"
                                 version "/stellarium-" version ".tar.gz"))
             (sha256 (base32
                      "1xxil0rv61zc08znfv83cpsc47y1gjl2f3njhz0pn5zd8jpaa15a"))))
    (build-system cmake-build-system)
    (inputs
      `(("qtbase" ,qtbase)
        ("zlib" ,zlib)
        ("qtserialport" ,qtserialport)
        ("qtscript" ,qtscript)
        ("gettext" ,gnu-gettext)))
    (native-inputs
      `(("qtbase" ,qtbase)                   ;Qt MOC is needed at compile time
        ("qttools" ,qttools)
        ("perl" ,perl)))                          ;for 'pod2man'
    (arguments
      `(#:test-target "tests"
        #:phases (modify-phases %standard-phases
                   (add-before 'check 'set-offscreen-display
                     (lambda _
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
