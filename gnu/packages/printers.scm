;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages printers)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt))

;; This is a module for packages related to printer-like devices, but not
;; related to CUPS.

(define-public robocut
  (package
    (name "robocut")
    (version "1.0.11")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Timmmm/robocut/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32
                "0nmr1plq1f6sarxwqwy4vzbxkljlx8y4xalm7r05vx4lrdai5pad"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* "Robocut.pro"
                          (("/usr/")
                           (string-append out "/")))

                        (invoke "qmake"
                                (string-append "PREFIX=" out))
                        #t))))))
    (inputs
     `(("libusb" ,libusb)
       ("qt" ,qtbase)
       ("qtsvg" ,qtsvg)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qmake" ,qtbase)))
    (synopsis "Graphical program to drive plotting cutters")
    (description
     "Robocut is a simple graphical program that allows you to cut graphics
with Graphtec and Sihouette plotting cutters using an SVG file as its input.")
    (home-page "http://robocut.org")
    (license license:gpl3+)))
