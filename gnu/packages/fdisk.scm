;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (gnu packages fdisk)
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module ((gnu packages gettext)
                #:renamer (symbol-prefix-proc 'guix:))
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages parted)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public fdisk
  (package
    (name "fdisk")
    (version "2.0.0a")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/fdisk/gnufdisk-"
                          version ".tar.gz"))
      (sha256
       (base32
        "04nd7civ561x2lwcmxhsqbprml3178jfc58fy1v7hzqg5k4nbhy3"))))
    (build-system gnu-build-system)
    (inputs
     `(("gettext" ,guix:gettext)
       ("guile" ,guile-1.8)
       ("util-linux" ,util-linux)
       ("parted" ,parted)))
    (home-page "https://www.gnu.org/software/fdisk/")
    (synopsis "Low-level disk partitioning and formatting")
    (description
     "GNU Fdisk provides alternatives to util-linux fdisk and util-linux
cfdisk.  It uses GNU Parted.")
    (license gpl3+)))
