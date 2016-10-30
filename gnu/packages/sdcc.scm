;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
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

(define-module (gnu packages sdcc)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages python)
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public sdcc
  (package
    (name "sdcc")
    (version "3.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/sdcc/sdcc"
                    "/" version "/sdcc-src-" version ".tar.bz2"))
              (sha256
               (base32
                "0x53gh5yrrfjvlnkk29mjn8hq4v52alrsf7c8nsyzzq13sqwwpg8"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("boost" ,boost)
       ("flex" ,flex)
       ("python-2" ,python-2)
       ("texinfo" ,texinfo)))
    (arguments
     `(;; gputils is required for PIC ports
       #:configure-flags
       '("--disable-pic14-port" "--disable-pic16-port" "--enable-ucsim")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-makefile
           (lambda _
             (substitute* (find-files "." "(\\.mk$|\\.in$)")
               (("/bin/sh") (which "sh")))
             #t)))))
    (home-page "http://sdcc.sourceforge.net")
    (synopsis "Small devices C compiler")
    (description "SDCC is a retargettable, optimizing Standard C compiler suite
that targets the Intel MCS51 based microprocessors (8031, 8032, 8051, 8052, ...),
Maxim (formerly Dallas) DS80C390 variants, Freescale (formerly Motorola)
HC08 based (hc08, s08), Zilog Z80 based MCUs (z80, z180, gbz80, Rabbit
2000/3000, Rabbit 3000A, TLCS-90) and STMicroelectronics STM8.
Work is in progress on supporting the Microchip PIC16 and PIC18 targets.
It can be retargeted for other microprocessors.")
    (license license:gpl2+)))
