;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
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

(define-module (gnu packages avr)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages cross-base))

(define-public avr-libc
  (package
    (name "avr-libc")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://savannah//avr-libc/avr-libc-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0sd9qkvhmk9av4g1f8dsjwc309hf1g0731bhvicnjb3b3d42l1n3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:out-of-source? #t
       #:configure-flags '("--host=avr")))

    (native-inputs `(("cross-binutils" ,(cross-binutils "avr"))
                     ("cross-gcc" ,xgcc-avr)))
    (home-page "http://www.nongnu.org/avr-libc/")
    (synopsis "The AVR C Library")
    (description
     "AVR Libc is a project whose goal is to provide a high quality C library
for use with GCC on Atmel AVR microcontrollers.")
    (license (bsd-style "http://www.nongnu.org/avr-libc/LICENSE.txt"))))
