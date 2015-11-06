;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
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

(define-module (gnu packages avr)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages zip))

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
    (license (license:non-copyleft "http://www.nongnu.org/avr-libc/LICENSE.txt"))))

(define-public microscheme
  (package
    (name "microscheme")
    (version "0.9.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ryansuchocki/"
                                  "microscheme/archive/v" version ".zip"))
              (sha256
               (base32
                "0cmp1c6ilcib4w9ysqghav310g8jsq9gdfpfa9sd23wgl7mlncxf"))
              (file-name (string-append name "-" version ".zip"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f ; fails to build otherwise
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (native-inputs
     `(("unzip" ,unzip)
       ("vim" ,vim))) ; for xxd
    (home-page "http://microscheme.org/")
    (synopsis "Scheme subset for Atmel microcontrollers")
    (description
     "Microscheme, or @code{(ms)} for short, is a functional programming
language for the Arduino, and for Atmel 8-bit AVR microcontrollers in general.
Microscheme is a subset of Scheme, in the sense that every valid @code{(ms)}
program is also a valid Scheme program (with the exception of Arduino
hardware-specific primitives).  The @code{(ms)} compiler performs function
inlining, and features an aggressive tree-shaker, eliminating unused top-level
definitions.  Microscheme has a robust @dfn{Foreign Function Interface} (FFI)
meaning that C code may be invoked directly from (ms) programs.")
    (license license:expat)))
