;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2016 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2015, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages flashing-tools)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages vim))

(define-public avr-binutils
  (package
    (inherit (cross-binutils "avr"))
    (name "avr-binutils")))

(define-public avr-gcc-4.9
  (let ((xgcc (cross-gcc "avr" #:xgcc gcc-4.9 #:xbinutils avr-binutils)))
    (package
      (inherit xgcc)
      (name "avr-gcc")
      (arguments
       (substitute-keyword-arguments (package-arguments xgcc)
         ((#:phases phases)
          `(modify-phases ,phases
             ;; Without a working multilib build, the resulting GCC lacks
             ;; support for nearly every AVR chip.
             (add-after 'unpack 'fix-genmultilib
               (lambda _
                 ;; patch-shebang doesn't work here because there are actually
                 ;; several scripts inside this script, each with a #!/bin/sh
                 ;; that needs patching.
                 (substitute* "gcc/genmultilib"
                   (("#!/bin/sh") (string-append "#!" (which "sh"))))
                 #t))))
         ((#:configure-flags flags)
          `(delete "--disable-multilib" ,flags))))
      (native-search-paths
       (list (search-path-specification
              (variable "CROSS_C_INCLUDE_PATH")
              (files '("avr/include")))
             (search-path-specification
              (variable "CROSS_CPLUS_INCLUDE_PATH")
              (files '("avr/include")))
             (search-path-specification
              (variable "CROSS_OBJC_INCLUDE_PATH")
              (files '("avr/include")))
             (search-path-specification
              (variable "CROSS_OBJCPLUS_INCLUDE_PATH")
              (files '("avr/include")))
             (search-path-specification
              (variable "CROSS_LIBRARY_PATH")
              (files '("avr/lib")))))
      (native-inputs
       `(("gcc" ,gcc-5)
         ,@(package-native-inputs xgcc))))))

(define-public avr-gcc-5
  (package
    (inherit avr-gcc-4.9)
    (version (package-version gcc-5))
    (source (origin
              (inherit (package-source gcc-5))
              (patches (append (origin-patches (package-source gcc-5))
                               (search-patches "gcc-cross-environment-variables.patch")))))))

(define (avr-libc avr-gcc)
  (package
    (name "avr-libc")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah//avr-libc/avr-libc-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "15svr2fx8j6prql2il2fc0ppwlv50rpmyckaxx38d3gxxv97zpdj"))))
    (build-system gnu-build-system)
    (arguments
     '(#:out-of-source? #t
       #:configure-flags '("--host=avr")
       #:phases
       (modify-phases %standard-phases
         (add-before 'unpack 'fix-cpath
           (lambda _
             ;; C_INCLUDE_PATH and CPATH pose issues for cross-building,
             ;; leading to failures when building avr-libc on 64-bit systems.
             ;; Simply unsetting them allows the build to succeed because it
             ;; doesn't try to use any of the native system's headers.
             (unsetenv "C_INCLUDE_PATH")
             (unsetenv "CPATH")
             #t)))))
    (native-inputs `(("avr-binutils" ,avr-binutils)
                     ("avr-gcc" ,avr-gcc)))
    (home-page "https://www.nongnu.org/avr-libc/")
    (synopsis "The AVR C Library")
    (description
     "AVR Libc is a project whose goal is to provide a high quality C library
for use with GCC on Atmel AVR microcontrollers.")
    (license
     (license:non-copyleft "http://www.nongnu.org/avr-libc/LICENSE.txt"))))

(define (avr-toolchain avr-gcc)
  ;; avr-libc checks the compiler version and passes "--enable-device-lib" for avr-gcc > 5.1.0.
  ;; It wouldn't install the library for atmega32u4 etc if we didn't use the corret avr-gcc.
  (let ((avr-libc (avr-libc avr-gcc)))
    (package
      (name "avr-toolchain")
      (version (package-version avr-gcc))
      (source #f)
      (build-system trivial-build-system)
      (arguments '(#:builder (begin (mkdir %output) #t)))
      (propagated-inputs
       `(("avrdude" ,avrdude)
         ("binutils" ,avr-binutils)
         ("gcc" ,avr-gcc)
         ("libc" ,avr-libc)))
      (synopsis "Complete GCC tool chain for AVR microcontroller development")
      (description "This package provides a complete GCC tool chain for AVR
microcontroller development.  This includes the GCC AVR cross compiler and
avrdude for firmware flashing.  The supported programming languages are C and
C++.")
      (home-page (package-home-page avr-libc))
      (license (package-license avr-gcc)))))

(define-public avr-toolchain-4.9 (avr-toolchain avr-gcc-4.9))
(define-public avr-toolchain-5 (avr-toolchain avr-gcc-5))

(define-public microscheme
  (package
    (name "microscheme")
    (version "0.9.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ryansuchocki/microscheme.git")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1r3ng4pw1s9yy1h5rafra1rq19d3vmb5pzbpcz1913wz22qdd976"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f             ; fails to build otherwise
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (native-inputs
     `(("unzip" ,unzip)
       ("xxd" ,xxd)))
    (home-page "https://github.com/ryansuchocki/microscheme/")
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
