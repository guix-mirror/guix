;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (gnu packages raspberry-pi)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages embedded)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define-public bcm2835
  (package
    (name "bcm2835")
    (version "1.64")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.airspayce.com/mikem/bcm2835/bcm2835-"
                    version ".tar.gz"))
              (sha256
               (base32
                "06s81540iz4vsh0cm6jwah2x0hih79v42pfa4pgr8kcbv56158h6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))    ; Would need to be root
    ;; doc/html docs would not be installed anyway.
    ;(native-inputs
    ; `(("doxygen" ,doxygen)))
    (synopsis "C library for Broadcom BCM 2835 as used in Raspberry Pi")
    (description "This package provides a C library for Broadcom BCM 2835 as
used in the Raspberry Pi")
    (home-page "http://www.airspayce.com/mikem/bcm2835/")
    (supported-systems '("armhf-linux" "aarch64-linux"))
    (license license:gpl3)))

(define raspi-gpio
  (let ((commit "6d0769ac04760b6e9f33b4aa1f11c682237bf368")
        (revision "1"))
    (package
      (name "raspi-gpio")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/RPi-Distro/raspi-gpio")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1fia1ma586hwhpda0jz86j6i55andq0wncbhzhzvhf7yc773cpi4"))))
      (build-system gnu-build-system)
      (synopsis "State dumper for BCM270x GPIOs")
      (description "Tool to help debug / hack at the BCM283x GPIO. You can dump
  the state of a GPIO or (all GPIOs). You can change a GPIO mode and pulls (and
  level if set as an output).  Beware this tool writes directly to the BCM283x
  GPIO reisters, ignoring anything else that may be using them (like Linux
  drivers).")
      (home-page "https://github.com/RPi-Distro/raspi-gpio")
      (supported-systems '("armhf-linux" "aarch64-linux"))
      (license license:bsd-3))))

(define %rpi-open-firmware-version "0.1")
(define %rpi-open-firmware-origin
  (origin
   (method git-fetch)
   (uri (git-reference
         (url "https://github.com/librerpi/rpi-open-firmware")
         (commit "6be45466e0be437a1b0b3512a86f3d9627217006")))
   (file-name "rpi-open-firmware-checkout")
   (sha256
    (base32 "1wyxvv62i3rjicg4hd94pzbgpadinnrgs27sk39md706mm0qixbh"))))

(define-public raspi-arm-chainloader
  (package
    (name "raspi-arm-chainloader")
    (version %rpi-open-firmware-version)
    (source %rpi-open-firmware-origin)
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                   ; No tests exist
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'setenv
           (lambda _
             (setenv "CC" "arm-none-eabi-gcc")
             (setenv "CXX" "arm-none-eabi-g++")
             (setenv "AS" "arm-none-eabi-as")
             (setenv "OBJCOPY" "arm-none-eabi-objcopy")
             (setenv "BAREMETAL" "1")
             #t))
         (add-after 'setenv 'build-tlsf
           (lambda _
             (with-directory-excursion "tlsf"
               ;; Note: Adding "-I../common -I../notc/include".
               (invoke "make"
                       "CFLAGS=-mtune=arm1176jzf-s -march=armv6zk -mfpu=vfp -mfloat-abi=softfp -I../common -I../notc/include"))))
         (add-after 'build-tlsf 'build-common
           (lambda _
             (with-directory-excursion "common"
               (invoke "make"
                       ;; Note: Adding "-I.. -I../notc/include".
                       "ARMCFLAGS=-mtune=arm1176jzf-s -march=armv6zk -marm -I.. -I../notc/include"))))
         (add-after 'build-common 'build-notc
           (lambda _
             (with-directory-excursion "notc"
               (invoke "make"))))
         (add-after 'build-notc 'chdir
           (lambda _
             (chdir "arm_chainloader")
             (substitute* "Makefile"
              (("-I[.][.]/")
               "-I../common -I../common/include -I../notc/include -I../")
              (("-ltlsf")
               "-L../common -L../notc -L../tlsf -ltlsf"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (libexec (string-append out "/libexec")))
               (mkdir-p libexec)
               (install-file "build/arm_chainloader.elf" libexec)
               (install-file "build/arm_chainloader.map" libexec)
               (install-file "build/arm_chainloader.bin" libexec)
               #t))))))
    (native-inputs
     `(("binutils" ,(cross-binutils "arm-none-eabi"))
       ("gcc" ,gcc-arm-none-eabi-6)))
    (inputs
     `())
    (synopsis "Raspberry Pi ARM bootloader")
    (description "This package provides a bootloader for the ARM part of a
Raspberry Pi.  Note: It does not work on Raspberry Pi 1.")
    (home-page "https://github.com/librerpi/rpi-open-firmware/")
    (license license:gpl2+)))

(define-public raspi-arm64-chainloader
  (package
    (inherit raspi-arm-chainloader)
    (name "raspi-arm64-chainloader")
    ;; These native-inputs especially don't contain a libc.
    (native-inputs
     `(("bash" ,bash)
       ("binutils" ,binutils)
       ("coreutils" ,coreutils)
       ("file" ,file)
       ("ld-wrapper" ,ld-wrapper)
       ("make" ,gnu-make)
       ("gcc" ,gcc-6)
       ("locales" ,glibc-utf8-locales)))
    (inputs
     `())
    (arguments
     `(#:implicit-inputs? #f
       ,@(substitute-keyword-arguments (package-arguments raspi-arm-chainloader)
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'setenv
               (lambda _
                 (setenv "AS" "as") ; TODO: as-for-target
                 (setenv "OBJCOPY" "objcopy")
                 (setenv "CC" ,(cc-for-target))
                 (setenv "CXX" ,(cc-for-target))
                 (setenv "BAREMETAL" "1")
                 #t))
             (add-after 'setenv 'build-tlsf
               (lambda _
                 (with-directory-excursion "tlsf"
                   (invoke "make"
                           "CFLAGS=-I../common -I../notc/include"))))
             (replace 'build-common
               (lambda _
                 (with-directory-excursion "common"
                   ;; Autodetection uses the CC filename for detecting the architecture.
                   ;; Since we are not using a cross-compiler, we side-step that.
                   (invoke "make"
                           "CFLAGS=-Ilib -I. -Iinclude -ffunction-sections -Wall -g -nostdlib -nostartfiles -ffreestanding -DBAREMETAL"))))
             (replace 'build-notc
               (lambda _
                 (with-directory-excursion "notc"
                   ;; Autodetection uses the CC filename for detecting the architecture.
                   ;; Since we are not using a cross-compiler, we side-step that.
                   (invoke "make"
                           "CFLAGS=-Iinclude -g"))))
             (replace 'chdir
               (lambda _
                 (chdir "arm64")
                 (substitute* "Makefile"
                  (("CFLAGS =")
                   "CFLAGS = -I../common -I../common/include -I../notc/include -I.. -DBAREMETAL")
                  (("-lcommon")
                   "-L../common -L../notc -lcommon"))
                 #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (libexec (string-append out "/libexec")))
               (mkdir-p libexec)
               (install-file "arm64.elf" libexec)
               (install-file "arm64.map" libexec)
               (install-file "arm64.bin" libexec)
               #t))))))))
    (supported-systems '("aarch64-linux"))))
