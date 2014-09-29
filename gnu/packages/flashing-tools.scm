;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages flashing-tools)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages libftdi)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages admin))

(define-public flashrom
  (package
    (name "flashrom")
    (version "0.9.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://download.flashrom.org/releases/flashrom-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1s9pc4yls2s1gcg2ar4q75nym2z5v6lxq36bl6lq26br00nj2mas"))
              (patches (list (search-patch "flashrom-use-libftdi1.patch")))))
    (build-system gnu-build-system)
    (inputs `(("dmidecode" ,dmidecode)
              ("pciutils" ,pciutils)
              ("libusb" ,libusb)
              ("libftdi" ,libftdi)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments
     '(#:make-flags (list "CC=gcc" (string-append "PREFIX=" %output))
       #:tests? #f   ; no 'check' target
       #:phases
       (alist-delete
        'configure
        (alist-cons-before
         'build 'patch-exec-paths
         (lambda* (#:key inputs #:allow-other-keys)
           (substitute* "dmi.c"
             (("\"dmidecode\"")
              (format #f "~S"
                      (string-append (assoc-ref inputs "dmidecode")
                                     "/sbin/dmidecode")))))
         %standard-phases))))
    (home-page "http://flashrom.org/")
    (synopsis "Identify, read, write, erase, and verify ROM/flash chips")
    (description
     "flashrom is a utility for identifying, reading, writing,
verifying and erasing flash chips.  It is designed to flash
BIOS/EFI/coreboot/firmware/optionROM images on mainboards,
network/graphics/storage controller cards, and various other
programmer devices.")
    (license gpl2)))

(define-public avrdude
  (package
    (name "avrdude")
    (version "6.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://savannah/avrdude/avrdude-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0frxg0q09nrm95z7ymzddx7ysl77ilfbdix1m81d9jjpiv5bm64y"))
      (patches (list (search-patch "avrdude-fix-libusb.patch")))))
    (build-system gnu-build-system)
    (inputs
     `(("libelf" ,libelf)
       ("libusb" ,libusb)
       ("libftdi" ,libftdi)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)))
    (home-page "http://www.nongnu.org/avrdude/")
    (synopsis "AVR downloader and uploader")
    (description
     "AVRDUDE is a utility to download/upload/manipulate the ROM and
EEPROM contents of AVR microcontrollers using the in-system programming
technique (ISP).")
    (license gpl2+)))

(define-public dfu-programmer
  (package
    (name "dfu-programmer")
    (version "0.7.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/dfu-programmer/dfu-programmer-"
                          version ".tar.gz"))
      (sha256
       (base32
        "17lglglk5xrqd2n0impg5bkq4j96qc51cw3kzcghzmzmn6fvg3gf"))
      (patches (list (search-patch "dfu-programmer-fix-libusb.patch")))))
    (build-system gnu-build-system)
    (inputs
     `(("libusb" ,libusb)))
    (home-page "http://dfu-programmer.github.io/")
    (synopsis "Device firmware update programmer for Atmel chips")
    (description
     "Dfu-programmer is a multi-platform command-line programmer for Atmel
(8051, AVR, XMEGA & AVR32) chips with a USB bootloader supporting ISP")
    (license gpl2+)))
