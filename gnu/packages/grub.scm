;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages grub)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages bison)
  #:use-module ((gnu packages gettext) #:renamer (symbol-prefix-proc 'gnu:))
  #:use-module (gnu packages freetype)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages qemu)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages cdrom))

(define-public grub
  (package
    (name "grub")
    (version "2.00")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/grub/grub-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0n64hpmsccvicagvr0c6v0kgp2yw0kgnd3jvsyd26cnwgs7c6kkq"))))
    (build-system gnu-build-system)
    (arguments
     '(#:patches (list (assoc-ref %build-inputs "patch/gets"))
       #:configure-flags '("--disable-werror")
       #:phases (alist-cons-before
                 'patch-source-shebangs 'patch-stuff
                 (lambda _
                   (substitute* "grub-core/Makefile.in"
                     (("/bin/sh") (which "sh")))

                   ;; TODO: Re-enable this test when we have Parted.
                   (substitute* "tests/partmap_test.in"
                     (("set -e") "exit 77")))
                 %standard-phases)))
    (inputs
     `(;; ("lvm2" ,lvm2)
       ("gettext" ,gnu:gettext)
       ("freetype" ,freetype)
       ;; ("libusb" ,libusb)
       ("ncurses" ,ncurses)

       ("patch/gets" ,(search-patch "grub-gets-undeclared.patch"))))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)

       ;; Dependencies for the test suite.  The "real" QEMU is needed here,
       ;; because several targets are used.
       ("qemu" ,qemu)
       ("xorriso" ,xorriso)))
    (home-page "http://www.gnu.org/software/grub/")
    (synopsis
     "GNU GRUB, the Grand Unified Boot Loader (2.x beta)")
    (description
     "GNU GRUB is a Multiboot boot loader. It was derived from GRUB, GRand
Unified Bootloader, which was originally designed and implemented by Erich
Stefan Boleyn.

Briefly, the boot loader is the first software program that runs when a
computer starts.  It is responsible for loading and transferring control to
the operating system kernel software (such as the Hurd or the Linux).  The
kernel, in turn, initializes the rest of the operating system (e.g., GNU).")
    (license gpl3+)))
