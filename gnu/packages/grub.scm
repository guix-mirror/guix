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
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages bison)
  #:use-module ((gnu packages gettext) #:renamer (symbol-prefix-proc 'gnu:))
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages qemu)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages cdrom)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (menu-entry
            menu-entry?
            grub-configuration-file))

(define qemu-for-tests
  ;; Newer QEMU versions, such as 1.5.1, no longer support the 'shutdown'
  ;; instruction.  This leads to test hangs, as reported at
  ;; <https://bugs.launchpad.net/bugs/947597> and fixed at
  ;; <http://bzr.savannah.gnu.org/lh/grub/trunk/grub/revision/4828>.
  ;; Work around it by using an older QEMU.
  (package (inherit qemu)
    (version "1.3.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://wiki.qemu-project.org/download/qemu-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1bqfrb5dlsxm8gxhkksz8qzi5fhj3xqhxyfwbqcphhcv1kpyfwip"))))

    ;; With recent GLib versions, we get a test failure:
    ;;   ERROR:tests/rtc-test.c:176:check_time: assertion failed (ABS(t - s) <= wiggle): (382597824 <= 2)
    ;; Simply disable the tests.
    (arguments `(#:tests? #f
                          ,@(package-arguments qemu)))

    ;; The manual fails to build with Texinfo 5.x.
    (native-inputs (alist-delete "texinfo" (package-native-inputs qemu)))))

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
       ("qemu" ,qemu-for-tests)
       ("xorriso" ,xorriso)))
    (home-page "http://www.gnu.org/software/grub/")
    (synopsis "GRand unified boot loader")
    (description
     "GNU GRUB is a Multiboot boot loader. It was derived from GRUB, GRand
Unified Bootloader, which was originally designed and implemented by Erich
Stefan Boleyn.

Briefly, the boot loader is the first software program that runs when a
computer starts.  It is responsible for loading and transferring control to
the operating system kernel software (such as the Hurd or the Linux).  The
kernel, in turn, initializes the rest of the operating system (e.g., GNU).")
    (license gpl3+)))


;;;
;;; Configuration.
;;;

(define-record-type* <menu-entry>
  menu-entry make-menu-entry
  menu-entry?
  (label           menu-entry-label)
  (linux           menu-entry-linux)
  (linux-arguments menu-entry-linux-arguments
                   (default '()))
  (initrd          menu-entry-initrd))

(define* (grub-configuration-file store entries
                                  #:key (default-entry 1) (timeout 5)
                                  (system (%current-system)))
  "Return the GRUB configuration file in STORE for ENTRIES, a list of
<menu-entry> objects, defaulting to DEFAULT-ENTRY and with the given TIMEOUT."
  (define prologue
    (format #f "
set default=~a
set timeout=~a
search.file ~a~%"
            default-entry timeout
            (any (match-lambda
                  (($ <menu-entry> _ linux)
                   (let* ((drv (package-derivation store linux system))
                          (out (derivation-path->output-path drv)))
                     (string-append out "/bzImage"))))
                 entries)))

  (define entry->text
    (match-lambda
     (($ <menu-entry> label linux arguments initrd)
      (let ((linux-drv  (package-derivation store linux system))
            (initrd-drv (package-derivation store initrd system)))
        ;; XXX: Assume that INITRD is a directory containing an 'initrd' file.
        (format #f "menuentry ~s {
  linux ~a/bzImage ~a
  initrd ~a/initrd
}~%"
                label
                (derivation-path->output-path linux-drv)
                (string-join arguments)
                (derivation-path->output-path initrd-drv))))))

  (add-text-to-store store "grub.cfg"
                     (string-append prologue
                                    (string-concatenate
                                     (map entry->text entries)))
                     '()))
