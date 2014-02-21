;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages qemu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:select (gpl2))
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libpng)
  #:use-module (gnu packages libjpeg)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages perl))

(define-public qemu-headless
  ;; This is QEMU without GUI support.
  (package
    (name "qemu-headless")
    (version "1.6.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://wiki.qemu-project.org/download/qemu-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "152jc18mjs543k8ggbcwgra8d0zw81z0lcc1r0iq4iqhm926ywzw"))
             (patches (list (search-patch "qemu-make-4.0.patch")))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-replace
                 'configure
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   ;; The `configure' script doesn't understand some of the
                   ;; GNU options.  Thus, add a new phase that's compatible.
                   (let ((out   (assoc-ref outputs "out"))
                         (samba (assoc-ref inputs "samba")))
                     (setenv "SHELL" (which "bash"))

                     ;; While we're at it, patch for tests.
                     (substitute* "tests/libqtest.c"
                       (("/bin/sh") (which "sh")))

                     ;; The binaries need to be linked against -lrt.
                     (setenv "LDFLAGS" "-lrt")
                     (zero?
                      (system* "./configure"
                               (string-append "--cc=" (which "gcc"))
                               "--disable-debug-info" ; save build space
                               (string-append "--prefix=" out)
                               (string-append "--smbd=" samba
                                              "/sbin/smbd")))))
                 (alist-cons-after
                  'install 'install-info
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    ;; Install the Info manual, unless Texinfo is missing.
                    (or (not (assoc-ref inputs "texinfo"))
                        (let ((out (assoc-ref outputs "out")))
                          (and (zero? (system* "make" "info"))
                               (let ((infodir (string-append out "/share/info")))
                                 (mkdir-p infodir)
                                 (for-each (lambda (info)
                                             (copy-file
                                              info
                                              (string-append infodir "/" info)))
                                           (find-files "." "\\.info$"))
                                 #t)))))
                  %standard-phases))))

    (inputs                                       ; TODO: Add optional inputs.
     `(;; ("libaio" ,libaio)
       ("glib" ,glib)
       ("ncurses" ,ncurses)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg-8)
       ("pixman" ,pixman)
       ;; ("vde2" ,vde2)
       ("util-linux" ,util-linux)
       ;; ("pciutils" ,pciutils)
       ("alsa-lib" ,alsa-lib)
       ("zlib" ,zlib)
       ("attr" ,attr)
       ("samba" ,samba)))                         ; an optional dependency
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("python" ,python-2) ; incompatible with Python 3 according to error message
                     ("texinfo" ,texinfo)
                     ("perl" ,perl)))
    (home-page "http://www.qemu-project.org")
    (synopsis "Machine emulator and virtualizer (without GUI)")
    (description
     "QEMU is a generic machine emulator and virtualizer.

When used as a machine emulator, QEMU can run OSes and programs made for one
machine (e.g. an ARM board) on a different machine---e.g., your own PC.  By
using dynamic translation, it achieves very good performance.

When used as a virtualizer, QEMU achieves near native performances by
executing the guest code directly on the host CPU.  QEMU supports
virtualization when executing under the Xen hypervisor or using
the KVM kernel module in Linux.  When using KVM, QEMU can virtualize x86,
server and embedded PowerPC, and S390 guests.")

    ;; Many files are GPLv2+, but some are GPLv2-only---e.g., `memory.c'.
    (license gpl2)))

(define-public qemu/smb-shares
  ;; A patched QEMU where `-net smb' yields two shares instead of one: one for
  ;; the store, and another one for exchanges with the host.

  ;; TODO: Use 9p/-virtfs instead of this SMB hack:
  ;; <http://wiki.qemu.org/Documentation/9psetup>.
  (package (inherit qemu-headless)
    (name "qemu-with-multiple-smb-shares")
    (source (origin (inherit (package-source qemu-headless))
              (patches
               (cons (search-patch "qemu-multiple-smb-shares.patch")
                     (origin-patches (package-source qemu-headless))))))))

(define-public qemu
  ;; QEMU with GUI support.
  (package (inherit qemu-headless)
    (name "qemu")
    (synopsis "Machine emulator and virtualizer")
    (inputs `(("sdl" ,sdl)
              ("mesa" ,mesa)
              ,@(package-inputs qemu-headless)))))
