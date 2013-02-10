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

(define-module (gnu packages qemu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:select (gpl2))
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libpng)
  #:use-module (gnu packages libjpeg)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl))

(define-public qemu-kvm
  (package
    (name "qemu-kvm")
    (version "1.2.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/kvm/qemu-kvm/"
                                 version "/qemu-kvm-" version ".tar.gz"))
             (sha256
              (base32
               "018vb5nmk2fsm143bs2bl2wirhasd4b10d7jchl32zik4inbk2p9"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-replace
                 'configure
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   ;; The `configure' script doesn't understand some of the
                   ;; GNU options.  Thus, add a new phase that's compatible.
                   (let ((out (assoc-ref outputs "out")))
                     (setenv "SHELL" (which "bash"))

                     ;; The binaries need to be linked against -lrt.
                     (setenv "LDFLAGS" "-lrt")
                     (zero?
                      (system* "./configure"
                               (string-append "--prefix=" out)))))
                 %standard-phases)))
    (inputs                                       ; TODO: Add optional inputs.
     `(;; ("mesa" ,mesa)
       ;; ("libaio" ,libaio)
       ("glib" ,glib)
       ("python" ,python)
       ("ncurses" ,ncurses)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg-8)
       ;; ("vde2" ,vde2)
       ("util-linux" ,util-linux)
       ;; ("pciutils" ,pciutils)
       ("pkg-config" ,pkg-config)
       ;; ("alsa-lib" ,alsa-lib)
       ;; ("SDL" ,SDL)
       ("zlib" ,zlib)
       ("attr" ,attr)))
    (home-page "http://www.linux-kvm.org/")
    (synopsis
     "Virtualization for Linux on x86 hardware containing virtualization extensions")
    (description
     "KVM (for Kernel-based Virtual Machine) is a full virtualization solution
for Linux on x86 hardware containing virtualization extensions (Intel VT or
AMD-V).  It consists of a loadable kernel module, kvm.ko, that provides the
core virtualization infrastructure and a processor specific module,
kvm-intel.ko or kvm-amd.ko. KVM also requires a modified QEMU although work is
underway to get the required changes upstream.")

    ;; Many files are GPLv2+, but some are GPLv2-only---e.g., `memory.c'.
    (license gpl2)))

(define-public qemu
  ;; The real one, with a complete target list.
  (package (inherit qemu-kvm)
    (name "qemu")
    (version "1.3.1")
    (location (source-properties->location (current-source-location)))
    (source (origin
             (method url-fetch)
             (uri (string-append "http://wiki.qemu-project.org/download/qemu-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1bqfrb5dlsxm8gxhkksz8qzi5fhj3xqhxyfwbqcphhcv1kpyfwip"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qemu-kvm)
       ((#:phases phases)
        `(alist-cons-before
          'build 'pre-build
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((libtool    (assoc-ref inputs "libtool"))
                  (pkg-config (assoc-ref inputs "pkg-config")))
              ;; XXX: For lack of generic search path handling.
              (setenv "ACLOCAL_PATH"
                      (format #f "~a/share/aclocal:~a/share/aclocal"
                              libtool pkg-config)))

            ;; For pixman's `configure' script.
            (setenv "CONFIG_SHELL" (which "bash"))

            (substitute* "pixman/configure.ac"
              (("AM_CONFIG_HEADER") "AC_CONFIG_HEADERS")))
          ,phases))))
    (native-inputs `(("autoconf" ,autoconf-wrapper) ; for "pixman"
                     ("automake" ,automake)
                     ("libtool" ,libtool)
                     ("libtool-bin" ,libtool "bin")
                     ("perl" ,perl)))
    (description
     "QEMU is a generic and open source machine emulator and virtualizer.

When used as a machine emulator, QEMU can run OSes and programs made for one
machine (e.g. an ARM board) on a different machine
(e.g. your own PC).  By using dynamic translation, it achieves very good
performance.

When used as a virtualizer, QEMU achieves near native performances by
executing the guest code directly on the host CPU.  QEMU supports
virtualization when executing under the Xen hypervisor or using
the KVM kernel module in Linux.  When using KVM, QEMU can virtualize x86,
server and embedded PowerPC, and S390 guests.")))
