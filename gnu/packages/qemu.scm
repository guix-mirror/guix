;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:select (gpl2 lgpl2.1+))
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define (qemu-patch commit file-name sha256)
  "Return an origin for COMMIT."
  (origin
    (method url-fetch)
    (uri (string-append
          "http://git.qemu.org/?p=qemu.git;a=commitdiff_plain;h="
          commit))
    (sha256 sha256)
    (file-name file-name)))

(define-public qemu
  (package
    (name "qemu")
    (version "2.6.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://wiki.qemu-project.org/download/qemu-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1v1lhhd6m59hqgmiz100g779rjq70pik5v4b3g936ci73djlmb69"))))
    (build-system gnu-build-system)
    (arguments
     '(;; Running tests in parallel can occasionally lead to failures, like:
       ;; boot_sector_test: assertion failed (signature == SIGNATURE): (0x00000000 == 0x0000dead)
       #:parallel-tests? #f

       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs (configure-flags '())
                           #:allow-other-keys)
             ;; The `configure' script doesn't understand some of the
             ;; GNU options.  Thus, add a new phase that's compatible.
             (let ((out (assoc-ref outputs "out")))
               (setenv "SHELL" (which "bash"))

               ;; While we're at it, patch for tests.
               (substitute* "tests/libqtest.c"
                 (("/bin/sh") (which "sh")))

               ;; The binaries need to be linked against -lrt.
               (setenv "LDFLAGS" "-lrt")
               (zero?
                (apply system*
                       `("./configure"
                         ,(string-append "--cc=" (which "gcc"))
                         "--disable-debug-info" ; save build space
                         "--enable-virtfs"      ; just to be sure
                         ,(string-append "--prefix=" out)
                         ,@configure-flags))))))
         (add-after 'install 'install-info
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Install the Info manual, unless Texinfo is missing.
             (or (not (assoc-ref inputs "texinfo"))
                 (let ((out (assoc-ref outputs "out")))
                   (and (zero? (system* "make" "info"))
                        (let ((infodir (string-append out "/share/info")))
                          (mkdir-p infodir)
                          (for-each (lambda (info)
                                      (install-file info infodir))
                                    (find-files "." "\\.info$"))
                          #t))))))
         (add-before 'check 'make-gtester-verbose
           (lambda _
             ;; Make GTester verbose to facilitate investigation upon failure.
             (setenv "V" "1")))
         (add-before 'check 'disable-test-qga
           (lambda _
             (substitute* "tests/Makefile"
               ;; Comment out the test-qga test, which needs /sys and
               ;; fails within the build environment.
               (("check-unit-.* tests/test-qga" all)
                (string-append "# " all)))
             #t)))))
    (inputs                                       ; TODO: Add optional inputs.
     `(("alsa-lib" ,alsa-lib)
       ("attr" ,attr)
       ("glib" ,glib)
       ;; ("libaio" ,libaio)
       ("libattr" ,attr)
       ("libcap" ,libcap)           ; virtfs support requires libcap & libattr
       ("libjpeg" ,libjpeg-8)
       ("libpng" ,libpng)
       ("libusb" ,libusb)                         ;USB pass-through support
       ("mesa" ,mesa)
       ("ncurses" ,ncurses)
       ;; ("pciutils" ,pciutils)
       ("pixman" ,pixman)
       ("sdl" ,sdl)
       ("spice" ,spice)
       ("util-linux" ,util-linux)
       ;; ("vde2" ,vde2)
       ("virglrenderer" ,virglrenderer)
       ("zlib" ,zlib)))
    (native-inputs `(("glib:bin" ,glib "bin") ; gtester, etc.
                     ("perl" ,perl)
                     ("pkg-config" ,pkg-config)
                     ("python" ,python-2) ; incompatible with Python 3 according to error message
                     ("texinfo" ,texinfo)))
    (home-page "http://www.qemu-project.org")
    (synopsis "Machine emulator and virtualizer")
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
    (license gpl2)

    ;; Several tests fail on MIPS; see <http://hydra.gnu.org/build/117914>.
    (supported-systems (delete "mips64el-linux" %supported-systems))))

(define-public qemu-minimal
  ;; QEMU without GUI support.
  (package (inherit qemu)
    (name "qemu-minimal")
    (synopsis "Machine emulator and virtualizer (without GUI)")
    (arguments
     `(#:configure-flags
       ;; Restrict to the targets supported by Guix.
       '("--target-list=i386-softmmu,x86_64-softmmu,mips64el-softmmu,arm-softmmu")
       ,@(package-arguments qemu)))

    ;; Remove dependencies on optional libraries, notably GUI libraries.
    (inputs (fold alist-delete (package-inputs qemu)
                  '("libusb" "mesa" "sdl" "spice" "virglrenderer")))))

(define-public libvirt
  (package
    (name "libvirt")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://libvirt.org/sources/libvirt-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0sriasjc573c519yqw1hcfb3qqjcsm9hm8vayw0anwkl6di9ay8s"))))
    (build-system gnu-build-system)
    (arguments
     `(;; FAIL: virshtest
       ;; FAIL: virfirewalltest
       ;; FAIL: virkmodtest
       ;; FAIL: virnetsockettest
       ;; FAIL: networkxml2firewalltest
       ;; FAIL: nwfilterebiptablestest
       ;; FAIL: nwfilterxml2firewalltest
       ;; Times out after PASS: virsh-vcpupin
       #:tests? #f
       #:configure-flags
       (list "--with-polkit"
             "--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             (substitute* '("tests/commandtest.c"
                            "gnulib/tests/test-posix_spawn1.c"
                            "gnulib/tests/test-posix_spawn2.c")
               (("/bin/sh") (which "sh")))
             #t))
         (add-after 'unpack 'do-not-mkdir-in-/var
           ;; Since the localstatedir should be /var at runtime, we must
           ;; prevent writing to /var at installation time.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out           (assoc-ref outputs "out"))
                    (localstatedir (string-append out "/var")))
               (substitute* '("src/Makefile.in"
                              "daemon/Makefile.in")
                 (("\\$\\(DESTDIR\\)\\$\\(localstatedir)") localstatedir)))
             #t)))))
    (inputs
     `(("libxml2" ,libxml2)
       ("gnutls" ,gnutls)
       ("dbus" ,dbus)
       ("qemu" ,qemu)
       ("polkit" ,polkit)
       ("libpcap" ,libpcap)
       ("libnl" ,libnl)
       ("libuuid" ,util-linux)
       ("lvm2" ,lvm2) ; for libdevmapper
       ("curl" ,curl)
       ("openssl" ,openssl)
       ("cyrus-sasl" ,cyrus-sasl)
       ("perl" ,perl)
       ("python" ,python-2)
       ("libyajl" ,libyajl)
       ("audit" ,audit)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://libvirt.org")
    (synopsis "Simple API for virtualization")
    (description "Libvirt is a C toolkit to interact with the virtualization
capabilities of recent versions of Linux.  The library aims at providing long
term stable C API initially for the Xen paravirtualization but should be able
to integrate other virtualization mechanisms if needed.")
    (license lgpl2.1+)))
