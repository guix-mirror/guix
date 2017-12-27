;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages virtualization)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:select (gpl2 gpl2+ gpl3+ lgpl2.1 lgpl2.1+))
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
    (version "2.10.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qemu.org/qemu-"
                                 version ".tar.xz"))
             (patches (search-patches "qemu-CVE-2017-15038.patch"
                                      "qemu-CVE-2017-15289.patch"))
             (sha256
              (base32
               "17w21spvaxaidi2am5lpsln8yjpyp2zi3s3gc6nsxj5arlgamzgw"))))
    (build-system gnu-build-system)
    (arguments
     '(;; Running tests in parallel can occasionally lead to failures, like:
       ;; boot_sector_test: assertion failed (signature == SIGNATURE): (0x00000000 == 0x0000dead)
       #:parallel-tests? #f
       #:configure-flags '("--enable-usb-redir" "--enable-opengl")
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
                         ;; Some architectures insist on using HOST_CC
                         ,(string-append "--host-cc=" (which "gcc"))
                         "--disable-debug-info" ; save build space
                         "--enable-virtfs"      ; just to be sure
                         ,(string-append "--prefix=" out)
                         ,(string-append "--sysconfdir=/etc")
                         ,@configure-flags))))))
         (add-after 'install 'install-info
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Install the Info manual, unless Texinfo is missing.
             (or (not (assoc-ref inputs "texinfo"))
                 (let ((out (assoc-ref outputs "out")))
                   (and (zero? (system* "make" "info"))
                        (let ((infodir (string-append out "/share/info")))
                          (for-each (lambda (info)
                                      (install-file info infodir))
                                    (find-files "." "\\.info"))
                          #t))))))
         (add-before 'check 'make-gtester-verbose
           (lambda _
             ;; Make GTester verbose to facilitate investigation upon failure.
             (setenv "V" "1") #t))
         (add-before 'check 'disable-test-qga
           (lambda _
             (substitute* "tests/Makefile.include"
               ;; Comment out the test-qga test, which needs /sys and
               ;; fails within the build environment.
               (("check-unit-.* tests/test-qga" all)
                (string-append "# " all)))
             #t)))))
    (inputs                                       ; TODO: Add optional inputs.
     `(("alsa-lib" ,alsa-lib)
       ("attr" ,attr)
       ("glib" ,glib)
       ("libaio" ,libaio)
       ("libattr" ,attr)
       ("libcap" ,libcap)           ; virtfs support requires libcap & libattr
       ("libdrm" ,libdrm)
       ("libepoxy" ,libepoxy)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libseccomp" ,libseccomp)
       ("libusb" ,libusb)                         ;USB pass-through support
       ("mesa" ,mesa)
       ("ncurses" ,ncurses)
       ;; ("pciutils" ,pciutils)
       ("pixman" ,pixman)
       ("sdl" ,sdl)
       ("spice" ,spice)
       ("usbredir" ,usbredir)
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
     (substitute-keyword-arguments (package-arguments qemu)
       ((#:configure-flags _ '(list))
        ;; Restrict to the targets supported by Guix.
        ''("--target-list=i386-softmmu,x86_64-softmmu,mips64el-softmmu,arm-softmmu,aarch64-softmmu"))))

    ;; Remove dependencies on optional libraries, notably GUI libraries.
    (inputs (fold alist-delete (package-inputs qemu)
                  '("libusb" "mesa" "sdl" "spice" "virglrenderer"
                    "usbredir" "libdrm" "libepoxy")))))

(define-public libosinfo
  (package
    (name "libosinfo")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://releases.pagure.org/libosinfo/libosinfo-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0srrs2m6irqd4f867g8ls6jp2dq3ql0l9d0fh80d55sivvn2bd7p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-usb-ids-path="
                            (assoc-ref %build-inputs "usb.ids"))
             (string-append "--with-pci-ids-path="
                            (assoc-ref %build-inputs "pci.ids")))
       #:phases
       (modify-phases %standard-phases
         ;; This odd test fails for unknown reasons.
         (add-after 'unpack 'disable-broken-test
           (lambda _
             (substitute* "test/Makefile.in"
               (("test-isodetect\\$\\(EXEEXT\\)") ""))
             #t)))))
    (inputs
     `(("libsoup" ,libsoup)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("gobject-introspection" ,gobject-introspection)))
    (native-inputs
     `(("check" ,check)
       ("glib" ,glib "bin")  ; glib-mkenums, etc.
       ("gtk-doc" ,gtk-doc)
       ("vala" ,vala)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("pci.ids"
        ,(origin
           (method url-fetch)
           (uri "https://github.com/pciutils/pciids/raw/ad02084f0bc143e3c15e31a6152a3dfb1d7a3156/pci.ids")
           (sha256
            (base32
             "0kfhpj5rnh24hz2714qhfmxk281vwc2w50sm73ggw5d15af7zfsw"))))
       ("usb.ids"
        ,(origin
           (method url-fetch)
           (uri "http://linux-usb.cvs.sourceforge.net/viewvc/linux-usb/htdocs/usb.ids?revision=1.551")
           (file-name "usb.ids")
           (sha256
            (base32
             "17rg5i0wbyk289gr8v4kgvnc9q5bidz7ldcvv9x58l083wn16hq3"))))))
    (home-page "https://libosinfo.org/")
    (synopsis "Operating system information database")
    (description "libosinfo is a GObject based library API for managing
information about operating systems, hypervisors and the (virtual) hardware
devices they can support.  It includes a database containing device metadata
and provides APIs to match/identify optimal devices for deploying an operating
system on a hypervisor.  Via GObject Introspection, the API is available in
all common programming languages.  Vala bindings are also provided.")
    ;; The library files are released under LGPLv2.1 or later; the source
    ;; files in the "tools" directory are released under GPLv2+.
    (license (list lgpl2.1+ gpl2+))))

(define-public lxc
  (package
    (name "lxc")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://linuxcontainers.org/downloads/lxc/lxc-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1xpghrinxhm2072fwmn42pxhjwh7qx6cbsipw4s6g38a8mkklrk8"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("gnutls" ,gnutls)
       ("libcap" ,libcap)
       ("libseccomp" ,libseccomp)
       ("libselinux" ,libselinux)))
    (arguments
     '(#:configure-flags
       '("--sysconfdir=/etc"
         "--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out         (assoc-ref outputs "out"))
                    (bashcompdir (string-append out "/etc/bash_completion.d")))
               (zero? (system*
                       "make" "install"
                       (string-append "bashcompdir=" bashcompdir)
                       ;; Don't install files into /var and /etc.
                       "LXCPATH=/tmp/var/lib/lxc"
                       "localstatedir=/tmp/var"
                       "sysconfdir=/tmp/etc"
                       "sysconfigdir=/tmp/etc/default"))))))))
    (synopsis "Linux container tools")
    (home-page "https://linuxcontainers.org/")
    (description
     "LXC is a userspace interface for the Linux kernel containment features.
Through a powerful API and simple tools, it lets Linux users easily create and
manage system or application containers.")
    (license lgpl2.1+)))

(define-public libvirt
  (package
    (name "libvirt")
    (version "3.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://libvirt.org/sources/libvirt-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "03kb37iv3dvvdlslznlc0njvjpmq082lczmsslz5p4fcwb50kwfz"))))
    (build-system gnu-build-system)
    (arguments
     `(;; FAIL: virshtest
       ;; FAIL: virfirewalltest
       ;; FAIL: virkmodtest
       ;; FAIL: virnetsockettest
       ;; FAIL: networkxml2firewalltest
       ;; FAIL: nwfilterebiptablestest
       ;; FAIL: nwfilterxml2firewalltest
       ;; Times while running commandest.
       #:tests? #f
       #:configure-flags
       (list "--with-polkit"
             "--sysconfdir=/etc"
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
         (replace 'install
           ;; Since the sysconfdir and localstatedir should be /etc and /var
           ;; at runtime, we must prevent writing to them at installation
           ;; time.
           (lambda _
             (zero? (system* "make" "install"
                             "sysconfdir=/tmp/etc"
                             "localstatedir=/tmp/var"))))
         (add-after 'install 'wrap-libvirtd
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/sbin/libvirtd")
                 `("PATH" = (,(string-append (assoc-ref inputs "iproute")
                                             "/sbin")
                             ,(string-append (assoc-ref inputs "qemu")
                                             "/bin"))))
               #t))))))
    (inputs
     `(("libxml2" ,libxml2)
       ("gnutls" ,gnutls)
       ("dbus" ,dbus)
       ("qemu" ,qemu)
       ("libpcap" ,libpcap)
       ("libnl" ,libnl)
       ("libuuid" ,util-linux)
       ("lvm2" ,lvm2) ; for libdevmapper
       ("curl" ,curl)
       ("openssl" ,openssl)
       ("cyrus-sasl" ,cyrus-sasl)
       ("libyajl" ,libyajl)
       ("audit" ,audit)
       ("dmidecode" ,dmidecode)
       ("dnsmasq" ,dnsmasq)
       ("ebtables" ,ebtables)
       ("iproute" ,iproute)
       ("iptables" ,iptables)))
    (native-inputs
     `(("xsltproc" ,libxslt)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("polkit" ,polkit)
       ("python" ,python-2)))
    (home-page "https://libvirt.org")
    (synopsis "Simple API for virtualization")
    (description "Libvirt is a C toolkit to interact with the virtualization
capabilities of recent versions of Linux.  The library aims at providing long
term stable C API initially for the Xen paravirtualization but should be able
to integrate other virtualization mechanisms if needed.")
    (license lgpl2.1+)))

(define-public libvirt-glib
  (package
    (name "libvirt-glib")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://libvirt.org/libvirt/glib/"
                                  "libvirt-glib-" version ".tar.gz"))
              (sha256
               (base32
                "0iwa5sdbii52pjpdm5j37f67sdmf0kpcky4liwhy1nf43k85i4fa"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             (substitute* "tests/test-events.c"
               (("/bin/true") (which "true")))
             #t)))))
    (inputs
     `(("libxml2" ,libxml2)
       ("libvirt" ,libvirt)
       ("gobject-introspection" ,gobject-introspection)
       ("glib" ,glib)
       ("openssl" ,openssl)
       ("cyrus-sasl" ,cyrus-sasl)
       ("lvm2" ,lvm2) ; for libdevmapper
       ("libyajl" ,libyajl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("glib" ,glib "bin")
       ("vala" ,vala)))
    (home-page "https://libvirt.org")
    (synopsis "GLib wrapper around libvirt")
    (description "libvirt-glib wraps the libvirt library to provide a
high-level object-oriented API better suited for glib-based applications, via
three libraries:

@enumerate
@item libvirt-glib - GLib main loop integration & misc helper APIs
@item libvirt-gconfig - GObjects for manipulating libvirt XML documents
@item libvirt-gobject - GObjects for managing libvirt objects
@end enumerate
")
    (license lgpl2.1+)))

(define-public python-libvirt
  (package
    (name "python-libvirt")
    (version "3.7.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "libvirt-python" version))
              (sha256
               (base32
                "0vy0ai8z88yhzqfk1n08z1gda5flrqxcw9lg1012b3zg125qljhy"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-nosetests-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "setup.py"
               (("\"/usr/bin/nosetests\"")
                (string-append "\"" (which "nosetests") "\""))
               (("self\\.spawn\\(\\[sys\\.executable, nose\\]\\)")
                (format #f "self.spawn([\"~a\", nose])" (which "bash"))))
             #t)))))
    (inputs
     `(("libvirt" ,libvirt)))
    (propagated-inputs
     `(("python-lxml" ,python-lxml)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-nose" ,python-nose)))
    (home-page "https://libvirt.org")
    (synopsis "Python bindings to libvirt")
    (description "This package provides Python bindings to the libvirt
virtualization library.")
    (license lgpl2.1+)))

(define-public python2-libvirt
  (package-with-python2 python-libvirt))

(define-public virt-manager
  (package
    (name "virt-manager")
    (version "1.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://virt-manager.org/download/sources"
                                  "/virt-manager/virt-manager-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "093azs8p4p7y4nf5j25xpsvdxww7gky1g0hs8mkcvmpxl2wjd0jj"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:use-setuptools? #f ; Uses custom distutils 'install' command.
       ;; Some of the tests seem to require network access to install virtual
       ;; machines.
       #:tests? #f
       #:imported-modules ((guix build glib-or-gtk-build-system)
                           ,@%python-build-system-modules)
       #:modules ((ice-9 match)
                  (srfi srfi-26)
                  (guix build python-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-setup
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "virtcli/cliconfig.py"
               (("/usr") (assoc-ref outputs "out")))
             #t))
         (add-after 'unpack 'fix-default-uri
           (lambda* (#:key inputs #:allow-other-keys)
             ;; xen is not available for now - so only patch qemu
             (substitute* "virtManager/connect.py"
               (("/usr(/bin/qemu-system)" _ suffix)
                (string-append (assoc-ref inputs "qemu") suffix)))
             #t))
         (add-before 'wrap 'wrap-with-GI_TYPELIB_PATH
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((bin       (string-append (assoc-ref outputs "out") "/bin"))
                    (bin-files (find-files bin ".*"))
                    (paths     (map (match-lambda
                                      ((output . directory)
                                       (let* ((girepodir (string-append
                                                          directory
                                                          "/lib/girepository-1.0")))
                                         (if (file-exists? girepodir)
                                             girepodir #f))))
                                    inputs)))
               (for-each (lambda (file)
                           (format #t "wrapping ~a\n" file)
                           (wrap-program file
                             `("GI_TYPELIB_PATH" ":" prefix
                               ,(filter identity paths))))
                         bin-files))
             #t))
         (add-after 'install 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'install 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (inputs
     `(("dconf" ,dconf)
       ("gtk+" ,gtk+)
       ("gtk-vnc" ,gtk-vnc)
       ("libvirt" ,libvirt)
       ("libvirt-glib" ,libvirt-glib)
       ("libosinfo" ,libosinfo)
       ("vte" ,vte)
       ("gobject-introspection" ,gobject-introspection)
       ("python2-libvirt" ,python2-libvirt)
       ("python2-requests" ,python2-requests)
       ("python2-ipaddr" ,python2-ipaddr)
       ("python2-pycairo" ,python2-pycairo)
       ("python2-pygobject" ,python2-pygobject)
       ("python2-libxml2" ,python2-libxml2)
       ("spice-gtk" ,spice-gtk)))
    ;; virt-manager searches for qemu-img or kvm-img in the PATH.
    (propagated-inputs
     `(("qemu" ,qemu)))
    (native-inputs
     `(("glib" ,glib "bin")             ; glib-compile-schemas.
       ("gtk+" ,gtk+ "bin")             ; gtk-update-icon-cache
       ("perl" ,perl)                   ; pod2man
       ("intltool" ,intltool)))
    (home-page "https://virt-manager.org/")
    (synopsis "Manage virtual machines")
    (description
     "The virt-manager application is a desktop user interface for managing
virtual machines through libvirt.  It primarily targets KVM VMs, but also
manages Xen and LXC (Linux containers).  It presents a summary view of running
domains, their live performance and resource utilization statistics.")
    (license gpl2+)))

(define-public criu
  (package
    (name "criu")
    (version "3.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.openvz.org/criu/criu-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1w0ybla7ac0ql0jzh0vxdf2w9amqp88jcg0na3b33r3hq8acry6x"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:tests? #f ; tests require mounting as root
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "LIBDIR=" (assoc-ref %outputs "out")
                            "/lib"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The includes for libnl are located in a sub-directory.
             (setenv "C_INCLUDE_PATH"
                     (string-append (assoc-ref inputs "libnl")
                                    "/include/libnl3:"
                                    (getenv "C_INCLUDE_PATH")))
             ;; Prevent xmlto from failing the install phase.
             (substitute* "Documentation/Makefile"
               (("XMLTO.*:=.*")
                (string-append "XMLTO:="
                               (assoc-ref inputs "xmlto")
                               "/bin/xmlto"
                               " --skip-validation "
                               " -x "
                               (assoc-ref inputs "docbook-xsl")
                               "/xml/xsl/docbook-xsl-"
                               ,(package-version docbook-xsl)
                               "/manpages/docbook.xsl")))
             #t))
         (add-before 'build 'fix-symlink
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The file 'images/google/protobuf/descriptor.proto' points to
             ;; /usr/include/..., which obviously does not exist.
             (let* ((file "google/protobuf/descriptor.proto")
                    (target (string-append "images/" file))
                    (source (string-append (assoc-ref inputs "protobuf")
                                           "/include/" file)))
               (delete-file target)
               (symlink source target)
               #t)))
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make sure 'crit' runs with the correct PYTHONPATH.
             (let* ((out (assoc-ref outputs "out"))
                    (path (string-append out
                                         "/lib/python"
                                         (string-take (string-take-right
                                                       (assoc-ref inputs "python") 5) 3)
                                         "/site-packages:"
                                         (getenv "PYTHONPATH"))))
               (wrap-program (string-append out "/bin/crit")
                 `("PYTHONPATH" ":" prefix (,path))))
             #t)))))
    (inputs
     `(("protobuf" ,protobuf)
       ("python" ,python-2)
       ("python2-protobuf" ,python2-protobuf)
       ("python2-ipaddr" ,python2-ipaddr)
       ("iproute" ,iproute)
       ("libaio" ,libaio)
       ("libcap" ,libcap)
       ("libnet" ,libnet)
       ("libnl" ,libnl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("perl" ,perl)
       ("protobuf-c" ,protobuf-c)
       ("asciidoc" ,asciidoc)
       ("xmlto" ,xmlto)
       ("docbook-xml" ,docbook-xml)
       ("docbook-xsl" ,docbook-xsl)))
    (home-page "https://criu.org")
    (synopsis "Checkpoint and restore in user space")
    (description "Using this tool, you can freeze a running application (or
part of it) and checkpoint it to a hard drive as a collection of files.  You
can then use the files to restore and run the application from the point it
was frozen at.  The distinctive feature of the CRIU project is that it is
mainly implemented in user space.")
    ;; The project is licensed under GPLv2; files in the lib/ directory are
    ;; LGPLv2.1.
    (license (list gpl2 lgpl2.1))))

(define-public qmpbackup
  (package
    (name "qmpbackup")
    (version "0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/abbbi/qmpbackup/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "10k9mnb1yrg4gw1rvz4kw4dxc4aajl8gnjrpm3axqkg63qmxj3qn"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (home-page "https://github.com/abbbi/qmpbackup")
    (synopsis "Backup and restore QEMU machines")
    (description "qmpbackup is designed to create and restore full and
incremental backups of running QEMU virtual machines via QMP, the QEMU
Machine Protocol.")
    (license gpl3+)))

(define-public lookingglass
  (package
   (name "lookingglass")
   (version "a9")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/gnif/LookingGlass/archive/"
                         version ".tar.gz"))
     (file-name (string-append name "-" version))
     (sha256
      (base32
       "015chy4x94x4dd5831d7n0gada8rhahmdx7bdbdhajlzivi3kjcw"))))
   (build-system gnu-build-system)
   (inputs `(("fontconfig" ,fontconfig)
             ("glu" ,glu)
             ("mesa" ,mesa)
             ("openssl" ,openssl)
             ("sdl2" ,sdl2)
             ("sdl2-ttf" ,sdl2-ttf)
             ("spice-protocol" ,spice-protocol)))
   (native-inputs `(("pkg-config", pkg-config)))
   (arguments
    `(#:tests? #f ;; No tests are available.
      #:phases (modify-phases %standard-phases
                 (replace 'configure
                   (lambda* (#:key outputs #:allow-other-keys)
                     (chdir "client")
                     #t))
                 (replace 'install
                   (lambda* (#:key outputs #:allow-other-keys)
                     (install-file "bin/looking-glass-client"
                                   (string-append (assoc-ref outputs "out")
                                                  "/bin"))
                     #t)))))
   (home-page "https://looking-glass.hostfission.com")
   (synopsis "KVM Frame Relay (KVMFR) implementation")
   (description "Looking Glass allows the use of a KVM (Kernel-based Virtual
Machine) configured for VGA PCI Pass-through without an attached physical
monitor, keyboard or mouse.  It displays the VM's rendered contents on your main
monitor/GPU.")
   ;; This package requires SSE instructions.
   (supported-systems '("i686-linux" "x86_64-linux"))
   (license gpl2+)))
