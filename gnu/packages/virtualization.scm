;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017, 2018. 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019 Guy Fleury Iteriteka <hoonandon@gmail.com>
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
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages figlet)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
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
    (version "3.1.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qemu.org/qemu-"
                                 version ".tar.xz"))
             (patches (search-patches "qemu-CVE-2018-16872.patch"
                                      "qemu-CVE-2019-6778.patch"))
             (sha256
              (base32
               "1z5bd5nfyjvhfi1s95labc82y4hjdjjkdabw931362ls0zghh1ba"))))
    (build-system gnu-build-system)
    (arguments
     '(;; Running tests in parallel can occasionally lead to failures, like:
       ;; boot_sector_test: assertion failed (signature == SIGNATURE): (0x00000000 == 0x0000dead)
       #:parallel-tests? #f
       #:configure-flags (list "--enable-usb-redir" "--enable-opengl"
                               (string-append "--smbd="
                                              (assoc-ref %outputs "out")
                                              "/libexec/samba-wrapper")
                               "--audio-drv-list=alsa,pa,sdl")
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
               (apply invoke
                      `("./configure"
                        ,(string-append "--cc=" (which "gcc"))
                        ;; Some architectures insist on using HOST_CC
                        ,(string-append "--host-cc=" (which "gcc"))
                        "--disable-debug-info" ; save build space
                        "--enable-virtfs"      ; just to be sure
                        ,(string-append "--prefix=" out)
                        ,(string-append "--sysconfdir=/etc")
                        ,@configure-flags)))))
         (add-after 'install 'install-info
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Install the Info manual, unless Texinfo is missing.
             (when (assoc-ref inputs "texinfo")
               (let* ((out  (assoc-ref outputs "out"))
                      (dir (string-append out "/share/info")))
                 (invoke "make" "info")
                 (for-each (lambda (info)
                             (install-file info dir))
                           (find-files "." "\\.info"))))
             #t))
         ;; Create a wrapper for Samba. This allows QEMU to use Samba without
         ;; pulling it in as an input. Note that you need to explicitly install
         ;; Samba in your Guix profile for Samba support.
         (add-after 'install-info 'create-samba-wrapper
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out    (assoc-ref %outputs "out"))
                    (libexec (string-append out "/libexec")))
               (call-with-output-file "samba-wrapper"
                 (lambda (port)
                   (format port "#!/bin/sh
exec smbd $@")))
               (chmod "samba-wrapper" #o755)
               (install-file "samba-wrapper" libexec))
             #t))
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
       ("gtk+" ,gtk+)
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
       ("pulseaudio" ,pulseaudio)
       ("sdl2" ,sdl2)
       ("spice" ,spice)
       ("usbredir" ,usbredir)
       ("util-linux" ,util-linux)
       ;; ("vde2" ,vde2)
       ("virglrenderer" ,virglrenderer)
       ("zlib" ,zlib)))
    (native-inputs `(("gettext" ,gettext-minimal)
                     ("glib:bin" ,glib "bin") ; gtester, etc.
                     ("perl" ,perl)
                     ("flex" ,flex)
                     ("bison" ,bison)
                     ("pkg-config" ,pkg-config)
                     ("python-wrapper" ,python-wrapper)
                     ("texinfo" ,texinfo)))
    (home-page "https://www.qemu.org")
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
    (license license:gpl2)

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
    (native-inputs (fold alist-delete (package-native-inputs qemu)
                  '("gettext")))
    (inputs (fold alist-delete (package-inputs qemu)
                  '("libusb" "mesa" "sdl2" "spice" "virglrenderer" "gtk+"
                    "usbredir" "libdrm" "libepoxy" "pulseaudio")))))

;; The GRUB test suite fails with later versions of Qemu, so we
;; keep it at 2.10 for now.  See
;; <https://lists.gnu.org/archive/html/bug-grub/2018-02/msg00004.html>.
;; This package is hidden since we do not backport updates to it.
(define-public qemu-minimal-2.10
  (hidden-package
   (package
    (inherit qemu-minimal)
    (version "2.10.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qemu.org/qemu-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "17w21spvaxaidi2am5lpsln8yjpyp2zi3s3gc6nsxj5arlgamzgw"))
             (patches
              (search-patches "qemu-glibc-2.27.patch"))))
    ;; qemu-minimal-2.10 needs Python 2. Remove below once no longer necessary.
    (native-inputs `(("python-2" ,python-2)
                     ,@(fold alist-delete (package-native-inputs qemu)
                             '("python-wrapper")))))))

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
           (uri "https://svn.code.sf.net/p/linux-usb/repo/trunk/htdocs/usb.ids?r=2681")
           (file-name "usb.ids")
           (sha256
            (base32
             "1m6yhvz5k8aqzxgk7xj3jkk8frl1hbv0h3vgj4wbnvnx79qnvz3r"))))))
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
    (license (list license:lgpl2.1+ license:gpl2+))))

(define-public lxc
  (package
    (name "lxc")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://linuxcontainers.org/downloads/lxc/lxc-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1igxqgx8q9cp15mcp1y8j564bl85ijw04jcmgb1s5bmfbg1751sd"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("gnutls" ,gnutls)
       ("libcap" ,libcap)
       ("libseccomp" ,libseccomp)
       ("libselinux" ,libselinux)))
    (arguments
     `(#:configure-flags
       (list (string-append "--docdir=" (assoc-ref %outputs "out")
                            "/share/doc/" ,name "-" ,version)
             "--sysconfdir=/etc"
             "--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out         (assoc-ref outputs "out"))
                    (bashcompdir (string-append out "/etc/bash_completion.d")))
               (invoke "make" "install"
                       (string-append "bashcompdir=" bashcompdir)
                       ;; Don't install files into /var and /etc.
                       "LXCPATH=/tmp/var/lib/lxc"
                       "localstatedir=/tmp/var"
                       "sysconfdir=/tmp/etc"
                       "sysconfigdir=/tmp/etc/default")))))))
    (synopsis "Linux container tools")
    (home-page "https://linuxcontainers.org/")
    (description
     "LXC is a userspace interface for the Linux kernel containment features.
Through a powerful API and simple tools, it lets Linux users easily create and
manage system or application containers.")
    (license license:lgpl2.1+)))

(define-public libvirt
  (package
    (name "libvirt")
    (version "4.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://libvirt.org/sources/libvirt-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0v17zzyyb25nn9l18v5244myg7590dp6ppwgi8xysipifc0q77bz"))))
    (build-system gnu-build-system)
    (arguments
     `(;; FAIL: virshtest
       ;; FAIL: virfirewalltest
       ;; FAIL: virkmodtest
       ;; FAIL: virnetsockettest
       ;; FAIL: networkxml2firewalltest
       ;; FAIL: nwfilterebiptablestest
       ;; FAIL: nwfilterxml2firewalltest
       ;; Time-out while running commandtest.
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
             (invoke "make" "install"
                            "sysconfdir=/tmp/etc"
                            "localstatedir=/tmp/var")))
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
       ("eudev" ,eudev)
       ("libpciaccess" ,libpciaccess)
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
       ("python" ,python)))
    (home-page "https://libvirt.org")
    (synopsis "Simple API for virtualization")
    (description "Libvirt is a C toolkit to interact with the virtualization
capabilities of recent versions of Linux.  The library aims at providing long
term stable C API initially for the Xen paravirtualization but should be able
to integrate other virtualization mechanisms if needed.")
    (license license:lgpl2.1+)))

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
    (license license:lgpl2.1+)))

(define-public python-libvirt
  (package
    (name "python-libvirt")
    (version "4.10.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "libvirt-python" version))
              (sha256
               (base32
                "11fipj9naihgc9afc8bz5hi05xa1shp4qcy170sa18p3sl4zljb9"))))
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
    (license license:lgpl2.1+)))

(define-public python2-libvirt
  (package-with-python2 python-libvirt))

(define-public virt-manager
  (package
    (name "virt-manager")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://virt-manager.org/download/sources"
                                  "/virt-manager/virt-manager-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1m038kyngmxlgz91c7z8g73lb2wy0ajyah871a3g3wb5cnd0dsil"))))
    (build-system python-build-system)
    (arguments
     `(#:use-setuptools? #f          ; uses custom distutils 'install' command
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
             ;; Xen is not available for now - so only patch qemu.
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
       ("python-libvirt" ,python-libvirt)
       ("python-requests" ,python-requests)
       ("python-ipaddress" ,python-ipaddress)
       ("python-pycairo" ,python-pycairo)
       ("python-pygobject" ,python-pygobject)
       ("python-libxml2" ,python-libxml2)
       ("spice-gtk" ,spice-gtk)))
    ;; virt-manager searches for qemu-img or kvm-img in the PATH.
    (propagated-inputs
     `(("qemu" ,qemu)))
    (native-inputs
     `(("glib" ,glib "bin")             ; glib-compile-schemas
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
    (license license:gpl2+)))

(define-public criu
  (package
    (name "criu")
    (version "3.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.openvz.org/criu/criu-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "03nimyn3wy5mlw30gq7bvlzvvprqjv8f25240yj5arzlld8mhsw8"))))
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
                               "/manpages/docbook.xsl"))
               (("\\$\\(XMLTO\\);")
                (string-append (assoc-ref inputs "xmlto")
                               "/bin/xmlto;")))
             #t))
         (add-after 'unpack 'hardcode-variables
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Hardcode arm version detection
             (substitute* "Makefile"
               (("ARMV.*:=.*") "ARMV := 7\n"))
             ;; We are currently using python-2
             (substitute* "crit/Makefile"
               (("\\$\\(PYTHON\\)") "python2"))
             (substitute* "lib/Makefile"
               (("\\$\\(PYTHON\\)")
                (string-append (assoc-ref inputs "python")
                               "/bin/python")))
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
    (license (list license:gpl2 license:lgpl2.1))))

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
    (license license:gpl3+)))

(define-public looking-glass-client
  (let ((commit "182c4752d57690da7f99d5e788de9b8baea33895"))
    (package
     (name "looking-glass-client")
     (version (string-append "a12-" (string-take commit 7)))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference (url "https://github.com/gnif/LookingGlass")
                           (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "02bq46ndmzq9cihazzn7xq1x7q5nzm7iw4l9lqzihxcxp9famkhw"))))
     (build-system cmake-build-system)
     (inputs `(("fontconfig" ,fontconfig)
               ("glu" ,glu)
               ("mesa" ,mesa)
               ("openssl" ,openssl)
               ("sdl2" ,sdl2)
               ("sdl2-ttf" ,sdl2-ttf)
               ("spice-protocol" ,spice-protocol)
               ("wayland" ,wayland)))
     (native-inputs `(("libconfig" ,libconfig)
                      ("nettle" ,nettle)
                      ("pkg-config" ,pkg-config)))
     (arguments
      `(#:tests? #f ;; No tests are available.
        #:make-flags '("CC=gcc")
        #:phases (modify-phases %standard-phases
                   (add-before 'configure 'chdir-to-client
                     (lambda* (#:key outputs #:allow-other-keys)
                       (chdir "client")
                       #t))
                   (replace 'install
                     (lambda* (#:key outputs #:allow-other-keys)
                       (install-file "looking-glass-client"
                                     (string-append (assoc-ref outputs "out")
                                                    "/bin"))
                       #t))
                   )))
     (home-page "https://looking-glass.hostfission.com")
     (synopsis "KVM Frame Relay (KVMFR) implementation")
     (description "Looking Glass allows the use of a KVM (Kernel-based Virtual
Machine) configured for VGA PCI Pass-through without an attached physical
monitor, keyboard or mouse.  It displays the VM's rendered contents on your main
monitor/GPU.")
     ;; This package requires SSE instructions.
     (supported-systems '("i686-linux" "x86_64-linux"))
     (license license:gpl2+))))

(define-public lookingglass
  (deprecated-package "lookingglass" looking-glass-client))

(define-public runc
  (package
    (name "runc")
    (version "1.0.0-rc6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/opencontainers/runc/releases/"
                    "download/v" version "/runc.tar.xz"))
              (file-name (string-append name "-" version ".tar.xz"))
              (patches (search-patches "runc-CVE-2019-5736.patch"))
              (sha256
               (base32
                "1c7832dq70slkjh8qp2civ1wxhhdd2hrx84pq7db1mmqc9fdr3cc"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/opencontainers/runc"
       #:install-source? #f
       ;; XXX: 20/139 tests fail due to missing /var, cgroups and apparmor in
       ;; the build environment.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source import-path #:allow-other-keys)
             ;; Unpack the tarball into 'runc' instead of 'runc-1.0.0-rc5'.
             (let ((dest (string-append "src/" import-path)))
               (mkdir-p dest)
               (invoke "tar" "-C" (string-append "src/" import-path)
                       "--strip-components=1"
                       "-xvf" source))))
         (replace 'build
           (lambda* (#:key import-path #:allow-other-keys)
             (chdir (string-append "src/" import-path))
             ;; XXX: requires 'go-md2man'.
             ;; (invoke "make" "man")
             (invoke "make")))
         ;; (replace 'check
         ;;   (lambda _
         ;;     (invoke "make" "localunittest")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
              (invoke "make" "install" "install-bash"
                      (string-append "PREFIX=" out))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libseccomp" ,libseccomp)))
    (synopsis "Open container initiative runtime")
    (home-page "https://www.opencontainers.org/")
    (description
     "@command{runc} is a command line client for running applications
packaged according to the
@uref{https://github.com/opencontainers/runtime-spec/blob/master/spec.md, Open
Container Initiative (OCI) format} and is a compliant implementation of the
Open Container Initiative specification.")
    (license license:asl2.0)))

(define-public umoci
  (package
    (name "umoci")
    (version "0.4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/openSUSE/umoci/releases/download/v"
                    version "/umoci.tar.xz"))
              (file-name (string-append "umoci-" version ".tar.xz"))
              (sha256
               (base32
                "1wchmha5k2f370jfijmx9fqp0cp99zfa9ajmfbq3j24qc8p5k8lk"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/openSUSE/umoci"
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source import-path #:allow-other-keys)
             ;; Unpack the tarball into 'umoci' instead of "runc-${version}".
             (let ((dest (string-append "src/" import-path)))
               (mkdir-p dest)
               (invoke "tar" "-C" (string-append "src/" import-path)
                       "--strip-components=1"
                       "-xvf" source))))
         (replace 'build
           (lambda* (#:key import-path #:allow-other-keys)
             (chdir (string-append "src/" import-path))
             ;; TODO: build manpages with 'go-md2man'.
             (invoke "make" "SHELL=bash")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bindir (string-append out "/bin")))
               (install-file "umoci" bindir)
               #t))))))
    (home-page "https://umo.ci/")
    (synopsis "Tool for modifying Open Container images")
    (description
     "@command{umoci} is a tool that allows for high-level modification of an
Open Container Initiative (OCI) image layout and its tagged images.")
    (license license:asl2.0)))

(define-public skopeo
  (package
    (name "skopeo")
    (version "0.1.28")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/projectatomic/skopeo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "068nwrr3nr27alravcq1sxyhdd5jjr24213vdgn1dqva3885gbi0"))))
    (build-system go-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("btrfs-progs" ,btrfs-progs)
       ("eudev" ,eudev)
       ("libassuan" ,libassuan)
       ("libselinux" ,libselinux)
       ("libostree" ,libostree)
       ("lvm2" ,lvm2)
       ("glib" ,glib)
       ("gpgme" ,gpgme)))
    (arguments
     '(#:import-path "github.com/projectatomic/skopeo"
       #:install-source? #f
       #:tests? #f ; The tests require Docker
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path #:allow-other-keys)
             (chdir (string-append "src/" import-path))
             ;; TODO: build manpages with 'go-md2man'.
             (invoke "make" "binary-local")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "make" "install-binary" "install-completions"
                       (string-append "PREFIX=" out))))))))
    (home-page "https://github.com/projectatomic/skopeo")
    (synopsis "Interact with container images and container image registries")
    (description
     "@command{skopeo} is a command line utility providing various operations
with container images and container image registries.  It can:
@enumerate

@item Copy container images between various containers image stores,
converting them as necessary.

@item Convert a Docker schema 2 or schema 1 container image to an OCI image.

@item Inspect a repository on a container registry without needlessly pulling
the image.

@item Sign and verify container images.

@item Delete container images from a remote container registry.

@end enumerate")
    (license license:asl2.0)))

(define-public python-vagrant
  (package
    (name "python-vagrant")
    (version "0.5.15")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-vagrant" version))
        (sha256
         (base32
          "1ikrh6canhcxg5y7pzmkcnnydikppv7s6sm9prfx90nk0ac8m6mg"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; tests involve running vagrant.
    (home-page "https://github.com/todddeluca/python-vagrant")
    (synopsis "Python bindings for Vagrant")
    (description
     "Python-vagrant is a Python module that provides a thin wrapper around the
@code{vagrant} command line executable, allowing programmatic control of Vagrant
virtual machines.")
    (license license:expat)))

(define-public bubblewrap
  (package
    (name "bubblewrap")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/projectatomic/bubblewrap/"
                                  "releases/download/v" version "/bubblewrap-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1y2bdlxnlr84xcbf31lzirc292c5ak9bd2wvcvh4ppsliih6pjny"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-test
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Tests try to access /var/tmp, which is not possible in our build
             ;; environment.  Let's give them another directory.
             ;; /tmp gets overriden in some tests, so we need another directory.
             ;; the only possibility is the output directory.
             (let ((tmp-dir (string-append (assoc-ref outputs "out") "/tmp")))
               (mkdir-p tmp-dir)
               (substitute* "tests/test-run.sh"
                 (("/var/tmp") tmp-dir)
                 ;; Tests create a temporary python script, so fix its shebang.
                 (("/usr/bin/env python") (which "python"))
                 ;; Some tests try to access /usr, but that doesn't exist.
                 ;; Give them /gnu instead.
                 (("/usr") "/gnu")
                 (("  */bin/bash") (which "bash"))
                 (("/bin/sh") (which "sh"))
                 (("findmnt") (which "findmnt"))))
             #t))
         ;; Remove the directory we gave to tests to have a clean package.
         (add-after 'check 'remove-tmp-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (delete-file-recursively (string-append (assoc-ref outputs "out") "/tmp"))
             #t)))))
    (inputs
     `(("libcap" ,libcap)))
    (native-inputs
     `(("python-2" ,python-2)
       ("util-linux" ,util-linux)))
    (home-page "https://github.com/projectatomic/bubblewrap")
    (synopsis "Unprivileged sandboxing tool")
    (description "Bubblewrap is aimed at running applications in a sandbox,
restricting their access to parts of the operating system or user data such as
the home directory.  Bubblewrap always creates a new mount namespace, and the
user can specify exactly what parts of the file system should be made visible
in the sandbox.  These directories are mounted with the @code{nodev} option
by default and can be made read-only.")
    (license license:lgpl2.0+)))

(define-public bochs
  (package
    (name "bochs")
    (version "2.6.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://sourceforge.net/projects/bochs/files/bochs/"
                           version "/bochs-" version ".tar.gz"))
       (sha256
        (base32
         "1379cq4cnfprhw8mgh60i0q9j8fz8d7n3d5fnn2g9fdiv5znfnzf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ; No tests exist
    (inputs
     `(("libxrandr" ,libxrandr)))
    (home-page "http://bochs.sourceforge.net/")
    (synopsis "Emulator for x86 PC")
    (description
     "Bochs is an emulator which can emulate Intel x86 CPU, common I/O
devices, and a custom BIOS.  It can also be compiled to emulate many different
x86 CPUs, from early 386 to the most recent x86-64 Intel and AMD processors.
Bochs can run most Operating Systems inside the emulation including Linux,
DOS or Microsoft Windows.")
    (license license:lgpl2.0+)))

(define-public xen
  (package
    (name "xen")
    (version "4.11.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://xenbits.xenproject.org/xen.git")
                    (commit (string-append "RELEASE-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wv1hyfii14vi9lfjmnv07h2gpm3b7kvh2p55f4yy2b40simksgk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-rpath"
             "--disable-qemu-traditional" ; It tries to do "git clone"
             "--disable-rombios" ; would try to "git clone" via etherboot.
             ;; TODO: Re-enable stubdom (it's "more secure" to use it).
             "--disable-stubdom" ; tries to "git clone" old patched newlib.
             (string-append "--with-initddir="
                            (assoc-ref %outputs "out")
                            "/etc/init.d")
             (string-append "--with-system-qemu="
                            (assoc-ref %build-inputs "qemu")
                            "/bin/qemu-system-i386")
             (string-append "--with-system-seabios="
                            (assoc-ref %build-inputs "seabios")
                            "/share/firmware/bios.bin")
             (string-append "--with-system-ovmf="
                            (assoc-ref %build-inputs "ovmf")
                            "/share/firmware/ovmf_ia32.bin"))
       #:make-flags (list "-j" "1"
                          "XEN_BUILD_DATE=Thu Jan  1 01:00:01 CET 1970"
                          "XEN_BUILD_TIME=01:00:01"
                          "XEN_BUILD_HOST="
                          "ETHERBOOT_NICS="
                          "SMBIOS_REL_DATE=01/01/1970"
                          "VGABIOS_REL_DATE=01 Jan 1970"
                          ; QEMU_TRADITIONAL_LOC
                          ; QEMU_UPSTREAM_LOC
                          "SYSCONFIG_DIR=/tmp/etc/default"
                          (string-append "BASH_COMPLETION_DIR="
                                         (assoc-ref %outputs "out")
                                         "/etc/bash_completion.d")
                          (string-append "BOOT_DIR="
                                         (assoc-ref %outputs "out")
                                         "/boot")
                          (string-append "DEBUG_DIR="
                                         (assoc-ref %outputs "out")
                                         "/lib/debug")
                          (string-append "EFI_DIR="
                                         (assoc-ref %outputs "out")
                                         "/lib/efi") ; TODO lib64 ?
                          "MINIOS_UPSTREAM_URL="
                          ;(string-append "DISTDIR="
                          ;               (assoc-ref %outputs "out"))
)
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
        (add-after 'unpack 'unpack-mini-os
          (lambda* (#:key inputs #:allow-other-keys)
            (copy-recursively (assoc-ref inputs "mini-os") "extras/mini-os")
            #t))
        (add-after 'unpack-mini-os 'patch
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "tools/firmware/Rules.mk"
             (("override XEN_TARGET_ARCH = x86_32")
              (string-append "override XEN_TARGET_ARCH = x86_32
override CC = " (assoc-ref inputs "cross-gcc") "/bin/i686-linux-gnu-gcc"))
             (("^CFLAGS =$")
              (string-append "CFLAGS=-I" (assoc-ref inputs "cross-libc")
                             "/include\n")))
            (substitute* "config/x86_32.mk"
             (("CFLAGS += -m32 -march=i686")
              (string-append "CFLAGS += -march=i686 -I"
                             (assoc-ref inputs "cross-libc")
                             "/include")))
            ;; /var is not in /gnu/store , so don't try to create it.
            (substitute* '("tools/Makefile"
                           "tools/xenstore/Makefile"
                           "tools/xenpaging/Makefile")
             (("\\$\\(INSTALL_DIR\\) .*XEN_(DUMP|LOG|RUN|LIB|PAGING)_DIR.*")
              "\n")
             (("\\$\\(INSTALL_DIR\\) .*XEN_(RUN|LIB)_STORED.*")
              "\n"))
            ;; Prevent xen from creating /etc .
            (substitute* "tools/examples/Makefile"
             ((" install-readmes") "")
             ((" install-configs") ""))
            ;; Set rpath.
            (substitute* "tools/pygrub/setup.py"
             (("library_dirs =")
              ; TODO: extra_link_args = ['-Wl,-rpath=/opt/foo'],
              (string-append "runtime_library_dirs = ['"
                             (assoc-ref outputs "out")
                             "/lib'],\nlibrary_dirs =")))
            #t))
        (add-before 'configure 'patch-xen-script-directory
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* '("configure"
                           "tools/configure"
                           "docs/configure")
             (("XEN_SCRIPT_DIR=.*")
              (string-append "XEN_SCRIPT_DIR="
                             (assoc-ref outputs "out")
                             "/etc/xen/scripts")))
            #t))
        (add-before 'configure 'set-environment-up
          (lambda* (#:key make-flags #:allow-other-keys)
             (define (cross? x)
               (string-contains x "cross-i686-linux"))
             (define (filter-environment! filter-predicate
                                          environment-variable-names)
               (for-each
                (lambda (env-name)
                  (let* ((env-value (getenv env-name))
                         (search-path (search-path-as-string->list env-value))
                         (new-search-path (filter filter-predicate
                                                  search-path))
                         (new-env-value (list->search-path-as-string
                                         new-search-path ":")))
                    (setenv env-name new-env-value)))
                environment-variable-names))
             (setenv "CROSS_C_INCLUDE_PATH" (getenv "C_INCLUDE_PATH"))
             (setenv "CROSS_CPLUS_INCLUDE_PATH" (getenv "CPLUS_INCLUDE_PATH"))
             (setenv "CROSS_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
             (filter-environment! cross?
              '("CROSS_C_INCLUDE_PATH" "CROSS_CPLUS_INCLUDE_PATH"
                "CROSS_LIBRARY_PATH"))
             (filter-environment! (lambda (e) (not (cross? e)))
              '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH"
                "LIBRARY_PATH"))
             ;; Guix tries to be helpful and automatically adds
             ;; mini-os-git-checkout/include to the include path,
             ;; but actually we don't want it to be there (yet).
             (filter-environment! (lambda (e)
                                    (not
                                     (string-contains e
                                      "mini-os-git-checkout")))
              '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH"
                "LIBRARY_PATH"))
            (setenv "EFI_VENDOR" "guix")
             #t))
        (replace 'build
          (lambda* (#:key make-flags #:allow-other-keys)
            (apply invoke "make" "world" make-flags))))))
    (inputs
     `(("acpica" ,acpica) ; TODO: patch iasl invocation.
       ("bridge-utils" ,bridge-utils) ; TODO: patch invocations.
       ("glib" ,glib)
       ("iproute" ,iproute) ; TODO: patch invocations.
       ("libaio" ,libaio)
       ("libx11" ,libx11)
       ("libyajl" ,libyajl)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("ovmf" ,ovmf)
       ("pixman" ,pixman)
       ("qemu" ,qemu-minimal)
       ("seabios" ,seabios)
       ("util-linux" ,util-linux) ; uuid
       ; TODO: ocaml-findlib, ocaml-nox.
       ("xz" ,xz) ; for liblzma
       ("zlib" ,zlib)))
    (native-inputs
     `(("dev86" ,dev86)
       ("bison" ,bison)
       ("cmake" ,cmake)
       ("figlet" ,figlet)
       ("flex" ,flex)
       ("gettext" ,gettext-minimal)
       ("libnl" ,libnl)
       ("mini-os"
       ,(origin
         (method git-fetch)
         (uri (git-reference
               (url "http://xenbits.xen.org/git-http/mini-os.git")
               (commit (string-append "xen-RELEASE-" version))))
         (sha256
          (base32
           "1i8pcl19n60i2m9vlg79q3nknpj209c9ic5x10wxaicx45kc107f"))
         (file-name "mini-os-git-checkout")))
       ("perl" ,perl)
       ; TODO: markdown
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("wget" ,wget)
       ("cross-gcc" ,(cross-gcc "i686-linux-gnu"
                                #:xbinutils (cross-binutils "i686-linux-gnu")
                                #:libc (cross-libc "i686-linux-gnu")))
       ("cross-libc" ,(cross-libc "i686-linux-gnu")) ; header files
       ("cross-libc-static" ,(cross-libc "i686-linux-gnu") "static")))
    (home-page "https://xenproject.org/")
    (synopsis "Xen Virtual Machine Monitor")
    (description "This package provides the Xen Virtual Machine Monitor
which is a hypervisor.")
    ;; TODO: Some files are licensed differently.  List those.
    (license license:gpl2)
    (supported-systems '("i686-linux" "x86_64-linux" "armhf-linux"))))
