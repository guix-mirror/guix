;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015, 2017 Andy Wingo <wingo@pobox.com>
;;; Copyright © 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017 Brendan Tildesley <brendan.tildesley@openmailbox.org>
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

(define-module (gnu packages freedesktop)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)                ;intltool
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public xdg-utils
  (package
    (name "xdg-utils")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
          (uri (string-append
                 "https://portland.freedesktop.org/download/xdg-utils-"
                 version ".tar.gz"))
          (sha256
            (base32
             "1k4b4m3aiyqn9k12a0ihcdahzlspl3zhskmm1d7228dvqvi546cm"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("docbook-xsl" ,docbook-xsl)
       ("docbook-xml" ,docbook-xml-4.1.2)
       ("libxslt" ,libxslt)
       ("w3m" ,w3m)
       ("xmlto" ,xmlto)))
    (propagated-inputs
     `(("perl-file-mimeinfo" ,perl-file-mimeinfo) ; for mimeopen fallback
       ("xprop" ,xprop) ; for Xfce detecting
       ("xset" ,xset))) ; for xdg-screensaver
    (arguments
     `(#:tests? #f   ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-hardcoded-paths
           (lambda _
             (substitute* "scripts/xdg-mime.in"
               (("/usr/bin/file") (which "file")))
             (substitute* "scripts/xdg-open.in"
               (("/usr/bin/printf") (which "printf")))
             #t))
         (add-before 'build 'locate-catalog-files
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xmldoc (string-append (assoc-ref inputs "docbook-xml")
                                          "/xml/dtd/docbook"))
                   (xsldoc (string-append (assoc-ref inputs "docbook-xsl")
                                          "/xml/xsl/docbook-xsl-"
                                          ,(package-version docbook-xsl))))
               (for-each (lambda (file)
                           (substitute* file
                             (("http://.*/docbookx\\.dtd")
                              (string-append xmldoc "/docbookx.dtd"))))
                         (find-files "scripts/desc" "\\.xml$"))
               (substitute* "scripts/Makefile"
                 ;; Apparently `xmlto' does not bother to looks up the stylesheets
                 ;; specified in the XML, unlike the above substitition. Instead it
                 ;; uses a hard-coded URL. Work around it here, but if this is
                 ;; common perhaps we should hardcode this path in xmlto itself.
                 (("\\$\\(XMLTO\\) man")
                  (string-append "$(XMLTO) -x " xsldoc
                                 "/manpages/docbook.xsl man")))
               (setenv "STYLESHEET"
                       (string-append xsldoc "/html/docbook.xsl"))
               #t))))))
    (home-page "https://www.freedesktop.org/wiki/Software/xdg-utils/")
    (synopsis "Freedesktop.org scripts for desktop integration")
    (description "The xdg-utils package is a set of simple scripts that
provide basic desktop integration functions in the framework of the
freedesktop.org project.")
    (license license:expat)))

(define-public libinput
  (package
    (name "libinput")
    (version "1.7.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://freedesktop.org/software/libinput/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "07fbzxddvhjcch43hdxb24sj7ri96zzpcjalvsicmw0i4wnn2v89"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("cairo" ,cairo)
       ("gtk+" ,gtk+)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("libudev" ,eudev))) ; required by libinput.pc
    (inputs
     `(("glib" ,glib)
       ("libevdev" ,libevdev)
       ("mtdev" ,mtdev)
       ("libwacom" ,libwacom)))
    (home-page "https://www.freedesktop.org/wiki/Software/libinput/")
    (synopsis "Input devices handling library")
    (description
     "Libinput is a library to handle input devices for display servers and
other applications that need to directly deal with input devices.")
    (license license:x11)))

(define-public libinput-minimal
  (package (inherit libinput)
    (name "libinput-minimal")
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libevdev" ,libevdev)
       ("mtdev" ,mtdev)))
    (arguments
      `(#:configure-flags
        '("--disable-libwacom")))))

(define-public libxdg-basedir
  (package
    (name "libxdg-basedir")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/devnev/libxdg-basedir/archive/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "0s28c7sfwqimsmb3kn91mx7wi55fs3flhbmynl9k60rrllr00aqw"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autogen
           (lambda _
             ;; Run 'configure' in its own phase, not now.
             (substitute* "autogen.sh"
               (("^.*\\./configure.*") ""))
             (zero? (system* "sh" "autogen.sh")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (home-page "https://github.com/devnev/libxdg-basedir")
    (synopsis "Implementation of the XDG Base Directory specification")
    (description
     "libxdg-basedir is a C library providing some functions to use with
the freedesktop.org XDG Base Directory specification.")
    (license license:expat)))

(define-public elogind
  (package
    (name "elogind")
    (version "232.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/elogind/elogind/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1qcxian48z2dj5gfmp7brrngdydqf2jm00f4rjr5sy1myh8fy931"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (use-modules (guix build utils))
                  (substitute* "Makefile.am"
                    ;; Avoid validation against DTD because the DTDs for
                    ;; both doctype 4.2 and 4.5 are needed.
                    (("XSLTPROC_FLAGS = ") "XSLTPROC_FLAGS = --novalid"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;FIXME: "make check" in the "po" directory fails.
       #:configure-flags
       (list (string-append "--with-udevrulesdir="
                            (assoc-ref %outputs "out")
                            "/lib/udev/rules.d")

             ;; Let elogind be its own cgroup controller, rather than relying
             ;; on systemd or OpenRC.  By default, 'configure' makes an
             ;; incorrect guess.
             "--with-cgroup-controller=elogind"

             (string-append "--with-rootprefix="
                            (assoc-ref %outputs "out"))
             (string-append "--with-rootlibexecdir="
                            (assoc-ref %outputs "out")
                            "/libexec/elogind")
             ;; These are needed to ensure that lto linking works.
             "RANLIB=gcc-ranlib"
             "AR=gcc-ar"
             "NM=gcc-nm")
       #:make-flags '("PKTTYAGENT=/run/current-system/profile/bin/pkttyagent")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-locale-header
           (lambda _
             ;; Fix compilation with glibc >= 2.26, which removed xlocale.h.
             ;; This can be removed for elogind 234.
             (substitute* "src/basic/parse-util.c"
               (("xlocale\\.h") "locale.h"))))
         (add-before 'configure 'autogen
           (lambda _
             (and (zero? (system* "intltoolize" "--force" "--automake"))
                  (zero? (system* "autoreconf" "-vif")))))
         (add-before 'build 'fix-service-file
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Fix the file name of the 'elogind' binary in the D-Bus
             ;; '.service' file.
             (substitute* "src/login/org.freedesktop.login1.service"
               (("^Exec=.*")
                (string-append "Exec=" (assoc-ref %outputs "out")
                               "/libexec/elogind/elogind\n")))))
         (add-after 'install 'add-libcap-to-search-path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Add a missing '-L' for libcap in libelogind.la.  See
             ;; <https://lists.gnu.org/archive/html/guix-devel/2017-09/msg00084.html>.
             (let ((libcap (assoc-ref inputs "libcap"))
                   (out    (assoc-ref outputs "out")))
               (substitute* (string-append out "/lib/libelogind.la")
                 (("-lcap")
                  (string-append "-L" libcap "/lib -lcap")))
               #t)))
         (add-after 'unpack 'remove-uaccess-tag
           (lambda _
             ;; systemd supports a "uaccess" built-in tag, but eudev currently
             ;; doesn't.  This leads to eudev warnings that we'd rather not
             ;; see, so remove the reference to "uaccess."
             (substitute* "src/login/73-seat-late.rules.in"
               (("^TAG==\"uaccess\".*" line)
                (string-append "# " line "\n")))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("intltool" ,intltool)
       ("gettext" ,gettext-minimal)
       ("python" ,python)
       ("docbook-xsl" ,docbook-xsl)
       ("docbook-xml" ,docbook-xml)
       ("xsltproc" ,libxslt)
       ("m4" ,m4)
       ("libxml2" ,libxml2)                     ;for XML_CATALOG_FILES
       ("pkg-config" ,pkg-config)

       ;; Use gperf 3.0 to work around
       ;; <https://github.com/wingo/elogind/issues/8>.
       ("gperf" ,gperf-3.0)))
    (inputs
     `(("linux-pam" ,linux-pam)
       ("linux-libre-headers" ,linux-libre-headers)
       ("libcap" ,libcap)
       ("shepherd" ,shepherd)                ;for 'halt' and 'reboot', invoked
                                             ;when pressing the power button
       ("dbus" ,dbus)
       ("eudev" ,eudev)
       ("acl" ,acl)))           ;to add individual users to ACLs on /dev nodes
    (home-page "https://github.com/elogind/elogind")
    (synopsis "User, seat, and session management service")
    (description "Elogind is the systemd project's \"logind\" service,
extracted out as a separate project.  Elogind integrates with PAM to provide
the org.freedesktop.login1 interface over the system bus, allowing other parts
of a the system to know what users are logged in, and where.")
    (license license:lgpl2.1+)))

(define-public packagekit
  (package
    (name "packagekit")
    (version "1.1.5")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://www.freedesktop.org/software/"
                   "PackageKit/releases/"
                   "PackageKit-" version ".tar.xz"))
             (sha256
              (base32
               "035pqxgkyki813hyw2frrbpfllq113zfk5qcp9wvsq5lsp74ix2h"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "BASH_COMPLETIONS_DIR="
                                         %output "/etc/bash_completion.d"))
       #:configure-flags
       '("--disable-systemd")))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("glib:bin" ,glib "bin")))
    (inputs
     `(("glib" ,glib)
       ("bash-completion", bash-completion)
       ("polkit" ,polkit)))
    (propagated-inputs
     `(("sqlite" ,sqlite)))
    (home-page "https://www.freedesktop.org/software/PackageKit/")
    (synopsis "API for package management, through D-Bus")
    (description
     "PackageKit provides a way of performing package management tasks,
e.g. updating, removing and installing software.  Through supporting many
backends, PackageKit can perform these tasks using the appropriate package
manager for the current system.")
    (license license:gpl2+)))

(define-public python-pyxdg
  (package
    (name "python-pyxdg")
    (version "0.25")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/pyxdg/pyxdg-"
             version ".tar.gz"))
       (sha256
        (base32
         "179767h8m634ydlm4v8lnz01ba42gckfp684id764zaip7h87s41"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "XDG_DATA_DIRS"
                     (string-append (assoc-ref inputs "shared-mime-info")
                                    "/share/"))
             (substitute* "test/test-icon.py"
               (("/usr/share/icons/hicolor/index.theme")
                (string-append (assoc-ref inputs "hicolor-icon-theme")
                               "/share/icons/hicolor/index.theme"))
               ;; FIXME: This test fails because the theme contains the unknown
               ;; key "Scale".
               (("theme.validate\\(\\)") "#"))

             ;; One test fails with:
             ;; AssertionError: 'x-apple-ios-png' != 'png'
             (substitute* "test/test-mime.py"
               (("self.check_mimetype\\(imgpng, 'image', 'png'\\)") "#"))
             (zero? (system* "nosetests" "-v")))))))
    (native-inputs
     `(("shared-mime-info" ,shared-mime-info) ;for tests
       ("hicolor-icon-theme" ,hicolor-icon-theme) ;for tests
       ("python-nose" ,python-nose)))
    (home-page "https://www.freedesktop.org/wiki/Software/pyxdg")
    (synopsis "Implementations of freedesktop.org standards in Python")
    (description
     "PyXDG is a collection of implementations of freedesktop.org standards in
Python.")
    (license license:lgpl2.0)))

(define-public python2-pyxdg
  (package-with-python2 python-pyxdg))

(define-public wayland
  (package
    (name "wayland")
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://wayland.freedesktop.org/releases/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1f3sla6h0bw15fz8pjc67jhwj7pwmfdc7qlj42j5k9v116ycm07d"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-tests? #f))
    (native-inputs
     `(("doxygen" ,doxygen)
       ("graphviz" ,graphviz)
       ("pkg-config" ,pkg-config)
       ("xmlto" ,xmlto)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("docbook-xml" ,docbook-xml)
       ("docbook-xsl" ,docbook-xsl)
       ("expat" ,expat)
       ("libffi" ,libffi)
       ("libxml2" ,libxml2))) ; for XML_CATALOG_FILES
    (home-page "https://wayland.freedesktop.org/")
    (synopsis "Display server protocol")
    (description
     "Wayland is a protocol for a compositor to talk to its clients as well as
a C library implementation of that protocol.  The compositor can be a standalone
display server running on Linux kernel modesetting and evdev input devices, an X
application, or a wayland client itself.  The clients can be traditional
applications, X servers (rootless or fullscreen) or other display servers.")
    (license license:x11)))

(define-public wayland-protocols
  (package
    (name "wayland-protocols")
    (version "1.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://wayland.freedesktop.org/releases/"
                    "wayland-protocols-" version ".tar.xz"))
              (sha256
               (base32
                "1cn8ny4zr9xlcdh8qi1qnkmvia8cp4ixnsbhd9sp9571w6lyh69v"))))
    (build-system gnu-build-system)
    (inputs
     `(("wayland" ,wayland)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Wayland protocols")
    (description "This package contains XML definitions of the Wayland protocols.")
    (home-page "https://wayland.freedesktop.org")
    (license license:expat)))

(define-public weston
  (package
    (name "weston")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://wayland.freedesktop.org/releases/"
                    "weston-" version ".tar.xz"))
              (sha256
               (base32
                "19936zlkb75xcaidd8fag4ah8000wrh2ziqy7nxkq36pimgdbqfd"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("xorg-server" ,xorg-server)))
    (inputs
     `(("cairo" ,cairo-xcb)
       ("dbus" ,dbus)
       ("elogind" ,elogind)
       ("libinput" ,libinput-minimal)
       ("libunwind" ,libunwind)
       ("libxcursor" ,libxcursor)
       ("libxkbcommon" ,libxkbcommon)
       ("mesa" ,mesa)
       ("mtdev" ,mtdev)
       ("linux-pam" ,linux-pam)
       ("wayland" ,wayland)
       ("wayland-protocols" ,wayland-protocols)
       ("xorg-server-xwayland" ,xorg-server-xwayland)))
    (arguments
     `(#:configure-flags
       (list "--disable-setuid-install"
             "--enable-systemd-login"
             (string-append "--with-xserver-path="
                            (assoc-ref %build-inputs "xorg-server-xwayland")
                            "/bin/Xwayland"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'use-elogind
           (lambda _
             ;; Use elogind instead of systemd
             (substitute* "configure"
               (("libsystemd-login >= 198") "libelogind"))
             (substitute* '("libweston/launcher-logind.c"
                            "libweston/weston-launch.c")
               (("#include <systemd/sd-login.h>")
                "#include <elogind/sd-login.h>"))
             #t))
         (add-after 'configure 'patch-confdefs.h
           (lambda _
             (system "echo \"#define HAVE_SYSTEMD_LOGIN_209 1\" >> confdefs.h")))
         (add-before 'check 'setup
           (lambda _
             (setenv "HOME" (getcwd))
             (setenv "XDG_RUNTIME_DIR" (getcwd))
             #t))
         (add-before 'check 'start-xorg-server
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The test suite requires a running X server.
             (system (string-append (assoc-ref inputs "xorg-server")
                                    "/bin/Xvfb :1 &"))
             (setenv "DISPLAY" ":1")
             #t)))))
    (home-page "https://wayland.freedesktop.org")
    (synopsis "Reference implementation of a Wayland compositor")
    (description "Weston is the reference implementation of a Wayland
compositor, and a useful compositor in its own right.

A Wayland compositor allows applications to render to a shared offscreen
buffer using OpenGL ES.  The compositor then culls the hidden parts and
composes the final output.  A Wayland compositor is essentially a
multiplexer to the KMS/DRM Linux kernel devices.")
    (license license:expat)))

(define-public exempi
  (package
    (name "exempi")
    (version "2.4.2")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://libopenraw.freedesktop.org/download/"
                   name "-" version ".tar.bz2"))
             (sha256
              (base32
               "1v665fc7x0yi7x6lzskvd8bd2anf7951svn2vd5384dblmgv43av"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list (string-append "--with-boost="
                               (assoc-ref %build-inputs "boost")))))
    (native-inputs
     `(("boost" ,boost))) ; tests
    (inputs
     `(("expat" ,expat)
       ("zlib" ,zlib)))
    (home-page "https://wiki.freedesktop.org/libopenraw/Exempi")
    (synopsis "XMP metadata handling library")
    (description "Exempi is an implementation of the Extensible Metadata
Platform (XMP), which enables embedding metadata in PDF and image formats.")
    (license license:bsd-3)))

(define-public libatasmart
  (package
    (name "libatasmart")
    (version "0.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://0pointer.de/public/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "138gvgdwk6h4ljrjsr09pxk1nrki4b155hqdzyr8mlk3bwsfmw31"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("udev" ,eudev)))
    (home-page "http://0pointer.de/blog/projects/being-smart.html")
    (synopsis "ATA S.M.A.R.T. reading and parsing library")
    (description
     "This library supports a subset of the ATA S.M.A.R.T. (Self-Monitoring,
Analysis and Reporting Technology) functionality.")
    (license license:lgpl2.1+)))

(define-public udisks
  (package
    (name "udisks")
    (version "2.1.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://udisks.freedesktop.org/releases/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1nkxhnqh39c9pzvm4zfj50rgv6apqawdx09bv3sfaxrah4a6jhfs"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.3) ; to build the manpages
       ("docbook-xsl" ,docbook-xsl)
       ("glib:bin" ,glib "bin") ; for glib-mkenums
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     `(("glib" ,glib))) ; required by udisks2.pc
    (inputs
     `(("acl" ,acl)
       ("libatasmart" ,libatasmart)
       ("libgudev" ,libgudev)
       ("polkit" ,polkit)
       ("util-linux" ,util-linux)
       ("cryptsetup" ,cryptsetup)
       ("parted" ,parted)))
    (outputs '("out"
               "doc"))                            ;5 MiB of gtk-doc HTML
    (arguments
     `(#:tests? #f ; requiring system message dbus
       #:disallowed-references ("doc")            ;enforce separation of "doc"
       #:configure-flags
       (list "--enable-man"
             "--localstatedir=/var"
             "--enable-fhs-media"     ;mount devices in /media, not /run/media
             (string-append "--with-html-dir="
                            (assoc-ref %outputs "doc")
                            "/share/doc/udisks/html")
             (string-append "--with-udevdir=" %output "/lib/udev"))
       #:make-flags
       (let*  ((docbook-xsl-name-version ,(string-append
                                           (package-name docbook-xsl) "-"
                                           (package-version  docbook-xsl)))
               (docbook-xsl-catalog-file (string-append
                                          (assoc-ref %build-inputs "docbook-xsl")
                                          "/xml/xsl/"
                                          docbook-xsl-name-version
                                          "/catalog.xml"))
               (docbook-xml-catalog-file (string-append
                                          (assoc-ref %build-inputs "docbook-xml")
                                          "/xml/dtd/docbook/catalog.xml")))
         ;; Reference the catalog files required to build the manpages.
         (list (string-append "XML_CATALOG_FILES=" docbook-xsl-catalog-file " "
                              docbook-xml-catalog-file)))
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'fix-girdir
          (lambda _
            ;; Install introspection data to its own output.
            (substitute* "udisks/Makefile.in"
              (("girdir = .*")
               "girdir = $(datadir)/gir-1.0\n")
              (("typelibsdir = .*")
               "typelibsdir = $(libdir)/girepository-1.0\n"))))
         (add-after 'install 'wrap-udisksd
           (lambda* (#:key outputs inputs #:allow-other-keys)
             ;; Tell 'udisksd' where to find the 'mount' command.
             (let ((out   (assoc-ref outputs "out"))
                   (utils (assoc-ref inputs "util-linux"))
                   (cryptsetup (assoc-ref inputs "cryptsetup"))
                   (parted (assoc-ref inputs "parted")))
               (wrap-program (string-append out "/libexec/udisks2/udisksd")
                 `("PATH" ":" prefix
                   (,(string-append utils "/bin") ;for 'mount'
                    ;; cryptsetup is required for setting encrypted
                    ;; partitions, e.g. in gnome-disks
                    ,(string-append cryptsetup "/sbin")
                    ;; parted is required for managing partitions, e.g. in
                    ;; gnome-disks
                    ,(string-append parted "/sbin")
                    "/run/current-system/profile/bin"
                    "/run/current-system/profile/sbin")))
               #t))))))
    (home-page "https://www.freedesktop.org/wiki/Software/udisks/")
    (synopsis "Disk manager service")
    (description
     "UDisks provides interfaces to enumerate and perform operations on disks
and storage devices.  Any application (including unprivileged ones) can access
the udisksd(8) daemon via the name org.freedesktop.UDisks2 on the system
message bus.")
    ;; The dynamic library are under LGPLv2+, others are GPLv2+.
    (license (list license:gpl2+ license:lgpl2.0+))))

(define-public accountsservice
  (package
    (name "accountsservice")
    (version "0.6.43")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.freedesktop.org/software/"
                                  name "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1k6n9079001sgcwlkq0bz6mkn4m8y4dwf6hs1qm85swcld5ajfzd"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; XXX: tests require DocBook 4.1.2
       #:configure-flags
       '("--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'pre-configure
          (lambda* (#:key inputs #:allow-other-keys)
            ;; Don't try to create /var/lib/AccountsService.
            (substitute* "src/Makefile.in"
              (("\\$\\(MKDIR_P\\).*/lib/AccountsService.*") "true"))
            (let ((shadow (assoc-ref inputs "shadow")))
              (substitute* '("src/user.c" "src/daemon.c")
                (("/usr/sbin/usermod") (string-append shadow "/sbin/usermod"))
                (("/usr/sbin/useradd") (string-append shadow "/sbin/useradd"))
                (("/usr/sbin/userdel") (string-append shadow "/sbin/userdel"))
                (("/usr/bin/passwd")   (string-append shadow "/bin/passwd"))
                (("/usr/bin/chage")    (string-append shadow "/bin/chage"))))
            #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for gdbus-codegen, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("shadow" ,shadow)
       ("polkit" ,polkit)))
    (home-page "https://www.freedesktop.org/wiki/Software/AccountsService/")
    (synopsis "D-Bus interface for user account query and manipulation")
    (description
     "The AccountService project provides a set of D-Bus interfaces for querying
and manipulating user account information and an implementation of these
interfaces, based on the useradd, usermod and userdel commands.")
    (license license:gpl3+)))

(define-public libmbim
  (package
    (name "libmbim")
    (version "1.12.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.freedesktop.org/software/" name "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0flpgzsqpjgybjkx4smbb4rjxf2w1xgd1v9gmz61rvl89qasznbv"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-mkenums
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (propagated-inputs
     `(("glib" ,glib))) ; required by mbim-glib.pc
    (inputs
     `(("libgudev" ,libgudev)))
    (synopsis "Library to communicate with MBIM-powered modems")
    (home-page "https://www.freedesktop.org/wiki/Software/libmbim/")
    (description
     "Libmbim is a GLib-based library for talking to WWAN modems and devices
which speak the Mobile Interface Broadband Model (MBIM) protocol.")
    (license
     ;; The libmbim-glib library is released under the LGPLv2+ license.
     ;; The mbimcli tool is released under the GPLv2+ license.
     (list license:lgpl2.0+ license:gpl2+))))

(define-public libqmi
  (package
    (name "libqmi")
    (version "1.14.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.freedesktop.org/software/" name "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0h009bzss4bal47nk21lyp4s3mmlcivhhaaj7r9229qvx85bi0v2"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-mkenums
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (propagated-inputs
     `(("glib" ,glib))) ; required by qmi-glib.pc
    (synopsis "Library to communicate with QMI-powered modems")
    (home-page "https://www.freedesktop.org/wiki/Software/libqmi/")
    (description
     "Libqmi is a GLib-based library for talking to WWAN modems and devices
which speak the Qualcomm MSM Interface (QMI) protocol.")
    (license
     ;; The libqmi-glib library is released under the LGPLv2+ license.
     ;; The qmicli tool is released under the GPLv2+ license.
     (list license:lgpl2.0+ license:gpl2+))))

(define-public modem-manager
  (package
    (name "modem-manager")
    (version "1.4.14")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.freedesktop.org/software/ModemManager/"
                    "ModemManager-" version ".tar.xz"))
              (sha256
               (base32
                "18hvffwcncwz14kdzk42jbkh362n0kjv3kgx7axbqx572pawvrmb"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       `(,(string-append "--with-udev-base-dir=" %output "/lib/udev"))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-mkenums
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ;; For testing.
       ("dbus" ,dbus)))
    (propagated-inputs
     `(("glib" ,glib))) ; required by mm-glib.pc
    (inputs
     `(("libgudev" ,libgudev)
       ("libmbim" ,libmbim)
       ("libqmi" ,libqmi)
       ("polkit" ,polkit)))
    (synopsis "Mobile broadband modems manager")
    (home-page "https://www.freedesktop.org/wiki/Software/ModemManager/")
    (description
     "ModemManager is a DBus-activated daemon which controls mobile
broadband (2G/3G/4G) devices and connections.  Whether built-in devices, USB
dongles, bluetooth-paired telephones, or professional RS232/USB devices with
external power supplies, ModemManager is able to prepare and configure the
modems and setup connections with them.")
    (license license:gpl2+)))

(define-public telepathy-logger
  (package
    (name "telepathy-logger")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://telepathy.freedesktop.org/releases/"
                                  name "/" name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1bjx85k7jyfi5pvl765fzc7q2iz9va51anrc2djv7caksqsdbjlg"))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
          (lambda _
            (setenv "HOME" (getenv "TMPDIR"))
            #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-genmarshal, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     ;; telepathy-logger-0.2.pc refers to all these.
     `(("libxml2" ,libxml2)
       ("sqlite" ,sqlite)
       ("telepathy-glib" ,telepathy-glib)))
    (synopsis "Telepathy logger library")
    (home-page "https://telepathy.freedesktop.org/")
    (description
     "Telepathy logger is a headless observer client that logs information
received by the Telepathy framework.  It features pluggable backends to log
different sorts of messages in different formats.")
    (license license:lgpl2.1+)))

(define-public telepathy-idle
  (package
    (name "telepathy-idle")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://telepathy.freedesktop.org/releases/"
                                  name "/" name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1argdzbif1vdmwp5vqbgkadq9ancjmgdm2ncp0qfckni715ss4rh"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("xsltproc" ,libxslt)
       ("python" ,python-2)
       ("python-dbus" ,python2-dbus)))
    (propagated-inputs
     `(("telepathy-glib" ,telepathy-glib)))
    (home-page "https://telepathy.freedesktop.org/")
    (synopsis "Telepathy IRC connection manager")
    (description
     "Idle is an IRC connection manager for the Telepathy framework.  This
package enables usage of IRC channels and private messages in Telepathy instant
messaging clients such as Empathy, GNOME Shell or KDE Telepathy.")
    (license (list license:lgpl2.1 license:lgpl2.1+))))

(define-public telepathy-mission-control
  (package
    (name "telepathy-mission-control")
    (version "5.16.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://telepathy.freedesktop.org/releases/"
                                  name "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jz6wwgsfxixha6ys2hbzbk5faqnj9kh2m5qdlgx5anqgandsscp"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("dconf" ,dconf)
       ("gtk-doc" ,gtk-doc)
       ("libgnome-keyring" ,libgnome-keyring)
       ("python" ,python-2)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     `(("telepathy-glib" ,telepathy-glib)))
    (home-page "https://telepathy.freedesktop.org/wiki/Components/Mission_Control/")
    (synopsis "Telepathy real-time communication framework management daemon")
    (description
     "Telepathy Mission Control 5 is an account manager and channel dispatcher
for the Telepathy framework, allowing user interfaces and other clients to
share connections to real-time communication services without conflicting.")
    (license license:lgpl2.1)))

(define-public colord-gtk
  (package
    (name "colord-gtk")
    (version "0.1.26")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.freedesktop.org/software/colord"
                                  "/releases/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0i9y3bb5apj6a0f8cx36l6mjzs7xc0k7nf0magmf58vy2mzhpl18"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f)) ; require the colord system service
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (propagated-inputs
     ;; colord-gtk.pc refers to all these.
     `(("colord" ,colord)
       ("gtk+" ,gtk+)))
    (synopsis "GTK integration for libcolord")
    (home-page "https://www.freedesktop.org/software/colord/")
    (description
     "This is a GTK+ convenience library for interacting with colord.  It is
useful for both applications which need colour management and applications that
wish to perform colour calibration.")
    (license license:lgpl2.1+)))

(define-public libfprint
  (package
    (name "libfprint")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://people.freedesktop.org/~hadess/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1giwh2z63mn45galsjb59rhyrvgwcy01hvvp4g01iaa2snvzr0r5"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list (string-append "--with-udev-rules-dir="
                                              (assoc-ref %outputs "out")
                                              "/lib/udev/rules.d"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libusb" ,libusb)
       ("nss" ,nss)
       ("glib" ,glib)
       ("eudev" ,eudev)
       ("pixman" ,pixman)))
    (home-page "https://www.freedesktop.org/wiki/Software/fprint/libfprint/")
    (synopsis "Library to access fingerprint readers")
    (description
     "libfprint is a library designed to make it easy for application
developers to add support for consumer fingerprint readers to their
software.")
    (license license:lgpl2.1+)))

(define-public fprintd
  (package
    (name "fprintd")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://people.freedesktop.org/~hadess/fprintd-"
                    version ".tar.xz"))
              (sha256
               (base32
                "05915i0bv7q62fqrs5diqwr8dz3pwqa1c1ivcgggkjyw0xk4ldp5"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'build 'set-sysconfdir
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Work around a bug whereby the 'SYSCONFDIR' macro
                      ;; expands literally to '${prefix}/etc'.
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* "src/main.c"
                          (("SYSCONFDIR, \"fprintd.conf\"")
                           (string-append "\"" out "/etc\", "
                                          "\"fprintd.conf\"")))
                        #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("libfprint" ,libfprint)
       ("dbus-glib" ,dbus-glib)
       ("polkit" ,polkit)
       ("linux-pam" ,linux-pam)))                 ;for pam_fprintd
    (home-page "https://www.freedesktop.org/wiki/Software/fprint/fprintd/")
    (synopsis "D-Bus daemon that exposes fingerprint reader functionality")
    (description
     "fprintd is a D-Bus daemon that offers functionality of libfprint, a
library to access fingerprint readers, over the D-Bus interprocess
communication bus.  This daemon layer above libfprint solves problems related
to applications simultaneously competing for fingerprint readers.")
    (license license:gpl2+)))

(define-public desktop-file-utils
  (package
    (name "desktop-file-utils")
    (version "0.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.freedesktop.org/software/" name
                                  "/releases/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "119kj2w0rrxkhg4f9cf5waa55jz1hj8933vh47vcjipcplql02bc"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)))
    (home-page "https://www.freedesktop.org/wiki/Software/desktop-file-utils/")
    (synopsis "Utilities for working with desktop entries")
    (description
     "This package contains a few command line utilities for working with
desktop entries:

desktop-file-validate: validates a desktop file and prints warnings/errors
                       about desktop entry specification violations.

desktop-file-install: installs a desktop file to the applications directory,
                      optionally munging it a bit in transit.

update-desktop-database: updates the database containing a cache of MIME types
                         handled by desktop files.")
    (license license:gpl2+)))

(define-public xdg-user-dirs
  (package
    (name "xdg-user-dirs")
    (version "0.17")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://user-dirs.freedesktop.org/releases/"
                                    name "-" version ".tar.gz"))
              (sha256
               (base32 "13216b8rfkzak5k6bvpx6jvqv3cnbgpijnjwj8a8d3kq4cl0a1ra"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("docbook-xsl" ,docbook-xsl)
       ("docbook-xml" ,docbook-xml-4.3)
       ("xsltproc" ,libxslt)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'locate-catalog-files
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xmldoc (string-append (assoc-ref inputs "docbook-xml")
                                          "/xml/dtd/docbook"))
                   (xsldoc (string-append (assoc-ref inputs "docbook-xsl")
                                          "/xml/xsl/docbook-xsl-"
                                          ,(package-version docbook-xsl))))
               (for-each (lambda (file)
                           (substitute* file
                             (("http://.*/docbookx\\.dtd")
                              (string-append xmldoc "/docbookx.dtd"))))
                         (find-files "man" "\\.xml$"))
               (substitute* "man/Makefile"
                 (("http://.*/docbook\\.xsl")
                  (string-append xsldoc "/manpages/docbook.xsl")))
               #t))))))
    (home-page "https://www.freedesktop.org/wiki/Software/xdg-user-dirs/")
    (synopsis "Tool to help manage \"well known\" user directories")
    (description "xdg-user-dirs is a tool to help manage \"well known\" user
directories, such as the desktop folder or the music folder. It also handles
localization (i.e. translation) of the file names.  Designed to be
automatically run when a user logs in, xdg-user-dirs can also be run
manually by a user.")
    (license license:gpl2)))

(define-public perl-file-basedir
  (package
    (name "perl-file-basedir")
    (version "0.07")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/K/KI/KIMRYAN/"
                           "File-BaseDir-" version ".tar.gz"))
       (sha256
        (base32
         "0aq8d4hsaxqibp36f773y6dfck7zd82v85sp8vhi6pjkg3pmf2hj"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-file-which" ,perl-file-which)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)
       ("xdg-user-dirs" ,xdg-user-dirs)))
    (propagated-inputs
     `(("perl-ipc-system-simple" ,perl-ipc-system-simple)))
    (home-page "http://search.cpan.org/dist/File-BaseDir/")
    (synopsis "Use the Freedesktop.org base directory specification")
    (description
     "@code{File::Basedir} can be used to find directories and files as
specified by the Freedesktop.org Base Directory Specification.  This
specifications gives a mechanism to locate directories for configuration,
application data and cache data.")
    (license license:perl-license)))

(define-public perl-file-desktopentry
  (package
    (name "perl-file-desktopentry")
    (version "0.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MICHIELB/"
                           "File-DesktopEntry-" version ".tar.gz"))
       (sha256
        (base32
         "1f1maqix2kbfg2rf008m7mqnvv6nvcf9y6pcgdv2kxp2vbih370n"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
    (propagated-inputs
     `(("perl-file-basedir" ,perl-file-basedir)
       ("perl-uri" ,perl-uri)))
    (home-page "http://search.cpan.org/~michielb/File-DesktopEntry/")
    (synopsis "Handle @file{.desktop} files")
    (description
     "@code{File::DesktopEntry} parses @file{.desktop} files defined by the
Freedesktop.org @dfn{Desktop Entry} specification.  It can also run the
applications define in those files.")
    (license license:perl-license)))

(define-public perl-file-mimeinfo
  (package
    (name "perl-file-mimeinfo")
    (version "0.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MICHIELB/"
                           "File-MimeInfo-" version ".tar.gz"))
       (sha256
        (base32
         "1ipbh63bkh1r2gy5g7q4bzhki8j29mm1jkhbv60p9vwsdys5s91a"))))
    (build-system perl-build-system)
    ;; If the tests are fixed, add perl-test-pod, perl-test-pod-coverage, and
    ;; perl-test-tiny as native-inputs.
    (propagated-inputs
     `(("shared-mime-info" ,shared-mime-info)
       ("perl-file-desktopentry" ,perl-file-desktopentry)))
    (arguments
     ;; Some tests fail due to requiring the mimetype of perl files to be
     ;; text/plain when they are actually application/x-perl.
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-programs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (prog)
                           (wrap-program (string-append out "/bin/" prog)
                             `("PERL5LIB" ":" prefix
                               (,(string-append (getenv "PERL5LIB") ":" out
                                                "/lib/perl5/site_perl")))))
                         '("mimeopen" "mimetype")))
             #t)))))
    (home-page "http://search.cpan.org/dist/File-MimeInfo/")
    (synopsis "Determine file type from the file name")
    (description
     "@code{File::Mimeinfo} can be used to determine the MIME type of a file.
It tries to implement the Freedesktop specification for a shared MIME
database.

This package also contains two related utilities:

@itemize
@item @command{mimetype} determines a file's MIME type;
@item @command{mimeopen} opens files in an appropriate program according to
their MIME type.
@end itemize")
    (license license:perl-license)))
