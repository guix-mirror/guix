;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015, 2017 Andy Wingo <wingo@pobox.com>
;;; Copyright © 2015, 2016, 2017, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2016, 2017, 2019, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017, 2018, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017, 2020 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Stefan Stefanović <stefanx2ovic@gmail.com>
;;; Copyright © 2019 Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Copyright © 2019, 2020 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Rene Saavedra <pacoon@protonmail.com>
;;; Copyright © 2020 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2020 Anders Thuné <asse.97@gmail.com>
;;; Copyright © 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2021 Brendan Tildesley <mail@brendan.scot>
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
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)                ;intltool
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages video)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public appstream
  (package
    (name "appstream")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.freedesktop.org/software/"
                       "appstream/releases/"
                       "AppStream-" version ".tar.xz"))
       (sha256
        (base32 "09l6ixz1w29pi0nb0flz14m4r3f2hpqpp1fq8y66v9xa4c9fczds"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-libstemmer
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "meson.build"
               (("/usr/include")
                (string-append (assoc-ref inputs "libstemmer")
                               "/include")))
             #t))
         (add-after 'patch-libstemmer 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs/api"
               (substitute* "appstream-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml-4.3")
                                 "/xml/dtd/docbook/"))))
             (for-each (lambda (file)
                         (substitute* file
                           (("http://www.oasis-open.org/docbook/xml/4.5/")
                            (string-append (assoc-ref inputs "docbook-xml")
                                           "/xml/dtd/docbook/"))))
                       (find-files "scripts/desc" "\\.xml$"))
             #t))
         (add-after 'patch-docbook-xml 'disable-failing-tests
           (lambda _
             (substitute* "tests/test-pool.c"
               (("[ \t]*g_test_add_func \\(\"/AppStream/PoolRead?.*;")
                "")
               (("[ \t]*g_test_add_func \\(\"/AppStream/PoolReadAsync?.*;")
                "")
               (("[ \t]*g_test_add_func \\(\"/AppStream/PoolEmpty?.*;")
                "")
               (("[ \t]*g_test_add_func \\(\"/AppStream/Cache?.*;")
                "")
               (("[ \t]*g_test_add_func \\(\"/AppStream/Merges?.*;")
                ""))
             #t))
         (add-after 'disable-failing-tests 'patch-install-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "data/meson.build"
               (("/etc")
                (string-append (assoc-ref outputs "out")
                               "/etc")))
             #t)))))
    (native-inputs
     `(("cmake" ,cmake)
       ("docbook-xml-4.3" ,docbook-xml-4.3)
       ("docbook-xml" ,docbook-xml)
       ("docbook-xsl" ,docbook-xsl)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gperf" ,gperf)
       ("gtk-doc" ,gtk-doc/stable)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("libsoup" ,libsoup)
       ("libstemmer" ,libstemmer)
       ("libxml2" ,libxml2)
       ("libyaml" ,libyaml)
       ("lmdb" ,lmdb)))
    (propagated-inputs
     `(("glib" ,glib)))
    (synopsis "Tools and libraries to work with AppStream metadata")
    (description "AppStream is a cross-distribution effort for enhancing the way
we interact with the software repositories provided by distributions by
standardizing software component metadata.  It provides the foundation to build
software-center applications, by providing metadata necessary for an
application-centric view on package repositories.  It additionally provides
specifications for things like an unified software metadata database, screenshot
services and various other things needed to create user-friendly
application-centers for distributions.")
    (home-page "https://www.freedesktop.org/wiki/Distributions/AppStream/")
    ;; XXX: meson.build claims both, headers just indicate lgpl2.1+
    ;;      there are also some (irrelevant) wtfpl2 examples
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public farstream
  (package
    (name "farstream")
    (version "0.2.9")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://gitlab.freedesktop.org/farstream/farstream.git")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sd8syldyq6bphfdm129s3gq554vfv7vh1vcwzk48gjryf101awk"))
       (patches
        (search-patches "farstream-make.patch"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:tests? #f ; https://gitlab.freedesktop.org/farstream/farstream/-/issues/18
       #:configure-flags
       (list
        "--enable-gtk-doc"
        "--enable-glib-asserts"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'copy-common
           (lambda _
             (delete-file "autogen.sh")
             (copy-recursively
              (assoc-ref %build-inputs "common")
              "common")
             #t))
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "docs"
               (substitute* '("libs/farstream-libs-docs.sgml"
                              "plugins/farstream-plugins-docs.sgml")
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("common"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://gitlab.freedesktop.org/gstreamer/common.git")
             (commit "88e512ca7197a45c4114f7fa993108f23245bf50")))
           (file-name
            (git-file-name "common" "latest.88e512c"))
           (sha256
            (base32 "1nk94pnskjyngqcfb9p32g4yvf4nzpjszisw24r9azl0pawqpsn6"))))
       ("docbook-xml" ,docbook-xml-4.1.2)
       ("docbook-xsl" ,docbook-xsl)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc/stable)
       ("libtool" ,libtool)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("gupnp-igd" ,gupnp-igd)
       ("libnice" ,libnice)))
    (propagated-inputs
     `(("gstreamer" ,gstreamer)
       ("gst-plugins-bad" ,gst-plugins-bad)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-good" ,gst-plugins-good)))
    (synopsis "The Farstream VVoIP framework")
    (description "Farstream is a collection of GStreamer modules and libraries
for videoconferencing.")
    (home-page "https://www.freedesktop.org/wiki/Software/Farstream/")
    (license license:lgpl2.1+)))

(define-public libglib-testing
  (package
    (name "libglib-testing")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/pwithnall/libglib-testing.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xmycsrlqyji6sc2i4wvp2gxf3897z65a57ygihfnpjpyl7zlwkr"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-before
             'check 'pre-check
           (lambda _
             ;; The test suite requires a running dbus-daemon.
             (system "dbus-daemon &")
             ;; Don't fail on missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("gtk-doc" ,gtk-doc/stable)))
    (inputs
     `(("dbus" ,dbus)
       ("glib" ,glib)))
    (synopsis "Glib testing library")
    (description "Libglib-testing is a test library providing test harnesses and
mock classes which complement the classes provided by GLib.  It is intended to
be used by any project which uses GLib and which wants to write internal unit
tests.")
    (home-page "https://gitlab.gnome.org/pwithnall/libglib-testing")
    (license license:lgpl2.1+)))

(define-public malcontent
  (package
    (name "malcontent")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/pwithnall/malcontent.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vnf0pk516fwwh41v96c29l2i7h1pnwhivlkbf53kkx1q35g7lb3"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         ;; AppInfo not available inside build environment.
         (add-after 'unpack 'fix-tests
           (lambda _
             (substitute* "libmalcontent/tests/app-filter.c"
               (("g_test_add_func \\(\"/app-filter/appinfo\", test_app_filter_appinfo\\);")
                 ""))
             #t)))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("itstool" ,itstool)
       ("libglib-testing" ,libglib-testing)
       ("libxml2" ,libxml2)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("accountsservice" ,accountsservice)
       ("appstream-glib" ,appstream-glib)
       ("dbus" ,dbus)
       ("flatpak" ,flatpak)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libostree" ,libostree)
       ("linux-pam" ,linux-pam)
       ("polkit" ,polkit)))
    (synopsis "Parental controls support")
    (description "MalContent implements parental controls support which can
be used by applications to filter or limit the access of child accounts to
inappropriate content.")
    (home-page "https://gitlab.freedesktop.org/pwithnall/malcontent")
    (license
     (list
      license:gpl2+
      license:lgpl2.1+))))

(define-public xdg-utils
  (package
    (name "xdg-utils")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
          (uri (string-append
                 "https://portland.freedesktop.org/download/xdg-utils-"
                 version ".tar.gz"))
          (sha256
            (base32
             "1nai806smz3zcb2l5iny4x7li0fak0rzmjg6vlyhdqm8z25b166p"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("docbook-xsl" ,docbook-xsl)
       ("docbook-xml" ,docbook-xml-4.1.2)
       ("libxslt" ,libxslt)
       ("w3m" ,w3m)
       ("xmlto" ,xmlto)))
    (inputs
     `(("awk" ,gawk)
       ("coreutils" ,coreutils)
       ("grep" ,grep)
       ("inetutils" ,inetutils) ; xdg-screensaver uses `hostname'
       ("perl-file-mimeinfo" ,perl-file-mimeinfo) ; for mimeopen fallback
       ("sed" ,sed)
       ("xprop" ,xprop) ; for Xfce detecting
       ("xset" ,xset))) ; for xdg-screensaver
    (arguments
     `(#:tests? #f   ; no check target
       #:modules ((srfi srfi-26)
                  ,@%gnu-build-system-modules)
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
               #t)))
         (add-after 'install 'wrap-executables
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion (string-append out "/bin")
                 (let ((path-ext
                        (map (cute string-append <> "/bin")
                             (cons out
                                   (map (cute assoc-ref inputs <>)
                                        '("awk" "coreutils" "grep" "inetutils"
                                          "perl-file-mimeinfo" "sed" "xprop"
                                          "xset"))))))
                   (for-each (cute wrap-program <>
                                   `("PATH" ":" prefix ,path-ext))
                             (find-files "."))))
               #t))))))
    (home-page "https://www.freedesktop.org/wiki/Software/xdg-utils/")
    (synopsis "Freedesktop.org scripts for desktop integration")
    (description "The xdg-utils package is a set of simple scripts that
provide basic desktop integration functions in the framework of the
freedesktop.org project.")
    (license license:expat)))

(define-public libinput
  ;; Updating this will rebuild over 700 packages through libinput-minimal.
  (package
    (name "libinput")
    (version "1.16.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://freedesktop.org/software/libinput/"
                                  "libinput-" version ".tar.xz"))
              (sha256
               (base32
                "0acywdjppj5i591l879bnqa9cs4vgdwnhilwk550x5x8sl33m4k5"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Ddocumentation=false")

       ;; XXX: Using 'debug' or 'debugoptimized' pulls in an additional test that
       ;; hangs, and the comments around it suggests that we should be using this
       ;; Meson target anyway.
       #:build-type "release"))
    (native-inputs
     `(("check" ,check)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("cairo" ,cairo)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libevdev" ,libevdev)
       ("libwacom" ,libwacom)
       ("mtdev" ,mtdev)))
    (propagated-inputs
     `(;; libinput.h requires <libudev.h>, so propagate it.
       ("udev" ,eudev)))
    (home-page "https://www.freedesktop.org/wiki/Software/libinput/")
    (synopsis "Input devices handling library")
    (description
     "Libinput is a library to handle input devices for display servers and
other applications that need to directly deal with input devices.")
    (license license:x11)))

(define-public libinput-minimal
  (package/inherit libinput
    (name "libinput-minimal")
    (inputs
     (fold alist-delete (package-inputs libinput)
           '("cairo" "glib" "gtk+" "libwacom")))
    (arguments
     (substitute-keyword-arguments (package-arguments libinput)
      ((#:configure-flags flags ''())
       `(cons* "-Dlibwacom=false"
               "-Ddebug-gui=false"    ;requires gtk+@3
               ,flags))))))

(define-public libxdg-basedir
  (package
    (name "libxdg-basedir")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/devnev/libxdg-basedir")
                     (commit (string-append name "-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12yz53ny5bi2dii3zwcr6b9ay0yy1g1xv13jg097k7gjligcq11m"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-autogen
           (lambda _
             ;; Run 'configure' in its own phase, not now.
             (substitute* "autogen.sh"
               (("^.*\\./configure.*") ""))
             #t)))))
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
    (version "243.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/elogind/elogind")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ccj3cbs9nsfg497wg195in1a7b9csm1jdm7z6q7vvx1ynpjxlxz"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (let* ((out (assoc-ref %outputs "out"))
              (sysconf (string-append out "/etc"))
              (libexec (string-append out "/libexec/elogind"))
              (dbuspolicy (string-append out "/etc/dbus-1/system.d"))
              (shadow (assoc-ref %build-inputs "shadow"))
              (shepherd (assoc-ref %build-inputs "shepherd"))
              (halt-path (string-append shepherd "/sbin/halt"))
              (kexec-path "")           ;not available in Guix yet
              (nologin-path (string-append shadow "/sbin/nologin"))
              (poweroff-path (string-append shepherd "/sbin/shutdown"))
              (reboot-path (string-append shepherd "/sbin/reboot")))
         (list
          (string-append "-Drootprefix=" out)
          (string-append "-Dsysconfdir=" sysconf)
          (string-append "-Drootlibexecdir=" libexec)
          (string-append "-Ddbuspolicydir=" dbuspolicy)
          (string-append "-Dc_link_args=-Wl,-rpath=" libexec)
          (string-append "-Dcpp_link_args=-Wl,-rpath=" libexec)
          (string-append "-Dhalt-path=" halt-path)
          (string-append "-Dkexec-path=" kexec-path)
          (string-append "-Dpoweroff-path=" poweroff-path)
          (string-append "-Dreboot-path=" reboot-path)
          (string-append "-Dnologin-path=" nologin-path)
          "-Dcgroup-controller=elogind"
          "-Dman=true"
          ;; Disable some tests.
          "-Dslow-tests=false"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-pkttyagent-path
           (lambda _
             (substitute* "meson.build"
               (("join_paths\\(bindir, 'pkttyagent'\\)")
                "'\"/run/current-system/profile/bin/pkttyagent\"'"))
             #t))
         (add-after 'unpack 'adjust-tests
           (lambda _
             ;; This test tries to copy some bytes from /usr/lib/os-release,
             ;; which does not exist in the build container.  Choose something
             ;; more likely to be available.
             (substitute* "src/test/test-copy.c"
               (("/usr/lib/os-release")
                "/etc/passwd"))
             ;; Use a shebang that works in the build container.
             (substitute* "src/test/test-exec-util.c"
               (("#!/bin/sh")
                (string-append "#!" (which "sh"))))
             ;; Do not look for files or directories that do not exist.
             (substitute* "src/test/test-fs-util.c"
               (("usr") "etc")
               (("/etc/machine-id") "/etc/passwd"))
             ;; FIXME: Why is sd_id128_get_machine_app_specific failing.
             ;; Disable for now by hooking into the kernel support check.
             (substitute* "src/test/test-id128.c"
               (("if \\(r == -EOPNOTSUPP\\)")
                "if (1)"))
             ;; This test expects that /sys is available.
             (substitute* "src/test/test-mountpoint-util.c"
               (("assert_se\\(path_is_mount_point\\(\"/sys.*")
                ""))
             ;; /bin/sh does not exist in the build container.
             (substitute* "src/test/test-path-util.c"
               (("/bin/sh") (which "sh")))
             ;; This test uses sd_device_new_from_syspath to allocate a
             ;; loopback device, but that fails because /sys is unavailable.
             (substitute* "src/libelogind/sd-device/test-sd-device-thread.c"
               ((".*sd_device_new_from_syspath.*/sys/class/net/lo.*")
                "return 77;"))
             ;; Most of these tests require cgroups or an actual live
             ;; logind system so that it can flicker the monitor, etc.
             ;; Just skip it until a more narrow selection can be made.
             (substitute* "src/libelogind/sd-login/test-login.c"
               (("r = sd_pid_get_slice.*")
                "return 77;"))
             #t))
         (add-after 'unpack 'change-pid-file-path
           (lambda _
             (substitute* "src/login/elogind.c"
               (("\"/run/elogind.pid\"") "\"/run/systemd/elogind.pid\""))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml)
       ("docbook-xml-4.2" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)
       ("gettext" ,gettext-minimal)
       ("gperf" ,gperf)
       ("libxml2" ,libxml2)                     ;for XML_CATALOG_FILES
       ("m4" ,m4)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("linux-pam" ,linux-pam)
       ("libcap" ,libcap)
       ("shadow" ,shadow)                    ;for 'nologin'
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

(define-public localed
  ;; XXX: This package is extracted from systemd but we retain so little of it
  ;; that it would make more sense to maintain a fork of the bits we need.
  (package
    (name "localed")
    (version "241")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/systemd/systemd")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0sy91flzbhpq58k7v0294pa2gxpr0bk27rcnxlbhk2fi6nc51d28"))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Connect to the right location for our D-Bus daemon.
                  (substitute* '("src/basic/def.h"
                                 "src/libsystemd/sd-bus/sd-bus.c"
                                 "src/stdio-bridge/stdio-bridge.c")
                    (("/run/dbus/system_bus_socket")
                     "/var/run/dbus/system_bus_socket"))

                  ;; Don't insist on having systemd as PID 1 (otherwise
                  ;; 'localectl' would exit without doing anything.)
                  (substitute* "src/shared/bus-util.c"
                    (("sd_booted\\(\\)")
                     "(1)"))
                  #t))
              (patches (search-patches "localed-xorg-keyboard.patch"))))
    (build-system meson-build-system)
    (arguments
     ;; Try to build as little as possible (list of components taken from the
     ;; top-level 'meson.build' file.)
     (let ((components '("utmp"
                         "hibernate"
                         "environment-d"
                         "binfmt"
                         "coredump"
                         "resolve"
                         "logind"
                         "hostnamed"
                         "localed"
                         "machined"
                         "portabled"
                         "networkd"
                         "timedated"
                         "timesyncd"
                         "firstboot"
                         "randomseed"
                         "backlight"
                         "vconsole"
                         "quotacheck"
                         "sysusers"
                         "tmpfiles"
                         "hwdb"
                         "rfkill"
                         "ldconfig"
                         "efi"
                         "tpm"
                         "ima"
                         "smack"
                         "gshadow"
                         "idn"
                         "nss-myhostname"
                         "nss-systemd")))
       `(#:configure-flags ',(map (lambda (component)
                                    (string-append "-D" component "=false"))
                                  (delete "localed" components))

         ;; It doesn't make sense to test all of systemd.
         #:tests? #f

         #:phases (modify-phases %standard-phases
                    (add-after 'unpack 'set-xkeyboard-config-file-name
                      (lambda* (#:key inputs #:allow-other-keys)
                        ;; Set the file name to xkeyboard-config and kbd.
                        ;; This is used by 'localectl list-x11-keymap-layouts'
                        ;; and similar functions.
                        (let ((xkb (assoc-ref inputs "xkeyboard-config"))
                              (kbd (assoc-ref inputs "kbd")))
                          (substitute* "src/locale/localectl.c"
                            (("/usr/share/X11/xkb/rules")
                             (string-append xkb "/share/X11/xkb/rules")))
                          (substitute* "src/basic/def.h"
                            (("/usr/share/keymaps")
                             (string-append kbd "/share/keymaps")))
                          #t)))
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        ;; Install 'localed', the D-Bus and polkit files, and
                        ;; 'localectl'.
                        (let* ((out (assoc-ref outputs "out"))
                               (libexec (string-append out "/libexec/localed"))
                               (bin     (string-append out "/bin"))
                               (lib     (string-append out "/lib"))
                               (dbus    (string-append out
                                                       "/share/dbus-1/system-services"))
                               (conf    (string-append out
                                                       "/etc/dbus-1/system.d/"))
                               (polkit  (string-append out
                                                       "/share/polkit-1/actions"))
                               (data    (string-append out "/share/systemd")))
                          (define (source-file regexp)
                            (car (find-files ".." regexp)))

                          (mkdir-p libexec)
                          (copy-file "systemd-localed"
                                     (string-append libexec "/localed"))
                          (install-file "localectl" bin)

                          (let ((service-file (source-file
                                               "\\.locale1\\.service$")))
                            (substitute* service-file
                              (("^Exec=.*$")
                               (string-append "Exec=" libexec "/localed\n")))
                            (install-file service-file dbus))
                          (install-file (source-file "\\.locale1\\.policy$")
                                        polkit)
                          (install-file (source-file "\\.locale1\\.conf$")
                                        conf)
                          (for-each (lambda (file)
                                      (install-file file lib))
                                    (find-files "src/shared"
                                                "libsystemd-shared.*\\.so"))

                          (for-each (lambda (map)
                                      (install-file map data))
                                    (find-files ".." "^(kbd-model-map|language-fallback-map)$"))
                          #t)))))))
    (native-inputs (package-native-inputs elogind))
    (inputs `(("libmount" ,util-linux "lib")
              ("xkeyboard-config" ,xkeyboard-config)
              ("kbd" ,kbd)
              ,@(package-inputs elogind)))
    (home-page "https://www.freedesktop.org/wiki/Software/systemd/localed/")
    (synopsis "Control the system locale and keyboard layout")
    (description
     "Localed is a tiny daemon that can be used to control the system locale
and keyboard mapping from user programs.  It is used among other things by the
GNOME Shell.  The @command{localectl} command-line tool allows you to interact
with localed.  This package is extracted from the broader systemd package.")
    (license license:lgpl2.1+)))

(define-public packagekit
  (package
    (name "packagekit")
    (version "1.1.13")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://www.freedesktop.org/software/"
                   "PackageKit/releases/"
                   "PackageKit-" version ".tar.xz"))
             (sha256
              (base32
               "1dr1laic65ld95abp2yxbwvijnngh0dwyb1x49x4wjm5rhq43dl8"))))
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
       ("bash-completion" ,bash-completion)
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
    (version "0.27")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyxdg" version))
       (sha256
        (base32
         "19f5j5mxp7ff0vp33s32qbpdi65iiwha0bj641gl70pdwnm97gc0"))))
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
                               "/share/icons/hicolor/index.theme")))

             ;; These two tests are known to fail in strange ways.
             (substitute* "test/test-mime.py"
               (("def test_get_type\\(self") "def _test_get_type(self")
               (("def test_get_type2\\(self") "def _test_get_type2(self"))

             ;; There are test files not shipped in the release tarball
             (substitute* "test/test-icon.py"
               (("def test_validate_icon_theme") "def _test_validate_icon_theme"))
             (invoke "nosetests" "-v"))))))
    (native-inputs
     ;; For tests.
     `(("shared-mime-info" ,shared-mime-info)
       ("hicolor-icon-theme" ,hicolor-icon-theme)
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
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://wayland.freedesktop.org/releases/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0k995rn96xkplrapz5k648j651wc43kq817xk1x8280h16gsfxa6"))))
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
    (version "1.20")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://wayland.freedesktop.org/releases/"
                    "wayland-protocols-" version ".tar.xz"))
              (sha256
               (base32
                "1rsdgvkkvxs3cjhpl6agvbkm53vm7k8rg127j9y2vn33m2hvg0lp"))))
    (build-system gnu-build-system)
    (inputs
     `(("wayland" ,wayland)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Wayland protocols")
    (description "This package contains XML definitions of the Wayland protocols.")
    (home-page "https://wayland.freedesktop.org")
    (license license:expat)))

(define-public waylandpp
  (package
    (name "waylandpp")
    (version "0.2.8")
    (home-page "https://github.com/NilsBrause/waylandpp")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kxiqab48p0n97pwg8c2zx56wqq32m3rcq7qd2pjj33ipcanb3qq"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; no tests
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("mesa" ,mesa)
       ("pugixml" ,pugixml)))
    (propagated-inputs
     `(;; In Requires of the .pc files.
       ("wayland" ,wayland)))
    (synopsis "Wayland C++ bindings")
    (description
     "This package provides C++ bindings for the Wayland display protocol.")
    (license license:bsd-2)))

(define-public weston
  (package
    (name "weston")
    (version "6.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://wayland.freedesktop.org/releases/"
                    "weston-" version ".tar.xz"))
              (sha256
               (base32
                "1d2m658ll8x7prlsfk71qgw89c7dz6y7d6nndfxwl49fmrd6sbxz"))))
    (build-system meson-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("xorg-server" ,xorg-server)))
    (inputs
     `(("cairo" ,cairo-xcb)
       ("colord" ,colord)
       ("dbus" ,dbus)
       ("elogind" ,elogind)
       ("lcms" ,lcms)
       ("libevdev" ,libevdev)
       ("libinput" ,libinput-minimal)
       ("libjpeg" ,libjpeg-turbo)
       ("libunwind" ,libunwind)
       ("libva" ,libva)
       ("libwebp" ,libwebp)
       ("libxcursor" ,libxcursor)
       ("libxkbcommon" ,libxkbcommon)
       ("libxml2" ,libxml2)
       ("mesa" ,mesa)
       ("mtdev" ,mtdev)
       ("linux-pam" ,linux-pam)
       ("pango" ,pango)
       ("wayland" ,wayland)
       ("wayland-protocols" ,wayland-protocols)
       ("xorg-server-xwayland" ,xorg-server-xwayland)))
    (arguments
     `(#:configure-flags
       (list "-Dbackend-rdp=false" ; TODO: Enable.
             "-Dremoting=false" ; TODO: Enable.
             "-Dsimple-dmabuf-drm=auto"
             "-Dsystemd=false"
             (string-append "-Dxwayland-path="
                            (assoc-ref %build-inputs "xorg-server-xwayland")
                            "/bin/Xwayland"))
       #:parallel-tests? #f ; Parallel tests cause failures.
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'use-elogind
           (lambda _
             ;; Use elogind instead of systemd
             (substitute* "libweston/meson.build"
               (("libsystemd-login") "libelogind"))
             (substitute* '("libweston/launcher-logind.c"
                            "libweston/weston-launch.c")
               (("#include <systemd/sd-login.h>")
                "#include <elogind/sd-login.h>"))
             #t))
         (add-after 'configure 'patch-confdefs.h
           (lambda _
             (system "echo \"#define HAVE_SYSTEMD_LOGIN_209 1\" >> confdefs.h")
             #t))
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

(define-public wev
  (package
    (name "wev")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~sircmpwn/wev")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0l71v3fzgiiv6xkk365q1l08qvaymxd4kpaya6r2g8yzkr7i2hms"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:make-flags
       (list "CC=gcc" (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("scdoc" ,scdoc)))
    (inputs
     `(("libxkbcommon" ,libxkbcommon)
       ("wayland" ,wayland)
       ("wayland-protocols" ,wayland-protocols)))
    (home-page "https://git.sr.ht/~sircmpwn/wev")
    (synopsis "Wayland event viewer")
    (description "Wev is a tool that opens a window, printing all events
sent to a Wayland window, such as key presses.  It is analogous to the X11 tool
XEv.")
    (license license:expat)))

(define-public exempi
  (package
    (name "exempi")
    (version "2.5.2")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://libopenraw.freedesktop.org/download/"
                   name "-" version ".tar.bz2"))
             (sha256
              (base32
               "1mdfxb36p8251n5m7l55gx3fcqpk46yz9v568xfr8igxmqa47xaj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list (string-append "--with-boost="
                                              (assoc-ref %build-inputs "boost")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'remove-static-library
           (lambda* (#:key outputs #:allow-other-keys)
             ;; XXX: Some tests fail to build with --disable-static due to
             ;; symbols not being visible in the shared library:
             ;; <https://gitlab.freedesktop.org/libopenraw/exempi/-/issues/17>.
             ;; Simply delete the static library instead to save ~4.3 MiB.
             (delete-file (string-append (assoc-ref outputs "out")
                                         "/lib/libexempi.a"))
             #t)))))
    (native-inputs
     `(("boost" ,boost))) ; tests
    (inputs
     `(("expat" ,expat)
       ("zlib" ,zlib)))
    (home-page "https://libopenraw.freedesktop.org/exempi/")
    (synopsis "XMP metadata handling library")
    (description "Exempi is an implementation of the Extensible Metadata
Platform (@dfn{XMP}), which enables embedding metadata in PDF and image
formats.")
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
    (version "2.8.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/storaged-project/udisks/releases/download/udisks-"
                    version "/udisks-" version ".tar.bz2"))
              (sha256
               (base32
                "06cq52kp1nyy15qzylywy9s7hhhqc45k0s3y68crf0zsmjyng0yj"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.3) ; to build the manpages
       ("docbook-xsl" ,docbook-xsl)
       ("glib:bin" ,glib "bin")         ; for glib-mkenums
       ("gnome-common" ,gnome-common)   ; TODO: Why is this needed?
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc/stable)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     `(("glib" ,glib))) ; required by udisks2.pc
    (inputs
     `(("acl" ,acl)
       ("cryptsetup" ,cryptsetup)
       ("libatasmart" ,libatasmart)
       ("libblockdev" ,libblockdev)
       ("libgudev" ,libgudev)
       ("polkit" ,polkit)
       ("util-linux" ,util-linux)))
    (outputs '("out"
               "doc"))                            ;5 MiB of gtk-doc HTML
    (arguments
     `(#:tests? #f ; requiring system message dbus
       #:disallowed-references ("doc")            ;enforce separation of "doc"
       #:configure-flags
       (list "--enable-man"
             "--enable-available-modules" ; Such as lvm2, btrfs, etc.
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
    (version "0.6.50")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.freedesktop.org/software/"
                           "accountsservice/accountsservice-" version ".tar.xz"))
       (sha256
        (base32 "0jn7vg1z4vxnna0hl33hbcb4bb3zpilxc2vyclh24vx4vvsjhn83"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; XXX: tests require DocBook 4.1.2
       #:configure-flags
       '("--localstatedir=/var"
         "--disable-systemd"
         "--enable-elogind")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/cat
           (lambda _
             (substitute* "src/user.c"
               (("/bin/cat") (which "cat")))
             #t))
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
     `(("elogind" ,elogind)
       ("polkit" ,polkit)
       ("shadow" ,shadow)))
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
    (version "1.20.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.freedesktop.org/software/libmbim/"
                    "libmbim-" version ".tar.xz"))
              (sha256
               (base32
                "16q550sy84izi5ic3sbbhjnnka2fwhj8vvdrirpn9xspbsgbc3sm"))))
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
    (version "1.24.14")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.freedesktop.org/software/libqmi/"
                    "libqmi-" version ".tar.xz"))
              (sha256
               (base32
                "0zshxqbm9ldybgrzh7pjmwmfjvvvfd0xh8qhgl8xiqdb9ply73r0"))))
    (build-system gnu-build-system)
    (inputs
     `(("libgudev" ,libgudev)))
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
    (version "1.12.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.freedesktop.org/software/ModemManager/"
                    "ModemManager-" version ".tar.xz"))
              (sha256
               (base32
                "1apq9camys2gaw6y6ic1ld20cncfwpmxnzvh4j5zkbbjpf5hbcxj"))))
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
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TelepathyIM/telepathy-idle")
             (commit (string-append "telepathy-idle-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pfw4g2cicw3ykxhsy743r0fc1yqbdrqxh2c5ha6am19dajcr95l"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
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
    (version "5.16.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://telepathy.freedesktop.org/releases/"
                           "telepathy-mission-control/"
                           "telepathy-mission-control-" version ".tar.gz"))
       (sha256
        (base32 "00xxv38cfdirnfvgyd56m60j0nkmsv5fz6p2ydyzsychicxl6ssc"))))
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
    (version "1.90.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/libfprint/libfprint")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00pmdpxxjj4sh8qjq9ch3pylgg9w019rg1bbaw53a4wr637rrz43"))))
    (build-system meson-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "-Dudev_rules_dir=" (assoc-ref %outputs "out")
                            "/lib/udev/rules.d"))))
    (native-inputs
     `(("eudev" ,eudev)
       ("glib:bin" ,glib "bin")         ; for {glib-,}mkenums
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc/stable)             ; for 88 KiB of API documentation
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("gusb" ,gusb)
       ("nss" ,nss)                     ; for the URU4x00 driver

       ;; Replacing this with cairo works but just results in a reference
       ;; (only) to pixman in the end.
       ("pixman" ,pixman)))
    (home-page "https://fprint.freedesktop.org/")
    (synopsis "Library to access fingerprint readers")
    (description
     "libfprint is a library designed to make it easy for application
developers to add support for consumer fingerprint readers to their
software.")
    (license license:lgpl2.1+)))

(define-public fprintd
  (package
    (name "fprintd")
    (version "1.90.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/libfprint/fprintd")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mbzk263x7f58i9cxhs44mrngs7zw5wkm62j5r6xlcidhmfn03cg"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (list "-Dsystemd_system_unit_dir=/tmp"
             (string-append "-Ddbus_service_dir=" (assoc-ref %outputs "out")
                            "/share/dbus-1/system-services")
             (string-append "-Dpam_modules_dir=" (assoc-ref %outputs "out")
                            "/lib/security"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-output-directories
           ;; Install files to our output, not that of the ‘owner’ package.
           ;; These are not exposed as Meson options and must be patched.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "meson.build"
                 (("(dbus_interfaces_dir = ).*" _ set)
                  (string-append set "'" out "/share/dbus-1/interfaces'\n"))
                 (("(polkit_policy_directory = ).*" _ set)
                  (string-append set "'" out "/share/polkit-1/actions/'\n"))
                 (("(dbus_data_dir = ).*" _ set)
                  (string-append set "get_option('prefix')"
                                 " / get_option('datadir')\n")))
               #t)))
         (add-before 'configure 'patch-mistake
           (lambda _
             (substitute* "meson.build"
               (("(storage_path = )(get_option\\('prefix'\\))(.*)"
                 _ set mistake value)
                (string-append set "''" value "\n")))
             #t))
         (add-before 'configure 'patch-systemd-dependencies
           (lambda _
             (substitute* "meson.build"
               (("'(libsystemd|systemd)'") "'libelogind'"))
             #t))
         (add-before 'configure 'ignore-test-dependencies
           (lambda _
             (substitute* "meson.build"
               (("pam_wrapper_dep .*") "")
               ((".*'(cairo|dbus|dbusmock|gi|pypamtest)': .*,.*") ""))
             #t))
         (add-before 'install 'no-polkit-magic
           ;; Meson ‘magically’ invokes pkexec, which fails (not setuid).
           (lambda _
             (setenv "PKEXEC_UID" "something")
             #t)))
       #:tests? #f))                    ; XXX depend on unpackaged packages
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")         ; for glib-genmarshal
       ("libxslt" ,libxslt)             ; for xsltproc
       ("perl" ,perl)                   ; for pod2man
       ("pkg-config" ,pkg-config)))
       ;; For tests.
       ;;("pam_wrapper" ,pam_wrapper)
       ;;("python-pycairo" ,python-pycairo)
       ;;("python-dbus" ,python-dbus)
       ;;("python-dbusmock" ,python-dbusmock)
       ;;("python-pygobject" ,python-pygobject)
       ;;("python-pypamtest" ,python-pypamtest)
    (inputs
     `(("dbus-glib" ,dbus-glib)
       ("elogind" ,elogind)
       ("libfprint" ,libfprint)
       ("linux-pam" ,linux-pam)
       ("polkit" ,polkit)

       ;; XXX These are in libfprint's Requires.private.  Meson refuses to grant
       ;; the ‘libfprint-2’ dependency if they are not provided here.
       ("gusb" ,gusb)
       ("nss" ,nss)
       ("pixman" ,pixman)))
    (home-page "https://fprint.freedesktop.org/")
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
    (version "0.26")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.freedesktop.org/software/"
                                  "desktop-file-utils/releases/"
                                  "desktop-file-utils-" version ".tar.xz"))
              (sha256
               (base32
                "02bkfi6fyk4c0gh2avd897882ww5zl7qg7bzzf28qb57kvkvsvdj"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
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
    (version "0.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/K/KI/KIMRYAN/"
                           "File-BaseDir-" version ".tar.gz"))
       (sha256
        (base32
         "1qq5ag9zffx8zc5i9b4z03ar80pqj4drgk3vjdlyfapjwb9zqrf0"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-file-which" ,perl-file-which)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-pod-coverage" ,perl-test-pod-coverage)
       ("xdg-user-dirs" ,xdg-user-dirs)))
    (propagated-inputs
     `(("perl-ipc-system-simple" ,perl-ipc-system-simple)))
    (home-page "https://metacpan.org/release/File-BaseDir")
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
    (home-page "https://metacpan.org/release/File-DesktopEntry")
    (synopsis "Handle @file{.desktop} files")
    (description
     "@code{File::DesktopEntry} parses @file{.desktop} files defined by the
Freedesktop.org @dfn{Desktop Entry} specification.  It can also run the
applications define in those files.")
    (license license:perl-license)))

(define-public perl-file-mimeinfo
  (package
    (name "perl-file-mimeinfo")
    (version "0.29")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MICHIELB/"
                           "File-MimeInfo-" version ".tar.gz"))
       (sha256
        (base32
         "1sh8r6vczyz08zm8vfsjmkg6a165wch54akjdrd1vbifcmwjg5pi"))))
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
    (home-page "https://metacpan.org/release/File-MimeInfo")
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

(define-public uchardet
  (package
    (name "uchardet")
    (version "0.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://www.freedesktop.org/software/"
                            name "/releases/" name "-" version ".tar.xz"))
        (sha256
          (base32 "1ca51sryhryqz82v4d0graaiqqq5w2f33a9gj83b910xmq499irz"))))
    (build-system cmake-build-system)
    (home-page "https://www.freedesktop.org/wiki/Software/uchardet/")
    (synopsis "Encoding detector library")
    (description "uchardet is an encoding detector library, which takes a
sequence of bytes in an unknown character encoding without any additional
information, and attempts to determine the encoding of the text.  Returned
encoding names are iconv-compatible.")

    ;; This combines code under MPL 1.1, LGPL 2.1+, and GPL 2.0+, so the
    ;; combination is GPL 2.0+.
    (license license:gpl2+)))

(define-public python-cchardet
  (package
  (name "python-cchardet")
  (version "2.1.7")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "cchardet" version))
      (sha256
        (base32
          "1bqfz85cd51sw0bvhvdq9ikccxi2ld7g5jpni4jkq1a5clrvca64"))))
  (build-system python-build-system)
  (inputs
   `(("uchardet" ,uchardet)))
  (home-page "https://github.com/PyYoshi/cChardet")
  (synopsis "High-performance character encoding detection for Python")
  (description "cChardet is a character encoding detector, written in
Python, that binds to the C library @code{uchardet} to increase performance.")
  (license license:gpl2+)))

(define-public udiskie
  (package
    (name "udiskie")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "udiskie" version))
       (sha256
        (base32
         "0smib8vbs9q37n7ynhzyw97q16fgdkcdw7fw69lci0xvyq00v1dz"))
       ;; Remove support for the libappindicator library of the
       ;; Unity desktop environment which is not in Guix.
       (patches (search-patches "udiskie-no-appindicator.patch"))))
    (build-system python-build-system)
    (native-inputs
     `(("asciidoc" ,asciidoc)
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("gtk+" ,gtk+)
       ("libnotify" ,libnotify)
       ("udisks" ,udisks)))
    (propagated-inputs
     `(("python-docopt" ,python-docopt)
       ("python-pygobject" ,python-pygobject)
       ("python-keyutils" ,python-keyutils)
       ("python-pyxdg" ,python-pyxdg)
       ("python-pyyaml" ,python-pyyaml)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-gi-typelib
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
               (wrap-program (string-append out "/bin/udiskie")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
             #t)))))
    (home-page "https://github.com/coldfix/udiskie")
    (synopsis "Automounter for removable media")
    (description
     "The @command{udiskie} program is a udisks2 front-end that
manages removable media such as CDs or flash drives from userspace.

Its features include:

@itemize
@item automount removable media,
@item notifications,
@item tray icon,
@item command line tools for manual (un)mounting,
@item LUKS encrypted devices,
@item unlocking with keyfiles,
@item loop devices (mounting ISO archives),
@item password caching.
@end itemize
")
    (license license:expat)))

(define-public plymouth
  (package
    (name "plymouth")
    (version "0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.freedesktop.org/software/"
                           "plymouth/releases/" name "-" version ".tar.xz"))
       (sha256
        (base32
         "0l8kg7b2vfxgz9gnrn0v2w4jvysj2cirp0nxads5sy05397pl6aa"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-logo="
                            "/etc/plymouth/logo.png")
             (string-append "--with-background-color="
                            "0x00ff00")
             (string-append "--with-background-start-color-stop="
                            "0xff0000")
             (string-append "--with-background-end-color-stop="
                            "0x0000ff")
             "--localstatedir=/var"
             "--with-boot-tty=/dev/console"
             "--without-system-root-install"
             "--without-rhgb-compat-link"
             "--enable-drm"
             "--disable-systemd-integration"
             ;; Disable GTK to dramatically reduce the closure
             ;; size from ~800 MiB to a little more than 200 MiB
             "--disable-gtk")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             (substitute* "src/main.c"
               (("__DATE__") "\"guix\""))
             #t))
         (add-before 'configure 'fix-docbook
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "docs/Makefile.in"
               (("http://docbook.sourceforge.net/release/xsl/current/manpages/docbook.xsl")
                (string-append (assoc-ref inputs "docbook-xsl")
                               "/xml/xsl/docbook-xsl-"
                               ,(package-version docbook-xsl)
                               "/manpages/docbook.xsl")))
             (setenv "XML_CATALOG_FILES"
                     (string-append (assoc-ref inputs "docbook-xml")
                                    "/xml/dtd/docbook/catalog.xml"))
             #t)))))
    (inputs
     `(("glib" ,glib)
       ("pango" ,pango)
       ("libdrm" ,libdrm)
       ("libpng" ,libpng)
       ("eudev" ,eudev)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("libxslt" ,libxslt)
       ("docbook-xsl" ,docbook-xsl)
       ("docbook-xml" ,docbook-xml)))
    (synopsis "Graphical boot animation (splash) and logger")
    (home-page "https://www.freedesktop.org/wiki/Software/Plymouth/")
    (description
     "Plymouth is an application that runs very early in the boot process and
that provides a graphical boot animation while the boot process happens in the
background.  You are not supposed to install this on your own, it is only
useful with system integration.")
    (license license:gpl2+)))

(define-public libindicator
  (package
    (name "libindicator")
    (version "12.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://launchpad.net/libindicator/"
             (version-major+minor version) "/" version
             "/+download/libindicator-" version ".tar.gz"))
       (sha256
        (base32
         "0zs4z7l9b57jldwz0ban77f3c2zq43ambd0dssf5qg9i216f9lmj"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("dbus-test-runner" ,dbus-test-runner)
       ("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)
       ("xvfb" ,xorg-server-for-tests)))
    (inputs
     `(("gtk+" ,gtk+)
       ("glib" ,glib)))
    (arguments
     `(#:make-flags '("CFLAGS=-Wno-error")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-missing-space-for-libm
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "configure"
               (("LIBM=\"-lm\"") "LIBM=\" -lm\""))
             #t))
         (add-before 'configure 'fix-test-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "tests/Makefile.in"
               (("/bin/sh") (which "sh"))
               (("#!/bin/bash") (string-append "#!" (which "bash")))
               (("/usr/share")
                (string-append (assoc-ref inputs "dbus-test-runner") "/share")))
             #t)))))
    (home-page "https://launchpad.net/libindicator")
    (synopsis "Ayatana indicators symbols and functions")
    (description "A set of symbols and convenience functions for Ayatana indicators.")
    (license license:gpl3)))

(define-public libappindicator
  (package
    (name "libappindicator")
    (version "12.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://launchpad.net/libappindicator/"
             (version-major+minor version) "/" version
             "/+download/libappindicator-" version ".tar.gz"))
       (sha256
        (base32
         "17xlqd60v0zllrxp8bgq3k5a1jkj0svkqn8rzllcyjh8k0gpr46m"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("dbus-test-runner" ,dbus-test-runner)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("xvfb" ,xorg-server-for-tests)))
    (inputs
     `(("dbus-glib" ,dbus-glib)
       ("libindicator" ,libindicator)
       ("python@2" ,python-2)
       ("python2-pygtk" ,python2-pygtk)
       ("python2-pygobject-2" ,python2-pygobject-2)
       ;; ("mono" ,mono) ; requires non-packaged gapi
       ("vala" ,vala)))
    (propagated-inputs
     `(("gtk+" ,gtk+)
       ("libdbusmenu" ,libdbusmenu)))
    (arguments
     ;; FIXME: do not hardcode gtk version
     `(#:configure-flags '("--with-gtk=3")
       #:make-flags '("CFLAGS=-Wno-error")
       #:tests? #f ; One test does not pass (it succeeds when it should fail).
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "docs/reference/Makefile.in"
               (("/bin/sh") (which "sh")))
             (substitute* "tests/Makefile.in"
               (("/bin/sh") (which "sh"))
               (("#!/bin/bash") (string-append "#!" (which "bash")))
               (("/usr") (string-append (assoc-ref inputs "dbus-test-runner"))))
             (substitute* "bindings/python/Makefile.in"
               (("-lappindicator") "-lappindicator3"))
             #t))
         (add-after 'unpack 'fix-codegen-path
           (lambda _
             (substitute* "configure"
               (("PYGTK_CODEGEN=.*") "PYGTK_CODEGEN=pygtk-codegen-2.0\n"))
             #t))
         (add-after 'build 'build-bindings
           (lambda _
             (invoke "make" "-C" "bindings/python")
             #t))
         (add-after 'install 'install-bindings
           (lambda _
             (invoke "make" "-C" "bindings/python" "install")
             #t)))))
    (home-page "https://launchpad.net/libappindicator")
    (synopsis "Allow applications to export a menu into the Unity menu bar")
    (description "A library to allow applications to export a menu, originally
into the Unity menu bar.  Based on KSNI, it also works in KDE and will
fallback to generic Systray support if none of those are available.")
    (license license:lgpl2.1+)))

(define-public libportal
  (let ((commit "bff3289")
        (revision "1"))
    (package
      (name "libportal")
      (version (git-version "0.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/flatpak/libportal")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "104b91qircr1i9jkmm6f725awywky52aimrki303kiaadn2v8b5i"))))
      (build-system meson-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'install 'move-doc
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (doc (assoc-ref outputs "doc"))
                     (html "/share/gtk-doc"))
                 (copy-recursively (string-append out html)
                                   (string-append doc html))
                 (delete-file-recursively (string-append out html))
                 #t))))))
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ("gtk-doc" ,gtk-doc/stable)
         ("docbook-xsl" ,docbook-xsl)
         ("docbook-xml" ,docbook-xml)
         ("libxml2" ,libxml2)
         ("glib:bin" ,glib "bin")))
      (propagated-inputs
       `(("glib" ,glib)))
      (outputs '("out" "doc"))
      (home-page "https://github.com/flatpak/libportal")
      (synopsis "Flatpak portal library")
      (description
       "libportal provides GIO-style async APIs for most Flatpak portals.")
      (license license:lgpl2.1+))))

(define-public xdg-desktop-portal
  (package
    (name "xdg-desktop-portal")
    (version "1.7.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/flatpak/xdg-desktop-portal")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rkwpsmbn3d3spkzc2zsd50l2r8pp4la390zcpsawaav8w7ql7xm"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("glib:bin" ,glib "bin")
       ("which" ,which)
       ("gettext" ,gettext-minimal)))
    (inputs
     `(("glib" ,glib)
       ("flatpak" ,flatpak)
       ("fontconfig" ,fontconfig)
       ("json-glib" ,json-glib)
       ("libportal" ,libportal)
       ("dbus" ,dbus)
       ("geoclue" ,geoclue)
       ("pipewire" ,pipewire-0.3)
       ("fuse" ,fuse)))
    (home-page "https://github.com/flatpak/xdg-desktop-portal")
    (synopsis "Desktop integration portal for sandboxed apps")
    (description
     "xdg-desktop-portal is a @dfn{portal front-end service} for Flatpak and
possibly other desktop containment frameworks.  It works by exposing a series
of D-Bus interfaces known as portals under a well-known
name (@code{org.freedesktop.portal.Desktop}) and object
path (@code{/org/freedesktop/portal/desktop}).

The portal interfaces include APIs for file access, opening URIs, printing
and others.")
    (license license:lgpl2.1+)))

(define-public xdg-desktop-portal-gtk
  (package
    (name "xdg-desktop-portal-gtk")
    (version "1.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/flatpak/xdg-desktop-portal-gtk")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "183iha9dxmvprn99ymgz17jx1lyn1fj5jyj6ghxl716zn9mxmird"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'po-chmod
           (lambda _
             ;; Make sure 'msgmerge' can modify the PO files.
             (for-each (lambda (po)
                         (chmod po #o666))
                       (find-files "po" "\\.po$"))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("xdg-desktop-portal" ,xdg-desktop-portal)
       ("glib:bin" ,glib "bin")
       ("which" ,which)
       ("gettext" ,gettext-minimal)))
    (inputs
     `(("glib" ,glib)
       ("gtk" ,gtk+)
       ("fontconfig" ,fontconfig)
       ("gnome-desktop" ,gnome-desktop)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)))
    (native-search-paths
     (list (search-path-specification
            (variable "XDG_DESKTOP_PORTAL_DIR")
            (files '("share/xdg-desktop-portal/portals")))))
    (home-page "https://github.com/flatpak/xdg-desktop-portal-gtk")
    (synopsis "GTK implementation of xdg-desktop-portal")
    (description
     "This package provides a backend implementation for xdg-desktop-portal
which uses GTK+ and various pieces of GNOME infrastructure, such as the
@code{org.gnome.Shell.Screenshot} or @code{org.gnome.SessionManager} D-Bus
interfaces.")
    (license license:lgpl2.1+)))
