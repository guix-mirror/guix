;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2014, 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014, 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015, 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2015, 2017 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2015, 2016, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2018 Rene Saavedra <pacoon@protonmail.com>
;;; Copyright © 2016 Jochem Raat <jchmrt@riseup.net>
;;; Copyright © 2016, 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016, 2017 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017, 2018 nee <nee-git@hidamari.blue>
;;; Copyright © 2017 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017 Mohammed Sadiq <sadiq@sadiqpk.org>
;;; Copyright © 2017 Brendan Tildesley <brendan.tildesley@openmailbox.org>
;;; Copyright © 2017, 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Jovany Leandro G.C <bit4bit@riseup.net>
;;; Copyright © 2018 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2018 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2018, 2019 Timothy Sample <samplet@ngyro.com>
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

(define-module (gnu packages gnome)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lirc)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdesktop)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages slang)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages web)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu artwork)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public brasero
  (package
    (name "brasero")
    (version "3.12.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/" name "/"
                                 (version-major+minor version) "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "0h90y674j26rvjahb8cc0w79zx477rb6zaqcj26wzvq8kmpic8k8"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags (list
                          (string-append "--with-girdir="
                                         (assoc-ref %outputs "out")
                                         "/share/gir-1.0")
                          (string-append "--with-typelibdir="
                                         (assoc-ref %outputs "out")
                                         "/lib/girepository-1.0"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'embed-growisofs
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "plugins/growisofs/burn-growisofs.c"
               (("\"growisofs") (string-append "\"" (which "growisofs"))))
             #t)))))
    (propagated-inputs
     `(("hicolor-icon-theme" ,hicolor-icon-theme)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin")                       ; glib-compile-schemas, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("dvd+rw-tools" ,dvd+rw-tools)
       ("glib" ,glib)
       ("gnome-doc-utils" ,gnome-doc-utils)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gtk+" ,gtk+)
       ("itstool" ,itstool)
       ("libcanberra" ,libcanberra)
       ("libice" ,libice)
       ("libnotify" ,libnotify)
       ("libsm" ,libsm)
       ("libxml2" ,libxml2)
       ("nettle" ,nettle)
       ("totem-pl-parser" ,totem-pl-parser)))
    (home-page "https://projects.gnome.org/brasero/")
    (synopsis "CD/DVD burning tool for Gnome")
    (description "Brasero is an application to burn CD/DVD for the Gnome
Desktop.  It is designed to be as simple as possible and has some unique
features to enable users to create their discs easily and quickly.")
    (license license:gpl2+)))

(define-public deja-dup
  (package
    (name "deja-dup")
    (version "34.3")
    (source (origin
             (method url-fetch)
             (uri "https://launchpadlibrarian.net/295170991/deja-dup-34.3.tar.xz")
             (sha256
              (base32
               "1xqcr61hpbahbla7gdjn4ngjfz7w6f57y7f5pkb77yk05f60j2n9"))
             (patches
               (search-patches "deja-dup-use-ref-keyword-for-iter.patch"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  ((guix build cmake-build-system) #:prefix cmake:)
                  (guix build glib-or-gtk-build-system)
                  (guix build utils))
       #:imported-modules (,@%glib-or-gtk-build-system-modules
                           (guix build cmake-build-system))
       #:test-target "test"
       #:configure-flags (list (string-append
                                "-DCMAKE_INSTALL_FULL_DATADIR=" %output)
                               (string-append
                                "-DCMAKE_INSTALL_LIBEXECDIR=" %output))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-lockfile-deletion
           (lambda rest
             (substitute* "libdeja/tools/duplicity/DuplicityInstance.vala"
               (("/bin/rm")
                (which "rm")))))
         (replace 'configure
           (assoc-ref cmake:%standard-phases 'configure))
         (delete 'check) ;; Fails due to issues with DBus
         (add-after 'install 'wrap-deja-dup
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((python      (assoc-ref inputs "python"))
                   (python-path (getenv "PYTHONPATH"))
                   (duplicity   (assoc-ref inputs "duplicity"))
                   (out         (assoc-ref outputs "out")))
               (for-each
                (lambda (program)
                  (wrap-program program
                    `("PATH" ":" prefix (,(string-append python "/bin")
                                         ,(string-append duplicity "/bin"))))
                  (wrap-program program
                    `("PYTHONPATH" ":" prefix (,python-path))))

                (find-files (string-append out "/bin")))
               #t))))))
    (inputs
     `(("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gobject-introspection" ,gobject-introspection)
       ("duplicity" ,duplicity)
       ("python" ,python-2)
       ("python-pygobject" ,python2-pygobject)
       ("gtk+" ,gtk+)
       ("libnotify" ,libnotify)
       ("libpeas" ,libpeas)
       ("libsecret" ,libsecret)
       ("packagekit" ,packagekit)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("gettext" ,gettext-minimal)
       ("itstool" ,itstool)
       ("intltool" ,intltool)
       ("cmake" ,cmake)))
    (home-page "https://launchpad.net/deja-dup")
    (synopsis "Simple backup tool, for regular encrypted backups")
    (description
     "Déjà Dup is a simple backup tool, for regular encrypted backups.  It
uses duplicity as the backend, which supports incremental backups and storage
either on a local, or remote machine via a number of methods.")
    (license license:gpl3+)))

(define-public dia
  ;; This version from GNOME's repository includes fixes for compiling with
  ;; recent versions of the build tools.  The latest activity on the
  ;; pre-GNOME version has been in 2014, while GNOME has continued applying
  ;; fixes in 2016.
  (let ((commit "fbc306168edab63db80b904956117cbbdc514ee4"))
    (package
      (name "dia")
      (version (string-append "0.97.2-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.gnome.org/browse/dia")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1b4bba0k8ph4cwgw8xjglss0p6n111bpd5app67lrq79mp0ad06l"))))
      (build-system gnu-build-system)
      (inputs
       `(("glib" ,glib "bin")
         ("pango" ,pango)
         ("gdk-pixbuf" ,gdk-pixbuf)
         ("gtk+" ,gtk+-2)
         ("libxml2" ,libxml2)
         ("freetype" ,freetype)
         ("libart-lgpl" ,libart-lgpl)))
      (native-inputs
       `(("intltool" ,intltool)
         ("pkg-config" ,pkg-config)
         ("automake" ,automake)
         ("autoconf" ,autoconf)
         ("libtool" ,libtool)
         ("perl" ,perl)
         ("python-wrapper" ,python-wrapper)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'run-autogen
             (lambda _
               (system* "sh" "autogen.sh"))))))
      (home-page "https://wiki.gnome.org/Apps/Dia")
      (synopsis "Diagram creation for GNOME")
      (description "Dia can be used to draw different types of diagrams, and
includes support for UML static structure diagrams (class diagrams), entity
relationship modeling, and network diagrams.  The program supports various file
formats like PNG, SVG, PDF and EPS.")
      (license license:gpl2+))))

(define-public libgdata
  (package
    (name "libgdata")
    (version "0.16.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "09q8h1129xjpw33rvzz7856drygxwlm0s64z9cm0vbmjxiqy0h47"))
              (patches
               (search-patches "libgdata-fix-tests.patch"
                               "libgdata-glib-duplicate-tests.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'disable-failing-tests
           (lambda _
             ;; The PicasaWeb API tests fail with gnome-online-accounts@3.24.2.
             ;; They have been removed in libgdata 0.17.6, so just do the same.
             (substitute* "gdata/tests/Makefile"
               (("picasaweb\\$\\(EXEEXT\\) ") ""))
             #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("uhttpmock" ,uhttpmock)))
    (inputs
     `(("cyrus-sasl" ,cyrus-sasl)
       ("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("json-glib" ,json-glib)
       ("libsoup" ,libsoup)))
    (propagated-inputs
     `(("gcr" ,gcr)
       ("gnome-online-accounts" ,gnome-online-accounts)
       ("liboauth" ,liboauth)
       ("libxml2" ,libxml2)))
    (home-page "https://wiki.gnome.org/Projects/libgdata")
    (synopsis "Library for accessing online service APIs")
    (description
     "libgdata is a GLib-based library for accessing online service APIs using
the GData protocol — most notably, Google's services.  It provides APIs to
access the common Google services, and has full asynchronous support.")
    (license license:lgpl2.1+)))

(define-public libgxps
  (package
    (name "libgxps")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "157s4c9gjjss6yd7qp7n4q6s72gz1k4ilsx4xjvp357azk49z4qs"))))
    (build-system meson-build-system)
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+)
       ("libjpeg" ,libjpeg-turbo)
       ("lcms" ,lcms)
       ("libtiff" ,libtiff)
       ("nettle" ,nettle)))
    (propagated-inputs
     ;; In Requires of libgxps.pc.
     `(("cairo" ,cairo)
       ("glib" ,glib)
       ("libarchive" ,libarchive)))
    (home-page "https://wiki.gnome.org/Projects/libgxps")
    (synopsis "GObject-based library for handling and rendering XPS documents")
    (description
     "libgxps is a GObject-based library for handling and rendering XPS
documents.  This package also contains binaries that can convert XPS documents
to other formats.")
    (license license:lgpl2.1+)))

(define-public gnome-common
  (package
    (name "gnome-common")
    (version "3.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version)  "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1kzqi8qvh5p1zncj8msazlmvcwsczjz2hqxp4x2y0mg718vrwmi2"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnome.org/")
    (synopsis "Bootstrap GNOME modules built from Git")
    (description "gnome-common contains various files needed to bootstrap
GNOME modules built from Git.  It contains a common \"autogen.sh\" script that
can be used to configure a source directory checked out from Git and some
commonly used macros.")
    (license license:gpl2+)))

(define-public gnome-desktop
  (package
    (name "gnome-desktop")
    (version "3.24.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/" name "/"
                          (version-major+minor version)  "/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "0pkq5l1llw8gkjhfq6y58iyj6wac8dh1mc3rzjzn6nd7lrkdx8cg"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libc   (assoc-ref inputs "libc")))
               (substitute* "libgnome-desktop/gnome-languages.c"
                 (("\"locale\"")
                  (string-append "\"" libc "/bin/locale\"")))
               #t))))))
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("itstool" ,itstool)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (propagated-inputs
     ;; Required by gnome-desktop-3.0.pc.
     `(("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)))
    (inputs
     `(("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("iso-codes" ,iso-codes)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxkbfile" ,libxkbfile)
       ("libxrandr" ,libxrandr)
       ("xkeyboard-config" ,xkeyboard-config)))
    (home-page "https://www.gnome.org/")
    (synopsis
     "Libgnome-desktop, gnome-about, and desktop-wide documents")
    (description
     "The libgnome-desktop library provides API shared by several applications
on the desktop, but that cannot live in the platform for various reasons.
There is no API or ABI guarantee, although we are doing our best to provide
stability.  Documentation for the API is available with gtk-doc.

The gnome-about program helps find which version of GNOME is installed.")
    ; Some bits under the LGPL.
    (license license:gpl2+)))

(define-public gnome-doc-utils
  (package
    (name "gnome-doc-utils")
    (version "0.20.10")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/" name "/"
                          (version-major+minor version)  "/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "19n4x25ndzngaciiyd8dd6s2mf9gv6nv3wv27ggns2smm7zkj1nb"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("docbook-xml" ,docbook-xml-4.4)
       ("python2-libxml2" ,python2-libxml2)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("pkg-config" ,pkg-config)
       ("python-2" ,python-2)))
    (home-page "https://wiki.gnome.org/GnomeDocUtils")
    (synopsis
     "Documentation utilities for the Gnome project")
    (description
     "Gnome-doc-utils is a collection of documentation utilities for the
Gnome project.  It includes xml2po tool which makes it easier to translate
and keep up to date translations of documentation.")
    (license license:gpl2+))) ; xslt under lgpl

(define-public gnome-disk-utility
  (package
    (name "gnome-disk-utility")
    (version "3.28.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "11ajz4cbsdns81kihd6242b6pwxbw8bkr9qqkf4qnb4kp363a38m"))))
    (build-system meson-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gtk+" ,gtk+ "bin")             ; gtk-update-icon-cache
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("docbook-xml" ,docbook-xml)
       ("docbook-xsl" ,docbook-xsl)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)))
    (inputs
     `(("glib" ,glib)
       ("appstream-glib" ,appstream-glib)
       ("gnome-settings-daemon" ,gnome-settings-daemon)
       ("gtk+" ,gtk+)
       ("libcanberra" ,libcanberra)
       ("libdvdread" ,libdvdread)
       ("libnotify" ,libnotify)
       ("libpwquality" ,libpwquality)
       ("libsecret" ,libsecret)
       ("udisks" ,udisks)))
    (home-page "https://git.gnome.org/browse/gnome-disk-utility")
    (synopsis "Disk management utility for GNOME")
    (description "Disk management utility for GNOME.")
    (license license:gpl2+)))

(define-public gcr
  (package
    (name "gcr")
    (version "3.20.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/" name "/"
                                 (version-major+minor version)  "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "0ydk9dzxx6snxza7j5ps8x932hbr3x1b8hhcaqjq4w4admi2qmwh"))
             (patches
              (search-patches "gcr-disable-failing-tests.patch"
                              "gcr-fix-collection-tests-to-work-with-gpg-21.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before
                   'check 'pre-check
                   (lambda* (#:key inputs #:allow-other-keys)
                     (substitute* "build/tap-driver"
                       (("/usr/bin/env python") (which "python"))))))))
    (inputs
     `(("dbus" ,dbus)
       ("gnupg" ,gnupg) ;called as a child process during tests
       ("libgcrypt" ,libgcrypt)))
    (native-inputs
     `(("python" ,python-2) ;for tests
       ("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("xsltproc" ,libxslt)))
    ;; mentioned in gck.pc, gcr.pc and gcr-ui.pc
    (propagated-inputs
     `(("p11-kit" ,p11-kit)
       ("glib" ,glib)
       ("gtk+" ,gtk+)))
    (home-page "https://www.gnome.org")
    (synopsis "Libraries for displaying certificates and accessing key stores")
    (description
     "The GCR package contains libraries used for displaying certificates and
accessing key stores.  It also provides the viewer for crypto files on the
GNOME Desktop.")
    (license license:lgpl2.1+)))

(define-public libgnome-keyring
  (package
    (name "libgnome-keyring")
    (version "3.12.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/" name "/"
                                 (version-major+minor version)  "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "10vpjhgbjm7z2djy04qakd02qlzpd02xnbfjhk2aqwjzn3xpihf4"))))
    (build-system gnu-build-system)
    (inputs
     `(("libgcrypt" ,libgcrypt)
       ("dbus" ,dbus)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")
       ("intltool" ,intltool)))
    (propagated-inputs
     ;; Referred to in .h files and .pc.
     `(("glib" ,glib)))
    (home-page "https://www.gnome.org")
    (synopsis "Accessing passwords from the GNOME keyring")
    (description
     "Client library to access passwords from the GNOME keyring.")

    ;; Though a couple of files are LGPLv2.1+.
    (license license:lgpl2.0+)))

(define-public gnome-keyring
  (package
    (name "gnome-keyring")
    (version "3.20.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/" name "/"
                                 (version-major+minor version)  "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "134ci3mn6jjap59z3lrvyiip7zf2nlw5xvanr44yajs57xr4x5lp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;48 of 603 tests fail because /var/lib/dbus/machine-id does
                   ;not exist
       #:configure-flags
       (list
        (string-append "--with-pkcs11-config="
                       (assoc-ref %outputs "out") "/share/p11-kit/modules/")
        (string-append "--with-pkcs11-modules="
                       (assoc-ref %outputs "out") "/share/p11-kit/modules/"))
       #:phases
       (modify-phases %standard-phases
         (add-before
          'check 'pre-check
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "build/tap-driver"
              (("/usr/bin/env python") (which "python")))))
         (add-before
          'configure 'fix-docbook
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "docs/Makefile.am"
              (("http://docbook.sourceforge.net/release/xsl/current/manpages/docbook.xsl")
               (string-append (assoc-ref inputs "docbook-xsl")
                              "/xml/xsl/docbook-xsl-"
                              ,(package-version docbook-xsl)
                              "/manpages/docbook.xsl")))
            (setenv "XML_CATALOG_FILES"
                    (string-append (assoc-ref inputs "docbook-xml")
                                   "/xml/dtd/docbook/catalog.xml"))

            ;; Rerun the whole thing to avoid version mismatch ("This is
            ;; Automake 1.15.1, but the definition used by this
            ;; AM_INIT_AUTOMAKE comes from Automake 1.15.").  Note: we don't
            ;; use 'autoreconf' because it insists on running 'libtoolize'.
            (invoke "autoconf")
            (invoke "aclocal")
            (invoke "automake" "-ac"))))))
    (inputs
     `(("libgcrypt" ,libgcrypt)
       ("linux-pam" ,linux-pam)
       ("dbus" ,dbus)
       ("gcr" ,gcr)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")
       ("python" ,python-2) ;for tests
       ("intltool" ,intltool)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libxslt" ,libxslt) ;for documentation
       ("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)))
    (home-page "https://www.gnome.org")
    (synopsis "Daemon to store passwords and encryption keys")
    (description
     "gnome-keyring is a program that keeps passwords and other secrets for
users.  It is run as a daemon in the session, similar to ssh-agent, and other
applications locate it via an environment variable or D-Bus.

The program can manage several keyrings, each with its own master password,
and there is also a session keyring which is never stored to disk, but
forgotten when the session ends.")
    (license license:lgpl2.1+)))

(define-public evince
  (package
    (name "evince")
    (version "3.28.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/" name "/"
                                 (version-major+minor version) "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "1qbk1x2c7iacmmfwjzh136v2sdacrkqn9d6bnqid7xn9hlnx4m89"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags '("--disable-nautilus")
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "data/Makefile"
               (("gtk-update-icon-cache") "true"))
             #t)))))
    (inputs
     `(("libarchive" ,libarchive)
       ("libgxps" ,libgxps)
       ("libspectre" ,libspectre)
       ("djvulibre" ,djvulibre)
       ("ghostscript" ,ghostscript)
       ("poppler" ,poppler)
       ("libtiff" ,libtiff)
       ;; TODO:
       ;;   Build libkpathsea as a shared library for DVI support.
       ;; ("libkpathsea" ,texlive-bin)
       ("gnome-desktop" ,gnome-desktop)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("libgnome-keyring" ,libgnome-keyring)
       ("adwaita-icon-theme" ,adwaita-icon-theme)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("atk" ,atk)
       ("pango" ,pango)
       ("gtk+" ,gtk+)
       ("glib" ,glib)
       ("libxml2" ,libxml2)
       ("libsm" ,libsm)
       ("libice" ,libice)
       ("shared-mime-info" ,shared-mime-info)
       ("dconf" ,dconf)
       ("libcanberra" ,libcanberra)
       ("libsecret" ,libsecret)

       ;; XXX: 'libarchive.pc' adds '-lnettle' so Nettle should really be
       ;; propagated from there.
       ("nettle" ,nettle)

       ;; For tests.
       ("dogtail" ,python2-dogtail)))
    (native-inputs
     `(("itstool" ,itstool)
       ("intltool" ,intltool)
       ("glib" ,glib "bin")
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (home-page
     "https://www.gnome.org/projects/evince/")
    (synopsis "GNOME's document viewer")
    (description
     "Evince is a document viewer for multiple document formats.  It
currently supports PDF, PostScript, DjVu, TIFF and DVI.  The goal
of Evince is to replace the multiple document viewers that exist
on the GNOME Desktop with a single simple application.")
    (license license:gpl2+)))

(define-public gsettings-desktop-schemas
  (package
    (name "gsettings-desktop-schemas")
    (version "3.24.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/" name "/"
                          (version-major+minor version)  "/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "19zfqy58missq5cck13ip8j824hb9whqg2c4cr6hgrlxkwqgm8vn"))))
    (build-system gnu-build-system)
    (inputs
     `(("glib" ,glib)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin")                       ; glib-compile-schemas, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (home-page "https://launchpad.net/gsettings-desktop-schemas")
    (synopsis
     "GNOME settings for various desktop components")
    (description
     "Gsettings-desktop-schemas contains a collection of GSettings schemas
for settings shared by various components of the GNOME desktop.")
    (license license:lgpl2.1+)))

(define-public icon-naming-utils
  (package
    (name "icon-naming-utils")
    (version "0.8.90")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://tango.freedesktop.org/releases/icon-naming-utils-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "1mc3v28fdfqanx3lqx233vcr4glb4c2376k0kx2v91a4vxwqcdxi"))))
    (build-system gnu-build-system)
    (inputs
     `(("perl" ,perl)
       ("perl-xml-simple" ,perl-xml-simple)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'set-load-paths
           ;; Tell 'icon-name-mapping' where XML::Simple is.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (prog (string-append out "/libexec/icon-name-mapping")))
               (wrap-program
                   prog
                 `("PERL5LIB" = ,(list (getenv "PERL5LIB")))))
             #t)))))
    (home-page "http://tango.freedesktop.org/Standard_Icon_Naming_Specification")
    (synopsis
     "Utility to implement the Freedesktop Icon Naming Specification")
    (description
     "To help with the transition to the Freedesktop Icon Naming
Specification, the icon naming utility maps the icon names used by the
GNOME and KDE desktops to the icon names proposed in the specification.")
    (license license:lgpl2.1+)))

(define-public gnome-icon-theme
  (package
    (name "gnome-icon-theme")
    (version "3.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/" name "/"
                          (version-major+minor version)  "/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "0fjh9qmmgj34zlgxb09231ld7khys562qxbpsjlaplq2j85p57im"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       ;; Don't create 'icon-theme.cache'.
       (let* ((coreutils (assoc-ref %build-inputs "coreutils"))
              (true      (string-append coreutils "/bin/true")))
         (list (string-append "GTK_UPDATE_ICON_CACHE=" true)))))
    (native-inputs
     `(("icon-naming-utils" ,icon-naming-utils)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://art.gnome.org/")
    (synopsis
     "GNOME icon theme")
    (description
     "Icons for the GNOME desktop.")
    (license license:lgpl3))) ; or Creative Commons BY-SA 3.0

;; gnome-icon-theme was renamed to adwaita-icon-theme after version 3.12.0.
(define-public adwaita-icon-theme
  (package (inherit gnome-icon-theme)
    (name "adwaita-icon-theme")
    (version "3.26.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "17fpahgh5dyckgz7rwqvzgnhx53cx9kr2xw0szprc6bnqy977fi8"))))
    (native-inputs
     `(("gtk-encode-symbolic-svg" ,gtk+ "bin")))))

(define-public tango-icon-theme
  (package
    (name "tango-icon-theme")
    (version "0.8.90")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://tango.freedesktop.org/releases/"
                                  "tango-icon-theme-" version ".tar.bz2"))
              (sha256
               (base32
                "034r9s944b4yikyfgn602yv7s54wdzlq0qfvqh52b9x6kbx08h79"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("icon-naming-utils" ,icon-naming-utils)
       ("intltool" ,intltool)
       ("imagemagick" ,imagemagick)
       ("pkg-config" ,pkg-config)))
    (home-page "http://tango-project.org/")
    (synopsis "Tango icon theme")
    (description "This is an icon theme that follows the Tango visual
guidelines.")
    (license license:public-domain)))

(define-public shared-mime-info
  (package
    (name "shared-mime-info")
    (version "1.9")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://freedesktop.org/~hadess/"
                                 "shared-mime-info-" version ".tar.xz"))
             (sha256
              (base32
               "10ywzhzg8v1xmb9sz5xbqaci90id38knswigynyl33i29vn360aw"))))
    (build-system gnu-build-system)
    (arguments
     ;; The build system appears not to be parallel-safe.
     '(#:parallel-build? #f))
    (inputs
     `(("glib" ,glib)
       ("libxml2" ,libxml2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://www.freedesktop.org/wiki/Software/shared-mime-info")
    (synopsis "Database of common MIME types")
    (description
     "The shared-mime-info package contains the core database of common types
and the update-mime-database command used to extend it.  It requires glib2 to
be installed for building the update command.  Additionally, it uses intltool
for translations, though this is only a dependency for the maintainers.  This
database is translated at Transifex.")
    (license license:gpl2+)))

(define-public system-config-printer
  (package
    (name "system-config-printer")
    (version "1.5.11")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/zdohnal/system-config-printer/releases/"
                   "download/" version
                   "/system-config-printer-" version ".tar.xz"))
             (sha256
              (base32
               "1lq0q51bhanirpjjvvh4xiafi8hgpk8r32h0dj6dn3f32z8pib9q"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:imported-modules ((guix build python-build-system)
                           ,@%glib-or-gtk-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-Makefile.am
           (lambda _
             ;; The Makefile generates some scripts, so set a valid shebang
             (substitute* "Makefile.am"
               (("/bin/bash") (which "bash")))
             (delete-file "configure")
             #t))
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Modify the man XML otherwise xmlto tries to access the network
             (substitute* "man/system-config-printer.xml"
               (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                (string-append (assoc-ref inputs "docbook-xml")
                               "/xml/dtd/docbook/")))
             #t))
         (add-after 'install 'wrap-for-python
           (@@ (guix build python-build-system) wrap))
         (add-after 'install 'wrap
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH")))
               (for-each
                (lambda (program)
                  (wrap-program program
                    `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
                (map (lambda (name)
                       (string-append out "/bin/" name))
                     '("system-config-printer"
                       "system-config-printer-applet"
                       "install-printerdriver"
                       "scp-dbus-service"))))
             #t)))))
    (inputs
     `(("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gobject-introspection" ,gobject-introspection)
       ("python" ,python)
       ("cups" ,cups)
       ("python-dbus" ,python-dbus)
       ("python-pygobject" ,python-pygobject)
       ("python-pycups" ,python-pycups)
       ("python-requests" ,python-requests)
       ("python-pycairo" ,python-pycairo)
       ("libnotify" ,libnotify)
       ("packagekit" ,packagekit)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("desktop-file-utils" ,desktop-file-utils)
       ("glib" ,glib)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("intltool" ,intltool)
       ("xmlto" ,xmlto)
       ("docbook-xml" ,docbook-xml-4.1.2)
       ("docbook-xsl" ,docbook-xsl)
       ("libxml2" ,libxml2)))
    (home-page "https://github.com/zdohnal/system-config-printer")
    (synopsis "CUPS administration tool")
    (description
     "system-config-printer is a CUPS administration tool.  It's written in
Python using GTK+, and uses the @acronym{IPP, Internet Printing Protocol} when
configuring CUPS.")
    (license license:gpl2+)))

(define-public hicolor-icon-theme
  (package
    (name "hicolor-icon-theme")
    (version "0.17")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://icon-theme.freedesktop.org/releases/"
                          "hicolor-icon-theme-" version ".tar.xz"))
      (sha256
       (base32
        "1n59i3al3zx6p90ff0l43gzpzmlqnzm6hf5cryxqrlbi48sq8x1i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ; no check target
    (home-page "https://icon-theme.freedesktop.org/releases/")
    (synopsis
     "Freedesktop icon theme")
    (description
     "Freedesktop icon theme.")
    (license license:gpl2)))

(define-public libnotify
  (package
    (name "libnotify")
    (version "0.7.7")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/" name "/"
                          (version-major+minor version)  "/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "017wgq9n00hx39n0hm784zn18hl721hbaijda868cm96bcqwxd4w"))))
    (build-system gnu-build-system)
    (inputs
     `(("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libpng" ,libpng)))
    (native-inputs
      `(("pkg-config" ,pkg-config)
        ("glib" ,glib "bin")
        ("gobject-introspection" ,gobject-introspection)))
    (home-page "https://developer-next.gnome.org/libnotify/")
    (synopsis
     "GNOME desktop notification library")
    (description
     "Libnotify is a library that sends desktop notifications to a
notification daemon, as defined in the Desktop Notifications spec.  These
notifications can be used to inform the user about an event or display
some form of information without getting in the user's way.")
    (license license:lgpl2.1+)))

(define-public libpeas
  (package
    (name "libpeas")
    (version "1.22.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/" name "/"
                          (version-major+minor version)  "/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "0qm908kisyjzjxvygdl18hjqxvvgkq9w0phs2g55pck277sw0bsv"))))
    (build-system gnu-build-system)
    (inputs
     `(("gtk+" ,gtk+)
       ("glade" ,glade3)
       ("python" ,python)
       ("python-pygobject" ,python-pygobject)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)))
    (propagated-inputs
     ;; The .pc file "Requires" gobject-introspection.
     `(("gobject-introspection" ,gobject-introspection)))
    (home-page "https://wiki.gnome.org/Libpeas")
    (synopsis "GObject plugin system")
    (description
     "Libpeas is a gobject-based plugin engine, targeted at giving every
application the chance to assume its own extensibility.  It also has a set of
features including, but not limited to: multiple extension points; on-demand
(lazy) programming language support for C, Python and JS; simplicity of the
API.")
    (license license:lgpl2.0+)))

(define-public gtkglext
  (package
    (name "gtkglext")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gtkglext/gtkglext/"
                                  version "/gtkglext-" version ".tar.gz"))
              (sha256
               (base32 "1ya4d2j2aacr9ii5zj4ac95fjpdvlm2rg79mgnk7yvl1dcy3y1z5"))
              (patches (search-patches
                        "gtkglext-disable-disable-deprecated.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Fix a collision between a local variable and a definition from
         ;; glibc's string.h.
         (add-after 'unpack 'fix-collision
           (lambda _
             (substitute* "gdk/gdkglshapes.c"
               ((" index") " triangle_index"))
             #t)))))
    (inputs `(("gtk+" ,gtk+-2)
              ("mesa" ,mesa)
              ("glu" ,glu)
              ("libx11" ,libx11)
              ("libxt" ,libxt)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("glib" ,glib "bin")))
    (propagated-inputs `(("pangox-compat" ,pangox-compat)))
    (home-page "https://projects.gnome.org/gtkglext")
    (synopsis "OpenGL extension to GTK+")
    (description "GtkGLExt is an OpenGL extension to GTK+.  It provides
additional GDK objects which support OpenGL rendering in GTK+ and GtkWidget
API add-ons to make GTK+ widgets OpenGL-capable.")
    (license license:lgpl2.1+)))

(define-public glade3
  (package
    (name "glade")
    (version "3.20.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1vdbqqvf6nrkqml4awrq4pzfwsm5v39wdzg943nys6lmf1am9l87"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:tests? #f ; needs X, GL, and software rendering
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-docbook
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "man/Makefile.in"
               (("http://docbook.sourceforge.net/release/xsl/current/manpages/docbook.xsl")
                (string-append (assoc-ref inputs "docbook-xsl")
                               "/xml/xsl/docbook-xsl-"
                               ,(package-version docbook-xsl)
                               "/manpages/docbook.xsl")))
             #t)))))
    (inputs
     `(("gtk+" ,gtk+)
       ("libxml2" ,libxml2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("itstool" ,itstool)
       ("libxslt" ,libxslt) ;for xsltproc
       ("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)
       ("python" ,python-2)
       ("pkg-config" ,pkg-config)))
    (home-page "https://glade.gnome.org")
    (synopsis "GTK+ rapid application development tool")
    (description "Glade is a rapid application development (RAD) tool to
enable quick & easy development of user interfaces for the GTK+ toolkit and
the GNOME desktop environment.")
    (license license:lgpl2.0+)))

(define-public libcroco
  (package
    (name "libcroco")
    (version "0.6.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (patches
                (search-patches "libcroco-CVE-2017-7960.patch"
                                "libcroco-CVE-2017-7961.patch"))
              (sha256
               (base32
                "0q7qhi7z64i26zabg9dbs5706fa8pmzp1qhpa052id4zdiabbi6x"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("libxml2" ,libxml2)
       ("zlib" ,zlib)))
    (home-page "https://github.com/GNOME/libcroco")
    (synopsis "CSS2 parsing and manipulation library")
    (description
     "Libcroco is a standalone CSS2 parsing and manipulation library.
The parser provides a low level event driven SAC-like API and a CSS object
model like API.  Libcroco provides a CSS2 selection engine and an experimental
XML/CSS rendering engine.")

    ;; LGPLv2.1-only.
    (license license:lgpl2.1)))

(define-public libgsf
  (package
    (name "libgsf")
    (version "1.14.44")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1ppzfk3zmmgrg9jh8vc4dacddbfngjslq2wpj94pcr3i0c8dxgk8"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("python" ,python)
       ("zlib" ,zlib)
       ("bzip2" ,bzip2)))
    (propagated-inputs
     `(("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("libxml2" ,libxml2)))
    (home-page "https://www.gnome.org/projects/libgsf")
    (synopsis "GNOME's Structured File Library")
    (description
     "Libgsf aims to provide an efficient extensible I/O abstraction for
dealing with different structured file formats.")

    ;; LGPLv2.1-only.
    (license license:lgpl2.1)))

(define-public librsvg
  (package
    (name "librsvg")
    (version "2.40.20")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0ay9himvw1l1swcf3h1312d2iqzfl65kpbfgiyfykgvq7cydvx6g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "gdk-pixbuf-loader/Makefile.in"
               ;; By default the gdk-pixbuf loader is installed under
               ;; gdk-pixbuf's prefix.  Work around that.
               (("gdk_pixbuf_moduledir = .*$")
                (string-append "gdk_pixbuf_moduledir = "
                               "$(prefix)/lib/gdk-pixbuf-2.0/2.10.0/"
                                "loaders\n"))
               ;; Drop the 'loaders.cache' file, it's in gdk-pixbuf+svg.
               (("gdk_pixbuf_cache_file = .*$")
                "gdk_pixbuf_cache_file = $(TMPDIR)/loaders.cache\n"))
             #t))
         (add-after 'unpack 'remove-failing-tests
           (lambda _
             (with-directory-excursion "tests/fixtures/reftests"
               (for-each delete-file
                         '(;; This test fails on i686:
                           "svg1.1/masking-path-04-b.svg"
                           "svg1.1/masking-path-04-b-ref.png"
                           ;; This test fails on armhf:
                           "svg1.1/masking-mask-01-b.svg"
                           "svg1.1/masking-mask-01-b-ref.png"
                           ;; This test fails on aarch64:
                           "bugs/777834-empty-text-children.svg"
                           "bugs/777834-empty-text-children-ref.png")))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")                               ; glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection))) ; g-ir-compiler, etc.
    (inputs
     `(("pango" ,pango)
       ("libcroco" ,libcroco)
       ("bzip2" ,bzip2)
       ("libgsf" ,libgsf)
       ("libxml2" ,libxml2)))
    (propagated-inputs
     ;; librsvg-2.0.pc refers to all of that.
     `(("cairo" ,cairo)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)))
    (home-page "https://wiki.gnome.org/LibRsvg")
    (synopsis "Render SVG files using Cairo")
    (description
     "Librsvg is a C library to render SVG files using the Cairo 2D graphics
library.")
    (license license:lgpl2.0+)))

(define-public libidl
  (package
    (name "libidl")
    (version "0.8.14")
    (source (origin
              (method url-fetch)
              (uri (let ((upstream-name "libIDL"))
		     (string-append "mirror://gnome/sources/" upstream-name "/"
                                    (version-major+minor version) "/"
                                    upstream-name "-" version ".tar.bz2")))
              (sha256
               (base32
                "08129my8s9fbrk0vqvnmx6ph4nid744g5vbwphzkaik51664vln5"))))
    (build-system gnu-build-system)
    (inputs `(("glib" ,glib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("flex" ,flex)
       ("bison" ,bison)))
    (home-page "http://freecode.com/projects/libidl")
    (synopsis "Create trees of CORBA Interface Definition Language files")
    (description  "Libidl is a library for creating trees of CORBA Interface
Definition Language (idl) files, which is a specification for defining
portable interfaces. libidl was initially written for orbit (the orb from the
GNOME project, and the primary means of libidl distribution).  However, the
functionality was designed to be as reusable and portable as possible.")
    (license license:lgpl2.0+)))


(define-public orbit2
  (package
    (name "orbit2")
    (version "2.14.19")
    (source (origin
              (method url-fetch)
              (uri (let ((upstream-name "ORBit2"))
                     (string-append "mirror://gnome/sources/" upstream-name "/"
                                    (version-major+minor version) "/"
                                    upstream-name "-" version ".tar.bz2")))
              (sha256
               (base32
                "0l3mhpyym9m5iz09fz0rgiqxl2ym6kpkwpsp1xrr4aa80nlh1jam"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       '(;; We don't need static libraries, plus they don't build reproducibly
         ;; (non-deterministic ordering of .o files in the archive.)
         "--disable-static"

         ;; The programmer kindly gives us a hook to turn off deprecation
         ;; warnings ...
         "DISABLE_DEPRECATED_CFLAGS=-DGLIB_DISABLE_DEPRECATION_WARNINGS")
       ;; ... which they then completly ignore !!
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'ignore-deprecations
           (lambda _
             (substitute* "linc2/src/Makefile.in"
               (("-DG_DISABLE_DEPRECATED") "-DGLIB_DISABLE_DEPRECATION_WARNINGS"))
             #t)))))
    (inputs `(("glib" ,glib)
              ("libidl" ,libidl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://projects.gnome.org/orbit2/")
    (synopsis "CORBA 2.4-compliant Object Request Broker")
    (description  "ORBit2 is a CORBA 2.4-compliant Object Request Broker (orb)
featuring mature C, C++ and Python bindings.")
    ;; Licence notice is unclear.  The Web page simply say "GPL" without giving
    ;; a version.  SOME of the code files have licence notices for GPLv2+.
    ;; The tarball contains files of the text of GPLv2 and LGPLv2.
    (license license:gpl2+)
    (properties `((upstream-name . "ORBit2")))))


(define-public libbonobo
  (package
    (name "libbonobo")
    (version "2.32.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)
                                  "/" name "-" version ".tar.bz2"))
              (sha256
               (base32 "0swp4kk6x7hy1rvd1f9jba31lvfc6qvafkvbpg9h0r34fzrd8q4i"))
              (patches (search-patches
                        "libbonobo-activation-test-race.patch"))))
    (build-system gnu-build-system)
    (arguments
     ;; The programmer kindly gives us a hook to turn off deprecation warnings ...
     `(#:configure-flags
       '("DISABLE_DEPRECATED_CFLAGS=-DGLIB_DISABLE_DEPRECATION_WARNINGS")
       ;; ... which they then completly ignore !!
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'ignore-deprecations
           (lambda _
             (substitute* "activation-server/Makefile.in"
               (("-DG_DISABLE_DEPRECATED") "-DGLIB_DISABLE_DEPRECATION_WARNINGS"))
             #t)))

       ;; There's apparently a race condition between the server stub
       ;; generation and linking of the example under 'samples/echo' that can
       ;; lead do undefined references when building in parallel, as reported
       ;; at <https://forums.gentoo.org/viewtopic-t-223376-start-550.html>.
       ;; Thus, disable parallel builds.
       #:parallel-build? #f))
    (inputs `(("popt" ,popt)
              ("libxml2" ,libxml2)))
    ;; The following are Required by the .pc file
    (propagated-inputs
     `(("glib" ,glib)
       ("orbit2" ,orbit2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("flex" ,flex)
       ("bison" ,bison)))
    (home-page "https://developer.gnome.org/libbonobo/")
    (synopsis "Framework for creating reusable components for use in GNOME applications")
    (description "Bonobo is a framework for creating reusable components for
use in GNOME applications, built on top of CORBA.")
    ;; Licence not explicitly stated.  Source files contain no licence notices.
    ;; Tarball contains text of both GPLv2 and LGPLv2
    ;; GPLv2 covers both conditions
    (license license:gpl2+)))


(define-public gconf
  (package
    (name "gconf")
    (version "3.2.6")
    (source (origin
              (method url-fetch)
              (uri
               (let ((upstream-name "GConf"))
                 (string-append "mirror://gnome/sources/" upstream-name "/"
                                (version-major+minor version) "/"
                                upstream-name "-" version ".tar.xz")))
              (sha256
               (base32 "0k3q9nh53yhc9qxf1zaicz4sk8p3kzq4ndjdsgpaa2db0ccbj4hr"))))
    (build-system gnu-build-system)
    (inputs `(("dbus-glib" ,dbus-glib)
              ("libxml2" ,libxml2)))
    (propagated-inputs `(("glib" ,glib) ; referred to in the .pc file
                         ("orbit2" ,orbit2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("pkg-config" ,pkg-config)))
    (home-page "https://projects.gnome.org/gconf/")
    (synopsis "Store application preferences")
    (description "Gconf is a system for storing application preferences.  It
is intended for user preferences; not arbitrary data storage.")
    (license license:lgpl2.0+)
    (properties '((upstream-name . "GConf")))))


(define-public gnome-mime-data
  (package
    (name "gnome-mime-data")
    (version "2.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1mvg8glb2a40yilmyabmb7fkbzlqd3i3d31kbkabqnq86xdnn69p"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("intltool" ,intltool)))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'configure 'use-our-intltool
                    (lambda _
                      ;; Do not use the bundled intltool commands, which lack
                      ;; the "dotless @INC" fixes of our 'intltool' package.
                      (substitute* (find-files "." "^Makefile$")
                        (("^INTLTOOL_(EXTRACT|UPDATE|MERGE) = .*$" _ tool)
                         (string-append "INTLTOOL_" tool " = intltool-"
                                        (string-downcase tool) "\n")))
                      #t)))))
    (home-page "https://www.gnome.org")
    (synopsis "Base MIME and Application database for GNOME")
    (description  "GNOME Mime Data is a module which contains the base MIME
and Application database for GNOME.  The data stored by this module is
designed to be accessed through the MIME functions in GnomeVFS.")
    (license license:gpl2+)))


(define-public gnome-vfs
  (package
    (name "gnome-vfs")
    (version "2.24.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1ajg8jb8k3snxc7rrgczlh8daxkjidmcv3zr9w809sq4p2sn9pk2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'ignore-deprecations
           (lambda _
             (substitute* '("libgnomevfs/Makefile.in"
                            "daemon/Makefile.in")
               (("-DG_DISABLE_DEPRECATED") "-DGLIB_DISABLE_DEPRECATION_WARNINGS"))
             #t))
         (add-before 'configure 'patch-test-async-cancel-to-never-fail
           (lambda _
             (substitute* "test/test-async-cancel.c"
               (("EXIT_FAILURE") "77"))
             #t)))))
    (inputs `(("libxml2" ,libxml2)
              ("dbus-glib" ,dbus-glib)
              ("gconf" ,gconf)
              ("gnome-mime-data" ,gnome-mime-data)
              ("zlib" ,zlib)))
    (native-inputs
     `(("glib" ,glib "bin")             ; for glib-mkenums, etc.
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://developer.gnome.org/gnome-vfs/")
    (synopsis "Access files and folders in GNOME applications")
    (description
     "GnomeVFS is the core library used to access files and folders in GNOME
applications.  It provides a file system abstraction which allows applications
to access local and remote files with a single consistent API.")
    (license license:lgpl2.0+)))



(define-public libgnome
  (package
    (name "libgnome")
    (version "2.32.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "197pnq8y0knqjhm2fg4j6hbqqm3qfzfnd0irhwxpk1b4hqb3kimj"))
              (patches (search-patches "libgnome-encoding.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'enable-deprecated
           (lambda _
             (substitute* "libgnome/Makefile.in"
               (("-DG_DISABLE_DEPRECATED") "-DGLIB_DISABLE_DEPRECATION_WARNINGS"))
             #t)))))
    (inputs `(("libxml2" ,libxml2)))
    (native-inputs
     `(("glib" ,glib "bin")             ; for glib-mkenums, etc.
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    ;; The following are listed as Required in the .pc file
    ;; (except for libcanberra -- which seems to be oversight on the part
    ;; of the upstream developers -- anything that links against libgnome,
    ;; must also link against libcanberra
    (propagated-inputs
     `(("libcanberra" ,libcanberra)
       ("libbonobo" ,libbonobo)
       ("gconf" ,gconf)
       ("gnome-vfs" ,gnome-vfs)
       ("popt" ,popt)))                       ;gnome-program.h includes popt.h
    (home-page "https://developer.gnome.org/libgnome/")
    (synopsis "Useful routines for building applications")
    (description  "The libgnome library provides a number of useful routines
for building modern applications, including session management, activation of
files and URIs, and displaying help.")
    (license license:lgpl2.0+)))


(define-public libart-lgpl
  (package
    (name "libart-lgpl")
    (version "2.3.21")
    (source (origin
              (method url-fetch)
              (uri (let ((upstream-name "libart_lgpl"))
                     (string-append "mirror://gnome/sources/" upstream-name "/"
                                    (version-major+minor version) "/"
                                    upstream-name "-" version ".tar.bz2")))
              (sha256
               (base32
                "1yknfkyzgz9s616is0l9gp5aray0f2ry4dw533jgzj8gq5s1xhgx"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://people.gnome.org/~mathieu/libart")
    (synopsis "2D drawing library")
    (description  "Libart is a 2D drawing library intended as a
high-quality vector-based 2D library with antialiasing and alpha composition.")
    (license license:lgpl2.0+)))



(define-public libgnomecanvas
  (package
    (name "libgnomecanvas")
    (version "2.30.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1nhnq4lfkk8ljkdafscwaggx0h95mq0rxnd7zgqyq0xb6kkqbjm8"))))
    (build-system gnu-build-system)
    ;; Mentioned as Required in the .pc file
    (propagated-inputs `(("libart-lgpl" ,libart-lgpl)
                         ("gtk+" ,gtk+-2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("pkg-config" ,pkg-config)))
    (home-page "https://developer.gnome.org/libgnomecanvas/")
    (synopsis "Flexible widget for creating interactive structured graphics")
    (description  "The GnomeCanvas widget provides a flexible widget for
creating interactive structured graphics.")
    (license license:lgpl2.0+)))

(define-public libgnomecanvasmm
  (package
    (name "libgnomecanvasmm")
    (version "2.26.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0679hcnpam2gkag2i63sm0wdm35gwvzafnz1354mg6j5gzwpfrcr"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("CXXFLAGS=-std=c++11"))) ; required by gtkmm
    (propagated-inputs `(("libgnomecanvas" ,libgnomecanvas)))
    (native-inputs
     `(("gtkmm-2" ,gtkmm-2)
       ("pkg-config" ,pkg-config)))
    (home-page "https://gtkmm.org")
    (synopsis "C++ bindings to the GNOME Canvas library")
    (description "C++ bindings to the GNOME Canvas library.")
    (license license:lgpl2.0+)))

(define-public libgnomeui
  (package
    (name "libgnomeui")
    (version "2.24.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (patches (search-patches "libgnomeui-utf8.patch"))
              (sha256
               (base32
                "03rwbli76crkjl6gp422wrc9lqpl174k56cp9i96b7l8jlj2yddf"))))
    (build-system gnu-build-system)
    ;; Mentioned as Required in the .pc file
    (propagated-inputs `(("libbonoboui" ,libbonoboui)
                         ("libgnome" ,libgnome)
                         ("libgnomecanvas" ,libgnomecanvas)
                         ("libgnome-keyring" ,libgnome-keyring)))
    (inputs `(("libjpeg" ,libjpeg)
              ("popt" ,popt)
              ("libbonobo" ,libbonobo)
              ("libxml2" ,libxml2)
              ("libglade" ,libglade)))
    (native-inputs
     `(("glib" ,glib "bin")             ; for glib-mkenums, etc.
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://developer.gnome.org/libgnomeui/")
    (synopsis "Additional widgets for applications")
    (description "The libgnomeui library provides additional widgets for
applications.  Many of the widgets from libgnomeui have already been
ported to GTK+.")
    (license license:lgpl2.0+)))

(define-public libglade
  (package
    (name "libglade")
    (version "2.6.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1v2x2s04jry4gpabws92i0wq2ghd47yr5n9nhgnkd7c38xv1wdk4"))))
    (build-system gnu-build-system)
    (inputs
     `(("python" ,python))) ;; needed for the optional libglade-convert program
    (propagated-inputs
     `(("gtk+-2" ,gtk+-2)
       ("libxml2" ,libxml2))) ; required by libglade-2.0.pc
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://developer.gnome.org/libglade")
    (synopsis "Load glade interfaces and access the glade built widgets")
    (description "Libglade is a library that provides interfaces for loading
graphical interfaces described in glade files and for accessing the
widgets built in the loading process.")
    (license license:gpl2+))) ; This is correct.  GPL not LGPL

(define-public libgnomeprint
  ;; This library has been deprecated since 2006; see
  ;; <https://mail.gnome.org/archives/devel-announce-list/2006-August/msg00005.html>.
  (package
    (name "libgnomeprint")
    (version "2.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "129ka3nn8gx9dlfry17ib79azxk45wzfv5rgqzw6dwx2b5ns8phm"))))
    (build-system gnu-build-system)
    (inputs
     `(("popt" ,popt)
       ("libart-lgpl" ,libart-lgpl)
       ("gtk+" ,gtk+-2)
       ("libxml2" ,libxml2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("pkg-config" ,pkg-config)))
    (home-page "https://projects.gnome.org/gnome-print/home/faq.html")
    (synopsis "Printing framework for GNOME")
    (description
     "GNOME-print was a printing framework for GNOME.  It has been deprecated
since ca. 2006, when GTK+ itself incorporated printing support.")
    (license license:lgpl2.0+)))


(define-public libgnomeprintui
  ;; Deprecated; see libgnomeprint.
  (package
    (name "libgnomeprintui")
    (version "2.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1ivipk7r61rg90p9kp889j28xlyyj6466ypvwa4jvnrcllnaajsw"))))
    (build-system gnu-build-system)
    ;; Mentioned as Required in the .pc file
    (propagated-inputs `(("libgnomeprint" ,libgnomeprint)))
    (inputs `(("gtk+" ,gtk+-2)
              ("glib" ,glib)
              ("gnome-icon-theme" ,gnome-icon-theme)
              ("libgnomecanvas" ,libgnomecanvas)
              ("libxml2" ,libxml2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://projects.gnome.org/gnome-print/home/faq.html")
    (synopsis "Printing framework for GNOME")
    (description (package-description libgnomeprint))
    (license license:lgpl2.0+)))

(define-public libbonoboui
  (package
    (name "libbonoboui")
    (version "2.24.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1kbgqh7bw0fdx4f1a1aqwpff7gp5mwhbaz60c6c98bc4djng5dgs"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'start-xserver
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xorg-server (assoc-ref inputs "xorg-server"))
                   (disp ":1"))

               (setenv "HOME" (getcwd))
               (setenv "DISPLAY" disp)
               ;; There must be a running X server and make check doesn't start one.
               ;; Therefore we must do it.
               (zero? (system (format #f "~a/bin/Xvfb ~a &" xorg-server disp)))))))))
    ;; Mentioned as Required by the .pc file
    (propagated-inputs `(("libxml2" ,libxml2)))
    (inputs
     `(("popt" ,popt)
       ("pangox-compat" ,pangox-compat)
       ("libgnome" ,libgnome)
       ("libgnomecanvas" ,libgnomecanvas)
       ("libglade" ,libglade)))
    (native-inputs
     `(("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("intltool" ,intltool)
       ("xorg-server" ,xorg-server) ; For running the tests
       ("pkg-config" ,pkg-config)))
    (home-page "https://developer.gnome.org/libbonoboui/")
    (synopsis "Some user interface controls using Bonobo")
    (description  "The Bonobo UI library provides a number of user interface
controls using the Bonobo component framework.")
    (license license:lgpl2.0+)))

(define-public libwnck
  (package
    (name "libwnck")
    (version "3.24.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "010zk9zvydggxqnxfml3scml5yxmpjy90irpqcayrzw26lldr9mg"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (propagated-inputs
     `(("gtk+" ,gtk+)
       ("libxres" ,libxres)
       ("startup-notification" ,startup-notification)))
    (home-page "https://developer.gnome.org/libwnck/")
    (synopsis "Window Navigator Construction Kit")
    (description
     "Libwnck is the Window Navigator Construction Kit, a library for use in
writing pagers, tasklists, and more generally applications that are dealing
with window management.  It tries hard to respect the Extended Window Manager
Hints specification (EWMH).")
    (license license:lgpl2.0+)))

;; stable version for gtk2, required by xfwm4.
(define-public libwnck-2
  (package (inherit libwnck)
    (name "libwnck")
    (version "2.30.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "15713yl0f8f3p99jzqqfmbicrdswd3vwpx7r3bkf1bgh6d9lvs4b"))))
    (propagated-inputs
     `(("gtk+" ,gtk+-2)
       ("libxres" ,libxres)
       ("startup-notification" ,startup-notification)))))

(define-public goffice
  (package
    (name "goffice")
    (version "0.10.44")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32 "1fd7cm6j0g0mqgpqs4y22b4gd2ll4mcyvg4d0q22d5ndjapl4q3d"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                            ;4.0 MiB of gtk-doc
    (arguments
     '(#:configure-flags (list (string-append "--with-html-dir="
                                              (assoc-ref %outputs "doc")
                                              "/share/gtk-doc/html"))))
    (inputs
     `(("gtk+" ,gtk+)
       ("libgsf" ,libgsf)
       ("librsvg" ,librsvg)
       ("libxslt" ,libxslt)
       ("libxml2" ,libxml2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (home-page "https://developer.gnome.org/goffice/")
    (synopsis "Document-centric objects and utilities")
    (description "A GLib/GTK+ set of document-centric objects and utilities.")
    (license
     ;; Dual licensed under GPLv2 or GPLv3 (both without "or later")
     ;; Note: NOT LGPL
     (list license:gpl2 license:gpl3))))

(define-public goffice-0.8
  (package (inherit goffice)
    (version "0.8.17")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" (package-name goffice) "/"
                                  (version-major+minor version)  "/"
                                  (package-name goffice) "-" version ".tar.xz"))
              (sha256
               (base32 "05fvzbs5bin05bbsr4dp79aiva3lnq0a3a40zq55i13vnsz70l0n"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-pcre-check
           (lambda _
             ;; Only glib.h can be included directly.  See
             ;; https://bugzilla.gnome.org/show_bug.cgi?id=670316
             (substitute* "configure"
               (("glib/gregex\\.h") "glib.h")) #t)))

       ,@(package-arguments goffice)))
    (propagated-inputs
     ;; libgoffice-0.8.pc mentions libgsf-1
     `(("libgsf" ,libgsf)))
    (inputs
     `(("gtk" ,gtk+-2)
       ,@(alist-delete "gtk" (package-inputs goffice))))))

(define-public gnumeric
  (package
    (name "gnumeric")
    (version "1.12.44")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnumeric/"
                                  (version-major+minor version)  "/"
                                  "gnumeric-" version ".tar.xz"))
              (sha256
               (base32
                "0147962c6ybdsj57rz95nla0rls7g545wc2n7pz59zmzyd5pksk0"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(;; The gnumeric developers don't worry much about failing tests.
       ;; See https://bugzilla.gnome.org/show_bug.cgi?id=732387
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'pre-conf
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Make install tries to write into the directory of goffice
             ;; I am informed that this only affects the possibility to embed a
             ;; spreadsheet inside an Abiword document.   So presumably when we
             ;; package Abiword we'll have to refer it to this directory.
             (substitute* "configure"
               (("^GOFFICE_PLUGINS_DIR=.*")
                (string-append "GOFFICE_PLUGINS_DIR="
                               (assoc-ref outputs "out")
                               "/goffice/plugins"))))))))
    (inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("goffice" ,goffice)
       ("libgsf" ,libgsf)
       ("librsvg" ,librsvg)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("python" ,python-2)
       ("python2-pygobject" ,python2-pygobject)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bison" ,bison)
       ("docbook-xml" ,docbook-xml)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (home-page "http://www.gnumeric.org")
    (synopsis "Spreadsheet application")
    (description
     "GNUmeric is a GNU spreadsheet application, running under GNOME.  It is
interoperable with other spreadsheet applications.  It has a vast array of
features beyond typical spreadsheet functionality, such as support for linear
and non-linear solvers, statistical analysis, and telecommunication
engineering.")
    (license
    ;; Dual licensed under GPLv2 or GPLv3 (both without "or later")
     (list license:gpl2 license:gpl3))))

(define-public gnome-themes-standard
  (package
    (name "gnome-themes-standard")
    (version "3.22.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/" name "-"
                           version ".tar.xz"))
       (sha256
        (base32
         "0smmiamrgcgf5sa88bsn8hwmvsyx4gczzs359nwxbkv14b2qgp31"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       ;; Don't create 'icon-theme.cache'.
       (let* ((coreutils (assoc-ref %build-inputs "coreutils"))
              (true      (string-append coreutils "/bin/true")))
         (list (string-append "GTK_UPDATE_ICON_CACHE=" true)))))
    (inputs
     `(("gtk+" ,gtk+)
       ("gtk+-2" ,gtk+-2)
       ("librsvg" ,librsvg)
       ("libxml2" ,libxml2)
       ("glib" ,glib)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (home-page "https://launchpad.net/gnome-themes-standard")
    (synopsis "Default GNOME 3 themes")
    (description
     "The default GNOME 3 themes (Adwaita and some accessibility themes).")
    (license license:lgpl2.1+)))

(define-public seahorse
  (package
    (name "seahorse")
    (version "3.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/" name "-"
                           version ".tar.xz"))
       (sha256
        (base32
         "1py6fj19kb8aaxvg6yrpd0876azc2zjvis98aqz37a2lxmhp9c72"))))
    (build-system glib-or-gtk-build-system)
    (inputs
     `(("gtk+" ,gtk+)
       ("gcr" ,gcr)
       ("gnupg" ,gnupg)
       ("gpgme" ,gpgme)
       ("openldap" ,openldap)
       ("openssh" ,openssh)
       ("libsecret" ,libsecret)
       ("libsoup" ,libsoup)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib:bin" ,glib "bin")
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (home-page "https://launchpad.net/gnome-themes-standard")
    (synopsis "Manage encryption keys and passwords in the GNOME keyring")
    (description
     "Seahorse is a GNOME application for managing encryption keys and
passwords in the GNOME keyring.")
    (license license:gpl2+)))

(define-public vala
  (package
    (name "vala")
    (version "0.40.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0yvaijkpahzz26sa37cyzbj75a9vbcbgvxbqzzb7hbcvfy009zy7"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
                     (lambda _
                       (setenv "CC" "gcc")
                       (substitute* "valadoc/tests/testrunner.sh"
                         (("export PKG_CONFIG_PATH=" m)
                          (string-append m "$PKG_CONFIG_PATH:")))
                       ;; For missing '/etc/machine-id'.
                       (setenv "DBUS_FATAL_WARNINGS" "0")
                       #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("flex" ,flex)
       ("bison" ,bison)
       ("xsltproc" ,libxslt)
       ("grep" ,grep)
       ("sed" ,sed)
       ("dbus" ,dbus)                                     ; for dbus tests
       ("gobject-introspection" ,gobject-introspection))) ; for gir tests
    (inputs
     `(("graphviz" ,graphviz)))
    (propagated-inputs
     `(("glib" ,glib))) ; required by libvala-0.40.pc
    (home-page "https://live.gnome.org/Vala/")
    (synopsis "Compiler for the GObject type system")
    (description
     "Vala is a programming language that aims to bring modern programming
language features to GNOME developers without imposing any additional runtime
requirements and without using a different ABI compared to applications and
libraries written in C.")
    (license license:lgpl2.1+)))

(define-public vte
  (package
    (name "vte")
    (version "0.52.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1br6kg0wzf1wmww1hadihhcpqbamalqmbppfdzjvzk1ayp75f9hg"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("vala" ,vala)
       ("gobject-introspection" ,gobject-introspection)
       ("glib" ,glib "bin") ; for glib-genmarshal, etc.
       ("gperf" ,gperf)
       ("xmllint" ,libxml2)))
    (propagated-inputs
     `(("gtk+" ,gtk+)                             ;required by vte-2.91.pc
       ("gnutls" ,gnutls)                         ;ditto
       ("pcre2" ,pcre2)))                         ;ditto
    (home-page "https://www.gnome.org/")
    (synopsis "Virtual Terminal Emulator")
    (description
     "VTE is a library (libvte) implementing a terminal emulator widget for
GTK+, and a minimal sample application (vte) using that.  Vte is mainly used in
gnome-terminal, but can also be used to embed a console/terminal in games,
editors, IDEs, etc.")
    (license license:lgpl2.1+)))

(define-public vte-ng
  (package
    (inherit vte)
    (name "vte-ng")
    (version "0.54.2.a")
    (home-page "https://github.com/thestinger/vte-ng")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1r7d9m07cpdr4f7rw3yx33hmp4jmsk0dn5byq5wgksb2qjbc4ags"))))
    (native-inputs
     `(("gtk-doc" ,gtk-doc)
       ("gperf" ,gperf)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ,@(package-native-inputs vte)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'bootstrap
                    (lambda _
                      (setenv "NOCONFIGURE" "true")
                      (invoke "sh" "autogen.sh"))))))
  (synopsis "Enhanced VTE terminal widget")
  (description
   "VTE is a library (libvte) implementing a terminal emulator widget for
GTK+, this fork provides additional functions exposed for keyboard text
selection and URL hints.")))

;; provides vte 2.90, required for some terminal emulators
;; tilda bug: https://github.com/lanoxx/tilda/issues/94
;; pantheon-terminal bug: https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=788021
;; roxterm bug: http://sourceforge.net/p/roxterm/bugs/107/
;; pantheon-terminal, roxterm are not currently packaged
(define-public vte-0.36
  (package (inherit vte)
    (name "vte")
    (version "0.36.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1psfnqsmxx4qzc55qwvb8jai824ix4pqcdqhgxk0g2zh82bcxhn2"))))
    (propagated-inputs
     `(("gtk" ,gtk+)
       ("ncurses" ,ncurses)))))

;; stable version for gtk2, required by xfce4-terminal.
(define-public vte/gtk+-2
  (package (inherit vte)
    (name "vte")
    (version "0.28.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1bmhahkf8wdsra9whd3k5l5z4rv7r58ksr8mshzajgq2ma0hpkw6"))
              (patches (search-patches
                         "vte-CVE-2012-2738-pt1.patch"
                         "vte-CVE-2012-2738-pt2.patch"))))
    (arguments
     '(#:configure-flags '("--disable-python")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("glib" ,glib "bin")))   ; for glib-genmarshal, etc.
    (propagated-inputs
     `(("gtk+" ,gtk+-2)         ; required by libvte.pc
       ("ncurses" ,ncurses))))) ; required by libvte.la

(define-public vinagre
  (package
    (name "vinagre")
    (version "3.22.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (patches (search-patches "vinagre-newer-freerdp.patch"
                                       "vinagre-newer-rdp-parameters.patch"))
              (sha256
               (base32
                "10jya3jyrm18nbw3v410gbkc7677bqamax44pzgd3j15randn76d"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'skip-gtk-update-icon-cache
           (lambda _
             ;; Don't create 'icon-theme.cache'
             (substitute* (find-files "." "^Makefile$")
               (("gtk-update-icon-cache") (which "true")))
             #t))
         (add-after 'unpack 'patch-configure
           (lambda _
             (substitute* "configure"
               (("freerdp") "freerdp2"))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("glib-bin" ,glib "bin")))                 ;for glib-compile-schemas
    (inputs
     `(("libxml2" ,libxml2)
       ("gtk-vnc" ,gtk-vnc)
       ("gnome-keyring" ,gnome-keyring)
       ("libsecret" ,libsecret)
       ("freerdp" ,freerdp)
       ("spice" ,spice)
       ("spice-gtk" ,spice-gtk)
       ("telepathy-glib" ,telepathy-glib)
       ("vte" ,vte)))
    (home-page "https://wiki.gnome.org/Apps/Vinagre")
    (synopsis "Remote desktop viewer for GNOME")
    (description "Vinagre is a remote display client supporting the VNC, SPICE
and RDP protocols.")
    (license license:gpl3+)))

(define-public dconf
  (package
    (name "dconf")
    (version "0.28.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0hn7v6769xabqz7kvyb2hfm19h46z1whkair7ff752zmbs3b7lv1"))))
    (build-system meson-build-system)
    (propagated-inputs
     ;; In Requires of dconf.pc.
     `(("glib" ,glib)))
    (inputs
     `(("gtk+" ,gtk+)
       ("dbus" ,dbus)))
    (native-inputs
     `(("libxslt" ,libxslt)                     ;for xsltproc
       ("libxml2" ,libxml2)                     ;for XML_CATALOG_FILES
       ("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)
       ("glib:bin" ,glib "bin")
       ("gtk-doc" ,gtk-doc)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (arguments
     `(#:tests? #f ; To contact dbus it needs to load /var/lib/dbus/machine-id
                   ; or /etc/machine-id.
       #:glib-or-gtk? #t
       #:configure-flags '("-Denable-gtk-doc=true")))
    (home-page "https://developer.gnome.org/dconf")
    (synopsis "Low-level GNOME configuration system")
    (description "Dconf is a low-level configuration system.  Its main purpose
is to provide a backend to GSettings on platforms that don't already have
configuration storage systems.")
    (license license:lgpl2.1+)))

(define-public json-glib
  (package
    (name "json-glib")
    (version "1.4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0ixwyis47v5bkx6h8a1iqlw3638cxcv57ivxv4gw2gaig51my33j"))))
    (build-system meson-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib" ,glib "bin")              ;for glib-mkenums and glib-genmarshal
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("glib" ,glib)))                         ;according to json-glib-1.0.pc
    (home-page "https://wiki.gnome.org/Projects/JsonGlib")
    (synopsis "Compiler for the GObject type system")
    (description
     "JSON-GLib is a C library based on GLib providing serialization and
deserialization support for the JavaScript Object Notation (JSON) format
described by RFC 4627.  It provides parser and generator GObject classes and
various wrappers for the complex data types employed by JSON, such as arrays
and objects.")
    (license license:lgpl2.1+)))

(define-public libxklavier
  (package
    (name "libxklavier")
    (version "5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "016lpdv35z0qsw1cprdc2k5qzkdi5waj6qmr0a2q6ljn9g2kpv7b"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--with-xkb-base="
                            (assoc-ref %build-inputs "xkeyboard-config")
                            "/share/X11/xkb"))))
    (native-inputs
     `(("glib:bin"              ,glib "bin") ; for glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config"            ,pkg-config)))
    (propagated-inputs
     ;; Required by libxklavier.pc.
     `(("glib"    ,glib)
       ("libxml2" ,libxml2)))
    (inputs
     `(("iso-codes"        ,iso-codes)
       ("libxi"            ,libxi)
       ("libxkbfile"       ,libxkbfile)
       ("xkbcomp"          ,xkbcomp)
       ("xkeyboard-config" ,xkeyboard-config)))
    (home-page "https://www.freedesktop.org/wiki/Software/LibXklavier/")
    (synopsis "High-level API for X Keyboard Extension")
    (description
     "LibXklavier is a library providing high-level API for X Keyboard
Extension known as XKB.  This library is intended to support XFree86 and other
commercial X servers.  It is useful for creating XKB-related software (layout
indicators etc).")
    (license license:lgpl2.0+)))

(define-public python2-rsvg
  ;; XXX: This is actually a subset of gnome-python-desktop.
  (package
    (name "python2-rsvg")
    (version "2.32.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://gnome/sources/gnome-python-desktop/2.32/gnome-python-desktop-"
             version ".tar.bz2"))
       (sha256
        (base32
         "1s8f9rns9v7qlwjv9qh9lr8crp88dpzfm45hj47zc3ivpy0dbnq9"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("python" ,python-2)
       ("python2-pygtk" ,python2-pygtk)
       ("librsvg" ,librsvg)))
    (home-page "https://www.gnome.org")
    (synopsis "Python bindings to librsvg")
    (description
     "This packages provides Python bindings to librsvg, the SVG rendering
library.")

    ;; This is the license of the rsvg bindings.  The license of each module
    ;; of gnome-python-desktop is given in 'COPYING'.
    (license license:lgpl2.1+)))

(define-public glib-networking
  (package
    (name "glib-networking")
    (version "2.58.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/glib-networking/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0s006gs9nsq6mg31spqha1jffzmp6qjh10y27h0fxf1iw1ah5ymx"))
              (patches (search-patches "glib-networking-connection.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Dlibproxy_support=false")
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'disable-TLSv1.3
                    (lambda _
                      ;; XXX: One test fails when TLS 1.3 is enabled, fixed in 2.60.0:
                      ;; <https://gitlab.com/gnutls/gnutls/issues/615>.
                      (setenv "G_TLS_GNUTLS_PRIORITY" "NORMAL:-VERS-TLS1.3")
                      #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("glib" ,glib)
       ("gnutls" ,gnutls)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("p11-kit" ,p11-kit)))
    (home-page "https://www.gnome.org")
    (synopsis "Network-related GIO modules")
    (description
     "This package contains various network related extensions for the GIO
library.")
    (license license:lgpl2.0+)))

(define-public rest
  (package
    (name "rest")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/rest/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1j81bgqmd55s5lxyaxcplym9n6xywcs1cm9wmvafsg2xiv9sl4q5"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; tests require internet connection
       #:configure-flags
       '("--with-ca-certificates=/etc/ssl/certs/ca-certificates.crt")))
    (native-inputs
     `(("glib-mkenums" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     ;; rest-0.7.pc refers to all these.
     `(("glib"    ,glib)
       ("libsoup" ,libsoup)
       ("libxml2" ,libxml2)))
    (home-page "https://www.gtk.org/")
    (synopsis "RESTful web api query library")
    (description
     "This library was designed to make it easier to access web services that
claim to be \"RESTful\".  It includes convenience wrappers for libsoup and
libxml to ease remote use of the RESTful API.")
    (license license:lgpl2.1+)))

(define-public libsoup
  (package
    (name "libsoup")
    (version "2.64.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libsoup/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1il6lyrmfi0hfh3ysw8w1qzc1rdz0igkb7dv6d8g5mmilnac3pbm"))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:modules ((guix build utils)
                  (guix build gnu-build-system)
                  (ice-9 popen))

       #:configure-flags
       (list (string-append "--with-html-dir="
                            (assoc-ref %outputs "doc")
                            "/share/gtk-doc/html")
             (string-append "--with-apache-module-dir="
                            (assoc-ref %build-inputs "httpd")
                            "/modules"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'disable-unconnected-socket-test
           ;; This test fails due to missing /etc/nsswitch.conf
           ;; in the build environment.
           (lambda _
             (substitute* "tests/socket-test.c"
               ((".*/sockets/unconnected.*") ""))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             ;; The 'check-local' target runs 'env LANG=C sort -u',
             ;; unset 'LC_ALL' to make 'LANG' working.
             (unsetenv "LC_ALL")
             ;; HTTPD in Guix uses mod_event and does not build prefork.
             (substitute* "tests/httpd.conf"
               (("^LoadModule mpm_prefork_module.*$") "\n"))

             ;; Generate a self-signed certificate that has "localhost" as its
             ;; 'dnsName'.  Failing to do that, and starting with GnuTLS
             ;; 3.5.12, tests such as "ssl-tests" fail:
             ;;
             ;; ERROR:ssl-test.c:406:do_tls_interaction_test: Unexpected status 6 Unacceptable TLS certificate (expected 200 OK)
             ;;
             ;; 'certtool' is interactive so we have to pipe it the answers.
             ;; Reported at <https://bugzilla.gnome.org/show_bug.cgi?id=784696>.
             (let ((pipe (open-output-pipe "certtool --generate-self-signed \
 --load-privkey tests/test-key.pem --outfile tests/test-cert.pem")))
               (for-each (lambda (line)
                           (display line pipe)
                           (newline pipe))
                         '(""               ;Common name
                           ""               ;UID
                           "Guix"           ;Organizational unit name
                           "GNU"            ;Organization name
                           ""               ;Locality name
                           ""               ;State or province
                           ""               ;Country
                           ""               ;subject's domain component (DC)
                           ""               ;E-mail
                           ""               ;serial number
                           "-1"             ;expiration time
                           "N"              ;belong to authority?
                           "N"              ;web client certificate?
                           "N"              ;IPsec IKE?
                           "Y"              ;web server certificate?
                           "localhost"      ;dnsName of subject
                           ""               ;dnsName of subject (end)
                           ""               ;URI of subject
                           "127.0.0.1"      ;IP address of subject
                           ""               ;signing?
                           ""               ;encryption (RSA)?
                           ""               ;data encryption?
                           ""               ;sign OCSP requests?
                           ""               ;sign code?
                           ""               ;time stamping?
                           ""               ;email protection?
                           ""               ;URI of the CRL distribution point
                           "y"              ;above info OK?
                           ))
               (close-pipe pipe))
             #t))
         (replace 'install
           (lambda _
             (invoke "make"
                     ;; Install vala bindings into $out.
                     (string-append "vapidir=" %output
                                    "/share/vala/vapi")
                     "install"))))))
    (native-inputs
     `(("glib:bin" ,glib "bin")                   ; for glib-mkenums
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("vala" ,vala)
       ;; These are needed for the tests.
       ;; FIXME: Add PHP once available.
       ("curl" ,curl)
       ("gnutls" ,gnutls)                         ;for 'certtool'
       ("httpd" ,httpd)))
    (propagated-inputs
     ;; libsoup-2.4.pc refers to all these.
     `(("glib" ,glib)
       ("libxml2" ,libxml2)))
    (inputs
     `(("glib-networking" ,glib-networking)
       ("libpsl" ,libpsl)
       ("sqlite" ,sqlite)))
    (home-page "https://live.gnome.org/LibSoup/")
    (synopsis "GLib-based HTTP Library")
    (description
     "LibSoup is an HTTP client/server library for GNOME.  It uses GObjects
and the GLib main loop, to integrate well with GNOME applications.")
    (license license:lgpl2.0+)))

(define-public libsecret
  (package
    (name "libsecret")
    (version "0.18.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/libsecret/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "058x64689k55wxfkdp4svhnwvv8jmqm7z5mrynybl38f4sfqiyiv"))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:tests? #f ; FIXME: Testing hangs.
       #:configure-flags
       (list (string-append "--with-html-dir="
                            (assoc-ref %outputs "doc")
                            "/share/gtk-doc/html"))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin") ; for gdbus-codegen, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xsltproc" ,libxslt)))
       ;; These are needed for the tests.
       ;; FIXME: Add gjs once available.
       ;("dbus" ,dbus)
       ;("python2" ,python-2)
       ;("python2-dbus" ,python2-dbus)
       ;("python2-pygobject" ,python2-pygobject)
       ;("python2-pygobject-2" ,python2-pygobject-2)))
    (propagated-inputs
     `(("glib" ,glib))) ; required by libsecret-1.pc
    (inputs
     `(("docbook-xsl" ,docbook-xsl)
       ("libgcrypt" ,libgcrypt)
       ("libxml2" ,libxml2))) ; for XML_CATALOG_FILES
    (home-page "https://wiki.gnome.org/Projects/Libsecret/")
    (synopsis "GObject bindings for \"Secret Service\" API")
    (description
     "Libsecret is a GObject based library for storing and retrieving passwords
and other secrets.  It communicates with the \"Secret Service\" using DBus.")
    (license license:lgpl2.1+)))

(define-public five-or-more
  (package
    (name "five-or-more")
    (version "3.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0wkp08xsqr9p9cgdfghi424diajs016b2h4bsfcr5y9xc3rgf93w"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("desktop-file-utils" ,desktop-file-utils)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("xmllint" ,libxml2)))
    (inputs
     `(("gtk+" ,gtk+)
       ("librsvg" ,librsvg)))
    (home-page "https://wiki.gnome.org/Apps/Five%20or%20more")
    (synopsis "Logic puzzle game")
    (description "Five or More is a game where you try to align
 five or more objects of the same color and shape causing them to disappear.
 On every turn more objects will appear, until the board is full.
 Try to last as long as possible.")
    (license license:gpl2+)))

(define-public gnome-mines
  (package
    (name "gnome-mines")
    (version "3.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1sfxdvns8nppdagnhpd9vd7n77sk5rdji3kdqnc78c2p1npiw11b"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-/bin/true
                     (lambda _
                       (substitute* "configure"
                         (("/bin/true") (which "true"))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("desktop-file-utils" ,desktop-file-utils)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("xmllint" ,libxml2)))
    (inputs
     `(("gtk+" ,gtk+)
       ("libgnome-games-support" ,libgnome-games-support)
       ("librsvg" ,librsvg)))
    (home-page "https://wiki.gnome.org/Apps/Mines")
    (synopsis "Minesweeper game")
    (description
     "Mines (previously gnomine) is a puzzle game where you locate mines
floating in an ocean using only your brain and a little bit of luck.")
    (license license:gpl2+)))

(define-public gnome-sudoku
  (package
    (name "gnome-sudoku")
    (version "3.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "07b4lzniaf3gjsss6zl1lslv18smwc4nrijykvn2z90f423q2xav"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("desktop-file-utils" ,desktop-file-utils)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("xmllint" ,libxml2)))
    (inputs
     `(("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("libgee" ,libgee)
       ("librsvg" ,librsvg)
       ("qqwing" ,qqwing)))
    (home-page "https://wiki.gnome.org/Apps/Sudoku")
    (synopsis "Japanese logic game")
    (description
     "Sudoku is a Japanese logic game that exploded in popularity in 2005.
GNOME Sudoku is meant to have an interface as simple and unobstrusive as
possible while still providing features that make playing difficult Sudoku
more fun.")
    (license license:gpl2+)))

(define-public gnome-terminal
  (package
    (name "gnome-terminal")
    (version "3.28.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0ybjansg6lr279191w8z8r45gy4rxwzw1ajm98cgkv0fk2jdr0x2"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:configure-flags
       (list "--disable-migration" "--disable-search-provider"
             "--without-nautilus-extension")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-/bin/true
                     (lambda _
                       (substitute* "configure"
                         (("/bin/true") (which "true"))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("desktop-file-utils" ,desktop-file-utils)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("xmllint" ,libxml2)))
    (propagated-inputs
     `(("dconf" ,dconf)))
    (inputs
     `(("gtk+" ,gtk+)
       ("vte" ,vte)
       ("gnutls" ,gnutls)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("util-linux" ,util-linux)
       ("vala" ,vala)))
    (home-page "https://wiki.gnome.org/Apps/Terminal")
    (synopsis "Terminal emulator")
    (description
     "GNOME Terminal is a terminal emulator application for accessing a
UNIX shell environment which can be used to run programs available on
your system.

It supports several profiles, multiple tabs and implements several
keyboard shortcuts.")
    (license license:gpl3+)))

(define-public colord
  (package
    (name "colord")
    (version "1.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.freedesktop.org/software/colord/releases/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "01w97rgzk4qi6fp03scq5jyw0ayx11b479p7dkm2r77k84b9agph"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(;; The tests want to run valgrind.  Punt for now.
       #:tests? #f
       #:configure-flags (list "--localstatedir=/var"
                               ;; GUSB not packaged yet.
                               "--disable-gusb"
                               ;; No dep on systemd.
                               "--disable-systemd-login"
                               ;; Wants to install to global completion dir;
                               ;; punt.
                               "--disable-bash-completion"
                               ;; colord-gtk not packaged yet.
                               "--disable-session-example"
                               "--with-daemon-user=colord"
                               "--enable-sane"
                               (string-append "--with-udevrulesdir="
                                              (assoc-ref %outputs "out")
                                              "/lib/udev/rules.d"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-/bin/true
                     (lambda _
                       (substitute* "configure"
                         (("/bin/true") (which "true")))
                       (substitute* "src/Makefile.in"
                         (("if test -w \\$\\(DESTDIR\\)\\$\\(prefix\\)/;")
                          "if test -w $(DESTDIR)$(localstatedir);")))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gobject-introspection" ,gobject-introspection)
       ("libtool" ,libtool)
       ("intltool" ,intltool)))
    (propagated-inputs
     ;; colord.pc refers to all these.
     `(("glib" ,glib)
       ("udev" ,eudev)
       ("lcms" ,lcms)))
    (inputs
     `(("dbus-glib" ,dbus-glib)
       ("libgudev" ,libgudev)
       ("libusb" ,libusb)
       ("sqlite" ,sqlite)
       ("polkit" ,polkit)
       ("sane-backends" ,sane-backends)))
    (home-page "https://www.freedesktop.org/software/colord/")
    (synopsis "Color management service")
    (description "Colord is a system service that makes it easy to manage,
install and generate color profiles to accurately color manage input and
output devices.")
    (license license:gpl2+)))

(define-public geoclue
  (package
    (name "geoclue")
    (version "2.4.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.freedesktop.org/software/" name
                           "/releases/" (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "08yg1r7m0n9hwyvcy769qkmkf8lslqwv69cjfffwnc3zm5km25qj"))
       (patches (search-patches "geoclue-config.patch"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(;; The tests want to run the system bus.
       #:tests? #f
       #:configure-flags (list ;; Disable bits requiring ModemManager.
                               "--disable-3g-source"
                               "--disable-cdma-source"
                               "--disable-modem-gps-source"
                               "--with-dbus-service-user=geoclue")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-/bin/true
                     (lambda _
                       (substitute* "configure"
                         (("/bin/true") (which "true")))
                       #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)))
    (inputs
     `(("avahi" ,avahi)
       ("glib" ,glib)
       ("json-glib" ,json-glib)
       ("libsoup" ,libsoup)))
    (home-page "https://www.freedesktop.org/wiki/Software/GeoClue/")
    (synopsis "Geolocation service")
    (description "Geoclue is a D-Bus service that provides location
information.  The primary goal of the Geoclue project is to make creating
location-aware applications as simple as possible, while the secondary goal is
to ensure that no application can access location information without explicit
permission from user.")
    (license license:gpl2+)))

(define-public geocode-glib
  (package
    (name "geocode-glib")
    (version "3.20.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/geocode-glib/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "18iphsx3bybw7lssbb7rxc1rrnsc8vxai521zkqc535zr8rci7v6"))))
    (build-system gnu-build-system)
    (arguments
     `(;; The tests want to write to $HOME/.cache/geocode-glib, which doesn't
       ;; work for the builder.  Punt.
       #:tests? #f))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-mkenums
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("json-glib" ,json-glib)))
    (propagated-inputs
     ;; geocode-glib-1.0.pc refers to GIO.
     `(("glib" ,glib)))
    (inputs
     `(("libsoup" ,libsoup)))
    (home-page "https://github.com/GNOME/geocode-glib/")
    (synopsis "Geocoding and reverse-geocoding library")
    (description
     "geocode-glib is a convenience library for geocoding (finding longitude,
and latitude from an address) and reverse geocoding (finding an address from
coordinates) using the Nominatim service.  geocode-glib caches requests for
faster results and to avoid unnecessary server load.")
    (license license:lgpl2.0+)))

(define-public upower
  (package
    (name "upower")
    (version "0.99.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.freedesktop.org/upower/upower/"
                                  "uploads/9125ab7ee96fdc4ecc68cfefb50c1cab/"
                                  "upower-" version ".tar.xz"))
              (sha256
               (base32
                "00lzr0vyxz5lvmgya48gdb2cdgmfdim4b34jlfdyqakk1i9sl8xv"))
              (patches (search-patches "upower-builddir.patch"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '( ;; The tests want to contact the system bus, which can't be done in the
       ;; build environment.  The integration test can run, but the last of
       ;; the up-self-tests doesn't.  Disable tests for now.
       #:tests? #f
       #:configure-flags (list "--localstatedir=/var"
                               (string-append "--with-udevrulesdir="
                                              (assoc-ref %outputs "out")
                                              "/lib/udev/rules.d"))))
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("python" ,python)

       ;; For man pages.
       ("libxslt" ,libxslt)                       ;for 'xsltproc'
       ("libxml2" ,libxml2)                       ;for 'XML_CATALOG_FILES'
       ("docbook-xsl" ,docbook-xsl)))
    (inputs
     `(("dbus-glib" ,dbus-glib)
       ("libgudev" ,libgudev)
       ("libusb" ,libusb)))
    (propagated-inputs
     ;; In Requires of upower-glib.pc.
     `(("glib" ,glib)))
    (home-page "https://upower.freedesktop.org/")
    (synopsis "System daemon for managing power devices")
    (description
     "UPower is an abstraction for enumerating power devices,
listening to device events and querying history and statistics.  Any
application or service on the system can access the org.freedesktop.UPower
service via the system message bus.")
    (license license:gpl2+)))

(define-public libgweather
  (package
    (name "libgweather")
    (version "3.26.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1f64ix7acj0j0qvxwxaazii2bhsjgh5ang1kw14fkg25ndq899zw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       `(,(string-append "--with-zoneinfo-dir="
                         (assoc-ref %build-inputs "tzdata")
                         "/share/zoneinfo"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "data/check-timezones.sh"
               (("/usr/share/zoneinfo/zone.tab")
                (string-append (assoc-ref inputs "tzdata")
                               "/share/zoneinfo/zone.tab")))
             #t))
         (replace 'install
           (lambda _
             (zero?
              (system* "make"
                       ;; Install vala bindings into $out.
                       (string-append "vapidir=" %output
                                      "/share/vala/vapi")
                       "install")))))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-mkenums
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("intltool" ,intltool)))
    (propagated-inputs
     ;; gweather-3.0.pc refers to GTK+, GDK-Pixbuf, GLib/GObject, libxml, and
     ;; libsoup.
     `(("gtk+" ,gtk+)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("libxml2" ,libxml2)
       ("libsoup" ,libsoup)))
    (inputs
     `(("tzdata" ,tzdata)
       ("geocode-glib" ,geocode-glib)))
    (home-page "https://wiki.gnome.org/action/show/Projects/LibGWeather")
    (synopsis "Location, time zone, and weather library for GNOME")
    (description
     "libgweather is a library to access weather information from online
services for numerous locations.")
    (license license:gpl2+)))

(define-public gnome-settings-daemon
  (package
    (name "gnome-settings-daemon")
    (version "3.24.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "14w5jhpq02mbcxvn41qcj3cjfqdr3sgzl96c6glwpdrjphw61i38"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(;; Color management test can't reach the colord system service.
       #:tests? #f))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("xsltproc" ,libxslt)
       ("libxml2" ,libxml2)                       ;for XML_CATALOG_FILES
       ("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)))
    (inputs
     `(("colord" ,colord)
       ("libgudev" ,libgudev)
       ("upower" ,upower)
       ("polkit" ,polkit)
       ("pulseaudio" ,pulseaudio)
       ("libcanberra" ,libcanberra)
       ("libx11" ,libx11)
       ("libxtst" ,libxtst)
       ("lcms" ,lcms)
       ("libnotify" ,libnotify)
       ("geoclue" ,geoclue)
       ("geocode-glib" ,geocode-glib)
       ("libgweather" ,libgweather)
       ("gnome-desktop" ,gnome-desktop)
       ("nss" ,nss)
       ("cups" ,cups)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("libwacom" ,libwacom)
       ("librsvg" ,librsvg)
       ("xf86-input-wacom" ,xf86-input-wacom)
       ("network-manager" ,network-manager)))
    (home-page "https://www.gnome.org")
    (synopsis "GNOME settings daemon")
    (description
     "This package contains the daemon responsible for setting the various
parameters of a GNOME session and the applications that run under it.  It
handles settings such keyboard layout, shortcuts, and accessibility, clipboard
settings, themes, mouse settings, and startup of other daemons.")
    (license license:gpl2+)))

(define-public totem-pl-parser
 (package
   (name "totem-pl-parser")
   (version "3.10.8")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/totem-pl-parser/"
                                (version-major+minor version) "/"
                                "totem-pl-parser-" version ".tar.xz"))
            (sha256
             (base32
              "0ayxg0gfs5h5jhr811ja5hxlhryklzp6jlal2ach9wym2c3hmigz"))))
   (build-system gnu-build-system)
   (arguments
    ;; FIXME: Tests require gvfs.
    `(#:tests? #f))
   (native-inputs
    `(("intltool" ,intltool)
      ("glib" ,glib "bin")
      ("gobject-introspection" ,gobject-introspection)
      ("pkg-config" ,pkg-config)))
   (propagated-inputs
    `(("glib" ,glib)
      ("gmime" ,gmime)
      ("libxml2" ,libxml2)))
   (inputs
    `(("libarchive" ,libarchive)
      ("libgcrypt" ,libgcrypt)
      ("nettle" ,nettle)
      ("libsoup" ,libsoup)))
   (home-page "https://projects.gnome.org/totem")
   (synopsis "Library to parse and save media playlists for GNOME")
   (description "Totem-pl-parser is a GObjects-based library to parse and save
playlists in a variety of formats.")
   (license license:lgpl2.0+)))

(define-public aisleriot
  (package
    (name "aisleriot")
    (version "3.22.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0rl39psr5xi584310pyrgw36ini4wn7yr2m1q5118w3a3v1dkhzh"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:configure-flags
       '("--with-platform=gtk-only"
         "--with-card-theme-formats=svg")))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("gtk+" ,gtk+)
       ("guile" ,guile-2.2)
       ("libcanberra" ,libcanberra)
       ("librsvg" ,librsvg)))
    (home-page "https://wiki.gnome.org/Apps/Aisleriot")
    (synopsis "Solitaire card games")
    (description
     "Aisleriot (also known as Solitaire or sol) is a collection of card games
which are easy to play with the aid of a mouse.")
    (license license:gpl3+)))

(define-public devhelp
  (package
    (name "devhelp")
    (version "3.26.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0f4fmkqzn95zmc2paljma33fkj74qj1m6n23qrc5hfwmgx7p3wkb"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("webkitgtk" ,webkitgtk)))
    (home-page "https://wiki.gnome.org/Apps/Devhelp")
    (synopsis "API documentation browser for GNOME")
    (description
     "Devhelp is an API documentation browser for GTK+ and GNOME.  It works
natively with GTK-Doc (the API reference system developed for GTK+ and used
throughout GNOME for API documentation).")
    (license license:gpl2+)))

(define-public cogl
  (package
    (name "cogl")
    (version "1.22.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "03f0ha3qk7ca0nnkkcr1garrm1n1vvfqhkz9lwjm592fnv6ii9rr"))))
    ;; NOTE: mutter exports a bundled fork of cogl, so when making changes to
    ;; cogl, corresponding changes may be appropriate in mutter as well.
    (build-system gnu-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin")     ; for glib-mkenums
       ("gobject-introspection" ,gobject-introspection)
       ;;("xorg-server" ,xorg-server) ; for the test suite
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("glib" ,glib)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("libxdamage" ,libxdamage)
       ("libxcomposite" ,libxcomposite)
       ("libxrandr" ,libxrandr)))
    (inputs
     `(("mesa" ,mesa)
       ("cairo" ,cairo)
       ("pango" ,pango)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("wayland" ,wayland)))
    (arguments
     `(#:configure-flags (list "--enable-cogl-gst"
                               "--enable-wayland-egl-platform"
                               "--enable-wayland-egl-server"

                               ;; Arrange to pass an absolute file name to
                               ;; dlopen for libGL.so.
                               (string-append "--with-gl-libname="
                                              (assoc-ref %build-inputs "mesa")
                                              "/lib/libGL.so"))
       ;; XXX FIXME: All tests fail, with many warnings printed like this:
       ;;   _FontTransOpen: Unable to Parse address
       ;;   ${prefix}/share/fonts/X11/misc/
       #:tests? #f
       #; #:phases
       #;
       (modify-phases %standard-phases
         (add-before 'check 'start-xorg-server
                     (lambda* (#:key inputs #:allow-other-keys)
                       ;; The test suite requires a running X server.
                       (system (format #f "~a/bin/Xvfb :1 &"
                                       (assoc-ref inputs "xorg-server")))
                       (setenv "DISPLAY" ":1")
                       #t)))))
    (home-page "http://www.cogl3d.org")
    (synopsis "Object oriented GL/GLES Abstraction/Utility Layer")
    (description
     "Cogl is a small library for using 3D graphics hardware to draw pretty
pictures.  The API departs from the flat state machine style of OpenGL and is
designed to make it easy to write orthogonal components that can render
without stepping on each others toes.")
    (license (list license:expat       ; most of the code
                   license:bsd-3       ; cogl/cogl-point-in-poly.c
                   license:sgifreeb2.0 ; cogl-path/tesselator/
                   license:asl2.0))))  ; examples/android/

(define-public clutter
  (package
    (name "clutter")
    (version "1.26.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0mif1qnrpkgxi43h7pimim6w6zwywa16ixcliw0yjm9hk0a368z7"))))
    ;; NOTE: mutter exports a bundled fork of clutter, so when making changes
    ;; to clutter, corresponding changes may be appropriate in mutter as well.
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                            ;9 MiB of gtk-doc HTML pages
    (native-inputs
     `(("glib:bin" ,glib "bin")     ; for glib-genmarshal
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     `(("cogl" ,cogl)
       ("cairo" ,cairo)
       ("atk" ,atk)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("glib" ,glib)
       ("libxcomposite" ,libxcomposite)
       ("libxdamage" ,libxdamage)
       ("libxext" ,libxext)
       ("xinput" ,xinput)))
    (inputs
     `(("libxkbcommon" ,libxkbcommon)
       ("udev" ,eudev)))
    (arguments
     `(#:configure-flags (list "--enable-x11-backend=yes"

                               ;; This produces share/doc/{clutter,cally}.
                               (string-append "--with-html-dir="
                                              (assoc-ref %outputs "doc")
                                              "/share/doc"))
       ;; XXX FIXME: Get test suite working.  It would probably fail in the
       ;; same way the cogl tests fail, since clutter is based on cogl.
       #:tests? #f))
    (home-page "http://www.clutter-project.org")
    (synopsis "OpenGL-based interactive canvas library")
    (description
     "Clutter is an OpenGL-based interactive canvas library, designed for
creating fast, mainly 2D single window applications such as media box UIs,
presentations, kiosk style applications and so on.")
    (license license:lgpl2.0+)))

(define-public clutter-gtk
  (package
    (name "clutter-gtk")
    (version "1.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "01ibniy4ich0fgpam53q252idm7f4fn5xg5qvizcfww90gn9652j"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gobject-introspection" ,gobject-introspection)))
    (propagated-inputs
     ;; clutter-gtk.pc refers to all these.
     `(("clutter" ,clutter)
       ("gtk+" ,gtk+)))
    (home-page "http://www.clutter-project.org")
    (synopsis "OpenGL-based interactive canvas library GTK+ widget")
    (description
     "Clutter is an OpenGL-based interactive canvas library, designed for
creating fast, mainly 2D single window applications such as media box UIs,
presentations, kiosk style applications and so on.")
    (license license:lgpl2.0+)))

(define-public clutter-gst
  (package
    (name "clutter-gst")
    (version "3.0.27")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/clutter-gst/"
                           (version-major+minor version) "/"
                           "clutter-gst-" version ".tar.xz"))
       (sha256
        (base32 "17czmpl92dzi4h3rn5rishk015yi3jwiw29zv8qan94xcmnbssgy"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin")         ; for glib-mkenums
       ("pkg-config" ,pkg-config)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("clutter" ,clutter)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)))
    (home-page "http://www.clutter-project.org")
    (synopsis "Integration library for using GStreamer with Clutter")
    (description
     "Clutter-Gst is an integration library for using GStreamer with Clutter.
It provides a GStreamer sink to upload frames to GL and an actor that
implements the ClutterGstPlayer interface using playbin.  Clutter is an
OpenGL-based interactive canvas library.")
    (license license:lgpl2.0+)))

(define-public libchamplain
  (package
    (name "libchamplain")
    (version "0.12.16")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/libchamplain/0.12/libchamplain-"
                    version ".tar.xz"))
              (sha256
               (base32
                "13chvc2n074i0jw5jlb8i7cysda4yqx58ca6y3mrlrl9g37k2zja"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--enable-vala")))
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (propagated-inputs
     `(("libsoup" ,libsoup)
       ("sqlite" ,sqlite)
       ("clutter" ,clutter)
       ("clutter-gtk" ,clutter-gtk)
       ("glib:bin" ,glib "bin")                   ;glib-mkenums, etc.
       ("cairo" ,cairo)
       ("gtk+3" ,gtk+)
       ("glib" ,glib)))
    (home-page "https://projects.gnome.org/libchamplain/")
    (synopsis "C library providing a ClutterActor to display maps")
    (description
     "libchamplain is a C library providing a ClutterActor to display maps.
It also provides a Gtk+ widget to display maps in Gtk+ applications.  Python
and Perl bindings are also available.  It supports numerous free map sources
such as OpenStreetMap, OpenCycleMap, OpenAerialMap, and Maps for free.")
    (license license:lgpl2.1+)))

(define-public gom
  (package
    (name "gom")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/gom/"
                           (version-major+minor version) "/"
                           "gom-" version ".tar.xz"))
       (sha256
        (base32
         "1zaqqwwkyiswib3v1v8wafpbifpbpak0nn2kp13pizzn9bwz1s5w"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("glib" ,glib)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("sqlite" ,sqlite)))
    ;; XXX TODO: Figure out how to run the test suite.
    (arguments `(#:tests? #f))
    (home-page "https://wiki.gnome.org/Projects/Gom")
    (synopsis "Object mapper from GObjects to SQLite")
    (description
     "Gom provides an object mapper from GObjects to SQLite.  It helps you
write applications that need to store structured data as well as make complex
queries upon that data.")
    (license license:lgpl2.1+)))

(define-public libgnome-games-support
  (package
    (name "libgnome-games-support")
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1vwad7kqy7yd6wqyr71nq0blh7m53r3lz6ya16dmh942kd0w48v1"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a writable HOME.
             (setenv "HOME" (getcwd))
             #t)))))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (propagated-inputs
     ;; Required by libgnome-games-support-1.0.pc
     `(("gtk+" ,gtk+)
       ("libgee" ,libgee)))
    (home-page "https://www.gnome.org/")
    (synopsis "Useful functionality shared among GNOME games")
    (description
     "libgnome-games-support is a small library intended for internal use by
GNOME Games, but it may be used by others.")
    (license license:lgpl3+)))

(define-public gnome-klotski
  (package
    (name "gnome-klotski")
    (version "3.22.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0prc0s28pdflgzyvk1g0yfx982q2grivmz3858nwpqmbkha81r7f"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("gtk+" ,gtk+)
       ("libgnome-games-support" ,libgnome-games-support)
       ("librsvg" ,librsvg)))
    (home-page "https://wiki.gnome.org/Apps/Klotski")
    (synopsis "Sliding block puzzles")
    (description
     "GNOME Klotski is a set of block sliding puzzles.  The objective is to move
the patterned block to the area bordered by green markers.  To do so, you will
need to slide other blocks out of the way.  Complete each puzzle in as few moves
as possible!")
    (license license:gpl2+)))

(define-public grilo
  (package
    (name "grilo")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1qx072m0gl6m3d5g5cbbf13p4h217icmlxjnrn829x5xqwi451sw"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin")         ; for glib-mkenums and glib-genmarshal
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("cyrus-sasl" ,cyrus-sasl)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libxml2" ,libxml2)
       ("liboauth" ,liboauth)
       ("libsoup" ,libsoup)
       ("nettle" ,nettle)
       ("totem-pl-parser" ,totem-pl-parser)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-introspection-install-dir
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* '("src/Makefile.in"
                                       "libs/pls/Makefile.in"
                                       "libs/net/Makefile.in")
                          (("@INTROSPECTION_GIRDIR@")
                           (string-append out "/share/gir-1.0/"))
                          (("@INTROSPECTION_TYPELIBDIR@")
                           (string-append out "/lib/girepository-1.0/")))
                        #t))))))
    (native-search-paths
     (list (search-path-specification
            (variable "GRL_PLUGIN_PATH")
            (files (list (string-append "lib/grilo-"
                                        (version-major+minor version)))))))
    (home-page "https://live.gnome.org/Grilo")
    (synopsis "Framework for discovering and browsing media")
    (description
     "Grilo is a framework focused on making media discovery and browsing easy
for application developers.")
    (license license:lgpl2.1+)))

(define-public grilo-plugins
  (package
    (name "grilo-plugins")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "172vr1y98d2mzlmg5akjn4ibrcj3gh22cwnb3cv9rvvzhj3yhrpy"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin")     ; for glib-mkenums and glib-genmarshal
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("grilo" ,grilo)
       ("nettle" ,nettle) ; XXX: required by libgrlpls-0.3.la
       ("glib" ,glib)
       ("libxml2" ,libxml2)
       ("sqlite" ,sqlite)
       ("gom" ,gom)
       ;; XXX TODO: Add oauth
       ;; XXX TODO: Add goa
       ;; XXX TODO: Add gdata (e.g. needed for youtube plugin)
       ;; XXX TODO: Add lua (needs help finding it)
       ("json-glib" ,json-glib)
       ("avahi" ,avahi)
       ("gmime" ,gmime)
       ("libsoup" ,libsoup)
       ("libarchive" ,libarchive)
       ("totem-pl-parser" ,totem-pl-parser)))
    (arguments
     `(#:make-flags (list (string-append "GRL_PLUGINS_DIR="
                                         %output
                                         "/lib/grilo-"
                                         ,(version-major+minor version)))
       ;; XXX FIXME: Try to get the test suite working.  It appears to require
       ;; a working system dbus.  Inside the build container, all tests fail
       ;; with: "assertion failed: (source)".  Outside of the build container,
       ;; most tests succeed.
       #:tests? #f))
    (home-page "https://live.gnome.org/Grilo")
    (synopsis "Plugins for the Grilo media discovery library")
    (description
     "Grilo is a framework focused on making media discovery and browsing easy
for application developers.")
    (license license:lgpl2.1+)))

(define-public totem
  (package
    (name "totem")
    (version "3.26.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1llyisls3pzf5bwkpxyfyxc2d3gpa09n5pjy7qsjdqrp3ya4k36g"))
       (patches (search-patches "totem-meson-easy-codec.patch"))))
    (build-system meson-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("desktop-file-utils" ,desktop-file-utils)
       ("gobject-introspection" ,gobject-introspection)
       ("glib:bin" ,glib "bin")                   ;for 'glib-mkenums'
       ("gtk:bin" ,gtk+ "bin")                    ;for 'gtk-update-icon-cache'
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("xmllint" ,libxml2)))
    (propagated-inputs
     `(("dconf" ,dconf)))
    (inputs
     `(("gtk+" ,gtk+)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("atk" ,atk)
       ("cairo" ,cairo)
       ("dbus-glib" ,dbus-glib)
       ("clutter" ,clutter)
       ("clutter-gtk" ,clutter-gtk)
       ("clutter-gst" ,clutter-gst)
       ("xorgproto" ,xorgproto)
       ("libxxf86vm" ,libxxf86vm)
       ("libxtst" ,libxtst)
       ("libxrandr" ,libxrandr)
       ("libxml2" ,libxml2)
       ("libsoup" ,libsoup)
       ("libpeas" ,libpeas)
       ("librsvg" ,librsvg)
       ("lirc" ,lirc)
       ("gnome-desktop" ,gnome-desktop)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-good" ,gst-plugins-good)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("adwaita-icon-theme" ,adwaita-icon-theme)
       ;; XXX We use python-2 because libxml2 because itstool (which needs
       ;; libxml) currently uses python-2.
       ("python" ,python-2)
       ("python-pygobject" ,python2-pygobject)
       ;; XXX TODO pylint needed for python support
       ("totem-pl-parser" ,totem-pl-parser)
       ("grilo" ,grilo)
       ("grilo-plugins" ,grilo-plugins)
       ("nettle" ,nettle)
       ("vala" ,vala)))
    (arguments
     `(#:glib-or-gtk? #t

       ;; Disable parallel builds until
       ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=28813 is
       ;; fixed.  Try enabling it when updating this package in case
       ;; upstream has fixed it.
       #:parallel-build? #f

       ;; Disable automatic GStreamer plugin installation via PackageKit and
       ;; all that.
       #:configure-flags '("-D" "enable-easy-codec-installation=no"

                           ;; Do not build .a files for the plugins, it's
                           ;; completely useless.  This saves 2 MiB.
                           "--default-library" "shared")

       #:phases
       (modify-phases %standard-phases
         (add-before
          'install 'disable-cache-generation
          (lambda _
            (setenv "DESTDIR" "/")
            #t))
         (add-after
          'install 'wrap-totem
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out             (assoc-ref outputs "out"))
                  (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH"))
                  (grl-plugin-path (getenv "GRL_PLUGIN_PATH")))
              (wrap-program (string-append out "/bin/totem")
                `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))
                `("GRL_PLUGIN_PATH"        ":" prefix (,grl-plugin-path)))
              (wrap-program (string-append out "/bin/totem-video-thumbnailer")
                `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))))
            #t)))))
    (home-page "https://wiki.gnome.org/Apps/Videos")
    (synopsis "Simple media player for GNOME based on GStreamer")
    (description "Totem is a simple yet featureful media player for GNOME
which can read a large number of file formats.")
    ;; GPL2+ with an exception clause for non-GPL compatible GStreamer plugins
    ;; to be used and distributed together with GStreamer and Totem.  See
    ;; file://COPYING in the source distribution for details.
    (license license:gpl2+)))

(define-public rhythmbox
 (package
   (name "rhythmbox")
   (version "3.4.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version) "/"
                                name "-" version ".tar.xz"))
            (patches
             (list
              ;; fmradio: Fix build with GStreamer master
              (origin
                (method url-fetch)
                (uri (string-append
                      "https://gitlab.gnome.org/GNOME/rhythmbox/commit/"
                      "b182c6b9e1d09e601bac0b703cc5f8b159ebbc3a.patch"))
                (sha256
                 (base32
                  "06n87xgf927djmv1vshal84nqx7g8nwgljza3g2vydhy7g2n1csq")))))
            (sha256
             (base32
              "0hzcns8gf5yb0rm4ss8jd8qzarcaplp5cylk6plwilsqfvxj4xn2"))))
   (build-system glib-or-gtk-build-system)
   (arguments
    `(#:configure-flags
      (list "--enable-lirc"
            "--enable-python"
            "--enable-vala"
            "--with-brasero"
            "--with-gudev"
            "--with-libsecret")
      #:phases
      (modify-phases %standard-phases
        (add-after
         'install 'wrap-rhythmbox
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let ((out               (assoc-ref outputs "out"))
                 (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                 (gst-plugin-path   (getenv "GST_PLUGIN_SYSTEM_PATH"))
                 (grl-plugin-path   (getenv "GRL_PLUGIN_PATH"))
                 (python-path       (getenv "PYTHONPATH")))
             (wrap-program (string-append out "/bin/rhythmbox")
               `("GI_TYPELIB_PATH"        ":" prefix (,gi-typelib-path))
               `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))
               `("GRL_PLUGIN_PATH"        ":" prefix (,grl-plugin-path))
               `("PYTHONPATH"             ":" prefix (,python-path))))
           #t)))))
   (propagated-inputs
    `(("dconf" ,dconf)))
   (native-inputs
    `(("itstool" ,itstool)
      ("intltool" ,intltool)
      ("glib" ,glib "bin")
      ("gobject-introspection" ,gobject-introspection)
      ("desktop-file-utils" ,desktop-file-utils)
      ("pkg-config" ,pkg-config)
      ("xmllint" ,libxml2)))
   (inputs
    `(("json-glib" ,json-glib)
      ("tdb" ,tdb)
      ("gnome-desktop" ,gnome-desktop)
      ("python" ,python)
      ("python-pygobject" ,python2-pygobject)
      ("vala" ,vala)
      ("gmime" ,gmime)
      ("nettle" ,nettle)
      ("adwaita-icon-theme" ,adwaita-icon-theme)
      ("grilo" ,grilo)
      ("grilo-plugins" ,grilo-plugins)
      ("gstreamer" ,gstreamer)
      ("gst-plugins-base" ,gst-plugins-base)
      ("gst-plugins-good" ,gst-plugins-good)
      ("totem-pl-parser" ,totem-pl-parser)
      ("libgudev" ,libgudev)
      ;;("libmtp" ,libmtp) FIXME: Not detected
      ("libsecret" ,libsecret)
      ("libsoup" ,libsoup)
      ("libnotify" ,libnotify)
      ("libpeas" ,libpeas)
      ("lirc" ,lirc)
      ;; TODO: clutter* only used by visualizer plugin, which also requires mx
      ;;("clutter" ,clutter)
      ;;("clutter-gtk" ,clutter-gtk)
      ;;("clutter-gst" ,clutter-gst)
      ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
      ("atk" ,atk)
      ("pango" ,pango)
      ("gtk+" ,gtk+)
      ;; TODO:
      ;;  * libgpod
      ;;  * mx
      ("brasero" ,brasero)))
   (home-page "https://wiki.gnome.org/Apps/Rhythmbox")
   (synopsis "Music player for GNOME")
   (description "Rhythmbox is a music playing application for GNOME.  It
supports playlists, song ratings, and any codecs installed through gstreamer.")
   (license license:gpl2+)))

(define-public eog
 (package
   (name "eog")
   (version "3.26.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "1b87i31mxzayd3knn9zg00y816d093qrbyx556w8a03xz96ksgmm"))))
   (build-system glib-or-gtk-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after
         'install 'wrap-eog
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((out               (assoc-ref outputs "out"))
                 (gi-typelib-path   (getenv "GI_TYPELIB_PATH")))
             (wrap-program (string-append out "/bin/eog")
               `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
           #t)))))
   (propagated-inputs
    `(("dconf" ,dconf)))
   (native-inputs
    `(("intltool" ,intltool)
      ("itstool" ,itstool)
      ("glib" ,glib "bin")
      ("gobject-introspection" ,gobject-introspection)
      ("pkg-config" ,pkg-config)
      ("xmllint" ,libxml2)))
   (inputs
    `(("gnome-desktop" ,gnome-desktop)
      ("shared-mime-info" ,shared-mime-info)
      ("adwaita-icon-theme" ,adwaita-icon-theme)
      ("exempi" ,exempi)
      ("lcms" ,lcms)
      ("libexif" ,libexif)
      ("libpeas" ,libpeas)
      ("libjpeg" ,libjpeg)
      ("librsvg" ,librsvg)
      ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
      ("gtk+" ,gtk+)))
   (home-page "https://wiki.gnome.org/Apps/EyeOfGnome")
   (synopsis "GNOME image viewer")
   (description "Eye of GNOME is the GNOME image viewer.  It
supports image conversion, rotation, and slideshows.")
   (license license:gpl2+)))

(define-public eog-plugins
  ;; Note: EOG looks for its plugins (via libpeas) in ~/.local as well as
  ;; $DATA/lib/eog/plugins, where DATA is one of the entries in
  ;; $XDG_DATA_DIRS.  Thus, for EOG to find these, you have to have
  ;; 'XDG_DATA_DIRS' appropriately set.
  (package
    (name "eog-plugins")
    (version "3.26.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/eog-plugins/"
                                  (version-major+minor version) "/"
                                  "eog-plugins-" version ".tar.xz"))
              (sha256
               (base32
                "06fnjs2p18ad5vk07z685cx26sc7d3azywss00w9xvz794b2i1g3"))))
    (build-system gnu-build-system)
    (home-page "https://wiki.gnome.org/Apps/EyeOfGnome/Plugins")
    (synopsis "Extensions for the Eye of GNOME image viewer")
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gnu-gettext)))
    (inputs
     `(("eog" ,eog)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libpeas" ,libpeas)
       ("libexif" ,libexif)
       ("libchamplain" ,libchamplain)))
    (description
     "This package provides plugins for the Eye of GNOME (EOG) image viewer,
notably:

@itemize
@item @dfn{EXIF Display}, which displays camera (EXIF) information;
@item @dfn{Map}, which displays a map of where the picture was taken on the
side panel;
@item @dfn{Slideshow Shuffle}, to shuffle images in slideshow mode.
@end itemize\n")

    ;; XXX: eog-postasa-plugin-resources.c (which we don't build) contains a
    ;; long suspicious byte stream that goes to a
    ;; ".gresource.eog_postasa_plugin" ELF section.
    (license license:gpl2+)))

(define-public libgudev
  (package
    (name "libgudev")
    (version "230")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "063w6j35n0i0ssmv58kivc1mw4070z6fzb83hi4xfrhcxnn7zrx2"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-genmarshal, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("glib" ,glib))) ; required by gudev-1.0.pc
    (inputs
     `(("udev" ,eudev)))
    (home-page "https://wiki.gnome.org/Projects/libgudev")
    (synopsis "GObject bindings for libudev")
    (description
     "This library provides GObject bindings for libudev.  It was originally
part of udev-extras, then udev, then systemd.  It's now a project on its own.")
    (license license:lgpl2.1+)))

(define-public gvfs
  (package
    (name "gvfs")
    (version "1.32.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1pkahczniar1yyas7awcqpkb4ca8l7qa4msn6mr29m89mgnwkdnh"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f)) ; XXX: requiring `pidof'
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-genmarshal, etc.
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("avahi" ,avahi)
       ("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)
       ("dbus" ,dbus)
       ("fuse" ,fuse)
       ("gcr" ,gcr)
       ("glib" ,glib)
       ("libarchive" ,libarchive)
       ("libbluray" ,libbluray)
       ("libcap" ,libcap)
       ("libcdio-paranoia" ,libcdio-paranoia)
       ("libgcrypt" ,libgcrypt)
       ("libgphoto2" ,libgphoto2)
       ("libgudev" ,libgudev)
       ("libmtp" ,libmtp)
       ("libsecret" ,libsecret)
       ("libsmbclient" ,samba)
       ("libsoup" ,libsoup)
       ("libxml2" ,libxml2)
       ("nettle" ,nettle) ; XXX: required by libarchive.pc
       ("polkit" ,polkit)
       ("udisks" ,udisks)))
    (home-page "https://wiki.gnome.org/gvfs/")
    (synopsis "Userspace virtual file system for GIO")
    (description
     "GVFS is a userspace virtual file system designed to work with the I/O
abstraction of GIO.  It contains a GIO module that seamlessly adds GVFS support
to all applications using the GIO API.  It also supports exposing the GVFS
mounts to non-GIO applications using FUSE.

GVFS comes with a set of backends, including trash support, SFTP, SMB, HTTP,
DAV, and others.")
    (license license:lgpl2.0+)))

(define-public gusb
  (package
    (name "gusb")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/hughsie/libgusb/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1wa9787ww7s1kl9jml6kiyrjgimlgagq4jmgdj7xcpsx83w10qxk"))))
    (build-system meson-build-system)
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("gtk-doc" ,gtk-doc)))
    (propagated-inputs
     ;; Both of these are required by gusb.pc.
     `(("glib" ,glib)
       ("libusb" ,libusb)))
    (arguments
     `(#:tests? #f)) ;libusb fails to initialize.  Wonder what that is.
    (home-page "https://github.com/hughsie/libgusb")
    (synopsis "GLib binding for libusb1")
    (description
     "GUsb is a GObject wrapper for libusb1 that makes it easy to do
asynchronous control, bulk and interrupt transfers with proper cancellation
and integration into a mainloop.  This makes it easy to integrate low level
USB transfers with your high-level application or system daemon.")
    (license license:lgpl2.1+)))

(define-public simple-scan
  (package
    (name "simple-scan")
    (version "3.24.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://launchpad.net/simple-scan/"
                                  (version-major+minor version) "/"
                                  version "/+download/simple-scan-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1czg21cdbd2fgqylxfnzfhhzy69gycf816d5bbaq6hb62hmq7bjy"))))
    (build-system glib-or-gtk-build-system)
    (inputs
     `(("gtk" ,gtk+)
       ("zlib" ,zlib)
       ("cairo" ,cairo)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gusb" ,gusb)
       ("libsane" ,sane-backends)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("itstool" ,itstool)
       ("colord" ,colord)
       ("glib" ,glib "bin")                       ; glib-compile-schemas, etc.
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xmllint" ,libxml2)))
    (arguments
     '(#:configure-flags '("--disable-packagekit")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'clean
                    (lambda _
                      ;; Remove a left-over reference to PackageKit.

                      ;; https://bugs.launchpad.net/simple-scan/+bug/1462769

                      ;; There are some generated C files erroneously
                      ;; included in the source distribution, and this
                      ;; one breaks the build by referring to a
                      ;; non-existent header (packagekit.h)
                      (delete-file "src/ui.c"))))))
    (home-page "https://launchpad.net/simple-scan")
    (synopsis "Document and image scanner")
    (description "Simple Scan is an easy-to-use application, designed to let
users connect their scanner and quickly have the image/document in an
appropriate format.  Simple Scan is basically a frontend for SANE - which is
the same backend as XSANE uses. This means that all existing scanners will
work and the interface is well tested.")
    (license license:gpl3+)))

(define-public eolie
  (package
    (name "eolie")
    (version "0.9.52")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.gnome.org/World/eolie/"
                                  "uploads/d95bf72958276c80dfaca8cce0e4e92c/"
                                  "eolie-" version ".tar.xz"))
              (sha256
               (base32
                "1s3b0rkm8sxmhzzi624snzqvz61i1rja5wxyzw6jg2kcdjcylwln"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'wrap 'wrap-more
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    ;; These libraries must be on LD_LIBRARY_PATH.
                    (libs '("gtkspell3" "webkitgtk" "libsoup" "libsecret"
                            "atk" "gtk+" "gsettings-desktop-schemas"
                            "gcc:lib" ; needed b/c webkitgtk is built with gcc-7
                            "gobject-introspection"))
                    (path (string-join
                           (map (lambda (lib)
                                  (string-append (assoc-ref inputs lib) "/lib"))
                                libs)
                           ":")))
               (wrap-program (string-append out "/bin/eolie")
                 `("LD_LIBRARY_PATH" ":" prefix (,path))
                 `("PYTHONPATH" ":" prefix (,(getenv "PYTHONPATH")))
                 `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))
             #t)))))
    (native-inputs
     `(("gcc:lib" ,gcc-7 "lib") ; needed because webkitgtk is built with gcc-7
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("glib:bin" ,glib "bin")
       ("gtk+" ,gtk+ "bin")))
    (inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("glib-networking" ,glib-networking)
       ("cairo" ,cairo)
       ("gtk+" ,gtk+)
       ("atk" ,atk)    ; propagated by gtk+, but we need it in LD_LIBRARY_PATH
       ("python" ,python-wrapper)
       ("python-dateutil" ,python-dateutil)
       ("python-pyfxa" ,python-pyfxa)
       ("python-pygobject" ,python-pygobject)
       ("python-pycairo" ,python-pycairo)
       ("python-pycrypto" ,python-pycrypto)
       ("libsecret" ,libsecret)
       ("gtkspell3" ,gtkspell3)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("webkitgtk" ,webkitgtk-2.22)))
    (home-page "https://wiki.gnome.org/Apps/Eolie")
    (synopsis "Web browser for GNOME")
    (description
     "Eolie is a new web browser for GNOME.  It features Firefox sync support,
a secret password store, an adblocker, and a modern UI.")
    (license license:gpl3+)))

(define-public epiphany
  (package
    (name "epiphany")
    (version "3.28.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1xz6xl6b0iihvczyr0cs1z5ifvpai6anb4m0ng1caiph06klc1b9"))))

    (build-system meson-build-system)
    (arguments
     ;; FIXME: tests run under Xvfb, but fail with:
     ;;   /src/bookmarks/ephy-bookmarks/create:
     ;;   ** (test-ephy-bookmarks:19591): WARNING **: Unable to start Zeroconf
     ;;      subsystem
     ;;   FAIL
     '(#:tests? #f
       #:glib-or-gtk? #t
       #:configure-flags
       ;; Otherwise, the RUNPATH will lack the final 'epiphany' path component.
       (list (string-append "-Dc_link_args=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib/epiphany"))))
    (propagated-inputs
     `(("dconf" ,dconf)))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils) ; for update-desktop-database
       ("gcc" ,gcc-7)  ; needed because webkitgtk-2.22 is compiled with gcc-7
       ("glib:bin" ,glib "bin") ; for glib-mkenums
       ("gtk+:bin" ,gtk+ "bin") ; for gtk-update-icon-cache
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("avahi" ,avahi)
       ("gcr" ,gcr)
       ("gdk-pixbuf+svg" ,gdk-pixbuf+svg) ; for loading SVG files
       ("glib-networking" ,glib-networking)
       ("gnome-desktop" ,gnome-desktop)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("json-glib" ,json-glib)
       ("iso-codes" ,iso-codes)
       ("libnotify" ,libnotify)
       ("libsecret" ,libsecret)
       ("libxslt" ,libxslt)
       ("nettle" ,nettle) ; for hogweed
       ("sqlite" ,sqlite)
       ("webkitgtk" ,webkitgtk-2.22)))
    (home-page "https://wiki.gnome.org/Apps/Web")
    (synopsis "GNOME web browser")
    (description
     "Epiphany is a GNOME web browser targeted at non-technical users.  Its
principles are simplicity and standards compliance.")
    (license license:gpl2+)))

(define-public d-feet
  (package
    (name "d-feet")
    (version "0.3.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1hmrijm4d9vwzx2r8qzzsy8ccpj79l1y6cc569n9jjzqcq699p53"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:out-of-source? #f ; tests need to run in the source directory.
       #:phases
       (modify-phases %standard-phases
         (add-before
          'check 'pre-check
          (lambda _
            ;; The test suite requires a running X server.
            (system "Xvfb :1 &")
            (setenv "DISPLAY" ":1")
            ;; Don't fail on missing '/etc/machine-id'.
            (setenv "DBUS_FATAL_WARNINGS" "0")
            ;; tests.py and window.py don't meet E402:
            ;;   E402 module level import not at top of file
            (substitute* "src/tests/Makefile"
              (("--ignore=E123") "--ignore=E123,E402"))
            #t))
         (add-after
          'install 'wrap-program
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((prog (string-append (assoc-ref outputs "out")
                                       "/bin/d-feet")))
              (wrap-program prog
                `("PYTHONPATH" = (,(getenv "PYTHONPATH")))
                `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))
              #t))))))
    (native-inputs
     `(("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("python-pep8" ,python-pep8)
       ("xmllint" ,libxml2)
       ("xorg-server" ,xorg-server)))
    (inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("gtk+" ,gtk+)
       ("python" ,python-wrapper)
       ("python-pygobject" ,python-pygobject)))
    (home-page "https://wiki.gnome.org/Apps/DFeet")
    (synopsis "D-Bus debugger")
    (description
     "D-Feet is a D-Bus debugger, which can be used to inspect D-Bus interfaces
of running programs and invoke methods on those interfaces.")
    (license license:gpl2+)))

(define-public yelp-xsl
  (package
    (name "yelp-xsl")
    (version "3.20.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "183vz4aw8fcmw8k8q7r4zrm1p76gwg2cca1fsdrkaiyabng88qfw"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("itstool" ,itstool)
       ("xmllint" ,libxml2)))
    (home-page "https://wiki.gnome.org/Apps/Yelp")
    (synopsis "XSL stylesheets for Yelp")
    (description
     "Yelp-xsl contains XSL stylesheets that are used by the yelp help browser
to format Docbook and Mallard documents.")
    (license license:gpl2+)))

(define-public yelp
  (package
    (name "yelp")
    (version "3.22.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "13kpi0qmnfx2xwizyhmf5i5xaw9ckcn1k7v0289p7als4dybf5l6"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-genmarshal, etc.
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("dconf" ,dconf)))
    (inputs
     `(("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("libxslt" ,libxslt)
       ("sqlite" ,sqlite)
       ("webkitgtk" ,webkitgtk)
       ("yelp-xsl" ,yelp-xsl)))
    (home-page "https://wiki.gnome.org/Apps/Yelp")
    (synopsis "GNOME help browser")
    (description
     "Yelp is the help viewer in Gnome.  It natively views Mallard, DocBook,
man, info, and HTML documents.  It can locate documents according to the
freedesktop.org help system specification.")
    (license license:gpl2+)))

(define-public yelp-tools
  (package
    (name "yelp-tools")
    (version "3.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0ck9f78c1xka8a823bd7w1k0gdn4k19zvaj7viy2d5r3h1gxdhf6"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     ;; Needed by `yelp-build', `yelp-check' or 'yelp.m4'.
     `(("itstool" ,itstool)
       ("xmllint" ,libxml2)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("yelp-xsl" ,yelp-xsl)))
    (home-page "https://wiki.gnome.org/Apps/Yelp/Tools")
    (synopsis "Yelp documentation tools")
    (description
     "Yelp-tools is a collection of scripts and build utilities to help create,
manage, and publish documentation for Yelp and the web.  Most of the heavy
lifting is done by packages like yelp-xsl and itstool.  This package just
wraps things up in a developer-friendly way.")
    (license license:gpl2+)))

(define-public libgee
  (package
    (name "libgee")
    (version "0.20.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0c26x8gi3ivmhlbqcmiag4jwrkvcy28ld24j55nqr3jikb904a5v"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-introspection-install-dir
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (substitute* "gee/Makefile.in"
                (("@INTROSPECTION_GIRDIR@")
                 (string-append out "/share/gir-1.0/"))
                (("@INTROSPECTION_TYPELIBDIR@")
                 (string-append out "/lib/girepository-1.0/")))))))))
    (native-inputs
     `(("glib" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("gobject-introspection" ,gobject-introspection)))
    (home-page "https://wiki.gnome.org/Projects/Libgee")
    (synopsis "GObject collection library")
    (description
     "Libgee is a utility library providing GObject-based interfaces and
classes for commonly used data structures.")
    (license license:lgpl2.1+)))

(define-public gexiv2
  (package
    (name "gexiv2")
    (version "0.10.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1qbcwq89g4r67k1dj4laqj441pj4195c8hzhxn8vc6mmg8adg6kx"))))
    (build-system meson-build-system)
    (native-inputs
     `(("glib" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     ;; Listed in "Requires" section of gexiv2.pc
     `(("exiv2" ,exiv2)))
    (inputs
     `(("glib" ,glib)
       ("gobject-introspection" ,gobject-introspection)))
    (home-page "https://wiki.gnome.org/Projects/gexiv2")
    (synopsis "GObject wrapper around the Exiv2 photo metadata library")
    (description
     "Gexiv2 is a GObject wrapper around the Exiv2 photo metadata library.  It
allows for GNOME applications to easily inspect and update EXIF, IPTC, and XMP
metadata in photo and video files of various formats.")
    (license license:gpl2+)))

(define-public shotwell
  (package
    (name "shotwell")
    (version "0.28.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "03k7n2kmzqn11kf3733w7m6xjh2b5q9xr84za2hli11fjymzaxm9"))))
    (build-system glib-or-gtk-build-system)
    (propagated-inputs
     `(("dconf" ,dconf)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("itstool" ,itstool)
       ("gettext" ,gettext-minimal)
       ("gtk+" ,gtk+ "bin") ; gtk-update-icon-cache
       ("itstool" ,itstool)
       ("vala" ,vala)))
    (inputs
     `(("glib:bin" ,glib "bin")
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("libgdata" ,libgdata)
       ("libgee" ,libgee)
       ("gexiv2" ,gexiv2)
       ("libraw" ,libraw)
       ("json-glib" ,json-glib)
       ("webkitgtk" ,webkitgtk)
       ("sqlite" ,sqlite)
       ("libsoup" ,libsoup)
       ("libxml2" ,libxml2)
       ("libgudev" ,libgudev)
       ("libgphoto2" ,libgphoto2)
       ("gcr" ,gcr)))
    (home-page "https://wiki.gnome.org/Apps/Shotwell")
    (synopsis "Photo manager for GNOME 3")
    (description
     "Shotwell is a digital photo manager designed for the GNOME desktop
environment.  It allows you to import photos from disk or camera, organize
them by keywords and events, view them in full-window or fullscreen mode, and
share them with others via social networking and more.")
    (license license:lgpl2.1+)))

(define-public file-roller
  (package
    (name "file-roller")
    (version "3.26.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "19d8pc5z2xzhnicgaysxmwx3ghwzl4cw8kygd6nsw69g3j77nrry"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'skip-gtk-update-icon-cache
           (lambda _
             ;; Don't create 'icon-theme.cache'
             (substitute* (find-files "data" "^Makefile$")
               (("gtk-update-icon-cache") (which "true")))
             #t)))))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    ;; TODO: Add libnautilus.
    (inputs
     `(("gtk+" ,gtk+)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("json-glib" ,json-glib)
       ("libarchive" ,libarchive)
       ("libnotify" ,libnotify)
       ("nettle" ,nettle)
       ("itstool" ,itstool)
       ("libxml2" ,libxml2)))
    (synopsis "Graphical archive manager for GNOME")
    (description "File Roller is an archive manager for the GNOME desktop
environment that allows users to view, unpack, and create compressed archives
such as gzip tarballs.")
    (home-page "http://fileroller.sourceforge.net/")
    (license license:gpl2+)))

(define-public gnome-session
  (package
    (name "gnome-session")
    (version "3.24.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1vkfjsgks9czajivcg3y1krzlnilv2cnzzbdc7wrasrriqilji1v"))))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Use elogind instead of systemd.
             (substitute* "configure"
               (("libsystemd-login >= 183 libsystemd-daemon libsystemd-journal")
                "libelogind")
               (("systemd") "elogind"))
             (substitute* "gnome-session/gsm-systemd.c"
               (("#include <systemd/sd-login.h>")
                "#include <elogind/sd-login.h>"))
             ;; Remove uses of the systemd journal.
             (substitute* "gnome-session/main.c"
               (("#ifdef HAVE_SYSTEMD") "#if 0"))
             (substitute* "gnome-session/gsm-manager.c"
               (("#ifdef HAVE_SYSTEMD") "#if 0"))
             (substitute* "gnome-session/gsm-autostart-app.c"
               (("#ifdef HAVE_SYSTEMD") "#if 0"))
             #t))
         (add-after 'install 'wrap-gnome-session
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make sure 'gnome-session' finds the 'gsettings' program.
             (let ((glib (assoc-ref inputs "glib:bin"))
                   (out  (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/gnome-session")
                 `("PATH" ":" prefix (,(string-append glib "/bin"))))
               #t)))
         (add-after 'install 'add-absolute-paths-to-desktop-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (substitute* (map (lambda (x)
                                   (string-append out "/share/xsessions/" x))
                                 '("gnome.desktop" "gnome-xorg.desktop"))
                 (("gnome-session") (string-append out "/bin/gnome-session")))
               #t))))

       #:configure-flags
       '("--enable-elogind")))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("elogind" ,elogind)
       ("gnome-desktop" ,gnome-desktop)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("libsm" ,libsm)
       ("libxcomposite" ,libxcomposite)
       ("libxtst" ,libxtst)
       ("mesa" ,mesa)
       ("upower" ,upower)
       ("xtrans" ,xtrans)))
    (synopsis "Session manager for GNOME")
    (description
     "This package contains the GNOME session manager, as well as a
configuration program to choose applications starting on login.")
    (home-page "https://wiki.gnome.org/Projects/SessionManagement")
    (license license:gpl2+)))

(define-public gjs
  (package
    (name "gjs")
    (version "1.48.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "04nkig077r7xq55dxg9v46w8i7p8zkkdyja92yv81grq9fx6apz8"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before
          'check 'pre-check
          (lambda _
            ;; The test suite requires a running X server.
            (system "Xvfb :1 &")
            (setenv "DISPLAY" ":1")

            ;; For the missing /etc/machine-id.
            (setenv "DBUS_FATAL_WARNINGS" "0")

            ;; Our mozjs-38 package does not compile the required Intl API
            ;; support for these failing tests.
            (substitute* "installed-tests/js/testLocale.js"
              ((".*toBeDefined.*") "")
              ((".*expect\\(datestr\\).*") ""))
            #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin")       ; for glib-compile-resources
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)
       ;; For testing
       ("dbus-launch" ,dbus)
       ("uuidgen" ,util-linux)
       ("xvfb" ,xorg-server)))
    (propagated-inputs
     ;; These are all in the Requires.private field of gjs-1.0.pc.
     `(("cairo" ,cairo)
       ("gobject-introspection" ,gobject-introspection)
       ("mozjs" ,mozjs-38)))
    (inputs
     `(("gtk+" ,gtk+)
       ("readline" ,readline)))
    (synopsis "Javascript bindings for GNOME")
    (home-page "https://live.gnome.org/Gjs")
    (description
     "Gjs is a javascript binding for GNOME.  It's mainly based on spidermonkey
javascript engine and the GObject introspection framework.")
    (license license:gpl2+)))

(define-public gedit
  (package
    (name "gedit")
    (version "3.28.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0791r07d3ixmmfk68lvhp3d5i4vnlrnx10csxwgpfqyfb04vwx7i"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after
          'install 'wrap-gedit
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out               (assoc-ref outputs "out"))
                  (gtksourceview     (assoc-ref inputs "gtksourceview"))
                  (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                  (python-path       (getenv "PYTHONPATH")))
              (wrap-program (string-append out "/bin/gedit")
                ;; For plugins.
                `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                `("PYTHONPATH" ":" prefix (,python-path))
                ;; For language-specs.
                `("XDG_DATA_DIRS" ":" prefix (,(string-append gtksourceview
                                                              "/share")))))
            #t)))))
    (propagated-inputs
     `(("dconf" ,dconf)))
    (native-inputs
     `(("intltool" ,intltool)
       ("itstool" ,itstool)
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("gspell" ,gspell)
       ("gtk+" ,gtk+)
       ("gtksourceview" ,gtksourceview)
       ("libpeas" ,libpeas)
       ("libxml2" ,libxml2)
       ("iso-codes" ,iso-codes)
       ("python-pygobject" ,python-pygobject)
       ("python" ,python)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("libx11" ,libx11)
       ("vala" ,vala)
       ("adwaita-icon-theme" ,adwaita-icon-theme)
       ("libsoup" ,libsoup)
       ("gnome-desktop" ,gnome-desktop)))
    (home-page "https://wiki.gnome.org/Apps/Gedit")
    (synopsis "GNOME text editor")
    (description "While aiming at simplicity and ease of use, gedit is a
powerful general purpose text editor.")
    (license license:gpl2+)))

(define-public zenity
  (package
    (name "zenity")
    (version "3.24.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1xzpm63cib2xzr99phplhbcjzy7lahggk3gp60dvrrclxhka1w3g"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libnotify" ,libnotify)
       ("webkitgtk" ,webkitgtk)))
    (synopsis "Display graphical dialog boxes from shell scripts")
    (home-page "https://www.gnome.org")
    (description
     "Zenity is a rewrite of gdialog, the GNOME port of dialog which allows you
to display dialog boxes from the commandline and shell scripts.")
    (license license:lgpl2.0+)))

(define-public mutter
  (package
    (name "mutter")
    (version "3.24.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1slspy5krbqfvnma72lvdnxjf8ag2cvxssa8bvi7y3xxy7xv603k"))))
    ;; NOTE: Since version 3.21.x, mutter now bundles and exports forked
    ;; versions of cogl and clutter.  As a result, many of the inputs,
    ;; propagated-inputs, and configure flags used in cogl and clutter are
    ;; needed here as well.
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       ;; XXX: build fails with [-Werror]:
       ;;    backends/meta-cursor-renderer.c:112:5: error:
       ;;      implicit declaration of function ?roundf?
       (list "--enable-compile-warnings=minimum"

             "--enable-native-backend"

             ;; The following flags are needed for the bundled clutter
             "--enable-x11-backend=yes"

             (string-append "--with-xwayland-path="
                            (assoc-ref %build-inputs "xorg-server-xwayland")
                            "/bin/Xwayland")

             ;; the remaining flags are needed for the bundled cogl
             "--enable-cogl-gst"
             (string-append "--with-gl-libname="
                            (assoc-ref %build-inputs "mesa")
                            "/lib/libGL.so"))
       #:phases
       (modify-phases %standard-phases
         ;; Replace references to systemd libraries to elogind references.
         (add-before 'configure 'use-elogind
           (lambda _
             (substitute* (list "configure"
                                "src/backends/native/meta-launcher.c"
                                "src/core/main.c")
               (("systemd") "elogind"))
             #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ;; For git build
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (propagated-inputs
     `(;; libmutter.pc refers to these:
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ;; mutter-clutter-1.0.pc and mutter-cogl-1.0.pc refer to these:
       ("atk" ,atk)
       ("cairo" ,cairo)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("libinput" ,libinput)
       ("libx11" ,libx11)
       ("libxcomposite" ,libxcomposite)
       ("libxdamage" ,libxdamage)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("libxkbcommon" ,libxkbcommon)
       ("libxrandr" ,libxrandr)
       ("mesa" ,mesa)
       ("pango" ,pango)
       ("udev" ,eudev)
       ("xinput" ,xinput)))
    (inputs
     `(("elogind" ,elogind)
       ("gnome-desktop" ,gnome-desktop)
       ("libcanberra-gtk" ,libcanberra)
       ("libgudev" ,libgudev)
       ("libice" ,libice)
       ("libsm" ,libsm)
       ("libxkbfile" ,libxkbfile)
       ("libxrandr" ,libxrandr)
       ("libxtst" ,libxtst)
       ("startup-notification" ,startup-notification)
       ("upower-glib" ,upower)
       ("xkeyboard-config" ,xkeyboard-config)
       ("xorg-server-xwayland" ,xorg-server-xwayland)
       ("zenity" ,zenity)))
    (synopsis "Window and compositing manager")
    (home-page "https://www.gnome.org")
    (description
     "Mutter is a window and compositing manager that displays and manages your
desktop via OpenGL.  Mutter combines a sophisticated display engine using the
Clutter toolkit with solid window-management logic inherited from the Metacity
window manager.")
    (license license:gpl2+)))

(define-public gnome-online-accounts
  (package
    (name "gnome-online-accounts")
    (version "3.26.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1l8p1ghknmkmjpnpl7jr53j66qbzpikickzbmrz0aczyhq6pdy29"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     `(("glib" ,glib)           ; required by goa-1.0.pc
       ("gtk+" ,gtk+)))         ; required by goa-backend-1.0.pc
    (inputs
     `(("docbook-xsl" ,docbook-xsl)
       ("json-glib" ,json-glib)
       ("libsecret" ,libsecret)
       ("rest" ,rest)
       ("webkitgtk" ,webkitgtk)))
    (synopsis "Single sign-on framework for GNOME")
    (home-page "https://wiki.gnome.org/Projects/GnomeOnlineAccounts")
    (description
     "GNOME Online Accounts provides interfaces so that applications and
libraries in GNOME can access the user's online accounts.  It has providers for
Google, ownCloud, Facebook, Flickr, Windows Live, Pocket, Foursquare, Microsoft
Exchange, Last.fm, IMAP/SMTP, Jabber, SIP and Kerberos.")
    (license license:lgpl2.0+)))

(define-public evolution-data-server
  (package
    (name "evolution-data-server")
    (version "3.28.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "12b9lfgwd57rzn9394xrbvl9ym5aqldpz9v7c9a421dsv8dgq13b"))))
    (build-system cmake-build-system)
    (arguments
     '(;; XXX FIXME: 11/85 tests are failing.
       #:tests? #f
       #:configure-flags
       (let* ((lib (string-append (assoc-ref %outputs "out")
                                  "/lib"))
              (runpaths (map (lambda (s) (string-append
                                          lib "/evolution-data-server/" s))
                             '("addressbook-backends" "calendar-backends"
                               "camel-providers" "credential-modules"
                               "registry-modules"))))
         (list "-DENABLE_UOA=OFF"             ;disable Ubuntu Online Accounts support
               "-DENABLE_GOOGLE=OFF"          ;disable Google Contacts support
               "-DENABLE_GOOGLE_AUTH=OFF"     ;disable Google authentication
               "-DENABLE_VALA_BINDINGS=ON"
               ;; FIXME: Building against ICU 60 requires C++11 or higher.  Remove
               ;; "-std=gnu++11" when our default compiler is >= GCC6.
               ;; FIXME: Temporarily use "-DU_USING_ICU_NAMESPACE=1" until
               ;; evolution-data-server has been updated to qualify ICU types
               ;; explicitly, as required by ICU 61 and later.  See:
               ;; <https://ssl.icu-project.org/repos/icu/trunk/icu4c/readme.html#RecBuild>
               "-DCMAKE_CXX_FLAGS=-std=gnu++11 -DU_USING_ICU_NAMESPACE=1"
               (string-append "-DCMAKE_INSTALL_RPATH=" lib ";"
                              (string-append lib "/evolution-data-server;")
                              (string-join runpaths ";"))
               "-DENABLE_INTROSPECTION=ON"))  ;required for Vala bindings
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
          (lambda _
            (substitute* "tests/test-server-utils/e-test-server-utils.c"
              (("/bin/rm") (which "rm")))
            #t))
         (add-before 'configure 'dont-override-rpath
           (lambda _
             (substitute* "CMakeLists.txt"
               ;; CMakeLists.txt hard-codes runpath to just the libdir.
               ;; Remove it so the configure flag is respected.
               (("SET\\(CMAKE_INSTALL_RPATH .*") ""))
             #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("gperf" ,gperf)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("python" ,python-wrapper)))
    (propagated-inputs
     ;; These are all in the Requires field of .pc files.
     `(("gtk+" ,gtk+)
       ("libical" ,libical)
       ("libsecret" ,libsecret)
       ("libsoup" ,libsoup)
       ("nss" ,nss)
       ("sqlite" ,sqlite)))
    (inputs
     `(("bdb" ,bdb)
       ("gcr" ,gcr)
       ("gnome-online-accounts" ,gnome-online-accounts)
       ("json-glib" ,json-glib)
       ("libgweather" ,libgweather)
       ("mit-krb5" ,mit-krb5)
       ("openldap" ,openldap)
       ("webkitgtk" ,webkitgtk)))
    (synopsis "Store address books and calendars")
    (home-page "https://wiki.gnome.org/Apps/Evolution")
    (description
     "This package provides a unified backend for programs that work with
contacts, tasks, and calendar information.  It was originally developed for
Evolution (hence the name), but is now used by other packages as well.")
    (license license:lgpl2.0)))

(define-public caribou
  (package
    (name "caribou")
    (version "0.4.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0mfychh1q3dx0b96pjz9a9y112bm9yqyim40yykzxx1hppsdjhww"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before
          'build 'pre-build
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              ;; Use absolute shared library path in Caribou-1.0.typelib.
              (substitute* "libcaribou/Makefile"
                (("--shared-library=libcaribou.so")
                 (string-append "--shared-library="
                                out "/lib/libcaribou.so")))
              #t)))
         (add-after 'install 'wrap-programs
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (python-path (getenv "PYTHONPATH"))
                   (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
              (for-each
               (lambda (prog)
                 (wrap-program prog
                   `("PYTHONPATH"      ":" prefix (,python-path))
                   `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
               (list (string-append out "/bin/caribou-preferences")
                     (string-append out "/libexec/antler-keyboard"))))
            #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2) ; incompatible with Python 3 (print syntax)
       ("vala" ,vala)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     ;; caribou-1.0.pc refers to all these.
     `(("libgee" ,libgee)
       ("libxklavier" ,libxklavier)
       ("libxtst" ,libxtst)
       ("gtk+" ,gtk+)))
    (inputs
     `(("clutter" ,clutter)
       ("dconf" ,dconf)
       ("gtk+-2" ,gtk+-2)
       ("python-pygobject" ,python2-pygobject)))
    (synopsis "Text entry and UI navigation application")
    (home-page "https://wiki.gnome.org/Projects/Caribou")
    (description
     "Caribou is an input assistive technology intended for switch and pointer
users.")
    (license license:lgpl2.1)))

(define-public network-manager
  (package
    (name "network-manager")
    (version "1.8.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/NetworkManager/"
                                  (version-major+minor version) "/"
                                  "NetworkManager-" version ".tar.xz"))
              (sha256
               (base32
                "04lj081a5cdkhcnj1xs77chhy08d2h0648kmj1csxp46cfrjwpk2"))
              (snippet
              '(begin
                 (use-modules (guix build utils))
                 (substitute* "configure"
                   ;; Replace libsystemd-login with libelogind.
                   (("libsystemd-login") "libelogind"))
                 (substitute* "src/devices/wwan/nm-modem-manager.c"
                   (("systemd") "elogind"))
                 (substitute* "src/nm-session-monitor.c"
                   (("systemd") "elogind"))
                 (substitute* "./src/nm-logging.c"
                   (("systemd") "elogind"))
                 #t))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc")) ; 8 MiB of gtk-doc HTML
    (arguments
     '(#:configure-flags
       (let ((out      (assoc-ref %outputs "out"))
             (doc      (assoc-ref %outputs "doc"))
             (dhclient (string-append (assoc-ref %build-inputs "isc-dhcp")
                                      "/sbin/dhclient")))
         (list "--with-systemd-logind=yes" ;In GuixSD, this is provided by elogind.
               "--with-consolekit=no"
               "--with-crypto=gnutls"
               "--disable-config-plugin-ibft"
               "--sysconfdir=/etc"
               "--localstatedir=/var"
               (string-append "--with-udev-dir="
                              out "/lib/udev")
               (string-append "--with-dbus-sys-dir="
                              out "/etc/dbus-1/system.d")
               (string-append "--with-html-dir="
                              doc "/share/gtk-doc/html")
               (string-append "--with-dhclient=" dhclient)))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda _
             ;; These tests try to test aspects of network-manager's
             ;; functionality within restricted containers, but they don't
             ;; cope with being already in the Guix build jail as that jail
             ;; lacks some features that they would like to proxy over (like
             ;; a /sys mount).
             (substitute* '("Makefile.in")
               (("src/platform/tests/test-address-linux") " ")
               (("src/platform/tests/test-cleanup-linux") " ")
               (("src/platform/tests/test-link-linux") " ")
               (("src/platform/tests/test-route-linux") " ")
               (("src/devices/tests/test-arping") " ")
               (("src/devices/tests/test-lldp") " ")
               (("src/tests/test-route-manager-linux") " "))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             ;; For the missing /etc/machine-id.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             #t))
         (replace 'install
           (lambda _
             (zero? (system* "make"
                             "sysconfdir=/tmp"
                             "rundir=/tmp"
                             "statedir=/tmp"
                             "nmstatedir=/tmp/nm"
                             "install")))))))
    (propagated-inputs
     `(("glib" ,glib)))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for gdbus-codegen
       ("gobject-introspection" ,gobject-introspection)
       ("docbook-xsl" ,docbook-xsl)
       ("intltool" ,intltool)
       ("libxslt" ,libxslt)
       ("libxml2" ,libxml2)
       ("pkg-config" ,pkg-config)
       ;; For testing.
       ("python" ,python-wrapper)
       ("python-dbus" ,python-dbus)
       ("python-pygobject" ,python-pygobject)))
    (inputs
     `(("curl" ,curl)
       ("cyrus-sasl" ,cyrus-sasl)
       ("dbus-glib" ,dbus-glib)
       ("dnsmasq" ,dnsmasq)
       ("eudev" ,eudev)
       ("gnutls" ,gnutls)
       ("iptables" ,iptables)
       ("isc-dhcp" ,isc-dhcp)
       ("jansson" ,jansson)
       ("libgcrypt" ,libgcrypt)
       ("libgudev" ,libgudev)
       ("libndp" ,libndp)
       ("libnl" ,libnl)
       ("libsoup" ,libsoup)
       ("modem-manager" ,modem-manager)
       ("newt" ,newt)                       ;for the 'nmtui' console interface
       ("polkit" ,polkit)
       ("ppp" ,ppp)
       ("readline" ,readline)
       ("util-linux" ,util-linux)
       ("elogind" ,elogind)))
    (synopsis "Network connection manager")
    (home-page "https://www.gnome.org/projects/NetworkManager/")
    (description
     "NetworkManager is a system network service that manages your network
devices and connections, attempting to keep active network connectivity when
available.  It manages ethernet, WiFi, mobile broadband (WWAN), and PPPoE
devices, and provides VPN integration with a variety of different VPN
services.")
    (license license:gpl2+)
    (properties '((upstream-name . "NetworkManager")))))

(define-public network-manager-openvpn
  (package
    (name "network-manager-openvpn")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/NetworkManager-openvpn/"
                    (version-major+minor version)
                    "/NetworkManager-openvpn-" version ".tar.xz"))
              (sha256
               (base32
                "1973n89g66a3jfx8r45a811fga4kadh6r1w35cb25cz1mlii2vhn"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-absolute-paths")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("gtk+" ,gtk+)
       ("openvpn" ,openvpn)
       ("network-manager" ,network-manager)
       ("network-manager-applet" ,network-manager-applet) ;for libnma
       ("libsecret" ,libsecret)))
    (home-page "https://wiki.gnome.org/Projects/NetworkManager/VPN")
    (synopsis "OpenVPN plug-in for NetworkManager")
    (description
     "This extension of NetworkManager allows it to take care of connections
to virtual private networks (VPNs) via OpenVPN.")
    (license license:gpl2+)
    (properties `((upstream-name . "NetworkManager-openvpn")))))

(define-public mobile-broadband-provider-info
  (package
    (name "mobile-broadband-provider-info")
    (version "20170310")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/"
                    "mobile-broadband-provider-info/" version "/"
                    "mobile-broadband-provider-info-" version ".tar.xz"))
              (sha256
               (base32
                "0fxm11x8k9hxjg8l5inaldfmmjnwkay3ibjv899jra03bv4h6kql"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ; No tests
    (home-page "https://wiki.gnome.org/Projects/NetworkManager")
    (synopsis "Database of broadband connection configuration")
    (description "Database of broadband connection configuration.")
    (license license:public-domain)))

(define-public network-manager-applet
  (package
    (name "network-manager-applet")
    (version "1.8.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0ag3pvjp58ykrzsjfbdxi0j5xd2i796jk7nns67zy03xwg9i0l0h"))))
    (build-system glib-or-gtk-build-system)
    (arguments '(#:configure-flags '("--disable-migration")))
    (native-inputs
     `(("intltool" ,intltool)
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     ;; libnm-gtk.pc refers to all these.
     `(("dbus-glib" ,dbus-glib)
       ("gtk+" ,gtk+)
       ("network-manager" ,network-manager)))
    (inputs
     `(("iso-codes" ,iso-codes)
       ("libgudev" ,libgudev)
       ("libnotify" ,libnotify)
       ("libsecret" ,libsecret)
       ("libselinux" ,libselinux)
       ("jansson" ,jansson) ; for team support
       ("modem-manager" ,modem-manager)))
    (synopsis "Applet for managing network connections")
    (home-page "https://www.gnome.org/projects/NetworkManager/")
    (description
     "This package contains a systray applet for NetworkManager.  It displays
the available networks and allows users to easily switch between them.")
    (license license:gpl2+)))

(define-public libxml++
  (package
    (name "libxml++")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "19kik79fmg61nv0by0a5f9wchrcfjwzvih4v2waw01hqflhqvp0r"))))
    (build-system gnu-build-system)
    ;; libxml++-3.0.pc refers to all these.
    (propagated-inputs
     `(("libxml2" ,libxml2)
       ("glibmm" ,glibmm)))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (home-page "http://libxmlplusplus.sourceforge.net/")
    (synopsis "C++ wrapper for XML parser library libxml2")
    (description
     "This package provides a C++ wrapper for the XML parser library
libxml2.")
    (license license:lgpl2.1+)))

(define-public libxml++-2
  (package
    (inherit libxml++)
    (name "libxml++")
    (version "2.40.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1sb3akryklvh2v6m6dihdnbpf1lkx441v972q9hlz1sq6bfspm2a"))))))

(define-public gdm
  (package
    (name "gdm")
    (version "3.26.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (patches (search-patches "gdm-CVE-2018-14424.patch"))
              (sha256
               (base32
                "0mxdal6hh345xk2xqmw5192jgpprkbcv1d4bwmnl4arcc00cpp8p"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:configure-flags
       `("--without-plymouth"
         "--disable-systemd-journal"

         ;; Using --with-initial-vt=7 allows GDM to run alongside TTY 1,
         ;; instead of having to replace it (i.e., stopping the mingetty
         ;; service for TTY 1 before starting GDM).
         "--with-initial-vt=7"

         ;; Use '/etc/environment' for locale settings instead of the
         ;; systemd-specific '/etc/locale.conf'.
         "--with-lang-file=/etc/environment"

         "--localstatedir=/var"
         ,(string-append "--with-default-path="
                         (string-join '("/run/setuid-programs"
                                        "/run/current-system/profile/bin"
                                        "/run/current-system/profile/sbin")
                                      ":"))
         ;; Put GDM in bindir so that glib-or-gtk-build-system wraps the
         ;; XDG_DATA_DIRS so that it finds its schemas.
         "--sbindir" ,(string-append (assoc-ref %outputs "out") "/bin"))
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'pre-configure
          (lambda* (#:key inputs #:allow-other-keys)
            ;; We don't have <systemd/sd-daemon.h>.
            (substitute* '("common/gdm-log.c"
                           "daemon/gdm-server.c"
                           "daemon/gdm-session-worker.c"
                           "daemon/gdm-session-worker-job.c")
              (("#include <systemd/sd-daemon\\.h>") ""))
            ;; Use elogind for sd-login.
            (substitute* '("common/gdm-common.c"
                           "daemon/gdm-manager.c"
                           "libgdm/gdm-user-switching.c")
              (("#include <systemd/sd-login\\.h>")
               "#include <elogind/sd-login.h>"))
            ;; Check for elogind.
            (substitute* '("configure")
              (("libsystemd")
               "libelogind"))
            ;; Look for system-installed sessions in
            ;; /run/current-system/profile/share.
            (substitute* '("libgdm/gdm-sessions.c"
                           "daemon/gdm-session.c"
                           "daemon/gdm-display.c"
                           "daemon/gdm-launch-environment.c")
              (("DATADIR \"/x")
               "\"/run/current-system/profile/share/x")
              (("DATADIR \"/wayland")
               "\"/run/current-system/profile/share/wayland")
              (("DATADIR \"/gnome")
               "\"/run/current-system/profile/share/gnome"))
            (let ((propagate '("GDM_CUSTOM_CONF"
                               "GDM_DBUS_DAEMON"
                               "GDM_X_SERVER"
                               "GDM_X_SESSION"
                               ;; XXX: Remove this once GNOME Shell is
                               ;; a dependency of GDM.
                               "XDG_DATA_DIRS")))
              (substitute* "daemon/gdm-session.c"
                (("set_up_session_environment \\(self\\);")
                 (apply string-append
                        "set_up_session_environment (self);\n"
                        (map (lambda (name)
                               (string-append
                                "gdm_session_set_environment_variable "
                                "(self, \"" name "\","
                                "g_getenv (\"" name "\"));\n"))
                             propagate)))))
            ;; Look for custom GDM conf in /run/current-system.
            (substitute* '("common/gdm-settings-desktop-backend.c")
              (("GDM_CUSTOM_CONF")
               (string-append "(g_getenv(\"GDM_CUSTOM_CONF\") != NULL"
                              " ? g_getenv(\"GDM_CUSTOM_CONF\")"
                              " : GDM_CUSTOM_CONF)")))
            ;; Use service-supplied path to X.
            (substitute* '("daemon/gdm-server.c")
              (("\\(X_SERVER X_SERVER_ARG_FORMAT")
               "(\"%s\" X_SERVER_ARG_FORMAT, g_getenv (\"GDM_X_SERVER\")"))
            (substitute* '("daemon/gdm-x-session.c")
              (("\"dbus-daemon\"")
               "g_getenv (\"GDM_DBUS_DAEMON\")")
              (("X_SERVER")
               "g_getenv (\"GDM_X_SERVER\")")
              (("GDMCONFDIR \"/Xsession\"")
               "g_getenv (\"GDM_X_SESSION\")"))
            ;; Use an absolute path for GNOME Session.
            (substitute* "daemon/gdm-launch-environment.c"
              (("\"gnome-session\"")
               (string-append "\"" (assoc-ref inputs "gnome-session")
                              "/bin/gnome-session\"")))
            #t))
         ;; GDM needs GNOME Session to run these applications.  We link
         ;; their autostart files in `share/gdm/greeter/autostart'
         ;; because GDM explicitly tells GNOME Session to look there.
         ;;
         ;; XXX: GNOME Shell should be linked here too, but currently
         ;; GNOME Shell depends on GDM.
         (add-after 'install 'link-autostart-files
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (autostart (string-append out "/share/gdm/"
                                              "greeter/autostart"))
                    (settings (assoc-ref inputs "gnome-settings-daemon")))
               (mkdir-p autostart)
               (with-directory-excursion autostart
                 (for-each (lambda (desktop)
                             (symlink desktop (basename desktop)))
                           (find-files (string-append settings "/etc/xdg"))))
               #t))))))
    (native-inputs
     `(("dconf" ,dconf)
       ("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("accountsservice" ,accountsservice)
       ("check" ,check) ; for testing
       ("elogind" ,elogind)
       ("gnome-session" ,gnome-session)
       ("gnome-settings-daemon" ,gnome-settings-daemon)
       ("gtk+" ,gtk+)
       ("iso-codes" ,iso-codes)
       ("libcanberra" ,libcanberra)
       ("linux-pam" ,linux-pam)))
    (synopsis "Display manager for GNOME")
    (home-page "https://wiki.gnome.org/Projects/GDM/")
    (description
     "GNOME Display Manager is a system service that is responsible for
providing graphical log-ins and managing local and remote displays.")
    (license license:gpl2+)))

(define-public libgtop
  (package
    (name "libgtop")
    (version "2.38.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "04mnxgzyb26wqk6qij4iw8cxwl82r8pcsna5dg8vz2j3pdi0wv2g"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("glib" ,glib))) ; required by libgtop-2.0.pc
    (synopsis "Portable system access library")
    (home-page "https://www.gnome.org/")
    (description
     "LibGTop is a library to get system specific data such as CPU and memory
usage and information about running processes.")
    (license license:gpl2+)))

(define-public gnome-bluetooth
  (package
    (name "gnome-bluetooth")
    (version "3.20.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1zlqcz6jz4vzzr8gd1678i9s4015kiwcpr5szrwz4kmryfsm147a"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for gdbus-codegen, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (propagated-inputs
     ;; gnome-bluetooth-1.0.pc refers to all these.
     `(("gtk+" ,gtk+)
       ("udev" ,eudev)))
    (inputs
     `(("libcanberra" ,libcanberra)
       ("libnotify" ,libnotify)))
    (synopsis "GNOME Bluetooth subsystem")
    (home-page "https://wiki.gnome.org/Projects/GnomeBluetooth")
    (description
     "This package contains tools for managing and manipulating Bluetooth
devices using the GNOME desktop.")
    (license license:lgpl2.1+)))

(define-public gnome-control-center
  (package
    (name "gnome-control-center")
    (version "3.24.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "18ncjqjj93a39sla2zjr9i6pw59yh87p4jla899lmvi2qajd5923"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libc   (assoc-ref inputs "libc"))
                   (tzdata (assoc-ref inputs "tzdata")))
               (substitute* "panels/datetime/tz.h"
                 (("/usr/share/zoneinfo/zone.tab")
                  (string-append tzdata "/share/zoneinfo/zone.tab")))
               (substitute* "panels/datetime/test-endianess.c"
                 (("/usr/share/locale")
                  (string-append libc "/share/locale")))
               #t))))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-mkenums, etc.
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("accountsservice" ,accountsservice)
       ("clutter-gtk" ,clutter-gtk)
       ("colord-gtk" ,colord-gtk)
       ("cups" ,cups)
       ("dconf" ,dconf)
       ("docbook-xsl" ,docbook-xsl)
       ("gdk-pixbuf" ,gdk-pixbuf) ; for loading SVG files
       ("gnome-bluetooth" ,gnome-bluetooth)
       ("gnome-desktop" ,gnome-desktop)
       ("gnome-online-accounts" ,gnome-online-accounts)
       ("gnome-settings-daemon" ,gnome-settings-daemon)
       ("grilo" ,grilo)
       ("ibus" ,ibus)
       ("libcanberra" ,libcanberra)
       ("libgudev" ,libgudev)
       ("libgtop" ,libgtop)
       ("libpwquality" ,libpwquality)
       ("libsoup" ,libsoup)
       ("libxml2" ,libxml2)
       ("libwacom" ,libwacom)
       ("mesa" ,mesa)
       ("mit-krb5" ,mit-krb5)
       ("modem-manager" ,modem-manager)
       ("network-manager-applet" ,network-manager-applet)
       ("polkit" ,polkit)
       ("pulseaudio" ,pulseaudio)
       ("smbclient" ,samba)
       ("tzdata" ,tzdata)
       ("upower" ,upower)))
    (synopsis "Utilities to configure the GNOME desktop")
    (home-page "https://www.gnome.org/")
    (description
     "This package contains configuration applets for the GNOME desktop,
allowing to set accessibility configuration, desktop fonts, keyboard and mouse
properties, sound setup, desktop theme and background, user interface
properties, screen resolution, and other GNOME parameters.")
    (license license:gpl2+)))

(define-public gnome-shell
  (package
    (name "gnome-shell")
    (version "3.24.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1f20x36ymkp1j667hb7s7byly2gqc4m0anldy3qwp38vm8437caq"))
              (patches (search-patches "gnome-shell-theme.patch"))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   ;; CSS files have to be regenerated from the .scss source
                   ;; that 'gnome-shell-theme.patch' modifies.
                   (for-each delete-file
                             (find-files "data/theme"
                                         "^gnome-shell.*\\.css$"))

                   ;; Copy images for use on the GDM log-in screen.
                   (copy-file #$(file-append %artwork-repository
                                             "/slim/0.x/background.png")
                              "data/theme/guix-background.png")
                   (invoke #+(file-append inkscape "/bin/inkscape")
                           "--export-png=data/theme/guix-logo.png"
                           #$(file-append %artwork-repository
                                          "/logo/Guix-horizontal-white.svg"))
                   #t))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'rebuild-css
           (lambda _
             ;; Rebuild the CSS files from the .scss files that our patch
             ;; modifies.
             (invoke "make" "-C" "data"
                     "theme/gnome-shell.css"
                     "theme/gnome-shell-high-contrast.css")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (keysdir (string-append
                              out "/share/gnome-control-center/keybindings")))
               (zero? (system* "make"
                               (string-append "keysdir=" keysdir)
                               "install")))))
         (add-after
          'install 'wrap-programs
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out              (assoc-ref outputs "out"))
                  (gi-typelib-path  (getenv "GI_TYPELIB_PATH"))
                  (python-path      (getenv "PYTHONPATH")))
              (wrap-program (string-append out "/bin/gnome-shell")
                `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))
              (for-each
               (lambda (prog)
                 (wrap-program (string-append out "/bin/" prog)
                   `("PYTHONPATH"      ":" prefix (,python-path))
                   `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
               '("gnome-shell-extension-tool" "gnome-shell-perf-tool"))
              #t))))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("xsltproc" ,libxslt)
       ("ruby-sass" ,ruby-sass)))
    (inputs
     `(("accountsservice" ,accountsservice)
       ("caribou" ,caribou)
       ("docbook-xsl" ,docbook-xsl)
       ("evolution-data-server" ,evolution-data-server)
       ("gcr" ,gcr)
       ("gdm" ,gdm)
       ("gjs" ,gjs)
       ("gnome-bluetooth" ,gnome-bluetooth)
       ("gnome-control-center" ,gnome-control-center)
       ("gnome-desktop" ,gnome-desktop)
       ("gnome-settings-daemon" ,gnome-settings-daemon)
       ("gst-plugins-base" ,gst-plugins-base)
       ("ibus" ,ibus)
       ("libcanberra" ,libcanberra)
       ("libcroco" ,libcroco)
       ("libgweather" ,libgweather)
       ("libsoup" ,libsoup)
       ("mesa-headers" ,mesa-headers)
       ("mutter" ,mutter)
       ("network-manager-applet" ,network-manager-applet)
       ("polkit" ,polkit)
       ("pulseaudio" ,pulseaudio)
       ("python-pygobject" ,python-pygobject)
       ("startup-notification" ,startup-notification)
       ("telepathy-logger" ,telepathy-logger)
       ("upower" ,upower)
       ;; XXX: These requirements were added in 3.24, but no mention in NEWS.
       ;; Missing propagation? See also: <https://bugs.gnu.org/27264>
       ("librsvg" ,librsvg)
       ("geoclue" ,geoclue)
       ;; XXX: required by libgjs.la.
       ("readline" ,readline)))
    (synopsis "Desktop shell for GNOME")
    (home-page "https://wiki.gnome.org/Projects/GnomeShell")
    (description
     "GNOME Shell provides core user interface functions for the GNOME desktop,
like switching to windows and launching applications.")
    (license license:gpl2+)))

(define-public gtk-vnc
  (package
    (name "gtk-vnc")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1dya1wc9vis8h0fv625pii1n70cckf1xjg1m2hndz989d118i6is"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--with-gtk=3.0")))
    (propagated-inputs
     `(("gtk+" ,gtk+))) ; required by gtk-vnc-2.0.pc.
    (inputs
     `(("cyrus-sasl" ,cyrus-sasl)
       ("gnutls" ,gnutls)
       ("libgcrypt" ,libgcrypt)
       ("pulseaudio" ,pulseaudio)))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)
       ("vala" ,vala)))
    (home-page "https://wiki.gnome.org/Projects/gtk-vnc")
    (synopsis "VNC viewer widget for GTK+")
    (description
     "GTK-VNC is a VNC viewer widget for GTK+, used by remote desktop viewing
applications, for instance the Vinagre client, GNOME Boxes and virt-viewer.
GTK-VNC implements client side RFB protocol and authentication extensions such
as SASL, TLS and VeNCrypt.  Additionally it supports encoding extensions.")
    (license license:lgpl2.1+)))

(define-public gnome-autoar
  (package
    (name "gnome-autoar")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "02i4zgqqqj56h7bcys6dz7n78m4nj2x4dv1ggjmnrk98n06xpsax"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("gnome-common" ,gnome-common)
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("libarchive" ,libarchive)))  ; XXX document why
    (inputs
     `(("gtk+" ,gtk+)
       ("nettle" ,nettle))) ; XXX: required by libarchive.pc
    (synopsis "Archives integration support for GNOME")
    (home-page "https://git.gnome.org/browse/gnome-autoar/")
    (description
     "GNOME Autoar is a library which makes creating and extracting archives
easy, safe, and automatic.")
    (license license:lgpl2.1+)))

(define-public tracker
  (package
    (name "tracker")
    (version "1.12.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1mpq418lzba7fad0w0m3bnxvz3khf461b5zya8zmq5n1g0w99ki3"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("gnome-common" ,gnome-common)
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("gtk+" ,gtk+)
       ("dbus" ,dbus)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("sqlite" ,sqlite)
       ("nettle" ,nettle)  ; XXX why is this needed?
       ("poppler" ,poppler)
       ("libgsf" ,libgsf)
       ("libexif" ,libexif)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("libvorbis" ,libvorbis)
       ("flac" ,flac)
       ("totem-pl-parser" ,totem-pl-parser)
       ("zlib" ,zlib)
       ("exempi" ,exempi)
       ("libxml2" ,libxml2)
       ("upower" ,upower)
       ("libunistring" ,libunistring)
       ("giflib" ,giflib)
       ("json-glib" ,json-glib)
       ("openjpeg" ,openjpeg-1)
       ("libosinfo" ,libosinfo)
       ("libcue" ,libcue)
       ("libseccomp" ,libseccomp)
       ("libsoup" ,libsoup)
       ("libuuid" ,util-linux)))
    (arguments `(#:tests? #f))  ; XXX FIXME enable tests (some fail)
    (synopsis "Metadata database, indexer and search tool")
    (home-page "https://wiki.gnome.org/Projects/Tracker")
    (description
     "Tracker is an advanced framework for first class objects with associated
metadata and tags.  It provides a one stop solution for all metadata, tags,
shared object databases, search tools and indexing.")
    ;; src/libtracker-*/* and src/tracker-extract/* are covered by lgpl2.1+,
    ;; src/gvdb/* are covered by lgpl2.0+, and the rest is gpl2+.
    (license (list license:gpl2+
                   license:lgpl2.1+
                   license:lgpl2.0+))))

(define-public nautilus
  (package
    (name "nautilus")
    (version "3.26.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1d0l4vgcjqj4671hb6s2a56baqlasbxn3wl6vfrpdsk5qq299jbr"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       ;; XXX: FAIL: check-nautilus
       ;;   Settings schema 'org.gnome.nautilus.preferences' is not installed
       #:tests? #f))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils) ; for update-desktop-database
       ("glib:bin" ,glib "bin")         ; for glib-mkenums, etc.
       ("gtk+:bin" ,gtk+ "bin")         ; for gtk-update-icon-cache
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("dconf" ,dconf)
       ("gvfs" ,gvfs)
       ("exempi" ,exempi)
       ("gnome-desktop" ,gnome-desktop)
       ("gnome-autoar" ,gnome-autoar)
       ("libselinux" ,libselinux)
       ("nettle" ,nettle) ; XXX required by libarchive.pc via gnome-autoar
       ("tracker" ,tracker)
       ;; XXX: gtk+ is required by libnautilus-extension.pc
       ;;
       ;; Don't propagate it to reduces "profile pollution" of the 'gnome' meta
       ;; package.  See:
       ;; <http://lists.gnu.org/archive/html/guix-devel/2016-03/msg00283.html>.
       ("gtk+" ,gtk+)
       ("libexif" ,libexif)
       ("libxml2" ,libxml2)))
    (synopsis "File manager for GNOME")
    (home-page "https://wiki.gnome.org/Apps/Nautilus")
    (description
     "Nautilus (Files) is a file manager designed to fit the GNOME desktop
design and behaviour, giving the user a simple way to navigate and manage its
files.")
    (license license:gpl2+)))

(define-public baobab
  (package
    (name "baobab")
    (version "3.28.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0qsx7vx5c3n4yxlxbr11sppw7qwcv9z3g45b5xb9y7wxw5lv42sk"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("desktop-file-utils" ,desktop-file-utils) ; for update-desktop-database
       ("gtk+-bin" ,gtk+ "bin") ; for gtk-update-icon-cache
       ("itstool" ,itstool)
       ("xmllint" ,libxml2)
       ("glib" ,glib "bin")
       ("vala" ,vala)))
    (inputs
     `(("gtk+" ,gtk+)))
    (synopsis "Disk usage analyzer for GNOME")
    (description
     "Baobab (Disk Usage Analyzer) is a graphical application to analyse disk
usage in the GNOME desktop environment.  It can easily scan device volumes or
a specific user-requested directory branch (local or remote).  Once the scan
is complete it provides a graphical representation of each selected folder.")
    (home-page "https://wiki.gnome.org/Apps/Baobab")
    (license license:gpl2+)))

(define-public gnome-backgrounds
  (package
    (name "gnome-backgrounds")
    (version "3.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1qgim0yhzjgcq172y4vp5hqz4rh1ak38a7pgi6s7dq0wklyrcnxj"))))
    (build-system meson-build-system)
    (native-inputs
     `(("intltool" ,intltool)))
    (home-page "https://git.gnome.org/browse/gnome-backgrounds")
    (synopsis "Background images for the GNOME desktop")
    (description
     "GNOME backgrounds package contains a collection of graphics files which
can be used as backgrounds in the GNOME Desktop environment.  Additionally,
the package creates the proper framework and directory structure so that you
can add your own files to the collection.")
    (license (list license:gpl2+
                   license:cc-by2.0
                   license:cc-by-sa2.0
                   license:cc-by-sa3.0))))

(define-public gnome-screenshot
  (package
    (name "gnome-screenshot")
    (version "3.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0c02n1hgv21m4082jx399b1yazsc0cr07ba8k0pv8v3w7i5z21ca"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+)
       ("libcanberra" ,libcanberra)
       ("libx11" ,libx11)
       ("libxext" ,libxext)))
    (home-page "https://git.gnome.org/browse/gnome-screenshot")
    (synopsis "Take pictures of your screen")
    (description
     "GNOME Screenshot is a utility used for taking screenshots of the entire
screen, a window or a user defined area of the screen, with optional
beautifying border effects.")
    (license license:gpl2+)))

(define-public dconf-editor
  (package
    (name "dconf-editor")
    (version "3.26.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1lcarg8igsqpq4iv5834mf9kz4cmfhfw11bwy3x7v7f497z57d18"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'skip-gtk-update-icon-cache
           (lambda _
             ;; Don't create 'icon-theme.cache'.
             (substitute* "editor/Makefile"
               (("gtk-update-icon-cache") "true"))
             #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-schemas, gio-2.0.
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("dconf" ,dconf)
       ("gtk+" ,gtk+)
       ("libxml2" ,libxml2)))
    (home-page "https://git.gnome.org/browse/dconf-editor")
    (synopsis "Graphical editor for GNOME's dconf configuration system")
    (description
     "Dconf-editor is a graphical tool for browsing and editing the dconf
configuration system for GNOME.  It allows users to configure desktop
software that do not provide their own configuration interface.")
    (license license:lgpl2.1+)))

(define-public gnome-default-applications
  (package
    (name "gnome-default-applications")
    (version "0")
    (build-system trivial-build-system)
    (source #f)
    (propagated-inputs
     `(("nautilus" ,nautilus)
       ("evince" ,evince)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (apps (string-append out "/share/applications")))
           (mkdir-p apps)
           (call-with-output-file (string-append apps "/defaults.list")
             (lambda (port)
               (format port "[Default Applications]\n")
               (format port "inode/directory=org.gnome.Nautilus.desktop\n")
               (format port "application/pdf=evince.desktop\n")
               (format port "application/postscript=evince.desktop\n")))
           #t))))
    (synopsis "Default MIME type associations for the GNOME desktop")
    (description
     "Given many installed packages which might handle a given MIME type, a
user running the GNOME desktop probably has some preferences: for example,
that folders be opened by default by the Nautilus file manager, not the Baobab
disk usage analyzer.  This package establishes that set of default MIME type
associations for GNOME.")
    (license license:gpl3+)
    (home-page #f)))

(define-public gnome
  (package
    (name "gnome")
    (version (package-version gnome-shell))
    (source #f)
    (build-system trivial-build-system)
    (arguments '(#:builder (begin (mkdir %output) #t)))
    (propagated-inputs
     ;; TODO: Add more packages according to:
     ;;       <https://packages.debian.org/jessie/gnome-core>.
     `(("adwaita-icon-theme"        ,adwaita-icon-theme)
       ("baobab"                    ,baobab)
       ("font-cantarell"            ,font-cantarell)
       ("font-dejavu"               ,font-dejavu)
       ("at-spi2-core"              ,at-spi2-core)
       ("dbus"                      ,dbus)
       ("dconf"                     ,dconf)
       ("desktop-file-utils"        ,desktop-file-utils)
       ("eog"                       ,eog)
       ("epiphany"                  ,epiphany)
       ("evince"                    ,evince)
       ("file-roller"               ,file-roller)
       ("gedit"                     ,gedit)
       ("glib-networking"           ,glib-networking)
       ("gnome-backgrounds"         ,gnome-backgrounds)
       ("gnome-bluetooth"           ,gnome-bluetooth)
       ("gnome-calculator"          ,gnome-calculator)
       ("gnome-control-center"      ,gnome-control-center)
       ("gnome-disk-utility"        ,gnome-disk-utility)
       ("gnome-default-applications" ,gnome-default-applications)
       ("gnome-keyring"             ,gnome-keyring)
       ("gnome-online-accounts"     ,gnome-online-accounts)
       ("gnome-session"             ,gnome-session)
       ("gnome-settings-daemon"     ,gnome-settings-daemon)
       ("gnome-shell"               ,gnome-shell)
       ("gnome-system-monitor"      ,gnome-system-monitor)
       ("gnome-terminal"            ,gnome-terminal)
       ("gnome-themes-standard"     ,gnome-themes-standard)
       ("gst-plugins-base"          ,gst-plugins-base)
       ("gst-plugins-good"          ,gst-plugins-good)
       ("gucharmap"                 ,gucharmap)
       ("gvfs"                      ,gvfs)
       ("hicolor-icon-theme"        ,hicolor-icon-theme)
       ("nautilus"                  ,nautilus)
       ("pinentry-gnome3"           ,pinentry-gnome3)
       ("pulseaudio"                ,pulseaudio)
       ("shared-mime-info"          ,shared-mime-info)
       ("system-config-printer"     ,system-config-printer)
       ("totem"                     ,totem)
       ("xdg-user-dirs"             ,xdg-user-dirs)
       ("yelp"                      ,yelp)
       ("zenity"                    ,zenity)))
    (synopsis "The GNU desktop environment")
    (home-page "https://www.gnome.org/")
    (description
     "GNOME is the graphical desktop for GNU.  It includes a wide variety of
applications for browsing the web, editing text and images, creating
documents and diagrams, playing media, scanning, and much more.")
    (license license:gpl2+)))

(define-public byzanz
  ;; The last stable release of Byzanz was in 2011, but there have been many
  ;; useful commits made to the Byzanz repository since then that it would be
  ;; silly to use such an old release.
  (let ((commit "f7af3a5bd252db84af8365bd059c117a7aa5c4af"))
    (package
      (name "byzanz")
      (version (string-append "0.2-1." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.gnome.org/browse/byzanz")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1l60myzxf9cav27v5v3nsijlslz9r7ip6d5kiirfpkf9k0w26hz3"))))
      (build-system glib-or-gtk-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (replace 'bootstrap
             (lambda _
               ;; The build system cleverly detects that we're not building from
               ;; a release tarball and turns on -Werror for GCC.
               ;; Unsurprisingly, there is a warning during compilation that
               ;; causes the build to fail unnecessarily, so we remove the flag.
               (substitute* '("configure.ac")
                 (("-Werror") ""))
               ;; The autogen.sh script in gnome-common will run ./configure
               ;; by default, which is problematic because source shebangs
               ;; have not yet been patched.
               (setenv "NOCONFIGURE" "t")
               (zero? (system* "sh" "autogen.sh")))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("gnome-common" ,gnome-common)
         ("intltool" ,intltool)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)
         ("which" ,which)))
      (inputs
       `(("glib" ,glib)
         ("gstreamer" ,gstreamer)
         ("gst-plugins-base" ,gst-plugins-base)
         ("gtk+" ,gtk+)))
      (synopsis "Desktop recording program")
      (description "Byzanz is a simple desktop recording program with a
command-line interface.  It can record part or all of an X display for a
specified duration and save it as a GIF encoded animated image file.")
      (home-page "https://git.gnome.org/browse/byzanz")
      (license license:gpl2+))))

(define-public gsound
  (package
    (name "gsound")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0lwfwx2c99qrp08pfaj59pks5dphsnxjgrxyadz065d8xqqgza5v"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gobject-introspection" ,gobject-introspection)
       ("vala" ,vala)))
    (inputs
     `(("glib" ,glib)
       ("libcanberra" ,libcanberra)))
    (home-page "https://wiki.gnome.org/Projects/GSound")
    (synopsis "GObject wrapper for libcanberra")
    (description
     "GSound is a small library for playing system sounds.  It's designed to be
used via GObject Introspection, and is a thin wrapper around the libcanberra C
library.")
    (license license:lgpl2.1+)))

(define-public libzapojit
  (package
    (name "libzapojit")
    (version "0.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0zn3s7ryjc3k1abj4k55dr2na844l451nrg9s6cvnnhh569zj99x"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gnome-online-accounts" ,gnome-online-accounts)
       ("json-glib" ,json-glib)
       ("rest" ,rest)))
    (home-page "https://wiki.gnome.org/Projects/Zapojit")
    (synopsis "Library for accessing SkyDrive and Hotmail")
    (description
     "Libzapojit is a GLib-based library for accessing online service APIs of
Microsoft SkyDrive and Hotmail, using their REST protocols.")
    (license license:lgpl2.1+)))

(define-public gnome-clocks
  (package
    (name "gnome-clocks")
    (version "3.26.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1xqirnf7xkiv5vj3ng4slpyh50ihn625vhdgskfzx83a3pgxin1b"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t))
    (native-inputs
     `(("vala" ,vala)
       ("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")             ; for glib-compile-resources
       ("gtk+-bin" ,gtk+ "bin")         ; for gtk-update-icon-cache
       ("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("itstool" ,itstool)))
    (inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("gsound" ,gsound)
       ("geoclue" ,geoclue)
       ("geocode-glib" ,geocode-glib)
       ("libgweather" ,libgweather)
       ("gnome-desktop" ,gnome-desktop)))
    (home-page "https://wiki.gnome.org/Apps/Clocks")
    (synopsis "GNOME's clock application")
    (description
     "GNOME Clocks is a simple clocks application designed to fit the GNOME
desktop.  It supports world clock, stop watch, alarms, and count down timer.")
    (license license:gpl3+)))

(define-public gnome-calendar
  (package
    (name "gnome-calendar")
    (version "3.26.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1clnfvvsaqw9vpxrs6qrxzmgpaw9x2nkjik2x2vwvm07pdvhddxn"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       ;; gnome-calendar has to be installed before the tests can be run
       ;; https://bugzilla.gnome.org/show_bug.cgi?id=788224
       #:tests? #f))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib-bin" ,glib "bin")         ; For glib-compile-schemas
       ("gtk+-bin" ,gtk+ "bin")         ; For gtk-update-icon-cache
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("evolution-data-server" ,evolution-data-server)
       ("gnome-online-accounts" ,gnome-online-accounts)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)))
    (home-page "https://wiki.gnome.org/Apps/Calendar")
    (synopsis "GNOME's calendar application")
    (description
     "GNOME Calendar is a simple calendar application designed to fit the GNOME
desktop.  It supports multiple calendars, month, week and year view.")
    (license license:gpl3+)))

(define-public gnome-todo
  (package
    (name "gnome-todo")
    (version "3.26.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (patches (search-patches "gnome-todo-libical-compat.patch"))
              (sha256
               (base32
                "106xx1w18pxjmj5k0k2qjzi6b3c3kaz7b5kyrpknykibnr401ff9"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases (modify-phases %standard-phases
                  (add-after
                      'install 'wrap-gnome-todo
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out               (assoc-ref outputs "out"))
                            (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                            (python-path       (getenv "PYTHONPATH")))
                        (wrap-program (string-append out "/bin/gnome-todo")
                          ;; XXX: gi plugins are broken.
                          ;; See https://bugzilla.gnome.org/show_bug.cgi?id=787212
                          ;; For plugins.
                          `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                          `("PYTHONPATH" ":" prefix (,python-path))))
                      #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("glib:bin" ,glib "bin")         ; For glib-compile-resources
       ("gtk+-bin" ,gtk+ "bin")         ; For gtk-update-icon-cache
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("rest" ,rest)                   ; For Todoist plugin
       ("json-glib" ,json-glib)         ; For Todoist plugin
       ("libical" ,libical)
       ("libpeas" ,libpeas)
       ("python-pygobject" ,python-pygobject)
       ("evolution-data-server" ,evolution-data-server)
       ("gnome-online-accounts" ,gnome-online-accounts)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)))
    (home-page "https://wiki.gnome.org/Apps/Todo")
    (synopsis "GNOME's ToDo Application")
    (description
     "GNOME To Do is a simplistic personal task manager designed to perfectly
fit the GNOME desktop.")
    (license license:gpl3+)))

(define-public gnome-dictionary
  (package
    (name "gnome-dictionary")
    (version "3.26.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "007k2bq8iplg4psdpz074r5d4zxvn4s95qym4rw9hs6giljbrf0n"))))
    (build-system meson-build-system)
    (arguments '(#:glib-or-gtk? #t
                 #:phases (modify-phases %standard-phases
                            (add-after 'unpack 'patch-install-script
                              (lambda _
                                ;; This script attempts to compile glib schemas
                                ;; and create an empty MIME database.  We do
                                ;; that elsewhere, so prevent it from running.
                                (substitute* "build-aux/post-install.sh"
                                  (("\\[ -z \"\\$DESTDIR\" \\]") "false"))
                                #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)))
    (home-page "https://wiki.gnome.org/Apps/Dictionary")
    (synopsis "Look up words in dictionary sources")
    (description
     "GNOME Dictionary can look for the definition or translation of a word in
existing databases over the internet.")
    (license license:gpl3+)))

(define-public gnome-tweak-tool
  (package
    (name "gnome-tweak-tool")
    (version "3.26.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-tweak-tool/"
                                  (version-major+minor version) "/"
                                  "gnome-tweak-tool-" version ".tar.xz"))
              (patches (list
                        (search-patch "gnome-tweak-tool-search-paths.patch")))
              (sha256
               (base32
                "1pq5a0kzh1sz7s7ax5c7p6212k9d51nk5bfvjfyqn99cs928187x"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags '("--localstatedir=/tmp"
                           "--sysconfdir=/tmp")
       #:imported-modules ((guix build python-build-system)
                           ,@%glib-or-gtk-build-system-modules)
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (replace 'build
                    (lambda* (#:key outputs #:allow-other-keys)
                      (invoke "meson" "build"
                                      "--prefix" (assoc-ref outputs "out"))))
                  (replace 'check
                    (lambda _ (invoke "ninja" "-C" "build" "test")))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (invoke "ninja" "-C" "build" "install")))
                  (add-after 'install 'wrap-program
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out               (assoc-ref outputs "out"))
                            (gi-typelib-path   (getenv "GI_TYPELIB_PATH")))
                        (wrap-program (string-append out "/bin/gnome-tweak-tool")
                          `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
                      #t))
                  (add-after 'install 'wrap
                    (@@ (guix build python-build-system) wrap)))))
    (native-inputs
     `(("gtk+:bin" ,gtk+ "bin")         ; For gtk-update-icon-cache
       ("intltool" ,intltool)
       ("meson" ,meson-for-build)
       ("ninja" ,ninja)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gnome-desktop" ,gnome-desktop)
       ("gtk+" ,gtk+)
       ("gobject-introspection" ,gobject-introspection)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("libnotify" ,libnotify)
       ("libsoup" ,libsoup)
       ("nautilus" ,nautilus)
       ("python" ,python)
       ("python-pygobject" ,python-pygobject)))
    (synopsis "Customize advanced GNOME 3 options")
    (home-page "https://wiki.gnome.org/action/show/Apps/GnomeTweakTool")
    (description
     "GNOME Tweak Tool allows adjusting advanced configuration settings in
GNOME 3.  This includes things like the fonts used in user interface elements,
alternative user interface themes, changes in window management behavior,
GNOME Shell appearance and extension, etc.")
    (license license:gpl3+)))

(define-public gnome-shell-extensions
  (package
    (name "gnome-shell-extensions")
    (version "3.24.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0y8anpp9ymp0lxn15w63ra6zxxf8nvbl48xqkvqdjzida73fyz9w"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-extensions=all")))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib" ,glib "bin")))
    (synopsis "Extensions for GNOME Shell")
    (description "GNOME Shell extensions modify and extend GNOME Shell
functionality and behavior.")
    (home-page "https://extensions.gnome.org/")
    (license license:gpl3+)))

(define-public arc-theme
  (package
    (name "arc-theme")
    (version "20181022")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/NicoHood/arc-theme.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08951dk1irfadwpr3p323a4fprmxg53rk2r2niwq3v62ryhi3663"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; autogen.sh calls configure at the end of the script.
         (replace 'bootstrap
           (lambda _ (invoke "autoreconf" "-vfi"))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("glib" ,glib "bin")             ; for glib-compile-resources
       ("gnome-shell" ,gnome-shell)
       ("gtk+" ,gtk+)
       ("inkscape" ,inkscape)
       ("optipng" ,optipng)
       ("pkg-config" ,pkg-config)
       ("sassc" ,sassc)))
    (synopsis "A flat GTK+ theme with transparent elements")
    (description "Arc is a flat theme with transparent elements for GTK 3, GTK
2, and GNOME Shell which supports GTK 3 and GTK 2 based desktop environments
like GNOME, Unity, Budgie, Pantheon, XFCE, Mate, etc.")
    (home-page "https://github.com/horst3180/arc-theme")
    ;; No "or later" language found.
    (license license:gpl3+)))

(define-public faba-icon-theme
  (package
    (name "faba-icon-theme")
    (version "4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/moka-project/"
                           name "/archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "18ln06xl60qzvzz61zq9q72hdbfgjsza3flph8i2asyzx3dffz68"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'disable-post-install
           (lambda _
             (substitute* "meson.build"
               (("meson.add_install_script.*") "")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (synopsis "Faba icon theme")
    (description
     "Faba is a minimal icon set used as a basis for other themes such as
Moka")
    (home-page "https://snwh.org/moka")
    (license (list license:lgpl3+
                   license:cc-by-sa4.0))))

(define-public moka-icon-theme
  (package
    (inherit faba-icon-theme)
    (name "moka-icon-theme")
    (version "5.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/moka-project"
                                  "/moka-icon-theme/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1nbwdjj268hxv9lfagd9aylily9f0hhallp841v0i3imljp84bmk"))))
    (propagated-inputs
     ;; Moka is based on Faba by using it as a fallback icon set instead of
     ;; bundling it, so we need to add it as a propagated input.
     `(("faba-icon-theme" ,faba-icon-theme)))
    (synopsis "Moka icon theme")
    (description "Moka is a stylized desktop icon set, designed to be clear,
simple and consistent.")
    (license (list license:gpl3+
                   license:cc-by-sa4.0))))

(define-public arc-icon-theme
  (package
    (name "arc-icon-theme")
    (version "20161122")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/horst3180/arc-icon-theme"
                                  "/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ya1cqvv8q847c0rpcg6apzky87q3h04y8jz5nmi52qk6kg8si0b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-configure-during-bootstrap
           (lambda _
             (substitute* "autogen.sh"
               (("^\"\\$srcdir/configure\".*") ""))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    ;; When Arc is missing an icon, it looks in the Moka icon theme for it.
    (propagated-inputs
     `(("moka-icon-theme" ,moka-icon-theme)))
    (synopsis "Arc icon theme")
    (description "The Arc icon theme provides a set of icons matching the
style of the Arc GTK theme.  Icons missing from the Arc theme are provided by
the Moka icon theme.")
    (home-page "https://github.com/horst3180/arc-icon-theme")
    (license license:gpl3+)))

(define-public folks
  (package
    (name "folks")
    (version "0.11.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1f51albxgfqxbax57i3pcgv2fx7i419xaapzdvldas6gw100ma1m"))))
    (build-system glib-or-gtk-build-system)
    (inputs
     `(("bdb" ,bdb)
       ("dbus-glib" ,dbus-glib)
       ("evolution-data-server" ,evolution-data-server)
       ("glib" ,glib)
       ("libgee" ,libgee)
       ("telepathy-glib" ,telepathy-glib)))
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (synopsis "Library to aggregate data about people")
    (description "Libfolks is a library that aggregates information about people
from multiple sources (e.g., Telepathy connection managers for IM contacts,
Evolution Data Server for local contacts, libsocialweb for web service contacts,
etc.) to create metacontacts.  It's written in Vala, which generates C code when
compiled.")
    (home-page "https://wiki.gnome.org/Projects/Folks")
    (license license:lgpl2.1+)))

(define-public gfbgraph
  (package
    (name "gfbgraph")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1dp0v8ia35fxs9yhnqpxj3ir5lh018jlbiwifjfn8ayy7h47j4fs"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:tests? #f ; Tests appear to require the network.
       ;; FIXME --enable-gtk-doc fails even with gtk-doc as a native-input.
       #:configure-flags '("--disable-gtk-doc"
                           "--disable-static"
                           "--enable-introspection")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("json-glib" ,json-glib)
       ("gnome-online-accounts" ,gnome-online-accounts)
       ("rest" ,rest)))
    (synopsis "GLib/GObject wrapper for the Facebook API")
    (description "This library allows you to use the Facebook API from
GLib/GObject code.")
    (home-page "https://wiki.gnome.org/Projects/GFBGraph")
    (license license:lgpl2.1+)))

(define-public libgnomekbd
  (package
    (name "libgnomekbd")
    (version "3.22.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1plkkack6s8b21gcmmly0lapgcjz53dmw2vixnn4rw4jxjwbdzaf"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")
       ("intltool" ,intltool)))
    (propagated-inputs
     ;; Referred to in .h files and .pc.
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libxklavier" ,libxklavier)))
    (home-page "https://www.gnome.org")
    (synopsis "GNOME keyboard configuration library")
    (description
     "Libgnomekbd is a keyboard configuration library for the GNOME desktop
environment, which can notably display keyboard layouts.")
    (license license:lgpl2.0+)))

;;; This package is no longer maintained:
;;; https://wiki.gnome.org/Attic/LibUnique
;;; "Unique is now in maintenance mode, and its usage is strongly discouraged.
;;; Applications should use the GtkApplication class provided by GTK+ 3.0."
(define-public libunique
  (package
    (name "libunique")
    (version "3.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0f70lkw66v9cj72q0iw1s2546r6bwwcd8idcm3621fg2fgh2rw58"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags '("--disable-static"
                           "--disable-dbus" ; use gdbus
                           "--enable-introspection")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gobject-introspection" ,gobject-introspection)
       ("glib:bin" ,glib "bin")
       ("gtk-doc" ,gtk-doc)))
    (propagated-inputs
     ;; Referred to in .h files and .pc.
     `(("gtk+" ,gtk+)))
    (home-page "https://wiki.gnome.org/Attic/LibUnique")
    (synopsis "Library for writing single instance applications")
    (description
     "Libunique is a library for writing single instance applications.  If you
launch a single instance application twice, the second instance will either just
quit or will send a message to the running instance.  Libunique makes it easy to
write this kind of application, by providing a base class, taking care of all
the IPC machinery needed to send messages to a running instance, and also
handling the startup notification side.")
    (license license:lgpl2.1+)))

(define-public gnome-calculator
  (package
    (name "gnome-calculator")
    (version "3.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1iv3b3mvqxm17r5gv15dyq6gz08w0ljhzgbf22xnnfnpzhvmn8b2"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-schemas, gio-2.0.
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtksourceview" ,gtksourceview)
       ("libsoup" ,libsoup)
       ("mpc" ,mpc)
       ("mpfr" ,mpfr)))
    (home-page "https://wiki.gnome.org/Apps/Calculator")
    (synopsis "Desktop calculator")
    (description
     "Calculator is an application that solves mathematical equations and
is suitable as a default application in a Desktop environment.")
    (license license:gpl3)))

(define-public xpad
  (package
    (name "xpad")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://launchpad.net/xpad/trunk/"
                           version "/+download/"
                           name "-" version ".tar.bz2"))
       (sha256
        (base32
         "0l0g5x8g6dwhf5ksnqqrjjsycy57kcvdslkmsr6bl3vrsjd7qml3"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gtk+:bin" ,gtk+ "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtksourceview" ,gtksourceview)
       ("libsm" ,libsm)))
    (home-page "https://wiki.gnome.org/Apps/Xpad")
    (synopsis "Virtual sticky note")
    (description
     "Xpad is a sticky note that strives to be simple, fault tolerant,
and customizable.  Xpad consists of independent pad windows, each is
basically a text box in which notes can be written.")
    (license license:gpl3+)))

(define-public gucharmap
  (package
    (name "gucharmap")
    (version "3.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0c1q9w5vql0vvg6g0knxfnv4ap19fg5cdrwndi1cj9lsym92c78j"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("glib:bin" ,glib "bin") ; for glib-compile-resources.
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+)
       ("xmllint" ,libxml2)))
    (home-page "https://wiki.gnome.org/Apps/Gucharmap")
    (synopsis "Unicode character picker and font browser")
    (description
     "This program allows you to browse through all the available Unicode
characters and categories for the installed fonts, and to examine their
detailed properties.  It is an easy way to find the character you might
only know by its Unicode name or code point.")
    (license license:gpl3+)))

(define-public bluefish
  (package
    (name "bluefish")
    (version "2.2.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.bennewitz.com/bluefish/stable/source/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "1jw4has7lbp77lqmzvnnjmqcf0lacjfnka873lkkwdyrpzc4c1q4"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("enchant" ,enchant)
       ("gtk+" ,gtk+)
       ("python" ,python-2)
       ("xmllint" ,libxml2)
       ("gucharmap" ,gucharmap)))
    (home-page "http://bluefish.openoffice.nl")
    (synopsis "Web development studio")
    (description
     "Bluefish is an editor targeted towards programmers and web developers,
with many options to write web sites, scripts and other code.
Bluefish supports many programming and markup languages.")
    (license license:gpl3+)))

(define-public gnome-system-monitor
  (package
    (name "gnome-system-monitor")
    (version "3.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1cz6s0cvagj422f9dc231nvg8jsfkva5s81skpq4q5jyrb1ahj7q"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-mkenums.
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("libgtop" ,libgtop)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gdk-pixbuf" ,gdk-pixbuf) ; for loading SVG files.
       ("gtk+" ,gtk+)
       ("gtkmm" ,gtkmm)
       ("librsvg" ,librsvg)
       ("libxml2" ,libxml2)))
    (home-page "https://wiki.gnome.org/Apps/SystemMonitor")
    (synopsis "Process viewer and system resource monitor for GNOME")
    (description
     "GNOME System Monitor is a GNOME process viewer and system monitor with
an attractive, easy-to-use interface.  It has features, such as a tree view
for process dependencies, icons for processes, the ability to hide processes,
graphical time histories of CPU/memory/swap usage and the ability to
kill/reinice processes.")
    (license license:gpl2+)))

(define-public python-pyatspi
  (package
    (name "python-pyatspi")
    (version "2.24.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/pyatspi/"
                    (version-major+minor version)
                    "/pyatspi-" version ".tar.xz"))
              (sha256
               (base32
                "14m6y27ziqc9f6339gjz49mlsk6mrsyg4bkj055cdzc7sfjlgvz7"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("python" ,python)
       ("python-pygobject" ,python-pygobject)))
    (synopsis "Python client bindings for D-Bus AT-SPI")
    (home-page "https://wiki.linuxfoundation.org/accessibility\
/atk/at-spi/at-spi_on_d-bus")
    (description
     "This package includes a python client library for the AT-SPI D-Bus
accessibility infrastructure.")
    (license license:lgpl2.0)
    (properties '((upstream-name . "pyatspi")))))

(define-public orca
  (package
    (name "orca")
    (version "3.26.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0xk5k9cbswymma60nrfj00dl97wypx59c107fb1hwi75gm0i07a7"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'qualify-xkbcomp
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xkbcomp (string-append
                             (assoc-ref inputs "xkbcomp") "/bin/xkbcomp")))
               (substitute* "src/orca/orca.py"
                 (("'xkbcomp'") (format #f "'~a'" xkbcomp))))
             #t))
         (add-after 'install 'wrap-orca
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (prog (string-append out "/bin/orca")))
               (wrap-program prog
                 `("GI_TYPELIB_PATH" ":" prefix
                   (,(getenv "GI_TYPELIB_PATH")))
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix
                   (,(getenv "GST_PLUGIN_SYSTEM_PATH")))
                 `("PYTHONPATH" ":" prefix
                   (,(getenv "PYTHONPATH")))))
             #t)))))
    (native-inputs
     `(("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("at-spi2-atk" ,at-spi2-atk)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-good" ,gst-plugins-good)
       ("gtk+" ,gtk+)
       ("python" ,python)
       ("python-pygobject" ,python-pygobject)
       ("python-pyatspi" ,python-pyatspi)
       ("python-speechd" ,speech-dispatcher)
       ("xkbcomp" ,xkbcomp)))
    (synopsis
     "Screen reader for individuals who are blind or visually impaired")
    (home-page "https://wiki.gnome.org/Projects/Orca")
    (description
     "Orca is a screen reader that provides access to the graphical desktop
via speech and refreshable braille.  Orca works with applications and toolkits
that support the Assistive Technology Service Provider Interface (AT-SPI).")
    (license license:lgpl2.1+)))

(define-public gspell
  (package
    (name "gspell")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1rdv873ixhwr15jwgc2z6k6y0hj353fqnwsy7zkh0c30qwiiv6l1"))
              (patches (search-patches "gspell-dash-test.patch"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Tests require a running X server.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")

             ;; For the missing /etc/machine-id.
             (setenv "DBUS_FATAL_WARNINGS" "0")

             ;; Allow Enchant and its Aspell backend to find the en_US
             ;; dictionary.
             (setenv "ASPELL_DICT_DIR"
                     (string-append (assoc-ref inputs "aspell-dict-en")
                                    "/lib/aspell"))
             #t)))))
    (inputs
     `(("gtk+" ,gtk+)
       ("glib" ,glib)
       ("iso-codes" ,iso-codes)))
    (native-inputs
     `(("glib" ,glib "bin")
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)

       ;; For tests.
       ("aspell-dict-en" ,aspell-dict-en)
       ("xorg-server" ,xorg-server)))
    (propagated-inputs
     `(("enchant" ,enchant)))            ;enchant.pc is required by gspell-1.pc
    (home-page "https://wiki.gnome.org/Projects/gspell")
    (synopsis "GNOME's alternative spell checker")
    (description
     "gspell provides a flexible API to add spell-checking to a GTK+
application.  It provides a GObject API, spell-checking to text entries and
text views, and buttons to choose the language.")
    (license license:gpl2+)))

(define-public gnome-planner
  (package
    (name "gnome-planner")
    (version "0.14.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/planner/"
                                  (version-major+minor version) "/planner-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "15h6ps58giy5r1g66sg1l4xzhjssl362mfny2x09khdqsvk2j38k"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     ;; Disable the Python bindings because the Planner program functions
     ;; without them, and (as of 2017-06-13) we have not packaged all of
     ;; packages that are necessary for building the Python bindings.
     `(#:configure-flags
       (list "--disable-python"
             ,@(if (string=? "aarch64-linux" (%current-system))
                   '("--build=aarch64-unknown-linux-gnu")
                   '()))))
    (inputs
     `(("libgnomecanvas" ,libgnomecanvas)
       ("libgnomeui" ,libgnomeui)
       ("libglade" ,libglade)
       ("gnome-vfs" ,gnome-vfs)
       ("gconf" ,gconf)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("gtk+" ,gtk+)
       ("glib" ,glib)))
    (native-inputs
     `(("intltool" ,intltool)
       ("scrollkeeper" ,scrollkeeper)
       ("pkg-config" ,pkg-config)))
    (home-page "https://wiki.gnome.org/Apps/Planner")
    (synopsis "Project management software for the GNOME desktop")
    (description
     "GNOME Planner is a project management tool based on the Work Breakdown
Structure (WBS).  Its goal is to enable you to easily plan projects.  Based on
the resources, tasks, and constraints that you define, Planner generates
various views into a project.  For example, Planner can show a Gantt chart of
the project.  It can show a detailed summary of tasks including their
duration, cost, and current progress.  It can also show a report of resource
utilization that highlights under-utilized and over-utilized resources.  These
views can be printed as PDF or PostScript files, or exported to HTML.")
    (license license:gpl2+)))

(define-public lollypop
  (package
    (name "lollypop")
    (version "0.9.521")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gitlab.gnome.org/World/lollypop/uploads/"
                           "e4df2ed75c5ed71d64afcc668e579b2a/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0knsqh24siyw98vmiq6b1hzq4y4cazs9f1hq1js9c96hqqj9rvdx"))))
    (build-system meson-build-system)
    (arguments
     `(#:imported-modules ((guix build python-build-system)
                           ,@%meson-build-system-modules)
       #:glib-or-gtk? #t
       #:tests? #f ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH")))
               (wrap-program (string-append out "/bin/lollypop")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
             #t))
         (add-after 'install 'wrap
           (@@ (guix build python-build-system) wrap)))))
    (native-inputs
     `(("intltool" ,intltool)
       ("itstool" ,itstool)
       ("glib:bin" ,glib "bin")         ; For glib-compile-resources
       ("gtk+:bin" ,gtk+ "bin")         ; For gtk-update-icon-cache
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("gst-plugins-base" ,gst-plugins-base)
       ("libnotify" ,libnotify)
       ("libsecret" ,libsecret)
       ("libsoup" ,libsoup)
       ("python" ,python)
       ("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-gst" ,python-gst)
       ("python-pycairo" ,python-pycairo)
       ("python-pygobject" ,python-pygobject)
       ("python-pylast" ,python-pylast)
       ("totem-pl-parser" ,totem-pl-parser)
       ("webkitgtk" ,webkitgtk)))
    (propagated-inputs
     `(;; gst-plugins-base is required to start Lollypop,
       ;; the others are required to play streaming.
       ("gst-plugins-good" ,gst-plugins-good)
       ("gst-plugins-ugly" ,gst-plugins-ugly)))
    (home-page "https://wiki.gnome.org/Apps/Lollypop")
    (synopsis "GNOME music playing application")
    (description
     "Lollypop is a music player designed to play well with GNOME desktop.
Lollypop plays audio formats such as mp3, mp4, ogg and flac and gets information
from artists and tracks from the web.  It also fetches cover artworks
automatically and it can stream songs from online music services and charts.")
    (license license:gpl3+)))

(define-public gnome-video-effects
  (package
    (name "gnome-video-effects")
    (version "0.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "06c2f1kihyhawap1s3zg5w7q7fypsybkp7xry4hxkdz4mpsy0zjs"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:out-of-source? #f))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("intltool" ,intltool)
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (home-page "https://wiki.gnome.org/Projects/GnomeVideoEffects")
    (synopsis "Video effects for Cheese and other GNOME applications")
    (description
     "A collection of GStreamer video filters and effects to be used in
photo-booth-like software, such as Cheese.")
    (license license:gpl2+)))

(define-public cheese
  (package
    (name "cheese")
    (version "3.28.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "06da5qc5hdvwwd5vkbgbx8pjx1l3mvr07yrnnv3v1hfc3wp7l7jw"))))
    (arguments
     ;; Tests require GDK.
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'skip-gtk-update-icon-cache
           (lambda _
             ;; Don't create 'icon-theme.cache'
             (substitute* "Makefile"
               (("gtk-update-icon-cache") (which "true")))
             #t))
         (add-after 'install 'wrap-cheese
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out             (assoc-ref outputs "out"))
                   (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH")))
               (wrap-program (string-append out "/bin/cheese")
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))))
             #t)))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("libxml2" ,libxml2)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (propagated-inputs
     `(("gnome-video-effects" ,gnome-video-effects)))
    (inputs
     `(("clutter" ,clutter)
       ("clutter-gst" ,clutter-gst)
       ("clutter-gtk" ,clutter-gtk)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gnome-desktop" ,gnome-desktop)
       ("gobject-introspection" ,gobject-introspection)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-bad" ,gst-plugins-bad)
       ("gtk+" ,gtk+)
       ("libcanberra" ,libcanberra)
       ("libx11" ,libx11)
       ("libxtst" ,libxtst)))
    (home-page "https://wiki.gnome.org/Apps/Cheese")
    (synopsis "Webcam photo booth software for GNOME")
    (description
     "Cheese uses your webcam to take photos and videos.  Cheese can also
apply fancy special effects and lets you share the fun with others.")
    (license license:gpl2+)))

(define-public sound-juicer
  (package
    (name "sound-juicer")
    (version "3.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "19qg4xv0f9rkq34lragkmhii1llxsa87llbl28i759b0ks4f6sny"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("gtk+" ,gtk+)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-good" ,gst-plugins-good)
       ("iso-codes" ,iso-codes)
       ("libbrasero-media3" ,brasero)
       ("libcanberra" ,libcanberra)
       ("libdiscid" ,libdiscid)
       ("libmusicbrainz" ,libmusicbrainz)
       ("neon" ,neon)))
    (home-page "https://wiki.gnome.org/Apps/SoundJuicer")
    (synopsis "Audio music cd ripper")
    (description "Sound Juicer extracts audio from compact discs and convert it
into audio files that a personal computer or digital audio player can play.
It supports ripping to any audio codec supported by a GStreamer plugin, such as
mp3, Ogg Vorbis and FLAC")
    (license license:gpl2+)))

(define-public soundconverter
  (package
    (name "soundconverter")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://launchpad.net/soundconverter/trunk/"
                           version "/+download/"
                           "soundconverter-" version ".tar.xz"))

       (sha256
        (base32 "1d6x1yf8psqbd9zbybxivfqg55khcnngp2mn92l161dfdk9512c5"))
       (patches
        (search-patches
         "soundconverter-remove-gconf-dependency.patch"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:imported-modules ((guix build python-build-system)
                           (guix build glib-or-gtk-build-system)
                           ,@%gnu-build-system-modules)

       #:modules ((guix build glib-or-gtk-build-system)
                  (guix build utils)
                  ((guix build gnu-build-system) #:prefix gnu:)
                  ((guix build python-build-system) #:prefix python:))

       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-soundconverter-for-python
           (assoc-ref python:%standard-phases 'wrap))
         (add-after 'install 'wrap-soundconverter
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                   (gst-plugin-path   (getenv "GST_PLUGIN_SYSTEM_PATH")))
               (wrap-program (string-append out "/bin/soundconverter")
                 `("GI_TYPELIB_PATH"        ":" prefix (,gi-typelib-path))
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))))
             #t)))))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("glib:bin" ,glib "bin")))
    (inputs
     `(("gtk+" ,gtk+)
       ("python" ,python)
       ("python-pygobject" ,python-pygobject)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)))
    (home-page "http://soundconverter.org/")
    (synopsis "Convert between audio formats with a graphical interface")
    (description
     "SoundConverter supports converting between many audio formats including
Opus, Ogg Vorbis, FLAC and more.  It supports parallel conversion, and
configurable file renaming. ")
    (license license:gpl3)))

(define-public workrave
  (package
    (name "workrave")
    (version "1.10.23")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rcaelers/workrave.git")
             (commit (string-append "v" (string-map
                                         (match-lambda (#\. #\_) (chr chr))
                                         version)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qhlwfhwk5agv4904d6bsf83k9k89q7bms6agg967vsca4905vcw"))))
    (build-system glib-or-gtk-build-system)
    (propagated-inputs `(("glib" ,glib)
                         ("gtk+" ,gtk+)
                         ("gdk-pixbuf" ,gdk-pixbuf)
                         ("gtkmm" ,gtkmm)
                         ("glibmm" ,glibmm)
                         ("libx11" ,libx11)
                         ("libxtst" ,libxtst)
                         ("dconf" ,dconf)
                         ("libice" ,libice)))
    (inputs `(("libsm" ,libsm)
              ("python-cheetah" ,python2-cheetah)))
    (native-inputs `(("glib" ,glib "bin")
                     ("pkg-config" ,pkg-config)
                     ("gettext" ,gnu-gettext)
                     ("autoconf" ,autoconf)
                     ("autoconf-archive" , autoconf-archive)
                     ("automake" ,automake)
                     ("libtool" ,libtool)
                     ("intltool" ,intltool)
                     ("libxscrnsaver" ,libxscrnsaver)
                     ("gobject-introspection" ,gobject-introspection)
                     ("python2" ,python-2)))
    (synopsis "Tool to help prevent repetitive strain injury (RSI)")
    (description
     "Workrave is a program that assists in the recovery and prevention of
repetitive strain injury (@dfn{RSI}).  The program frequently alerts you to take
micro-pauses and rest breaks, and restricts you to your daily limit.")
    (home-page "http://www.workrave.org")
    (license license:gpl3+)))

(define-public ghex
  (package
    (name "ghex")
    (version "3.18.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/ghex/"
                                  (version-major+minor version) "/"
                                  "ghex-" version ".tar.xz"))
              (sha256
               (base32
                "1lq8920ad2chi9ibmyq0x9hg9yk63b0kdbzid03w42cwdzw50x66"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gnome-common" ,gnome-common)
       ("which" ,which)
       ("intltool" ,intltool)
       ("yelp-tools" ,yelp-tools)
       ("desktop-file-utils" ,desktop-file-utils))) ; for 'desktop-file-validate'
    (inputs
     `(("atk" ,atk)
       ("gtk" ,gtk+)))
    (synopsis "GNOME hexadecimal editor")
    (description "The GHex program can view and edit files in two ways:
hexadecimal or ASCII.  It is useful for editing binary files in general.")
    (home-page "https://wiki.gnome.org/Apps/Ghex")
    (license license:gpl2)))

(define-public libdazzle
  (package
    (name "libdazzle")
    (version "3.28.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libdazzle/"
                                  (version-major+minor version) "/"
                                  "libdazzle-" version ".tar.xz"))
              (sha256
               (base32
                "08qdwv2flywnh6kibkyv0pnm67pk8xlmjh4yqx6hf13hyhkxkqgg"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-test
           (lambda _
             ;; Disable failing test.
             (substitute* "tests/meson.build"
               (("test\\('test-application") "#"))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")
             #t)))))
    (native-inputs
     `(("glib" ,glib "bin")             ; glib-compile-resources
       ("pkg-config" ,pkg-config)
       ;; For tests.
       ("xorg-server" ,xorg-server)))
    (inputs
     `(("glib" ,glib)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+" ,gtk+)
       ("vala" ,vala)))
    (home-page "https://gitlab.gnome.org/GNOME/libdazzle")
    (synopsis "Companion library to GObject and Gtk+")
    (description "The libdazzle library is a companion library to GObject and
Gtk+.  It provides various features that the authors wish were in the
underlying library but cannot for various reasons.  In most cases, they are
wildly out of scope for those libraries.  In other cases, they are not quite
generic enough to work for everyone.")
    (license license:gpl3+)))
