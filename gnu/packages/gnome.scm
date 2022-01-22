;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2014, 2016, 2020 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2014, 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015, 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2015, 2017 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017, 2018, 2021 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2018 Rene Saavedra <pacoon@protonmail.com>
;;; Copyright © 2016 Jochem Raat <jchmrt@riseup.net>
;;; Copyright © 2016, 2017, 2019 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017, 2018 nee <nee-git@hidamari.blue>
;;; Copyright © 2017 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017 Mohammed Sadiq <sadiq@sadiqpk.org>
;;; Copyright © 2017, 2020, 2021 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2017, 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Jovany Leandro G.C <bit4bit@riseup.net>
;;; Copyright © 2018 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2018 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2018, 2019 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2019, 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2019 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2019 Jonathan Frederickson <jonathan@terracrypt.net>
;;; Copyright © 2019, 2020, 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019, 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2019 David Wilson <david@daviwil.com>
;;; Copyright © 2019, 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2019, 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2019, 2020, 2021 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 raingloom <raingloom@riseup.net>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Naga Malleswari <nagamalli@riseup.net>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020, 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020, 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2020, 2021 Andy Tai <atai@atai.org>
;;; Copyright © 2020, 2021 Sébastien Lerique <sl@eauchat.org>
;;; Copyright © 2021 Trevor Hass <thass@okstate.edu>
;;; Copyright © 2021 Solene Rapenne <solene@perso.pw>
;;; Copyright © 2021, 2022 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Josselin Poiret <josselin.poiret@protonmail.ch>
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2022 Daniel Meißner <daniel.meissner-i4k@ruhr-uni-bochum.de>
;;; Copyright © 2022 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
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
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-gtk)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lirc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages man)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages node)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdesktop)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages search)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages slang)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu artwork)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public gupnp-igd
  (package
    (name "gupnp-igd")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1q9bw12ibih3yxpha3gm1dabyqg9gx6yxacbh4kxsgm1i84j0lab"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and compile schemas
       #:configure-flags (list "-Dgtk_doc=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* "gupnp-igd-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml-4.1.2")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-before 'check 'set-home
           (lambda _
             ;; A test using GIO expects ~/.config/glib-2.0/settings to be
             ;; writable.
             (setenv "HOME" (getcwd))))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/gtk-doc")
                (string-append doc "/share/gtk-doc"))
               #t))))))
    (native-inputs
     `(("docbook-xml-4.1.2" ,docbook-xml-4.1.2)
       ("docbook-xsl" ,docbook-xsl)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk-doc" ,gtk-doc/stable)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     (list glib glib-networking gssdp gupnp libsoup))
    (synopsis "UPnP IGD for GNOME")
    (description "GUPnP-IGD is a library to handle UPnP IGD port mapping.")
    (home-page "https://gitlab.gnome.org/GNOME/gupnp-igd")
    (license license:lgpl2.1+)))

(define-public brasero
  (package
    (name "brasero")
    (version "3.12.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/brasero/"
                                 (version-major+minor version) "/"
                                 "brasero-" version ".tar.xz"))
             (sha256
              (base32
               "05gabybkl7xfinwx97i4scp9hic0dlxj7gh03dyj0hd16fp9wx47"))))
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
         (add-before 'configure 'embed-growisofs-reference
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((dvd+rw-tools (assoc-ref inputs "dvd+rw-tools")))
               (substitute* "plugins/growisofs/burn-growisofs.c"
                 (("(\")(growisofs)" _ prefix command)
                  (string-append prefix dvd+rw-tools "/bin/" command)))))))))
    (propagated-inputs
     (list hicolor-icon-theme))
    (native-inputs
     (list intltool
           itstool
           `(,glib "bin") ; glib-compile-schemas, etc.
           gobject-introspection
           pkg-config))
    (inputs
     (list dvd+rw-tools
           glib
           gnome-doc-utils
           gstreamer
           gst-plugins-base
           gtk+
           libcanberra
           libice
           libnotify
           libsm
           libxml2
           totem-pl-parser))
    (home-page "https://wiki.gnome.org/Apps/Brasero")
    (synopsis "CD/DVD burning tool for Gnome")
    (description "Brasero is an application to burn CD/DVD for the Gnome
Desktop.  It is designed to be as simple as possible and has some unique
features to enable users to create their discs easily and quickly.")
    (license license:gpl2+)))

;;; Minimal variant, used to break a cycle with Inkscape.
(define-public libcloudproviders-minimal
  (package
    (name "libcloudproviders-minimal")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/libcloudproviders/"
                       (version-major+minor version)
                       "/libcloudproviders-" version ".tar.xz"))
       (sha256
        (base32 "0aars24myf6n8b8hm1n12hsgcm54097kpbpm4ba31zp1l4y22qs7"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t             ; To wrap binaries and/or compile schemas
       #:configure-flags (list "-Dintrospection=false"
                               "-Denable-gtk-doc=false"
                               "-Dvapigen=false")))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     (list glib glib-networking))
    (synopsis "Cloudproviders Integration API")
    (description "Libcloudproviders is a DBus API that allows cloud storage sync
clients to expose their services.  Clients such as file managers and desktop
environments can then provide integrated access to the cloud providers
services.")
    (home-page "https://csorianognome.wordpress.com/2015/07/07/cloud-providers/")
    (license license:lgpl3+)))

(define-public libcloudproviders
  (package/inherit libcloudproviders-minimal
    (name "libcloudproviders")
    (version "0.3.1")
    (outputs (cons "doc" (package-outputs libcloudproviders-minimal)))
    (arguments
     (substitute-keyword-arguments (package-arguments libcloudproviders-minimal)
       ((#:configure-flags _)
        '(list "-Denable-gtk-doc=true")) ;false by default
       ((#:phases phases '%standard-phases)
        `(modify-phases %standard-phases
           (add-after 'install 'move-doc
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (doc (assoc-ref outputs "doc")))
                 (mkdir-p (string-append doc "/share"))
                 (rename-file
                  (string-append out "/share/gtk-doc")
                  (string-append doc "/share/gtk-doc")))))))))
    (native-inputs
     (append
         `(("gobject-introspection" ,gobject-introspection)
           ("gtk-doc" ,gtk-doc/stable))
         (package-native-inputs libcloudproviders-minimal)))))

(define-public libgrss
  (package
    (name "libgrss")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1nalslgyglvhpva3px06fj6lv5zgfg0qmj0sbxyyl5d963vc02b7"))
       (patches
        (search-patches "libgrss-CVE-2016-2001.patch"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--enable-gtk-doc"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc/reference"
               (substitute* "libgrss-docs.sgml"
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     (list docbook-xml-4.1.2 gobject-introspection gtk-doc/stable
           pkg-config))
    (propagated-inputs
     (list glib libsoup libxml2))
    (synopsis "Glib library for feeds")
    (description "LibGRSS is a Glib abstraction to handle feeds in RSS, Atom,
and other formats.")
    (home-page "https://wiki.gnome.org/Projects/Libgrss")
    (license license:lgpl3+)))

(define-public gnome-js-common
  (package
    (name "gnome-js-common")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.gz"))
       (sha256
        (base32 "1zv5b9bcbclzj64xd9kgql4ndmbwvvi6cl937ykw8fp21xgh8z7y"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--disable-static")))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (synopsis "Common JS Modules")
    (description "GNOME-JS-Common provides common modules for GNOME JavaScript
bindings.")
    (home-page "https://wiki.gnome.org/Projects/Seed")
    (license license:gpl3+)))

(define-public seed
  (package
    (name "seed")
    (version "3.8.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0cmcxaggcdcy13j27gy8id2qsf2p2sl4bz2mwb9zhv3gzavlvjw0"))
       (patches
        (search-patches "seed-webkit.patch"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-xorg-module"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html")
        "--with-webkit=4.0")
       #:phases
       (modify-phases %standard-phases
         ;; The seed-webkit.patch patches configure.ac.
         ;; So the source files need to be re-bootstrapped.
         (add-after 'unpack 'trigger-bootstrap
           (lambda _
             (for-each delete-file
                       (list
                        "configure"
                        "Makefile.in"))
             #t))
         (add-after 'unpack 'patch-tests
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* (find-files "." "\\.js$")
              (("#!/usr/bin/env seed")
               (string-append "#!" (getcwd) "/src/seed")))
             #t))
         (add-before 'build 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* '("reference/seed-docs.sgml" "modules/book.xml")
                 (("http://www.oasis-open.org/docbook/xml/4.1.2/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("docbook-xml" ,docbook-xml-4.1.2)
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc/stable)
       ("intltool" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("cairo" ,cairo)
       ("dbus" ,dbus)
       ("dbus-glib" ,dbus-glib)
       ("gnome-js-common" ,gnome-js-common)
       ("gtk+" ,gtk+)
       ("gtk+-2" ,gtk+-2)
       ("libffi" ,libffi)
       ("libxml2" ,libxml2)
       ("mpfr" ,mpfr)
       ("readline" ,readline)
       ("sqlite" ,sqlite)
       ("xscrnsaver" ,libxscrnsaver)))
    (propagated-inputs
     `(("glib" ,glib)
       ("webkit" ,webkitgtk)))
    (synopsis "GObject JavaScriptCore bridge")
    (description "Seed is a library and interpreter, dynamically bridging
(through GObjectIntrospection) the WebKit JavaScriptCore engine, with the
GNOME platform.  It serves as something which enables you to write standalone
applications in JavaScript, or easily enable your application to be extensible
in JavaScript.")
    (home-page "https://wiki.gnome.org/Projects/Seed")
    (license license:lgpl2.0+)))

(define-public libdmapsharing
  (package
    (name "libdmapsharing")
    (version "3.9.10")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.flyn.org/projects/"
                       name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "152hnddwxv590cn802awv3mn27ixc3s6ac691a7z02d1c5fl45p2"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:tests? #f                      ; Tests require networking.
       #:configure-flags
       (list
        "--disable-static"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* "libdmapsharing-4.0-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     (list check
           docbook-xml-4.3
           gobject-introspection
           pedansee
           pkg-config
           vala))
    (inputs
     `(("avahi" ,avahi)
       ("librsvg" ,librsvg)
       ("gee" ,libgee)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gtk+" ,gtk+)))
    (propagated-inputs
     (list glib glib-networking gstreamer libsoup-minimal-2))
    (synopsis "Media management library")
    (description "Libdmapsharing is a library which allows programs to access,
share and control the playback of media content using DMAP (DAAP, DPAP & DACP).
It is written in C using GObject and libsoup.")
    (home-page "https://launchpad.net/gtx")
    (license license:lgpl2.1+)))

(define-public gtx
  (package
    (name "gtx")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://launchpad.net/gtx/trunk/"
                       version "/+download/gtx-" version ".tar.gz"))
       (sha256
        (base32 "0i4zvn5v4rf0cw3fxylk6j2pyy5lkrswdiw8jdxkys0ph0nan33n"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-gtk-doc"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))))
    (native-inputs
     (list gobject-introspection gtk-doc/stable pkg-config))
    (propagated-inputs
     (list glib))
    (synopsis "GLib Testing Framework")
    (description "GTX is a small collection of convenience functions intended to
enhance the GLib testing framework.  With specific emphasis on easing the pain
of writing test cases for asynchronous interactions.")
    (home-page "https://launchpad.net/gtx")
    (license license:lgpl2.1+)))

(define-public dee
  (package
    (name "dee")
    (version "1.2.7")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://launchpad.net/dee/1.0/"
                       version "/+download/dee-" version ".tar.gz"))
       (sha256
        (base32 "12mzffk0lyd566y46x57jlvb9af152b4dqpasr40zal4wrn37w0v"))
       (patches
        (search-patches "dee-vapi.patch"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-maintainer-flags"
        (string-append "--with-pygi-overrides-dir="
                       (assoc-ref %outputs "out")
                       "/lib/python"
                       ,(version-major+minor
                         (package-version python))
                       "/site-packages/gi/overrides")
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc/reference/dee-1.0"
               (substitute* "dee-1.0-docs.sgml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-after 'patch-docbook-xml 'disable-failing-tests
           (lambda _
             (substitute* "tests/test-icu.c"
               (("g_test_add \\(DOMAIN\"/Default/AsciiFolder\", Fixture, 0,")
                "")
               (("setup, test_ascii_folder, teardown\\);")
                ""))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running dbus-daemon.
             (system "dbus-daemon &")
             ;; For missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             #t)))))
    (native-inputs
     `(("dbus" ,dbus)
       ("dbus-test-runner" ,dbus-test-runner)
       ("docbook-xml" ,docbook-xml-4.3)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc/stable)
       ;; Would only be required by configure flag "--enable-extended-tests".
       ;("gtx" ,gtx)
       ("pkg-config" ,pkg-config)
       ("pygobject" ,python-pygobject)
       ("python" ,python-wrapper)
       ("vala" ,vala)))
    (inputs
     `(("icu" ,icu4c)))
    (propagated-inputs
     (list glib))
    (synopsis "Model to synchronize multiple instances over DBus")
    (description "Dee is a library that uses DBus to provide objects allowing
you to create Model-View-Controller type programs across DBus.  It also consists
of utility objects which extend DBus allowing for peer-to-peer discoverability
of known objects without needing a central registrar.")
    (home-page "https://launchpad.net/dee")
    (license
     ;; Dual-licensed
     (list
      license:lgpl3+
      license:gpl3+))))

(define-public zeitgeist
  (package
    (name "zeitgeist")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://gitlab.freedesktop.org/zeitgeist/zeitgeist.git")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "0y6fyzxl5np4yskcxibd0p03h619w9ir907nhf40h02y0pk1kgkp"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--enable-explain-queries"
        "--enable-fts"
        "--enable-docs")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc/libzeitgeist"
               (substitute* "zeitgeist-gtkdoc-index.sgml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t))
         (add-after 'patch-docbook-xml 'disable-failing-tests
           (lambda _
             (substitute* "test/direct/Makefile.am"
               (("	log-test ")
                ""))
             (substitute* "test/c/Makefile.am"
               (("	test-log ")
                ""))
             #t))
         (add-before 'bootstrap 'remove-autogen-script
           (lambda _
             ;; To honor `autoreconf -vif` by build-system.
             (delete-file "autogen.sh")
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("docbook-xml" ,docbook-xml-4.3)
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc/stable)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xorg-server-for-tests" ,xorg-server-for-tests)))
    (inputs
     `(("dee-icu" ,dee)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("sqlite" ,sqlite)
       ("telepathy-glib" ,telepathy-glib)
       ("python" ,python-wrapper)
       ("python-rdflib" ,python-rdflib)
       ("xapian-config" ,xapian)))
    (propagated-inputs
     (list glib))
    (synopsis "Desktop Activity Logging")
    (description "Zeitgeist is a service which logs the users’s activities and
events, anywhere from files opened to websites visited and conversations.  It
makes this information readily available for other applications to use.  It is
able to establish relationships between items based on similarity and usage
patterns.")
    (home-page "https://zeitgeist.freedesktop.org/")
    (license
     ;; Dual-licensed
     (list
      license:lgpl2.1+
      license:gpl2+))))

(define-public gnome-recipes
  (package
    (name "gnome-recipes")
    (version "2.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/GNOME/recipes")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h049mzqnlcfqwrhmzbq3pzzdglvy2bn9fj1p8wql7a60pn8sr32"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-gtk-update-icon-cache
            (lambda _
              (substitute* "meson_post_install.py"
                (("gtk-update-icon-cache") (which "true")))))
          (add-after 'unpack 'unpack-libgd
            (lambda _
              (copy-recursively
               #$(origin
                   (method git-fetch)
                   (uri (git-reference
                         (url "https://gitlab.gnome.org/GNOME/libgd")
                         (commit "c7c7ff4e05d3fe82854219091cf116cce6b19de0")))
                   (file-name (git-file-name "libgd" version))
                   (sha256
                    (base32
                     "16yld0ap7qj1n96h4f2sqkjmibg7xx5xwkqxdfzam2nmyfdlrrrs")))
               "subprojects/libgd"))))))
    (inputs (list glib
                  gnome-autoar
                  `(,gnome-online-accounts "lib")
                  gspell
                  gtk+
                  json-glib
                  libcanberra
                  libsoup
                  rest))
    (native-inputs (list desktop-file-utils ;for update-desktop-database
                         gettext-minimal
                         `(,glib "bin")
                         itstool
                         pkg-config
                         python))
    (home-page "https://wiki.gnome.org/Apps/Recipes")
    (synopsis "Discover recipes for preparing food")
    (description "GNOME Recipes helps you discover what to cook today,
tomorrow, the rest of the week and for special occasions.")
    (license license:gpl3+)))

(define-public gnome-photos
  (package
    (name "gnome-photos")
    (version "40.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32
         "1bzi79plw6ji6qlckhxnwfnswy6jpnhzmmyanml2i2xg73hp6bg0"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       (list "-Ddogtail=false"     ; Not available
             ;; Required for RUNPATH validation.
             (string-append "-Dc_link_args=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib/gnome-photos"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-gnome-photos
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let*
                 ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/gnome-photos")
                 `("GRL_PLUGIN_PATH" =
                   (,(getenv "GRL_PLUGIN_PATH"))))))))))
    (native-inputs
     `(("dbus" ,dbus)
       ("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("git" ,git-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+:bin" ,gtk+ "bin")
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("babl" ,babl)
       ("cairo" ,cairo)
       ("librsvg" ,librsvg)
       ("gegl" ,gegl)
       ("geocode-glib" ,geocode-glib)
       ("gexiv2" ,gexiv2)
       ("gnome-online-accounts" ,gnome-online-accounts)
       ("gnome-online-miners" ,gnome-online-miners)
       ("grilo" ,grilo)
       ("grilo-plugins" ,grilo-plugins)
       ("gtk+" ,gtk+)
       ("libdazzle" ,libdazzle)
       ("libgdata" ,libgdata)
       ("libgfbgraph" ,gfbgraph)
       ("libhandy" ,libhandy)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("librest" ,rest)
       ("pygobject" ,python-pygobject)
       ("tracker" ,tracker)
       ("tracker-miners" ,tracker-miners)))
    (synopsis "Access, organize and share your photos on GNOME desktop")
    (description "GNOME Photos is a simple and elegant replacement for using a
file manager to deal with photos.  Enhance, crop and edit in a snap.  Seamless
cloud integration is offered through GNOME Online Accounts.")
    (home-page "https://wiki.gnome.org/Apps/Photos")
    (license license:gpl3+)))

(define-public gnome-music
  (package
    (name "gnome-music")
    (version "40.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32
         "0lcdal4qdhclr8961p57xf010y92l6wwmkw86lyi9wy224z6gjr0"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-gnome-music
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let*
                 ((out (assoc-ref outputs "out"))
                  (pylib (string-append out "/lib/python"
                                        ,(version-major+minor
                                          (package-version python))
                                        "/site-packages")))
               (wrap-program (string-append out "/bin/gnome-music")
                 `("GI_TYPELIB_PATH" =
                   (,(getenv "GI_TYPELIB_PATH")))
                 `("GST_PLUGIN_SYSTEM_PATH" =
                   (,(getenv "GST_PLUGIN_SYSTEM_PATH")))
                 `("GRL_PLUGIN_PATH" =
                   (,(getenv "GRL_PLUGIN_PATH")))
                 `("GUIX_PYTHONPATH" =
                   (,(getenv "GUIX_PYTHONPATH") ,pylib)))))))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("itstools" ,itstool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("grilo" ,grilo)
       ("grilo-plugins" ,grilo-plugins)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-good" ,gst-plugins-good)
       ("gstreamer" ,gstreamer)
       ("gvfs" ,gvfs)
       ("json-glib" ,json-glib)
       ("libdazzle" ,libdazzle)
       ("libmediaart" ,libmediaart)
       ("libsoup" ,libsoup-minimal-2)
       ("pycairo" ,python-pycairo)
       ("pygobject" ,python-pygobject)
       ("python" ,python)
       ("tracker" ,tracker)
       ("tracker-miners" ,tracker-miners)))
    (synopsis "Simple music player for GNOME desktop")
    (description "GNOME Music is the new GNOME music playing application that
aims to combine an elegant and immersive browsing experience with simple
and straightforward controls.")
    (home-page "https://wiki.gnome.org/Apps/Music")
    (license license:gpl2+)))

(define-public portablexdr
  (package
    (name "portablexdr")
    (version "4.9.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://people.redhat.com/~rjones/" name "/files/"
                       name "-" version ".tar.gz"))
       (sha256
        (base32
         "0b77ipvvi520nv7rr6jb1c3xryhc3m2mywhby7m48kfgag8vvx2w"))))
    (build-system gnu-build-system)
    (synopsis "External Data Representation Library")
    (description "PortableXDR is an implementation of External Data
Representation (XDR) Library.  It is a standard data serialization format, for
uses such as computer network protocols.  It allows data to be transferred
between different kinds of computer systems.")
    (home-page "https://people.redhat.com/~rjones/portablexdr/")
    (license
     (list
      license:gpl2+
      license:lgpl2.1+))))

(define-public tepl
  (package
    (name "tepl")
    (version "6.00.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32
         "0qvs7s86gqyyrzi0r5fbrj8zczlgv8xhdjswgbgc1afwjnl9fqx8"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:tests? #f))                    ; FIX-ME: Requires gvfs
    (native-inputs
     (list `(,glib "bin") gobject-introspection pkg-config))
    (inputs
     (list amtk
           glib
           gtk+
           gtksourceview
           libxml2
           uchardet))
    (synopsis "Text editor product line")
    (description "Tepl is a library that eases the development of
GtkSourceView-based text editors and IDEs.")
    (home-page "https://wiki.gnome.org/Projects/Tepl")
    (license license:lgpl2.1+)))

(define-public tepl-5
  (package
    (inherit tepl)
    (name "tepl")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32
         "0118j64s4fb350phbgda2rk6sv19rfgglxc7nf248llzc1klj9xi"))))))

(define-public krb5-auth-dialog
  (package
    (name "krb5-auth-dialog")
    (version "3.26.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32
         "1w91gsvmifqhiam3xqf88i5rk2w6qadjalmbvvamjdc37j0vdc6x"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list intltool itstool libxml2 pkg-config python-wrapper))
    (inputs
     (list glib gtk+ libnotify mit-krb5 network-manager))
    (synopsis "Popup dialogs for Kerberos 5")
    (description "krb5-auth-dialog is a simple dialog that monitors Kerberos
tickets, and pops up a dialog when they are about to expire.")
    (home-page "https://gitlab.gnome.org/GNOME/krb5-auth-dialog")
    (license license:gpl2+)))

(define-public notification-daemon
  (package
    (name "notification-daemon")
    (version "3.20.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32
         "1rgchqi4j2ll7d6a7lgy7id0w9rrkwkgic1096fbm2zx6n7pc4yx"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list glib gtk+ libx11))
    (synopsis "Notification Daemon for GNOME Desktop")
    (description "Notification-Daemon is the server implementation of the
freedesktop.org desktop notification specification.")
    (home-page "https://wiki.gnome.org/Projects/NotificationDaemon")
    (license license:gpl2+)))

(define-public mm-common
  (package
    (name "mm-common")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/mm-common/"
                                  (version-major+minor version) "/"
                                  "mm-common-" version ".tar.xz"))
              (sha256
               (base32
                "1x8yvjy0yg17qyhmqws8xh2k8dvzrhpwqz7j1cfwzalrb1i9c5g8"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "util/mm-common-prepare.in"
               (("ln") (search-input-file inputs "/bin/ln"))
               (("cp") (search-input-file inputs "/bin/cp"))
               (("sed") (search-input-file inputs "/bin/sed"))
               (("cat") (search-input-file inputs "/bin/cat"))))))))
    (native-inputs
     `(("coreutils" ,coreutils)
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("sed" ,sed)))
    (inputs
     (list python))
    (synopsis "Module of GNOME C++ bindings")
    (description "The mm-common module provides the build infrastructure
and utilities shared among the GNOME C++ binding libraries.  Release
archives of mm-common include the Doxygen tag file for the GNU C++
Library reference documentation.")
    (home-page "https://gitlab.gnome.org/GNOME/mm-common")
    (license license:gpl2+)))

(define-public phodav
  (package
   (name "phodav")
   (version "2.5")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "045rdzf8isqmzix12lkz6z073b5qvcqq6ad028advm5gf36skw3i"))))
   (build-system meson-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'fix-udev-rules-directory
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out   (assoc-ref outputs "out"))
                   (rules (string-append out "/lib/udev/rules.d")))
              (substitute* "data/meson.build"
                (("udev\\.get_pkgconfig_variable\\('udevdir'\\)")
                 (format #f "'~a'" rules))))))
        (add-before 'check 'start-virtual-dir-server
          ;; The same server when started by tests/virtual-dir returns an
          ;; unexpected status (4 instead of 200) and fails a test.  It is
          ;; unclear why starting it manually here makes it pass.
          (lambda _
            (system "tests/virtual-dir-server &"))))))
   (native-inputs
    `(("docbook-xml" ,docbook-xml-4.3)
      ("gettext" ,gettext-minimal)
      ("glib:bin" ,glib "bin")
      ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
      ("gtk-doc" ,gtk-doc/stable)
      ("pkg-config" ,pkg-config)))
   (inputs
    (list avahi libgudev libsoup))
   (synopsis "WebDav server implementation using libsoup")
   (description "PhoDav was initially developed as a file-sharing mechanism for Spice,
but it is generic enough to be reused in other projects,
in particular in the GNOME desktop.")
   (home-page "https://wiki.gnome.org/phodav")
   (license license:lgpl2.1+)))

(define-public gnome-color-manager
  (package
   (name "gnome-color-manager")
   (version "3.36.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "0fxdng74d8hwhfx1nwl1i4jx9h9f6c2hkyc12f01kqbjcimrxnwx"))))
   (build-system meson-build-system)
   (arguments
    `(#:glib-or-gtk? #t
      #:phases
       (modify-phases %standard-phases
        (add-before
         'check 'pre-check
         (lambda _
           ;; Tests require a running X server.
           (system "Xvfb :1 &")
           (setenv "DISPLAY" ":1")
           #t)))))
   (native-inputs
    `(("desktop-file-utils" ,desktop-file-utils)
      ("gettext" ,gettext-minimal)
      ("glib:bin" ,glib "bin")
      ("gtk+:bin" ,gtk+ "bin")
      ("itstool" ,itstool)
      ("pkg-config" ,pkg-config)
      ("xorg-server" ,xorg-server-for-tests)))
   (inputs
    (list adwaita-icon-theme
          appstream-glib
          colord-gtk
          exiv2
          gnome-desktop
          libcanberra
          libexif
          libtiff
          libxrandr
          libxtst
          libxxf86vm
          vte
          xorgproto))
   (synopsis "Color profile manager for the GNOME desktop")
   (description "GNOME Color Manager is a session framework that makes
it easy to manage, install and generate color profiles
in the GNOME desktop.")
   (home-page "https://gitlab.gnome.org/GNOME/gnome-color-manager")
   (license license:gpl2+)))

(define-public gnome-online-miners
  (package
    (name "gnome-online-miners")
    (version "3.34.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1n2jz9i8a42zwxx5h8j2gdy6q1vyydh4vl00r0al7w8jzdh24p44"))
              (patches
               (search-patches
                "gnome-online-miners-tracker-3.patch"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-configure
           (lambda _
             (substitute* "configure.ac"
               (("AX_CHECK_ENABLE_DEBUG.*")
                ""))))
         (add-after 'fix-configure 'autoreconf
           (lambda _
             (invoke "autoreconf" "-vif"))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gnome-online-accounts" ,gnome-online-accounts)
       ("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("grilo" ,grilo)
       ("libgdata" ,libgdata)
       ("libgfbgraph" ,gfbgraph)
       ("libzapojit" ,libzapojit)
       ("rest" ,rest)
       ("tracker" ,tracker)))
    (synopsis "Web Crawlers for GNOME")
    (description "GNOME Online Miners provides a set of crawlers that
go through your online content and index them locally in Tracker.
It has miners for Facebook, Flickr, Google, ownCloud and SkyDrive.")
    (home-page "https://wiki.gnome.org/Projects/GnomeOnlineMiners")
    (license license:gpl2+)))

(define-public gssdp
  (package
   (name "gssdp")
   (version "1.2.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "1s57i8a8wnnxnsfl27cq4503dkdlzbrhry5zpg23sfqfffvdqqx2"))))
   (build-system meson-build-system)
   (native-inputs
    `(("gettext" ,gettext-minimal)
      ("glib:bin" ,glib "bin")
      ("gobject-introspection" ,gobject-introspection)
      ("gtk-doc" ,gtk-doc/stable)
      ("pkg-config" ,pkg-config)
      ("vala" ,vala)))
   (inputs
    (list gtk+))
   (propagated-inputs
    ;; The .pc file "Requires" libsoup-2.4.
    `(("libsoup" ,libsoup-minimal-2)))
   (synopsis "GObject-based API over @acronym{SSDP, Simple Service Discovery
Protocol} for GNOME")
   (description "This package provides a library to handle resource discovery
and announcement over @acronym{SSDP, Simple Service Discovery Protocol} and
a debugging tool, @command{gssdp-device-sniffer}.")
   (home-page "https://gitlab.gnome.org/GNOME/gssdp")
   (license license:lgpl2.0+)))

(define-public gupnp
  (package
   (name "gupnp")
   (version "1.2.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "1ld7mrpdv9cszmfzh7i19qx4li25j3fr7x1jp38l8phzlmz3187p"))))
   (build-system meson-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-before 'check 'pre-check
          (lambda _
            ;; Tests require a writable HOME.
            (setenv "HOME" (getcwd)))))))
   (native-inputs
    `(("gettext" ,gettext-minimal)
      ("glib:bin" ,glib "bin")
      ("gobject-introspection" ,gobject-introspection)
      ("gtk-doc" ,gtk-doc/stable)
      ("pkg-config" ,pkg-config)
      ("vala" ,vala)))
   (inputs
    (list gssdp gtk+ libsoup))
   (propagated-inputs
    (list ;; For ‘org.gnome.system.proxy’.
          gsettings-desktop-schemas))
   (synopsis "PnP API for GNOME")
   (description "This package provides GUPnP, an object-oriented framework
for creating UPnP devices and control points, written in C using
@code{GObject} and @code{libsoup}.")
   (home-page "https://gitlab.gnome.org/GNOME/gupnp")
   (license license:lgpl2.0+)))

(define-public gupnp-dlna
  (package
   (name "gupnp-dlna")
   (version "0.10.5")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "0spzd2saax7w776p5laixdam6d7smyynr9qszhbmq7f14y13cghj"))))
   (build-system gnu-build-system)
   (native-inputs
    (list gettext-minimal
          `(,glib "bin")
          gobject-introspection
          gtk-doc/stable
          libxml2
          pkg-config
          vala))
   (inputs
    (list gstreamer gupnp))
   (propagated-inputs
    (list gst-plugins-base gst-plugins-good))
   (synopsis "GUPnP DLNA for GNOME")
   (description "This package provides a small utility library to
support DLNA-related tasks such as media profile guessing, transcoding to a
given profile, etc.  DLNA is a subset of UPnP A/V.")
   (home-page "https://gitlab.gnome.org/GNOME/gupnp-dlna")
   (license license:lgpl2.0+)))

(define-public gupnp-av
  (package
   (name "gupnp-av")
   (version "0.12.11")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "1p3grslwqm9bc8rmpn4l48d7v9s84nina4r9xbd932dbj8acz7b8"))))
   (build-system gnu-build-system)
   (native-inputs
    (list gettext-minimal
          `(,glib "bin")
          gobject-introspection
          gtk-doc/stable
          libxml2
          pkg-config
          vala))
   (inputs
    (list gtk+ gupnp))
   (synopsis "GUPnP A/V for GNOME")
   (description "This package provides a small library for handling
and implementation of UPnP A/V profiles.")
   (home-page "https://gitlab.gnome.org/GNOME/gupnp-av")
   (license license:lgpl2.0+)))

(define-public libmediaart
  (package
    (name "libmediaart")
    (version "1.9.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1mlw1qgj8nkd9ll6b6h54r1gfdy3zp8a8xqz7qfyfaj85jjgbph7"))))
    (build-system meson-build-system)
    (native-inputs
     (list `(,glib "bin") pkg-config))
    (inputs
     (list gdk-pixbuf gettext-minimal gobject-introspection
           `(,gtk+ "doc") vala))
    (synopsis "Media art library for the GNOME desktop")
    (description
     "The libmediaart library is the foundation for media art caching,
extraction, and lookup for applications on the desktop.")
    (home-page "https://gitlab.gnome.org/GNOME/libmediaart")
    (license license:lgpl2.1+)))

(define-public gnome-initial-setup
  (package
   (name "gnome-initial-setup")
   (version "40.4")
   (source (origin
            (method url-fetch)
           (uri (string-append "mirror://gnome/sources/gnome-initial-setup/"
                                (version-major version)
                                "/gnome-initial-setup-" version ".tar.xz"))
            (sha256
             (base32
              "06q3p4f8g9zr7a4mw3qr556mi0dg9qzrj8n46ybdz93fxs26aaj1"))))
   (build-system meson-build-system)
   (arguments
    `(#:configure-flags '(;; Enable camera support for user selfie.
                          "-Dcheese=auto"
                          "-Dsystemd=false")
      #:phases (modify-phases %standard-phases
                 (add-after 'unpack 'set-gkbd-file-name
                   (lambda* (#:key inputs #:allow-other-keys)
                     ;; Allow the "Preview" button in the keyboard layout
                     ;; selection dialog to display the layout.
                     (let ((libgnomekbd (assoc-ref inputs "libgnomekbd")))
                       (substitute* "gnome-initial-setup/pages/keyboard/cc-input-chooser.c"
                         (("\"gkbd-keyboard-display")
                          (string-append "\"" libgnomekbd
                                         "/bin/gkbd-keyboard-display")))
                       #t))))))
   (native-inputs
    `(("gettext" ,gettext-minimal)
      ("glib:bin" ,glib "bin")
      ("gobject-introspection" ,gobject-introspection)
      ("gtk+:bin" ,gtk+ "bin")
      ("pkg-config" ,pkg-config)))
   (inputs
    `(("accountsservice" ,accountsservice)
      ;("adwaita-icon-theme" ,adwaita-icon-theme)
      ("elogind" ,elogind)
      ("gdm" ,gdm)
      ("geoclue" ,geoclue)
      ("gnome-desktop" ,gnome-desktop)
      ("gnome-online-accounts" ,gnome-online-accounts)
      ("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
      ("gstreamer" ,gstreamer)
      ("ibus" ,ibus)
      ("json-glib" ,json-glib)
      ("krb5" ,mit-krb5)
      ("libgweather" ,libgweather)
      ("libnma" ,libnma)
      ("libsecret" ,libsecret)
      ("network-manager" ,network-manager)
      ("packagekit" ,packagekit)
      ("polkit" ,polkit)
      ("pwquality" ,libpwquality)
      ("rest" ,rest)
      ("upower" ,upower)
      ("webkitgtk" ,webkitgtk-with-libsoup2)
      ("libgnomekbd" ,libgnomekbd)))
   (synopsis "Initial setup wizard for GNOME desktop")
   (description "This package provides a set-up wizard when a
user logs into GNOME for the first time.  It typically provides a
tour of all gnome components and allows the user to set them up.")
   (home-page "https://gitlab.gnome.org/GNOME/gnome-initial-setup")
   (license license:gpl2)))

(define-public gnome-user-share
  (package
   (name "gnome-user-share")
   (version "3.34.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "04r9ck9v4i0d31grbli1d4slw2d6dcsfkpaybkwbzi7wnj72l30x"))))
   (build-system meson-build-system)
   (arguments
    `(#:glib-or-gtk? #t
      #:configure-flags
       `("-Dsystemduserunitdir=/tmp/empty"
         ;; Enable nautilus extension for file sharing.
         "-Dnautilus_extension=true")))
   (native-inputs
    `(("gettext" ,gettext-minimal)
      ("glib:bin" ,glib "bin")
      ("gobject-introspection" ,gobject-introspection)
      ("gtk+:bin" ,gtk+ "bin")
      ("pkg-config" ,pkg-config)
      ("yelp-tools" ,yelp-tools)))
   (inputs
    (list glib
          gnome-bluetooth
          gtk+
          libcanberra
          libnotify
          nautilus))      ; For nautilus extension.
   (synopsis "File sharing for GNOME desktop")
   (description "GNOME User Share is a small package that binds together
various free software projects to bring easy to use user-level file
sharing to the masses.")
   (home-page "https://gitlab.gnome.org/GNOME/gnome-user-share")
   (license license:gpl2+)))

(define-public sushi
  (package
    (name "sushi")
    (version "3.38.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1sc8i0vrw6bn43rklnwp2fl1gq0r0xfd3bszcqxd0a2mh46d3rpk"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'glib-or-gtk-wrap 'wrap-typelib
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((prog (string-append (assoc-ref outputs "out")
                                        "/bin/sushi")))
               ;; Put existing typelibs before sushi's deps, so as to
               ;; correctly infer gdk-pixbuf.
               (wrap-program prog
                 `("GI_TYPELIB_PATH" suffix
                   (,(getenv "GI_TYPELIB_PATH"))))))))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("clutter" ,clutter)
       ("clutter-gst" ,clutter-gst)
       ("clutter-gtk" ,clutter-gtk)
       ("evince" ,evince)                         ; For file previewing.
       ("freetype" ,freetype)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gjs" ,gjs)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gstreamer" ,gstreamer)
       ("gtksourceview" ,gtksourceview)
       ("harfbuzz" ,harfbuzz)
       ("libepoxy" ,libepoxy)
       ("libmusicbrainz" ,libmusicbrainz)
       ("libxml2" ,libxml2)
       ("neon" ,neon)
       ("webkitgtk" ,webkitgtk-with-libsoup2)))
    (synopsis "File previewer for the GNOME desktop")
    (description "Sushi is a DBus-activated service that allows applications
to preview files on the GNOME desktop.")
    (home-page "https://gitlab.gnome.org/GNOME/sushi")
    (license license:gpl2+)))

(define-public rygel
  (package
    (name "rygel")
    (version "0.40.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0bd3d2swa8lq47b9r04x8xwdr9nhjnrwqi6scygd134wb5vxfh22"))))
    (build-system meson-build-system)
    (arguments
     ;; Disable the tracker plugin.
     '(#:configure-flags
       (list "-Dplugins=external,gst-launch,lms,media-export,
mpris,playbin,ruih,tracker3")))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")               ; for glib-compile-schemas, etc.
           gobject-introspection
           gtk-doc/stable
           pkg-config
           python
           vala
           docbook-xml
           docbook-xsl
           libxslt))
    (inputs
     (list gdk-pixbuf
           gssdp
           gstreamer
           gst-plugins-base
           gst-editing-services
           gtk+
           gupnp
           gupnp-av
           gupnp-dlna
           json-glib
           libgee
           libmediaart
           libsoup
           libxslt
           libunistring
           tracker))
    (synopsis "Share audio, video, and pictures with other devices")
    (description
     "Rygel is a home media solution (@dfn{UPnP AV MediaServer and
MediaRenderer}) for GNOME that allows you to easily share audio, video, and
pictures, and to control a media player on your home network.

Rygel achieves interoperability with other devices by trying to conform to the
strict requirements of DLNA and by converting media on-the-fly to formats that
client devices can handle.")
    (home-page "https://wiki.gnome.org/Projects/Rygel")
    (license (list
              ;; For logo (data/icons/*).
              license:cc-by-sa3.0
              ;; For all others.
              license:lgpl2.1+))))

(define-public libnma
  (package
    (name "libnma")
    (version "1.8.28")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "09mp6k0hfam1vyyv9kcd8j4gb2r58i05ipx2nswb58ris599bxja"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xmldoc (string-append (assoc-ref inputs "docbook-xml")
                                          "/xml/dtd/docbook")))
               (substitute* "libnma-docs.xml"
                 (("http://.*/docbookx\\.dtd")
                  (string-append xmldoc "/docbookx.dtd")))
               #t))))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.3)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gtk-doc" ,gtk-doc/stable)
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     (list gcr gtk+ iso-codes mobile-broadband-provider-info
           network-manager))
    (synopsis "Network Manager's applet library")
    (description "Libnma is an applet library for Network Manager.  It was
initially part of network-manager-applet and has now become a separate
project.")
    (home-page "https://gitlab.gnome.org/GNOME/libnma")
    ;; Some files carry the "GPL-2.0+" SPDX identifier while others say
    ;; "LGPL-2.1+".
    (license license:gpl2+)))

(define-public gnome-menus
  (package
    (name "gnome-menus")
    (version "3.36.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-menus/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "07xvaf8s0fiv0035nk8zpzymn5www76w2a1vflrgqmp9plw8yd6r"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gettext-minimal glib pkg-config))
    (synopsis "Menu support for GNOME desktop")
    (description "GNOME Menus contains the libgnome-menu library, the layout
configuration files for the GNOME menu, as well as a simple menu editor.")
    (home-page "https://gitlab.gnome.org/GNOME/gnome-menus")
    (license license:lgpl2.0+)))

(define-public deja-dup
  (package
    (name "deja-dup")
    (version "42.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.gnome.org/World/deja-dup/-/archive/"
                                  version "/deja-dup-" version ".tar.bz2"))
              (sha256
               (base32
                "0d1jnlxpk52x56aqxz1g2xb4y4sm24h08p2di8mc1k8n8b52rpi4"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       (list
        ;; Otherwise, the RUNPATH will lack the final path component.
        (string-append "-Dc_link_args=-Wl,-rpath="
                       (assoc-ref %outputs "out") "/lib/deja-dup"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((python (assoc-ref inputs "python")))
               (substitute* '("libdeja/tools/duplicity/DuplicityInstance.vala"
                              "libdeja/tests/scripts/instance-error.test")
                 (("/bin/rm")
                  (which "rm")))
               (substitute* "libdeja/tests/runner.vala"
                 (("/bin/sh")
                  (which "sh")))
               (substitute* "libdeja/tests/scripts/instance-error.test"
                 (("`which python3`")
                  (string-append python "/bin/python3"))))))
         (add-after 'unpack 'patch-libgpg-error
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libgpg-error (assoc-ref inputs "libgpg-error")))
               (substitute* "meson.build"
                 (("(gpgerror_libs = ).*" _ var)
                  (format #f "~a '-L~a/lib -lgpg-error'\n" var libgpg-error))))
             #t))
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "data/post-install.sh"
               (("gtk-update-icon-cache") "true"))
             #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Add duplicity to the search path
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/deja-dup")
               `("PATH" ":" prefix
                 (,(string-append (assoc-ref inputs "duplicity") "/bin")))))))))
    (inputs
     `(("bash-minimal" ,bash-minimal)
       ("duplicity" ,duplicity)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("libgpg-error" ,libgpg-error)
       ("libnotify" ,libnotify)
       ("libsecret" ,libsecret)
       ("libsoup" ,libsoup-minimal-2)
       ("libhandy" ,libhandy)
       ("packagekit" ,packagekit)
       ("python" ,python)
       ("python-pygobject" ,python-pygobject)))
    (native-inputs
     `(("appstream-glib" ,appstream-glib)
       ("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("glib" ,glib "bin")             ; for glib-compile-schemas.
       ("gobject-introspection" ,gobject-introspection)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://wiki.gnome.org/Apps/DejaDup")
    (synopsis "Simple backup tool, for regular encrypted backups")
    (description
     "Déjà Dup is a simple backup tool, for regular encrypted backups.  It
uses duplicity as the backend, which supports incremental backups and storage
either on a local, or remote machine via a number of methods.")
    (license license:gpl3+)))

(define-public gnome-commander
  (package
    (name "gnome-commander")
    (version "1.12.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version)  "/"
                           "gnome-commander-" version ".tar.xz"))
       (sha256
        (base32 "0fhmfxh6kbchggvpbin7d4g4fq5vc49y0w48wdkyxln5dswidss2"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           googletest
           intltool
           itstool
           libtool
           pkg-config))
    (inputs
     (list gconf gnome-vfs gtk+-2 libxml2))
    (home-page "https://gcmd.github.io/")
    (synopsis "Two-pane graphical file manager for the GNOME desktop")
    (description
     "GNOME Commander is a two-pane graphical file manager using GNOME
libraries.  It aims to fulfill the demands of more advanced users who
like to focus on file management, their work through special applications
and running smart commands.")
    (license license:gpl2+)))

(define-public gnome-user-docs
  (package
   (name "gnome-user-docs")
   (version "40.5")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/gnome-user-docs/"
                          (version-major version)
                          "/gnome-user-docs-" version ".tar.xz"))
      (sha256
       (base32 "0p6ysdqlfc7vvzsrcanl9bhsc7666sv42xxzpbgsf5j55z3yrkpr"))))
   (build-system gnu-build-system)
   (native-inputs
    (list gettext-minimal itstool pkg-config libxml2))
   (synopsis "User documentation for the GNOME desktop")
   (description
    "The GNOME User Documentation explains how to use the GNOME desktop and its
components.  It covers usage and setup of the core GNOME programs by end-users
and system administrators.")
   (home-page "https://live.gnome.org/DocumentationProject")
   (license license:cc-by3.0)))

(define-public dia
  ;; This version from GNOME's repository includes fixes for compiling with
  ;; recent versions of the build tools.  The latest activity on the
  ;; pre-GNOME version has been in 2014, while GNOME has continued applying
  ;; fixes since.
  (let ((commit "3cf7ec4c2e5bca139a7f3e17f9fc9009c237fcc5")
        (revision "2"))
    (package
      (name "dia")
      (version (git-version "0.97.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.gnome.org/GNOME/dia.git/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "04r8dspa6nmicrifhi3sh46hqvyy88hzq37xx99q3q1mwsrpmwy8"))))
      (build-system meson-build-system)
      (arguments `(#:meson ,meson-0.59))
      (inputs
       (list graphene
             gtk+-2
             libxml2
             libxslt

             ;; XXX: PDF plugin fails to build with poppler 21.07.0.
             ;; poppler

             ;; Without Python 2, build fails: plug-ins/python/meson.build:4:0:
             ;; ERROR: Unknown method "dependency" in object.
             python-2))
      (native-inputs
       (list appstream-glib docbook-xsl
             `(,glib "bin") intltool pkg-config))
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
    (version "0.18.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1iq4d1qy0vkmy29xvr13dgz4pxvn5v3yi2swryld0ajinvp951fx"))))
    (build-system meson-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-home-for-tests
           (lambda _
             (setenv "HOME" "/tmp"))))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("uhttpmock" ,uhttpmock)))
    (inputs
     (list cyrus-sasl glib-networking vala))
    (propagated-inputs
     `(("gcr" ,gcr)
       ("glib" ,glib)
       ("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("json-glib" ,json-glib)
       ("liboauth" ,liboauth)
       ("libsoup" ,libsoup-minimal-2)
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
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "07156nj7yrp3h9zyzx6mjwxwmfijialb4gi5d6dwyp53arr8c9vd"))))
    (build-system meson-build-system)
    (native-inputs
     (list gobject-introspection pkg-config))
    (inputs
     (list gtk+ libjpeg-turbo lcms libtiff))
    (propagated-inputs
     ;; In Requires of libgxps.pc.
     (list cairo glib libarchive))
    (home-page "https://wiki.gnome.org/Projects/libgxps")
    (synopsis "GObject-based library for handling and rendering XPS documents")
    (description
     "libgxps is a GObject-based library for handling and rendering XPS
documents.  This package also contains binaries that can convert XPS documents
to other formats.")
    (license license:lgpl2.1+)))

(define-public gnome-characters
  (package
    (name "gnome-characters")
    (version "40.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/"
                           "gnome-characters/" (version-major version)
                           "/gnome-characters-" version ".tar.xz"))
       (sha256
        (base32
         "0z2xa4w921bzpzj6gv88pvbrijcnnwni6jxynwz0ybaravyzaqha"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'wrap
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; GNOME Characters needs Typelib files from GTK and
                      ;; gnome-desktop.
                      (wrap-program (string-append (assoc-ref outputs "out")
                                                   "/bin/gnome-characters")
                        `("GI_TYPELIB_PATH" ":" prefix
                          (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gtk+:bin" ,gtk+ "bin")
       ("pkg-config" ,pkg-config)
       ("python" ,python-minimal)))
    (inputs
     (list gjs gtk+ libhandy libunistring gnome-desktop))
    (home-page "https://wiki.gnome.org/Apps/CharacterMap")
    (synopsis "Find and insert unusual characters")
    (description "Characters is a simple utility application to find
and insert unusual characters.  It allows you to quickly find the
character you are looking for by searching for keywords.")
    (license license:bsd-3)))

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

(define-public gnome-contacts
  (package
    (name "gnome-contacts")
    (version "40.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-contacts/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0w2g5xhw65adzvwzakrj5kaim4sw1w7s8qqwm3nm6inq50znzpn9"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Dcheese=disabled")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'generate-vapis
           (lambda* (#:key inputs #:allow-other-keys)
             ;; To generate goa's missing .vapi file
             (define goa
               (assoc-ref inputs "gnome-online-accounts:lib"))

             (invoke "vapigen" "--directory=vapi" "--pkg=gio-2.0"
                     "--library=goa-1.0"
                     (string-append goa "/share/gir-1.0/Goa-1.0.gir"))))
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson_post_install.py"
               (("gtk-update-icon-cache") "true")))))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)
       ("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("docbook-xml" ,docbook-xml)
       ("dockbook-xsl" ,docbook-xsl)
       ("evolution-data-server" ,evolution-data-server)
       ("gettext" ,gettext-minimal)
       ("gnome-desktop" ,gnome-desktop)
       ("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("gobject-introspection" ,gobject-introspection)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gtk+" ,gtk+)
       ("libgee" ,libgee)
       ("libhandy" ,libhandy)
       ("libxslt" ,libxslt)
       ("telepathy-glib" ,telepathy-glib)
       ("vala" ,vala)))
    (propagated-inputs
     (list folks telepathy-mission-control))
    (synopsis "GNOME's integrated address book")
    (description
     "GNOME Contacts organizes your contact information from online and
offline sources, providing a centralized place for managing your contacts.")
    (home-page "https://wiki.gnome.org/Apps/Contacts")
    (license license:gpl2+)))

(define-public gnome-desktop
  (package
    (name "gnome-desktop")
    (version "40.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/" name "/"
                          (version-major version)  "/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "1vs5knn2yj6a449p9bn5w5407i1yvxfxg8xv844qgl70hpsrdbl8"))))
    (build-system meson-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libc   (assoc-ref inputs "libc")))
               (substitute* "libgnome-desktop/gnome-languages.c"
                 (("\"locale\"")
                  (string-append "\"" libc "/bin/locale\"")))
               #t)))
         (add-before 'configure 'patch-bubblewrap
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "libgnome-desktop/gnome-desktop-thumbnail-script.c"
               (("\"bwrap\",")
                (string-append "\"" (which "bwrap") "\","))
               (("\"--ro-bind\", \"/usr\", \"/usr\",")
                (string-append "\"--ro-bind\", \""
                               (%store-directory)
                               "\", \""
                               (%store-directory)
                               "\","))
               (("\"--ro-bind\", \"/etc/ld.so.cache\", \"/etc/ld.so.cache\",") ""))
             #t))
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Tests require a running X server and locales.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")
             (setenv "XDG_CACHE_HOME" "/tmp/xdg-cache")
             (setenv "XDG_CONFIG_HOME" "/tmp")
             (setenv "GUIX_LOCPATH"
                     (search-input-directory inputs
                                             "lib/locale")))))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for gdbus-codegen
       ("glibc-locales" ,glibc-locales) ; for tests
       ("gobject-introspection" ,gobject-introspection)
       ("itstool" ,itstool)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)
       ("xorg-server" ,xorg-server-for-tests)))
    (propagated-inputs
     ;; Required by gnome-desktop-3.0.pc.
     (list gsettings-desktop-schemas
           gtk+
           iso-codes
           libseccomp
           libx11
           xkeyboard-config))
    (inputs
     (list gdk-pixbuf
           glib
           bubblewrap
           libxext
           libxkbfile
           libxrandr))
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
     (list intltool
           docbook-xml-4.4
           python2-libxml2
           libxml2
           libxslt
           pkg-config
           python-2))
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
    (version "40.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1rr1ypb89p51b6428yqvczmpmylwjfnhnkgx78hzm3vxm3m15lff"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Dlogind=libelogind")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") "true"))
             #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("docbook-xml" ,docbook-xml)
       ("docbook-xsl" ,docbook-xsl)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)))
    (inputs
     (list elogind
           glib
           appstream-glib
           gnome-settings-daemon
           gtk+
           libcanberra
           libdvdread
           libhandy
           libnotify
           libpwquality
           libsecret
           udisks))
    (home-page "https://git.gnome.org/browse/gnome-disk-utility")
    (synopsis "Disk management utility for GNOME")
    (description "Disk management utility for GNOME.")
    (license license:gpl2+)))

(define-public gnome-font-viewer
  (package
    (name "gnome-font-viewer")
    (version "40.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-font-viewer/"
                                  (version-major version)
                                  "/gnome-font-viewer-" version ".tar.xz"))
              (sha256
               (base32
                "0hpyi0sz3gcqqs9lkwyk8b6hr39m3n27432x98kxr436jj37dk6j"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-post-install-script
           (lambda _
             (substitute* "meson-postinstall.sh"
               (("update-desktop-database") (which "true")))
             #t))
         (add-after 'install 'patch-thumbnailer
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute*
                   (string-append
                    out
                    "/share/thumbnailers/gnome-font-viewer.thumbnailer")
                 (("gnome-thumbnail-font")
                  (string-append out "/bin/gnome-thumbnail-font"))))
             #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (inputs
     (list glib gnome-desktop gtk+ libhandy libxml2))
    (home-page "https://gitlab.gnome.org/GNOME/gnome-font-viewer")
    (synopsis "GNOME Fonts")
    (description "Application to show you the fonts installed on your computer
for your use as thumbnails.  Selecting any thumbnails shows the full view of how
the font would look under various sizes.")
    (license license:gpl2+)))

(define-public gcr
  (package
    (name "gcr")
    (version "3.41.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "00fsf82ycac8qi0kkiq759p6jrn63pyz4ksn4wnq7m4ax94zq289"))))
    (build-system meson-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; These fail because /var/lib/dbus/machine-id is not present in the
         ;; build environment.
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (substitute* "gcr/meson.build"
               (("[[:blank:]]+'system-prompt',")
                ""))
             #t))
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") "true"))
             #t))
         (add-after 'unpack 'fix-systemd-detection
           (lambda _
             (substitute* "gcr/gcr-ssh-agent-service.c"
               (("#ifdef WITH_SYSTEMD")
                "#if (WITH_SYSTEMD)"))))
         (add-before 'check 'pre-check
           (lambda _
             ;; Some tests expect to write to $HOME.
             (setenv "HOME" "/tmp")
             #t)))))
    (inputs
     (list dbus gnupg ;called as a child process during tests
           libgcrypt libsecret))
    (native-inputs
     `(("python" ,python-wrapper)       ;for tests
       ("openssh" ,openssh)             ;for tests
       ("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("gtk-doc" ,gtk-doc)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("libxml2" ,libxml2)
       ("vala" ,vala)
       ("xsltproc" ,libxslt)))
    ;; mentioned in gck.pc, gcr.pc and gcr-ui.pc
    (propagated-inputs
     (list p11-kit glib gtk+))
    (home-page "https://www.gnome.org")
    (synopsis "Libraries for displaying certificates and accessing key stores")
    (description
     "The GCR package contains libraries used for displaying certificates and
accessing key stores.  It also provides the viewer for crypto files on the
GNOME Desktop.")
    (license license:lgpl2.1+)))

(define-public gdl
  (package
    (name "gdl")
    (version "3.34.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/GNOME/gdl.git")
                    (commit (string-append "GDL_" (string-map (match-lambda
                                                                (#\. #\_)
                                                                (c c))
                                                              version)))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "154qcr0x6f68f4q526y87imv0rscmp34n47nk1pp82rsq52h2zna"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           `(,glib "bin") ; for glib-genmarshal, etc.
           gnome-common
           gtk-doc/stable
           intltool
           pkg-config
           libtool
           which))
    (inputs
     (list libxml2))
    (propagated-inputs
     ;; The gdl-3.0.pc file 'Requires' GTK+.
     (list gtk+))
    (home-page "https://gitlab.gnome.org/GNOME/gdl/")
    (synopsis "GNOME docking library")
    (description "This library provides docking features for gtk+.")
    (license license:lgpl2.1+)))

;;; A minimal variant used to break a cycle with Inkscape.
(define-public gdl-minimal
  (package/inherit gdl
    (name "gdl-minimal")
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-doc-generation
           ;; XXX: There is no easy way to disable generating the
           ;; documentation.
           (lambda _
             (substitute* "configure.in"
               (("GTK_DOC_CHECK.*") "")
               (("docs/.*") ""))
             (substitute* "Makefile.am"
               (("gdl docs po") "gdl po"))
             #t)))))
    (native-inputs (alist-delete "gtk-doc" (package-native-inputs gdl)))))

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
     (list libgcrypt dbus))
    (native-inputs
     (list pkg-config
           `(,glib "bin") intltool))
    (propagated-inputs
     ;; Referred to in .h files and .pc.
     (list glib))
    (home-page "https://www.gnome.org")
    (synopsis "Accessing passwords from the GNOME keyring")
    (description
     "Client library to access passwords from the GNOME keyring.")

    ;; Though a couple of files are LGPLv2.1+.
    (license license:lgpl2.0+)))

(define-public gnome-keyring
  (package
    (name "gnome-keyring")
    (version "40.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/" name "/"
                                 (version-major version)  "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "0cdrlcw814zayhvlaxqs1sm9bqlfijlp22dzzd0g5zg2isq4vlm3"))))
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
         (add-after 'unpack 'fix-/bin/sh-reference
           (lambda _
             (substitute* "po/Makefile.in.in"
               (("/bin/sh") (which "sh")))
             #t))
         (add-after 'unpack 'fix-docbook
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
     (list libgcrypt linux-pam openssh dbus gcr))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")
       ("glib" ,glib) ; for m4 macros
       ("python" ,python-2) ;for tests
       ("intltool" ,intltool)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libxslt" ,libxslt) ;for documentation
       ("docbook-xml" ,docbook-xml-4.3)
       ("docbook-xsl" ,docbook-xsl)))
    (propagated-inputs
     (list gcr))
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
    (version "40.2")
    (source (origin
              (method url-fetch)
              (uri "mirror://gnome/sources/evince/40/evince-40.2.tar.xz")
              (sha256
               (base32
                "0xrwls1bhvny8vvd7mfjy9p26zjch0pd6x6j9jn9g2ka6xwyrxqg"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:build-type "release"
       #:configure-flags
       '("-Dnautilus=false"
         "-Dintrospection=true"
         ;; XXX: Generating the documentation fails because the
         ;; libevdocument.devhelp document cannot be created. This seems to be
         ;; caused by a problem during the XSL transformation.
         "-Dgtk_doc=false")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") "true"))
             #t)))))
    (inputs
     (list libarchive
           libgxps
           libspectre
           djvulibre
           ghostscript
           poppler
           libtiff
           texlive-libkpathsea ; for DVI support
           gnome-desktop
           gsettings-desktop-schemas
           gspell
           libgnome-keyring
           adwaita-icon-theme
           gdk-pixbuf
           atk
           pango
           gtk+
           glib
           libxml2
           libsm
           libice
           shared-mime-info
           dconf
           libcanberra
           libsecret
           libhandy))
    (native-inputs
     `(("itstool" ,itstool)
       ("intltool" ,intltool)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (home-page "https://www.gnome.org/projects/evince/")
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
    (version "41.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version)  "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1v9jagk679m01nji0acirynxinziv036618c7xc49l4nwmr9ja3p"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-schemas
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* (find-files "schemas"
                                               "\\.gschema\\.xml\\.in$")
                        ;; Provide the correct file name of the default
                        ;; GNOME background, 'adwaita-timed.xml'.
                        (("@datadir@/backgrounds/gnome")
                         (search-input-directory inputs
                                                 "/share/backgrounds/gnome"))
                        ;; Do not reference fonts, that may not exist.
                        (("'Source Code Pro 10'") "'Monospace 11'")))))))
    (inputs (list glib gnome-backgrounds))
    (native-inputs (list gettext-minimal
                         `(,glib "bin") ;glib-compile-schemas, etc.
                         gobject-introspection
                         pkg-config
                         python))  ;for build-aux/meson/post-install.py
    (home-page "https://launchpad.net/gsettings-desktop-schemas")
    (synopsis "GNOME settings for various desktop components")
    (description "Gsettings-desktop-schemas contains a collection of GSettings
schemas for settings shared by various components of the GNOME desktop.")
    (license license:lgpl2.1+)))

(define-public python-liblarch
  (package
    (name "python-liblarch")
    (version "3.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/getting-things-gnome/liblarch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xv2mfvyzipbny3iz8vll77wsqxfwh28xj6bj1ff0l452waph45m"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'start-xserver
           (lambda* (#:key inputs #:allow-other-keys)
             (system (format #f "~a/bin/Xvfb :1 &"
                             (assoc-ref inputs "xorg-server")))
             (setenv "DISPLAY" ":1")
             #t)))))
    (native-inputs
     (list xorg-server-for-tests))
    (inputs
     (list gtk+))
    (propagated-inputs
     (list python-pygobject))
    (home-page "https://wiki.gnome.org/Projects/liblarch")
    (synopsis "Library to easily handle complex data structures")
    (description
     "Liblarch is a Python library built to easily handle data structures such
as lists, trees and acyclic graphs.  There's also a GTK binding that will
allow you to use your data structure in a @code{Gtk.Treeview}.

Liblarch support multiple views of one data structure and complex filtering.
That way, you have a clear separation between your data themselves (Model)
and how they are displayed (View).")
    (license license:lgpl3+)))

(define-public gtg
  (package
    (name "gtg")
    (version "0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/getting-things-gnome/gtg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b2slm7kjq6q8c7v4m7aqc8m1ynjxn3bl7445srpv1xc0dilq403"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((prog (string-append (assoc-ref outputs "out")
                                        "/bin/gtg"))
                   (pylib (string-append (assoc-ref outputs "out")
                                         "/lib/python"
                                         ,(version-major+minor
                                           (package-version python))
                                         "/site-packages")))
               (wrap-program prog
                 `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH") ,pylib))
                 `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))
               #t))))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("pkg-config" ,pkg-config)))
    (inputs
     (list librsvg
           gsettings-desktop-schemas
           gtk+
           pango
           python-dbus
           python-liblarch
           python-lxml
           python-pycairo
           python-pygobject
           python-pyxdg))
    (home-page "https://wiki.gnome.org/Apps/GTG")
    (synopsis "Personal organizer for the GNOME desktop")
    (description
     "Getting Things GNOME! (GTG) is a personal tasks and TODO list items
organizer for the GNOME desktop environment inspired by the Getting Things
Done (GTD) methodology.  GTG is designed with flexibility, adaptability,
and ease of use in mind so it can be used as more than just GTD software.
GTG is intended to help you track everything you need to do and need to
know, from small tasks to large projects.")
    (license license:gpl3+)))

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
     (list perl perl-xml-simple))
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
     (list icon-naming-utils intltool pkg-config))
    (home-page "https://wiki.gnome.org/Personalization")
    (synopsis "GNOME icon theme")
    (description "Icons for the GNOME desktop.")
    (license license:lgpl3))) ; or Creative Commons BY-SA 3.0

;; gnome-icon-theme was renamed to adwaita-icon-theme after version 3.12.0.
(define-public adwaita-icon-theme
  (package/inherit gnome-icon-theme
    (name "adwaita-icon-theme")
    (version "40.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1xpqa1rgmixlp953938d08xvf7kv36h747ysial8g65dsrp46v0b"))))
    (native-inputs
     ;; The following requires the SVG pixbuf loader, provided by librsvg,
     ;; available on x86_64 only.
     `(,@(if (target-64bit?)
             `(("gtk-encode-symbolic-svg" ,gtk+ "bin"))
             '())))))

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
     (list icon-naming-utils intltool imagemagick pkg-config))
    (home-page "http://tango-project.org/")
    (synopsis "Tango icon theme")
    (description "This is an icon theme that follows the Tango visual
guidelines.")
    (license license:public-domain)))

(define-public shared-mime-info
  (package
    (name "shared-mime-info")
    (version "1.15")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://gitlab.freedesktop.org/xdg/shared-mime-info/uploads/"
                   "b27eb88e4155d8fccb8bb3cd12025d5b/shared-mime-info-" version
                   ".tar.xz"))
             (sha256
              (base32
               "146vynj78wcwdq0ms52jzm1r4m6dzi1rhyh3h4xyb6bw8ckv10pl"))))
    (build-system gnu-build-system)
    (arguments
     ;; The build system appears not to be parallel-safe.
     '(#:parallel-build? #f))
    (inputs
     (list glib libxml2))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("itstool" ,itstool)
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
    (version "1.5.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/OpenPrinting/system-config-printer/releases/"
             "download/v" version
             "/system-config-printer-" version ".tar.xz"))
       (sha256
        (base32 "12d6xx51vizc476zfnsga9q09nflp51ipn6y7lhi9w2v4772dlpv"))))
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
     (list gsettings-desktop-schemas
           gobject-introspection
           python
           cups
           gtk+
           python-dbus
           python-pygobject
           python-pycups
           python-requests
           python-pycairo
           libnotify
           packagekit))
    (native-inputs
     (list pkg-config
           desktop-file-utils
           glib
           autoconf
           automake
           intltool
           xmlto
           docbook-xml-4.1.2
           docbook-xsl
           libxml2))
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
    (version "0.7.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version)  "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0qa7cx6ra5hwqnxw95b9svgjg5q6ynm8y843iqjszxvds5z53h36"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-docbook
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Don't attempt to download XSL schema.
             (substitute* "meson.build"
               (("http://docbook.sourceforge.net/release/xsl-ns/current\
/manpages/docbook.xsl")
                (string-append (assoc-ref inputs "docbook-xsl")
                               "/xml/xsl/docbook-xsl-"
                               ,(package-version docbook-xsl)
                               "/manpages/docbook.xsl")))
             #t)))))
    (propagated-inputs
     (list ;; In Requires of libnotify.pc.
           gdk-pixbuf glib))
    (inputs
     (list gtk+ libpng))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)

       ;; For the documentation.
       ("gtk-doc" ,gtk-doc/stable)
       ("xsltproc" ,libxslt)
       ("docbook-xsl" ,docbook-xsl)))
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
    (version "1.30.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/" name "/"
                          (version-major+minor version)  "/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "18xrk1c1ixlhkmykcfiafrl2am470ws687xqvjlq40zwkcp5dx8b"))))
    (build-system meson-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'start-xserver
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xorg-server (assoc-ref inputs "xorg-server"))
                   (disp ":1"))
               (setenv "DISPLAY" disp)
               (setenv "XDG_CACHE_HOME" "/tmp/xdg-cache")
               (setenv "XDG_CONFIG_HOME" "/tmp")
               ;; Tests require a running X server.
               (system (format #f "~a/bin/Xvfb ~a &" xorg-server disp))
               #t))))))
    (inputs
     (list gtk+ glade3 python python-pygobject))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("xorg-server" ,xorg-server-for-tests)))
    (propagated-inputs
     ;; The .pc file "Requires" gobject-introspection.
     (list gobject-introspection))
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
    (inputs (list gtk+-2 mesa glu libx11 libxt))
    (native-inputs (list pkg-config
                         `(,glib "bin")))
    (propagated-inputs (list pangox-compat))
    (home-page "https://projects.gnome.org/gtkglext")
    (synopsis "OpenGL extension to GTK+")
    (description "GtkGLExt is an OpenGL extension to GTK+.  It provides
additional GDK objects which support OpenGL rendering in GTK+ and GtkWidget
API add-ons to make GTK+ widgets OpenGL-capable.")
    (license license:lgpl2.1+)))

(define-public glade3
  (package
    (name "glade")
    (version "3.38.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1dxsiz9ahqkxg2a1dw9sbd8jg59y5pdz4c1gvnbmql48gmj8gz4q"))
              (patches (search-patches
                        "glade-gls-set-script-name.patch"
                        "glade-test-widget-null-icon.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") "true"))))

         ,@(if (this-package-native-input "gjs")
               '()
               '((add-after 'unpack 'skip-gjs-test
                   (lambda _
                     ;; When the optional dependency on GJS is missing, skip
                     ;; the GJS plugin tests.
                     (substitute* "tests/modules.c"
                       (("g_test_add.*JavaScript.*" all)
                        (string-append "// " all "\n")))
                     (delete-file "tests/catalogs/gjsplugin.xml")))))

         (add-before 'configure 'fix-docbook
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "man/meson.build"
               (("http://docbook.sourceforge.net/release/xsl/\
current/manpages/docbook.xsl")
                (string-append (assoc-ref inputs "docbook-xsl")
                               "/xml/xsl/docbook-xsl-"
                               ,(package-version docbook-xsl)
                               "/manpages/docbook.xsl")))))
         (add-before 'check 'pre-check
           (lambda _
             (setenv "HOME" "/tmp")
             ;; Tests require a running X server.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1"))))))
    (inputs
     (list gtk+ libxml2))
    (native-inputs
     `(("hicolor-icon-theme" ,hicolor-icon-theme)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("libxslt" ,libxslt) ;for xsltproc
       ("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)
       ("glib:bin" ,glib "bin")
       ("python" ,python)
       ("python-pygobject" ,python-pygobject)
       ("gobject-introspection" ,gobject-introspection)

       ;; GJS depends on Rust, which is x86_64-only so far, so remove the GJS
       ;; dependency on other platforms (FIXME).
       ,@(if (target-x86-64?)
             `(("gjs" ,gjs))
             '())

       ("pkg-config" ,pkg-config)
       ("xorg-server" ,xorg-server-for-tests)))
    (home-page "https://glade.gnome.org")
    (synopsis "GTK+ rapid application development tool")
    (description "Glade is a rapid application development (RAD) tool to
enable quick & easy development of user interfaces for the GTK+ toolkit and
the GNOME desktop environment.")
    (license license:lgpl2.0+)
    (native-search-paths (list (search-path-specification
                                (variable "GLADE_CATALOG_SEARCH_PATH")
                                (files '("share/glade/catalogs")))
                               (search-path-specification
                                (variable "GLADE_MODULE_SEARCH_PATH")
                                (files '("lib/glade/modules")))))))

(define-public libcroco
  (package
    (name "libcroco")
    (version "0.6.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (patches (search-patches "libcroco-CVE-2020-12825.patch"))
              (sha256
               (base32
                "1m110rbj5d2raxcdp4iz0qp172284945awrsbdlq99ksmqsc4zkn"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list glib libxml2 zlib))
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
    (version "1.14.47")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0kbpp9ksl7977xiga37sk1gdw1r039v6zviqznl7alvvg39yp26i"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "bin" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-introspection"
        (string-append "--with-gir-dir="
                       (assoc-ref %outputs "out")
                       "/share/gir-"
                       ,(version-major
                         (package-version gobject-introspection))
                       ".0")
        (string-append "--with-typelib-dir="
                       (assoc-ref %outputs "out")
                       "/lib/girepository-"
                       ,(version-major
                         (package-version gobject-introspection))
                       ".0")
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html")
        "--with-zlib"
        "--with-bz2")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* "gsf-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.5/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))
             #t)))))
    (native-inputs
     `(("docbook-xml" ,docbook-xml)
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("perl" ,perl)
       ("perl-xml-parser" ,perl-xml-parser)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     (list bzip2 gdk-pixbuf zlib))
    (propagated-inputs
     (list glib libxml2))
    (synopsis "G Structured File Library")
    (description "Libgsf aims to provide an efficient extensible I/O abstraction
for dealing with different structured file formats.")
    (home-page "https://gitlab.gnome.org/GNOME/libgsf")
    (license
     (list
      ;; Library
      license:lgpl2.1+
      ;; Others
      license:lgpl2.0+))))

(define-public librsvg
  (package
    (name "librsvg")
    (version "2.50.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/librsvg/"
                                  (version-major+minor version)  "/"
                                  "librsvg-" version ".tar.xz"))
              (sha256
               (base32
                "1g3f8byg5w08fx1bka12mmpl59v6a4q2p827w6m2la6mijq63yzz"))
              (modules '((guix build utils)))
              (snippet
               '(begin (delete-file-recursively "vendor")))))
    (build-system cargo-build-system)
    (outputs '("out" "doc" "debug"))
    (arguments
     `(#:install-source? #f
       #:modules
       ((guix build cargo-build-system)
        (guix build utils)
        ((guix build gnu-build-system) #:prefix gnu:))
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cairo-rs" ,rust-cairo-rs-0.8)
        ("rust-cairo-sys-rs" ,rust-cairo-sys-rs-0.9)
        ("rust-cast" ,rust-cast-0.2)
        ("rust-cssparser" ,rust-cssparser-0.27)
        ("rust-data-url" ,rust-data-url-0.1)
        ("rust-encoding" ,rust-encoding-0.2)
        ("rust-float-cmp" ,rust-float-cmp-0.8)
        ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.8)
        ("rust-gdk-pixbuf-sys" ,rust-gdk-pixbuf-sys-0.9)
        ("rust-gio" ,rust-gio-0.8)
        ("rust-gio-sys" ,rust-gio-sys-0.9)
        ("rust-glib" ,rust-glib-0.9)
        ("rust-glib-sys" ,rust-glib-sys-0.9)
        ("rust-gobject-sys" ,rust-gobject-sys-0.9)
        ("rust-itertools" ,rust-itertools-0.9)
        ("rust-language-tags" ,rust-language-tags-0.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-locale-config" ,rust-locale-config-0.3)
        ("rust-markup5ever" ,rust-markup5ever-0.10)
        ("rust-nalgebra" ,rust-nalgebra-0.21)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-pango" ,rust-pango-0.8)
        ("rust-pango-sys" ,rust-pango-sys-0.9)
        ("rust-pangocairo" ,rust-pangocairo-0.9)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-rctree" ,rust-rctree-0.3)
        ("rust-rgb" ,rust-rgb-0.8)
        ("rust-regex" ,rust-regex-1)
        ("rust-selectors" ,rust-selectors-0.22)
        ("rust-string-cache" ,rust-string-cache-0.8)
        ("rust-tinyvec" ,rust-tinyvec-0.3)
        ("rust-url" ,rust-url-2)
        ("rust-xml5ever" ,rust-xml5ever-0.16))
       #:cargo-development-inputs
       (("rust-assert-cmd" ,rust-assert-cmd-1)
        ("rust-cairo-rs" ,rust-cairo-rs-0.8)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-criterion" ,rust-criterion-0.3)
        ("rust-float-cmp" ,rust-float-cmp-0.8)
        ("rust-lopdf" ,rust-lopdf-0.26)
        ("rust-png" ,rust-png-0.16)
        ("rust-predicates" ,rust-predicates-1)
        ("rust-tempfile" ,rust-tempfile-3))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* "rsvg-docs.xml"
                 (("http://www.oasis-open.org/docbook/xml/4.3/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))))
         (add-after 'unpack 'prepare-for-build
           (lambda _
             ;; In lieu of #:make-flags
             (setenv "CC" ,(cc-for-target))
             ;; Something about the build environment resists building
             ;; successfully with the '--locked' flag.
             (substitute* '("Makefile.am" "Makefile.in")
               (("--locked") ""))))
         (add-before 'configure 'pre-configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "gdk-pixbuf-loader/Makefile.in"
               ;; By default the gdk-pixbuf loader is installed under
               ;; gdk-pixbuf's prefix.  Work around that.
               (("gdk_pixbuf_moduledir = .*$")
                (string-append "gdk_pixbuf_moduledir = "
                               "$(prefix)/"
                               ,(dirname %gdk-pixbuf-loaders-cache-file) "/"
                               "loaders\n")))
             (substitute* "configure"
               (("gdk_pixbuf_cache_file=.*")
                (string-append "gdk_pixbuf_cache_file="
                               (assoc-ref outputs "out") "/"
                               ,%gdk-pixbuf-loaders-cache-file "\n")))))
         (add-after 'configure 'gnu-configure
           (lambda* (#:key outputs #:allow-other-keys #:rest args)
             (apply (assoc-ref gnu:%standard-phases 'configure)
                    #:configure-flags
                    (list "--disable-static"
                          "--enable-vala"
                          (string-append "--with-html-dir="
                                         (assoc-ref outputs "doc")
                                         "/share/gtk-doc/html"))
                    args)))
         (add-after 'configure 'dont-vendor-self
           (lambda* (#:key vendor-dir #:allow-other-keys)
             ;; Don't keep the whole tarball in the vendor directory
             (delete-file-recursively
              (string-append vendor-dir "/" ,name "-" ,version ".tar.xz"))))
         (replace 'build
           (assoc-ref gnu:%standard-phases 'build))
         (add-before 'check 'ignore-failing-tests
           ;; stderr=```/tmp/guix-build-.../librsvg-2.50.1/rsvg-convert: line 150: ls: command not found
           (lambda _
             (substitute* "tests/src/cmdline/rsvg_convert.rs"
               (("fn background_color_option_invalid_color_yields_error" all)
                (string-append "#[ignore] " all))
               (("fn empty_input_yields_error" all)
                (string-append "#[ignore] " all))
               (("fn empty_svg_yields_error" all)
                (string-append "#[ignore] " all))
               (("fn env_source_data_epoch_empty" all)
                (string-append "#[ignore] " all))
               (("fn env_source_data_epoch_no_digits" all)
                (string-append "#[ignore] " all))
               (("fn env_source_data_epoch_trailing_garbage" all)
                (string-append "#[ignore] " all))
               (("fn export_id_option_error" all)
                (string-append "#[ignore] " all))
               (("fn huge_zoom_factor_yields_error" all)
                (string-append "#[ignore] " all))
               (("fn multiple_input_files_not_allowed_for_png_output" all)
                (string-append "#[ignore] " all))
               (("fn stylesheet_option_error" all)
                (string-append "#[ignore] " all)))))
         (replace 'check
           (lambda* args
             ((assoc-ref gnu:%standard-phases 'check)
              #:test-target "check")))
         (replace 'install
           (assoc-ref gnu:%standard-phases 'install)))))
    (native-inputs
     (list docbook-xml-4.3
           `(,glib "bin")
           gobject-introspection
           pkg-config
           python-wrapper
           ruby
           vala))
    (inputs
     (list bzip2
           fontconfig
           freetype
           harfbuzz
           libcroco
           libgsf
           libxml2
           pango))
    (propagated-inputs
     (list cairo gdk-pixbuf glib))
    (synopsis "SVG rendering library")
    (description "Librsvg is a library to render SVG images to Cairo surfaces.
GNOME uses this to render SVG icons.  Outside of GNOME, other desktop
environments use it for similar purposes.  Wikimedia uses it for Wikipedia's SVG
diagrams.")
    (home-page "https://wiki.gnome.org/LibRsvg")
    (license license:lgpl2.1+)))

;; This copy of librsvg uses the bundled rust libraries. It is useful for
;; packages which have too many dependencies to be rebuilt as frequently
;; as the rust inputs are updated.
;; TODO: Remove this package and use packaged rust libraries!
(define-public librsvg-bootstrap
  (package
    (inherit librsvg)
    (name "librsvg")
    (version "2.50.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/librsvg/"
                                  (version-major+minor version)  "/"
                                  "librsvg-" version ".tar.xz"))
              (sha256
               (base32
                "1g3f8byg5w08fx1bka12mmpl59v6a4q2p827w6m2la6mijq63yzz"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file (find-files "vendor" "\\.a$"))))))
    (arguments
     (substitute-keyword-arguments (package-arguments librsvg)
       ((#:vendor-dir _ "vendor") "vendor")
       ((#:cargo-inputs _) '())
       ((#:cargo-development-inputs _) '())))
    (properties '((hidden? . #t)))))

(define-public librsvg-2.40
  ;; This is the last version implemented in C.
  (package
    (inherit librsvg)
    (version "2.40.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/librsvg/"
                                  (version-major+minor version)
                                  "/librsvg-" version ".tar.xz"))
              (sha256
               (base32
                "1fljkag2gr7c4k5mn798lgf9903xslz8h51bgvl89nnay42qjqpp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--disable-static")
       #:phases
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
         (add-before 'check 'remove-failing-tests
           (lambda _
             (with-directory-excursion "tests/fixtures/reftests"
               (for-each delete-file
                         '(;; This test fails on i686:
                           "svg1.1/masking-path-04-b.svg"
                           ;; This test fails on armhf:
                           "svg1.1/masking-mask-01-b.svg"
                           ;; This test fails on aarch64:
                           "bugs/777834-empty-text-children.svg"

                           ;; These two tests fail due to slightly different
                           ;; text rendering (different kerning or similar),
                           ;; nothing alarming.
                           "bugs/340047.svg"
                           "bugs/749415.svg"))))))))
    (native-inputs
     (list pkg-config
           `(,glib "bin") ; glib-mkenums, etc.
           gobject-introspection)) ; g-ir-compiler, etc.
    (inputs
     (list pango libcroco bzip2 libgsf libxml2))
    (propagated-inputs
     ;; librsvg-2.0.pc refers to all of that.
     (list cairo gdk-pixbuf glib))
    (synopsis "Render SVG files using Cairo (ancient C version)")
    (properties '((hidden? . #t)))))

(define* (librsvg-for-system #:optional
                             (system (or (%current-target-system)
                                         (%current-system))))
  ;; Since librsvg 2.50 depends on Rust, and Rust is only correctly supported
  ;; on x86_64 so far, use the ancient C version on other platforms (FIXME).
  (if (string-prefix? "x86_64-" system)
      librsvg
      librsvg-2.40))

(export librsvg-for-system)

(define-public libidl
  (package
    (name "libidl")
    (version "0.8.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libIDL/"
                                  (version-major+minor version) "/"
                                  "libIDL-" version ".tar.bz2"))
              (sha256
               (base32
                "08129my8s9fbrk0vqvnmx6ph4nid744g5vbwphzkaik51664vln5"))))
    (build-system gnu-build-system)
    (inputs (list glib))
    (native-inputs
     (list pkg-config flex bison))
    (home-page "http://freecode.com/projects/libidl")
    (synopsis "Create trees of CORBA Interface Definition Language files")
    (description  "Libidl is a library for creating trees of CORBA Interface
Definition Language (idl) files, which is a specification for defining
portable interfaces. libidl was initially written for orbit (the orb from the
GNOME project, and the primary means of libidl distribution).  However, the
functionality was designed to be as reusable and portable as possible.")
    (properties `((upstream-name . "libIDL")))
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
     `(;; The "timeout-server" test hangs when run in parallel.
       #:parallel-tests? #f
       #:configure-flags
       '(;; We don't need static libraries, plus they don't build reproducibly
         ;; (non-deterministic ordering of .o files in the archive.)
         "--disable-static"

         ;; The programmer kindly gives us a hook to turn off deprecation
         ;; warnings ...
         "DISABLE_DEPRECATED_CFLAGS=-DGLIB_DISABLE_DEPRECATION_WARNINGS")
       ;; ... which they then completly ignore !!
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-parallel-build
           ;; Parallel build fails because of a failed dependency,
           ;; https://bugzilla.gnome.org/show_bug.cgi?id=732274
           (lambda _
             (substitute* "src/services/name/Makefile.am"
               (("orbit_name_server_2_DEPENDENCIES = \\$(DEPS) CosNaming.h")
                "orbit_name_server_2_DEPENDENCIES = \
$(DEPS) CosNaming.h libname-server-2.a"))))
         (add-before 'configure 'ignore-deprecations
           (lambda _
             (substitute* "linc2/src/Makefile.in"
               (("-DG_DISABLE_DEPRECATED")
                "-DGLIB_DISABLE_DEPRECATION_WARNINGS")))))))
    (inputs (list glib libidl))
    (native-inputs
     (list pkg-config))
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
    (inputs (list popt libxml2))
    ;; The following are Required by the .pc file
    (propagated-inputs
     (list glib orbit2))
    (native-inputs
     (list intltool
           pkg-config
           `(,glib "bin") ; for glib-genmarshal, etc.
           flex
           bison))
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
    (inputs (list dbus-glib libxml2))
    (propagated-inputs (list glib ; referred to in the .pc file
                             orbit2))
    (native-inputs
     (list intltool
           `(,glib "bin") ; for glib-genmarshal, etc.
           pkg-config))
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
     (list perl intltool))
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
    (inputs (list libxml2 dbus-glib gconf gnome-mime-data zlib))
    (native-inputs
     (list `(,glib "bin") ; for glib-mkenums, etc.
           intltool pkg-config))
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
    (inputs (list libxml2))
    (native-inputs
     (list `(,glib "bin") ; for glib-mkenums, etc.
           intltool pkg-config))
    ;; The following are listed as Required in the .pc file
    ;; (except for libcanberra -- which seems to be oversight on the part
    ;; of the upstream developers -- anything that links against libgnome,
    ;; must also link against libcanberra
    (propagated-inputs
     (list libcanberra libbonobo gconf gnome-vfs popt))                       ;gnome-program.h includes popt.h
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
     (list pkg-config))
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
    (propagated-inputs (list libart-lgpl gtk+-2))
    (native-inputs
     (list intltool
           `(,glib "bin") ; for glib-genmarshal, etc.
           pkg-config))
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
    (propagated-inputs (list libgnomecanvas))
    (native-inputs
     (list gtkmm-2 pkg-config))
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
    (propagated-inputs (list libbonoboui libgnome libgnomecanvas
                             libgnome-keyring))
    (inputs (list libjpeg-turbo popt libbonobo libxml2 libglade))
    (native-inputs
     (list `(,glib "bin") ; for glib-mkenums, etc.
           intltool pkg-config))
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
     (list python)) ;; needed for the optional libglade-convert program
    (propagated-inputs
     (list gtk+-2 libxml2)) ; required by libglade-2.0.pc
    (native-inputs
     (list pkg-config))
    (home-page "https://developer.gnome.org/libglade")
    (synopsis "Load glade interfaces and access the glade built widgets")
    (description "Libglade is a library that provides interfaces for loading
graphical interfaces described in glade files and for accessing the
widgets built in the loading process.")
    (license license:gpl2+))) ; This is correct.  GPL not LGPL

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
    (propagated-inputs (list libxml2))
    (inputs
     (list popt pangox-compat libgnome libgnomecanvas libglade))
    (native-inputs
     (list `(,glib "bin") ; for glib-genmarshal, etc.
           intltool
           xorg-server-for-tests ; For running the tests
           pkg-config))
    (home-page "https://developer.gnome.org/libbonoboui/")
    (synopsis "Some user interface controls using Bonobo")
    (description  "The Bonobo UI library provides a number of user interface
controls using the Bonobo component framework.")
    (license license:lgpl2.0+)))

(define-public libwnck
  (package
    (name "libwnck")
    (version "3.32.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1jp3p1lnwnwi6fxl2rz3166cmwzwy9vqz896anpwc3wdy9f875cm"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config
           `(,glib "bin") ; for glib-mkenums
           gobject-introspection ; for g-ir-scanner
           intltool))
    (propagated-inputs
     (list gtk+ libxres startup-notification))
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
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config intltool))
    (propagated-inputs
     (list gtk+-2 libxres startup-notification))))

(define-public goffice
  (package
    (name "goffice")
    (version "0.10.50")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/goffice/"
                                  (version-major+minor version)  "/"
                                  "goffice-" version ".tar.xz"))
              (sha256
               (base32 "1p5zbj7cbcfcxd6l8pnph54p6ah1bwf146y810j4bcq8ggf3sp1c"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                  ; 4.0 MiB of gtk-doc
    (arguments
     '(#:configure-flags (list (string-append "--with-html-dir="
                                              (assoc-ref %outputs "doc")
                                              "/share/gtk-doc/html"))))
    (inputs
     (list gtk+ libgsf librsvg libxslt libxml2))
    (native-inputs
     (list intltool
           `(,glib "bin") pkg-config))
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
     (list libgsf))
    (inputs
     `(("gtk" ,gtk+-2)
       ,@(alist-delete "gtk" (package-inputs goffice))))))

(define-public gnumeric
  (package
    (name "gnumeric")
    (version "1.12.50")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnumeric/"
                                  (version-major+minor version)  "/"
                                  "gnumeric-" version ".tar.xz"))
              (sha256
               (base32
                "1f0lrj5msg80pgjp38jj6rddf352gwddgip7z4lki66n3fx1k23m"))))
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
     (list glib
           gtk+
           goffice
           libgsf
           librsvg
           libxml2
           libxslt
           python
           python-pygobject
           zlib))
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

(define-public drawing
  (package
    (name "drawing")
    (version "0.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/maoschanz/drawing")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wz9p47riyy3h8b0sqsb6bx416hc6d1a1wyzlfmsxkrqrkwcjcm8"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((prog (string-append (assoc-ref outputs "out")
                                        "/bin/drawing"))
                   (pylib (string-append (assoc-ref outputs "out")
                                         "/lib/python"
                                         ,(version-major+minor
                                           (package-version python))
                                         "/site-packages")))
               (wrap-program prog
                 `("PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH") ,pylib))
                 `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))))))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("pkg-config" ,pkg-config)))
    (inputs
     (list librsvg
           gsettings-desktop-schemas
           gtk+
           pango
           python-pycairo
           python-pygobject))
    (home-page "https://maoschanz.github.io/drawing/")
    (synopsis "Basic image editor for GNOME")
    (description
     "Drawing is a basic image editor aiming at the GNOME desktop.")
    (license license:gpl3+)))

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
    (version "41.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version) "/" name "-"
                           version ".tar.xz"))
       (sha256
        (base32 "1x99i7kdvd8hbxcs5rfrq7nw6r9bfzaw263zaigjjj04h6gc1vp6"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson_post_install.py"
               (("gtk-update-icon-cache") "true"))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a writable HOME.
             (setenv "HOME" (getcwd)))))))
    (inputs
     `(("gtk+" ,gtk+)
       ("gcr" ,gcr)
       ("gnupg" ,gnupg)
       ("gpgme" ,gpgme)
       ("openldap" ,openldap)
       ("openssh" ,openssh)
       ("avahi" ,avahi)
       ("libhandy" ,libhandy)
       ("libpwquality" ,libpwquality)
       ("libsecret" ,libsecret)
       ("libsoup" ,libsoup-minimal-2)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("vala" ,vala)
       ("xmllint" ,libxml2)))
    (home-page "https://wiki.gnome.org/Apps/Seahorse")
    (synopsis "Manage encryption keys and passwords in the GNOME keyring")
    (description
     "Seahorse is a GNOME application for managing encryption keys and
passwords in the GNOME keyring.")
    (license license:gpl2+)))

(define-public vala
  (package
    (name "vala")
    (version "0.54.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/vala/"
                                  (version-major+minor version) "/"
                                  "vala-" version ".tar.xz"))
              (sha256
               (base32
                "048k5c6c6y7jyb961krnrb7m0kghr0yrkpnfx3j5ckbx652yfkc8"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:configure-flags '("--enable-coverage")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "doc/manual"
               (substitute* '("manual.xml" "version.xml.in")
                 (("http://www.oasis-open.org/docbook/xml/4.4/")
                  (string-append (assoc-ref inputs "docbook-xml")
                                 "/xml/dtd/docbook/"))))))
         (add-before 'check 'pre-check
           (lambda _
             (setenv "CC" "gcc")
             (substitute* "valadoc/tests/libvaladoc\
/tests-extra-environment.sh"
               (("export PKG_CONFIG_PATH=" m)
                (string-append m "$PKG_CONFIG_PATH:"))))))))
    (native-inputs
     `(("bison" ,bison)
       ("dbus" ,dbus)                   ; for dbus tests
       ("docbook-xml" ,docbook-xml-4.4)
       ("docbook-xsl" ,docbook-xsl)
       ("flex" ,flex)
       ("gobject-introspection" ,gobject-introspection) ; for gir tests
       ("help2man" ,help2man)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     `(("glib" ,glib)                   ; required by libvala-0.40.pc
       ("libgvc" ,graphviz)))
    (home-page "https://wiki.gnome.org/Projects/Vala/")
    (synopsis "Compiler using the GObject type system")
    (description "Vala is a programming language using modern high level
abstractions without imposing additional runtime requirements and without using
a different ABI compared to applications and libraries written in C.  Vala uses
the GObject type system and has additional code generation routines that make
targeting the GNOME stack simple.")
    (license license:lgpl2.1+)))

;;; An older variant kept to build libsoup-minimal-2.
(define-public vala-0.52
  (package/inherit vala
    (version "0.52.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/vala/"
                                  (version-major+minor version) "/"
                                  "vala-" version ".tar.xz"))
              (sha256
               (base32
                "12y6p8wdjp01vmfhxg2cgh32xnyqq6ivblvrar9clnj6vc867qhx"))))))

(define-public vte
  (package
    (name "vte")
    (version "0.64.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/vte/"
                                  (version-major+minor version) "/"
                                  "vte-" version ".tar.xz"))
              (sha256
               (base32
                "063ys3330cvhbz1dln9irhciavb2prw098lvhpcc2rx6cl5q4g1b"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       '("-Dvapi=true"
         "-D_systemd=false")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)
       ("vala" ,vala)
       ("gobject-introspection" ,gobject-introspection)
       ("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("gperf" ,gperf)
       ("python" ,python)
       ("xmllint" ,libxml2)))
    (propagated-inputs
     (list gtk+ ; required by vte-2.91.pc
           gnutls ; ditto
           pcre2))               ; ditto
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
    (version "0.58.2.a")
    (home-page "https://github.com/thestinger/vte-ng")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rnm5c6m3abbm81jsfdas0y80z299ny54gr4syn4bfrms3s4g19l"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags #~(list "-Ddocs=false")))
  (synopsis "Enhanced VTE terminal widget")
  (description
   "VTE is a library (libvte) implementing a terminal emulator widget for
GTK+, this fork provides additional functions exposed for keyboard text
selection and URL hints.")))

;; Stable version for gtk2, required by gnurobots and lxterminal as of 2020-07.
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
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-python")))
    (native-inputs
     (list pkg-config intltool
           `(,glib "bin")))   ; for glib-genmarshal, etc.
    (propagated-inputs
     (list gtk+-2 ; required by libvte.pc
           ncurses)))) ; required by libvte.la

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
     ;; Disable -Werror and such, to avoid build failures on compilation
     ;; warnings.
     '(#:configure-flags '("--enable-compile-warnings=minimum"
                           "CFLAGS=-O2 -g -fcommon")
       #:phases
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
     (list libxml2
           gtk-vnc
           gnome-keyring
           libsecret
           freerdp
           spice
           spice-gtk
           telepathy-glib
           vte))
    (home-page "https://wiki.gnome.org/Apps/Vinagre")
    (synopsis "Remote desktop viewer for GNOME")
    (description "Vinagre is a remote display client supporting the VNC, SPICE
and RDP protocols.")
    (license license:gpl3+)))

(define-public dconf
  (package
    (name "dconf")
    (version "0.40.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0cs5nayg080y8pb9b7qccm1ni8wkicdmqp1jsgc22110r6j24zyg"))))
    (build-system meson-build-system)
    (propagated-inputs
     ;; In Requires of dconf.pc.
     (list glib))
    (inputs
     (list gtk+ dbus))
    (native-inputs
     `(("bash-completion" ,bash-completion)
       ("libxslt" ,libxslt)                     ;for xsltproc
       ("libxml2" ,libxml2)                     ;for XML_CATALOG_FILES
       ("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)
       ("glib:bin" ,glib "bin")
       ("gtk-doc" ,gtk-doc/stable)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("vala" ,vala)))
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags '("-Dgtk_doc=true")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'increase-test-timeout
                    (lambda _
                      ;; On big-memory systems, the engine test may take
                      ;; much longer than the default of 30 seconds.
                      (substitute* "tests/meson.build"
                        (("test\\(unit_test\\[0\\], exe" all)
                         (string-append all ", timeout: 300"))))))))
    (home-page "https://developer.gnome.org/dconf/")
    (synopsis "Low-level GNOME configuration system")
    (description "Dconf is a low-level configuration system.  Its main purpose
is to provide a backend to GSettings on platforms that don't already have
configuration storage systems.")
    (license license:lgpl2.1+)))

(define-public json-glib-minimal
  (package
    (name "json-glib-minimal")
    (version "1.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/json-glib/"
                                  (version-major+minor version)
                                  "/json-glib-" version ".tar.xz"))
              (sha256
               (base32
                "092g2dyy1hhl0ix9kp33wcab0pg1qicnsv0cj5ms9g9qs336cgd3"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t))           ; To wrap binaries and/or compile schemas
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib" ,glib "bin")             ;for glib-mkenums and glib-genmarshal
       ("pkg-config" ,pkg-config)))
    (inputs
     (list bash-minimal))
    (propagated-inputs
     (list glib))                 ;according to json-glib-1.0.pc
    (home-page "https://wiki.gnome.org/Projects/JsonGlib")
    (synopsis "Glib and GObject implementation of JSON")
    (description "JSON-GLib is a library providing serialization and
described by RFC 4627.  It implements a full JSON parser and generator using
GLib and GObject, and integrates JSON with GLib data types.")
    (license license:lgpl2.1+)))

(define-public json-glib
  (package/inherit json-glib-minimal
    (name "json-glib")
    (outputs (cons "doc" (package-outputs json-glib-minimal)))
    (arguments
     (substitute-keyword-arguments (package-arguments json-glib-minimal)
       ((#:configure-flags _)
        `(list "-Ddocs=true"
               "-Dman=true"
               ,@(if (%current-target-system)
                     ;; If enabled, gtkdoc-scangobj will try to execute a
                     ;; cross-compiled binary.
                     '("-Dgtk_doc=disabled"
                       ;; Trying to build introspection data when cross-compiling
                       ;; causes errors during linking.
                       "-Dintrospection=disabled")
                     '())))
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'unpack 'patch-docbook
             (lambda* (#:key native-inputs inputs #:allow-other-keys)
               (with-directory-excursion "doc"
                 (substitute* (find-files "." "\\.xml$")
                   (("http://www.oasis-open.org/docbook/xml/4\\.3/")
                    (string-append (assoc-ref (or native-inputs inputs)
                                              "docbook-xml")
                                   "/xml/dtd/docbook/")))
                 (substitute* "meson.build"
                   (("http://docbook.sourceforge.net/release/xsl/current/")
                    (string-append (assoc-ref (or native-inputs inputs)
                                              "docbook-xsl")
                                   "/xml/xsl/docbook-xsl-1.79.2/"))))))
           ;; When cross-compiling, there are no docs to move.
           ,(if (%current-target-system)
                '(add-after 'install 'stub-docs
                   (lambda* (#:key outputs #:allow-other-keys)
                     ;; The daemon doesn't like empty output paths.
                     (mkdir (assoc-ref outputs "doc"))))
                '(add-after 'install 'move-docs
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out"))
                            (doc (assoc-ref outputs "doc")))
                       (mkdir-p (string-append doc "/share"))
                       (rename-file
                        (string-append out "/share/gtk-doc")
                        (string-append doc "/share/gtk-doc"))))))))))
    (native-inputs
     (append
         `(("docbook-xml" ,docbook-xml-4.3)
           ("docbook-xsl" ,docbook-xsl)
           ("gobject-introspection" ,gobject-introspection)
           ("gtk-doc" ,gtk-doc)
           ("xsltproc" ,libxslt))
         (package-native-inputs json-glib-minimal)))))

(define-public libxklavier
  (package
    (name "libxklavier")
    (version "5.4")
    (source (origin
              ;; Note: There's no tarball at ftp.gnome.org for this version.
              (method git-fetch)
              (uri (git-reference
                    (url "https://anongit.freedesktop.org/git/libxklavier")
                    (commit (string-append "libxklavier-" version))))
              (sha256
               (base32
                "1w1x5mrgly2ldiw3q2r6y620zgd89gk7n90ja46775lhaswxzv7a"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--with-xkb-base="
                            (assoc-ref %build-inputs "xkeyboard-config")
                            "/share/X11/xkb")
             "--disable-xmodmap-support")))
    (native-inputs
     `(("glib:bin"              ,glib "bin") ; for glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config"            ,pkg-config)
       ("gtk-doc" ,gtk-doc/stable)
       ("intltool" ,intltool)
       ("which" ,which)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (propagated-inputs
     ;; Required by libxklavier.pc.
     (list glib libxml2))
    (inputs
     (list iso-codes libxi libxkbfile xkbcomp xkeyboard-config))
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
     (list pkg-config))
    (inputs
     (list python-2 python2-pygtk librsvg))
    (home-page "https://www.gnome.org")
    (synopsis "Python bindings to librsvg")
    (description
     "This package provides Python bindings to librsvg, the SVG rendering
library.")

    ;; This is the license of the rsvg bindings.  The license of each module
    ;; of gnome-python-desktop is given in 'COPYING'.
    (license license:lgpl2.1+)))

(define-public glib-networking
  (package
    (name "glib-networking")
    (version "2.70.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/glib-networking/"
                                  (version-major+minor version) "/"
                                  "glib-networking-" version ".tar.xz"))
              (sha256
               (base32
                "0dbg1na239mbavn4hknkax5sns9q2dbdnqw9wcpmhv58mzkhid36"))
              (patches
               (search-patches "glib-networking-gnutls-binding.patch"))))
    (build-system meson-build-system)
    (arguments
     (if (target-64bit?)
         '()
         (list #:phases
               #~(modify-phases %standard-phases
                   (add-after 'unpack 'work-around-32-bit-time-t
                     (lambda _
                       (invoke "patch" "--force" "-p1" "-i"
                               #$(local-file
                                  (search-patch
                                   "glib-networking-32-bit-time.patch")))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)))
    (inputs
     (list glib gnutls gsettings-desktop-schemas libproxy))
    (home-page "https://wiki.gnome.org/Projects/GLib")
    (synopsis "Network extensions for GLib")
    (description
     "Glib-networking contains the implementations of certain GLib networking
features that cannot be implemented directly in GLib itself because of their
dependencies.  Currently it contains GnuTLS and OpenSSL-based implementations of
GTlsBackend, a libproxy-based implementation of GProxyResolver,
GLibproxyResolver, and a GNOME GProxyResolver that uses the proxy information
from the GSettings schemas in gsettings-desktop-schemas.")
    (license license:lgpl2.1+)))

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
       ("libsoup" ,libsoup-minimal-2)
       ("libxml2" ,libxml2)))
    (home-page "https://www.gtk.org/")
    (synopsis "RESTful web api query library")
    (description
     "This library was designed to make it easier to access web services that
claim to be \"RESTful\".  It includes convenience wrappers for libsoup and
libxml to ease remote use of the RESTful API.")
    (license license:lgpl2.1+)))

;;; A minimal version of libsoup used to prevent a cycle with Inkscape.
(define-public libsoup-minimal
  (package
    (name "libsoup-minimal")
    (version "3.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libsoup/"
                                  (version-major+minor version) "/"
                                  "libsoup-" version ".tar.xz"))
              (sha256
               (base32
                "0ysnvvfd2f6w2z6g31spqqb8wgyamixc7mryzbbpyw0z15g8plsv"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Dgtk_doc=false")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'adjust-tests
           (lambda _
             ;; This test fails due to missing /etc/nsswitch.conf
             ;; in the build environment.
             (substitute* "tests/socket-test.c"
               ((".*/sockets/unconnected.*") ""))

             ;; These fail because "subdomain.localhost" does not resolve in
             ;; the build environment.  Moreover, the hsts-test suite fails on
             ;; i686-linux because of errors from `session_get_uri' like
             ;; "Unexpected status 200 OK (expected 301 Moved Permanently)"
             ;; (see: https://gitlab.gnome.org/GNOME/libsoup/-/issues/239).
             (substitute* "tests/meson.build"
               ((".*'name': 'hsts'.*") ""))
             (substitute* "tests/hsts-db-test.c"
               ((".*/hsts-db/subdomains.*") "")))))))
    (native-inputs
     (list `(,glib "bin") ;for glib-mkenums
           gobject-introspection
           intltool
           pkg-config
           python-wrapper
           vala
           curl
           gnutls ;for 'certtool'
           httpd))
    (propagated-inputs
     ;; libsoup-3.0.pc refers to all of these (except where otherwise noted)
     (list brotli
           glib
           glib-networking ; for GIO runtime modules
           libpsl
           nghttp2 ;for pkg-config
           `(,nghttp2 "lib")
           libxml2
           sqlite
           zlib))
    (inputs
     (list mit-krb5 samba/fixed))     ; For ntlm_auth support
    (home-page "https://wiki.gnome.org/Projects/libsoup")
    (synopsis "GLib-based HTTP Library")
    (description
     "LibSoup is an HTTP client/server library for GNOME.  It uses GObjects
and the GLib main loop, to integrate well with GNOME applications.")
    (license license:lgpl2.0+)))

;;; An older variant kept to build the 'rest' package.
(define-public libsoup-minimal-2
  (package/inherit libsoup-minimal
    (version "2.72.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libsoup/"
                                  (version-major+minor version) "/"
                                  "libsoup-" version ".tar.xz"))
              (sha256
               (base32
                "11skbyw2pw32178q3h8pi7xqa41b2x4k6q4k9f75zxmh8s23y30p"))))
    (arguments
     (substitute-keyword-arguments (package-arguments libsoup-minimal)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'disable-failing-tests
             (lambda _
               ;; Disable the SSL test, failing since 2.68 and resolved in
               ;; libsoup 3.
               (substitute* "tests/meson.build"
                 (("[ \t]*\\['ssl', true, \\[\\]\\],") ""))))))))
    (native-inputs
     (modify-inputs (package-native-inputs libsoup-minimal)
       (replace "vala" vala-0.52)))))

(define-public libsoup
  (package/inherit libsoup-minimal
    (name "libsoup")
    (outputs (cons "doc" (package-outputs libsoup-minimal)))
    (arguments
     (substitute-keyword-arguments (package-arguments libsoup-minimal)
       ((#:configure-flags configure-flags)
        `(cons "-Dgtk_doc=true"
               (delete "-Dgtk_doc=false" ,configure-flags)))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'patch-docbook-xml
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((xmldoc (string-append (assoc-ref inputs "docbook-xml")
                                            "/xml/dtd/docbook")))
                 (substitute* (find-files "docs/reference")
                   (("http://.*/docbookx\\.dtd")
                    (string-append xmldoc "/docbookx.dtd"))))))
           (add-after 'install 'move-doc
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (doc (assoc-ref outputs "doc")))
                 (mkdir-p (string-append doc "/share"))
                 (copy-recursively (string-append out "/share/gtk-doc")
                                   (string-append doc "/share/gtk-doc"))
                 (delete-file-recursively
                  (string-append out "/share/gtk-doc")))))))))
    (native-inputs (modify-inputs (package-native-inputs libsoup-minimal)
                     (prepend docbook-xml-4.1.2 gtk-doc)))))

(define-public libsecret
  (package
    (name "libsecret")
    (version "0.20.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/libsecret/"
                    (version-major+minor version) "/"
                    "libsecret-" version ".tar.xz"))
              (sha256
               (base32
                "0a4xnfmraxchd9cq5ai66j12jv2vrgjmaaxz25kl031jvda4qnij"))))
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
     (list glib)) ; required by libsecret-1.pc
    (inputs
     ;; The ‘build’ phase complains about missing docbook-xml-4.2 but adding it
     ;; doesn't seem to affect the build result.
     (list docbook-xsl libgcrypt libxml2)) ; for XML_CATALOG_FILES
    (home-page "https://wiki.gnome.org/Projects/Libsecret/")
    (synopsis "GObject bindings for \"Secret Service\" API")
    (description
     "Libsecret is a GObject based library for storing and retrieving passwords
and other secrets.  It communicates with the \"Secret Service\" using DBus.")
    (license license:lgpl2.1+)))

(define-public five-or-more
  (package
    (name "five-or-more")
    (version "3.32.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/five-or-more/"
                           (version-major+minor version) "/"
                           "five-or-more-" version ".tar.xz"))
       (sha256
        (base32 "19pf8wzbf3ciqf2k4bj9sddvyhckfd62x86pnqr6s8h4vn9jc6ii"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") (which "true")))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("appstream-glib" ,appstream-glib)
       ("desktop-file-utils" ,desktop-file-utils)
       ("glib:bin" ,glib "bin") ; for glib-compile-resources
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("vala" ,vala)
       ("xmllint" ,libxml2)))
    (inputs
     (list gtk+ libgnome-games-support librsvg))
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
    (version "40.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "0sf6kdvhr4pr3hddnj6ql9larz2wy108sri31id6x9g459nbly8z"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "build-aux/meson_post_install.py"
               (("gtk-update-icon-cache") (which "true"))))))))
    (native-inputs
     `(("glib:bin" ,glib "bin")       ; for glib-compile-resources
       ("pkg-config" ,pkg-config)
       ("desktop-file-utils" ,desktop-file-utils)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("vala" ,vala)
       ("yelp" ,yelp)
       ("appstream-glib" ,appstream-glib)))
    (inputs
     (list gtk+ libgnome-games-support librsvg))
    (home-page "https://wiki.gnome.org/Apps/Mines")
    (synopsis "Minesweeper game")
    (description
     "Mines (previously gnomine) is a puzzle game where you locate mines
floating in an ocean using only your brain and a little bit of luck.")
    (license license:gpl2+)))

(define-public gnome-multi-writer
  (package
    (name "gnome-multi-writer")
    (version "3.35.90")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/gnome-multi-writer/"
                           (version-major+minor version) "/"
                           "gnome-multi-writer-" version ".tar.xz"))
       (sha256
        (base32
         "07vgzjjdrxcp7h73z13h9agafxb4vmqx5i81bcfyw0ilw9kkdzmp"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-post-install
           (lambda _
             (substitute* "meson.build"
               (("meson.add_install_script" &) (string-append "# " &)))
             #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (inputs
     (list gtk+
           glib ; for gio
           gusb
           udisks
           libgudev
           libcanberra
           polkit))
    (home-page "https://wiki.gnome.org/Apps/MultiWriter")
    (synopsis "Write to multiple USB devices at once")
    (description
     "MultiWriter can be used to write an ISO file to multiple USB devices at
once.")
    (license license:gpl2+)))

(define-public gnome-sudoku
  (package
    (name "gnome-sudoku")
    (version "40.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "18slsxifad5cjz4fqi818i66jc9b7kzgn01qxa0ra4y7wcqha4in"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "build-aux/post_install.py"
               (("gtk-update-icon-cache") (which "true"))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("desktop-file-utils" ,desktop-file-utils)
       ("glib:bin" ,glib "bin") ; for glib-compile-resources
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("vala" ,vala)
       ("xmllint" ,libxml2)))
    (inputs
     (list gtk+ json-glib libgee librsvg qqwing))
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
    (version "3.40.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "08hsic7sn32xw12i3j0ard2bhfhp8gmzqm0pa8xzl5l1jhzsmsfb"))))
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
     `(("docbook-xsl" ,docbook-xsl)
       ("pkg-config" ,pkg-config)
       ("desktop-file-utils" ,desktop-file-utils)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("libxslt" ,libxslt)
       ("xmllint" ,libxml2)))
    (propagated-inputs
     (list dconf))
    (inputs
     (list gtk+
           vte
           gnutls
           gsettings-desktop-schemas
           `(,util-linux "lib")
           vala))
    (home-page "https://wiki.gnome.org/Apps/Terminal")
    (synopsis "Terminal emulator")
    (description
     "GNOME Terminal is a terminal emulator application for accessing a
UNIX shell environment which can be used to run programs available on
your system.

It supports several profiles, multiple tabs and implements several
keyboard shortcuts.")
    (license license:gpl3+)))

(define-public colord-minimal
  (package
    (name "colord-minimal")
    (version "1.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.freedesktop.org/software/colord/releases/"
                           "colord-" version ".tar.xz"))
       (sha256
        (base32 "05sydi6qqqx1rrqwnga1vbg9srkf89wdcfw5w4p4m7r37m2flx5p"))))
    (build-system meson-build-system)
    (arguments
     '( ;; FIXME: One test fails:
       ;; /colord/icc-store (in lib/colord/colord-self-test-private):
       ;; Incorrect content type for /tmp/colord-vkve/already-exists.icc, got
       ;; application/x-zerosize
       #:tests? #f
       #:glib-or-gtk? #t
       #:configure-flags (list "-Dargyllcms_sensor=false" ;requires spotread
                               "-Dbash_completion=false"
                               "-Ddaemon_user=colord"
                               "-Ddocs=false"
                               "-Dlocalstatedir=/var"
                               "-Dman=false"
                               "-Dsane=true"
                               "-Dsystemd=false") ;no systemd
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-build-system
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "rules/meson.build"
               (("udev.get_pkgconfig_variable\\('udevdir'\\)")
                (string-append "'" (assoc-ref outputs "out") "/lib/udev'")))))
         (add-before 'configure 'set-sqlite3-file-name
           (lambda* (#:key inputs #:allow-other-keys)
             ;; "colormgr dump" works by invoking the "sqlite3" command.
             ;; Record its absolute file name.
             (let ((sqlite (assoc-ref inputs "sqlite")))
               (substitute* "client/cd-util.c"
                 (("\"sqlite3\"")
                  (string-append "\"" sqlite "/bin/sqlite3\"")))))))))
    (native-inputs
     `(("glib:bin" ,glib "bin")         ; for glib-compile-resources, etc.
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (propagated-inputs
     ;; colord.pc refers to all these.
     `(("glib" ,glib)
       ("lcms" ,lcms)
       ("udev" ,eudev)))
    (inputs
     `(("dbus-glib" ,dbus-glib)
       ("gobject-introspection" ,gobject-introspection)
       ("gusb" ,gusb-minimal)
       ("libgudev" ,libgudev)
       ("libusb" ,libusb)
       ("polkit" ,polkit)
       ("python" ,python-wrapper)
       ("sqlite" ,sqlite)
       ("sane-backends" ,sane-backends)))
    (home-page "https://www.freedesktop.org/software/colord/")
    (synopsis "Color management service")
    (description "Colord is a system service that makes it easy to manage,
install and generate color profiles to accurately color manage input and
output devices.")
    (license license:gpl2+)))

(define-public colord
  (package/inherit colord-minimal
    (name "colord")
    (arguments
     (substitute-keyword-arguments
         (package-arguments colord-minimal)
       ((#:configure-flags flags)
        `(begin
           (use-modules (srfi srfi-1))
           (append '("-Dbash_completion=true"
                     "-Ddocs=true"
                     "-Dman=true"
                     "-Dvapi=true")
               (fold delete ,flags '("-Dbash_completion=false"
                                     "-Ddocs=false"
                                     "-Dman=false")))))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'fix-bash-completion-dir
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "data/meson.build"
                 (("bash_completion.get_pkgconfig_variable\
\\('completionsdir'\\)")
                  (string-append "'" (assoc-ref outputs "out")
                                 "/etc/bash_completion.d'")))))))))
    (native-inputs
     (append
         `(("bash-completion" ,bash-completion)
           ("docbook-xsl-ns" ,docbook-xsl-ns)
           ("gtk-doc" ,gtk-doc/stable)
           ("libxml2" ,libxml2)         ;for XML_CATALOG_FILES
           ("libxslt" ,libxslt)
           ("vala" ,vala))              ;for VAPI, needed by simple-scan
         (package-native-inputs colord-minimal)))))

(define-public geoclue
  (package
    (name "geoclue")
    (version "2.5.7")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://gitlab.freedesktop.org/geoclue/geoclue/-/archive/"
                       version "/geoclue-" version ".tar.bz2"))
       (sha256
        (base32 "09ww26a76kanldhgwnn5sqy9ly2940k45zpp68spykkv2zjdpivc"))
       (patches (search-patches "geoclue-config.patch"))))
    (build-system meson-build-system)
    (arguments
     '(#:configure-flags (list "-Ddbus-srv-user=geoclue")))
    (native-inputs
     (list pkg-config
           gobject-introspection
           modem-manager
           libnotify
           gtk-doc/stable
           intltool))
    (inputs
     `(("avahi" ,avahi)
       ("glib:bin" ,glib "bin")
       ("glib-networking" ,glib-networking)
       ("json-glib" ,json-glib)
       ("libsoup" ,libsoup-minimal-2)))
    (home-page "https://gitlab.freedesktop.org/geoclue/geoclue/-/wikis/home")
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
    (version "3.26.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/geocode-glib/"
                                  (version-major+minor version) "/"
                                  "geocode-glib-" version ".tar.xz"))
              (sha256
               (base32
                "1l8g0f13xgkrk335afr9w8k46mziwb2jnyhl07jccl5yl37q9zh1"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The tests require a bunch of locales.
         (add-before 'check 'set-locales
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "GUIX_LOCPATH"
                     (search-input-directory inputs "lib/locale")))))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-mkenums
       ("glibc-locales" ,glibc-locales) ; for tests
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc/stable)
       ("pkg-config" ,pkg-config)
       ("json-glib" ,json-glib)))
    (propagated-inputs
     ;; geocode-glib-1.0.pc refers to GIO.
     (list glib))
    (inputs
     `(("libsoup" ,libsoup-minimal-2)))
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
    (version "0.99.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/upower/upower")
             (commit (string-append "UPOWER_"
                                    (string-map (match-lambda (#\. #\_)
                                                              (chr chr))
                                                version)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0khwg6jpq81dcylkpi7cn75bjzif0q9qscwvirrk41krma23iddj"))
       (patches (search-patches "upower-builddir.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Upstream commit
           ;; <https://cgit.freedesktop.org/upower/commit/?id=18457c99b68786cd729b315723d680e6860d9cfa>
           ;; moved 'dbus-1/system.d' from etc/ to share/.  However,
           ;; 'dbus-configuration-directory' in (gnu services dbus)
           ;; expects it in etc/.  Thus, move it back to its previous
           ;; location.
           (substitute* "src/Makefile.am"
             (("^dbusconfdir =.*$")
              "dbusconfdir = $(sysconfdir)/dbus-1/system.d\n"))))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((umockdev (string-append (assoc-ref inputs "umockdev")
                                            "/lib")))
               (setenv "LD_LIBRARY_PATH" umockdev)))))
       #:configure-flags (list "--localstatedir=/var"
                               (string-append "--with-udevrulesdir="
                                              (assoc-ref %outputs "out")
                                              "/lib/udev/rules.d"))))
    (native-inputs
     (list autoconf
           automake
           gobject-introspection
           gtk-doc
           intltool
           libtool
           pkg-config
           python
           which ; for ./autogen.sh
           ;; For tests.
           python-dbus
           python-dbusmock
           python-packaging
           python-pygobject
           umockdev
           ;; For man pages.
           docbook-xsl
           libxslt ; for 'xsltproc'
           libxml2))           ; for 'XML_CATALOG_FILES'
    (inputs
     (list dbus-glib libgudev libusb))
    (propagated-inputs
     ;; In Requires of upower-glib.pc.
     (list glib))
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
    (version "40.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1rkf4yv43qcahyx7bismdv6z2vh5azdnm1fqfmnzrada9cm8ykna"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f ; one of two tests requires network access
       #:configure-flags
       `(,(string-append "-Dzoneinfo_dir="
                         (assoc-ref %build-inputs "tzdata")
                         "/share/zoneinfo"))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-mkenums
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("vala" ,vala)
       ("intltool" ,intltool)
       ("python-pygobject" ,python-pygobject)))
    (propagated-inputs
     ;; gweather-3.0.pc refers to GTK+, GDK-Pixbuf, GLib/GObject, libxml, and
     ;; libsoup.
     `(("gtk+" ,gtk+)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("libxml2" ,libxml2)
       ("libsoup" ,libsoup-minimal-2)
       ("geocode-glib" ,geocode-glib)))
    (inputs
     (list tzdata))
    (home-page "https://wiki.gnome.org/action/show/Projects/LibGWeather")
    (synopsis "Location, time zone, and weather library for GNOME")
    (description
     "libgweather is a library to access weather information from online
services for numerous locations.")
    (license license:gpl2+)))

(define-public gnome-settings-daemon
  (package
    (name "gnome-settings-daemon")
    (version "41.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "111qcvlqjpasnswdniy5n53wyrs1y7a91qx1w4nsppnizdhn7jp6"))
       (patches (search-patches "gnome-settings-daemon-gc.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       (list (string-append "-Dudev_dir="
                            (assoc-ref %outputs "out") "/lib/udev")
             "-Dsystemd=false"
             ;; Otherwise, the RUNPATH will lack the final path component.
             (string-append "-Dc_link_args=-Wl,-rpath="
                            (assoc-ref %outputs "out")
                            "/lib/gnome-settings-daemon-3.0"))

       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'set-baobab-file-name
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; Hard-code the file name of Baobab instead of looking
                      ;; it up in $PATH.  This ensures users get the "Examine"
                      ;; button in the low disk space notification of GDM even
                      ;; if they don't have GNOME in their main profile.
                      (let ((baobab (assoc-ref inputs "baobab")))
                        (substitute* "plugins/housekeeping/gsd-disk-space.c"
                          (("g_find_program_in_path \\(DISK_SPACE_ANALYZER\\)")
                           (string-append "g_strdup (\"" baobab
                                          "/bin/baobab\")")))))))

       ;; Color management test can't reach the colord system service.
       #:tests? #f))
    (native-inputs
     `(("glib:bin" ,glib "bin")     ; for glib-mkenums
       ("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("xsltproc" ,libxslt)
       ("libxml2" ,libxml2)                       ;for XML_CATALOG_FILES
       ("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)))
    (inputs
     (list alsa-lib
           baobab
           colord
           libgudev
           upower
           polkit
           pulseaudio
           libcanberra
           libx11
           libxtst
           lcms
           libnotify
           geoclue
           geocode-glib
           libgweather
           gnome-desktop
           nss
           cups
           gsettings-desktop-schemas
           libwacom
           (librsvg-for-system)
           xf86-input-wacom
           wayland
           network-manager
           gcr
           modem-manager))
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
   (version "3.26.6")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/totem-pl-parser/"
                                (version-major+minor version) "/"
                                "totem-pl-parser-" version ".tar.xz"))
            (sha256
             (base32
              "075csd5x0frgf93jvhlqiwv5i0qm24zz3iw17jj7v7fgsml0zpy0"))))
   (build-system meson-build-system)
   (arguments
    ;; FIXME: Tests require gvfs.
    `(#:tests? #f))
   (native-inputs
    (list intltool
          `(,glib "bin") gobject-introspection pkg-config))
   (propagated-inputs
    (list glib gmime libarchive libgcrypt libxml2))
   (inputs
    (list libsoup))
   (home-page "https://projects.gnome.org/totem")
   (synopsis "Library to parse and save media playlists for GNOME")
   (description "Totem-pl-parser is a GObjects-based library to parse and save
playlists in a variety of formats.")
   (license license:lgpl2.0+)))

(define-public aisleriot
  (package
    (name "aisleriot")
    (version "3.22.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/aisleriot/"
                                  (version-major+minor version) "/"
                                  "aisleriot-" version ".tar.xz"))
              (sha256
               (base32
                "0yzdh9cw5cjjgvfh75bihl968czlgfmpmn1z0fdk88sgvpjgzwji"))))
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
     (list gtk+ guile-2.2 libcanberra librsvg))
    (home-page "https://wiki.gnome.org/Apps/Aisleriot")
    (synopsis "Solitaire card games")
    (description
     "Aisleriot (also known as Solitaire or sol) is a collection of card games
which are easy to play with the aid of a mouse.")
    (license license:gpl3+)))

(define-public amtk
  (package
    (name "amtk")
    (version "5.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/amtk/"
                                  (version-major+minor version) "/"
                                  "amtk-" version ".tar.xz"))
              (sha256
               (base32
                "12v3nj1bb7507ndprjggq0hpz8k719b4bwvl8sm43p3ibmn27anm"))))
    (build-system meson-build-system)
    (native-inputs
     (list gobject-introspection
           `(,glib "bin") ; for glib-mkenums
           gtk-doc/stable pkg-config))
    (inputs
     (list glib gtk+))
    (home-page "https://wiki.gnome.org/Projects/Amtk")
    (synopsis "Actions, Menus and Toolbars Kit for GTK+ applications")
    (description
     "Amtk is the acronym for @acronym{Amtk, Actions Menus and Toolbars Kit}.
It is a basic GtkUIManager replacement based on GAction.  It is suitable for
both a traditional UI or a modern UI with a GtkHeaderBar.")
    (license license:lgpl2.1+)))

(define-public devhelp
  (package
    (name "devhelp")
    (version "41.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1lk0gycjvs6gibhy0zs3ffkrkzrkyl5nkp7n60hgpa6syjq91apc"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson/meson_post_install.py"
               (("gtk-update-icon-cache") "true"))))
         (add-after 'unpack 'fix-devhelp-gir-inputs
           ;; It still mentions webkitgtk 4.0
           (lambda _
             (substitute* "devhelp/meson.build"
               (("'WebKit2-4.0'") "'WebKit2-4.1'")))))))
    (propagated-inputs
     (list gsettings-desktop-schemas))
    (native-inputs
     `(("intltool" ,intltool)
       ("itstool" ,itstool)
       ("gobject-introspection" ,gobject-introspection)
       ("glib:bin" ,glib "bin") ; for glib-mkmenus
       ("pkg-config" ,pkg-config)))
    (inputs
     (list amtk gsettings-desktop-schemas webkitgtk))
    (home-page "https://wiki.gnome.org/Apps/Devhelp")
    (synopsis "API documentation browser for GNOME")
    (description
     "Devhelp is an API documentation browser for GTK+ and GNOME.  It works
natively with GTK-Doc (the API reference system developed for GTK+ and used
throughout GNOME for API documentation).")
    (license license:gpl2+)))

(define-public devhelp-with-libsoup2
  (package/inherit devhelp
    (arguments
     (substitute-keyword-arguments (package-arguments devhelp)
       ((#:phases phases '%standard-phases)
        `(modify-phases %standard-phases
           (add-after 'unpack 'skip-gtk-update-icon-cache
             ;; Don't create 'icon-theme.cache'.
             (lambda _
               (substitute* "build-aux/meson/meson_post_install.py"
                 (("gtk-update-icon-cache") "true"))))))))
    (inputs
     `(("amtk" ,amtk)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("webkitgtk" ,webkitgtk-with-libsoup2)))))

(define-public cogl
  (package
    (name "cogl")
    (version "1.22.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/cogl/"
                           (version-major+minor version) "/"
                           "cogl-" version ".tar.xz"))
       (sha256
        (base32 "0nfph4ai60ncdx7hy6hl1i1cmp761jgnyjfhagzi0iqq36qb41d8"))))
    ;; NOTE: mutter exports a bundled fork of cogl, so when making changes to
    ;; cogl, corresponding changes may be appropriate in mutter as well.
    (build-system gnu-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin")     ; for glib-mkenums
       ("gobject-introspection" ,gobject-introspection)
       ("xorg-server" ,xorg-server-for-tests)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     (list glib
           gdk-pixbuf
           libx11
           libxext
           libxfixes
           libxdamage
           libxcomposite
           libxrandr))
    (inputs
     (list mesa
           cairo
           pango
           gstreamer
           gst-plugins-base
           wayland))
    (arguments
     `(#:disallowed-references (,xorg-server-for-tests)
       #:configure-flags (list "--enable-cogl-gst"
                               "--enable-wayland-egl-platform"
                               "--enable-wayland-egl-server"

                               ;; Arrange to pass an absolute file name to
                               ;; dlopen for libGL.so.
                               (string-append "--with-gl-libname="
                                              (assoc-ref %build-inputs "mesa")
                                              "/lib/libGL.so"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build-with-mesa-20
           (lambda _
             ;; Work around a problem with Mesa 20 where some macros used by
             ;; Cogl went missing from eglext.h.  This can likely be removed
             ;; for newer versions of Cogl or Mesa.
             ;; https://gitlab.gnome.org/GNOME/cogl/-/merge_requests/19
             (substitute* '("configure"
                            "cogl/winsys/cogl-winsys-egl-kms.c")
               (("#include <EGL/eglext.h>" all)
                (string-append all "\n#include <EGL/eglmesaext.h>\n")))
             #t))
         (add-before 'check 'start-xorg-server
                     (lambda* (#:key tests? inputs #:allow-other-keys)
                       (if tests?
                           (begin
                             ;; The test suite requires a running X server.
                             (system (format #f "~a/bin/Xvfb :1 +extension GLX &"
                                             (assoc-ref inputs "xorg-server")))
                             (setenv "DISPLAY" ":1")
                             #t)
                           (format #t "test suite not run~%"))
                       #t)))))
    (home-page "https://www.cogl3d.org")
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
     (list cogl
           cairo
           atk
           gtk+
           json-glib
           glib
           libxcomposite
           libxdamage
           libxext
           xinput))
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
     (list pkg-config gobject-introspection))
    (propagated-inputs
     ;; clutter-gtk.pc refers to all these.
     (list clutter gtk+))
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
     (list `(,glib "bin") ; for glib-mkenums
           pkg-config gobject-introspection))
    (inputs
     (list clutter gstreamer gst-plugins-base))
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
    (version "0.12.20")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/libchamplain/0.12/libchamplain-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0rihpb0npqpihqcdz4w03rq6xl7jdckfqskvv9diq2hkrnzv8ch2"))))
    (build-system meson-build-system)
    (native-inputs
     (list gobject-introspection pkg-config vala))
    (propagated-inputs
     (list libsoup-minimal-2
           sqlite
           clutter
           clutter-gtk
           `(,glib "bin") ;glib-mkenums, etc.
           cairo
           gtk+
           glib))
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
     (list intltool pkg-config gobject-introspection))
    (inputs
     (list glib gdk-pixbuf sqlite))
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
    (version "1.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libgnome-games-support/"
                                  (version-major+minor version) "/"
                                  "libgnome-games-support-" version ".tar.xz"))
              (sha256
               (base32
                "11g1r3ppb9v8m3anks9gxf7fv1x38vmjiya3lr7zjjv328pb69d6"))))
    (build-system meson-build-system)
    (arguments
      '(#:glib-or-gtk? #t
        #:phases
          (modify-phases %standard-phases
            (add-before 'check 'pre-check
              (lambda _
                ;; Tests require a writable HOME.
                (setenv "HOME" (getcwd))
                #t)))))
    (native-inputs
     (list intltool pkg-config vala))
    (propagated-inputs
     ;; Required by libgnome-games-support-1.0.pc
     (list gtk+ libgee))
    (home-page "https://www.gnome.org/")
    (synopsis "Useful functionality shared among GNOME games")
    (description
     "libgnome-games-support is a small library intended for internal use by
GNOME Games, but it may be used by others.")
    (license license:lgpl3+)))

(define-public gnome-klotski
  (package
    (name "gnome-klotski")
    (version "3.34.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0sbx0bzy32lh2c9jp8v7gz788wn9y1la8mr5a7gf7370szsl4d4f"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson_post_install.py"
               (("gtk-update-icon-cache") (which "true")))
             #t)))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("glib:bin" ,glib "bin") ; for glib-compile-resources
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xmllint" ,libxml2)))
    (inputs
     (list gtk+ libgnome-games-support librsvg))
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
    (version "0.3.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/grilo/"
                           (version-major+minor version) "/"
                           "grilo-" version ".tar.xz"))
       (sha256
        (base32 "0ywjvh7xw4ql1q4fvl0q5n06n08pga1g1nc9l7c3x5214gr3fj6i"))))
    (build-system meson-build-system)
    (native-inputs
     (list `(,glib "bin") ; for glib-mkenums and glib-genmarshal
           intltool
           pkg-config
           gobject-introspection
           gtk-doc/stable
           vala))
    (inputs
     (list cyrus-sasl
           glib
           gtk+
           libxml2
           liboauth
           libsoup-minimal-2
           totem-pl-parser))
    (native-search-paths
     (list (search-path-specification
            (variable "GRL_PLUGIN_PATH")
            (files (list (string-append "lib/grilo-"
                                        (version-major+minor version)))))))
    (home-page "https://wiki.gnome.org/action/show/Projects/Grilo")
    (synopsis "Framework for discovering and browsing media")
    (description
     "Grilo is a framework focused on making media discovery and browsing easy
for application developers.")
    (license license:lgpl2.1+)))

(define-public grilo-plugins
  (package
    (name "grilo-plugins")
    (version "0.3.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "1y10nnd711qxwgpz6spzp1rnk50fyf11kk1n71a31ab4fgb0ahqw"))))
    (build-system meson-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gtk+:bin" ,gtk+ "bin")
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)))
    ;; TODO: ahavi, gstreamer
    (inputs
     `(("grilo" ,grilo)
       ;("gmime" ,gmime) ; unused
       ("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("gom" ,gom)
       ;("gssdp" ,gssdp) ; unused
       ;("gupnp" ,gupnp) ; unused
       ;("gupnp-av" ,gupnp-av) ; unused
       ("json-glib" ,json-glib)
       ("avahi" ,avahi)
       ("libgdata" ,libgdata)
       ("libmediaart" ,libmediaart)
       ;("librest" ,rest) ; unused
       ("libsoup" ,libsoup)
       ("python-pygobject" ,python-pygobject)
       ("totam-pl-parser" ,totem-pl-parser)
       ("tracker" ,tracker)
       ("tracker-miners" ,tracker-miners)))
    (arguments
     `(#:glib-or-gtk? #t
       ;;Disable lua-factory as it needs missing dependencies
       #:configure-flags '("-Denable-lua-factory=no")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-shell
           (lambda _
             (setenv "SHELL" (which "bash"))))
         ;; Disable the tracker test that requires the UPower daemon.
         (add-before 'configure 'fix-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "tests/tracker3/meson.build"
               (("'test_tracker3'.*") "")))))))
    (home-page "https://live.gnome.org/Grilo")
    (synopsis "Plugins for the Grilo media discovery library")
    (description
     "Grilo is a framework focused on making media discovery and browsing easy
for application developers.  This package provides plugins for common media
discovery protocols.")
    (license license:lgpl2.1+)))

(define-public totem
  (package
    (name "totem")
    (version "3.38.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/totem/"
                           (version-major+minor version) "/"
                           "totem-" version ".tar.xz"))
       (sha256
        (base32 "02510lvzvxvmpcs64k6sqix8ysl7sihhhwvp0vmfv7521ryczylg"))))
    (build-system meson-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("glib:bin" ,glib "bin")                   ;for 'glib-mkenums'
       ("itstool" ,itstool)
       ("xmllint" ,libxml2)
       ("xorg-server" ,xorg-server-for-tests)))
    (propagated-inputs
     (list dconf))
    (inputs
     (list gtk+
           gdk-pixbuf
           atk
           cairo
           dbus-glib
           clutter
           clutter-gtk
           clutter-gst
           xorgproto
           libxxf86vm
           libxtst
           libxrandr
           libxml2
           libsoup
           libpeas
           librsvg
           lirc
           gnome-desktop
           gstreamer
           gst-plugins-base
           gst-plugins-good
           gsettings-desktop-schemas
           adwaita-icon-theme
           python
           python-pygobject
           totem-pl-parser
           grilo
           grilo-plugins
           vala))
    (arguments
     `(#:glib-or-gtk? #t

       ;; Disable automatic GStreamer plugin installation via PackageKit and
       ;; all that.
       #:configure-flags '("-D" "enable-easy-codec-installation=no"

                           ;; Do not build .a files for the plugins, it's
                           ;; completely useless.  This saves 2 MiB.
                           "--default-library" "shared")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") "true"))
             #t))
         (add-before
          'install 'disable-cache-generation
          (lambda _
            (setenv "DESTDIR" "/")
            #t))
         (add-before
          'check 'pre-check
          (lambda _
            ;; Tests require a running X server.
            (system "Xvfb :1 &")
            (setenv "DISPLAY" ":1")
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
   (version "3.4.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/rhythmbox/"
                                (version-major+minor version) "/"
                                "rhythmbox-" version ".tar.xz"))
            (sha256
             (base32
              "142xcvw4l19jyr5i72nbnrihs953pvrrzcbijjn9dxmxszbv03pf"))))
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
                 (python-path       (getenv "GUIX_PYTHONPATH")))
             (wrap-program (string-append out "/bin/rhythmbox")
               `("GI_TYPELIB_PATH"        ":" prefix (,gi-typelib-path))
               `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))
               `("GRL_PLUGIN_PATH"        ":" prefix (,grl-plugin-path))
               `("GUIX_PYTHONPATH"             ":" prefix (,python-path))))
           #t)))))
   (propagated-inputs
    (list dconf))
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
      ("python-pygobject" ,python-pygobject)
      ("vala" ,vala)
      ("gmime" ,gmime)
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
      ("libsoup" ,libsoup-minimal-2)
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
   (version "40.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major version) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "0ddjwcd77nw0rxb5x5bz5hd671m8gya9827p8rsnb58x103kpai8"))
            ;; XXX: Remove when upgrading to 42.0
            (patches (search-patches "eog-update-libportal-usage.patch"))))
   (build-system meson-build-system)
   (arguments
    `(#:configure-flags
      ;; Otherwise, the RUNPATH will lack the final 'eog' path component.
      (list (string-append "-Dc_link_args=-Wl,-rpath="
                           (assoc-ref %outputs "out") "/lib/eog"))
      #:phases
      (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") "true"))))
        (add-after 'install 'wrap-eog
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out               (assoc-ref outputs "out"))
                  (gi-typelib-path   (getenv "GI_TYPELIB_PATH")))
              (wrap-program (string-append out "/bin/eog")
                `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))))))))
   (propagated-inputs
    (list dconf))
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
      ("libportal" ,libportal)
      ("libjpeg" ,libjpeg-turbo)
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
    (version "3.26.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/eog-plugins/"
                                  (version-major+minor version) "/"
                                  "eog-plugins-" version ".tar.xz"))
              (sha256
               (base32
                "1dcxdjp7x092wn0iq7975f9b05ksb5kl5mxmyiqmydrja91ryw40"))))
    (build-system gnu-build-system)
    (home-page "https://wiki.gnome.org/Apps/EyeOfGnome/Plugins")
    (synopsis "Extensions for the Eye of GNOME image viewer")
    (native-inputs
     (list pkg-config gettext-minimal))
    (inputs
     (list eog
           glib
           gtk+
           libpeas
           libexif
           libchamplain))
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
    (version "236")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "094mgjmwgsgqrr1i0vd20ynvlkihvs3vgbmpbrhswjsrdp86j0z5"))))
    (build-system meson-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-genmarshal, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     (list glib ; in Requires of gudev-1.0.pc
           eudev))               ; in Requires.private of gudev-1.0.pc
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
    (version "1.48.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gvfs/"
                                  (version-major+minor version) "/"
                                  "gvfs-" version ".tar.xz"))
              (sha256
               (base32
                "1hlxl6368h6nyqp1888szxs9hnpcw98k3h23dgqi29xd38klzsmj"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       (list "-Dsystemduserunitdir=no"
             "-Dtmpfilesdir=no"
             ;; Otherwise, the RUNPATH will lack the final path component.
             (string-append "-Dc_link_args=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib/gvfs"))))
    (native-inputs
     `(("glib:bin" ,glib "bin")         ;for glib-genmarshal, etc.
       ("gettext" ,gettext-minimal)
       ("gtk-doc" ,gtk-doc/stable)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("avahi" ,avahi)
       ("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)
       ("dbus" ,dbus)
       ("elogind" ,elogind)
       ("fuse" ,fuse-3)
       ("gcr" ,gcr)
       ("glib" ,glib)
       ("gnome-online-accounts" ,gnome-online-accounts)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("libarchive" ,libarchive)
       ("libbluray" ,libbluray)
       ("libcap" ,libcap)
       ("libcdio-paranoia" ,libcdio-paranoia)
       ("libgcrypt" ,libgcrypt)
       ("libgdata" ,libgdata)
       ("libgphoto2" ,libgphoto2)
       ("libgudev" ,libgudev)
       ("libimobiledevice" ,libimobiledevice)
       ("libmtp" ,libmtp)
       ("libnfs" ,libnfs)
       ("libsecret" ,libsecret)
       ("libsmbclient" ,samba)
       ("libsoup" ,libsoup)
       ("libxml2" ,libxml2)
       ("openssh" ,openssh)
       ("polkit" ,polkit)
       ("udisks" ,udisks)))
    (home-page "https://wiki.gnome.org/gvfs/")
    (synopsis "Userspace virtual file system for GIO")
    (description
     "GVFS is a userspace virtual file system designed to work with the I/O
abstraction of GIO.  It contains a GIO module that seamlessly adds GVFS
support to all applications using the GIO API.  It also supports exposing the
GVFS mounts to non-GIO applications using FUSE.

GVFS comes with a set of backends, including trash support, SFTP, SMB, HTTP,
DAV, and others.")
    (license license:lgpl2.0+)))

(define-public gusb-minimal
  (package
    (name "gusb-minimal")
    (version "0.3.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hughsie/libgusb")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ifhdqhpyxwsg0z9s1anj7cf5pya5qsqyp5ksh9n7mqwa4lrjkl8"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f          ;libusb fails to initialize.  Wonder what that is.
       #:configure-flags
       (cons "-Ddocs=false"
             (if ,(%current-target-system)
                 ;; Introspection data cannot currently be cross-compiled.
                 '("-Dintrospection=false"
                   ;; Requires introspection data.
                   "-Dvapi=false")
                 '()))))
    (native-inputs
     (list gobject-introspection pkg-config python vala))
    (propagated-inputs
     ;; Both of these are required by gusb.pc.
     (list glib libusb))
    (home-page "https://github.com/hughsie/libgusb")
    (synopsis "GLib binding for libusb1")
    (description
     "GUsb is a GObject wrapper for libusb1 that makes it easy to do
asynchronous control, bulk and interrupt transfers with proper cancellation
and integration into a mainloop.  This makes it easy to integrate low level
USB transfers with your high-level application or system daemon.")
    (license license:lgpl2.1+)))

(define-public gusb
  (package/inherit gusb-minimal
    (name "gusb")
    (arguments
     (substitute-keyword-arguments (package-arguments gusb-minimal)
       ((#:configure-flags flags)
        `(cons "-Ddocs=true"
               (delete "-Ddocs=false" ,flags)))))
    (native-inputs
     (cons `("gtk-doc" ,gtk-doc/stable)
           (package-native-inputs gusb-minimal)))))

(define-public simple-scan
  (package
    (name "simple-scan")
    (version "40.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/simple-scan/"
                           (version-major version) "/"
                           "simple-scan-" version ".tar.xz"))
       (sha256
        (base32 "0lrxmk8xqvrb5gn5mpgqij133giwvrgavy49m55d7xssrd91hmbw"))))
    (build-system meson-build-system)
    ;; TODO: Fix icons in home screen, About dialogue, and scan menu.
    (arguments
     `(#:glib-or-gtk? #t))
    (native-inputs
     (list gettext-minimal
           itstool
           `(,glib "bin")               ; glib-compile-schemas, etc.
           pkg-config
           python
           vala
           libxml2))
    (inputs
     (list gtk+
           zlib
           cairo
           colord
           gdk-pixbuf
           gusb
           libhandy
           sane-backends))
    (home-page "https://gitlab.gnome.org/GNOME/simple-scan")
    (synopsis "Document and image scanner")
    (description
     "Document Scanner is an easy-to-use application that lets you connect your
scanner and quickly capture images and documents in an appropriate format.  It
supports any scanner for which a suitable SANE driver is available, which is
almost all of them.")
    (license license:gpl3+)))

(define-public eolie
  (package
    (name "eolie")
    (version "0.9.101")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://adishatz.org/eolie/eolie-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1v8n21y75abdzsnx5idyd0q6yfb6cd0sqbknlbkwh5fdgvjzyvwn"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") "true"))
             #t))
         (add-after 'wrap 'wrap-more
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    ;; These libraries must be on LD_LIBRARY_PATH.
                    (libs '("gtkspell3" "webkitgtk" "libsoup" "libsecret"
                            "atk" "gtk+" "gsettings-desktop-schemas"
                            "gobject-introspection"))
                    (path (string-join
                           (map (lambda (lib)
                                  (string-append (assoc-ref inputs lib) "/lib"))
                                libs)
                           ":")))
               (wrap-program (string-append out "/bin/eolie")
                 `("LD_LIBRARY_PATH" ":" prefix (,path))
                 `("GUIX_PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH")))
                 `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))
             #t)))))
    (native-inputs
     `(("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("glib:bin" ,glib "bin")))
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
       ("libhandy" ,libhandy)
       ("libsecret" ,libsecret)
       ("gtkspell3" ,gtkspell3)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gnome-settings-daemon" ,gnome-settings-daemon) ; desktop-schemas are not enough
       ("webkitgtk" ,webkitgtk)))
    (home-page "https://wiki.gnome.org/Apps/Eolie")
    (synopsis "Web browser for GNOME")
    (description
     "Eolie is a new web browser for GNOME.  It features Firefox sync support,
a secret password store, an adblocker, and a modern UI.")
    (license license:gpl3+)))

(define-public epiphany
  (package
    (name "epiphany")
    (version "41.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/epiphany/"
                                  (version-major version) "/"
                                  "epiphany-" version ".tar.xz"))
              (sha256
               (base32
                "0k7b22zq3z1kllzqxgwsvwb1lp0j6rjb3k1hvhna3i573wc4mpji"))
              (patches
               (search-patches "epiphany-update-libportal-usage.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "post_install.py"
               (("gtk-update-icon-cache") "true"))))
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (substitute* "tests/meson.build"
               ;; embed_shell fails, because webkitgtk apparently no longer
               ;; supports overriding the ftp schema web_app_utils fails due
               ;; to missing network access.
               (("(embed_shell|web_app_utils)_test,")
                "find_program('sh'), args: ['-c', 'exit 77'],"))))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1"))))
       #:configure-flags
       ;; Otherwise, the RUNPATH will lack the final 'epiphany' path component.
       (list (string-append "-Dc_link_args=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib/epiphany"))))
    (propagated-inputs
     (list dconf))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils) ; for update-desktop-database
       ("glib:bin" ,glib "bin") ; for glib-mkenums
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("avahi" ,avahi)
       ("gcr" ,gcr)
       ("librsvg" ,librsvg) ; for loading SVG files
       ("glib-networking" ,glib-networking)
       ("gnome-desktop" ,gnome-desktop)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("json-glib" ,json-glib)
       ("iso-codes" ,iso-codes)
       ("libarchive" ,libarchive)
       ("libdazzle" ,libdazzle)
       ("libhandy" ,libhandy)
       ("libnotify" ,libnotify)
       ("libportal" ,libportal)
       ("libsecret" ,libsecret)
       ("libxslt" ,libxslt)
       ("nettle" ,nettle) ; for hogweed
       ("sqlite" ,sqlite)
       ("webkitgtk" ,webkitgtk-with-libsoup2)))
    (home-page "https://wiki.gnome.org/Apps/Web")
    (synopsis "GNOME web browser")
    (description
     "Epiphany is a GNOME web browser targeted at non-technical users.  Its
principles are simplicity and standards compliance.")
    (license license:gpl2+)))

(define-public d-feet
  (package
    (name "d-feet")
    (version "0.3.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1m8lwiwl5jhi0x7y6x5zmd3hjplgvdjrb8a8jg74rvkygslj1p7f"))))
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
                `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")))
                `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))
              #t))))))
    (native-inputs
     `(("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("python-pep8" ,python-pep8)
       ("xmllint" ,libxml2)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("gtk+" ,gtk+)
       ("python" ,python-wrapper)
       ("hicolor-icon-theme" ,hicolor-icon-theme)
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
    (version "41.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "19d46rkajvr0f04560vlrzwvac88x5j8ilvzwkawbn5vjg069kf8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--enable-doc")))
    (native-inputs
     `(("ducktype" ,mallard-ducktype)
       ("gettext" ,gettext-minimal)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("xmllint" ,libxml2)
       ("xsltproc" ,libxslt)))
    (synopsis "XSL stylesheets for Yelp")
    (description "Yelp-XSL is a collection of programs and data files to help
you build, maintain, and distribute documentation.  It provides XSLT stylesheets
that can be built upon for help viewers and publishing systems.  These
stylesheets output JavaScript and CSS content, and reference images
provided by yelp-xsl. It also redistributes copies of the jQuery and
jQuery.Syntax JavaScript libraries.")
    (home-page "https://wiki.gnome.org/Apps/Yelp")
    (license
     (list
      ;; XSLT
      license:gpl2+
      ;; Images
      license:lgpl2.1+
      ;; JavaScript
      license:expat))))

(define-public yelp
  (package
    (name "yelp")
    (version "41.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "11shrqass8ak8m10nayqssa6sbrrxa13ffpamvqi1c0yzkxdk9r5"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list `(,glib "bin") ; for glib-genmarshal, etc.
           intltool itstool pkg-config))
    (propagated-inputs
     (list dconf))
    (inputs
     (list gsettings-desktop-schemas libxslt sqlite webkitgtk yelp-xsl))
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
    (version "3.32.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1yg8f5g5wadhmy4yfd9yjhvd8vll4gq4l86ibp0b42qbxnsmcf0q"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list yelp-xsl))
    (propagated-inputs
     ;; Needed by `yelp-build', `yelp-check' or 'yelp.m4'.
     `(("itstool" ,itstool)
       ("xmllint" ,libxml2)
       ("xsltproc" ,libxslt)))
    (synopsis "Yelp documentation tools")
    (description
     "Yelp-tools is a collection of scripts and build utilities to help create,
manage, and publish documentation for Yelp and the web.  Most of the heavy
lifting is done by packages like yelp-xsl and itstool.  This package just
wraps things up in a developer-friendly way.")
    (home-page "https://wiki.gnome.org/Apps/Yelp/Tools")
    (license license:gpl2+)))

(define-public libgee
  (package
    (name "libgee")
    (version "0.20.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libgee/"
                                  (version-major+minor version) "/"
                                  "libgee-" version ".tar.xz"))
              (sha256
               (base32
                "1pm525wm11dhwz24m8bpcln9547lmrigl6cxf3qsbg4cr3pyvdfh"))))
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
     (list `(,glib "bin") pkg-config))
    (inputs
     (list glib gobject-introspection))
    (home-page "https://wiki.gnome.org/Projects/Libgee")
    (synopsis "GObject collection library")
    (description
     "Libgee is a utility library providing GObject-based interfaces and
classes for commonly used data structures.")
    (license license:lgpl2.1+)))

(define-public gexiv2
  (package
    (name "gexiv2")
    (version "0.12.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0k4ljzzz5dwqndw8awvlw3ala8rh3b2rk9i4jzvywc53mi9ba8i3"))))
    (build-system meson-build-system)
    (native-inputs
     (list gcr
           `(,glib "bin") pkg-config vala))
    (propagated-inputs
     ;; Listed in "Requires" section of gexiv2.pc
     (list exiv2))
    (inputs
     (list glib gobject-introspection))
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
    (version "0.30.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/shotwell/"
                                  (version-major+minor version) "/"
                                  "shotwell-" version ".tar.xz"))
              (sha256
               (base32
                "1h5crjq59lqi8f8mdkadzi8pc3i9i2ik4lsx2nrhzq486fzdfhw6"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "build-aux/meson/postinstall.py"
               (("gtk-update-icon-cache") (which "true"))
               (("update-desktop-database") (which "true")))
             #t)))))
    (propagated-inputs
     (list dconf))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("vala" ,vala)))
    (inputs
     `(("gcr" ,gcr)
       ("gexiv2" ,gexiv2)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gstreamer" ,gstreamer)
       ("json-glib" ,json-glib)
       ("libgdata" ,libgdata)
       ("libgee" ,libgee)
       ("libgphoto2" ,libgphoto2)
       ("libgudev" ,libgudev)
       ("libraw" ,libraw)
       ("libxml2" ,libxml2)
       ("sqlite" ,sqlite)
       ("webkitgtk" ,webkitgtk-with-libsoup2)))
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
    (version "3.40.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/file-roller/"
                                  (version-major+minor version) "/"
                                  "file-roller-" version ".tar.xz"))
              (sha256
               (base32
                "039w1dcpa5ypmv6sm634alk9vbcdkyvy595vkh5gn032jsiqca2a"))))
    (build-system meson-build-system)
    (native-inputs
     (list desktop-file-utils ; for update-desktop-database
           intltool
           itstool
           pkg-config
           python
           `(,gtk+ "bin") ; gtk-update-icon-cache
           `(,glib "bin")))
    ;; TODO: Add libnautilus.
    (inputs
     (list gtk+
           gdk-pixbuf
           json-glib
           libarchive
           libnotify
           nettle
           libxml2))
    (synopsis "Graphical archive manager for GNOME")
    (description "File Roller is an archive manager for the GNOME desktop
environment that allows users to view, unpack, and create compressed archives
such as gzip tarballs.")
    (home-page "http://fileroller.sourceforge.net/")
    (license license:gpl2+)))

(define-public gnome-session
  (package
    (name "gnome-session")
    (version "40.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (patches (search-patches "gnome-session-support-elogind.patch"))
              (sha256
               (base32
                "02z0xr6sv9ibl7awbw9j4y05hf4jk1zgvsbbmh7n27hhjvsvc8pl"))))
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-gnome-session
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make sure 'gnome-session' finds the 'gsettings' program.
             (let ((glib (assoc-ref inputs "glib:bin"))
                   (out  (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/gnome-session")
                 `("PATH" ":" prefix (,(string-append glib "/bin"))))))))

       #:configure-flags
       '("-Ddocbook=false" ; FIXME: disabled because of docbook validation error
         "-Dman=false" ; FIXME: disabled because of docbook validation error
         "-Delogind=true"
         "-Dsystemd=false"
         "-Dsystemd_session=disable"
         "-Dsystemd_journal=false")))
    (build-system meson-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("xsltproc" ,libxslt)
       ("libxml2" ,libxml2) ;for 'XML_CATALOG_FILES'
       ("docbook-xsl" ,docbook-xsl)
       ("docbook-xml" ,docbook-xml)
       ("xmlto" ,xmlto)))
    (inputs
     (list elogind
           gnome-desktop
           gsettings-desktop-schemas
           gtk+
           json-glib
           libsm
           libxcomposite
           libxtst
           mesa
           upower
           xtrans))
    (synopsis "Session manager for GNOME")
    (description
     "This package contains the GNOME session manager, as well as a
configuration program to choose applications starting on login.")
    (home-page "https://wiki.gnome.org/Projects/SessionManagement")
    (license license:gpl2+)))

(define-public gjs
  (package
    (name "gjs")
    (version "1.70.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0pqwhq0znprs0h5ixz396912acwzk9zvjfhi9qvh52ii38s2j1jb"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "installed-tests/scripts/testCommandLine.sh"
                    (("Valentín") "")
                    (("☭") ""))))))
    (build-system meson-build-system)
    (arguments
     ;; Use meson-0.59, otherwise we'd get "ERROR: "install_dir" must be
     ;; specified when installing a target".
     `(#:meson ,meson-0.59
       #:configure-flags '("-Dinstalled_tests=false")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; The test suite requires a running X server.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")

             ;; For the missing /etc/machine-id.
             (setenv "DBUS_FATAL_WARNINGS" "0"))))))
    (native-inputs
     `(("glib:bin" ,glib "bin")       ; for glib-compile-resources
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)
       ;; For testing
       ("dbus-launch" ,dbus)
       ("dconf" ,dconf) ; required to properly store settings
       ("uuidgen" ,util-linux)
       ("xvfb" ,xorg-server-for-tests)))
    (propagated-inputs
     ;; These are all in the Requires.private field of gjs-1.0.pc.
     (list cairo gobject-introspection mozjs-78))
    (inputs
     (list gtk+ readline))
    (synopsis "Javascript bindings for GNOME")
    (home-page "https://live.gnome.org/Gjs")
    (description
     "Gjs is a javascript binding for GNOME.  It's mainly based on spidermonkey
javascript engine and the GObject introspection framework.")
    (license license:gpl2+)))

(define-public gedit
  (package
    (name "gedit")
    (version "40.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "149ngl9qw6h59546lir1pa7hvw23ppsnqlj9mfqphmmn5jl99qsm"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       ;; Otherwise, the RUNPATH will lack the final path component.
       (list (string-append "-Dc_link_args=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib/gedit"))

       ;; XXX: Generated .h files are sometimes used before being built.
       #:parallel-build? #f

       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson/post_install.py"
               (("gtk-update-icon-cache") (which "true")))
             #t))
         (add-after 'unpack 'patch-libgd-fetch
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libgd (assoc-ref inputs "libgd")))
               ;; Calling git is unnecessary because libgd is fetched as a
               ;; native input to this package.
               (substitute* "meson.build"
                 ((".*git.*") ""))
               (copy-recursively libgd "subprojects/libgd")
               #t)))
         (add-after 'install 'wrap-gedit
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gtksourceview     (assoc-ref inputs "gtksourceview"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                   (python-path       (getenv "GUIX_PYTHONPATH")))
               (wrap-program (string-append out "/bin/gedit")
                 ;; For plugins.
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                 `("GUIX_PYTHONPATH" ":" prefix (,python-path))
                 ;; For language-specs.
                 `("XDG_DATA_DIRS" ":" prefix (,(string-append gtksourceview
                                                               "/share")))))
             #t)))))
    (propagated-inputs
     (list dconf))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils) ; for update-desktop-database
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("glib:bin" ,glib "bin") ; for glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("libgd"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://gitlab.gnome.org/GNOME/libgd")
                 (commit "c7c7ff4e05d3fe82854219091cf116cce6b19de0")))
           (file-name (git-file-name "libgd" version))
           (sha256
            (base32 "16yld0ap7qj1n96h4f2sqkjmibg7xx5xwkqxdfzam2nmyfdlrrrs"))))
       ("pkg-config" ,pkg-config)))
    (inputs
     (list amtk
           glib
           gspell
           gtk+
           gtksourceview
           libpeas
           libxml2
           iso-codes
           python-pygobject
           python
           tepl
           gsettings-desktop-schemas
           libx11
           vala
           adwaita-icon-theme
           libsoup
           gnome-desktop))
    (home-page "https://wiki.gnome.org/Apps/Gedit")
    (synopsis "GNOME text editor")
    (description "While aiming at simplicity and ease of use, gedit is a
powerful general purpose text editor.")
    (license license:gpl2+)))

(define-public zenity
  (package
    (name "zenity")
    (version "3.32.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/zenity/"
                                  (version-major+minor version) "/"
                                  "zenity-" version ".tar.xz"))
              (sha256
               (base32
                "15fdh8xfdhnwcynyh4byx3mrjxbyprqnwxzi7qn3g5wwaqryg1p7"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gettext-minimal itstool pkg-config))
    (inputs
     (list libnotify webkitgtk))
    (synopsis "Display graphical dialog boxes from shell scripts")
    (home-page "https://www.gnome.org")
    (description
     "Zenity is a rewrite of gdialog, the GNOME port of dialog which allows you
to display dialog boxes from the commandline and shell scripts.")
    (license license:lgpl2.0+)))

(define-public mutter
  (package
    (name "mutter")
    (version "41.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "17pqrm48kddqrc3fl96n5knhaxyn0crg0zv7zpmqhk848jks307s"))))
    ;; NOTE: Since version 3.21.x, mutter now bundles and exports forked
    ;; versions of cogl and clutter.  As a result, many of the inputs,
    ;; propagated-inputs, and configure flags used in cogl and clutter are
    ;; needed here as well.
    (build-system meson-build-system)
    (arguments
     `(#:imported-modules (,@%meson-build-system-modules
                           (guix build syscalls))
       #:modules ((guix build meson-build-system)
                  (guix build syscalls)
                  (guix build utils)
                  (ice-9 match))
       #:glib-or-gtk? #t
       #:configure-flags
       (list
        ;; Otherwise, the RUNPATH will lack the final path component.
        (string-append "-Dc_link_args=-Wl,-rpath="
                       (assoc-ref %outputs "out") "/lib:"
                       (assoc-ref %outputs "out") "/lib/mutter-9")
        ;; Disable systemd support.
        "-Dsystemd=false"
        ;; The following flags are needed for the bundled clutter
        (string-append "-Dxwayland_path="
                       (assoc-ref %build-inputs "xorg-server-xwayland")
                       "/bin/Xwayland")
        ;; the remaining flags are needed for the bundled cogl
        (string-append "-Dopengl_libname="
                       (assoc-ref %build-inputs "mesa")
                       "/lib/libGL.so")
        (string-append "-Dgles2_libname="
                       (assoc-ref %build-inputs "mesa")
                       "/lib/libGLESv2.so")
        "-Degl_device=true"              ;false by default
        "-Dwayland_eglstream=true")      ;false by default
       #:test-options
       (list "--verbose")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-dlopen-calls
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/wayland/meta-wayland-egl-stream.c"
               (("libnvidia-egl-wayland.so.1")
                (string-append (assoc-ref inputs "egl-wayland")
                               "/lib/libnvidia-egl-wayland.so.1")))))
         (add-before 'configure 'set-udev-dir
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (setenv "PKG_CONFIG_UDEV_UDEVDIR"
                     (string-append (assoc-ref outputs "out")
                                    "/lib/udev"))))
         (add-after 'unpack 'disable-problematic-tests
           (lambda _
             ;; The native-headless test hangs due to attempting to use audio,
             ;; unavailable in the container.
             ;; Note: the following sed expression deletes the whole test(...)
             ;; expression paragraph.  For an explanation, see: info '(sed)
             ;; Multiline techniques'.
             (invoke "sed" "/./{H;$!d} ; x ; s/^.*native-headless.*$//"
                     "-i" "src/tests/meson.build")))
         (replace 'check
           (lambda* (#:key tests? test-options parallel-tests?
                     #:allow-other-keys)
             (when tests?
               ;; Setup (see the 'test-mutter' CI target at
               ;; https://gitlab.gnome.org/GNOME/mutter/-/raw/main/.gitlab-ci.yml).
               (setenv "XDG_RUNTIME_DIR" "runtime-dir")
               (setenv "GSETTINGS_SCHEMA_DIR" "data")
               (setenv "MUTTER_DEBUG_DUMMY_MODE_SPECS" "800x600@10.0")
               (setenv "PIPEWIRE_DEBUG" "2")
               (setenv "PIPEWIRE_LOG" "meson-logs/pipewire.log")
               (setenv "XVFB_SERVER_ARGS" "+iglx -noreset")
               (setenv "G_SLICE" "always-malloc")
               (setenv "MALLOC_CHECK" "3")
               (setenv "NO_AT_BRIDGE" "1")
               ;; This is needed, otherwise the "mutter:core+mutter/unit /
               ;; anonymous-file" test would fail (see:
               ;; https://gitlab.gnome.org/GNOME/mutter/-/issues/2017).
               (setenv "CI_JOB_ID" "1")

               (invoke "glib-compile-schemas" (getenv "GSETTINGS_SCHEMA_DIR"))
               (mkdir-p (getenv "XDG_RUNTIME_DIR"))
               (chmod (getenv "XDG_RUNTIME_DIR") #o755)
               (invoke "pipewire" "--version") ;check for pipewire
               (system "pipewire &")    ;always returns 0 due to forking

               (setenv "MESON_TESTTHREADS"
                       (if parallel-tests?
                           (number->string (parallel-job-count))
                           "1"))
               (match (primitive-fork)
                 (0                     ;child process
                  (set-child-subreaper!)
                  ;; Use tini so that signals are properly handled and
                  ;; doubly-forked processes get reaped; otherwise,
                  ;; python-dbusmock would waste time polling for the dbus
                  ;; processes it spawns to be reaped, in vain.
                  (apply execlp "tini" "--"
                         "dbus-run-session" "--"
                         "xvfb-run" "-a" "-s" (getenv "XVFB_SERVER_ARGS")
                         "meson" "test" "-t" "0" "--print-errorlogs"
                         test-options))
                 (pid
                  (match (waitpid pid)
                    ((_ . status)
                     (unless (zero? status)
                       (error "`meson test' exited with status"
                              status))))))))))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils) ; for update-desktop-database
       ("glib:bin" ,glib "bin")         ; for glib-compile-schemas, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("xvfb-run" ,xvfb-run)
       ;; For git build
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ;; For tests.
       ;; Warnings are configured to be fatal during the tests; add an icon
       ;; theme to please libxcursor.
       ("adwaita-icon-theme" ,adwaita-icon-theme)
       ("libxcursor" ,libxcursor)       ;for XCURSOR_PATH
       ("pipewire" ,pipewire-0.3)
       ("python" ,python)
       ("python-dbus" ,python-dbus)
       ("python-dbusmock" ,python-dbusmock)
       ("tini" ,tini)))                 ;acting as init (zombie reaper)
    (propagated-inputs
     `( ;; libmutter.pc refers to these:
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ;; mutter-clutter-1.0.pc and mutter-cogl-1.0.pc refer to these:
       ("atk" ,atk)
       ("cairo" ,cairo)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("json-glib" ,json-glib)
       ("libinput" ,libinput)
       ("libx11" ,libx11)
       ("libxcomposite" ,libxcomposite)
       ("libxcvt" ,libxcvt)
       ("libxdamage" ,libxdamage)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("libxkbcommon" ,libxkbcommon)
       ("libxml2" ,libxml2)
       ("libxrandr" ,libxrandr)
       ("mesa" ,mesa)
       ("pango" ,pango)
       ("udev" ,eudev)
       ("xinput" ,xinput)))
    (inputs
     `(("egl-wayland" ,egl-wayland)     ;for wayland-eglstream-protocols
       ("elogind" ,elogind)
       ("gnome-desktop" ,gnome-desktop)
       ("gnome-settings-daemon" ,gnome-settings-daemon)
       ("graphene" ,graphene)
       ("libcanberra-gtk" ,libcanberra)
       ("libgudev" ,libgudev)
       ("libice" ,libice)
       ("libsm" ,libsm)
       ("libwacom" ,libwacom)
       ("libxkbfile" ,libxkbfile)
       ("libxrandr" ,libxrandr)
       ("libxtst" ,libxtst)
       ("pipewire" ,pipewire-0.3)
       ("startup-notification" ,startup-notification)
       ("sysprof" ,sysprof)
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
    (version "3.43.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1s5pmy3hx6hhnyi40r7b773py9kn2qbkxnpxv6149z9fl5ikdjrv"))))
    (outputs '("out" "lib"))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--libdir=" (assoc-ref %outputs "out") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-libgoa-output
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((lib (assoc-ref outputs "lib")))
               (substitute* '("src/goa/Makefile.in" "src/goa/goa-1.0.pc.in")
                 (("@prefix@") lib)
                 (("@exec_prefix@") lib)
                 (("@libdir@") (string-append lib "/lib"))
                 (("@includedir@") (string-append lib "/include"))
                 (("@datadir@") (string-append lib "/share")))
               ;; Make sure gobject-introspection knows about the output
               ;; too (see <https://bugs.gnu.org/36535>).
               (setenv "outputs" "out lib")))))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     (list glib ; required by goa-1.0.pc
           gtk+))         ; required by goa-backend-1.0.pc
    (inputs
     `(("docbook-xsl" ,docbook-xsl)
       ("json-glib" ,json-glib)
       ("libsecret" ,libsecret)
       ("rest" ,rest)
       ;; WebKitGtk propagates libsoup 3, which causes the build to fail; so
       ;; use a special variant.
       ("webkitgtk" ,webkitgtk-with-libsoup2)))
    (synopsis "Single sign-on framework for GNOME")
    (home-page "https://wiki.gnome.org/Projects/GnomeOnlineAccounts")
    (description
     "GNOME Online Accounts provides interfaces so that applications and
libraries in GNOME can access the user's online accounts.  It has providers
for Google, ownCloud, Facebook, Flickr, Windows Live, Pocket, Foursquare,
Microsoft Exchange, Last.fm, IMAP/SMTP, Jabber, SIP and Kerberos.")
    (license license:lgpl2.0+)))

(define-public evolution-data-server
  (package
    (name "evolution-data-server")
    (version "3.42.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "0a7my8spwcaf2i2fz8ndddi1drv6l9gxq0qblmnkxzyhfwm7zrp6"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (let* ((lib (string-append (assoc-ref %outputs "out") "/lib"))
              (runpaths (map (lambda (s)
                               (string-append lib "/evolution-data-server/" s))
                             '("addressbook-backends" "calendar-backends"
                               "camel-providers" "credential-modules"
                               "registry-modules"))))
         (list "-DENABLE_UOA=OFF"             ;disable Ubuntu Online Accounts support
               "-DENABLE_GOOGLE=OFF"          ;disable Google Contacts support
               "-DENABLE_GOOGLE_AUTH=OFF"     ;disable Google authentication
               "-DENABLE_VALA_BINDINGS=ON"
               (string-append "-DCMAKE_INSTALL_RPATH=" lib ";"
                              (string-append lib "/evolution-data-server;")
                              (string-join runpaths ";"))
               "-DENABLE_INTROSPECTION=ON"))  ;required for Vala bindings
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             ;; tests/book-migration/test-migration.c:160:test_fetch_contacts:
             ;; assertion failed (g_slist_length (contacts) == 20): (0 == 20)
             (delete-file-recursively "tests/book-migration")
             (substitute* "tests/CMakeLists.txt"
               (("add_subdirectory\\(book-migration\\)") ""))))
         (add-after 'unpack 'patch-paths
          (lambda _
            (substitute* '("tests/test-server-utils/e-test-server-utils.c"
                           "tests/libedata-book/data-test-utils.c"
                           "tests/libedata-book/test-book-cache-utils.c"
                           "tests/libedata-cal/test-cal-cache-utils.c")
              (("/bin/rm") (which "rm")))))
         (add-before 'configure 'dont-override-rpath
           (lambda _
             (substitute* "CMakeLists.txt"
               ;; CMakeLists.txt hard-codes runpath to just the libdir.
               ;; Remove it so the configure flag is respected.
               (("SET\\(CMAKE_INSTALL_RPATH .*") "")))))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("gperf" ,gperf)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("python" ,python-wrapper)))
    (propagated-inputs
     ;; These are all in the Requires field of .pc files.
     `(("gtk+" ,gtk+)
       ("libical" ,libical)
       ("libsecret" ,libsecret)
       ("libsoup" ,libsoup-minimal-2)
       ("nss" ,nss)
       ("sqlite" ,sqlite)))
    (inputs
     `(("bdb" ,bdb)
       ("gcr" ,gcr)
       ("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("json-glib" ,json-glib)
       ("libcanberra" ,libcanberra)
       ("libgweather" ,libgweather)
       ("mit-krb5" ,mit-krb5)
       ("openldap" ,openldap)
       ("webkitgtk" ,webkitgtk-with-libsoup2)))
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
                   (python-path (getenv "GUIX_PYTHONPATH"))
                   (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
              (for-each
               (lambda (prog)
                 (wrap-program prog
                   `("GUIX_PYTHONPATH"      ":" prefix (,python-path))
                   `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
               (list (string-append out "/bin/caribou-preferences")
                     (string-append out "/libexec/antler-keyboard"))))
            #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("vala" ,vala)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     ;; caribou-1.0.pc refers to all these.
     (list libgee libxklavier libxtst gtk+))
    (inputs
     `(("clutter" ,clutter)
       ("dconf" ,dconf)
       ("gtk+-2" ,gtk+-2)
       ("python-pygobject" ,python-pygobject)))
    (synopsis "Text entry and UI navigation application")
    (home-page "https://wiki.gnome.org/Projects/Caribou")
    (description
     "Caribou is an input assistive technology intended for switch and pointer
users.")
    (license license:lgpl2.1)))

(define-public network-manager
  (package
    (name "network-manager")
    (version "1.32.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/NetworkManager/"
                                  (version-major+minor version) "/"
                                  "NetworkManager-" version ".tar.xz"))
              (patches (search-patches "network-manager-plugin-path.patch"
                                       "network-manager-meson.patch"))
              (sha256
               (base32
                "0jzmz0zw64dgvdn2g7pppr7bkywpbxcbdb1viv6p7zh2lnh3dax8"))))
    (build-system meson-build-system)
    (outputs '("out"
               "doc"))                  ; 8 MiB of gtk-doc HTML
    (arguments
     `(#:configure-flags
       (let ((out      (assoc-ref %outputs "out"))
             (dhclient (search-input-file %build-inputs "/sbin/dhclient")))
         (list
          ;; Otherwise, the RUNPATH will lack the final 'NetworkManager' path
          ;; component.
          (string-append "-Dc_link_args=-Wl,-rpath="
                         out "/lib:"
                         out "/lib/NetworkManager/" ,version)
          "-Dsystemd_journal=false"
          "-Dsession_tracking=elogind"
          "-Dsuspend_resume=elogind"
          "-Dsystemdsystemunitdir=no"
          "-Dsession_tracking_consolekit=false"
          "-Ddhcpcd=no"
          "-Ddhcpcanon=no"
          "-Dcrypto=gnutls"
          "-Diwd=true"
          "-Dlibaudit=yes"
          "-Dqt=false"
          "-Ddocs=true"
          "--sysconfdir=/etc"
          "--localstatedir=/var"
          (string-append "-Dudev_dir="
                         out "/lib/udev")
          (string-append "-Ddbus_conf_dir="
                         out "/etc/dbus-1/system.d")

          (string-append "-Ddhclient=" dhclient)))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-dlopen-call-to-libjansson.so
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/libnm-glib-aux/nm-json-aux.c"
               (("(handle = dlopen\\()soname" _ head)
                (string-append
                 head "\"" (search-input-file inputs
                                              "lib/libjansson.so") "\"")))))
         (add-before 'configure 'pre-configure
           (lambda _
             ;; These tests try to test aspects of network-manager's
             ;; functionality within restricted containers, but they don't
             ;; cope with being already in the Guix build jail as that jail
             ;; lacks some features that they would like to proxy over (like
             ;; a /sys mount).
             (substitute* "src/core/tests/meson.build"
               ((".*test-l3cfg.*") ""))
             (substitute* "src/core/devices/tests/meson.build"
               ((".*test-acd.*") "")
               ((".*test-lldp.*") ""))
             (substitute* "src/core/platform/tests/meson.build"
               ((".*test-address-linux.*") "")
               ((".*test-cleanup-linux.*") "")
               ((".*test-link-linux.*") "")
               ((".*test-lldp.*") "")
               ((".*test-route-linux.*") "")
               ((".*test-tc-linux.*") ""))))
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xmldoc (string-append (assoc-ref inputs "docbook-xml")
                                          "/xml/dtd/docbook")))
               (substitute* (find-files "." ".*\\.(xsl|xml)")
                 (("http://.*/docbookx\\.dtd")
                  (string-append xmldoc "/docbookx.dtd"))))))
         (add-before 'check 'pre-check
           (lambda _
             ;; For the missing /etc/machine-id.
             (setenv "DBUS_FATAL_WARNINGS" "0")))
         (add-before 'install 'no-polkit-magic
           ;; Meson ‘magically’ invokes pkexec, which fails (not setuid).
           (lambda _
             (setenv "PKEXEC_UID" "something")))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (for-each (lambda (directory)
                           (copy-recursively (string-append out directory)
                                             (string-append doc directory))
                           (delete-file-recursively
                            (string-append out directory)))
                         '("/share/doc" "/share/gtk-doc"))))))))
    (propagated-inputs
     (list glib))
    (native-inputs
     `(("glib:bin" ,glib "bin")         ; for gdbus-codegen
       ("gtk-doc" ,gtk-doc/stable)
       ("gobject-introspection" ,gobject-introspection)
       ("docbook-xml" ,docbook-xml)
       ("docbook-xsl" ,docbook-xsl)
       ("intltool" ,intltool)
       ("libxslt" ,libxslt)
       ("libxml2" ,libxml2)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
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
       ("iwd" ,iwd)                     ; wpa_supplicant alternative
       ("jansson" ,jansson)
       ("libaudit" ,audit)
       ("libgcrypt" ,libgcrypt)
       ("libgudev" ,libgudev)
       ("libndp" ,libndp)
       ("libnl" ,libnl)
       ("libselinux" ,libselinux)
       ("libsoup" ,libsoup)
       ("mobile-broadband-provider-info" ,mobile-broadband-provider-info)
       ("modem-manager" ,modem-manager)
       ("newt" ,newt)                   ;for the 'nmtui' console interface
       ("openresolv" ,openresolv)       ; alternative resolv.conf manager
       ("polkit" ,polkit)
       ("ppp" ,ppp)
       ("readline" ,readline)
       ("util-linux" ,util-linux)
       ("elogind" ,elogind)))
    (synopsis "Network connection manager")
    (home-page "https://wiki.gnome.org/Projects/NetworkManager")
    (description
     "NetworkManager is a system network service that manages your network
devices and connections, attempting to keep active network connectivity when
available.  It manages ethernet, WiFi, mobile broadband (WWAN), and PPPoE
devices, and provides VPN integration with a variety of different VPN
services.")
    ;; “This NetworkManager project consists of the daemon, client tools, and
    ;; libnm. libnm is licensed LGPL-2.1+, while the rest is licensed under
    ;; GPL-2.0+.”
    (license (list license:gpl2+
                   license:lgpl2.1+))
    (properties '((upstream-name . "NetworkManager")))))

(define-public network-manager-openvpn
  (package
    (name "network-manager-openvpn")
    (version "1.8.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/NetworkManager-openvpn/"
                    (version-major+minor version)
                    "/NetworkManager-openvpn-" version ".tar.xz"))
              (sha256
               (base32
                "062kh4zj7jfbwy4zzcwpq2m457bzbpm3l18s0ysnw3mgia3siz8f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-absolute-paths" "--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-path
           (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
             (let* ((ovpn (search-input-file inputs "/sbin/openvpn"))
                    (modprobe (search-input-file inputs "/bin/modprobe"))
                    (pretty-ovpn (string-append "\"" ovpn "\"")))
               (for-each
                (lambda (file)
                  (substitute* file
                    (("\"/usr/local/sbin/openvpn\"") pretty-ovpn)
                    (("\"/usr/sbin/openvpn\"") pretty-ovpn)
                    (("\"/sbin/openvpn\"") pretty-ovpn)
                    (("/sbin/modprobe") modprobe)))
                '("src/nm-openvpn-service.c" "properties/nm-openvpn-editor.c")))
             #t)))))
    (native-inputs
     (list pkg-config intltool))
    (inputs
     (list gtk+
           kmod
           openvpn
           network-manager
           libnma
           libsecret))
    (home-page "https://wiki.gnome.org/Projects/NetworkManager/VPN")
    (synopsis "OpenVPN plug-in for NetworkManager")
    (description
     "This extension of NetworkManager allows it to take care of connections
to virtual private networks (VPNs) via OpenVPN.")
    (license license:gpl2+)
    (properties `((upstream-name . "NetworkManager-openvpn")))))

(define-public network-manager-vpnc
  (package
    (name "network-manager-vpnc")
    (version "1.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/NetworkManager-vpnc/"
                    (version-major+minor version)
                    "/NetworkManager-vpnc-" version ".tar.xz"))
              (sha256
               (base32
                "1js5lwcsqws4klgypfxl4ikmakv7v7xgddij1fj6b0y0qicx0kyy"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-absolute-paths"
                           "--localstatedir=/var"
                           ;; libnm-glib has been removed from network-manager
                           ;; 1de8383ad9fdfc8f552117e5d109bdfa7005634b
                           "--with-libnm-glib=no")
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-path
           (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
             (let* ((vpnc (search-input-file inputs "/sbin/vpnc"))
                    (modprobe (search-input-file inputs "/bin/modprobe"))
                    (pretty-ovpn (string-append "\"" vpnc "\"")))
               (substitute* "src/nm-vpnc-service.c"
                    (("\"/usr/local/sbin/vpnc\"") pretty-ovpn)
                    (("\"/usr/sbin/vpnc\"") pretty-ovpn)
                    (("\"/sbin/vpnc\"") pretty-ovpn)
                    (("/sbin/modprobe") modprobe)))
             #t)))))
    (native-inputs
     (list pkg-config intltool))
    (inputs
     (list gtk+
           kmod
           vpnc
           network-manager
           libnma
           libsecret))
    (home-page "https://wiki.gnome.org/Projects/NetworkManager/VPN")
    (synopsis "VPNC plug-in for NetworkManager")
    (description
     "Support for configuring virtual private networks based on VPNC.
Compatible with Cisco VPN concentrators configured to use IPsec.")
    (license license:gpl2+)
    (properties `((upstream-name . "NetworkManager-vpnc")))))

(define-public network-manager-openconnect
  (package
    (name "network-manager-openconnect")
    (version "1.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/NetworkManager-openconnect/"
                    (version-major+minor version)
                    "/NetworkManager-openconnect-" version ".tar.xz"))
              (sha256
               (base32
                "0nlp290nkawc4wqm978n4vhzg3xdqi8kpjjx19l855vab41rh44m"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-absolute-paths" "--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-path
           (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
             (let* ((openconnect (search-input-file inputs
                                                    "/sbin/openconnect"))
                    (modprobe (search-input-file inputs "/bin/modprobe"))
                    (pretty-ovpn (string-append "\"" openconnect "\"")))
               (substitute* "src/nm-openconnect-service.c"
                 (("\"/usr(/local)?/s?bin/openconnect\"") pretty-ovpn)
                 (("/sbin/modprobe") modprobe)))
             #t)))))
    (native-inputs
     (list intltool libnma pkg-config))
    (inputs
     (list gcr
           gtk+
           kmod
           libsecret
           libxml2
           lz4
           network-manager
           openconnect))
    (home-page "https://wiki.gnome.org/Projects/NetworkManager/VPN")
    (synopsis "OpenConnect plug-in for NetworkManager")
    (description
     "This extension of NetworkManager allows it to take care of connections
to @acronym{VPNs, virtual private networks} via OpenConnect, an open client for
Cisco's AnyConnect SSL VPN.")
    (license license:gpl2+)
    (properties `((upstream-name . "NetworkManager-openconnect")

                  ;; The 'etc/dbus-1/system.d/nm-openconnect-service.conf'
                  ;; file refers to account "nm-openconnect".  Specify it here
                  ;; so that 'network-manager-service-type' creates it.
                  (user-accounts . ("nm-openconnect"))))))

(define-public mobile-broadband-provider-info
  (package
    (name "mobile-broadband-provider-info")
    (version "20190116")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/"
                    "mobile-broadband-provider-info/" version "/"
                    "mobile-broadband-provider-info-" version ".tar.xz"))
              (sha256
               (base32
                "16y5lc7pfdvai9c8xwb825zc3v46039gghbip13fqslf5gw11fic"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))                    ; no tests
    (home-page "https://wiki.gnome.org/Projects/NetworkManager")
    (synopsis "Database of broadband connection configuration")
    (description "Database of broadband connection configuration.")
    (license license:public-domain)))

(define-public network-manager-applet
  (package
    (name "network-manager-applet")
    (version "1.22.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/network-manager-applet/"
                                  (version-major+minor version) "/"
                                  "network-manager-applet-" version ".tar.xz"))
              (sha256
               (base32
                "1gj6lqqi613j2m49v9i82lqg1rv7kwwc8z4nxjcwpaa0ins803f7"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       '("-Dappindicator=yes")))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib:bin" ,glib "bin") ; for glib-compile-resources, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc/stable)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     ;; libnm-gtk.pc refers to all these.
     (list dbus-glib gtk+ network-manager
           ;; nm-applet need by org.gnome.nm-applet.gschema.xml
           libnma))
    (inputs
     (list gcr
           libappindicator
           libgudev
           libnotify
           libsecret
           libselinux
           jansson ; for team support
           modem-manager))
    (synopsis "Applet for managing network connections")
    (home-page "https://wiki.gnome.org/Projects/NetworkManager")
    (description
     "This package contains a systray applet for NetworkManager.  It displays
the available networks and allows users to easily switch between them.")
    (license license:gpl2+)))

(define-public libxml++
  (package
    (name "libxml++")
    (version "3.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libxmlplusplus/libxmlplusplus")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wjz591rjlgbah7dcq8i0yn0zw9d62b7g6r0pppx81ic0cx8n8ga"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-documentation
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xmldoc (string-append (assoc-ref inputs "docbook-xml")
                                          "/xml/dtd/docbook"))
                   (xsldoc (string-append (assoc-ref inputs "docbook-xsl")
                                          "/xml/xsl/docbook-xsl-"
                                          ,(package-version docbook-xsl))))
               (substitute* '("examples/dom_xpath/example.xml"
                              "docs/manual/libxml++_without_code.xml")
                 (("http://.*/docbookx\\.dtd")
                  (string-append xmldoc "/docbookx.dtd")))
               (setenv "SGML_CATALOG_FILES"
                       (string-append xmldoc "/catalog.xml"))
               (substitute* "docs/manual/docbook-customisation.xsl"
                 (("http://docbook.sourceforge.net/release/xsl/current/html/chunk.xsl")
                  (string-append xsldoc "/html/chunk.xsl")))))))))
    (propagated-inputs
     ;; libxml++-3.0.pc refers to all these.
     (list glibmm-2.64 libxml2))
    (native-inputs
     (list autoconf
           automake
           doxygen
           docbook-xml
           docbook-xsl
           graphviz ; for dot
           libtool
           libxslt
           mm-common
           perl
           pkg-config))
    (home-page "https://github.com/libxmlplusplus/libxmlplusplus/")
    (synopsis "C++ bindings to the libxml2 XML parser library")
    (description
     "This package provides a C++ interface to the libxml2 XML parser
library.")
    (license license:lgpl2.1+)))

;; This is the last release providing the 2.6 API, hence the name.
;; This is needed by tascam-gtk
(define-public libxml++-2
  (package
    (inherit libxml++)
    (name "libxml++")
    (version "2.40.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libxmlplusplus/libxmlplusplus")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gbfi4l88w828gmyc9br11l003ylyi4vigp5d1kfgsn0k4cig3y9"))))))

(define-public gdm
  (package
    (name "gdm")
    (version "40.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1zbks7bwbys1pn7sk0aq0dbn09adx51amb441z4z4j28cvma3f5b"))
              (patches
               (search-patches
                "gdm-default-session.patch"
                "gdm-elogind-support.patch"
                "gdm-remove-hardcoded-xwayland-path.patch"
                "gdm-wayland-session-wrapper-from-env.patch"
                "gdm-pass-gdk-pixbuf-loader-env.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       ,#~(list
           "-Dplymouth=disabled"
           "-Dsystemd-journal=false"

           ;; Using --with-initial-vt=7 allows GDM to run alongside TTY 1,
           ;; instead of having to replace it (i.e., stopping the mingetty
           ;; service for TTY 1 before starting GDM).
           "-Dinitial-vt=7"

           ;; Use elogind instead of systemd.
           "-Dlogind-provider=elogind"
           "-Dsystemdsystemunitdir=no"
           "-Dsystemduserunitdir=no"

           ;; Use '/etc/environment' for locale settings instead of the
           ;; systemd-specific '/etc/locale.conf'.
           "-Dlang-file=/etc/environment"

           (string-append "-Dudev-dir=" #$output "/lib/udev")

           "--localstatedir=/var"
           (string-append "-Ddefault-path="
                          (string-join '("/run/setuid-programs"
                                         "/run/current-system/profile/bin"
                                         "/run/current-system/profile/sbin")
                                       ":"))
           ;; Put GDM in bindir so that glib-or-gtk-build-system wraps the
           ;; XDG_DATA_DIRS so that it finds its schemas.
           "--sbindir" (string-append #$output "/bin"))
       #:phases
       (modify-phases %standard-phases
         (add-before
             'configure 'pre-configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; We don't have <systemd/sd-daemon.h>.
             (substitute* '("common/gdm-log.c"
                            "daemon/gdm-server.c"
                            "daemon/gdm-session-worker.c"
                            "daemon/gdm-session-worker-job.c")
               (("#include <systemd/sd-daemon\\.h>") ""))
             ;; Use elogind for sd-login.
             (substitute* '("common/gdm-common.c"
                            "daemon/gdm-local-display-factory.c"
                            "daemon/gdm-manager.c"
                            "libgdm/gdm-user-switching.c")
               (("#include <systemd/sd-login\\.h>")
                "#include <elogind/sd-login.h>"))
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
             ;; Find the configuration file using an environment variable.
             (substitute* '("common/gdm-settings.c")
               (("GDM_CUSTOM_CONF")
                (string-append "(g_getenv(\"GDM_CUSTOM_CONF\") != NULL"
                               " ? g_getenv(\"GDM_CUSTOM_CONF\")"
                               " : GDM_CUSTOM_CONF)")))
             ;; Use service-supplied path to X.
             (substitute* '("daemon/gdm-server.c")
               (("\\(X_SERVER X_SERVER_ARG_FORMAT")
                "(\"%s\" X_SERVER_ARG_FORMAT, g_getenv (\"GDM_X_SERVER\")"))
             (substitute* '("daemon/gdm-wayland-session.c"
                            "daemon/gdm-x-session.c")
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
             ;; Do not automatically select the placeholder session.
             (substitute* "daemon/gdm-session.c"
               (("!g_str_has_suffix [(]base_name, \"\\.desktop\"[)]")
                (string-append "!g_str_has_suffix (base_name, \".desktop\") || "
                               "(g_strcmp0(search_dirs[i], \""
                               (assoc-ref outputs "out") "/share/gdm/BuiltInSessions/"
                               "\") == 0 && "
                               "g_strcmp0(base_name, \"fail.desktop\") == 0)"))
               (("g_error [(]\"GdmSession: no session desktop files installed, aborting\\.\\.\\.\"[)];")
                "{ self->fallback_session_name = g_strdup(\"fail\"); goto out; }"))))
         (add-before 'install 'install-logo
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (guix-icons (assoc-ref inputs "guix-icons"))
                    (icon
                     (string-append guix-icons "/share/icons/hicolor/\
scalable/apps/guix-white-icon.svg"))
                    (schema
                     (string-append out "/share/glib-2.0/schemas/\
org.gnome.login-screen.gschema.override")))
               (mkdir-p (dirname schema))
               (with-output-to-file schema
                 (lambda ()
                   (format #t "\
[org.gnome.login-screen]
logo='~a'~%" icon))))))
         ;; GDM requires that there be at least one desktop entry
         ;; file.  This phase installs a hidden one that simply
         ;; fails.  This enables users to use GDM with a
         ;; '~/.xsession' script with no other desktop entry files.
         ;; See <https://bugs.gnu.org/35068>.
         (add-after 'install 'install-placeholder-desktop-entry
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (sessions (string-append out "/share/gdm/BuiltInSessions"))
                    (fail (string-append sessions "/fail.desktop")))
               (mkdir-p sessions)
               (with-output-to-file fail
                 (lambda ()
                   (for-each
                    display
                    '("[Desktop Entry]\n"
                      "Encoding=UTF-8\n"
                      "Type=Application\n"
                      "Name=Fail\n"
                      "Comment=This session fails immediately.\n"
                      "NoDisplay=true\n"
                      "Exec=false\n")))))))
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
                           (find-files
                            (string-append settings "/etc/xdg"))))))))))
    (native-inputs
     `(("dconf" ,dconf)
       ("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("guix-icons" ,guix-icons)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     (list accountsservice
           check-0.14 ;for testing
           elogind
           eudev
           gnome-session
           gnome-settings-daemon
           gtk+
           iso-codes
           libcanberra
           linux-pam))
    (synopsis "Display manager for GNOME")
    (home-page "https://wiki.gnome.org/Projects/GDM/")
    (description
     "GNOME Display Manager is a system service that is responsible for
providing graphical log-ins and managing local and remote displays.")
    (license license:gpl2+)))

(define-public libgtop
  (package
    (name "libgtop")
    (version "2.40.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libgtop/"
                                  (version-major+minor version) "/"
                                  "libgtop-" version ".tar.xz"))
              (sha256
               (base32
                "1m6jbqk8maa52gxrf223442fr5bvvxgb7ham6v039i3r1i62gwvq"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gobject-introspection intltool perl pkg-config))
    (propagated-inputs
     (list glib)) ; required by libgtop-2.0.pc
    (synopsis "Portable system access library")
    (home-page "https://www.gnome.org/")
    (description
     "LibGTop is a library to get system specific data such as CPU and memory
usage and information about running processes.")
    (license license:gpl2+)))

(define-public gnome-bluetooth
  (package
    (name "gnome-bluetooth")
    (version "3.34.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-bluetooth/"
                                  (version-major+minor version) "/"
                                  "gnome-bluetooth-" version ".tar.xz"))
              (sha256
               (base32
                "1a9ynlwwkb3wpg293ym517vmrkk63y809mmcv9a21k5yr199x53c"))))
    (build-system meson-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for gdbus-codegen, etc.
       ("gtk+" ,gtk+ "bin") ; gtk-update-icon-cache
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("python-dbus" ,python-dbus)
       ("xmllint" ,libxml2)))
    (propagated-inputs
     ;; gnome-bluetooth-1.0.pc refers to all these.
     (list glib gtk+))
    (inputs
     (list eudev libcanberra libnotify python-dbus))
    (synopsis "GNOME Bluetooth subsystem")
    (home-page "https://wiki.gnome.org/Projects/GnomeBluetooth")
    (description
     "This package contains tools for managing and manipulating Bluetooth
devices using the GNOME desktop.")
    (license license:lgpl2.1+)))

(define-public gnome-control-center
  (package
    (name "gnome-control-center")
    (version "41.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0j72ixhli621psbrma86qxy0spv6gpjx6k9hg2jih97c6dmzqwc2"))
              (patches (search-patches
                        "gnome-control-center-libexecdir.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       (list "-Dcheese=false"
             (string-append "-Dgnome_session_libexecdir="
                            (assoc-ref %build-inputs "gnome-session")
                            "/libexec"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libc   (assoc-ref inputs "libc"))
                   (tzdata (assoc-ref inputs "tzdata"))
                   (libgnomekbd (assoc-ref inputs "libgnomekbd"))
                   (nm-applet   (assoc-ref inputs "network-manager-applet"))
                   (gnome-desktop (assoc-ref inputs "gnome-desktop")))
               (substitute* "panels/datetime/tz.h"
                 (("/usr/share/zoneinfo/zone.tab")
                  (string-append tzdata "/share/zoneinfo/zone.tab")))
               (substitute* "tests/datetime/test-endianess.c"
                 (("/usr/share/locale")
                  (string-append libc "/share/locale")))
               (substitute* "panels/region/cc-region-panel.c"
                 (("\"gkbd-keyboard-display")
                  (string-append "\"" libgnomekbd
                                 "/bin/gkbd-keyboard-display")))
               (substitute* '("panels/network/net-device-bluetooth.c"
                              "panels/network/net-device-mobile.c"
                              "panels/network/connection-editor/net-connection-editor.c")
                 (("\"nm-connection-editor")
                  (string-append "\"" nm-applet
                                 "/bin/nm-connection-editor")))
               (substitute* '("panels/user-accounts/run-passwd.c")
                 (("/usr/bin/passwd")
                  "/run/setuid-programs/passwd"))
               (substitute* "panels/info-overview/cc-info-overview-panel.c"
                 (("DATADIR \"/gnome/gnome-version.xml\"")
                  (string-append "\"" gnome-desktop
                                 "/share/gnome/gnome-version.xml\""))))))
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson/meson_post_install.py"
               (("gtk-update-icon-cache") (which "true")))))
         (add-before 'install 'no-polkit-magic
           ;; Meson ‘magically’ invokes pkexec, which fails (not setuid).
           (lambda _
             (setenv "PKEXEC_UID" "something"))))))
    (native-inputs
     (list `(,glib "bin")               ;for glib-mkenums, etc.
           intltool
           pkg-config
           python
           libxslt
           ;; For tests
           hicolor-icon-theme
           python-dbusmock
           xorg-server-for-tests))
    (inputs
     (list accountsservice
           clutter-gtk
           colord-gtk
           cups
           dconf
           docbook-xsl
           gcr
           gnome-bluetooth
           gnome-desktop
           gnome-online-accounts
           `(,gnome-online-accounts "lib")
           gnome-session
           gnome-settings-daemon
           grilo
           gsound
           ibus
           libcanberra
           libgnomekbd
           libgudev
           libgtop
           libnma
           libpwquality
           librsvg                      ;for loading SVG files
           libsecret
           libsoup-minimal-2
           libxml2
           libwacom
           mesa
           mit-krb5
           modem-manager
           network-manager-applet
           polkit
           pulseaudio
           samba
           tzdata
           udisks
           upower))
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
    (version "41.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0ragmcln210zvzhc2br33yprbkj9drjzd7inp5sdxra0a7l73yaj"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:disallowed-references ,(list (gexp-input glib "bin")
                                      (gexp-input libxslt)
                                      (gexp-input ruby-sass))
       #:configure-flags
       (list "-Dsystemd=false"
             ;; Otherwise, the RUNPATH will lack the final path component.
             (string-append "-Dc_link_args=-Wl,-rpath="
                            (assoc-ref %outputs "out")
                            "/lib/gnome-shell"))

       #:modules ((guix build meson-build-system)
                  (guix build utils)
                  (srfi srfi-1))

       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-keysdir
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (keysdir (string-append
                              out "/share/gnome-control-center/keybindings")))
               (substitute* "meson.build"
                 (("keysdir =.*")
                  (string-append "keysdir = '" keysdir "'\n"))))))
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson/postinstall.py"
               (("gtk-update-icon-cache") "true"))))
         (add-before 'configure 'record-absolute-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "js/misc/ibusManager.js"
               (("'ibus-daemon'")
                (string-append "'" (assoc-ref inputs "ibus")
                               "/bin/ibus-daemon'")))
             (substitute* "js/ui/status/keyboard.js"
               (("'gkbd-keyboard-display'")
                (string-append "'" (assoc-ref inputs "libgnomekbd")
                               "/bin/gkbd-keyboard-display'")))))
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Tests require a running X server.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")
             (setenv "HOME" "/tmp")))   ;to avoid "fatal" warnings
         (add-after 'install 'wrap-programs
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out              (assoc-ref outputs "out"))
                   (gi-typelib-path  (getenv "GI_TYPELIB_PATH"))
                   (python-path      (getenv "GUIX_PYTHONPATH")))
               (for-each
                (lambda (prog)
                  (wrap-program (string-append out "/bin/" prog)
                    `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
                '("gnome-shell" "gnome-extensions-app"))
               (substitute* (string-append out "/share/gnome-shell/"
                                           "org.gnome.Shell.Extensions")
                 (("imports\\.package\\.start" all)
                  (string-append "'" gi-typelib-path "'.split(':').forEach("
                                 "path => imports.gi.GIRepository.Repository."
                                 "prepend_search_path(path));\n"
                                 all)))
               (for-each
                (lambda (prog)
                  (wrap-program (string-append out "/bin/" prog)
                    `("GUIX_PYTHONPATH"      ":" prefix (,python-path))
                    `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
                '("gnome-shell-perf-tool")))))
         (add-after 'install 'rewire
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (for-each
              (lambda (tool)
                (call-with-output-file (string-append
                                        (assoc-ref outputs "out")
                                        "/bin/" tool)
                  (lambda (port)
                    (format port "#!~a
printf '~a is deprecated.  Use the \"gnome-extensions\" CLI or \
\"gnome-extensions-app\" instead.\\n'"
                            (search-input-file inputs "bin/bash")
                            tool))))
              '("gnome-shell-extension-tool" "gnome-shell-extension-prefs"))))
         (replace 'glib-or-gtk-wrap
           (let ((wrap (assoc-ref %standard-phases 'glib-or-gtk-wrap)))
             (lambda* (#:key inputs outputs #:allow-other-keys #:rest rest)
               ;; By default intltool et al. would end up in the XDG_DATA_DIRS
               ;; settings of the wrappers created by the 'glib-or-gtk-wrap'
               ;; phase.  Fix that since we don't need these.
               (wrap #:inputs (fold alist-delete inputs
                                    '("intltool" "glib:bin"))
                     #:outputs outputs)))))))
    (native-inputs
     `(("asciidoc" ,asciidoc)
       ("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("desktop-file-utils" ,desktop-file-utils) ; for update-desktop-database
       ("gobject-introspection" ,gobject-introspection)
       ("hicolor-icon-theme" ,hicolor-icon-theme)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("ruby-sass" ,ruby-sass)
       ("sassc" ,sassc)
       ("xsltproc" ,libxslt)
       ;; For tests
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     (list accountsservice
           caribou
           docbook-xsl
           evolution-data-server
           gcr
           gdm
           librsvg
           gjs
           gtk
           gnome-autoar
           gnome-bluetooth
           gnome-desktop
           gnome-settings-daemon
           graphene
           gst-plugins-base
           ibus
           libcanberra
           libcroco
           libgnomekbd ;for gkbd-keyboard-display
           libgweather
           libnma
           libsoup
           mesa-headers
           mutter
           network-manager-applet
           polkit
           pulseaudio
           python-pygobject
           startup-notification
           telepathy-logger
           upower
           ;; XXX: These requirements were added in 3.24, but no mention in NEWS.
           ;; Missing propagation? See also: <https://bugs.gnu.org/27264>
           librsvg
           geoclue))
    (synopsis "Desktop shell for GNOME")
    (home-page "https://wiki.gnome.org/Projects/GnomeShell")
    (description
     "GNOME Shell provides core user interface functions for the GNOME desktop,
like switching to windows and launching applications.")
    (license license:gpl2+)))

(define-public gtk-vnc
  (package
    (name "gtk-vnc")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1060ws037v556rx1qhfrcg02859rscksrzr8fq11himdg4d1y6m8"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t))   ; To wrap binaries and/or compile schemas
    (native-inputs
     `(;; GJS depends on Rust, which is x86_64-only so far, so remove the GJS
       ;; dependency on other platforms (FIXME).
       ,@(if (target-x86-64?)
             `(("gjs" ,gjs))
             '())
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("node" ,node)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("vala" ,vala)))
    (inputs
     `(("cairo" ,cairo)
       ("librsvg" ,(librsvg-for-system))
       ("glib" ,glib)
       ("gnutls" ,gnutls)
       ("libgcrypt" ,libgcrypt)
       ("libsasl" ,cyrus-sasl)
       ("pulseaudio" ,pulseaudio)
       ("x11" ,libx11)
       ("zlib" ,zlib)))
    (propagated-inputs
     (list gtk+))
    (synopsis "VNC client viewer widget for GTK+")
    (description "GTK-VNC is a project providing client side APIs for the RFB
protocol / VNC remote desktop technology.  It is built using coroutines allowing
it to be completely asynchronous while remaining single threaded.  It provides a
core C library, and bindings for Python (PyGTK).")
    (home-page "https://wiki.gnome.org/Projects/gtk-vnc")
    (license license:lgpl2.1+)))

(define-public gnome-autoar
  (package
    (name "gnome-autoar")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "03hmm7cjgjvyxlflghfa89s1amj16qapl2c9pv0r2bfrp87dasv4"))))
    (build-system meson-build-system)
    (native-inputs
     (list gobject-introspection
           `(,glib "bin") pkg-config))
    (propagated-inputs
     (list libarchive))  ; Required by gnome-autoar-0.pc
    (inputs
     (list gtk+))
    (synopsis "Archives integration support for GNOME")
    (home-page "https://git.gnome.org/browse/gnome-autoar/")
    (description
     "GNOME Autoar is a library which makes creating and extracting archives
easy, safe, and automatic.")
    (license license:lgpl2.1+)))

(define-public tracker
  (package
    (name "tracker")
    (version "3.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/tracker/"
                                  (version-major+minor version) "/"
                                  "tracker-" version ".tar.xz"))
              (sha256
               (base32
                "13zcc07wd51sz7kglk3xbjrsq7d835cxfr7iwjr7nn2xcri8jdns"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       ;; Otherwise, the RUNPATH will lack the final path component.
       (list (string-append "-Dc_link_args=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib:"
                            (assoc-ref %outputs "out") "/lib/tracker-3.0")
             "-Ddocs=false"
             "-Dsystemd_user_services=false")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-shell
           (lambda _
             (setenv "SHELL" (which "bash"))))
         (add-before 'configure 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((manpage "/etc/asciidoc/docbook-xsl/manpage.xsl")
                    (file (search-input-file inputs manpage)))
               (substitute* "docs/manpages/meson.build"
                 (("/etc/asciidoc[^']+")
                  file)))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Some tests expect to write to $HOME.
               (setenv "HOME" "/tmp")
               (invoke "dbus-run-session" "--" "meson" "test"
                       "--print-errorlogs")))))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("docbook-xsl" ,docbook-xsl)
       ("docbook-xml-4.5" ,docbook-xml)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("asciidoc" ,asciidoc)
       ("xsltproc" ,libxslt)
       ("cmake-minimal" ,cmake-minimal)
       ("python-pygobject" ,python-pygobject)
       ("gtk-doc" ,gtk-doc/stable)
       ("intltool" ,intltool)
       ("dbus" ,dbus)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("vala" ,vala)))
    (inputs
     `(("dbus" ,dbus)
       ("sqlite" ,sqlite)
       ("libxml2" ,libxml2)
       ("icu4c" ,icu4c)                 ; libunistring gets miner-miner-fs test to fail.
       ("json-glib" ,json-glib)
       ("libsoup" ,libsoup-minimal-2))) ; tracker-miners requires the same version.
    (synopsis "Metadata database, indexer and search tool")
    (home-page "https://wiki.gnome.org/Projects/Tracker")
    (description
     "Tracker is a search engine and triplestore for desktop, embedded and mobile.

It is a middleware component aimed at desktop application developers who want
their apps to browse and search user content.  It's not designed to be used
directly by desktop users, but it provides a commandline tool named
@command{tracker} for the adventurous.

Tracker allows your application to instantly perform full-text searches across
all documents.  This feature is used by the @{emph{search} bar in GNOME Files, for
example.  This is achieved by indexing the user's home directory in the
background.

Tracker also allows your application to query and list content that the user
has stored.  For example, GNOME Music displays all the music files that are
found by Tracker.  This means that GNOME Music doesn't need to maintain a
database of its own.

If you need to go beyond simple searches, Tracker is also a linked data
endpoint and it understands SPARQL.")
    ;; https://gitlab.gnome.org/GNOME/tracker/-/blob/master/COPYING:
    ;; src/libtracker-*/* and src/tracker-extract/* are covered by lgpl2.1+,
    ;; libstemmer is bsd-3 and the rest is gpl2+.
    (license (list license:gpl2+
                   license:bsd-3
                   license:lgpl2.1+))))

(define-public tracker-miners
  (package
    (name "tracker-miners")
    (version "3.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/tracker-miners/"
                                  (version-major+minor version)
                                  "/tracker-miners-" version ".tar.xz"))
              (sha256
               (base32
                "0fpd69lgm8cckbamcf9c2q57glxf0s3jcfwkq8p3s4lfsvdclmd0"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       (list "-Dminer_rss=false" ; libgrss is required.
             ;; Ensure the RUNPATH contains all installed library locations.
             (string-append "-Dc_link_args=-Wl,-rpath="
                            (assoc-ref %outputs "out")
                            "/lib/tracker-miners-3.0")
             ;; TODO: Enable functional tests. Currently, the following error
             ;; appears:
             ;; Exception: The functional tests require DConf to be the default
             ;; GSettings backend. Got GKeyfileSettingsBackend instead.
             "-Dfunctional_tests=false"
             "-Dsystemd_user_services=false")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-shell
           (lambda _
             (setenv "SHELL" (which "bash"))))
         (add-before 'configure 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((manpage "/etc/asciidoc/docbook-xsl/manpage.xsl")
                    (file (search-input-file inputs manpage)))
               (substitute* "docs/manpages/meson.build"
                 (("/etc/asciidoc[^']+")
                  file)))))
         (add-before 'configure 'fix-tests
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Disable those tests that require the functional_tests option
             ;; to be true and the UPower daemon to be started.
             (substitute* "examples/python/meson.build"
               (("foreach example_name:.*")
                "foreach example_name: []"))
             ;; Disable this test that is failing randomly:
             ;; https://gitlab.gnome.org/GNOME/tracker-miners/-/issues/170.
            (substitute* "tests/libtracker-miner/meson.build"
               (("'miner-fs'.*")
                ""))))
        (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Some tests expect to write to $HOME.
               (setenv "HOME" "/tmp")
               (setenv "LANG" "en_US.UTF-8")
               (invoke "dbus-run-session" "--" "meson" "test"
                       "--print-errorlogs")))))))
    (native-inputs
     `(("dbus" ,dbus)
       ("intltool" ,intltool)
       ("glib:bin" ,glib "bin")
       ("docbook-xsl" ,docbook-xsl)
       ("docbook-xml-4.5" ,docbook-xml)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("asciidoc" ,asciidoc)
       ("xsltproc" ,libxslt)
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("python-pygobject" ,python-pygobject)))
    (inputs
     `(("exempi" ,exempi)
       ("ffmpeg" ,ffmpeg)
       ("flac" ,flac)
       ("giflib" ,giflib)
       ("glib" ,glib)
       ("gstreamer" ,gstreamer)
       ("icu4c" ,icu4c)
       ("json-glib" ,json-glib)
       ("libcue" ,libcue)
       ("libexif" ,libexif)
       ("libgsf" ,libgsf)
       ("libgxps" ,libgxps)
       ("libiptcdata" ,libiptcdata)
       ("libjpeg" ,libjpeg-turbo)
       ("libosinfo" ,libosinfo)
       ("libpng" ,libpng)
       ("libseccomp" ,libseccomp)
       ("libsoup" ,libsoup-minimal-2) ; because tracker requires this version.
       ("libtiff" ,libtiff)
       ("libvorbis" ,libvorbis)
       ("libxml2" ,libxml2)
       ("poppler" ,poppler)
       ("shared-mime-info" ,shared-mime-info)
       ("taglib" ,taglib)
       ("totem-pl-parser" ,totem-pl-parser)
       ("tracker" ,tracker)
       ("upower" ,upower)
       ("zlib" ,zlib)))
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
    (version "41.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0bsqh241m43x3qb3z0mjakjic610ncl95zhjdgls649phnc30qry"))
              (patches
               ;; This patch is already upstream and can be removed next
               ;; release.
               (search-patches "nautilus-add-libportal-gtk3.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-extensible
           (lambda _
             (substitute* "src/nautilus-module.c"
               (("static gboolean initialized = FALSE;" all)
                (string-append all "
const char *extension_path;
char **extension_dirs, **d;
")
                )
               (("load_module_dir \\(NAUTILUS_EXTENSIONDIR\\);" all)
                (string-append all
                               "
extension_path = g_getenv (\"NAUTILUS_EXTENSION_PATH\");
if (extension_path)
{
    extension_dirs = g_strsplit (extension_path, \":\", -1);
    for (d = extension_dirs; d != NULL && *d != NULL; d++)
        load_module_dir(*d);
    g_strfreev(extension_dirs);
}
")))))
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson/postinstall.py"
               (("gtk-update-icon-cache") "true")))))
       ;; XXX: FAIL: check-nautilus
       ;;   Settings schema 'org.gnome.nautilus.preferences' is not installed
       #:tests? #f))
    (native-inputs
     (list desktop-file-utils     ; for update-desktop-database
           `(,glib "bin")         ; for glib-mkenums, etc.
           gobject-introspection
           intltool
           pkg-config
           python))
    (inputs
     (list dconf
           gexiv2
           gvfs
           exempi
           gnome-desktop
           gnome-autoar
           gst-plugins-base
           json-glib
           libhandy
           libportal
           libseccomp
           libselinux
           libsoup-minimal-2 ; to satisfy tracker dependencies
           tracker
           tracker-miners
           ;; XXX: gtk+ is required by libnautilus-extension.pc
           ;;
           ;; Don't propagate it to reduces "profile pollution" of the 'gnome' meta
           ;; package.  See:
           ;; <http://lists.gnu.org/archive/html/guix-devel/2016-03/msg00283.html>.
           gtk+
           libexif
           libxml2))
    (native-search-paths
     (list (search-path-specification
            (variable "NAUTILUS_EXTENSION_PATH")
            (files '("lib/nautilus/site-extensions")))))
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
    (version "40.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/"
                    (version-major version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "19yii3bdgivxrcka1c4g6dpbmql5nyawwhzlsph7z6bs68nambm6"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("desktop-file-utils" ,desktop-file-utils) ; for update-desktop-database
       ("gtk+-bin" ,gtk+ "bin") ; for gtk-update-icon-cache
       ("itstool" ,itstool)
       ("xmllint" ,libxml2)
       ("glib" ,glib "bin")
       ("vala" ,vala)))
    (inputs
     (list gtk+ libhandy))
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
    (version "41.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0i9id5z72dqddh94648zylyf41amqq5lhny8sbyg1v8v4q6sr88x"))))
    (build-system meson-build-system)
    (native-inputs (list gettext-minimal))
    (home-page "https://gitlab.gnome.org/GNOME/gnome-backgrounds")
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
    (version "40.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1qm544ymwibk31s30k47vnn79xg30m18r7l4di0c57g375dak31n"))
       (patches
        (search-patches "gnome-screenshot-meson-0.60.patch"))))
    (build-system meson-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/postinstall.py"
               (("gtk-update-icon-cache") "true")))))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("desktop-file-utils" ,desktop-file-utils) ; for update-desktop-database
       ("intltool" ,intltool)
       ("appstream-glib" ,appstream-glib)
       ("pkg-config" ,pkg-config)
       ("python" ,python)))
    (inputs
     (list gtk+ libcanberra libhandy libx11 libxext))
    (home-page "https://gitlab.gnome.org/GNOME/gnome-screenshot")
    (synopsis "Take pictures of your screen")
    (description
     "GNOME Screenshot is a utility used for taking screenshots of the entire
screen, a window or a user defined area of the screen, with optional
beautifying border effects.")
    (license license:gpl2+)))

(define-public dconf-editor
  (package
    (name "dconf-editor")
    (version "3.38.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1qvrxrk1h8bd75xwasxbvlkqrw6xkavjimvc7sslkw6lvb3z86jp"))))
    (build-system meson-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-schemas, gio-2.0.
       ("gtk+-bin" ,gtk+ "bin") ; for gtk-update-icon-cache
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     (list dconf gtk+ libxml2))
    (home-page "https://gitlab.gnome.org/GNOME/dconf-editor")
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
     (list nautilus evince))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (apps (string-append out "/share/applications")))
           (mkdir-p apps)
           (call-with-output-file (string-append apps "/gnome-mimeapps.list")
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

(define-public libgovirt
  (package
   (name "libgovirt")
   (version "0.3.8")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "1y0x1wyakj3ya33hgj0w1jkbcn50q21gmn2zyalxysqp55i1ij8x"))))
   (build-system glib-or-gtk-build-system)
   (native-inputs
    (list gettext-minimal
          `(,glib "bin")
          gobject-introspection
          gsettings-desktop-schemas
          `(,gtk+ "bin")
          pkg-config))
   (inputs
    (list glib-networking ; GIO plugin--for the tests
          rest))
   (synopsis "GoVirt Library")
   (description "GoVirt is a GObject wrapper for the oVirt REST API.")
   (home-page "https://gitlab.gnome.org/GNOME/libgovirt")
   (license license:gpl2+)))

(define-public gnome-weather
  (package
   (name "gnome-weather")
   (version "40.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major version) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "0k9wnyinvx6433r07kvjyahgqc605g7gbpf3d0h6vi4p8x61849x"))))
   (build-system meson-build-system)
   (native-inputs
    `(("gettext" ,gettext-minimal)
      ("glib:bin" ,glib "bin")
      ("gobject-introspection" ,gobject-introspection)
      ("gtk+:bin" ,gtk+ "bin")
      ("pkg-config" ,pkg-config)))
   (inputs
    (list appstream-glib
          geoclue
          gjs
          gnome-desktop
          libgweather
          libhandy))
   (arguments
    `(#:glib-or-gtk? #t
      #:phases
      ,#~(modify-phases %standard-phases
           (add-after 'unpack 'fix-service-file
             (lambda _
               (substitute* "data/org.gnome.Weather.service.in"
                 (("Exec=[[:graph:]]+")
                  (string-append "Exec=" #$output
                                 "/bin/gnome-weather")))))
           (add-after 'install 'fix-desktop-file
             ;; FIXME: "gapplication launch org.gnome.Weather" fails for some
             ;; reason.  See https://issues.guix.gnu.org/issue/39324.
             (lambda _
               (let ((applications
                      (string-append #$output "/share/applications")))
                 (substitute* (string-append applications
                                             "/org.gnome.Weather.desktop")
                   (("Exec=.*") "Exec=gnome-weather\n")))))
           (add-after 'install 'wrap
             (lambda _
               (let ((gi-typelib-path   (getenv "GI_TYPELIB_PATH")))
                 ;; GNOME Weather needs the typelib files of GTK+, Pango etc
                 ;; at runtime.
                 (wrap-program (string-append #$output "/bin/gnome-weather")
                   `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))))))))
   (synopsis "Weather monitoring for GNOME desktop")
   (description "GNOME Weather is a small application that allows you to
monitor the current weather conditions for your city, or anywhere in the
world.")
   (home-page "https://wiki.gnome.org/Apps/Weather")
   (license license:gpl2+)))

(define-public gnome
  (package
    (name "gnome")
    (version (package-version gnome-shell))
    (source #f)
    (build-system trivial-build-system)
    (arguments '(#:builder (begin (mkdir %output) #t)))
    (propagated-inputs
     `(;; GNOME-Core-OS-Services
       ("accountsservice" ,accountsservice)
       ("network-manager" ,network-manager)
       ("packagekit" ,packagekit)
       ("upower" ,upower)
       ;; GNOME-Core-Shell
       ("adwaita-icon-theme" ,adwaita-icon-theme)
       ("gdm" ,gdm)
       ("glib-networking" ,glib-networking)
       ("gnome-backgrounds" ,gnome-backgrounds)
       ("gnome-bluetooth" ,gnome-bluetooth)
       ("gnome-color-manager" ,gnome-color-manager)
       ("gnome-control-center" ,gnome-control-center)
       ("gnome-desktop" ,gnome-desktop)
       ("gnome-initial-setup" ,gnome-initial-setup)
       ("gnome-keyring" ,gnome-keyring)
       ("gnome-menus" ,gnome-menus)
       ("gnome-session" ,gnome-session)
       ("gnome-settings-daemon" ,gnome-settings-daemon)
       ("gnome-shell-extensions" ,gnome-shell-extensions)
       ("gnome-shell" ,gnome-shell)
       ("gnome-themes-extra" ,gnome-themes-extra)
       ("gnome-user-docs" ,gnome-user-docs)
       ("gnome-user-share" ,gnome-user-share)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gvfs" ,gvfs)
       ("mutter" ,mutter)
       ("orca" ,orca)
       ("rygel" ,rygel)
       ("sushi" ,sushi)
       ;; GNOME-Core-Utilities
       ("baobab" ,baobab)
       ("cheese" ,cheese)
       ;; XXX: EoG requires librsvg-next, which depends on Rust, which currently
       ;; only works on x86_64, so exclude it on other architectures.
       ,@(if (string-prefix? "x86_64" (%current-system))
             `(("eog" ,eog))
             '())
       ("epiphany" ,epiphany)
       ("evince" ,evince)
       ("file-roller" ,file-roller)
       ("gedit" ,gedit)
       ("gnome-boxes" ,gnome-boxes)
       ("gnome-calculator" ,gnome-calculator)
       ("gnome-calendar" ,gnome-calendar)
       ("gnome-characters" ,gnome-characters)
       ("gnome-clocks" ,gnome-clocks)
       ("gnome-contacts" ,gnome-contacts)
       ("gnome-disk-utility" ,gnome-disk-utility)
       ("gnome-font-viewer" ,gnome-font-viewer)
       ("gnome-maps" ,gnome-maps)
       ("gnome-music" ,gnome-music)
       ("gnome-photos" ,gnome-photos)
       ("gnome-screenshot" ,gnome-screenshot)
       ("gnome-system-monitor" ,gnome-system-monitor)
       ("gnome-terminal" ,gnome-terminal)
       ("gnome-weather" ,gnome-weather)
       ("nautilus" ,nautilus)
       ("simple-scan" ,simple-scan)
       ("totem" ,totem)
       ("tracker-miners" ,tracker-miners)
       ("yelp" ,yelp)
       ;; Others
       ("hicolor-icon-theme" ,hicolor-icon-theme)
       ("gnome-online-accounts" ,gnome-online-accounts)

       ;; Packages not part of GNOME proper but that are needed for a good
       ;; experience.  See <https://bugs.gnu.org/39646>.
       ;; XXX: Find out exactly which ones are needed and why.
       ("font-abattis-cantarell"            ,font-abattis-cantarell)
       ("font-dejavu"               ,font-dejavu)
       ("at-spi2-core"              ,at-spi2-core-minimal)
       ("dbus"                      ,dbus)
       ("dconf"                     ,dconf)
       ("desktop-file-utils"        ,desktop-file-utils)
       ("gnome-default-applications" ,gnome-default-applications)
       ("gnome-themes-standard"     ,gnome-themes-standard)
       ("gst-plugins-base"          ,gst-plugins-base)
       ("gst-plugins-good"          ,gst-plugins-good)
       ("gucharmap"                 ,gucharmap)
       ("pinentry-gnome3"           ,pinentry-gnome3)
       ("pulseaudio"                ,pulseaudio)
       ("shared-mime-info"          ,shared-mime-info)
       ("system-config-printer"     ,system-config-printer)
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
           (add-before 'bootstrap 'build-without-Werror
             (lambda _
               ;; The build system cleverly detects that we're not building from
               ;; a release tarball and turns on -Werror for GCC.
               ;; Unsurprisingly, there is a warning during compilation that
               ;; causes the build to fail unnecessarily, so we remove the flag.
               (substitute* '("configure.ac")
                 (("-Werror") ""))
               #t)))))
      (native-inputs
       (list autoconf
             automake
             gnome-common
             intltool
             libtool
             pkg-config
             which))
      (inputs
       (list glib gstreamer gst-plugins-base gtk+))
      (synopsis "Desktop recording program")
      (description "Byzanz is a simple desktop recording program with a
command-line interface.  It can record part or all of an X display for a
specified duration and save it as a GIF encoded animated image file.")
      (home-page "https://git.gnome.org/browse/byzanz")
      (license license:gpl2+))))

(define-public authenticator
  (package
    (name "authenticator")
    (version "3.32.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/World/Authenticator")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c4r9rnrz5gazrfg0z2rcwax4nscs7z391bcjcl74k6ln3blwzpr"))))
    (build-system meson-build-system)
    (arguments
     `(#:meson ,meson-0.59
       #:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((prog (string-append (assoc-ref outputs "out")
                                        "/bin/authenticator"))
                   (pylib (string-append (assoc-ref outputs "out")
                                         "/lib/python"
                                         ,(version-major+minor
                                           (package-version
                                            (this-package-input "python")))
                                         "/site-packages")))
               (wrap-program prog
                 `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH") ,pylib))
                 `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))))))))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           `(,gtk+ "bin")
           pkg-config))
    (inputs
     (list gsettings-desktop-schemas
           gtk+
           libhandy-0.0
           libsecret
           python
           python-beautifulsoup4
           python-pillow
           python-pyfavicon
           python-pygobject
           python-pyotp
           python-pyzbar
           yoyo-migrations
           zbar))
    (home-page "https://gitlab.gnome.org/World/Authenticator/")
    (synopsis "Two-factor authentication application built for GNOME")
    (description
     "Authenticator is a two-factor authentication (2FA) application built for
the GNOME desktop environment.

Features:

@itemize
@item QR code scanner
@item Beautiful UI
@item Huge database of more than 560 supported services
@item Keep your PIN tokens secure by locking the application with a password
@item Automatically fetch an image for services using their favicon
@item The possibility to add new services
@end itemize")
    (license license:gpl3+)))

(define-public gsound
  (package
    (name "gsound")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gsound/"
                                  (version-major+minor version) "/"
                                  "gsound-" version ".tar.xz"))
              (sha256
               (base32
                "06l80xgykj7x1kqkjvcq06pwj2rmca458zvs053qc55x3sg06bfa"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config gobject-introspection vala))
    (propagated-inputs
     (list libcanberra))   ; in Requires.private of gsound.pc
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
     (list gobject-introspection intltool pkg-config))
    (inputs
     (list `(,gnome-online-accounts "lib") json-glib rest))
    (home-page "https://wiki.gnome.org/Projects/Zapojit")
    (synopsis "Library for accessing SkyDrive and Hotmail")
    (description
     "Libzapojit is a GLib-based library for accessing online service APIs of
Microsoft SkyDrive and Hotmail, using their REST protocols.")
    (license license:lgpl2.1+)))

(define-public gnome-clocks
  (package
    (name "gnome-clocks")
    (version "40.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "02d3jg46sn8d9gd4dsaly22gg5vkbz2gpq4pmwpvncb4rsqk7sn2"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/post-install.py"
               (("gtk-update-icon-cache") "true")))))))
    (native-inputs
     `(("vala" ,vala)
       ("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")             ; for glib-compile-resources
       ("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("itstool" ,itstool)))
    (inputs
     (list glib
           gtk+
           gsound
           geoclue
           geocode-glib
           libgweather
           libhandy
           gnome-desktop))
    (home-page "https://wiki.gnome.org/Apps/Clocks")
    (synopsis "GNOME's clock application")
    (description
     "GNOME Clocks is a simple clocks application designed to fit the GNOME
desktop.  It supports world clock, stop watch, alarms, and count down timer.")
    (license license:gpl3+)))

(define-public gnome-calendar
  (package
    (name "gnome-calendar")
    (version "40.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0czasxnmbw2zm21lmxam8qixacagciq3f815s809hn0f9byh2dwy"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       ;; gnome-calendar has to be installed before the tests can be run
       ;; https://bugzilla.gnome.org/show_bug.cgi?id=788224
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson/meson_post_install.py"
               (("gtk-update-icon-cache") "true"))
             #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib-bin" ,glib "bin")         ; For glib-compile-schemas
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("libdazzle" ,libdazzle)
       ("libedataserverui" ,evolution-data-server)
       ("libgweather" ,libgweather)
       ("libhandy" ,libhandy)
       ("geoclue" ,geoclue)))
    (propagated-inputs
     (list evolution-data-server))
    (home-page "https://wiki.gnome.org/Apps/Calendar")
    (synopsis "GNOME's calendar application")
    (description
     "GNOME Calendar is a simple calendar application designed to fit the GNOME
desktop.  It supports multiple calendars, month, week and year view.")
    (license license:gpl3+)))

(define-public gnome-todo
  (package
    (name "gnome-todo")
    (version "41.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1r94880d4khbjhhfnhaba3y3d4hv2bri82rzfzxn27s5iybpqras"))
              (patches (search-patches "gnome-todo-libportal.patch"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-gtk-update-icon-cache
            (lambda _
              (substitute* "build-aux/meson/meson_post_install.py"
                (("gtk-update-icon-cache") "true"))))
          (delete 'check)
          (add-after 'install 'check
            (assoc-ref %standard-phases
                       'check))
          (add-before 'check 'pre-check
            (lambda _
              (setenv "GSETTINGS_SCHEMA_DIR"
                      (string-append #$output "/share/glib-2.0/schemas")))))))
    (native-inputs
     (list gettext-minimal
           gobject-introspection
           `(,glib "bin")               ;for glib-compile-resources
           `(,gtk "bin")                ;for gtk-update-icon-cache
           itstool
           pkg-config))
    (inputs
     (list rest                         ;for Todoist plugin
           gtk
           json-glib                    ;for Todoist plugin
           libadwaita
           evolution-data-server
           libical
           libpeas
           libportal
           python-pygobject
           evolution-data-server
           `(,gnome-online-accounts "lib")
           gsettings-desktop-schemas))
    (home-page "https://wiki.gnome.org/Apps/Todo")
    (synopsis "GNOME's ToDo Application")
    (description "GNOME To Do is a simplistic personal task manager designed
to perfectly fit the GNOME desktop.")
    (license license:gpl3+)))

(define-public gnome-dictionary
  (package
    (name "gnome-dictionary")
    (version "40.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1d8dhcfys788vv27v34i3s3x3jdvdi2kqn2a5p8c937a9hm0qr9f"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:meson ,meson-0.59
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson/post-install.py"
               (("gtk-update-icon-cache") "true")))))))
    (native-inputs
     (list `(,glib "bin")
           gobject-introspection
           intltool
           itstool
           pkg-config
           libxml2))
    (inputs
     (list gsettings-desktop-schemas gtk+))
    (home-page "https://wiki.gnome.org/Apps/Dictionary")
    (synopsis "Look up words in dictionary sources")
    (description
     "GNOME Dictionary can look for the definition or translation of a word in
existing databases over the internet.")
    (license license:gpl3+)))

(define-public gnome-tweaks
  (package
    (name "gnome-tweaks")
    (version "40.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-tweaks/"
                                  (version-major version) "/"
                                  "gnome-tweaks-" version ".tar.xz"))
              (patches
               (list (search-patch "gnome-tweaks-search-paths.patch")))
              (sha256
               (base32
                "0sn3xsjhnini0f2dyi1ymrr3fb8mi7w5j5lsyw11rc5h67h3ypzr"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags '("-Dlocalstatedir=/tmp"
                           "-Dsysconfdir=/tmp")
       #:imported-modules ((guix build python-build-system)
                           ,@%meson-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson-postinstall.py"
               (("gtk-update-icon-cache") "true"))))
         (add-after 'install 'wrap
           (@@ (guix build python-build-system) wrap))
         (add-after 'wrap 'wrap-gi-typelib-and-python
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH")))
               (let ((python-path
                      (string-append out "/lib/python"
                                     ,(version-major+minor
                                       (package-version python))
                                     "/site-packages")))
                 (wrap-program (string-append out "/bin/gnome-tweaks")
                   `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                   `("GUIX_PYTHONPATH" ":" prefix (,python-path))))))))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-resources, etc.
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list bash-minimal ; to execute the wrapper program
           gnome-desktop
           gtk+
           gobject-introspection
           gsettings-desktop-schemas
           libhandy
           libnotify
           libsoup
           nautilus
           python
           python-pygobject))
    (synopsis "Customize advanced GNOME 3 options")
    (home-page "https://wiki.gnome.org/Apps/Tweaks")
    (description
     "GNOME Tweaks allows adjusting advanced configuration settings in
GNOME 3.  This includes things like the fonts used in user interface elements,
alternative user interface themes, changes in window management behavior,
GNOME Shell appearance and extension, etc.")
    (license license:gpl3+)))

(define-public gnome-shell-extensions
  (package
    (name "gnome-shell-extensions")
    (version "41.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0s7wcsa0ysdgh5zpxpa95id5rz4ajb2r1561v5szjf4b8k0slkyq"))))
    (build-system meson-build-system)
    (arguments
     '(#:configure-flags '("-Dextension_set=all")))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     (list glib))
    (synopsis "Extensions for GNOME Shell")
    (description "GNOME Shell extensions modify and extend GNOME Shell
functionality and behavior.")
    (home-page "https://extensions.gnome.org/")
    (license license:gpl3+)))

(define-public folks
  (package
    (name "folks")
    (version "0.15.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/folks/"
                    (version-major+minor version) "/"
                    "folks-" version ".tar.xz"))
              (sha256
               (base32
                "19a4qp9ry8y24jx1v5456qn9lnw843571vkkah3bxx4ky3x3gmr1"))))
    (build-system meson-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") "true")))))))
    (inputs
     (list bdb
           dbus-glib
           evolution-data-server
           glib
           libgee
           readline
           telepathy-glib))
    (native-inputs
     (list `(,glib "bin")
           gobject-introspection
           python-dbusmock
           intltool
           pkg-config
           python
           vala))
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
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/gfbgraph/"
                    (version-major+minor version) "/"
                    "gfbgraph-" version ".tar.xz"))
              (sha256
               (base32
                "0yck7dwvjk16a52nafjpi0a39rxwmg0w833brj45acz76lgkjrb0"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:tests? #f                      ; tests appear to require the network
       #:configure-flags '("--disable-static"
                           "--enable-gtk-doc"
                           "--enable-introspection")))
    (native-inputs
     (list gobject-introspection
           gtk-doc/stable
           pkg-config
           ;; The 0.2.4 ‘release’ tarball isn't bootstrapped.
           autoconf
           automake
           libtool
           which))
    (inputs
     `(("json-glib" ,json-glib)
       ("gnome-online-accounts:lib" ,gnome-online-accounts "lib")
       ("rest" ,rest)))
    (synopsis "GLib/GObject wrapper for the Facebook API")
    (description "This library allows you to use the Facebook API from
GLib/GObject code.")
    (home-page "https://wiki.gnome.org/Projects/GFBGraph")
    (license license:lgpl2.1+)))

(define-public libgnomekbd
  (package
    (name "libgnomekbd")
    (version "3.26.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libgnomekbd/"
                                  (version-major+minor version)  "/"
                                  "libgnomekbd-" version ".tar.xz"))
              (sha256
               (base32
                "0y962ykn3rr9gylj0pwpww7bi20lmhvsw6qvxs5bisbn2mih5jpp"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config
           `(,glib "bin") intltool))
    (propagated-inputs
     ;; Referred to in .h files and .pc.
     (list glib gtk+ libxklavier))
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
       ("gtk-doc" ,gtk-doc/stable)))
    (propagated-inputs
     ;; Referred to in .h files and .pc.
     (list gtk+))
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
    (version "41.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "16fwwfnw1w8p53ffny6zkff5cfsmg7xax9kmfgb7czjqv15w0vd6"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a writable HOME.
             (setenv "HOME" (getcwd)))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")               ;for glib-compile-schemas, gio-2.0.
           `(,gtk+ "bin")               ;for gtk-update-icon-cache
           itstool
           vala
           pkg-config
           python))
    (inputs
     (list `(,glib "bin")
           gtksourceview
           libgee
           libhandy
           libsoup-minimal-2
           libxml2
           mpc
           mpfr))
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
     (list autoconf automake
           `(,gtk+ "bin") intltool pkg-config))
    (inputs
     (list gtksourceview-3 libsm))
    (home-page "https://wiki.gnome.org/Apps/Xpad")
    (synopsis "Virtual sticky note")
    (description
     "Xpad is a sticky note that strives to be simple, fault tolerant,
and customizable.  Xpad consists of independent pad windows, each is
basically a text box in which notes can be written.")
    (license license:gpl3+)))

(define-public gucharmap
  (let ((unicode-files
         '(("Blocks.txt"
            "041sk54v6rjzb23b9x7yjdwzdp2wc7gvfz7ybavgg4gbh51wm8x1")
           ("DerivedAge.txt"
            "04j92xp07v273z3pxkbfmi1svmw9kmnjl9nvz9fv0g5ybk9zk7r6")
           ("NamesList.txt"
            "0vsq8gx7hws8mvxy3nlglpwxw7ky57q0fs09d7w9xgb2ylk7fz61")
           ("Scripts.txt"
            "18c63hx4y5yg408a8d0wx72d2hfnlz4l560y1fsf9lpzifxpqcmx")
           ("UnicodeData.txt"
            "07d1kq190kgl92ispfx6zmdkvwvhjga0ishxsngzlw8j3kdkz4ap")
           ("Unihan.zip"
            "1kfdhgg2gm52x3s07bijb5cxjy0jxwhd097k5lqhvzpznprm6ibf"))))
    (package
      (name "gucharmap")
      (version "12.0.1")
      (source
       (origin
         (method url-fetch)
         (uri (string-append "mirror://gnome/sources/" name "/"
                             (version-major+minor version) "/"
                             name "-" version ".tar.xz"))
         (sha256
          (base32
           "0m915hm2b2d6r3vs1l80rqpssvg78pv8j6nv54yg62kzknnqmpir"))))
      (build-system glib-or-gtk-build-system)
      (arguments
       `(#:modules ((ice-9 match)
                    (guix build glib-or-gtk-build-system)
                    (guix build utils))
         #:configure-flags
         (list "--with-unicode-data=../unicode-data")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'prepare-unicode-data
             (lambda* (#:key inputs #:allow-other-keys)
               (mkdir-p "../unicode-data")
               (with-directory-excursion "../unicode-data"
                 (for-each (match-lambda
                             ((file _)
                              (install-file (assoc-ref inputs file) ".")))
                           ',unicode-files))
               #t)))))
      (native-inputs
       `(("desktop-file-utils" ,desktop-file-utils)
         ("glib:bin" ,glib "bin")       ; for glib-compile-resources.
         ("gobject-introspection" ,gobject-introspection)
         ("intltool" ,intltool)
         ("itstool" ,itstool)
         ("pkg-config" ,pkg-config)
         ,@(map (match-lambda
                  ((file hash)
                   `(,file
                     ,(origin
                        (method url-fetch)
                        (uri (string-append
                              "http://www.unicode.org/Public/12.0.0/ucd/"
                              file))
                        (sha256 (base32 hash))))))
                unicode-files)
         ("unzip" ,unzip)))
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
      (license license:gpl3+))))

(define-public gcolor3
  (package
    (name "gcolor3")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/World/gcolor3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1igqmach1vhcrvbpw346pmkbb5kabbb806ssswrvkp569n700wmc"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gtk+:bin" ,gtk+ "bin")
       ("pkg-config" ,pkg-config)))
    (inputs
     (list gsettings-desktop-schemas gtk+ libportal))
    (home-page "https://www.hjdskes.nl/projects/gcolor3/")
    (synopsis "Simple color chooser written in GTK3")
    (description "Color Picker is a simple color chooser written in GTK3.  It
supports both X and Wayland display servers.")
    (license license:gpl2+)))

(define-public bluefish
  (package
    (name "bluefish")
    (version "2.2.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.bennewitz.com/bluefish/stable/source/"
                           "bluefish-" version ".tar.gz"))
       (sha256
        (base32 "09hgxq139kbkjda5y073lqhq1z1x7cx0j80jh77afrqa3y9c53wl"))))
    (build-system gnu-build-system)
    (native-inputs
     (list desktop-file-utils intltool pkg-config))
    (inputs
     (list enchant gtk+ python-wrapper libxml2 gucharmap))
    (home-page "http://bluefish.openoffice.nl")
    (synopsis "Web development studio")
    (description
     "Bluefish is an editor aimed at programmers and web developers,
with many options to write web sites, scripts and other code.
Bluefish supports many programming and markup languages.")
    (license license:gpl3+)))

(define-public gnome-system-monitor
  (package
    (name "gnome-system-monitor")
    (version "41.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0pwy2c95rm0ym3x5pr6rqg7zh58crjxyns4r52q99ds937349z67"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:configure-flags '("-Dsystemd=false")))
    (native-inputs
     (list `(,glib "bin")               ;for glib-mkenums.
           `(,gtk+ "bin")               ;gtk-update-icon-cache
           intltool
           itstool
           libgtop
           polkit
           pkg-config))
    (inputs
     (list gdk-pixbuf                   ;for loading SVG files.
           gtk+
           gtkmm-3
           libhandy
           librsvg
           libxml2
           libwnck))
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
    (version "2.34.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/pyatspi/"
                    (version-major+minor version)
                    "/pyatspi-" version ".tar.xz"))
              (sha256
               (base32
                "0j3f75j0zd6ca8msg7yr19qsfirqkn9fk8pqbjnlhqrpri455g4p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-atk-load
           (lambda _
             (substitute* "pyatspi/__init__.py"
               (("from gi.repository import Atspi")
                "gi.require_version('Gtk', '3.0')
from gi.repository import Gtk
from gi.repository import Atspi"))
             #t)))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list python))
    (propagated-inputs
     (list python-pygobject))
    (synopsis "Python client bindings for D-Bus AT-SPI")
    (home-page "https://wiki.linuxfoundation.org/accessibility\
/atk/at-spi/at-spi_on_d-bus")
    (description
     "This package includes a python client library for the AT-SPI D-Bus
accessibility infrastructure.")
    (license license:lgpl2.0)
    (properties '((upstream-name . "pyatspi")))))

(define-public python2-pyatspi
  (package/inherit python-pyatspi
    (name "python2-pyatspi")
    (inputs
     `(("python" ,python-2)))
    (propagated-inputs
     `(("python-pygobject" ,python2-pygobject)))))

(define-public orca
  (package
    (name "orca")
    (version "41.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/"
                    (version-major version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1gflnsv6d5qn5rh1f16yfa0q0yv8yvd0l5lbwrsdg7z18lafb5vn"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'qualify-xkbcomp
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xkbcomp (string-append
                             (assoc-ref inputs "xkbcomp") "/bin/xkbcomp")))
               (substitute* "src/orca/orca.py"
                 (("'xkbcomp'") (format #f "'~a'" xkbcomp))))))
         (add-after 'install 'wrap-orca
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (prog (string-append out "/bin/orca")))
               (wrap-program prog
                 `("GI_TYPELIB_PATH" ":" prefix
                   (,(getenv "GI_TYPELIB_PATH")))
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix
                   (,(getenv "GST_PLUGIN_SYSTEM_PATH")))
                 `("GUIX_PYTHONPATH" ":" prefix
                   (,(getenv "GUIX_PYTHONPATH"))))))))))
    (native-inputs
     (list intltool
           itstool
           pkg-config
           libxml2))
    (inputs
     (list at-spi2-atk
           gsettings-desktop-schemas
           gstreamer
           gst-plugins-base
           gst-plugins-good
           gtk+
           python
           python-pygobject
           python-pyatspi
           speech-dispatcher
           xkbcomp))
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
    (version "1.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1miybm1z5cl91i25l7mfqlxhv7j8yy8rcgi0s1bgbb2vm71rb4dv"))
              (patches (search-patches "gspell-dash-test.patch"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:configure-flags (list "--enable-vala")
       #:phases
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
                     (search-input-directory inputs "/lib/aspell")))))))
    (inputs
     (list iso-codes))
    (native-inputs
     `(("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)                             ;for VAPI, needed by Geary
       ("xmllint" ,libxml2)

       ;; For tests.
       ("aspell-dict-en" ,aspell-dict-en)
       ("xorg-server" ,xorg-server-for-tests)))
    (propagated-inputs
     ;; Referred by .pc file.
     (list enchant glib gtk+))
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
     (list libgnomecanvas
           libgnomeui
           libglade
           gnome-vfs
           gconf
           libxml2
           libxslt
           gtk+
           glib))
    (native-inputs
     (list intltool scrollkeeper pkg-config))
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
    (version "1.4.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://adishatz.org/lollypop/"
                           "lollypop-" version ".tar.xz"))
       (sha256
        (base32 "10cw3x75siibmnbh4zhfmf2vd08fqjs3lj3l4wpk6zj9h22ncfxw"))))
    (build-system meson-build-system)
    (arguments
     `(#:imported-modules
       (,@%meson-build-system-modules
        (guix build python-build-system))
       #:modules
       ((guix build meson-build-system)
        ((guix build python-build-system) #:prefix python:)
        (guix build utils))
       #:glib-or-gtk? #t
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH")))
               (wrap-program (string-append out "/bin/lollypop")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
             #t))
         (add-after 'install 'wrap-python
           (assoc-ref python:%standard-phases 'wrap)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")         ; For glib-compile-resources
       ("gtk+:bin" ,gtk+ "bin")         ; For gtk-update-icon-cache
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib-networking" ,glib-networking)
       ("gobject-introspection" ,gobject-introspection)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gst-plugins-base" ,gst-plugins-base)
       ("libnotify" ,libnotify)
       ("libsecret" ,libsecret)
       ("libhandy" ,libhandy)
       ("libsoup" ,libsoup-minimal-2)
       ("python" ,python)
       ("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-gst" ,python-gst)
       ("python-pil" ,python-pillow)
       ("python-pycairo" ,python-pycairo)
       ("python-pygobject" ,python-pygobject)
       ("python-pylast" ,python-pylast)
       ("totem-pl-parser" ,totem-pl-parser)
       ("webkitgtk" ,webkitgtk)))
    (propagated-inputs
     (list ;; gst-plugins-base is required to start Lollypop,
           ;; the others are required to play streaming.
           gst-plugins-good gst-plugins-ugly))
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
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1j6h98whgkcxrh30bwvnxvyqxrxchgpdgqhl0j71xz7x72dqxijd"))))
    (build-system meson-build-system)
    (native-inputs
     (list gettext-minimal pkg-config))
    (home-page "https://wiki.gnome.org/Projects/GnomeVideoEffects")
    (synopsis "Video effects for Cheese and other GNOME applications")
    (description
     "A collection of GStreamer video filters and effects to be used in
photo-booth-like software, such as Cheese.")
    (license license:gpl2+)))

(define-public cheese
  (package
    (name "cheese")
    (version "3.38.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0vyim2avlgq3a48rgdfz5g21kqk11mfb53b2l883340v88mp7ll8"))
              (patches (search-patches "cheese-vala-update.patch"))))
    (arguments
     `(#:glib-or-gtk? #t
       ;; Tests require GDK.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             ;; Don't create 'icon-theme.cache'
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") (which "true")))
             #t))
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Avoid a network connection attempt during the build.
             (substitute* '("docs/reference/cheese.xml"
                            "docs/reference/cheese-docs.xml")
               (("http://www.oasis-open.org/docbook/xml/4.3/docbookx.dtd")
                (string-append (assoc-ref inputs "docbook-xml")
                               "/xml/dtd/docbook/docbookx.dtd")))
             #t))
         (add-after 'install 'wrap-cheese
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out             (assoc-ref outputs "out"))
                   (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH")))
               (wrap-program (string-append out "/bin/cheese")
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))))
             #t)))))
    (build-system meson-build-system)
    (native-inputs
     `(("docbook-xsl" ,docbook-xsl)
       ("docbook-xml" ,docbook-xml-4.3)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gtk-doc" ,gtk-doc/stable)
       ("itstool" ,itstool)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (propagated-inputs
     (list gnome-video-effects
           clutter
           clutter-gst
           clutter-gtk
           libcanberra
           gdk-pixbuf
           glib
           gstreamer))
    (inputs
     (list gnome-desktop
           gobject-introspection
           gst-plugins-base
           gst-plugins-good
           gst-plugins-bad
           gtk+
           libx11
           libxtst))
    (home-page "https://wiki.gnome.org/Apps/Cheese")
    (synopsis "Webcam photo booth software for GNOME")
    (description
     "Cheese uses your webcam to take photos and videos.  Cheese can also
apply fancy special effects and lets you share the fun with others.")
    (license license:gpl2+)))

(define-public passwordsafe
  (package
    (name "passwordsafe")
    (version "5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/World/secrets")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kd43j6i017wdsqj6d5gnxhfv4ijnh3xahlw7md2gh51i8p682j6"))))
    (build-system meson-build-system)
    (arguments
     (list #:glib-or-gtk? #t
           #:meson meson-0.59
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((prog (string-append #$output
                                              "/bin/gnome-passwordsafe"))
                         (pylib (string-append
                                 #$output "/lib/python"
                                 #$(version-major+minor
                                    (package-version (this-package-input "python")))
                                 "/site-packages")))
                     (wrap-program prog
                       `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH") ,pylib))
                       `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))))))))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           `(,gtk+ "bin")
           pkg-config))
    (inputs
     (list glib
           gsettings-desktop-schemas
           gtk+
           libhandy
           libpwquality
           python
           python-pygobject
           python-pykeepass))
    (home-page "https://gitlab.gnome.org/World/PasswordSafe")
    (synopsis "Password manager for the GNOME desktop")
    (description
     "Password Safe is a password manager which makes use of the KeePass v4
format.  It integrates perfectly with the GNOME desktop and provides an easy
and uncluttered interface for the management of password databases.")
    (license license:gpl3+)))

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
     (list desktop-file-utils intltool itstool pkg-config libxml2))
    (inputs
     (list gtk+
           gsettings-desktop-schemas
           gstreamer
           gst-plugins-base
           gst-plugins-good
           iso-codes
           brasero
           libcanberra
           libdiscid
           libmusicbrainz
           neon))
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
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://launchpad.net/soundconverter/trunk/"
                           version "/+download/"
                           "soundconverter-" version ".tar.xz"))

       (sha256
        (base32 "1jv8m82hi23ilrgdznlc1jhp2jm8bw1yrw0chh3qw2l0sixvkl11"))))
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
     (list gtk+ python python-pygobject gstreamer gst-plugins-base))
    (home-page "https://soundconverter.org/")
    (synopsis "Convert between audio formats with a graphical interface")
    (description
     "SoundConverter supports converting between many audio formats including
Opus, Ogg Vorbis, FLAC and more.  It supports parallel conversion, and
configurable file renaming.")
    (license license:gpl3)))

(define-public workrave
  (package
    (name "workrave")
    (version "1.10.48")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rcaelers/workrave")
             (commit (string-append "v" (string-map
                                         (match-lambda (#\. #\_) (chr chr))
                                         version)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qcknxylk9mr0xzszsd1rkgh2zpnix20m998dfclkm9x8zh9pvyr"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     ;; The only tests are maintainer tests (in po/), which fail.
     `(#:tests? #f))
    (inputs (list glib
                  gtk+
                  gdk-pixbuf
                  gtkmm-3
                  glibmm
                  libx11
                  libxtst
                  dconf
                  libice
                  libsm
                  libxscrnsaver))
    (native-inputs `(("boost" ,boost)
                     ("pkg-config" ,pkg-config)
                     ("gettext" ,gettext-minimal)
                     ("autoconf" ,autoconf)
                     ("autoconf-archive" , autoconf-archive)
                     ("automake" ,automake)
                     ("libtool" ,libtool)
                     ("intltool" ,intltool)
                     ("gobject-introspection" ,gobject-introspection)
                     ("python3" ,python-3)
                     ("python-jinja2" ,python-jinja2)))
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
    (version "3.18.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/ghex/"
                                  (version-major+minor version) "/"
                                  "ghex-" version ".tar.xz"))
              (sha256
               (base32
                "1h1pjrr9wynclfykizqd78dbi785wjz6b63p31k87kjvzy8w3nf2"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") (which "true")))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib:bin" ,glib "bin") ; for glib-compile-schemas
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
    (version "3.37.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libdazzle/"
                                  (version-major+minor version) "/"
                                  "libdazzle-" version ".tar.xz"))
              (sha256
               (base32
                "03r5cr11dc031qa3694bpgm3lajrhiiln67kvl7vjj4q0scf7w7x"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")
             #t)))))
    (native-inputs
     (list `(,glib "bin") ; glib-compile-resources
           pkg-config
           ;; For tests.
           xorg-server-for-tests))
    (inputs
     (list glib gobject-introspection gtk+ vala))
    (home-page "https://gitlab.gnome.org/GNOME/libdazzle")
    (synopsis "Companion library to GObject and Gtk+")
    (description "The libdazzle library is a companion library to GObject and
Gtk+.  It provides various features that the authors wish were in the
underlying library but cannot for various reasons.  In most cases, they are
wildly out of scope for those libraries.  In other cases, they are not quite
generic enough to work for everyone.")
    (license license:gpl3+)))

(define-public evolution
  (package
    (name "evolution")
    (version "3.42.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/evolution/"
                                  (version-major+minor version) "/"
                                  "evolution-" version ".tar.xz"))
              (sha256
               (base32
                "0igfzapdvgfx2gnpwfkjfkn7l5j186wk88ni39vqas1sl7ijlls6"))))
    (build-system cmake-build-system)
    (arguments
     `(#:imported-modules (,@%cmake-build-system-modules
                           (guix build glib-or-gtk-build-system))
       #:modules ((guix build cmake-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
       #:configure-flags
       (list "-DENABLE_PST_IMPORT=OFF"    ; libpst is not packaged
             "-DENABLE_LIBCRYPTUI=OFF")   ; libcryptui hasn't seen a release
                                          ; in four years and cannot be built.
       #:phases
       (modify-phases %standard-phases
         ;; The build system attempts to install user interface modules to the
         ;; output directory of the "evolution-data-server" package.  This
         ;; change redirects that change.
         (add-after 'unpack 'patch-ui-module-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/modules/alarm-notify/CMakeLists.txt"
               (("\\$\\{edsuimoduledir\\}")
                (string-append (assoc-ref outputs "out")
                               "/lib/evolution-data-server/ui-modules")))))
         (add-after 'install 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'install 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     (list `(,glib "bin") ; glib-mkenums
           pkg-config intltool itstool))
    (inputs
     `(("enchant" ,enchant)
       ("evolution-data-server" ,evolution-data-server) ; must be the same version
       ("gcr" ,gcr)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gnome-autoar" ,gnome-autoar)
       ("gnome-desktop" ,gnome-desktop)
       ("gspell" ,gspell)
       ("highlight" ,highlight)
       ("libcanberra" ,libcanberra)
       ("libgweather" ,libgweather)
       ("libnotify" ,libnotify)
       ("libsoup" ,libsoup)
       ("nss" ,nss)
       ("openldap" ,openldap)
       ("webkitgtk" ,webkitgtk-with-libsoup2) ; because of evolution-data-server
       ("ytnef" ,ytnef)))
    (home-page "https://gitlab.gnome.org/GNOME/evolution")
    (synopsis "Manage your email, contacts and schedule")
    (description "Evolution is a personal information management application
that provides integrated mail, calendaring and address book
functionality.")
    ;; See COPYING for details.
    (license (list license:lgpl2.1 license:lgpl3 ; either one of these
                   license:openldap2.8 ; addressbook/gui/component/openldap-extract.h
                   license:lgpl2.1+))))  ; smime/lib/*

(define-public gthumb
  (package
    (name "gthumb")
    (version "3.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gthumb/"
                                  (version-major+minor version) "/"
                                  "gthumb-" version ".tar.xz"))
              (sha256
               (base32
                "0grqiq6v26z8avl7mj24xy4i9bl1niwpqhqw6rblprl40c1zrvrx"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       ;; Ensure the RUNPATH contains all installed library locations.
       (list (string-append "-Dc_link_args=-Wl,-rpath="
                            (assoc-ref %outputs "out")
                            "/lib/gthumb/extensions")
             (string-append "-Dcpp_link_args=-Wl,-rpath="
                            (assoc-ref %outputs "out")
                            "/lib/gthumb/extensions"))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils) ; for update-desktop-database
       ("glib:bin" ,glib "bin")                   ; for glib-compile-resources
       ("gtk+:bin" ,gtk+ "bin")                   ; for gtk-update-icon-cache
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("clutter" ,clutter)
       ("clutter-gst" ,clutter-gst)
       ("clutter-gtk" ,clutter-gtk)
       ("colord" ,colord)
       ("exiv2" ,exiv2)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gstreamer" ,gstreamer)
       ("gtk" ,gtk+)
       ("libheif" ,libheif)
       ("libjpeg" ,libjpeg-turbo)
       ("libraw" ,libraw)
       ("librsvg" ,librsvg)
       ("libtiff" ,libtiff)
       ("libwebp" ,libwebp)))
    (home-page "https://wiki.gnome.org/Apps/Gthumb")
    (synopsis "GNOME image viewer and browser")
    (description "GThumb is an image viewer, browser, organizer, editor and
advanced image management tool")
    (license license:gpl2+)))

(define-public terminator
  (package
    (name "terminator")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/gnome-terminator/terminator/"
                           "releases/download/v" version "/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32 "0xdgmam7ghnxw6g38a4gjw3kk3rhga8c66lns18k928jlr9fmddw"))))
    (build-system python-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")         ; for glib-compile-resources
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python-psutil" ,python-psutil)
       ("python-pytest-runner" ,python-pytest-runner)
       ("python-pytest" ,python-pytest)))
    (inputs
     `(("cairo" ,cairo)
       ("dbus-glib" ,dbus-glib)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ("python" ,python-wrapper)
       ("python-dbus" ,python-dbus)
       ("python-notify2" ,python-notify2)
       ("python-pycairo" ,python-pycairo)
       ("python-pygobject" ,python-pygobject)
       ("vte" ,vte)))
    (propagated-inputs
     (list python-configobj))
    (arguments
     ;; One test out of 28 fails due to dbus-python and python-notify; skip
     ;; tests.
     `(#:tests? #f
       #:imported-modules ((guix build glib-or-gtk-build-system)
                           ,@%python-build-system-modules)
       #:modules ((guix build python-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'handle-dbus-python
           (lambda _
             ;; python-dbus cannot be found but it's really there.  See
             ;; https://github.com/SpotlightKid/jack-select/issues/2
             (substitute* "setup.py"
               (("'dbus-python',") ""))))
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((prog (string-append (assoc-ref outputs "out")
                                        "/bin/terminator"))
                   (pylib (string-append (assoc-ref outputs "out")
                                         "/lib/python"
                                         ,(version-major+minor
                                           (package-version python))
                                         "/site-packages")))
               (wrap-program prog
                 `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH") ,pylib))
                 `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))))
         (add-after 'wrap-program 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (home-page "https://gnome-terminator.org/")
    (synopsis "Store and run multiple GNOME terminals in one window")
    (description
     "Terminator allows you to run multiple GNOME terminals in a grid and
tabs, and it supports drag and drop re-ordering of terminals.")
    (license license:gpl2)))

(define-public libhandy
  (package
    (name "libhandy")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/GNOME/libhandy")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19gl0xxybraw2iyn6cdi9w7nps825lb2bhav7d2x51wla0lq3kdc"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       '("-Dglade_catalog=enabled"
         ;; XXX: Generating the documentation fails because the
         ;; libhandy.devhelp2 document cannot be created. This seems to be
         ;; caused by a problem during the XSL transformation.
         "-Dgtk_doc=false")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each
              (lambda (file)
                (substitute* file
                  (("http://www.oasis-open.org/docbook/xml/4.3/docbookx.dtd")
                   (search-input-file inputs
                                      "/xml/dtd/docbook/docbookx.dtd"))))
              (find-files "doc" "\\.xml"))))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")
             #t)))))
    (inputs
     (list gtk+ glade3))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection) ; for g-ir-scanner
       ("vala" ,vala)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("docbook-xsl" ,docbook-xsl)
       ("docbook-xml" ,docbook-xml-4.3)
       ("gtk-doc" ,gtk-doc/stable)
       ("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)

       ;; Test suite dependencies.
       ("xorg-server" ,xorg-server-for-tests)
       ("hicolor-icon-theme" ,hicolor-icon-theme)))
    (home-page "https://gitlab.gnome.org/GNOME/libhandy/")
    (synopsis "Library full of GTK+ widgets for mobile phones")
    (description "The aim of the handy library is to help with developing user
interfaces for mobile devices using GTK+.  It provides responsive GTK+ widgets
for usage on small and big screens.")
    (license license:lgpl2.1+)))

(define-public libhandy-0.0
  (package
    (inherit libhandy)
    (version "0.0.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/GNOME/libhandy")
             (commit (string-append "v" version))))
       (file-name (git-file-name "libhandy" version))
       (sha256
        (base32 "1y23k623sjkldfrdiwfarpchg5mg58smcy1pkgnwfwca15wm1ra5"))))
    (arguments
     (substitute-keyword-arguments (package-arguments libhandy)
       ((#:configure-flags flags)
        '(list "-Dglade_catalog=disabled" "-Dgtk_doc=true"))))))

(define-public libgit2-glib
  (package
    (name "libgit2-glib")
    (version "1.0.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0mrb21cgqmbhbxqxwx76yry1ifqj1k4vy6455n6s034m6rlms2j6"))))
    (build-system meson-build-system)
    (native-inputs
     (list `(,glib "bin") ;; For glib-mkenums
           gobject-introspection
           pkg-config
           python-pygobject
           python-wrapper
           vala))
    (inputs
     (list glib libssh2))
    (propagated-inputs
     (list ;; In Requires of libgit2-glib.pc.
           libgit2))
    (synopsis "GLib wrapper around the libgit2 Git access library")
    (description "libgit2-glib is a GLib wrapper library around the libgit2 Git
access library.  It only implements the core plumbing functions, not really the
higher level porcelain stuff.")
    (home-page "https://wiki.gnome.org/Projects/Libgit2-glib")
    (license license:gpl2+)))

(define-public gitg
  (package
    (name "gitg")
    (version "3.32.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0npg4kqpwl992fgjd2cn3fh84aiwpdp9kd8z7rw2xaj2iazsm914"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-post-install-partially
           (lambda _
             (substitute* "meson_post_install.py"
               (("'python'") ; there are no python sources to compile
                (string-append "'" (which "true") "'"))
               (("gtk-update-icon-cache") (which "true")))
             #t))
         (add-after 'unpack 'fix-test-sources
           (lambda _
             (substitute* "tests/libgitg/test-commit.vala"
               (("/bin/bash") (which "bash")))
             #t))
         ;; XXX: Remove upon next version bump
         (add-after 'unpack 'harden
           (lambda _
             ;; See <https://gitlab.gnome.org/GNOME/gitg/-/issues/337>
             (substitute* "libgitg/gitg-date.vala"
               (("(val\|tzs) == null" all val)
                (string-append val " == null || " val " == \"\""))
               (("(val\|tzs) != null" all val)
                (string-append val " != null && " val " != \"\"")))
             ;; See <https://gitlab.gnome.org/GNOME/gitg/-/merge_requests/159>
             (substitute* "gitg/gitg-action-support.vala"
               (("stash_if_needed\\((.*), Gitg.Ref head" all other)
                (string-append "stash_if_needed(" other ", Gitg.Ref? head")))))
         (add-after 'glib-or-gtk-wrap 'wrap-typelib
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((prog (string-append (assoc-ref outputs "out")
                                        "/bin/gitg")))
               (wrap-program prog
                 `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))
               #t))))))
    (inputs
     (list glib
           gsettings-desktop-schemas
           gtk+
           gtkspell3
           gtksourceview-3
           json-glib
           libdazzle
           libgee
           libgit2-glib
           libpeas
           libsecret
           libsoup-minimal-2
           libxml2))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gtk+:bin" ,gtk+ "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (synopsis "Graphical user interface for git")
    (description
     "gitg is a graphical user interface for git.  It aims at being a small,
fast and convenient tool to visualize the history of git repositories.
Besides visualization, gitg also provides several utilities to manage your
repository and commit your work.")
    (home-page "https://wiki.gnome.org/Apps/Gitg")
    (license license:gpl2+)))

(define-public gamin
  (package
    (name "gamin")
    (version "0.1.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "02n1zr9y8q9lyczhcz0nxar1vmf8p2mmbw8kq0v43wg21jr4i6d5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The 'config.sub' is too old to recognise aarch64.
         ,@(if (and=> (%current-target-system) target-aarch64?)
               `((add-after 'unpack 'replace-config.sub
                   (lambda _
                     (delete-file "config.sub")
                     (symlink (which "config.sub") "config.sub"))))
               '())
         (add-after 'unpack 'remove-deprecated-macro
           (lambda _
             (substitute* '("server/gam_node.c"
                            "server/gam_subscription.h"
                            "server/gam_node.h"
                            "server/gam_subscription.c")
               (("G_CONST_RETURN") "const"))
             #t))
         ;; The configure script runs a test program unconditionally,
         ;; without an option to manually set the test result.
         ;; Override this test anyway.
         ,@(if (%current-target-system)
               `((add-after 'bootstrap 'set-have-abstract-sockets
                   (lambda _
                     (define in-abstract-sockets-test? #f)
                     (substitute* "configure"
                       (("^#### Abstract sockets\n$")
                        (set! in-abstract-sockets-test? #t)
                        "#### Abstract sockets\n")
                       (("^have_abstract_sockets=no\n$")
                        (set! in-abstract-sockets-test? #f)
                        ;; ‘Abstract sockets’ appear to be Linux-only.
                        (string-append "have_abstract_sockets="
                                       ,(if (target-linux?)
                                            "yes"
                                            "no")
                                       "\nif false; then\nif false; then :\n"))
                       (("^(.*\n)$" line)
                        (if in-abstract-sockets-test?
                            "" ; delete
                            line))))))
               '()))))
    (inputs
     (list glib))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ,@(if (and=> (%current-target-system) target-aarch64?)
             `(("config" ,config))
             '())))
    (home-page "https://people.gnome.org/~veillard/gamin/")
    (synopsis "File alteration monitor")
    (description
     "Gamin is a file and directory monitoring system defined to be a subset
of the FAM (File Alteration Monitor) system.  This is a service provided by a
library which detects when a file or a directory has been modified.")
    (license license:gpl2+)))

(define-public gnome-mahjongg
  (package
    (name "gnome-mahjongg")
    (version "3.38.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "144ia3zn9rhwa1xbdkvsz6m0dsysl6mxvqw9bnrlh845hmyy9cfj"))))
    (build-system meson-build-system)
    (arguments
     `(#:meson ,meson-0.59
       #:glib-or-gtk? #t))
    (native-inputs
     `(("appstream-glib" ,appstream-glib)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin") ;; For glib-compile-resources
       ("gtk+" ,gtk+ "bin")     ;; For gtk-update-icon-cache
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (propagated-inputs
     (list dconf))
    (inputs
     (list glib gtk+))
    (synopsis "Mahjongg tile-matching game")
    (description "GNOME Mahjongg is a game based on the classic Chinese
tile-matching game Mahjongg.  It features multiple board layouts, tile themes,
and a high score table.")
    (home-page "https://wiki.gnome.org/Apps/Mahjongg")
    (license license:gpl2+)))

(define-public gnome-themes-extra
  (package
    (name "gnome-themes-extra")
    (version "3.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/" name "-"
                           version ".tar.xz"))
       (sha256
        (base32
         "06aqg9asq2vqi9wr29bs4v8z2bf4manhbhfghf4nvw01y2zs0jvw"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       ;; Don't create 'icon-theme.cache'.
       (let* ((coreutils (assoc-ref %build-inputs "coreutils"))
              (true      (string-append coreutils "/bin/true")))
         (list (string-append "GTK_UPDATE_ICON_CACHE=" true)))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("gtk+-2" ,gtk+-2)
       ("librsvg" ,librsvg)
       ("libxml2" ,libxml2)))
    (home-page "https://gitlab.gnome.org/GNOME/gnome-themes-extra")
    (synopsis "GNOME Extra Themes")
    (description "This package provides themes and related elements that don't
really fit in other upstream packages.  It offers legacy support for GTK+ 2
versions of Adwaita, Adwaita-dark and HighContrast themes.  It also provides
index files needed for Adwaita to be used outside of GNOME.")
    (license license:lgpl2.1+)))

(define-public gnote
  (package
    (name "gnote")
    (version "41.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version)  "/"
                           "gnote-" version ".tar.xz"))
       (sha256
        (base32 "0gs2j988rwfrxckb8qxlkyxnvqsv30q32myqish6hssfa51yzc11"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'skip-gtk-update-icon-cache
                    ;; Don't create 'icon-theme.cache'.
                    (lambda _
                      (substitute* "post-install.py"
                        (("gtk-update-icon-cache") "true")))))))
    (native-inputs
     (list desktop-file-utils
           `(,glib "bin")
           gobject-introspection
           intltool
           itstool
           pkg-config
           python))
    (inputs
     (list glibmm
           gsettings-desktop-schemas
           gspell
           gtk+
           gtkmm-3
           libsecret
           `(,util-linux "lib")
           libxml2
           libxslt))
    (synopsis "Note-taking application for the GNOME desktop")
    (description "Gnote is a note-taking application written for the GNOME
desktop environment.")
    (home-page "https://wiki.gnome.org/Apps/Gnote")
    (license license:gpl3+)))

(define-public polari
  (package
    (name "polari")
    (version "41.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/polari/"
                                  (version-major version)
                                  "/polari-" version ".tar.xz"))
              (sha256
               (base32
                "1ss0x8idwar1q9p9535kzqb8idy7k2r48vrbjiyccw03cs0mzc53"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "meson/meson-postinstall.sh"
               (("gtk-update-icon-cache") (which "true")))))
         (add-after 'install 'fix-desktop-file
           ;; Hardcode launcher to be on the safe side
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* (string-append out "/share/applications/"
                                           "org.gnome.Polari.desktop")
                 (("Exec=.*") (string-append "Exec=" out "/bin/polari\n"))))))
         (add-after 'glib-or-gtk-wrap 'wrap-typelib
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((prog (string-append (assoc-ref outputs "out")
                                        "/bin/polari")))
               (wrap-program prog
                 `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))))))))
    (inputs
     (list glib
           gsettings-desktop-schemas
           gspell
           gtk
           gjs
           libsecret
           libsoup-minimal-2
           telepathy-glib
           telepathy-logger))
    (native-inputs
     (list `(,glib "bin")
           gobject-introspection
           intltool
           pkg-config
           yelp-tools))
    (propagated-inputs
     (list telepathy-idle telepathy-mission-control))
    (synopsis "Simple IRC Client")
    (description
     "Polari is a simple Internet Relay Chat (IRC) client that is designed to
integrate seamlessly with the GNOME desktop.")
    (home-page "https://wiki.gnome.org/Apps/Polari")
    (license license:gpl2+)))

(define-public gnome-boxes
  (package
    (name "gnome-boxes")
    (version "41.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/gnome-boxes/"
                           (version-major version) "/"
                           "gnome-boxes-" version ".tar.xz"))
       (sha256
        (base32 "1wzhm8n485cqhbai4qshgrwl05ix881g8gjshilrj6vg8p1li79h"))
       (patches
        (search-patches "gnome-boxes-add-guix-logo.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags (list "-Drdp=false"
                               (string-append "-Dc_link_args=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib/gnome-boxes"))))
    (native-inputs
     (list `(,glib "bin")               ;for glib-compile-resources
           `(,gtk+ "bin")               ;for gtk-update-icon-cache
           desktop-file-utils           ;for update-desktop-database
           itstool
           intltool
           vala
           pkg-config
           python))
    (inputs
     (list libarchive
           glib-networking              ;for TLS support
           gsettings-desktop-schemas
           gtk+
           gtk-vnc
           gtksourceview
           json-glib
           libhandy
           libosinfo
           libsecret
           libsoup-minimal-2
           libusb
           libvirt
           libvirt-glib
           libxml2
           spice-gtk
           sparql-query
           vte
           webkitgtk-with-libsoup2      ;for webkit2gtk-4.0
           tracker
           libgudev))
    (home-page "https://wiki.gnome.org/Apps/Boxes")
    (synopsis "View, access, and manage remote and virtual systems")
    (description "GNOME Boxes is a simple application to view, access, and
manage remote and virtual systems.  Note that this application requires the
@code{libvirt} and @code{virtlog} daemons to run.  Use the command
@command{info '(guix) Virtualization Services'} to learn how to configure
these services on the Guix System.")
    (license (list
              ;; For data/icons/empty-boxes.png.
              license:cc-by2.0
              ;; For all others.
              license:lgpl2.0+))))

(define-public geary
  (package
    (name "geary")
    (version "40.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/GNOME/geary.git")
                    (commit (string-append "gnome-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04hvw86r8sczvjm1z3ls5y5y5h6nyfb648rjkfx05ib00mqq5v1x"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       '("-Dprofile=release")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-failing-tests
                    (lambda _
                      (substitute* "test/test-client.vala"
                        (("client.add_suite\\(new Application.CertificateManagerTest\\(\\).suite\\);")
                         ""))))
                  (add-after 'unpack 'generate-vapis
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; It’s not possible to generate the GMime vapi, because
                      ;; there’s custom metadata that gmime didn’t
                      ;; install. Thus, the vapi should be built and installed
                      ;; with gmime.
                      (define gmime
                        (assoc-ref inputs "gmime"))
                      (copy-file (string-append gmime "/share/vala/vapi/gmime-3.0.vapi")
                                 "bindings/vapi/gmime-3.0.vapi")))
                  (add-after 'unpack 'disable-postinstall-script
                    (lambda _
                      (substitute* "build-aux/post_install.py"
                        (("gtk-update-icon-cache")
                         "true"))))
                  (add-before 'check 'setup-home
                    (lambda _
                      ;; Tests require a writable HOME.
                      (setenv "HOME" (getcwd))))
                  (add-before 'check 'setup-xvfb
                    (lambda _
                      (system "Xvfb :1 &")
                      (setenv "DISPLAY" ":1"))))))
    (inputs
     `(("enchant" ,enchant)
       ("folks" ,folks)
       ("gcr" ,gcr)
       ("glib" ,glib)
       ("gmime" ,gmime)
       ("gnome-online-accounts:lib"
        ,gnome-online-accounts "lib")
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gspell" ,gspell)
       ("gsound" ,gsound)
       ("gtk+" ,gtk+)
       ("iso-codes" ,iso-codes)
       ("json-glib" ,json-glib)
       ("libcanberra" ,libcanberra)
       ("libgee" ,libgee)
       ("libhandy" ,libhandy)
       ("libpeas" ,libpeas)
       ("libsecret" ,libsecret)
       ("libstemmer" ,libstemmer)
       ("libunwind" ,libunwind)
       ("sqlite" ,sqlite)
       ("webkitgtk" ,webkitgtk-with-libsoup2)
       ("ytnef" ,ytnef)))
    (native-inputs
     `(("appstream-glib" ,appstream-glib)
       ("cmake-minimal" ,cmake-minimal)
       ("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("glib:bin" ,glib "bin")
       ("gmime" ,gmime)
       ("gobject-introspection" ,gobject-introspection)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("itstool" ,itstool)
       ("libarchive" ,libarchive)
       ("libxml2" ,libxml2)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xvfb" ,xorg-server-for-tests)))
    (synopsis "GNOME email application built around conversations")
    (description
     "Geary collects related messages together into conversations,
making it easy to find and follow your discussions.  Full-text and keyword
search makes it easy to find the email you are looking for.  Geary's
full-featured composer lets you send rich, styled text with images, links, and
lists, but also send lightweight, easy to read text messages.  Geary
automatically picks up your existing GNOME Online Accounts, and adding more is
easy.  Geary has a clean, fast, modern interface that works like you want it
to.")
    (home-page "https://wiki.gnome.org/Apps/Geary")
    (license (list
              ;; geary
              license:lgpl2.1+
              ;; icons
              license:cc-by3.0
              license:cc-by-sa3.0
              license:public-domain
              ;; snowball
              license:bsd-2))))

(define-public glabels
  (package
    (name "glabels")
    (version "3.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version)  "/"
                           "glabels-" version ".tar.xz"))
       (sha256
        (base32 "0f2rki8i27pkd9r0gz03cdl1g4vnmvp0j49nhxqn275vi8lmgr0q"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list gtk+ librsvg libxml2))
    (arguments
     `(#:configure-flags '("CFLAGS=-fcommon")))
    (home-page "https://glabels.org/")
    (synopsis "Program for creating labels and business cards")
    (description
     "gLabels is a program for creating labels and business cards.  It is
designed to work with various laser/ink-jet peel-off label and business
card sheets that you’ll find at most office supply stores.")
    (license license:gpl3+)))

(define-public gnome-latex
  (package
    (name "gnome-latex")
    (version "3.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version)  "/"
                           "gnome-latex-" version ".tar.xz"))
       (sha256
        (base32 "0xqd49pgi82dygqnxj08i1v22b0vwwhx3zvdinhrx4jny339yam8"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           gtk-doc/stable
           intltool
           itstool
           pkg-config
           vala))
    (inputs
     (list amtk
           dconf
           glib
           gsettings-desktop-schemas
           gspell
           gtk+
           gtksourceview
           libgee
           tepl-5
           uchardet))
    (home-page "https://wiki.gnome.org/Apps/GNOME-LaTeX")
    (synopsis "LaTeX editor for the GNOME desktop")
    (description
     "GNOME LaTeX is a LaTeX editor for the GNOME desktop.  It has features
such as build tools, completion of LaTeX commands, structure navigation,
symbol tables, document templates, project management, spell-checking, menus
and toolbars.")
    (license license:gpl3+)))

(define-public setzer
  (package
    (name "setzer")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cvfosammmm/Setzer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rcx2c07jg1ij81pnvg3px49hfbjmkagn68d3gp79z3gcajbp2av"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((prog (string-append (assoc-ref outputs "out")
                                        "/bin/setzer"))
                   (pylib (string-append (assoc-ref outputs "out")
                                         "/lib/python"
                                         ,(version-major+minor
                                           (package-version python))
                                         "/site-packages")))
               (wrap-program prog
                 `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH") ,pylib))
                 `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))
               #t))))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")))
    (inputs
     `(("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gspell" ,gspell)
       ("gtk+" ,gtk+)
       ("gtksourceview" ,gtksourceview)
       ("pango" ,pango)
       ("poppler" ,poppler)
       ("python-pdfminer" ,python-pdfminer-six)
       ("python-pycairo" ,python-pycairo)
       ("python-pygobject" ,python-pygobject)
       ("python-pyxdg" ,python-pyxdg)
       ("webkitgtk" ,webkitgtk)
       ("xdg-utils" ,xdg-utils)))
    (home-page "https://www.cvfosammmm.org/setzer/")
    (synopsis "LaTeX editor written in Python with GTK+")
    (description
     "Setzer is a simple yet full-featured LaTeX editor written in Python with
GTK+.  It integrates well with the GNOME desktop environment.")
    (license license:gpl3+)))

(define-public apostrophe
  (package
    (name "apostrophe")
    (version "2.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/somas/apostrophe")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06yfiflmj3ip7ppcz41nb3xpgb5ggw5h74w0v87yaqqkq7qh31lp"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:meson ,meson-0.59              ;fails with 0.60
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-meson
           (lambda _
             (substitute* "build-aux/meson_post_install.py"
               (("gtk-update-icon-cache") "true"))))
         (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((prog (string-append (assoc-ref outputs "out")
                                        "/bin/apostrophe"))
                   (pylib (string-append (assoc-ref outputs "out")
                                         "/lib/python"
                                         ,(version-major+minor
                                           (package-version
                                            (this-package-input "python")))
                                         "/site-packages")))
               (wrap-program prog
                 `("PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH") ,pylib))
                 `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))
                 `("PATH" prefix (,(dirname
                                    (search-input-file inputs
                                                       "/bin/pandoc")))))))))))
    (inputs
     (list glib
           gobject-introspection
           gspell
           gtk+
           libhandy
           pandoc
           python
           python-chardet
           python-levenshtein
           python-regex
           python-pycairo
           python-pygobject
           python-pyenchant
           python-pypandoc
           webkitgtk))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           pkg-config
           sassc))
    (home-page "https://gitlab.gnome.org/somas/apostrophe")
    (synopsis "Markdown editor written in Python with GTK+")
    (description "Apostrophe is a GTK+ based distraction-free Markdown editor.
It uses pandoc as back-end for parsing Markdown.")
    (license license:gpl3)))

(define-public libratbag
  (package
    (name "libratbag")
    (version "0.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libratbag/libratbag")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jjf6xc3a37icp5dvbxla3ai9is2ns31m0llbfq1bmb6dk8cd4n0"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (list "-Dsystemd=false"
             "-Dlogind-provider=elogind")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (site (string-append
                           "/lib/python"
                           ,(version-major+minor (package-version python))
                           "/site-packages"))
                    (evdev (string-append
                            (assoc-ref inputs "python-evdev") site))
                    (pygo (string-append
                           (assoc-ref inputs "python-pygobject") site))
                    (python-wrap
                     `("GUIX_PYTHONPATH" = (,evdev ,pygo))))
               (wrap-program (string-append out "/bin/" "ratbagctl")
                 python-wrap)
               #t))))))
    (native-inputs
     (list check pkg-config swig valgrind))
    (inputs
     `(("glib" ,glib)
       ("json-glib" ,json-glib)
       ("libevdev" ,libevdev)
       ("libsystemd" ,elogind)
       ("libunistring" ,libunistring)
       ("python" ,python)
       ("python-evdev" ,python-evdev)
       ("python-pygobject" ,python-pygobject)
       ("udev" ,eudev)))
    (home-page "https://github.com/libratbag/libratbag")
    (synopsis "DBus daemon and utility for configuring gaming mice")
    (description "libratbag provides @command{ratbagd}, a DBus daemon to
configure input devices, mainly gaming mice.  The daemon provides a generic
way to access the various features exposed by these mice and abstracts away
hardware-specific and kernel-specific quirks.  There is also the
@command{ratbagctl} command line interface for configuring devices.

libratbag currently supports devices from Logitech, Etekcity, GSkill, Roccat,
Steelseries.

The ratbagd DBus service can be enabled by adding the following service to
your operating-system definition:

  (simple-service 'ratbagd dbus-root-service-type (list libratbag))")
    (license license:expat)))

(define-public piper
  (package
    (name "piper")
    (version "0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libratbag/piper")
             (commit version)))
       (sha256
        (base32 "02x4d4n0078slj2pl0rvgayrrxvna6y6vj8fxfamvazsh5xyfzwk"))
       (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           pkg-config
           python-flake8))
    (inputs
     (list adwaita-icon-theme
           gtk+
           `(,gtk+ "bin")
           libratbag
           python
           python-evdev
           python-lxml
           python-pycairo
           python-pygobject))
    (arguments
     `(#:imported-modules ((guix build python-build-system)
                           ,@%meson-build-system-modules)
       #:modules (((guix build python-build-system) #:prefix python:)
                  (guix build meson-build-system)
                  (guix build utils))
       #:tests? #f ;; The flake8 test fails trying to validate piper.in as code.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'dont-update-gtk-icon-cache
           (lambda _
             (substitute* "meson.build"
               (("meson.add_install_script('meson_install.sh')") ""))))
         ;; TODO: Switch to wrap-script when it is fixed.
         (add-after 'install 'wrap-python
           (assoc-ref python:%standard-phases 'wrap))
         (add-after 'wrap-python 'wrap
           (lambda* (#:key outputs #:allow-other-keys)
             (wrap-program
                 (string-append (assoc-ref outputs "out" )"/bin/piper")
               `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))))))
    (home-page "https://github.com/libratbag/piper/")
    (synopsis "Configure bindings and LEDs on gaming mice")
    (description "Piper is a GTK+ application for configuring gaming mice with
onboard configuration for key bindings via libratbag.  Piper requires
a @command{ratbagd} daemon running with root privileges.  It can be run
manually as root, but is preferably configured as a DBus service that can
launch on demand.  This can be configured by enabling the following service,
provided there is a DBus service present:

  (simple-service 'ratbagd dbus-root-service-type (list libratbag))")
    (license license:gpl2)))

(define-public parlatype
  (package
    (name "parlatype")
    (version "3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gkarsay/parlatype")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cqrzbkyksfsm57riirmjkwf2nf2dgl1xpps1wvqxpij475qcb9b"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:tests? #f                      ;require internet access
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "data/meson_post_install.py"
               (("gtk-update-icon-cache") "true"))))
         (add-after 'install 'wrap-parlatype
           ;; Add gstreamer plugin provided in this package to system's
           ;; plugins.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (gst-plugin-path (string-append
                                      out "/lib/gstreamer-1.0/"
                                      ":"
                                      (getenv "GST_PLUGIN_SYSTEM_PATH"))))
               (wrap-program (string-append out "/bin/parlatype")
                 `("GST_PLUGIN_SYSTEM_PATH" ":" = (,gst-plugin-path)))))))))
    (native-inputs
     (list appstream-glib
           desktop-file-utils           ;for desktop-file-validate
           gettext-minimal
           `(,glib "bin")               ;for glib-compile-resources
           pkg-config
           yelp-tools))
    (inputs
     (list gst-plugins-base
           gst-plugins-good
           gstreamer
           gtk+
           iso-codes
           pocketsphinx
           pulseaudio
           sphinxbase))
    (home-page "https://www.parlatype.org")
    (synopsis "GNOME audio player for transcription")
    (description "Parlatype is an audio player for the GNOME desktop
environment.  Its main purpose is the manual transcription of spoken
audio files.")
    (license license:gpl3+)))

(define-public jsonrpc-glib
  (package
    (name "jsonrpc-glib")
    (version "3.34.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                   name "-" version ".tar.xz"))
              (sha256
               (base32
                "0j05x4xv2cp3cbmp30m68z8g4rdw7b030ip4wszyfj9ya15v5kni"))))
    (build-system meson-build-system)
    (inputs
     (list json-glib glib))
    (native-inputs
     (list pkg-config
           `(,glib "bin") ; for glib-genmarshal, etc.
           gobject-introspection vala))
    (home-page "https://gitlab.gnome.org/GNOME/jsonrpc-glib")
    (synopsis "JSON-RPC library for GLib")
    (description "Jsonrpc-GLib is a library to communicate with JSON-RPC based
peers in either a synchronous or asynchronous fashion.  It also allows
communicating using the GVariant serialization format instead of JSON when
both peers support it.  You might want that when communicating on a single
host to avoid parser overhead and memory-allocator fragmentation.")
    (license license:lgpl2.1+)))

(define-public feedbackd
  (package
    (name "feedbackd")
    (version "0.0.0+git20200527")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://source.puri.sm/Librem5/feedbackd.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wbkzxnqjydfgjvp7vz4ghczcz740zcb1yn90cb6gb5md4n6qx2y"))))
    (build-system meson-build-system)
    (native-inputs
     (list `(,glib "bin") gobject-introspection pkg-config vala))
    (inputs
     (list dbus gsound json-glib libgudev))
    (propagated-inputs
     (list glib)) ; in Requires of libfeedback-0.0.pc
    (synopsis "Haptic/visual/audio feedback via DBus")
    (description "Feedbackd provides a DBus daemon to act on events to provide
haptic, visual and audio feedback.  It offers the libfeedbackd library and
GObject introspection bindings.")
     (home-page "https://source.puri.sm/Librem5/feedbackd")
     (license (list license:lgpl2.1+   ; libfeedbackd
                    license:lgpl3+)))) ; the rest

(define-public sysprof
  (package
    (name "sysprof")
    (version "3.42.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/sysprof/"
                           (version-major+minor version) "/"
                           "sysprof-" version ".tar.xz"))
       (sha256
        (base32 "0090986ar3lz9m9fy7l5y9ibzzmgsx54cm6gp8ggsxgf0habi5hp"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-Dsystemdunitdir="
                            %output
                            "/share/systemd"))
       #:tests? #f ; 3/4 test-model-filter barfs some dbus nonsense
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-install-script
           (lambda _
             (substitute* "build-aux/meson/post_install.sh"
               (("gtk-update-icon-cache") "true")
               (("update-desktop-database") "true"))
             #t)))))
    (propagated-inputs
     (list polkit))
    (inputs
     (list glib gtk+ json-glib libdazzle polkit))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin") ; for gdbus-codegen, etc.
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    ;; This home page is so woefully out of date as to be essentially useless.
    ;; (home-page "http://www.sysprof.com")
    (home-page "https://wiki.gnome.org/Apps/Sysprof")
    (synopsis "System-wide performance profiler for GNU/Linux")
    (description
     "Sysprof performs detailed, accurate, and fast CPU profiling of an entire
GNU/Linux system including the kernel and all user-space applications.  This
helps find the function(s) in which a program spends most of its time.

It uses the kernel's built-in @code{ptrace} feature and handles shared
libraries.  Applications do not need to be recompiled--or even restarted.")
    (license license:gpl3+)))

(define-public gnome-builder
  (package
    (name "gnome-builder")
    (version "41.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "04p031i999dccbnlbysmr6f93x7dji7b559j6yhdsqbqgxb7ncan"))
              (patches
               (search-patches "gnome-builder-update-libportal.patch"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t ;To wrap binaries and compile schemas
      #:configure-flags
      #~(list "-Dnetwork_tests=false"
              ;; TODO: Enable all plugins...
              ;; Flatpak plugin wants libsoup 2
              "-Dplugin_flatpak=false"
              ;; ... except this one.
              "-Dplugin_update_manager=false")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-meson
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "build-aux/meson/post_install.py"
                (("gtk-update-icon-cache") "true")
                (("update-desktop-database") "true"))
              (substitute* "src/libide/meson.build"
                (("/usr/lib") (string-append (assoc-ref inputs
                                                        "python-pygobject")
                                             "/lib")))))
          (add-after 'configure 'fix-ninja
            (lambda _
              ;; #43296: meson(?) incorrectly assumes we want to link
              ;; this PIE against a static libselinux.
              (substitute* "build.ninja"
                (("libselinux\\.a") "libselinux.so"))))
          (add-before 'check 'pre-check
            (lambda _
              (system "Xvfb :1 &")
              (setenv "DISPLAY" ":1"))))))
    (inputs (list cmark
                  clang
                  devhelp-with-libsoup2
                  glade3
                  gspell
                  gtk+
                  json-glib
                  jsonrpc-glib
                  libdazzle
                  libgit2-glib
                  libpeas
                  libportal
                  libsoup-minimal-2
                  llvm
                  python
                  python-pygobject
                  sysprof
                  template-glib
                  vte
                  webkitgtk-with-libsoup2))
    (propagated-inputs (list gtksourceview)) ; needed for settings
    (native-inputs (list desktop-file-utils  ; for desktop-file-validate
                         `(,glib "bin")
                         gettext-minimal
                         pkg-config
                         python              ; for meson scripts
                         vala
                         xorg-server-for-tests))
    (home-page "https://wiki.gnome.org/Apps/Builder")
    (synopsis "Toolsmith for GNOME-based applications")
    (description
     "Builder aims to be an integrated development environment (IDE) for
writing GNOME-based software.  It features fuzzy search, auto-completion,
a mini code map, documentation browsing, Git integration, an integrated
profiler via Sysprof, debugging support, and more.")
    (license license:gpl3+)))

(define-public komikku
  (package
    (name "komikku")
    (version "0.36.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/valos/Komikku/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0iqis1b248gj1jb1yzw5dcs99bm078hixg9ir0vb961ymwzv2mmc"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-sources
           (lambda _
             (substitute* "komikku/utils.py"
               (("from komikku\\.servers import get_servers_list")
                ;; code following that line should migrate old databases
                ;; but the line itself results in an import error
                "return data_dir_path"))))
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") (which "true")))
             #t))
         (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((prog (string-append (assoc-ref outputs "out")
                                       "/bin/komikku")))
              (wrap-program prog
                `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")))
                `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))
              #t))))))
    (inputs
     (list gtk+
           libhandy
           libnotify
           libsecret
           python
           python-beautifulsoup4
           python-brotli
           python-cloudscraper
           python-dateparser
           python-keyring
           python-lxml
           python-magic
           python-natsort
           python-pillow
           python-pure-protobuf
           python-pycairo
           python-pygobject
           python-requests
           python-unidecode
           webkitgtk-with-libsoup2))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           pkg-config))
    (home-page "https://gitlab.com/valos/Komikku")
    (synopsis "Manga reader for GNOME")
    (description "Komikku is an online/offline manga reader for GNOME,
developed with the aim of being used with the Librem 5 phone.")
    (license license:gpl3+)))

(define-public libgda
  (package
    (name "libgda")
    (version "5.2.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/GNOME/libgda.git/")
             (commit (string-append "LIBGDA_" (string-replace-substring
                                               version "." "_")))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18rg773gq9v3cdywpmrp12c5xyp97ir9yqjinccpi22sksb1kl8a"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove the bundled sqlite, but keep its header because code relies
        ;; on this header variant.
        '(delete-file "libgda/sqlite/sqlite-src/sqlite3.c"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-system-sqlite" "--enable-vala")
       ;; There's a race between check_cnc_lock and check_threaded_cnc
       ;; in tests/multi-threading.
       #:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-glade-install
           (lambda _
             (substitute* "configure.ac"
               (("`\\$PKG_CONFIG --variable=catalogdir gladeui-2\\.0`")
                "${datadir}/glade/catalogs")
               (("`\\$PKG_CONFIG --variable=pixmapdir gladeui-2\\.0`")
                "${datadir}/glade/pixmaps"))
             #t))
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Tests require a running X server.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")
             #t))
         (add-after 'install 'symlink-glade-module
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((shlib "libgda-ui-5.0.so")
                    (out (assoc-ref outputs "out"))
                    (out/lib (string-append out "/lib"))
                    (moduledir (string-append out/lib "/glade/modules")))
               (mkdir-p moduledir)
               (symlink (string-append out/lib "/" shlib)
                        (string-append moduledir "/" shlib))
               #t))))))
    (propagated-inputs
     (list libxml2))           ; required by libgda-5.0.pc
    (inputs
     (list glib
           glade3
           gtk+
           libsecret
           libxslt
           openssl
           sqlite
           vala))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("autoconf-archive" ,autoconf-archive)
       ("automake" ,automake)
       ("glib:bin" ,glib "bin")
       ("gnome-common" ,gnome-common)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc/stable)
       ("intltool" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("which" ,which)
       ("xorg-server" ,xorg-server-for-tests)
       ("yelp-tools" ,yelp-tools)))
    (home-page "https://gitlab.gnome.org/GNOME/libgda")
    (synopsis "Uniform data access")
    (description
     "GNU Data Access (GDA) is an attempt to provide uniform access to
different kinds of data sources (databases, information servers, mail spools,
etc).  It is a complete architecture that provides all you need to access
your data.")
    (license license:lgpl2.1+)))

(define-public gtranslator
  (package
    (name "gtranslator")
    (version "40.0")
        (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0d48nc11z0m91scy21ah56ysxns82zvswx8lglvlkig1vqvblgpc"))))
    (build-system meson-build-system)
    (inputs
     `(("json-glib" ,json-glib)
       ("jsonrpc-glib" ,jsonrpc-glib)
       ("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gspell" ,gspell)
       ("libdazzle" ,libdazzle)
       ("libgda" ,libgda)
       ("libhandy" ,libhandy)
       ("libsoup" ,libsoup-minimal-2)))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     (list gtksourceview)) ; required for source view
    (arguments
     `(#:build-type "release"
       #:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "build-aux/meson/meson_post_install.py"
               (("gtk-update-icon-cache") (which "true")))
             #t)))))
    (home-page "https://wiki.gnome.org/Apps/Gtranslator")
    (synopsis "Translation making program")
    (description
     "gtranslator is a quite comfortable gettext po/po.gz/(g)mo files editor
for the GNOME 3.x platform with many features.  It aims to be a very complete
editing environment for translation issues within the GNU gettext/GNOME desktop
world.")
    (license license:gpl3+)))


(define-public ocrfeeder
  (package
    (name "ocrfeeder")
    (version "0.8.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/ocrfeeder/"
                                  (version-major+minor version) "/"
                                  "ocrfeeder-" version ".tar.xz"))
              (sha256
               (base32
                "12f5gnq92ffnd5zaj04df7jrnsdz1zn4zcgpbf5p9qnd21i2y529"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after
          'install 'wrap-program
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((prog (string-append (assoc-ref outputs "out")
                                       "/bin/" "ocrfeeder"))
                  (pylib (string-append (assoc-ref outputs "out")
                                        "/lib/python"
                                        ,(version-major+minor
                                          (package-version python))
                                        "/site-packages")))
              (wrap-program prog
                `("PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH") ,pylib))
                `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))
              #t))))))
    (native-inputs
     `(("glib:bin" ,glib "bin")                   ; for glib-compile-resources
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")                   ; for gtk-update-icon-cache
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("enchant" ,enchant)
       ("glib" ,glib)
       ("goocanvas" ,goocanvas)
       ("gtk" ,gtk+)
       ("gtkspell3" ,gtkspell3)
       ("libjpeg" ,libjpeg-turbo)
       ("libtiff" ,libtiff)
       ("libraw" ,libraw)
       ("ocrad" ,ocrad)
       ("python" ,python-wrapper)
       ("python-pygobject" ,python-pygobject)
       ("python-odfpy" ,python-odfpy)
       ("python-pillow" ,python-pillow)
       ("python-pyenchant" ,python-pyenchant)
       ("python-reportlab" ,python-reportlab)
       ("python-sane" ,python-sane)
       ("sane-backends" ,sane-backends)
       ("tesseract-ocr" ,tesseract-ocr)))
    (home-page "https://wiki.gnome.org/Apps/OCRFeeder")
    (synopsis "Complete OCR Suite")
    (description "OCRFeeder is a complete Optical Character Recognition and
Document Analysis and Recognition program.")
    (license license:gpl3+)))

(define-public libadwaita
  (let ((commit "8d66b987a19979d9d7b85dacc6bad5ce0c8743fe")
        (revision "1"))
    (package
      (name "libadwaita")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.gnome.org/GNOME/libadwaita.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0i3wav6jsyi4w4i2r1rad769m5y5s9djj4zqb7dfyh0bad24ba3q"))))
      (build-system meson-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'check 'pre-check
             (lambda* (#:key inputs #:allow-other-keys)
               ;; Tests require a running X server.
               (system "Xvfb :1 &")
               (setenv "DISPLAY" ":1"))))))
      (native-inputs
       `(("sassc" ,sassc)
         ("glib:bin" ,glib "bin")
         ("gtk-doc" ,gtk-doc/stable)
         ("pkg-config" ,pkg-config)
         ("vala" ,vala)
         ("xvfb" ,xorg-server-for-tests)))
      (inputs
       (list gtk gobject-introspection libportal))
      (home-page "https://gnome.pages.gitlab.gnome.org/libadwaita/")
      (synopsis "Building blocks for GNOME applications")
      (description
       "@code{libadwaita} offers widgets and objects to build GNOME
applications scaling from desktop workstations to mobile phones.  It is the
successor of @code{libhandy} for GTK4.")
      (license license:lgpl2.1+))))

(define-public gnome-power-manager
  (package
    (name "gnome-power-manager")
    (version "3.32.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0drfn3wcc8l4n07qwv6p0rw2dwcd00hwzda282q62l6sasks2b2g"))))
    (build-system meson-build-system)
    (inputs
     (list upower gtk+ gsettings-desktop-schemas adwaita-icon-theme))
    (native-inputs
     (list desktop-file-utils
           `(,glib "bin") gettext-minimal pkg-config))
    (home-page "https://gitlab.gnome.org/GNOME/gnome-power-manager")
    (synopsis "Power management daemon for the GNOME desktop")
    (description "@code{gnome-power-manager} is a tool for viewing present and
historical battery usage and related statistics.")
    (license license:gpl2)))

(define-public xffm+
  (package
    (name "xffm+")
    (version "0.94")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/xffm/xffm+/xffm+-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0gwbgmjzlgv9ba95cgaigjnc9njzi7qznhvzp0qrnnlq3nbcm1k1"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; No tests exist
       #:configure-flags
       (let ((shared-mime-info (assoc-ref %build-inputs "shared-mime-info"))
             (out (assoc-ref %outputs "out")))
         (list (string-append "-DFREEDESKTOP_GLOBS=" shared-mime-info
                              "/share/mime/globs")
               (string-append "-DFREEDESKTOP_ALIAS=" shared-mime-info
                              "/share/mime/aliases")
               (string-append "-DFREEDESKTOP_ICONS=" shared-mime-info
                              "/share/mime/generic-icons")
               (string-append "-DCMAKE_INSTALL_PREFIX=" out)
               (string-append "-DPREFIX_BIN=" out "/bin")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-installation-destination
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
              ;; This is done so we can override.
              (("^set.CMAKE_INSTALL_PREFIX") "set(QCMAKE_INSTALL_PREFIX")
              ;; This is done so we can override.
              (("`set.PREFIX_BIN") "set(QPREFIX_BIN")))))))
    (native-inputs
     (list cmake pkg-config intltool gnu-gettext))
    (inputs
     (list glib gtk+ libx11 libsm libxv libxaw libxcb libxkbfile
           shared-mime-info))
    (synopsis "File manager")
    (description "This package provides a graphical file manager.")
    (home-page "http://xffm.org/")
    (license license:gpl3+)
    (properties '((upstream-name . "xffm")))))
