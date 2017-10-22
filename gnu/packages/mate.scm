;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Fabian Harfert <fhmgufs@web.de>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 ng0 <ng0@infotropique.org>
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

(define-module (gnu packages mate)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public mate-icon-theme
  (package
    (name "mate-icon-theme")
    (version "1.18.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pub.mate-desktop.org/releases/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0si3li3kza7s45zhasjvqn5f85zpkn0x8i4kq1dlnqvjjqzkg4ch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("icon-naming-utils" ,icon-naming-utils)))
    (home-page "https://mate-desktop.org/")
    (synopsis "The MATE desktop environment icon theme")
    (description
     "This package contains the default icon theme used by the MATE desktop.")
    (license license:lgpl3+)))

(define-public mate-themes
  (package
    (name "mate-themes")
    (version "3.22.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pub.mate-desktop.org/releases/themes/"
                                  (version-major+minor version) "/mate-themes-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1p7w63an8qs15hkj79nppy7471glv0rm1b0himn3c4w69q8qdc9i"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("gdk-pixbuf" ,gdk-pixbuf) ; gdk-pixbuf+svg isn't needed
       ("gtk" ,gtk+-2)))
    (home-page "https://mate-desktop.org/")
    (synopsis
     "Official themes for the MATE desktop")
    (description
     "This package includes the standard themes for the MATE desktop, for
example Menta, TraditionalOk, GreenLaguna or BlackMate.  This package has
themes for both gtk+-2 and gtk+-3.")
    (license (list license:lgpl2.1+ license:cc-by-sa3.0 license:gpl3+
                   license:gpl2+))))

(define-public mate-desktop
  (package
    (name "mate-desktop")
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pub.mate-desktop.org/releases/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "12iv2y4dan962fs7vkkxbjkp77pbvjnwfa43ggr0zkdsc3ydjbbg"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("yelp-tools" ,yelp-tools)
       ("gtk-doc" ,gtk-doc)))
    (inputs
     `(("gtk+" ,gtk+)
       ("libxrandr" ,libxrandr)
       ("startup-notification" ,startup-notification)))
    (propagated-inputs
     `(("dconf" ,dconf))) ; mate-desktop-2.0.pc
    (home-page "https://mate-desktop.org/")
    (synopsis "Library with common API for various MATE modules")
    (description
     "This package contains a public API shared by several applications on the
desktop and the mate-about program.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.1+))))

(define-public libmateweather
  (package
    (name "libmateweather")
    (version "1.18.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pub.mate-desktop.org/releases/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0z6vfh42fv9rqjrraqfpf6h9nd9h662bxy3l3r48j19xvxrwmx3a"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--with-zoneinfo-dir="
                            (assoc-ref %build-inputs "tzdata")
                            "/share/zoneinfo"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-tzdata-location
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "data/check-timezones.sh"
              (("/usr/share/zoneinfo/zone.tab")
               (string-append (assoc-ref inputs "tzdata")
                              "/share/zoneinfo/zone.tab")))
            #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("dconf" ,dconf)
       ("glib:bin" ,glib "bin")))
    (inputs
     `(("gtk+" ,gtk+)
       ("tzdata" ,tzdata)))
    (propagated-inputs
      ;; both of these are requires.private in mateweather.pc
     `(("libsoup" ,libsoup)
       ("libxml2" ,libxml2)))
    (home-page "https://mate-desktop.org/")
    (synopsis "MATE library for weather information from the Internet")
    (description
     "This library provides access to weather information from the internet for
the MATE desktop environment.")
    (license license:lgpl2.1+)))

(define-public mate-terminal
  (package
    (name "mate-terminal")
    (version "1.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pub.mate-desktop.org/releases/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1zihm609d2d9cw53ry385whshjl1dnkifpk41g1ddm9f58hv4da1"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("gobject-introspection" ,gobject-introspection)
       ("libxml2" ,libxml2)
       ("yelp-tools" ,yelp-tools)))
    (inputs
     `(("dconf" ,dconf)
       ("gtk+" ,gtk+)
       ("libice" ,libice)
       ("libsm" ,libsm)
       ("libx11" ,libx11)
       ("mate-desktop" ,mate-desktop)
       ("pango" ,pango)
       ("vte" ,vte)))
    (home-page "https://mate-desktop.org/")
    (synopsis "MATE Terminal Emulator")
    (description
     "MATE Terminal is a terminal emulation application that you can
use to access a shell.  With it, you can run any application that
is designed to run on VT102, VT220, and xterm terminals.
MATE Terminal also has the ability to use multiple terminals
in a single window (tabs) and supports management of different
configurations (profiles).")
    (license license:gpl3)))

(define-public mate-session-manager
  (package
    (name "mate-session-manager")
    (version "1.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pub.mate-desktop.org/releases/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0i0xq6041x2qmb26x9bawx0qpfkgjn6x9w3phnm9s7rc4s0z20ll"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("xtrans" ,xtrans)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("gtk+" ,gtk+)
       ("dbus-glib" ,dbus-glib)
       ("libsm" ,libsm)
       ("mate-desktop" ,mate-desktop)))
    (home-page "https://mate-desktop.org/")
    (synopsis "Session manager for MATE")
    (description
     "Mate-session contains the MATE session manager, as well as a
configuration program to choose applications starting on login.")
    (license license:gpl2)))

(define-public mate-settings-daemon
  (package
    (name "mate-settings-daemon")
    (version "1.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pub.mate-desktop.org/releases/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "07b2jkxqv07njdrgkdck93d872p6lch1lrvi7ydnpicspg3rfid6"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("cairo" ,cairo)
       ("dbus" ,dbus)
       ("dbus-glib" ,dbus-glib)
       ("dconf" ,dconf)
       ("fontconfig" ,fontconfig)
       ("gtk+" ,gtk+)
       ("libcanberra" ,libcanberra)
       ("libmatekbd" ,libmatekbd)
       ("libmatemixer" ,libmatemixer)
       ("libnotify" ,libnotify)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxi" ,libxi)
       ("libxklavier" ,libxklavier)
       ("mate-desktop" ,mate-desktop)
       ("nss" ,nss)
       ("polkit" ,polkit)
       ("startup-notification" ,startup-notification)))
    (home-page "https://mate-desktop.org/")
    (synopsis "Settings Daemon for MATE")
    (description
     "Mate-settings-daemon is a fork of gnome-settings-daemon.")
    (license (list license:lgpl2.1 license:gpl2))))

(define-public libmatemixer
  (package
    (name "libmatemixer")
    (version "1.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pub.mate-desktop.org/releases/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "09vyxnlnalws318gsafdfi5c6jwpp92pbafn1ddlqqds23ihk4mr"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("glib" ,glib)
       ("pulseaudio" ,pulseaudio)
       ("alsa-lib" ,alsa-lib)))
    (home-page "https://mate-desktop.org/")
    (synopsis "Mixer library for the MATE desktop")
    (description
     "Libmatemixer is a mixer library for MATE desktop.  It provides an abstract
API allowing access to mixer functionality available in the PulseAudio and ALSA
sound systems.")
    (license license:lgpl2.1)))

(define-public libmatekbd
  (package
    (name "libmatekbd")
    (version "1.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pub.mate-desktop.org/releases/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "030bl18qbjm7l92bp1bhs7v82bp8j3mv7c1j1a4gd89iz4611pq3"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("cairo" ,cairo)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libx11" ,libx11)
       ("libxklavier" ,libxklavier)))
    (home-page "https://mate-desktop.org/")
    (synopsis "MATE keyboard configuration library")
    (description
     "Libmatekbd is a keyboard configuration library for the
MATE desktop environment.")
    (license license:lgpl2.1)))

(define-public mate-menus
  (package
    (name "mate-menus")
    (version "1.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pub.mate-desktop.org/releases/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "05kyr37xqv6hm1rlvnqd5ng0x1n883brqynkirkk5drl56axnz7h"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'fix-introspection-install-dir
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (substitute* '("configure")
                (("`\\$PKG_CONFIG --variable=girdir gobject-introspection-1.0`")
                 (string-append "\"" out "/share/gir-1.0/\""))
                (("\\$\\(\\$PKG_CONFIG --variable=typelibdir gobject-introspection-1.0\\)")
                 (string-append out "/lib/girepository-1.0/")))
              #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("glib" ,glib)
       ("python" ,python-2)))
    (home-page "https://mate-desktop.org/")
    (synopsis "Freedesktop menu specification implementation for MATE")
    (description
     "The package contains an implementation of the freedesktop menu
specification, the MATE menu layout configuration files, .directory files and
assorted menu related utility programs.")
    (license (list license:gpl2+ license:lgpl2.0+))))

(define-public mate-applets
  (package
    (name "mate-applets")
    (version "1.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pub.mate-desktop.org/releases/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1nplr8i1mxbxd7pqhcy8j69v25nsp5dk9fq7ffrmjmp39lrf3fh5"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("libxslt" ,libxslt)
       ("yelp-tools" ,yelp-tools)
       ("scrollkeeper" ,scrollkeeper)
       ("gettext" ,gettext-minimal)
       ("docbook-xml" ,docbook-xml)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("atk" ,atk)
       ("dbus" ,dbus)
       ("dbus-glib" ,dbus-glib)
       ("glib" ,glib)
       ("gucharmap" ,gucharmap)
       ("gtk+" ,gtk+)
       ("gtksourceview" ,gtksourceview)
       ("libgtop" ,libgtop)
       ("libmateweather" ,libmateweather)
       ("libnotify" ,libnotify)
       ("libx11" ,libx11)
       ("libxml2" ,libxml2)
       ("libwnck" ,libwnck)
       ("mate-panel" ,mate-panel)
       ("pango" ,pango)
       ("polkit" ,polkit) ; either polkit or setuid
       ("python" ,python-2)
       ("upower" ,upower)
       ("wireless-tools" ,wireless-tools)))
    (propagated-inputs
     `(("python-pygobject" ,python-pygobject)))
    (home-page "https://mate-desktop.org/")
    (synopsis "Various applets for the MATE Panel")
    (description
     "Mate-applets includes various small applications for Mate-panel:

@enumerate
@item accessx-status: indicates keyboard accessibility settings,
including the current state of the keyboard, if those features are in use.
@item Battstat: monitors the power subsystem on a laptop.
@item Character palette: provides a convenient way to access
non-standard characters, such as accented characters,
mathematical symbols, special symbols, and punctuation marks.
@item MATE CPUFreq Applet: CPU frequency scaling monitor
@item Drivemount: lets you mount and unmount drives and file systems.
@item Geyes: pair of eyes which follow the mouse pointer around the screen.
@item Keyboard layout switcher: lets you assign different keyboard
layouts for different locales.
@item Modem Monitor: monitors the modem.
@item Invest: downloads current stock quotes from the Internet and
displays the quotes in a scrolling display in the applet. The
applet downloads the stock information from Yahoo! Finance.
@item System monitor: CPU, memory, network, swap file and resource.
@item Trash: lets you drag items to the trash folder.
@item Weather report: downloads weather information from the
U.S National Weather Service (NWS) servers, including the
Interactive Weather Information Network (IWIN).
@end enumerate\n")
    (license (list license:gpl2+ license:lgpl2.0+ license:gpl3+))))

(define-public mate-media
  (package
    (name "mate-media")
    (version "1.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pub.mate-desktop.org/releases/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1l0j71d07898wb6ily09sj1xczwrmcw13wyhxwns7sxw592nwi04"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("cairo" ,cairo)
       ("gtk+" ,gtk+)
       ("libcanberra" ,libcanberra)
       ("libmatemixer" ,libmatemixer)
       ("libxml2" ,libxml2)
       ("mate-applets" ,mate-applets)
       ("mate-desktop" ,mate-desktop)
       ("mate-panel" ,mate-panel)
       ("pango" ,pango)
       ("startup-notification" ,startup-notification)))
    (home-page "https://mate-desktop.org/")
    (synopsis "Multimedia related programs for the MATE desktop")
    (description
     "Mate-media includes the MATE media tools for MATE, including
mate-volume-control, a MATE volume control application and applet.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.1+))))

(define-public mate-panel
  (package
    (name "mate-panel")
    (version "1.18.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pub.mate-desktop.org/releases/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1n565ff1n7jrfx223i3cl3m69wjda506nvbn8gra7m1jwdfzpbw1"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-zoneinfo-dir="
                            (assoc-ref %build-inputs "tzdata")
                            "/share/zoneinfo")
             "--with-in-process-applets=all")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-timezone-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((tzdata (assoc-ref inputs "tzdata")))
               (substitute* "applets/clock/system-timezone.h"
                 (("/usr/share/lib/zoneinfo/tab")
                  (string-append tzdata "/share/zoneinfo/zone.tab"))
                 (("/usr/share/zoneinfo")
                  (string-append tzdata "/share/zoneinfo"))))
             #t))
         (add-after 'unpack 'fix-introspection-install-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* '("configure")
                 (("`\\$PKG_CONFIG --variable=girdir gobject-introspection-1.0`")
                  (string-append "\"" out "/share/gir-1.0/\""))
                 (("\\$\\(\\$PKG_CONFIG --variable=typelibdir gobject-introspection-1.0\\)")
                  (string-append out "/lib/girepository-1.0/")))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("xtrans" ,xtrans)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("dconf" ,dconf)
       ("cairo" ,cairo)
       ("dbus-glib" ,dbus-glib)
       ("gtk+" ,gtk+)
       ("libcanberra" ,libcanberra)
       ("libice" ,libice)
       ("libmateweather" ,libmateweather)
       ("librsvg" ,librsvg)
       ("libsm" ,libsm)
       ("libx11" ,libx11)
       ("libxau" ,libxau)
       ("libxml2" ,libxml2)
       ("libxrandr" ,libxrandr)
       ("libwnck" ,libwnck)
       ("mate-desktop" ,mate-desktop)
       ("mate-menus" ,mate-menus)
       ("pango" ,pango)
       ("tzdata" ,tzdata)))
    (home-page "https://mate-desktop.org/")
    (synopsis "Panel for MATE")
    (description
     "Mate-panel contains the MATE panel, the libmate-panel-applet library and
several applets.  The applets supplied here include the Workspace Switcher,
the Window List, the Window Selector, the Notification Area, the Clock and the
infamous 'Wanda the Fish'.")
    (license (list license:gpl2+ license:lgpl2.0+))))

(define-public caja
  (package
    (name "caja")
    (version "1.18.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pub.mate-desktop.org/releases/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0mljqcx7k8p27854zm7qzzn8ca6hs7hva9p43hp4p507z52caqmm"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags '("--disable-update-mimedb")
       #:tests? #f ; tests fail even with display set
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")
             ;; For the missing /etc/machine-id.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("glib:bin" ,glib "bin")
       ("xorg-server" ,xorg-server)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("exempi" ,exempi)
       ("gtk+" ,gtk+)
       ("gvfs" ,gvfs)
       ("libexif" ,libexif)
       ("libnotify" ,libnotify)
       ("libsm" ,libsm)
       ("libxml2" ,libxml2)
       ("mate-desktop" ,mate-desktop)
       ("startup-notification" ,startup-notification)))
    (home-page "https://mate-desktop.org/")
    (synopsis "File manager for the MATE desktop")
    (description
     "Caja is the official file manager for the MATE desktop.
It allows for browsing directories, as well as previewing files and launching
applications associated with them.  Caja is also responsible for handling the
icons on the MATE desktop.  It works on local and remote filesystems.")
    ;; There is a note about a TRADEMARKS_NOTICE file in COPYING which
    ;; does not exist. It is safe to assume that this is of no concern
    ;; for us.
    (license license:gpl2+)))

(define-public mate-control-center
  (package
    (name "mate-control-center")
    (version "1.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pub.mate-desktop.org/releases/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0flnn0h8f5aqyccwrlv7qxchvr3kqmlfdga6wq28d55zkpv5m7dl"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("yelp-tools" ,yelp-tools)
       ("desktop-file-utils" ,desktop-file-utils)
       ("kbproto" ,kbproto)
       ("randrproto" ,randrproto)
       ("renderproto" ,renderproto)
       ("scrnsaverproto" ,scrnsaverproto)
       ("xextpro" ,xextproto)
       ("xproto" ,xproto)
       ("xmodmap" ,xmodmap)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("atk" ,atk)
       ("cairo" ,cairo)
       ("caja" ,caja)
       ("dconf" ,dconf)
       ("dbus" ,dbus)
       ("dbus-glib" ,dbus-glib)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libcanberra" ,libcanberra)
       ("libmatekbd" ,libmatekbd)
       ("libx11" ,libx11)
       ("libxcursor" ,libxcursor)
       ("libxext" ,libxext)
       ("libxi" ,libxi)
       ("libxklavier" ,libxklavier)
       ("libxml2" ,libxml2)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxscrnsaver" ,libxscrnsaver)
       ("marco" ,marco)
       ("mate-desktop" ,mate-desktop)
       ("mate-menus" ,mate-menus)
       ("mate-settings-daemon" ,mate-settings-daemon)
       ("pango" ,pango)
       ("startup-notification" ,startup-notification)))
    (propagated-inputs
     `(("gdk-pixbuf" ,gdk-pixbuf+svg) ; mate-slab.pc
       ("librsvg" ,librsvg))) ; mate-slab.pc
    (home-page "https://mate-desktop.org/")
    (synopsis "MATE Desktop configuration tool")
    (description
     "MATE control center is MATE's main interface for configuration
of various aspects of your desktop.")
    (license license:gpl2+)))

(define-public marco
  (package
    (name "marco")
    (version "1.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pub.mate-desktop.org/releases/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0lwbp9wyd66hl5d7g272l8g3k1pb9s4s2p9fb04750a58w87d8k5"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("glib" ,glib)
       ("gobject-introspection" ,gobject-introspection)
       ("libxft" ,libxft)
       ("libxml2" ,libxml2)
       ("zenity" ,zenity)))
    (inputs
     `(("gtk+" ,gtk+)
       ("libcanberra" ,libcanberra)
       ("libgtop" ,libgtop)
       ("libice" ,libice)
       ("libsm" ,libsm)
       ("libx11" ,libx11)
       ("libxcomposite" ,libxcomposite)
       ("libxcursor" ,libxcursor)
       ("libxdamage" ,libxdamage)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("libxinerama" ,libxinerama)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("mate-desktop" ,mate-desktop)
       ("pango" ,pango)
       ("startup-notification" ,startup-notification)))
    (home-page "https://mate-desktop.org/")
    (synopsis "Window manager for the MATE desktop")
    (description
     "Marco is a minimal X window manager that uses GTK+ for drawing
window frames.  It is aimed at non-technical users and is designed to integrate
well with the MATE desktop.  It lacks some features that may be expected by
some users; these users may want to investigate other available window managers
for use with MATE or as a standalone window manager.")
    (license license:gpl2+)))

(define-public mate
  (package
    (name "mate")
    (version (package-version mate-desktop))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build union))
       #:builder
       (begin
         (use-modules (ice-9 match)
                      (guix build union))
         (match %build-inputs
           (((names . directories) ...)
            (union-build (assoc-ref %outputs "out")
                         directories))))))
    (inputs
     ;; TODO: Add more packages
     `(("at-spi2-core"              ,at-spi2-core)
       ("caja"                      ,caja)
       ("dbus"                      ,dbus)
       ("dconf"                     ,dconf)
       ("desktop-file-utils"        ,desktop-file-utils)
       ("font-cantarell"            ,font-cantarell)
       ("glib-networking"           ,glib-networking)
       ("gnome-keyring"             ,gnome-keyring)
       ("gvfs"                      ,gvfs)
       ("libmatekbd"                ,libmatekbd)
       ("libmateweather"            ,libmateweather)
       ("libmatemixer"              ,libmatemixer)
       ("marco"                     ,marco)
       ("mate-session-manager"      ,mate-session-manager)
       ("mate-settings-daemon"      ,mate-settings-daemon)
       ("mate-desktop"              ,mate-desktop)
       ("mate-terminal"             ,mate-terminal)
       ("mate-themes"               ,mate-themes)
       ("mate-icon-theme"           ,mate-icon-theme)
       ("mate-menu"                 ,mate-menus)
       ("mate-panel"                ,mate-panel)
       ("mate-control-center"       ,mate-control-center)
       ("mate-media"                ,mate-media)
       ("mate-applets"              ,mate-applets)
       ("pinentry-gnome3"           ,pinentry-gnome3)
       ("pulseaudio"                ,pulseaudio)
       ("shared-mime-info"          ,shared-mime-info)
       ("yelp"                      ,yelp)
       ("zenity"                    ,zenity)))
    (synopsis "The MATE desktop environment")
    (home-page "https://mate-desktop.org/")
    (description
     "The MATE Desktop Environment is the continuation of GNOME 2.  It provides
an intuitive and attractive desktop environment using traditional metaphors for
GNU/Linux systems.  MATE is under active development to add support for new
technologies while preserving a traditional desktop experience.")
    (license license:gpl2+)))
