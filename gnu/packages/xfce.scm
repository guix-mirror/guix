;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Florian Paul Schmidt <mista.tapas@gmx.net>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Pkill -9 <pkill9@runbox.com>
;;; Copyright © 2019 L  p R n  d n <guix@lprndn.info>
;;; Copyright © 2019 Ingo Ruhnke <grumbel@gmail.com>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Naga Malleswari <nagamalli@riseup.net>
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

(define-module (gnu packages xfce)
  #:use-module ((guix licenses) #:hide (freetype))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu artwork)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages web)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages image)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages wm))

(define-public gtk-xfce-engine
  (package
    (name "gtk-xfce-engine")
    (version "2.10.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0g86ywkx0ghzhhn96k88p67bbzlm1aqckly85izp07w80l1934ja"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs `(("gtk+" ,gtk+-2)))
    (home-page "https://www.xfce.org/")
    (synopsis "GTK+ theme engine for Xfce")
    (description
     "Default GTK+ engine and themes for Xfce Desktop Environment.")
    (license gpl2+)))

(define-public libxfce4util
  (package
    (name "libxfce4util")
    (version "4.15.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/libxfce4util/"
                                  (version-major+minor version)
                                  "/" name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1lq9i30jdci4if2daxdcqni0x5jvpnaflgp19za9sks3gm4jma5v"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("vala" ,vala)))
    (propagated-inputs `(("glib" ,glib))) ; required by libxfce4util-1.0.pc
    (home-page "https://www.xfce.org/")
    (synopsis "Basic utility library for Xfce")
    (description
     "A general-purpose utility library with core application support for the
Xfce Desktop Environment.")
    (license lgpl2.0+)))

(define-public xfconf
  (package
    (name "xfconf")
    (version "4.14.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0n8d55c98ff7wgwv3qa4g369sv4iasgm1w62zq10kq5f56iy14xq"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       ;; Run check after install phase to test dbus activation.
       (modify-phases %standard-phases
         (add-after 'install 'custom-check
           (lambda _
             (setenv "HOME" (getenv "TMPDIR")) ; xfconfd requires a writable HOME
             ;; Run test-suite under a dbus session.
             (setenv "XDG_DATA_DIRS" ; for finding org.xfce.Xfconf.service
                     (string-append %output "/share"))
             ;; For the missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0");
             (invoke "dbus-launch" "make" "check")))
         (delete 'check))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("glib:bin" ,glib "bin") ;; for gdbus-codegen
       ("gobject-introspection" ,gobject-introspection)
       ("vala" ,vala)
       ("dbus" ,dbus)))
    (propagated-inputs
     ;; libxfconf-0.pc refers to all these.
     `(("glib" ,glib)))
    (inputs
     `(("libxfce4util" ,libxfce4util)))
    (home-page "https://www.xfce.org/")
    (synopsis "Configuration storage and query system for Xfce")
    (description
     "Settings daemon for Xfce, implemented as a D-Bus-based configuration
storage system.")
    (license lgpl2.0+)))

(define-public libxfce4ui
  (package
    (name "libxfce4ui")
    (version "4.14.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1npjhznmnckhnylsv3l7p1zvhckhmp9d7vifs8w12kdfmrg0fjf4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--with-vendor-info=GNU Guix")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("gobject-introspection" ,gobject-introspection)))
    (propagated-inputs
     `(("gtk+-3" ,gtk+)    ; required by libxfce4ui-2.pc
       ;; libxfce4kbd-private-2.pc refers to all these.
       ("libxfce4util" ,libxfce4util)
       ("xfconf" ,xfconf)))
    (inputs `(("libsm" ,libsm)
              ("libice" ,libice)
              ;; FIXME: required by libxfce4ui-1.pc, so should be propagated,
              ;; but will lead to a conflict with gtk+.
              ("gtk+-2" ,gtk+-2)
              ("startup-notification" ,startup-notification)))
    (home-page "https://www.xfce.org/")
    (synopsis "Widgets library for Xfce")
    (description
     "Libxfce4ui is the replacement of the old libxfcegui4 library.  It is used
to share commonly used Xfce widgets among the Xfce applications.")
    (license lgpl2.0+)))

(define-public exo
  (package
    (name "exo")
    (version "0.12.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  "exo/" (version-major+minor version) "/"
                                  "exo-" version ".tar.bz2"))
              (sha256
               (base32
                "1b3w4pf9gkcp13h63nf93k95hkw0ij7v5y7wjklqd1qifm8xd3w4"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (propagated-inputs
     ;; exo-2.pc refers to all these.
     `(("gtk+-3" ,gtk+)
       ("libxfce4util" ,libxfce4util)))
    (inputs
     `(;; FIXME Refered to in exo-1.pc but conflict with gtk+-3
       ("gtk+-2" ,gtk+-2)
       ("libxfce4ui" ,libxfce4ui)
       ("perl-uri" ,perl-uri)))
    (home-page "https://www.xfce.org/")
    (synopsis "Extension library for Xfce")
    (description
     "An extension library to Xfce.  While Xfce comes with quite a few libraries
that are targeted at desktop development, libexo is targeted at application
development.")
    ;; Libraries are under LGPLv2+, and programs under GPLv2+.
    (license (list gpl2+ lgpl2.1+))))

(define-public garcon
  (package
    (name "garcon")
    (version "0.6.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  "garcon/" (version-major+minor version) "/"
                                  "garcon-" version ".tar.bz2"))
              (sha256
               (base32
                "0bbngb4bn1m325j7y40gky36kn2nlsvqs6xp0wy76x3s0d9lfpnp"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("glib:bin" ,glib "bin")))
    (inputs
     `(("gtk+-2" ,gtk+-2))); required by garcon-gtk2-1.pc
    (propagated-inputs
     `(("gtk+-3" ,gtk+)                 ; required by garcon-gtk3-1.pc
       ("libxfce4ui" ,libxfce4ui))) ; required by garcon-gtk3-1.pc
    (home-page "https://www.xfce.org/")
    (synopsis "Implementation of the freedesktop.org menu specification")
    (description
     "Garcon is a freedesktop.org compliant menu implementation based on
GLib and GIO.  It was started as a complete rewrite of the former Xfce menu
library called libxfce4menu, which, in contrast to garcon, was lacking menu
merging features essential for loading menus modified with menu editors.")
    (license lgpl2.0+)))

(define-public tumbler
  (package
    (name "tumbler")
    (version "0.2.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  "tumbler/" (version-major+minor version) "/"
                                  "tumbler-" version ".tar.bz2"))
              (sha256
               (base32
                "15iyx4xk6v3vk8272c848y8rj4sq43khlqxndnai0w5mvsivk689"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("glib:bin" ,glib "bin")         ; need glib-genmarshal
       ("dbus-glib" ,dbus-glib)))       ; need dbus-binding-tool
    (propagated-inputs
     `(("glib" ,glib)))                 ; required by tumbler-1.pc
    (inputs
     `(("dbus" ,dbus)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("cairo" ,cairo) ;; Needed for pdf thumbnails (poppler-glibc.pc)
       ("freetype" ,freetype)
       ("libjpeg" ,libjpeg)
       ("libgsf" ,libgsf)
       ("poppler" ,poppler)
       ;; FIXME Provide gstreamer and gstreamer-tag to get video thumbnails
       ;; ("gstreamer" ,gstreamer)
       ))
    (home-page "https://www.xfce.org/")
    (synopsis "D-Bus service for applications to request thumbnails")
    (description
     "Tumbler is a D-Bus service for applications to request thumbnails for
various URI schemes and MIME types.  It is an implementation of the thumbnail
management D-Bus specification.")
    (license gpl2+)))

(define-public xfce4-panel
  (package
    (name "xfce4-panel")
    (version "4.14.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1x3flv86jh9vqah7mr5mmfx2991mc6icsqjygsc3j88lgsyz7y6m"))
              (patches (search-patches "xfce4-panel-plugins.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tzdata-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (string-append "plugins/clock/clock.c")
               (("/usr/share/zoneinfo")
                (string-append (assoc-ref inputs "tzdata") "/share/zoneinfo")))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("glib:bin" ,glib "bin")))
    (propagated-inputs
     `(("gtk+-3" ,gtk+)                 ; required by libxfce4panel-2.0.pc
       ("libxfce4util" ,libxfce4util))) ; required by libxfce4panel-2.0.pc
    (inputs
     `(("tzdata" ,tzdata) ;; For fix-tzdata-path phase only.
       ("exo" ,exo)
       ("gtk+-2" ,gtk+-2)
       ("xfconf" ,xfconf)
       ("garcon" ,garcon)
       ("libwnck" ,libwnck)
       ("libxfce4ui" ,libxfce4ui)))
    (native-search-paths
     (list (search-path-specification
            (variable "X_XFCE4_LIB_DIRS")
            (files '("lib/xfce4")))))
    (home-page "https://www.xfce.org/")
    (synopsis "Xfce desktop panel")
    (description
     "Desktop panel for Xfce, which contains program launchers, window buttons,
applications menu, workspace switcher and more.")
    ;; Libraries are under LGPLv2.1+, and programs under GPLv2+.
    (license (list gpl2+ lgpl2.1+))))

(define-public xfce4-battery-plugin
  (package
    (name "xfce4-battery-plugin")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "18s0s004nidii8cc3ldp5n3jajc18vwn9vhkhmhy3lbbs520mghj"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("intltool" ,intltool)))
    (inputs `(("glib" ,glib)
              ("gtk+" ,gtk+)
              ("libxfce4util" ,libxfce4util)
              ("libxfce4ui" ,libxfce4ui)
              ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-battery-plugin")
    (synopsis "Battery monitor panel plugin for Xfce4")
    (description
     "A battery monitor panel plugin for Xfce4, compatible with APM and ACPI.")
    ;; The main plugin code is covered by gpl2+, but the files containing code
    ;; to read the battery state via ACPI or APM are covered by lgpl2.0+.
    (license (list gpl2+ lgpl2.0+))))

(define-public xfce4-clipman-plugin
  (package
    (name "xfce4-clipman-plugin")
    (version "1.4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-clipman-plugin/" (version-major+minor version) "/"
                                  "xfce4-clipman-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "1819kjn7gs30zhhj2ppfw4zjpcgj9amw1vvppjsavsff1xflrq88"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("exo" ,exo)
       ("libxfce4ui" ,libxfce4ui)
       ("libxtst" ,libxtst)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-clipman-plugin")
    (synopsis "Clipboard manager for Xfce")
    (description
     "Clipman is a clipboard manager for Xfce.  It keeps the clipboard contents
around while it is usually lost when you close an application.  It is able to
handle text and images, and has a feature to execute actions on specific text by
matching them against regular expressions.")
    (license (list gpl2+))))

(define-public xfce4-pulseaudio-plugin
  (package
    (name "xfce4-pulseaudio-plugin")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0851b0vs5xmy3cq899khcghmkqwvh9rnzwavi17msrsq4jyaxs2a"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       ;; For dbus/dbus-glib.h in pulseaudio-config.h
       (modify-phases %standard-phases
         (add-after 'set-paths 'augment-cflags
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "C_INCLUDE_PATH"
                     (string-append (assoc-ref inputs "dbus-glib")
                                    "/include/dbus-1.0" ":"
                                    (assoc-ref inputs "dbus")
                                    "/include/dbus-1.0" ":"
                                    (or (getenv "C_INCLUDE_PATH") "")))
             #t)))))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("dbus-glib" ,dbus-glib)
       ("dbus" ,dbus)))
    (inputs
     `(("exo" ,exo)
       ("libnotify" ,libnotify)
       ("libxfce4ui" ,libxfce4ui)
       ("pulseaudio" ,pulseaudio)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page "https://git.xfce.org/panel-plugins/xfce4-pulseaudio-plugin/")
    (synopsis "PulseAudio panel plugin for Xfce")
    (description
     "Xfce PulseAudio plugin is a plugin for the Xfce panel which provides a
convenient way to adjust the audio volume of the PulseAudio sound system and
to an auto mixer tool like pavucontrol.  It can optionally handle multimedia
keys for controlling the audio volume.")
    (license gpl2+)))

(define-public xfce4-whiskermenu-plugin
  (package
    (name "xfce4-whiskermenu-plugin")
    (version "2.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                           "xfce4-whiskermenu-plugin/" (version-major+minor version) "/"
                           "xfce4-whiskermenu-plugin-" version ".tar.bz2"))
       (sha256
        (base32 "03jpcbdpkgg825g7mr630wxynachymsrnyhz32mkl0jsd4sxxlw4"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("xfce4-panel" ,xfce4-panel)
       ("garcon" ,garcon)
       ("exo" ,exo)
       ("gtk+" ,gtk+)
       ("libxfce4ui" ,libxfce4ui)))
    (arguments
      `(#:tests? #f)) ; no tests
    (home-page "https://goodies.xfce.org/projects/panel-plugins/xfce4-whiskermenu-plugin")
    (synopsis "Application menu panel plugin for Xfce")
    (description
     "This package provides an alternative to the default application menu
panel plugin for Xfce4.  It uses separate sections to display categories and
applications, and includes a search bar to search for applications.")
    ;; The main plugin code is covered by gpl2, but files in panel-plugin directory
    ;; are covered by gpl2+.  The SVG icon is covered by gpl2.
    (license (list gpl2 gpl2+))))

(define-public xfce4-xkb-plugin
  (package
    (name "xfce4-xkb-plugin")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "18b7cnaf3zxm598p2i47vim3kbbi8w923ia1hwabdph1c89cz7n1"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("garcon" ,garcon)
       ("librsvg" ,librsvg)
       ("libwnck" ,libwnck)
       ("libx11" ,libx11)
       ("libxfce4ui" ,libxfce4ui)
       ("libxklavier" ,libxklavier)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page "https://git.xfce.org/panel-plugins/xfce4-xkb-plugin/")
    (synopsis "XKB layout switching panel plug-in for Xfce")
    (description
     "Xfce XKB plugin makes it possible to set up and use multiple
keyboard layouts.

One can choose the keyboard model, what key combination to
use to switch between the layouts, the actual keyboard layouts,
the way in which the current layout is being displayed (country
flag image or text) and the layout policy, which is whether to
store the layout globally (for all windows), per application or
per window.")
    (license bsd-2)))

(define-public xfce4-appfinder
  (package
    (name "xfce4-appfinder")
    (version "4.14.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://archive.xfce.org/xfce/"
                                  (version-major+minor version)
                                  "/src/" name "-" version ".tar.bz2"))
              (sha256
               (base32
                "162dibl6ipp72x0s35yhk7kkzxd4qimagg5zdkkv5kjgjpa7bhby"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("garcon" ,garcon)
       ("gtk+" ,gtk+)
       ("libxfce4ui" ,libxfce4ui)))
    (home-page "https://www.xfce.org/")
    (synopsis "Xfce application finder")
    (description
     "Application finder for Xfce, it will show the applications installed on
your system in categories, so you can quickly find and launch them.")
    (license gpl2+)))

(define-public xfce4-session
  (package
    (name "xfce4-session")
    (version "4.14.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0gq4a8yiw58hb4d5dhvprxvzamqfg8qblmiqcw0b97mn9svnvyql"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "xfsm-shutdown-helper/main.c"
                    (("/sbin/shutdown -h now")  "halt")
                    (("/sbin/shutdown -r now")  "restart")
                    (("/usr/sbin/pm-suspend")   "pm-suspend")
                    (("/usr/sbin/pm-hibernate") "pm-hibernate"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--with-xsession-prefix=" %output))
       ;; Disable icon cache update.
       #:make-flags
       '("gtk_update_icon_cache=true")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("iceauth" ,iceauth)
       ("upower" ,upower)
       ("polkit" ,polkit)
       ("libsm" ,libsm)
       ("libwnck" ,libwnck)
       ("libxfce4ui" ,libxfce4ui)))
    (home-page "https://www.xfce.org/")
    (synopsis "Xfce session manager")
    (description
     "Session manager for Xfce, it will restore your session on startup and
allows you to shutdown the computer from Xfce.")
    (license gpl2+)))

(define-public xfce4-settings
  (package
    (name "xfce4-settings")
    (version "4.14.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0g0ipkg2fyg8r1z95ynx0xjr78bp49c2dwh4mli05nmb4gb40c70"))
              (patches (search-patches "xfce4-settings-defaults.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-pluggable-dialogs"
                           "--enable-sound-settings"
                           "--enable-xrandr")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("exo" ,exo)
       ("garcon" ,garcon)
       ("libnotify" ,libnotify)
       ("libxcursor" ,libxcursor)
       ("libxi" ,libxi)
       ("libxklavier" ,libxklavier)
       ("libxrandr" ,libxrandr)
       ("libxfce4ui" ,libxfce4ui)
       ("upower" ,upower)
       ("xf86-input-libinput" ,xf86-input-libinput)))
    (home-page "https://www.xfce.org/")
    (synopsis "Xfce settings manager")
    (description
     "Settings manager for Xfce, it can control various aspects of the desktop
like appearance, display, keyboard and mouse settings.")
    (license gpl2+)))

(define-public thunar
  (package
    (name "thunar")
    (version "1.8.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  "thunar/" (version-major+minor version) "/"
                                  "thunar-" version ".tar.bz2"))
              (sha256
               (base32
                "1y1548jrx93c9v8ks60mkd9sx5xl69fmwh1pbr9gsnqz1gp5na99"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("exo" ,exo)
       ("libexif" ,libexif)
       ("libgudev" ,libgudev)
       ("libnotify" ,libnotify)
       ("libxfce4ui" ,libxfce4ui)
       ("pcre" ,pcre)
       ("xfce4-panel" ,xfce4-panel)
       ("startup-notification" ,startup-notification)))
    (home-page "https://www.xfce.org/")
    (synopsis "Xfce file manager")
    (description
     "A modern file manager for graphical desktop, aiming to be easy-to-use and
fast.")
    (license gpl2+)))

(define-public thunar-volman
  (package
    (name "thunar-volman")
    (version "0.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.xfce.org/src/xfce/thunar-volman/"
                           (version-major+minor version) "/"
                           "thunar-volman-" version ".tar.bz2"))
       (sha256
        (base32 "0dqqkbhn43hhmhqyx1fnmawpvysdjzw6ln4ryf629wil6dlwd9vy"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("exo" ,exo)
       ("libgudev" ,libgudev)
       ("libnotify" ,libnotify)
       ("libxfce4ui" ,libxfce4ui)))
    (home-page "https://www.xfce.org/")
    (synopsis "Removable media manager for Thunar")
    (description
     "Thunar-volman is an extension for the Thunar File Manager, which enables
automatic management of removable drives and media.  For example, if
thunar-volman is installed and configured properly, and you plug in your
digital camera, it will automatically spawn your preferred photo application
and import the new pictures from your camera.")
    (license gpl2+)))

(define-public xfwm4
  (package
    (name "xfwm4")
    (version "4.14.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "05dn4a1i0nm6wm3nyj7qli5bvfalxghcl7x543qr5l33vkw2n65l"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("libdrm" ,libdrm)
       ("libwnck" ,libwnck)
       ("libxcomposite" ,libxcomposite)
       ("libxdamage" ,libxdamage)
       ("libxfce4ui" ,libxfce4ui)
       ("libxrandr" ,libxrandr)))
    (home-page "https://www.xfce.org/")
    (synopsis "Xfce window manager")
    (description
     "Window manager for Xfce, it handles the placement of windows
on the screen.")
    (license gpl2+)))

(define-public xfdesktop
  (package
    (name "xfdesktop")
    (version "4.14.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "10pqxgpj7b57wpcsh2k98sj4aavcgxbs1lc8qsq4mibf4hba01gp"))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   (copy-file #$(file-append %artwork-repository "/logo/Guix.svg")
                              "backgrounds/guix-logo.svg")
                   #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'prepare-background-image
                    (lambda _
                      ;; Stick a Guix logo in the background image.  XXX: It
                      ;; has to go to the center because the image might be
                      ;; truncated on the edges.  :-/
                      (invoke "inkscape" "--export-dpi=120"
                              "--export-png=/tmp/guix.png"
                              "backgrounds/guix-logo.svg")
                      (for-each (lambda (image)
                                  (invoke "composite" "-gravity" "center"
                                          "/tmp/guix.png" image
                                          "/tmp/final.jpg")
                                  (copy-file "/tmp/final.jpg" image))
                                '(;; "backgrounds/xfce-blue.jpg"
                                  "backgrounds/xfce-teal.jpg"))
                      #t)))

       #:disallowed-references (,inkscape ,imagemagick)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)

       ("inkscape" ,inkscape)
       ("imagemagick" ,imagemagick)))
    (inputs
     `(("exo" ,exo)
       ("garcon" ,garcon)
       ("libnotify" ,libnotify)
       ("libwnck" ,libwnck)
       ("libxfce4ui" ,libxfce4ui)
       ("thunar" ,thunar)))
    (home-page "https://www.xfce.org/")
    (synopsis "Xfce desktop manager")
    (description
     "Desktop manager for Xfce, it sets the background color or image with
optional application menu or icons for minimized applications or launchers,
devices and folders.")
    (license gpl2+)))

(define-public xfce4-terminal
  (package
    (name "xfce4-terminal")
    (version "0.8.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1zc7hkq77ajia099wxgh4wdvwifcg2zkcz5d2xsf1zm0sdh6mflg"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("vte" ,vte)))
    (home-page "https://www.xfce.org/")
    (synopsis "Xfce terminal emulator")
    (description
     "A lightweight and easy to use terminal emulator for Xfce.  Features
include a simple configuration interface, the ability to use multiple tabs
with terminals within a single window, the possibility to have a
pseudo-transparent terminal background, and a compact mode (where both the
menubar and the window decorations are hidden) that helps you to save space
on your desktop.")
    (license gpl2+)))

(define-public xfce
  (package
    (name "xfce")
    (version (package-version xfce4-session))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder
       (begin
         (use-modules (ice-9 match)
                      (guix build union))
         (match %build-inputs
           (((names . directories) ...)
            (union-build (assoc-ref %outputs "out")
                         directories)
            #t)))))
    (inputs
     `(("exo"                  ,exo)
       ("garcon"               ,garcon)
       ("gnome-icon-theme"     ,gnome-icon-theme)
       ("gtk-xfce-engine"      ,gtk-xfce-engine)
       ("hicolor-icon-theme"   ,hicolor-icon-theme)
       ("ristretto"            ,ristretto)
       ("shared-mime-info"     ,shared-mime-info)
       ("thunar"               ,thunar)
       ("thunar-volman"        ,thunar-volman)
       ("tumlber"              ,tumbler)
       ("xfce4-appfinder"      ,xfce4-appfinder)
       ("xfce4-panel"          ,xfce4-panel)
       ("xfce4-power-manager"  ,xfce4-power-manager)
       ("xfce4-session"        ,xfce4-session)
       ("xfce4-settings"       ,xfce4-settings)
       ("xfce4-terminal"       ,xfce4-terminal)
       ("xfconf"               ,xfconf)
       ("xfdesktop"            ,xfdesktop)
       ("xfwm4"                ,xfwm4)
       ;; Panel plugins.
       ("xfce4-battery-plugin"    ,xfce4-battery-plugin)
       ("xfce4-clipman-plugin"    ,xfce4-clipman-plugin)
       ("xfce4-pulseaudio-plugin" ,xfce4-pulseaudio-plugin)
       ("xfce4-xkb-plugin" ,xfce4-xkb-plugin)))
    (native-search-paths
     ;; For finding panel plugins.
     (package-native-search-paths xfce4-panel))
    (home-page "https://www.xfce.org/")
    (synopsis "Desktop environment (meta-package)")
    (description
     "Xfce is a lightweight desktop environment.  It aims to be fast and low on
system resources, while still being visually appealing and user friendly.")
    (license gpl2+)))

(define-public xfce4-power-manager
  (package
    (name "xfce4-power-manager")
    (version "1.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  "xfce4-power-manager/" (version-major+minor version) "/"
                                  "xfce4-power-manager-" version ".tar.bz2"))
              (sha256
               (base32
                "0jqjwy341dxyijjm9k77a12iih6b5r3f4cmpr2lppa7mf37qqdj5"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("libxrandr" ,libxrandr)
       ("gtk+" ,gtk+)
       ("upower" ,upower)
       ("libnotify" ,libnotify)
       ("libxfce4ui" ,libxfce4ui)))
    (home-page "https://www.xfce.org/")
    (synopsis "Xfce Power Manager")
    (description
     "This is a power manager for the Xfce desktop.  It manages the power
sources on the computer and the devices that can be controlled to reduce their
power consumption (such as LCD brightness level, monitor sleep, CPU frequency
scaling, etc).  In addition, xfce4-power-manager provides a set of
freedesktop-compliant DBus interfaces to inform other applications about current
power level so that they can adjust their power consumption, and it provides the
inhibit interface which allows applications to prevent automatic sleep.")
    (license gpl2+)))

(define-public ristretto
  (package
    (name "ristretto")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/ristretto/"
                                  (version-major+minor version) "/"
                                  "ristretto-" version ".tar.bz2"))
              (sha256
               (base32
                "0sa75m1w6yvv4xvzrwqiif6vnqgi29hjrixrh87nxss58bbms8hn"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("desktop-file-utils" ,desktop-file-utils)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+)
       ("libexif" ,libexif)
       ("libxfce4ui" ,libxfce4ui)
       ("librsvg" ,librsvg)
       ("tumbler" ,tumbler)))
    (home-page "https://docs.xfce.org/apps/ristretto/start")
    (synopsis "Fast and lightweight picture-viewer")
    (description
     "The Ristretto Image Viewer is an application that can be used to view,
and scroll through images.  It can be used to run a slideshow of images, open
images with other applications like an image-editor or configure an image as
the desktop wallpaper.")
    (license gpl2+)))

(define-public xfce4-taskmanager
  (package
    (name "xfce4-taskmanager")
    (version "1.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "04qflazmdrj4ys4r54yg4s5pqcjgk02idrjsls395zd4374636p4"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libwnck" ,libwnck)
       ("libxmu" ,libxmu)
       ("gtk+" ,gtk+)
       ;; FIXME: Remove libxext and libxt when libxmu propagates them.
       ("libxext" ,libxext)
       ("libxt" ,libxt)))
    (home-page "https://goodies.xfce.org/projects/applications/xfce4-taskmanager")
    (synopsis "Easy to use task manager")
    (description
     "This is a task manager for the Xfce desktop.  It displays the CPU and
memory usage graphically, and it can display processes as a tree.")
    (license gpl2+)))

(define-public orage
  (package
    (name "orage")
    (version "4.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0qlhvnl2m33vfxqlbkic2nmfpwyd4mq230jzhs48cg78392amy9w"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build-with-libical3
           (lambda* _
             (substitute* "src/ical-code.c" ;; .is_utc not available in libical3
               ((".*\\.is_utc.*$") ""))
             #t)))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+-2)
       ("libical" ,libical)
       ("libnotify" ,libnotify)
       ("popt" ,popt)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page "https://www.xfce.org/projects/")
    (synopsis "Simple calendar application with reminders")
    (description
     "This is a simple calendar application for the Xfce desktop.  Orage has
alarms and uses the iCalendar format, making it compatible with many other
calendar applications.  It also includes a panel clock plugin and an
international clock application capable of simultaneously showing clocks from
several different time zones.")
    (license gpl2+)))

(define-public xfce4-notifyd
  (package
    (name "xfce4-notifyd")
    (version "0.4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0m8vlbwdxiw9nmimaj5np9l5qm784gxpkdvc881k0hjcz6n72189"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("libnotify" ,libnotify)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page "https://goodies.xfce.org/projects/applications/xfce4-notifyd")
    (synopsis "Show notification bubbles on Xfce")
    (description
     "The Xfce Notify Daemon (xfce4-notifyd for short) is a smallish program
that implements the “server-side” portion of the Freedesktop desktop
notifications specification.  Applications that wish to pop up a notification
bubble in a standard way can implicitly make use of xfce4-notifyd to do so by
sending standard messages over D-Bus using the
@code{org.freedesktop.Notifications} interface.")
    (license gpl2)))

(define-public xfburn
  (package
    (name "xfburn")
    (version "0.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/xfburn/"
                                  (version-major+minor version) "/"
                                  "xfburn-" version ".tar.bz2"))
              (sha256
               (base32
                "09q3s2rkpf0ljzq6bv4hl9byvaggjq7lchfw5zaircwv5q9nwhc3"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("exo" ,exo)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-good" ,gst-plugins-good)
       ("gst-plugins-ugly" ,gst-plugins-ugly)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("libburn" ,libburn)
       ("libisofs" ,libisofs)
       ("libxfce4ui" ,libxfce4ui)))
    (home-page "https://goodies.xfce.org/projects/applications/xfburn")
    (synopsis "GTK+ based CD, DVD and Blu-ray burning application")
    (description
     "Xfburn is a simple CD, DVD, and Blu-ray burning tool based on
the libburnia libraries.  It can blank CD/DVD/BD(-RW)s, burn and
create iso images, audio CDs, as well as burn personal compositions
of data to either CD/DVD/BD.")
    (license gpl2+)))

(define-public mousepad
  (package
    (name "mousepad")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/mousepad/"
                                  (version-major+minor version) "/mousepad-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1myy7954r1a30dk7inwy7kwki7zvfbnnsc3a8swk72vzrbgjmh44"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '(;; Use the GSettings keyfile backend rather than
                           ;; DConf.
                           "--enable-keyfile-settings")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (gtksourceview (assoc-ref inputs "gtksourceview")))
              (wrap-program (string-append out "/bin/mousepad")
                ;; For language-specs.
                `("XDG_DATA_DIRS" ":" prefix (,(string-append gtksourceview
                                                              "/share")))))
             #t)))))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin") ; for glib-compile-schemas.
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+)
       ("gtksourceview" ,gtksourceview-3)
       ("xfconf" ,xfconf)))
    (home-page "https://git.xfce.org/apps/mousepad/")
    (synopsis "Simple text editor for Xfce")
    (description
     "Mousepad is a graphical text editor for Xfce based on Leafpad.")
    (license gpl2+)))

(define-public xfce4-screenshooter
  (package
   (name "xfce4-screenshooter")
   (version "1.9.7")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://archive.xfce.org/src/apps/"
                                "xfce4-screenshooter/"
                                (version-major+minor version)
                                "/xfce4-screenshooter-"
                                version ".tar.bz2"))
            (sha256
             (base32
              "1lbhl0sh0ayv3zhgzcd9hj9q9m3lnyv7vlglfqrl39i3782n2w8g"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("intltool" ,intltool)
      ("glib:bin" ,glib "bin")))        ; glib-genmarshal
   (inputs
    `(("exo" ,exo)
      ("libsoup" ,libsoup)
      ("libxfce4ui" ,libxfce4ui)
      ("xfce4-panel" ,xfce4-panel)))
   (home-page "https://goodies.xfce.org/projects/applications/xfce4-screenshooter")
   (synopsis "Xfce's application to take screenshots")
   (description
    "This application allows you to capture the entire screen, the active
window or a selected region.  You can set the delay that elapses before the screenshot
is taken and the action that will be done with the screenshot.
A plugin for the Xfce panel is also available.")
   (license gpl2+)))

(define-public xfce4-screensaver
  (package
    (name "xfce4-screensaver")
    (version "0.1.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/"
                                  "xfce4-screensaver/"
                                  (version-major+minor version)
                                  "/xfce4-screensaver-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "11p48yyjgy6crwfbyvm16yg0rkzn90ssd2wygzmwvwrx3wkzyhsp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-dbus-1-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dbus-dir (string-append out "/share/dbus-1/services")))
               (substitute* "configure"
                 (("DBUS_SESSION_SERVICE_DIR=.*")
                  (string-append "DBUS_SESSION_SERVICE_DIR="
                                 dbus-dir)))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("glib" ,glib)                             ; glib-compile-schemas
       ("glib:bin" ,glib "bin")))                 ; glib-compile-schemas
    (inputs
     `(("dbus-glib" ,dbus-glib)
       ("libux-pam" ,linux-pam)
       ("elogind" ,elogind)
       ("garcon" ,garcon)
       ("libxklavier" ,libxklavier)
       ("libwnxk" ,libwnck)
       ("libxscrnsaver" ,libxscrnsaver)
       ("xfconf" ,xfconf)))
    (home-page "https://docs.xfce.org/apps/screensaver/start")
    (synopsis "Screensaver for the Xfce desktop")
    (description
     "Xfce Screensaver is a screen saver and locker that aims to have simple,
 sane, secure defaults and be well integrated with the Xfce desktop. ")
    (license gpl2+)))

(define-public xfce4-volumed-pulse
  (package
    (name "xfce4-volumed-pulse")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1q639iwwj7q2plgz0wdgdbi5wkgaq177ca9rnnlrnbdmid5z5fqk"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("xfconf" ,xfconf)
       ("libnotify" ,libnotify)
       ("pulseaudio" ,pulseaudio)
       ("keybinder-3.0" ,keybinder-3.0)
       ("gtk+" ,gtk+)))
    (home-page "https://goodies.xfce.org/projects/applications/xfce4-volumed")
    (synopsis "XFCE volume keys daemon")
    (description
     "This is a volume keys control daemon for Xfce Desktop environment. It controls
 the volume using multimedia keys. It also provides volume change notifications.")
    (license gpl3+)))

(define-public xfce4-cpugraph-plugin
  (package
   (name "xfce4-cpugraph-plugin")
   (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-cpugraph-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-cpugraph-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "193bj1p54l4zrvgdjj0pvjn161d6dn82jh9invcy09sqwlj0mkiy"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-cpugraph-plugin")
    (synopsis "Display CPU load as a graph in the Xfce panel")
    (description "This panel plugin offers multiple display
modes (LED, gradient, fire, etc…) to show the current CPU load of the
system.  Various appearance options, like colors or size, are
customizable.

On multi core or multi CPU systems, CPU Graph can either track and
display all of them at once, or at the user's option only a specific
core or CPU.")
    (license gpl2+)))

(define-public xfce4-eyes-plugin
  (package
   (name "xfce4-eyes-plugin")
   (version "4.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-eyes-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-eyes-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "17gj6fbvvrdzvz61czmia8hqynllsnmhk61fs4aml443cc1h1bpx"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-eyes-plugin")
    (synopsis "Display a pair of eyes for the Xfce panel")
    (description "Eyes is a toy Xfce panel plugin that adds eyes which
watch your every step.")
    (license gpl2+)))

(define-public xfce4-equake-plugin
   (package
   (name "xfce4-equake-plugin")
   (version "1.3.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-equake-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-equake-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "09b9k0n5xm115k44x74w4ad0xqklilyfh0hglsps7zj97pd7a5a3"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+-2" ,gtk+-2)
       ("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-equake-plugin")
    (synopsis "Earthquake monitor for the Xfce panel")
    (description "Equake is a panel plugin for the XFCE desktop
environment.  Equake monitors earthquakes and will display an update
each time a new earthquake occurs.")
    (license gpl2+)))

(define-public xfce4-datetime-plugin
  (package
   (name "xfce4-datetime-plugin")
   (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-datetime-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-datetime-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "1m7bmpvbmiz2xdffpg675qn80zx2n0cnlf842ppvh1q7zz18ndfd"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-datetime-plugin")
    (synopsis "Display date and time inside the Xfce panel")
    (description "This plugin shows the date and time in the panel,
and a calendar appears when you left-click on it.")
    (license gpl2+)))

(define-public xfce4-calculator-plugin
  (package
   (name "xfce4-calculator-plugin")
   (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-calculator-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-calculator-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "1scx7z5ijg2fpcqrzv1nxhpj9vrqic7pyghig70f2n5hgaaanl3v"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-calculator-plugin")
    (synopsis "Calculator for the Xfce panel")
    (description "This plugin is a calculator for the Xfce4 panel.  It
supports common mathematical operators (+, -, *, /, ^) with usual
precedence rules, and the following functions and common constants.")
    (license gpl2+)))

(define-public xfce4-cpufreq-plugin
  (package
   (name "xfce4-cpufreq-plugin")
   (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-cpufreq-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-cpufreq-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "1dgmx3ygil51s1az298ly0gybcw8ln1dz8q8y9k207a0vk049q65"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-cpufreq-plugin")
    (synopsis "Xfce panel plugin for displaying CPU frequency")
    (description "This panel plugin shows information about the CPU
governor and frequencies supported and used by your system.")
    (license gpl2+)))

(define-public xfce4-diskperf-plugin
  (package
   (name "xfce4-diskperf-plugin")
   (version "2.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-diskperf-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-diskperf-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "0i4nrsvwcn15g5gmnba9p07sad3c96x517l2lybdw8jqv91rhbpx"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-diskperf-plugin")
    (synopsis "Display disk performance in the Xfce panel")
    (description "This Xfce panel plugin displays instant disk/partition
performance (bytes transferred per second).")
    (license gpl2+)))

(define-public xfce4-embed-plugin
  (package
   (name "xfce4-embed-plugin")
   (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-embed-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-embed-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "0a72kqsjjh45swimqlpyrahdnplp0383v0i4phr4n6g8c1ixyry7"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)
       ("gtk+-2" ,gtk+-2)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-embed-plugin")
    (synopsis "Embed arbitrary applications inside the Xfce panel")
    (description "This plugin enables the embedding of arbitrary
application windows into the Xfce panel.  The window is resized into
the panel space available, and the associated program can be
automatically launched if it is not open.

Example uses include embedding an instant messaging buddy list, a mail
client's new mail ticker, a simple media application, or a fancy clock
or timer.  Combining with Xfce's ability to auto-hide panels can make
this very convenient.")
    (license gpl2+)))

(define-public xfce4-fsguard-plugin
  (package
   (name "xfce4-fsguard-plugin")
   (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-fsguard-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-fsguard-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "05nmfkrmifm76bsywqmbjd1qdvzagv5cbvnwbkb57156j055vl6n"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-fsguard-plugin")
    (synopsis "Xfce panel plugin to monitor free disk space")
    (description "The panel plugin checks free space on a chosen mount
point frequently and displays a message when a limit is reached.  There
are two limits: a warning limit where only the icon changes, and an
urgent limit that advise the user with a message.  The icon button can
be clicked to open the chosen mount point.")
    (license bsd-2)))

(define-public xfce4-genmon-plugin
  (package
   (name "xfce4-genmon-plugin")
   (version "4.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-genmon-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-genmon-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "1ai3pwgv61nv7i2dyrvncnc63r8kdjbkp40vp51vzak1dx924v15"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-genmon-plugin")
    (synopsis "Generic program output monitor for the Xfce panel")
    (description "This plugin cyclically spawns the indicated
script/program, captures its output (stdout) and displays the
resulting string into the panel.

The string can also contain markup to displayed an image, a bar, a
button and a personalized tooltip.")
    (license gpl2+)))

(define-public xfce4-kbdleds-plugin
  (package
   (name "xfce4-kbdleds-plugin")
   (version "0.0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-kbdleds-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-kbdleds-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "1k810asjjxwix1c7ixl7bqr97zc4j2mw7797gk49rjvv43bhla3d"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+-2" ,gtk+-2)
       ("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-kbdleds-plugin")
    (synopsis "Display keyboard LEDs in the Xfce panel")
    (description "This plugin shows the state of your keyboard LEDs:
Caps, Scroll and Num Lock in Xfce panel.")
    (license gpl2+)))

(define-public xfce4-mailwatch-plugin
  (package
   (name "xfce4-mailwatch-plugin")
   (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-mailwatch-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-mailwatch-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "1bfw3smwivr9mzdyq768biqrl4aq94zqi3xjzq6kqnd8561cqjk2"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+-2" ,gtk+-2)
       ("libxfce4ui" ,libxfce4ui)
       ("exo" ,exo)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-mailwatch-plugin")
    (synopsis "Mail watch plugin for the Xfce panel")
    (description "The Xfce4 Mailwatch Plugin is a multi-protocol,
multi-mailbox mail watcher.  Currently, the protocols supported are:

@itemize
@item IMAP (SSL/TLS and cleartext, CRAM-MD5)
@item POP3 (SSL/TLS and cleartext, CRAM-MD5)
@item Mbox mail spool (local)
@item Maildir mail spool (local)
@item MH-Maildir mail spool (local)
@item Google Mail (GMail) mailbox (remote) (requires gnutls)
@end itemize")
    (license gpl2)))

(define-public xfce4-mpc-plugin
  (package
   (name "xfce4-mpc-plugin")
   (version "0.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-mpc-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-mpc-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "0q3pysdp85b3c7g3b59y3c69g4nw6bvbf518lnri4lxrnsvpizpf"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page "https://goodies.xfce.org/projects/panel-plugins/xfce4-mpc-plugin")
    (synopsis "Music Player Daemon plugin for the Xfce panel")
    (description "This is a simple client plugin for Music Player Daemon.

Features:
@itemize
@item send Play/Stop/Next/Previous command to MPD.
@item uses media icons names from icon-naming-spec (at least nuvola,
tango and rodent themes provides these icons)
@item decrease/increase volume using the mouse wheel.
@item show the current volume, status and title as a tooltip when
hovering the mouse over the plugin.
@item show a simple playlist window upon middle-click, permitting to
select a track to play
@item configurable MPD host/port/password.
@item toggles repeat/random features + enable/disable MPD outputs in
the right-click menu.
@item launch configurable client (gmpc, xterm -e ncmpc,..) through
right-click menu
@item configurable markup for tooltip and playlist, using a gmpc-like markup
@end itemize")
    (license isc)))

(define-public xfce4-mount-plugin
  (package
   (name "xfce4-mount-plugin")
   (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-mount-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-mount-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "07lijjhimjrcyrhasr2299km6wm22xcd3fazdfpqvisbk1mvvrda"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-mount-plugin")
    (synopsis "Mount/unmount plugin for the Xfce panel")
    (description "The plugin will display a list of items representing
your various devices.  If you click on an unmounted devices it will
mount it and vice versa.  There is a warning in case a device can't be
mounted or when unmounting fails.")
    (license gpl2+)))

(define-public xfce4-netload-plugin
  (package
   (name "xfce4-netload-plugin")
   (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-netload-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-netload-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "0p91875n6s8n88l7wb4w9prqly3wvkyilnr7zq0ppq71rwjh9r12"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-netload-plugin")
    (synopsis "Netload plugin for the Xfce Panel")
    (description "This plugin displays the current load of the network
interfaces of your choice in the panel.")
    (license gpl2+)))

(define-public xfce4-places-plugin
  (package
   (name "xfce4-places-plugin")
   (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-places-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-places-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "1chac4ki70axgvkmhw94m0srsv0pwiwqrqbh8di0y9n90fgj24gj"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("desktop-file-utils" ,desktop-file-utils)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+-2" ,gtk+-2)
       ("exo" ,exo)
       ("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-places-plugin")
    (synopsis "Gnome-like Places menu for the Xfce panel")
    (description "This plugin provides a menu with quick access to folders,
documents, and removable media.  The places plugin brings much of the
functionality of GNOME's Places menu to Xfce.

The plugin puts a simple button on the panel.  Clicking on this button
opens up a menu with the following:

@itemize
@item System-defined directories (home folder, trash, desktop, file system)
@item Removable media (using thunar-vfs)
@item User-defined bookmarks (reads @file{~/.gtk-bookmarks})
@item Search program launcher (optional)
@item Recent documents submenu
@end itemize")
    (license gpl2+)))

(define-public xfce4-smartbookmark-plugin
  (package
   (name "xfce4-smartbookmark-plugin")
   (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-smartbookmark-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-smartbookmark-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "001nf2bqly8vm868qvafzgihc9463c488mkim24iw9g2s9ygna1y"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-smartbookmark-plugin")
    (synopsis "Perform custom searches in your browser from the Xfce panel")
    (description "This plugin allows you to send search requests
directly to your browser, such that you can search through your
favorite search engine or bug tracker right from the Xfce panel.")
    (license gpl2+)))

(define-public xfce4-statusnotifier-plugin
  (package
   (name "xfce4-statusnotifier-plugin")
   (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-statusnotifier-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-statusnotifier-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "1yic99jx7013pywpd0k31pxab8s8lv3wcq364iha99qhsm25k42c"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("glib:bin" ,glib "bin")))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("libdbusmenu" ,libdbusmenu)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-statusnotifier-plugin")
    (synopsis "Xfce panel plugin for status notifier items")
(description "This plugin provides a panel area for status
notifier items (application indicators).  Applications may use these
items to display their status and interact with the user.  This
technology is a modern alternative to systray and follows the
freedesktop.org specification.")
    (license gpl2+)))

(define-public xfce4-stopwatch-plugin
  (package
   (name "xfce4-stopwatch-plugin")
   (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-stopwatch-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-stopwatch-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "07dadvgivly44w7qj74i2w60nk01h8567paxrli8vzmhb3p6xi2h"))))
    (build-system gnu-build-system)
    (arguments
     ;; test fails in po/ directory
     `(#:tests? #f))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-stopwatch-plugin")
    (synopsis "Stopwatch plugin for the Xfce panel")
    (description "This Xfce panel plugin keeps track of elapsed time.")
    (license bsd-2)))

(define-public xfce4-systemload-plugin
  (package
   (name "xfce4-systemload-plugin")
   (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-systemload-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-systemload-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "0x87a8h5l3ashz1ksfaxcpn9a392jzlsbx5n5pga8g90fp2hf905"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-systemload-plugin")
    (synopsis "System load display plugin for the Xfce panel")
    (description "A system load plugin for the Xfce4 desktop
environment.  It displays the current CPU load, the memory in use, the
swap space and the system uptime in the Xfce4 panel.")
    (license bsd-2)))

(define-public xfce4-time-out-plugin
  (package
   (name "xfce4-time-out-plugin")
   (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-time-out-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-time-out-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "068a8ck08g4g1msbr7hd5zzp6lsq2a1ry1r472x0rmbna2im2jpf"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+)
       ("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-time-out-plugin")
    (synopsis "Xfce panel plugin that encourages periodical breaks")
    (description "This plugin encourages to take periodical
breaks from the computer every X minutes.  During breaks it locks your
screen.  It optionally allows you to postpone breaks for a certain
time.")
    (license gpl2+)))

(define-public xfce4-timer-plugin
  (package
   (name "xfce4-timer-plugin")
   (version "1.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-timer-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-timer-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "16vypwwjwfk7nn4n16rfgn0z78jqrmbgxmc1r46269lrwd1m6kif"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-timer-plugin")
    (synopsis "Simple countdown and alarm plugin for the Xfce panel")
    (description "This is a simple plugin that lets the user run an
alarm at a specified time or at the end of a specified countdown
period.")
    (license gpl2+)))

(define-public xfce4-verve-plugin
  (package
   (name "xfce4-verve-plugin")
   (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-verve-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-verve-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "1ljcmgc8ixrbz134ggxbbh4zzdnp7mhi9ay6s5hgrz28djx10rcy"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-verve-plugin")
    (synopsis "Command line for the Xfce panel")
    (description "The Verve plugin provides a comfortable command line
for the Xfce panel.  It supports several features, such as:
@itemize
@item Opens URLs, e-mail addresses, directories, and programs
@item Command history
@item Auto-completion (including command history)
@item Focus grabbing via D-BUS (so you can bind a shortcut to it)
@item Custom input field width
@end itemize")
    (license gpl2+)))

(define-public xfce4-wavelan-plugin
  (package
   (name "xfce4-wavelan-plugin")
   (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-wavelan-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-wavelan-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "05zdiq1igr1fcvnwlivg8g3szvxmlr3yc7jfj3bwgqrs0vm827zl"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-wavelan-plugin")
    (synopsis "Show stats from WLAN interface in Xfce panel")
    (description "This plugin is used to display stats from a wireless
lan interface (signal state, signal quality, network name (SSID)).")
    (license bsd-2)))

(define-public xfce4-weather-plugin
  (package
   (name "xfce4-weather-plugin")
   (version "0.10.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-weather-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-weather-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "12bs2rfmmy021087i10vxibdbbvd5vld0vk3h5hymhpz7rgszcmg"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+)
       ("libsoup" ,libsoup)
       ("libxfce4ui" ,libxfce4ui)
       ("libxml2" ,libxml2)
       ("xfce4-panel" ,xfce4-panel)))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-weather-plugin")
    (synopsis "Show information about local weather in the Xfce panel")
    (description "This Xfce panel plugin shows information about your
local weather in the panel, using forecast data provided by the
@uref{https://met.no, Norwegian Meteorological Institute}.")
    (license gpl2+)))
