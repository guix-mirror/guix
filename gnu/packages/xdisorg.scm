;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014, 2015, 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2013, 2015, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2015 Alexander I.Grafov <grafov@gmail.com>
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2015 xd1le <elisp.vim@gmail.com>
;;; Copyright © 2015 Florian Paul Schmidt <mista.tapas@gmx.net>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2016, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016, 2017, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016 Petter <petter@mykolab.ch>
;;; Copyright © 2017 Mekeor Melire <mekeor.melire@gmail.com>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Marek Benc <dusxmt@gmx.com>
;;; Copyright © 2017 Mike Gerwitz <mtg@gnu.org>
;;; Copyright © 2018 Thomas Sigurdsen <tonton@riseup.net>
;;; Copyright © 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Nam Nguyen <namn@berkeley.edu>
;;; Copyright © 2019 Wiktor Żelazny <wzelazny@vurv.cz>
;;; Copyright © 2019 Kyle Andrews <kyle.c.andrews@gmail.com>
;;; Copyright © 2019, 2020 Josh Holland <josh@inv.alid.pw>
;;; Copyright © 2019 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 David Wilson <david@daviwil.com>
;;; Copyright © 2020 Ivan Vilata i Balaguer <ivan@selidor.net>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Damien Cassou <damien@cassou.me>
;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Boris A. Dekshteyn <boris.dekshteyn@gmail.com>
;;; Copyright © 2020 Alex McGrath <amk@amk.ie>
;;; Copyright © 2020 Ivan Kozlov <kanichos@yandex.ru>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Gabriel Arazas <foo.dogsquared@gmail.com>
;;; Copyright © 2020 James Smith <jsubuntuxp@disroot.org>
;;; Copyright © 2020 B. Wilson <elaexuotee@wilsonb.com>
;;; Copyright © 2020, 2021 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
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

(define-module (gnu packages xdisorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system scons)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (ice-9 match))

;; packages outside the x.org system proper

(define-public xtitle
  (package
    (name "xtitle")
    (version "0.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/baskerville/xtitle")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f5070k2bwarghl1vq886pl52xck1x5p7x3qhlfchsc2y3dcqms9"))))
    (build-system gnu-build-system)
    (inputs
     `(("libxcb" ,libxcb)
       ("xcb-util" ,xcb-util)
       ("xcb-util-wm" ,xcb-util-wm)))
    (arguments
     `(#:tests? #f                      ;no test suite
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (home-page "https://github.com/baskerville/xtitle")
    (synopsis "Output X window titles")
    (description
     "If arguments are given, @code{xtitle} outputs the title of each
specified window, otherwise it outputs the title of the active window.  With
@emph{snoop} mode on, it continuously monitors the specified windows and
outputs when titles change.")
    (license license:unlicense)))

(define-public arandr
  (package
    (name "arandr")
    (version "0.1.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://christian.amsuess.com/tools/arandr"
                                  "/files/arandr-" version ".tar.gz"))
              (sha256
               (base32
                "135q0llvm077jil2fr92ssw3p095m4r8jfj0lc5rr3m71n4srj6v"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Do not record a timestamp and file name in gzipped man
                  ;; pages (this is equivalent to 'gzip --no-name'.)
                  (substitute* "setup.py"
                    (("gzip\\.open\\(gzfile, 'w', 9\\)")
                     "gzip.GzipFile('', 'wb', 9, open(gzfile, 'wb'), 0.)"))
                  #t))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "screenlayout/xrandr.py"
               (("\"xrandr\"") (string-append "\"" (assoc-ref inputs "xrandr")
                                              "/bin/xrandr\"")))
             #t))
         (add-after 'install 'wrap-gi-typelib
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH")))
               (wrap-program (string-append out "/bin/arandr")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
             #t)))
       #:tests? #f)) ;no tests
    (inputs `(("gtk+" ,gtk+)
              ("pycairo" ,python-pycairo)
              ("pygobject" ,python-pygobject)
              ("xrandr" ,xrandr)))
    (native-inputs `(("gettext"           ,gettext-minimal)
                     ("python-docutils"   ,python-docutils)))
    (home-page "https://christian.amsuess.com/tools/arandr/")
    (synopsis "Another RandR graphical user interface")
    ;; TRANSLATORS: "X11 resize-and-rotate" should not be translated.
    (description "ARandR is designed to provide a simple visual front end for
the X11 resize-and-rotate (RandR) extension.  Relative monitor positions are
shown graphically and can be changed in a drag-and-drop way.  Configurations
are saved as executable shell scripts which can be loaded without using this
program.")
    (license license:gpl3+)))

(define-public autorandr
  (package
    (name "autorandr")
    (version "1.10.1")
    (home-page "https://github.com/phillipberndt/autorandr")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0msw9b1hdy3gbq9w5d04mfizhyirz1c648x84mlcbzl8salm7vpg"))))
    (build-system python-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("xrandr" ,xrandr)
       ("libxcb" ,libxcb)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((xrandr (string-append (assoc-ref inputs "xrandr")
                                          "/bin/xrandr")))
               (substitute* "contrib/etc/xdg/autostart/autorandr.desktop"
                 (("/usr") (assoc-ref outputs "out")))
               (substitute* "autorandr.py"
                 (("popen\\(\"xrandr") (string-append "popen(\"" xrandr))
                 (("\\[\"xrandr") (string-append "[\"" xrandr)))
               (substitute* "contrib/autorandr_launcher/autorandr_launcher.c"
                 (("/usr/bin/autorandr")
                  (string-append (assoc-ref outputs "out") "/bin/autorandr")))
               (setenv "CC" "gcc"))
             #t))
         (add-after 'install 'install-contrib
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "make"
                     (string-append "DESTDIR=" (assoc-ref outputs "out"))
                     "PREFIX="
                     "BASH_COMPLETIONS_DIR=etc/bash_completion.d"
                     "install"
                     "TARGETS=autorandr launcher manpage bash_completion"))))))
    (synopsis "Auto-detect connected displays and load appropriate setup")
    (description "Autorandr wraps around xrandr to help with X11
multi-screen configuration management.  It allows the user to create profiles
for various multi-screen setups.  Autorandr automatically detects the profiles
that can be activated based on the connected hardware.  Hook scripts can be
used to further tweak the behaviour of the different profiles.")
    (license license:gpl3+)))

(define-public bemenu
  (package
    (name "bemenu")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Cloudef/bemenu")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fjcs9d3533ay3nz79cx3c0lmy2chgragr2lhsy0xl2ckr0iins0"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:make-flags (list "CC=gcc"
                          "CFLAGS=-O2 -fPIC"
                          (string-append "LDFLAGS=-Wl,-rpath="
                                         (assoc-ref %outputs "out") "/lib")
                          (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     `(("cairo" ,cairo)
       ("libx11" ,libx11)
       ("libxkbcomon" ,libxkbcommon)
       ("libxinerama" ,libxinerama)
       ("ncurses" ,ncurses)
       ("pango" ,pango)
       ("wayland" ,wayland)
       ("wayland-protocols" ,wayland-protocols)))
    (native-inputs
     `(("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/Cloudef/bemenu")
    (synopsis "Dynamic menu library and client program inspired by dmenu")
    (description
     "bemenu is a dynamic menu which allows the user to flexibly select from a
list of options (usually programs to launch).  It renders the menu graphically
with X11 or Wayland, or in a text terminal with ncurses.")
    (license (list license:gpl3+ ; client program[s] and other sources
                   license:lgpl3+))))   ; library and bindings

(define-public copyq
(package
  (name "copyq")
  (version "3.9.3")
  (source (origin
            (method git-fetch)
            (uri (git-reference
                   (url "https://github.com/hluk/CopyQ")
                   (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0wlwq9xg8rzsbj0b29z358k4mbrqy04iraa8x0p26pa95yskgcma"))))
  (build-system cmake-build-system)
  (arguments
   `(#:configure-flags '("-DCMAKE_BUILD_TYPE=Release")
     #:tests? #f)) ; Test suite is a rather manual process.
  (inputs
   `(("qtbase" ,qtbase)
     ("qtscript" ,qtscript)
     ("qtsvg" ,qtsvg)
     ("qtx11extras" ,qtx11extras)))
  (synopsis "Clipboard manager with advanced features")
  (description "CopyQ is clipboard manager with editing and scripting
features.  CopyQ monitors system clipboard and saves its content in customized
tabs.  Saved clipboard can be later copied and pasted directly into any
application.")
  (home-page "https://hluk.github.io/CopyQ/")
  (license license:gpl3+)))

(define-public xkeysnail
  (package
    (name "xkeysnail")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xkeysnail" version))
       (sha256
        (base32
         "1xyqp6yqxcwmxaqj86qcsiz0ly7bwr0a2w835myz909irhip3ngf"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;tests need /dev/uinput
    (inputs
     `(("python-appdirs" ,python-appdirs)
       ("python-evdev" ,python-evdev)
       ("python-inotify-simple" ,python-inotify-simple)
       ("python-xlib" ,python-xlib)
       ("python-six" ,python-six)))
    (home-page "https://github.com/mooz/xkeysnail")
    (synopsis "Keyboard remapping tool for the X11 environment")
    (description
     "Xkeysnail is an X environment keyboard remapping tool, featuring
high-level and flexible remapping mechanisms.  It affects the low-level
layers (evdev and uinput), making remapping work in almost all the places.")
    (license license:gpl3+)))           ; see README.md (no licence headers)

(define-public xclip
  (package
    (name "xclip")
    (version "0.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/astrand/xclip")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0q0hmvcjlv8arhh1pzhja2wglyj6n7z209jnpnzd281kqqv4czcs"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f))                              ;there is no test suite
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs `(("libxmu" ,libxmu)
              ("libxt" ,libxt)))
    (home-page "https://github.com/astrand/xclip")
    (synopsis "Command line interface to X11 clipboard")
    (description "Xclip is a command line interface to the X11 clipboard.  It
can also be used for copying files, as an alternative to sftp/scp, thus
avoiding password prompts when X11 forwarding has already been setup.")
    (license license:gpl2+)))

(define-public libxkbcommon
  (package
    (name "libxkbcommon")
    (version "1.0.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://xkbcommon.org/download/libxkbcommon-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0lmwglj16anhpaq0h830xsl1ivknv75i4lir9bk88aq73s2jy852"))))
    (build-system meson-build-system)
    (inputs
     `(("libx11" ,libx11)
       ("libxcb" ,libxcb)
       ("libxml2" ,libxml2)
       ("wayland" ,wayland)
       ("wayland-protocols" ,wayland-protocols)
       ("xkeyboard-config" ,xkeyboard-config)))
    (native-inputs
     `(("bison" ,bison)
       ("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       (list (string-append "-Dxkb-config-root="
                            (assoc-ref %build-inputs "xkeyboard-config")
                            "/share/X11/xkb")
             (string-append "-Dx-locale-root="
                            (assoc-ref %build-inputs "libx11")
                            "/share/X11/locale"))))
    (home-page "https://xkbcommon.org/")
    (synopsis "Library to handle keyboard descriptions")
    (description "Xkbcommon is a library to handle keyboard descriptions,
including loading them from disk, parsing them and handling their
state.  It is mainly meant for client toolkits, window systems, and other
system applications; currently that includes Wayland, kmscon, GTK+, Qt,
Clutter, and more.  Despite the name, it is not currently used by anything
X11 (yet).")
    (license (license:x11-style "file://COPYING"
                                "See 'COPYING' in the distribution."))
    (properties '((cpe-name . "xkbcommon")))))

(define-public libfakekey
  (package
    (name "libfakekey")
    (version "0.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://git.yoctoproject.org/git/libfakekey")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1jw1d4wc1ysiijirc7apnz3sryrxbl9akgb92mh06dvfkz2nblj0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "AM_LDFLAGS=-lX11")
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           ;; ./autogen.sh calls ./configure before shebangs have been patched.
           (lambda _
             (invoke "autoreconf" "-vfi"))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)

       ;; For bootstrapping from git.
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (inputs
     `(("libxtst" ,libxtst)
       ("libx11" ,libx11)))
    (home-page "https://www.yoctoproject.org/tools-resources/projects/matchbox")
    (synopsis "X virtual keyboard library")
    (description
     "Libfakekey is a virtual keyboard library for X.")
    (license license:gpl2)))

(define-public xdotool
  (package
    (name "xdotool")
    (version "3.20160805.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "https://github.com/jordansissel/xdotool/releases/download/v"
              version "/xdotool-" version ".tar.gz"))
        (sha256
          (base32
           "1a6c1zr86zb53352yxv104l76l8x21gfl2bgw6h21iphxpv5zgim"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; Test suite requires a lot of black magic
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys #:rest args)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/lib"))
               (setenv "PREFIX" out)
               (setenv "LDFLAGS" (string-append "-Wl,-rpath=" out "/lib"))
               (setenv "CC" "gcc")
               #t))))))
    (native-inputs `(("perl" ,perl))) ; for pod2man
    (inputs `(("libx11" ,libx11)
              ("libxext" ,libxext)
              ("libxi" ,libxi)
              ("libxinerama" ,libxinerama)
              ("libxtst" ,libxtst)
              ("libxkbcommon" ,libxkbcommon)))
    (home-page "https://www.semicomplete.com/projects/xdotool/")
    (synopsis "Fake keyboard/mouse input, window management, and more")
    (description "Xdotool lets you simulate keyboard input and mouse activity,
move and resize windows, etc.  It does this using X11's XTEST extension and
other Xlib functions.  Additionally, you can search for windows and move,
resize, hide, and modify window properties like the title.  If your window
manager supports it, you can use xdotool to switch desktops, move windows
between desktops, and change the number of desktops.")
    (license license:bsd-3)))

(define-public xeyes
  (package
    (name "xeyes")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.x.org/releases/individual/app/"
                           name "-" version ".tar.bz2"))
       (sha256
        (base32 "0lq5j7fryx1wn998jq6h3icz1h6pqrsbs3adskjzjyhn5l6yrg2p"))))
    (build-system gnu-build-system)
    (inputs
      `(("libxext" ,libxext)
        ("libxmu" ,libxmu)
        ("libxrender" ,libxrender)
        ("libxt" ,libxt)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/")    ; no dedicated Xeyes page exists
    (synopsis "Follow-the-mouse X demo")
    (description "Xeyes is a demo program for x.org.  It shows eyes
following the mouse.")
    (license license:x11)))


(define-public pixman
  (package
    (name "pixman")
    (version "0.38.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://www.cairographics.org/releases/pixman-"
         version ".tar.gz"))
       (sha256
        (base32 "1ryxzdf048x7wsx4dlvrr1p00gzwfs7lybnhgc7ygbj0dvyxcrns"))
       (patches
        (search-patches
         "pixman-CVE-2016-5296.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libpng" ,libpng)
       ("zlib" ,zlib)))
    (synopsis "Low-level pixel manipulation library")
    (description "Pixman is a low-level software library for pixel
manipulation, providing features such as image compositing and trapezoid
rasterisation.")
    (home-page "http://www.pixman.org/")
    (license license:x11)))

(define-public libdrm
  (package
    (name "libdrm")
    (version "2.4.103")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dri.freedesktop.org/libdrm/libdrm-"
                    version ".tar.xz"))
              (sha256
               (base32
                "08h2nnf4w96b4ql7485mvjgbbsb8rwc0qa93fdm1cq34pbyszq1z"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       '(,@(match (%current-system)
             ((or "armhf-linux" "aarch64-linux")
              '("-Dexynos=true"
                "-Domap=true"
                "-Detnaviv=true"
                "-Dtegra=true"
                "-Dfreedreno-kgsl=true"))
             (_ '())))

       #:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "meson" "test" "--timeout-multiplier" "5"))))))
    (inputs
     `(("libpciaccess" ,libpciaccess)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://dri.freedesktop.org/wiki/")
    (synopsis "Direct rendering userspace library")
    (description "The Direct Rendering Infrastructure, also known as the DRI,
is a framework for allowing direct access to graphics hardware under the
X Window System in a safe and efficient manner.  It includes changes to the
X server, to several client libraries, and to the kernel (DRM, Direct
Rendering Manager).  The most important use for the DRI is to create fast
OpenGL implementations providing hardware acceleration for Mesa.
Several 3D accelerated drivers have been written to the DRI specification,
including drivers for chipsets produced by 3DFX, AMD (formerly ATI), Intel
and Matrox.")
    (license license:x11)))


(define-public mtdev
  (package
    (name "mtdev")
    (version "1.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "http://bitmath.org/code/mtdev/mtdev-"
               version ".tar.bz2"))
        (sha256
         (base32
          "1q700h9dqcm3zl6c3gj0qxxjcx6ibw2c51wjijydhwdcm26v5mqm"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--disable-static")))
    (home-page "http://bitmath.org/code/mtdev/")
    (synopsis "Multitouch protocol translation library")
    (description "Mtdev is a stand-alone library which transforms all
variants of kernel MT events to the slotted type B protocol.  The events
put into mtdev may be from any MT device, specifically type A without
contact tracking, type A with contact tracking, or type B with contact
tracking.")
    (license license:x11)))

(define-public startup-notification
  (package
    (name "startup-notification")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.freedesktop.org/software/" name
                           "/releases/" name "-" version ".tar.gz"))
       (sha256
        (base32
         "0jmyryrpqb35y9hd5sgxqy2z0r1snw7d3ljw0jak0n0cjdz1yf9w"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libx11" ,libx11)
       ("xcb-util" ,xcb-util)))
    (home-page "https://www.freedesktop.org/wiki/Software/startup-notification/")
    (synopsis "Application startup notification and feedback library")
    (description
     "Startup-notification contains a reference implementation of the startup
notification protocol.  The reference implementation is mostly under an X Window
System style license, and has no special dependencies.")
    ;; Most of the code is provided under x11 license.
    (license license:lgpl2.0+)))

(define-public wmctrl
  (package
    (name "wmctrl")
    (version "1.07")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://sites.google.com/site/tstyblo/wmctrl/wmctrl-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1afclc57b9017a73mfs9w7lbdvdipmf9q0xdk116f61gnvyix2np"))
              (patches (search-patches "wmctrl-64-fix.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--mandir="
                            (assoc-ref %outputs "out")
                            "/share/man"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libx11" ,libx11)
       ("libxmu" ,libxmu)
       ("glib" ,glib)))
    (home-page "http://tomas.styblo.name/wmctrl/")
    (synopsis "Command-line tool to control X window managers")
    (description
     "Wmctrl interacts with an X window manager that is compatible
with the EWMH/NetWM specification.  It can query the window manager for
information, and request for certain window management actions (resize and
move windows, switch between desktops, etc.).")
    (license license:gpl2+)))

(define-public scrot
  (package
    (name "scrot")
    (version "1.5")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/resurrecting-open-source-projects/scrot")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x64b7xqi5cbq29pb8s8r2kzbxaday1f5k0j70n3s2p7sahjxy72"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("autoconf-archive" ,autoconf-archive)
       ("automake" ,automake)))
    (inputs
     `(("giblib" ,giblib)
       ("libx11" ,libx11)
       ("libxcomposite" ,libxcomposite)
       ("libxext" ,libxext)
       ("libXfixes" ,libxfixes)))
    (home-page "https://github.com/resurrecting-open-source-projects/scrot")
    (synopsis "Command-line screen capture utility for X Window System")
    (description
     "Scrot saves a screenshot of a full screen, a window or a part
of the screen selected by mouse.")
    ;; This license removes a clause about X Consortium from the original
    ;; X11 license.
    (license (license:x11-style "file://COPYING"
                                "See 'COPYING' in the distribution."))))

(define-public slop
  (package
    (name "slop")
    (version "7.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/naelstrof/slop")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k8xxb4rj2fylr4vj16yvsf73cyywliz9cy78pl4ibmi03jhg837"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f)) ; no "check" target
    (inputs
     `(("glew" ,glew)
       ("glm" ,glm)
       ("icu4c" ,icu4c)
       ("libxext" ,libxext)
       ("libxrender" ,libxrender)
       ("mesa" ,mesa)))
    (home-page "https://github.com/naelstrof/slop")
    (synopsis "Select a region and print its bounds to stdout")
    (description
     "slop (Select Operation) is a tool that queries for a selection from a
user and prints the region to stdout.  It grabs the mouse and turns it into a
crosshair, lets the user click and drag to make a selection (or click on a
window) while drawing a pretty box around it, then finally prints the
selection's dimensions to stdout.")
    (license license:gpl3+)))

(define-public maim
  (package
    (name "maim")
    (version "5.6.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/naelstrof/maim")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "181mjjrjb9fs1ficcv9miqbk94v95j1yli7fjp2dj514g7nj9l3x"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))            ; no "check" target
    (inputs
     `(("glm" ,glm)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libxcomposite" ,libxcomposite)
       ("libxfixes" ,libxfixes)
       ("libxrandr" ,libxrandr)
       ("mesa" ,mesa)
       ("slop" ,slop)
       ("zlib" ,zlib)))
    (home-page "https://github.com/naelstrof/maim")
    (synopsis "Screenshot utility for X Window System")
    (description
     "maim (Make Image) is a tool that takes screenshots of your desktop and
saves it in any format.  Along with a full screen, it allows you to capture a
predefined region or a particular window.  Also, it makes it possible to
include cursor in the resulting image.")
    (license license:gpl3+)))

(define-public unclutter
  (package
    (name "unclutter")
    (version "8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://ftp.x.org/contrib/utilities/unclutter-"
                    version ".tar.Z"))
              (sha256
               (base32
                "0ahrr5z6wxqqfyihm112hnq0859zlxisrb3y5232zav58j6sfmdq"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin"))
                    (man1 (string-append out "/share/man/man1")))
               (mkdir-p bin)
               (mkdir-p man1)
               (invoke "make" "install" "install.man"
                       (string-append "BINDIR=" bin)
                       (string-append "MANDIR=" man1))))))))
    (inputs `(("libx11" ,libx11)))
    (home-page "http://ftp.x.org/contrib/utilities/")
    (synopsis "Hide idle mouse cursor")
    (description
     "Unclutter is a program which runs permanently in the background of an
X11 session.  It checks on the X11 pointer (cursor) position every few
seconds, and when it finds it has not moved (and no buttons are pressed
on the mouse, and the cursor is not in the root window) it creates a
small sub-window as a child of the window the cursor is in.  The new
window installs a cursor of size 1x1 but a mask of all 0, i.e. an
invisible cursor.  This allows you to see all the text in an xterm or
xedit, for example.  The human factors crowd would agree it should make
things less distracting.")
    (license license:public-domain)))

(define-public xautomation
  (package
    (name "xautomation")
    (version "1.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.hoopajoo.net/static/projects/"
                           "xautomation-" version ".tar.gz"))

       (sha256
        (base32
         "03azv5wpg65h40ip2kk1kdh58vix4vy1r9bihgsq59jx2rhjr3zf"))))
    (build-system gnu-build-system)
    (inputs
     `(("libpng" ,libpng)
       ("libx11" ,libx11)
       ("libxi" ,libxi)
       ("libxtst" ,libxtst)))
    (native-inputs
     `(("xorgproto" ,xorgproto)))
    (synopsis "Tools to automate tasks in X such as detecting on screen images")
    (description
     "Xautomation can control X from the command line for scripts, and
do visual scraping to find things on the screen.  The control interface
allows mouse movement, clicking, button up/down, key up/down, etc, and
uses the XTest extension so you don't have the annoying problems that
xse has when apps ignore sent events.  The visgrep program can find
images inside of images and reports the coordinates, allowing programs
to find buttons, etc, on the screen to click on.")
    (home-page "https://www.hoopajoo.net/projects/xautomation.html")
    (license license:gpl2+)))

(define-public xbanish
  (package
    (name "xbanish")
    (version "1.7")
    (home-page "https://github.com/jcs/xbanish")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ic5f7zgc32p5g1wxas9y5h8dhik0pvsa8wmn6skdry56gw9vg9q"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)))) ; no configure script
    (inputs
     `(("libx11" ,libx11)
       ("libxfixes" ,libxfixes)
       ("libxi" ,libxi)
       ("libxt" ,libxt)))
    (synopsis "Banish the mouse cursor")
    (description
     "@command{xbanish} hides the mouse cursor when you start typing, and
shows it again when the mouse cursor moves or a mouse button is pressed.")
    (license license:bsd-3)))

(define-public xlockmore
  (package
    (name "xlockmore")
    (version "5.66")
    (source (origin
             (method url-fetch)
             (uri (list (string-append "http://sillycycle.com/xlock/"
                                       "xlockmore-" version ".tar.xz")
                        ;; Previous releases are moved to a subdirectory.
                        (string-append "http://sillycycle.com/xlock/"
                                       "recent-releases/"
                                       "xlockmore-" version ".tar.xz")))
             (sha256
              (base32
               "0wdb7gpyjw3sigmhiplgg1bqxz6wipr0c3n9492x2a18cv1saxjr"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list (string-append "--enable-appdefaultdir="
                                              (assoc-ref %outputs "out")
                                              "/lib/X11/app-defaults"))
       #:tests? #f))                            ;no such thing as a test suite
    (inputs
     `(("libX11" ,libx11)
       ("libXext" ,libxext)
       ("libXt" ,libxt)
       ("linux-pam" ,linux-pam)))
    (home-page "https://sillycycle.com/xlockmore.html")
    (synopsis "Screen locker for the X Window System")
    (description
     "XLockMore is a classic screen locker and screen saver for the
X Window System.")
    (license (license:non-copyleft #f "See xlock.c.")
             ))) ; + GPLv2 in modes/glx/biof.c.

(define-public xosd
  (package
    (name "xosd")
    (version "2.2.14")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/libxosd/libxosd/xosd-" version "/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "025m7ha89q29swkc7s38knnbn8ysl24g2h5s7imfxflm91psj7sg"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--mandir=" %output "/share/man"))))
    (inputs
     `(("libx11" ,libx11)
       ("libxt" ,libxt)
       ("libxext" ,libxext)
       ("libxinerama" ,libxinerama)))
    (home-page "https://sourceforge.net/projects/libxosd/")
    (synopsis "X On Screen Display")
    (description
     "XOSD provides a C library and a simple utility (osd_cat) for displaying
transparent text on your screen.")
    (license license:gpl2+)))

(define-public wob
  (package
    (name "wob")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/francma/wob/releases/download/"
                           version "/wob-" version ".tar.gz"))
       (sha256
        (base32 "1vgngcg8wxn6zfg34czn9w55ia0zmhlgnpzf0gh31dc72li9353k"))))
    (build-system meson-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("scdoc" ,scdoc)))
    (inputs
     `(("libseccomp" ,libseccomp)
       ("wayland" ,wayland)
       ("wayland-protocols" ,wayland-protocols)))
    (home-page "https://github.com/francma/wob")
    (synopsis "Lightweight overlay bar for Wayland")
    (description
     "Wob, or Wayland Overlay Bar, is a lightweight overlay volume,
backlight, progress, or anything bar for Wayland.")
    (license license:isc)))

(define-public xbindkeys
  (package
    (name "xbindkeys")
    (version "1.8.7")
    (source (origin
              (method url-fetch)
              ;; Download from the savannah mirror list fails
              (uri (string-append
                    "http://www.nongnu.org/xbindkeys/xbindkeys-"
                    version
                    ".tar.gz"))
              (sha256
               (base32
                "1wl2vc5alisiwyk8m07y1ryq8w3ll9ym83j27g4apm4ixjl8d6x2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'sanitise-shebang
           ;; This wish script uses a strange double shebang that escapes our
           ;; patch-shebangs phase.  Assume that it's unnecessary & replace it.
           (lambda _
             (substitute* "xbindkeys_show"
               (("^#!.*|^exec wish.*") "")
               (("^# \\\\") (string-append "#!" (which "wish"))))
             #t))
         (add-after 'unpack 'patch-references
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "xbindkeys_show"
                 (("\"(xbindkeys)\"" _ command)
                  (format #f "\"~a/bin/~a\"" out command)))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-2.2)
       ("libx11" ,libx11)
       ("tk" ,tk)))
    (home-page "https://www.nongnu.org/xbindkeys/")
    (synopsis "Associate a combination of keys with a shell command")
    (description
     "XBindKeys is a program that allows you to launch shell commands with
your keyboard or your mouse under the X Window System.  It links commands to
keys or mouse buttons, using a configuration file.  It's independent of the
window manager and can capture all keyboard keys (ex: Power, Wake...).  It
optionally supports a Guile-based configuration file layout, which enables you
to access all XBindKeys internals, so you can have key combinations, double
clicks or timed double clicks take actions.  Also all functions that work in
Guile will work for XBindKeys.")
    (license license:gpl2+)))

(define-public sxhkd
  (package
    (name "sxhkd")
    (version "0.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/baskerville/sxhkd")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1winwzdy9yxvxnrv8gqpigl9y0c2px27mnms62bdilp4x6llrs9r"))))
    (build-system gnu-build-system)
    (inputs
     `(("asciidoc" ,asciidoc)
       ("libxcb" ,libxcb)
       ("xcb-util" ,xcb-util)
       ("xcb-util-keysyms" ,xcb-util-keysyms)
       ("xcb-util-wm" ,xcb-util-wm)))
    (arguments
     `(#:phases (modify-phases %standard-phases (delete 'configure))
       #:tests? #f  ; no check target
       #:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" %output)
             ;; Keep the documentation where the build system installs LICENSE.
             (string-append "DOCPREFIX=" %output
                            "/share/doc/" ,name "-" ,version))))
    (home-page "https://github.com/baskerville/sxhkd")
    (synopsis "Simple X hotkey daemon")
    (description "sxhkd is a simple X hotkey daemon with a powerful and
compact configuration syntax.")
    (license license:bsd-2)))

(define-public rxvt-unicode
  (package
    (name "rxvt-unicode")
    (version "9.22")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://dist.schmorp.de/rxvt-unicode/Attic/"
                                  name "-" version ".tar.bz2"))
              (patches (search-patches "rxvt-unicode-escape-sequences.patch"))
              (sha256
               (base32
                "1pddjn5ynblwfrdmskylrsxb9vfnk3w4jdnq2l8xn2pspkljhip9"))))
    (build-system gnu-build-system)
    (arguments
     ;; This sets the destination when installing the necessary terminal
     ;; capability data, which are not provided by 'ncurses'.  See
     ;; https://lists.gnu.org/archive/html/bug-ncurses/2009-10/msg00031.html
     `(#:configure-flags (list "--enable-256-color")
       #:make-flags (list (string-append "TERMINFO="
                                         (assoc-ref %outputs "out")
                                         "/share/terminfo"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-desktop-urxvt
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((output (assoc-ref outputs "out"))
                    (desktop (string-append output "/share/applications")))
               (mkdir-p desktop)
               (with-output-to-file
                   (string-append desktop "/urxvt.desktop")
                 (lambda _
                   (format #t
                           "[Desktop Entry]~@
                           Name=rxvt-unicode~@
                           Comment=~@
                           Exec=~a/bin/urxvt~@
                           TryExec=~@*~a/bin/urxvt~@
                           Icon=~@
                           Type=Application~%"
                           output)))
               #t)))
         (add-after 'install 'install-desktop-urxvtc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((output (assoc-ref outputs "out"))
                    (desktop (string-append output "/share/applications")))
               (mkdir-p desktop)
               (with-output-to-file
                   (string-append desktop "/urxvtc.desktop")
                 (lambda _
                   (format #t
                           "[Desktop Entry]~@
                           Name=rxvt-unicode (client)~@
                           Comment=Rxvt clone with XFT and unicode support~@
                           Exec=~a/bin/urxvtc~@
                           TryExec=~@*~a/bin/urxvtc~@
                           Icon=~@
                           Type=Application~%"
                           output)))
               #t))))))
    (inputs
     `(("libXft" ,libxft)
       ("libX11" ,libx11)))
    (native-inputs
     `(("ncurses" ,ncurses)         ;trigger the installation of terminfo data
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    ;; FIXME: This should only be located in 'ncurses'.  Nonetheless it is
    ;; provided for usability reasons.  See <https://bugs.gnu.org/22138>.
    (native-search-paths
     (list (search-path-specification
            (variable "TERMINFO_DIRS")
            (files '("share/terminfo")))))
    (home-page "http://software.schmorp.de/pkg/rxvt-unicode.html")
    (synopsis "Rxvt clone with XFT and unicode support")
    (description "Rxvt-unicode (urxvt) is a colour vt102 terminal emulator
intended as an xterm replacement for users who do not require features such as
Tektronix 4014 emulation and toolkit-style configurability.  It supports
unicode, XFT and may be extended with Perl plugins.  It also comes with a
client/daemon pair that lets you open any number of terminal windows from
within a single process.")
    (license license:gpl3+)))

(define-public xcape
  (package
    (name "xcape")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/alols/xcape")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09a05cxgrip6nqy1qmwblamp2bhknqnqmxn7i2a1rgxa0nba95dm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       ;; no configure script
       #:phases (modify-phases %standard-phases (delete 'configure))
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "MANDIR=/share/man/man1"
                          "CC=gcc")))
    (inputs
     `(("libxtst" ,libxtst)
       ("libx11" ,libx11)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/alols/xcape")
    (synopsis "Use a modifier key in X.org as another key")
    (description
     "This utility for X.org uses a modifier key as another key when
pressed and released on its own.  The default behaviour is to generate the
Escape key when Left Control is pressed and released on its own.")
    (license license:gpl3+)))

(define-public libwacom
  (package
    (name "libwacom")
    (version "1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/linuxwacom/libwacom/releases/download/"
                    "libwacom-" version "/libwacom-" version ".tar.bz2"))
              (sha256
               (base32
                "0797gc055dgg2jfqijy9823bd83jwr4wb2z9id992qlcr0xmz1rw"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ;; For tests.
       ("python" ,python)))
    (inputs
     `(("gtk+" ,gtk+)
       ("libgudev" ,libgudev)
       ("eudev" ,eudev)
       ("libxml2" ,libxml2)))
    (propagated-inputs
     ;; libwacom includes header files that include GLib, and libinput uses
     ;; those header files.
     `(("glib" ,glib)))
    (home-page "https://linuxwacom.github.io/")
    (synopsis "Helper library for Wacom tablet settings")
    (description
     "Libwacom is a library to help implement Wacom tablet settings.  It is
intended to be used by client-programs that need model identification.  It is
already being used by the gnome-settings-daemon and the GNOME Control Center
Wacom tablet applet.")
    (license license:x11)))

(define-public xf86-input-wacom
  (package
    (name "xf86-input-wacom")
    (version "0.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/linuxwacom/xf86-input-wacom/releases/download/"
             "xf86-input-wacom-" version "/"
             "xf86-input-wacom-" version ".tar.bz2"))
       (sha256
        (base32 "11qk58az6qwii774ga45h5yqzipwn56f0d74kdbajqdv45p85gqj"))))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-sdkdir="
                            (assoc-ref %outputs "out")
                            "/include/xorg")
             (string-append "--with-xorg-conf-dir="
                            (assoc-ref %outputs "out")
                            "/share/X11/xorg.conf.d"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("xorg-server" ,xorg-server)
       ("libxrandr" ,libxrandr)
       ("libxinerama" ,libxinerama)
       ("libxi" ,libxi)
       ("eudev" ,eudev)))
    (home-page "https://linuxwacom.github.io/")
    (synopsis "Wacom input driver for X")
    (description
     "The xf86-input-wacom driver is the wacom-specific X11 input driver for
the X.Org X Server version 1.7 and later (X11R7.5 or later).")
    (license license:x11)))

(define-public redshift
  (package
    (name "redshift")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/jonls/redshift/"
                       "releases/download/v" version
                       "/redshift-" version ".tar.xz"))
       (sha256
        (base32
         "1fi27b73x85qqar526dbd33av7mahca2ykaqwr7siqiw1qqcby6j"))))
    (build-system gnu-build-system)
    (arguments
     `(#:imported-modules (,@%gnu-build-system-modules
                           (guix build python-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'split-outputs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (gtk (assoc-ref outputs "gtk"))
                   (desktop-file "/share/applications/redshift-gtk.desktop"))
               (mkdir-p (string-append gtk "/bin"))
               (link (string-append out "/bin/redshift-gtk")
                     (string-append gtk "/bin/redshift-gtk"))
               (delete-file (string-append out "/bin/redshift-gtk"))
               (copy-recursively (string-append out "/lib")
                                 (string-append gtk "/lib"))
               (delete-file-recursively (string-append out "/lib"))
               (mkdir-p (string-append gtk "/share/applications"))
               (link (string-append out desktop-file)
                     (string-append gtk desktop-file))
               (delete-file (string-append out desktop-file))
               (with-directory-excursion (string-append out "/share")
                 (for-each (lambda (dir)
                             (copy-recursively
                              (string-append out "/share/" dir)
                              (string-append gtk "/share/" dir))
                             (delete-file-recursively dir))
                           '("appdata" "icons")))
               #t)))
         (add-after 'split-outputs 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((gtk (assoc-ref outputs "gtk"))
                    (python-version
                     (@ (guix build python-build-system) python-version))
                    (python (assoc-ref inputs "python"))
                    (sitedir (string-append gtk "/lib/python"
                                            (python-version python)
                                            "/site-packages")))
               (wrap-program (string-append gtk "/bin/redshift-gtk")
                 `("PYTHONPATH" ":" prefix
                   (,(string-append sitedir ":" (getenv "PYTHONPATH"))))
                 `("GI_TYPELIB_PATH" ":" prefix (,(getenv "GI_TYPELIB_PATH"))))
               #t))))))
    (outputs '("out" "gtk"))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("libdrm" ,libdrm)
       ("libx11" ,libx11)
       ("libxcb" ,libxcb)
       ("libxxf86vm" ,libxxf86vm)
       ("glib" ,glib)                   ;for Geoclue2 support

       ;; To build the GTK3 GUI, we need these.
       ("gtk+" ,gtk+)
       ("python" ,python)
       ("python-pygobject" ,python-pygobject)
       ("python-pyxdg" ,python-pyxdg)))
    (home-page "https://github.com/jonls/redshift")
    (synopsis "Adjust the color temperature of your screen")
    (description
     "Redshift adjusts the color temperature according to the position of the
sun.  A different color temperature is set during night and daytime.  During
twilight and early morning, the color temperature transitions smoothly from
night to daytime temperature to allow your eyes to slowly adapt.  At night the
color temperature should be set to match the lamps in your room.")
    (license license:gpl3+)))

(define-public redshift-wayland
  (let ((commit "7da875d34854a6a34612d5ce4bd8718c32bec804")
        (revision "1"))
    (package
      (name "redshift-wayland")
      (version (string-append "1.12-"
                              revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/minus7/redshift")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "0nbkcw3avmzjg1jr1g9yfpm80kzisy55idl09b6wvzv2sz27n957"))))
      (build-system gnu-build-system)
      (arguments
       '(#:phases (modify-phases %standard-phases
                    (add-after 'install 'create-desktop-file
                      (lambda* (#:key outputs #:allow-other-keys)
                        ;; For the GeoClue provider to work, a .desktop file
                        ;; needs to be provided.  A template is available,
                        ;; but it only gets installed when the GUI is enabled.
                        ;; Install it manually for this Wayland variant.
                        (let* ((out (assoc-ref outputs "out"))
                               (desktop-file
                                (string-append
                                 out "/share/applications/redshift.desktop")))
                          (mkdir-p (dirname desktop-file))
                          (copy-file "data/applications/redshift.desktop.in"
                                     desktop-file)
                          (substitute* desktop-file
                            (("^_") ""))
                          #t))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)
         ("intltool" ,intltool)))
      (inputs
       `(("libdrm" ,libdrm)
         ("libx11" ,libx11)
         ("libxcb" ,libxcb)
         ("libxxf86vm" ,libxxf86vm)
         ("glib" ,glib)                 ; for Geoclue2 support
         ("wayland" ,wayland)))
      (home-page "https://github.com/minus7/redshift")
      (synopsis "Adjust the color temperature of your screen (with Wayland support)")
      (description
       "Redshift adjusts the color temperature according to the position of the
sun.  A different color temperature is set during night and daytime.  During
twilight and early morning, the color temperature transitions smoothly from
night to daytime temperature to allow your eyes to slowly adapt.  At night the
color temperature should be set to match the lamps in your room.

This is a fork with added support for Wayland using the wlr-gamma-control
protocol.")
      (license license:gpl3+))))

(define-public gammastep
  (package
    (name "gammastep")
    (version "2.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/chinstrap/gammastep")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11j54rdd3cgngdhjwyapwjbrdm8cii4i7g4zdvfykvmb1w4zdk7g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-python-and-typelib
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Gammastep GUI needs Typelib files from GTK and access
             ;; to Python libraries.
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/gammastep-indicator")
               `("PYTHONPATH" ":" prefix (,(getenv "PYTHONPATH")))
               `("GI_TYPELIB_PATH" ":" prefix
                 (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("intltool" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("gtk" ,gtk+)
       ("libappindicator" ,libappindicator)
       ("libdrm" ,libdrm)
       ("libX11" ,libx11)
       ("libxxf86vm" ,libxxf86vm)
       ("libxcb" ,libxcb)
       ("python" ,python)
       ("python-pygobject" ,python-pygobject)
       ("python-pyxdg" ,python-pyxdg)
       ("wayland" ,wayland)))
    (home-page "https://gitlab.com/chinstrap/gammastep")
    (synopsis "Adjust the color temperature of your screen")
    (description
     "Gammastep automatically adjusts the color temperature of your
screen according to your surroundings.  This may help your eyes hurt
less if you are working in front of the screen at night.")
    (license license:gpl3)))

(define-public xscreensaver
  (package
    (name "xscreensaver")
    (version "5.45")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.jwz.org/xscreensaver/xscreensaver-"
                       version ".tar.gz"))
       (sha256
        (base32 "03fmyjlwjinzv7mih6n07glmys8s877snd8zijk2c0ds6rkxy5kh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'adjust-gtk-resource-paths
           (lambda _
             (substitute* '("driver/Makefile.in" "po/Makefile.in.in")
               (("@GTK_DATADIR@") "@datadir@")
               (("@PO_DATADIR@") "@datadir@"))
             #t)))
       #:configure-flags '("--with-pam" "--with-proc-interrupts"
                           "--without-readdisplay")
       #:make-flags (list (string-append "AD_DIR="
                                         (assoc-ref %outputs "out")
                                         "/lib/X11/app-defaults"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxi" ,libxi)
       ("libxt" ,libxt)
       ("libxft" ,libxft)
       ("libxmu" ,libxmu)
       ("libxpm" ,libxpm)
       ("libglade" ,libglade)
       ("libxml2" ,libxml2)
       ("libsm" ,libsm)
       ("libjpeg" ,libjpeg-turbo)
       ("linux-pam" ,linux-pam)
       ("pango" ,pango)
       ("gtk+" ,gtk+)
       ("perl" ,perl)
       ("cairo" ,cairo)
       ("bc" ,bc)
       ("libxrandr" ,libxrandr)
       ("glu" ,glu)
       ("glib" ,glib)))
    (home-page "https://www.jwz.org/xscreensaver/")
    (synopsis "Classic screen saver suite supporting screen locking")
    (description
     "xscreensaver is a popular screen saver collection with many entertaining
demos.  It also acts as a nice screen locker.")
    ;; xscreensaver doesn't have a single copyright file and instead relies on
    ;; source comment headers, though most files have the same lax
    ;; permissions.  To reduce complexity, we're pointing at Debian's
    ;; breakdown of the copyright information.
    (license (license:non-copyleft
              (string-append
               "http://metadata.ftp-master.debian.org/changelogs/"
               "/main/x/xscreensaver/xscreensaver_5.36-1_copyright")))))

(define-public xssproxy
  (package
    (name "xssproxy")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/timakro/xssproxy")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0c83wmipnsdnbihc5niyczs7jrkss2s8n6iwwjdia7hkjzbd0hl7"))))
    (build-system gnu-build-system)
    (arguments `(#:make-flags `("bindir=/bin"
                                "man1dir=/share/man/man1"
                                ,(string-append "DESTDIR=" (assoc-ref %outputs "out"))
                                "CC=gcc")
                 #:phases (modify-phases %standard-phases
                            (delete 'configure)
                            (delete 'check))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxscrnsaver" ,libxscrnsaver)
       ("dbus" ,dbus)))
    (synopsis "Forward freedesktop.org Idle Inhibition Service calls to Xss")
    (description "xssproxy implements the @code{org.freedesktop.ScreenSaver}
D-Bus interface described in the Idle Inhibition Service Draft by the
freedesktop.org developers.  The inhibition of the screensaver is then
controlled using the XScreenSaverSuspend function from the Xss (X11 Screen
Saver extension) library.")
    (home-page "https://github.com/timakro/xssproxy")
    (license license:gpl3+)))

(define-public xsel
  (package
    (name "xsel")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.vergenet.net/~conrad/software"
                                  "/xsel/download/xsel-" version ".tar.gz"))
              (sha256
               (base32
                "070lbcpw77j143jrbkh0y1v10ppn1jwmjf92800w7x42vh4cw9xr"))))
    (build-system gnu-build-system)
    (inputs
     `(("libxt" ,libxt)))
    (home-page "http://www.vergenet.net/~conrad/software/xsel/")
    (synopsis "Manipulate X selection")
    (description
     "XSel is a command-line program for getting and setting the contents of
the X selection.  Normally this is only accessible by manually highlighting
information and pasting it with the middle mouse button.

XSel reads from standard input and writes to standard output by default,
but can also follow a growing file, display contents, delete entries and more.")
    (license (license:x11-style "file://COPYING"
                                "See COPYING in the distribution."))))

(define-public xdpyprobe
  (package
    (name "xdpyprobe")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alezost/" name
                                  "/releases/download/v" version
                                  "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h09wd2qcg08rj5hcakvdh9q01hkrj8vxly94ax3ch2x06lm0zq8"))))
    (build-system gnu-build-system)
    (inputs
     `(("libx11" ,libx11)))
    (home-page "https://github.com/alezost/xdpyprobe")
    (synopsis "Probe X server for connectivity")
    (description
     "Xdpyprobe is a tiny C program whose only purpose is to probe a
connectivity of the X server running on a particular @code{DISPLAY}.")
    (license license:gpl3+)))

(define-public rofi
  (package
    (name "rofi")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/DaveDavenport/rofi/"
                                  "releases/download/"
                                  version "/rofi-" version ".tar.xz"))
              (sha256
               (base32
                "12p9z8bl1gg8k024m4a6zfz7gf1zbyffardh98raqgabn6knwk22"))))
    (build-system gnu-build-system)
    (inputs
     `(("pango" ,pango)
       ("cairo" ,cairo)
       ("glib" ,glib)
       ("startup-notification" ,startup-notification)
       ("libjpeg" ,libjpeg-turbo)
       ("librsvg" ,librsvg)
       ("libxkbcommon" ,libxkbcommon)
       ("libxcb" ,libxcb)
       ("xcb-util" ,xcb-util)
       ("xcb-util-xrm" ,xcb-util-xrm)
       ("xcb-util-wm" ,xcb-util-wm)))
    (native-inputs
     `(("bison" ,bison)
       ("check" ,check)
       ("flex" ,flex)
       ("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (arguments
     `(#:parallel-tests? #f             ; fails in some circumstances
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'adjust-tests
           (lambda _
             (substitute* '("test/helper-expand.c")
               (("~root") "/root")
               (("~") "")
               (("g_get_home_dir \\(\\)") "\"/\""))
             #t)))))
    (home-page "https://github.com/DaveDavenport/rofi")
    (synopsis "Application launcher")
    (description "Rofi is a minimalist application launcher.  It memorizes which
applications you regularly use and also allows you to search for an application
by name.")
    (license license:expat)))

(define-public tint2
  (package
    (name "tint2")
    (version "0.14.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.com/o9000/" name
                                  "/repository/archive.tar.gz?ref=" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kwzwxy4myagybm3rc7dgynfgp75742n348qibn1p2an9ggyivda"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ;no test target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-installation-prefix
           (lambda _
             (substitute* "CMakeLists.txt"
               (("/etc") "${CMAKE_INSTALL_PREFIX}/etc"))
             #t)))))
    (inputs
     `(("gtk+" ,gtk+-2)
       ("imlib2" ,imlib2)
       ("librsvg" ,librsvg)
       ("libxcomposite" ,libxcomposite)
       ("libxdamage" ,libxdamage)
       ("libxft" ,libxft)
       ("libxinerama" ,libxinerama)
       ("libxrandr" ,libxrandr)
       ("startup-notification" ,startup-notification)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (home-page "https://gitlab.com/o9000/tint2")
    (synopsis "Lightweight task bar")
    (description
     "Tint2 is a simple task bar made for modern X window managers.  It was
specifically made for Openbox but it should also work with other window
managers (GNOME, KDE, XFCE etc.).

The taskbar includes transparency and color settings for the font, icons,
border, and background.  It also supports multihead setups, customized mouse
actions, a built-in clock, a battery monitor and a system tray.")
    (license license:gpl2)))

(define-public dzen
  (let ((commit "488ab66019f475e35e067646621827c18a879ba1")
        (revision "1"))
    (package
     (name "dzen")
     (version (string-append "0.9.5-" ; Taken from `config.mk`.
                             revision "." (string-take commit 7)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/robm/dzen")
                    (commit commit)))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "0y47d6ii87vf4a517gi4fh0yl06f8b085sra77immnsasbq9pxnw"))))
     (build-system gnu-build-system)
     (arguments
      `(#:tests? #f ; No test suite.
        #:make-flags ; Replacement for `config.mk`.
        (list
         (string-append "VERSION = " ,version)
         (string-append "PREFIX = " %output)
         "MANPREFIX = ${PREFIX}/share/man"
         "INCS = -I."
         "LIBS = -lc -lX11 -lXinerama -lXpm $(shell pkg-config --libs xft)"
         "CFLAGS = -Wall -Os ${INCS} -DVERSION=\\\"${VERSION}\\\"\
         -DDZEN_XINERAMA -DDZEN_XPM -DDZEN_XFT $(shell pkg-config --cflags xft)"
         "LDFLAGS = ${LIBS}"
         "CC = gcc"
         "LD = ${CC}")
        #:phases
        (modify-phases %standard-phases
          (delete 'configure) ; No configuration script.
          ;; Use own make-flags instead of `config.mk`.
          (add-before 'build 'dont-include-config-mk
            (lambda _
              (substitute* "Makefile" (("include config.mk") ""))
              #t)))))
     (inputs
      `(("libx11"      ,libx11)
        ("libxft"      ,libxft)
        ("libxpm"      ,libxpm)
        ("libxinerama" ,libxinerama)))
     (native-inputs `(("pkg-config" ,pkg-config)))
     (synopsis "General purpose messaging, notification and menuing program for X11")
     (description "Dzen is a general purpose messaging, notification and menuing
program for X11.  It was designed to be fast, tiny and scriptable in any language.")
     (home-page "https://github.com/robm/dzen")
     (license license:expat))))

(define-public xftwidth
  (package
    (name "xftwidth")
    (version "20170402")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "http://github.com/vixus0/xftwidth")
	     (commit "35ff963908d41a8a6a7101c434c88255728025ee")))
       (sha256
	(base32
	 "1jwl25785li24kbp0m1wxfwk4dgxkliynn03nsj813cjr34kq16h"))
       (file-name (string-append name "-" version "-checkout"))))
    (build-system gnu-build-system)
    (inputs `(("freetype" ,freetype)
              ("libx11" ,libx11)
              ("fontconfig" ,fontconfig)
              ("libxft" ,libxft)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-makefile ; /usr/bin doesn't show up in PATH
           (lambda _ (substitute* "Makefile" (("usr/") "")) #t))
         (delete 'check) ; no check included in Makefile
         (delete 'configure))
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "DESTDIR=" out)))))
    (home-page "https://github.com/vixus0/xftwidth")
    (synopsis "Calculator for determining pixel widths of displayed text using Xft fonts")
    (description "xftwidth is a small C program for calculating the pixel
widths of displayed text using Xft fonts. It is especially useful in scripts
for displaying text in graphical panels, menus, popups, and notification
windows generated using dzen. These scripts are often used in conjunction with
minimalistic tiling window managers such as herbstluftwm and bspwm.")
    (license license:expat)))

(define-public xcb-util-xrm
  (package
    (name "xcb-util-xrm")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Airblader/xcb-util-xrm/releases"
                    "/download/v" version "/xcb-util-xrm-" version ".tar.bz2"))
              (sha256
               (base32
                "118cj1ybw86pgw0l5whn9vbg5n5b0ijcpx295mwahzi004vz671h"))
              (modules '((guix build utils)))
              (snippet
               ;; Drop bundled m4.
               '(begin
                  (delete-file-recursively "m4")
                  #t))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("m4" ,m4)
       ("libx11" ,libx11))) ; for tests
    (inputs
     `(("libxcb" ,libxcb)
       ("xcb-util" ,xcb-util)))
    (home-page "https://github.com/Airblader/xcb-util-xrm")
    (synopsis "XCB utility functions for the X resource manager")
    (description
     "The XCB util module provides a number of libraries which sit on
top of libxcb, the core X protocol library, and some of the extension
libraries.  These experimental libraries provide convenience functions
and interfaces which make the raw X protocol more usable.  Some of the
libraries also provide client-side code which is not strictly part of
the X protocol but which has traditionally been provided by Xlib.

XCB util-xrm module provides the following libraries:

- xrm: utility functions for the X resource manager.")
    (license license:x11)))

(define-public xcalib
  (package
    (name "xcalib")
    (version "0.10")
    (home-page "https://github.com/OpenICC/xcalib")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05fzdjmhiafgi2jf0k41i3nm0837a78sb6yv59cwc23nla8g0bhr"))
              (patches
               (list
                ;; Add missing documentation for the new --output option.
                ;; This upstream patch can be removed on the next update.
                (origin
                  (method url-fetch)
                  (uri (string-append
                        home-page "/commit/"
                        "ae03889b91fe984b18e925ad2b5e6f2f7354e058.patch"))
                  (file-name "xcalib-update-man-page.patch")
                  (sha256
                   (base32
                    "0f7b4d5484x4b9n1bwhqmar0kcaa029ffff7bp3xpr734n1qgqb6")))))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; no test suite
    (inputs `(("libx11" ,libx11)
              ("libxext" ,libxext)
              ("libxrandr" ,libxrandr)
              ("libxxf86vm" ,libxxf86vm)))
    (synopsis "Tiny monitor calibration loader for XFree86 (or X.org)")
    (description "xcalib is a tiny tool to load the content of vcgt-Tags in ICC
profiles to the video card's gamma ramp.  It does work with most video card
drivers except the generic VESA driver.  Alter brightness, contrast, RGB, and
invert colors on a specific display/screen.")
    (license license:gpl2)))

(define-public nxbelld
  (package
    (name "nxbelld")
    (version "0.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dusxmt/nxbelld")
                    (commit version)))
              (sha256
               (base32
                "04qwhmjs51irinz5mjlxdb3dc6vr79dqmc5fkj80x1ll3ylh5n3z"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags `("--enable-sound"
                                     "--enable-wave"
                                     "--enable-alsa")))
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("pkg-config" ,pkg-config)
                     ("perl" ,perl)))
    (inputs `(("libx11" ,libx11)
              ("alsa-lib" ,alsa-lib)))
    (synopsis "Daemon that performs an action every time the X11 bell is rung")
    (description "nxbelld is a tiny utility to aid people who either don't
like the default PC speaker beep, or use a sound driver that doesn't have
support for the PC speaker.  The utility performs a given action every time
the X bell is rung.  The actions nxbelld can currently perform include running
a specified program, emulating the PC speaker beep using the sound card (default),
or playing a PCM encoded WAVE file.")
    (home-page "https://github.com/dusxmt/nxbelld")
    (license license:gpl3+)))

(define-public xautolock
  (package
    (name "xautolock")
    (version "2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.ibiblio.org/pub/linux/X11/"
                                  "screensavers/xautolock-" version ".tgz"))
              (sha256
               (base32
                "18jd3k3pvlm5x1adyqw63z2b3f4ixh9mfvz9asvnskk3fm8jgw0i"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("imake" ,imake)))
    (inputs
     `(("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxscrnsaver" ,libxscrnsaver)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((imake (assoc-ref inputs "imake"))
                   (out   (assoc-ref outputs "out")))
               ;; Generate Makefile
               (invoke "xmkmf")
               (substitute* "Makefile"
                 ;; These imake variables somehow remain undefined
                 (("DefaultGcc2[[:graph:]]*Opt") "-O2")
                 ;; Reset a few variable defaults that are set in imake templates
                 ((imake) out)
                 (("(MANPATH = )[[:graph:]]*" _ front)
                  (string-append front out "/share/man")))
               ;; Old BSD-style 'union wait' is unneeded (defining
               ;; _USE_BSD did not seem to fix it)
               (substitute* "src/engine.c"
                 (("union wait  status") "int status = 0"))
               #t)))
         (add-after 'install 'install/man
           (lambda _
             (invoke "make" "install.man"))))))
    (home-page "https://ibiblio.org/pub/Linux/X11/screensavers/")
    (synopsis "Program launcher for idle X sessions")
    (description "Xautolock monitors input devices under the X Window
System, and launches a program of your choice if there is no activity after
a user-configurable period of time.")
    (license license:gpl2)))

(define-public screen-message
  (package
    (name "screen-message")
    (version "0.26")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.joachim-breitner.de/archive/screen-message"
                    "/screen-message-" version ".tar.gz"))
              (sha256
               (base32
                "0dwgm4z3dfk6xz41w8xiv0hmnwr74gf3ykb91b090hc4ffwsf4mw"))))
    (build-system gnu-build-system)
    (inputs `(("gtk3" ,gtk+)
              ("gdk" ,gdk-pixbuf)
              ("pango" ,pango)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments
     ;; The default configure puts the 'sm' binary in games/ instead of bin/ -
     ;; this fixes it:
     `(#:make-flags (list (string-append "execgamesdir=" %output "/bin"))))
    (synopsis "Print messages on your screen")
    (description "@code{screen-message} is a tool for displaying text on
your screen.  It will make the text as large as possible and display it
with black color on a white background (colors are configurable on the
commandline).")
    (home-page "https://www.joachim-breitner.de/projects#screen-message")
    (license license:gpl2+)))

(define-public xss-lock
  ;; xss-lock does not seem to be maintained any longer, but the last commits
  ;; fix important issues so we package them.
  (let ((version "0.3.0")
        (revision "1")
        (commit "1e158fb20108058dbd62bd51d8e8c003c0a48717"))
    (package
      (name "xss-lock")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://bitbucket.org/raymonad/xss-lock.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "10hx7k7ga8g08akwz8qrsvj8iqr5nd4siiva6sjx789jvf0sak7r"))))
      (build-system cmake-build-system)
      (inputs `(("glib" ,glib)
                ("xcb-util" ,xcb-util)))
      (native-inputs
       `(("python-docutils" ,python-docutils)
         ("pkg-config" ,pkg-config)))
      (arguments
       `(#:tests? #f))
      (synopsis "Use external screen locker on events")
      (description "@code{xss-lock} listens to X signals to fire up a
user-defined screensaver.  In effect this automatically locks the
screen when closing a laptop lid or after a period of user inactivity (as set
with @code{xset s TIMEOUT}).  The notifier command, if specified, is executed
first.  Additionally, xss-lock uses the inhibition logic to lock the screen
before the system goes to sleep.")
      (home-page "https://bitbucket.org/raymonad/xss-lock")
      (license license:expat))))

(define-public python-pyperclip
  (package
    (name "python-pyperclip")
    (version "1.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pyperclip" version))
        (sha256
         (base32
          "1p505c23ji06r28k1y67siihsbdzdf1brhlqpyv9ams4gk9863pp"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; Not clear how to make tests pass.
    (inputs
     `(("xclip" ,xclip)
       ("xsel" ,xsel)))
    (home-page "https://github.com/asweigart/pyperclip")
    (synopsis "Python clipboard module")
    (description
     "Pyperclip is a clipboard module for Python, handling copy/pasting from
the X11 clipboard")
    (license license:bsd-3)))

(define-public numlockx
  (package
    (name "numlockx")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    ;; It seems that upstream is gone.
                    (url "https://github.com/rg3/numlockx")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1w49fayhwzn5rx0z1q2lrvm7z8jrd34lgb89p853a024bixc3cf2"))))
    (build-system gnu-build-system)
    (inputs
     `(("xorg-server" ,xorg-server)))
    (home-page "https://github.com/rg3/numlockx")
    (synopsis "Turns on the numlock key in X11")
    (description "@command{numlockx} is a tiny program that lets you turn on
the numlock key in X11.  It can be called from the user's initialization files
to automatically turn it on on login.")
    (license license:expat)))

(define-public xrandr-invert-colors
  (package
    (name "xrandr-invert-colors")
    (version "0.01")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zoltanp/xrandr-invert-colors")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1br3x9vr6xm4ika06n8cfxx1b3wdchdqvyzjl4y1chmivrml8x9h"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CC=gcc")
       #:tests? #f ; there are none
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin")))
               (install-file "xrandr-invert-colors.bin" bin)
               #t))))))
    (inputs
     `(("libxrandr" ,libxrandr)))
    (home-page "https://github.com/zoltanp/xrandr-invert-colors")
    (synopsis "Invert display colors")
    (description "This package provides a small utility for inverting the
colors on all monitors attached to an XRandR-capable X11 display server.")
    (license license:gpl3+)))

(define-public sct
  (package
    (name "sct")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.umaxx.net/dl/sct-"
                       version ".tar.gz"))
       (sha256
        (base32 "0lrhx771iccbw04wrhj0ygids1pzmjfc4hvklm30m3p3flvhqf0m"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list ,(string-append "CC=" (cc-for-target)))
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'fix-sctd-paths
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (coreutils (assoc-ref inputs "coreutils"))
                   (inetutils (assoc-ref inputs "inetutils"))
                   (sed (assoc-ref inputs "sed")))
               (substitute* "sctd.sh"
                 (("\\$\\(which sct\\)") (string-append out "/bin/sct"))
                 (("date") (string-append coreutils "/bin/date"))
                 (("printf") (string-append coreutils "/bin/printf"))
                 (("sleep") (string-append coreutils "/bin/sleep"))
                 (("logger") (string-append inetutils "/bin/logger"))
                 (("sed") (string-append sed "/bin/sed"))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "sct" (string-append out "/bin"))
               (install-file "sctd.sh" (string-append out "/bin"))
               (install-file "sct.1" (string-append out "/man/man1"))
               (install-file "sctd.1" (string-append out "/man/man1"))
               (rename-file (string-append out "/bin/sctd.sh")
                            (string-append out "/bin/sctd"))
               #t))))))
    (inputs
     `(("coreutils" ,coreutils) ; sctd uses "date", "printf" and "sleep"
       ("inetutils" ,inetutils) ; sctd uses "logger"
       ("libxrandr" ,libxrandr)
       ("sed" ,sed))) ; sctd uses "sed"
    (home-page "https://www.umaxx.net")
    (synopsis "Set the color temperature of the screen")
    (description "@code{sct} is a lightweight utility to set the color
temperature of the screen.")
    (license (license:non-copyleft "file://sct.c")))) ; "OpenBSD" license

(define-public xsecurelock
  (package
    (name "xsecurelock")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/google/xsecurelock/releases"
                    "/download/v" version "/xsecurelock-" version ".tar.gz"))
              (sha256
               (base32 "070gknyv0s5hz9hkc6v73m2v7ssyjwgl93b5hd4glayfqxqjbmdp"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       '("--with-pam-service-name=login"
         "--with-xkb"
         "--with-default-authproto-module=/run/setuid-programs/authproto_pam")))
    (native-inputs
     `(("pandoc" ,pandoc)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("libX11" ,libx11)
       ("libxcomposite" ,libxcomposite)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("libxft" ,libxft)
       ("libxmu" ,libxmu)
       ("libxrandr" ,libxrandr)
       ("libxscrnsaver" ,libxscrnsaver)
       ("linux-pam" ,linux-pam)))
    (home-page "https://github.com/google/xsecurelock")
    (synopsis "X11 screen lock utility with the primary goal of security")
    (description "@code{xsecurelock} is an X11 screen locker which uses
a modular design to avoid the usual pitfalls of screen locking utility design.

As a consequence of the modular design, the usual screen locker service
shouldn't be used with @code{xsecurelock}.  Instead, you need to add a helper
binary to setuid-binaries:
@example
(setuid-programs (cons*
                   (file-append xsecurelock \"/libexec/xsecurelock/authproto_pam\")
                   %setuid-programs))
@end example")
    (license license:asl2.0)))

(define-public wl-clipboard
  (package
    (name "wl-clipboard")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bugaevc/wl-clipboard")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c4w87ipsw09aii34szj9p0xfy0m00wyjpll0gb0aqmwa60p0c5d"))))
    (build-system meson-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("wayland" ,wayland)
       ("wayland-protocols" ,wayland-protocols)))
    (home-page "https://github.com/bugaevc/wl-clipboard")
    (synopsis "Command-line copy/paste utilities for Wayland")
    (description "Wl-clipboard is a set of command-line copy/paste utilities for
Wayland.")
    (license license:gpl3+)))

(define-public autocutsel
  (package
    (name "autocutsel")
    (version "0.10.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/sigmike/autocutsel"
                                  "/releases/download/" version "/"
                                  "autocutsel-" version ".tar.gz"))
              (sha256
               (base32
                "05zb85imp42birvrc320q20r98qddc5vxx169dnl753l5za0czpi"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f)) ; no "check" target
    (native-inputs `(("libx11" ,libx11)
                     ("libxaw" ,libxaw)))
    (home-page "https://www.nongnu.org/autocutsel/")
    (synopsis "Automated X11 clipboard and cutbuffer synchronization")
    (description "@code{autocutsel} tracks changes in the server's cutbuffer
and clipboard selection.  When the clipboard is changed, it updates the
cutbuffer.  When the cutbuffer is changed, it owns the clipboard selection.
The cutbuffer and clipboard selection are always synchronized.")
    (license license:gpl2+)))

(define-public jgmenu
  (package
    (name "jgmenu")
    (version "4.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/johanmalm/jgmenu")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1wsh37rapb1bszlq36hvwxqvfds39hbvbl152m8as4zlh93wfvvk"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("cppcheck" ,cppcheck)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("cairo" ,cairo)
       ("glib" ,glib)
       ("librsvg" ,librsvg)
       ("libx11" ,libx11)
       ("libxml2" ,libxml2)
       ("libxrandr" ,libxrandr)
       ("pango" ,pango)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             (substitute* "scripts/cppcheck-wrapper.sh"
               (("--library=/usr/share/cppcheck/cfg/gnu\\.cfg")
                ""))
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "CC" "gcc")
             (invoke "./configure"
                     (string-append "--prefix=" (assoc-ref outputs "out")))
             #t)))))
    (synopsis "Simple X11 menu")
    (description
     "This is a simple menu for X11 designed for scripting and tweaking.  It
can optionally use some appearance settings from XSettings, tint2 and GTK.")
    (home-page "https://jgmenu.github.io/")
    (license license:gpl2)))

(define-public xwallpaper
  (package
    (name "xwallpaper")
    (version "0.6.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stoeckmann/xwallpaper")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10klm81rs3k3l2i7whpvcsg95x51ja11l86fmwbrvg3kq705p2sr"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libjpeg-turbo" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libxpm" ,libxpm)
       ("pixman" ,pixman)
       ("xcb-util" ,xcb-util)
       ("xcb-util-image" ,xcb-util-image)))
    (home-page "https://github.com/stoeckmann/xwallpaper")
    (synopsis "Wallpaper setting utility for X")
    (description
     "The xwallpaper utility allows you to set image files as your X
wallpaper. JPEG, PNG, and XPM file formats are supported.

The wallpaper is also advertised to programs which support semi-transparent
backgrounds.")
    (license license:isc)))

(define-public xwrits
  (package
    (name "xwrits")
    (version "2.26")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.lcdf.org/~eddietwo/xwrits/"
                           "xwrits-" version ".tar.gz"))
       (sha256
        (base32 "1n7y0fqpcvmzznvbsn14hzy5ddaa3lilm8aw6ckscqndnh4lijma"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/xwrits")))
               (install-file "GESTURES" doc)
               (install-file "README" doc)
               #t))))))
    (inputs
     `(("libx11" ,libx11)
       ("libxinerama" ,libxinerama)))
    (home-page "https://www.lcdf.org/~eddietwo/xwrits/")
    (synopsis "Reminds you to take wrist breaks")
    (description "Xwrits reminds you to take wrist breaks for prevention or
management of repetitive stress injuries.  When you should take a break, it
pops up an X window, the warning window.  You click on the warning window,
then take a break.  The window changes appearance while you take the break.
It changes again when your break is over.  Then you just resume typing.
Xwrits hides itself until you should take another break.")
    (license license:gpl2)))

(define-public xsettingsd
  (package
    (name "xsettingsd")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/derat/xsettingsd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14gnkz18dipsa2v24f4nm9syxaa7g21iqjm7y65jn849ka2jr1h8"))))
    (build-system scons-build-system)
    (inputs
     `(("libx11" ,libx11)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("googletest" ,googletest)
       ("googletest-source" ,(package-source googletest))))
    (arguments
     `(#:scons ,scons-python2
       #:scons-flags
       (list "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-sconstruct
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "SConstruct"
               ;; scons doesn't pick up environment variables automatically
               ;; so it needs help to find path variables
               (("env = Environment\\(")
                "env = Environment(
                         ENV = {
                           'PATH': os.environ['PATH'],
                           'CPATH': os.environ['C_INCLUDE_PATH'],
                           'LIBRARY_PATH': os.environ['LIBRARY_PATH'],
                           'PKG_CONFIG_PATH': os.environ['PKG_CONFIG_PATH']
                         },")
               ;; Update path to gtest source files used in tests
               (("/usr/src/gtest") (string-append
                                    (assoc-ref inputs "googletest-source")
                                    "/googletest"))
               ;; Exclude one warning that causes a build error
               (("-Werror") "-Werror -Wno-error=sign-compare"))
             #t))
         ;; The SConstruct script doesn't configure installation so
         ;; binaries must be copied to the output path directly
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (mkdir-p bin)
               (install-file "xsettingsd" bin)
               (install-file "dump_xsettings" bin)
               #t))))))
    (home-page "https://github.com/derat/xsettingsd")
    (synopsis "Xorg settings daemon")
    (description "@command{xsettingsd} is a lightweight daemon that provides settings to
Xorg applications via the XSETTINGS specification.  It is used for defining
font and theme settings when a complete desktop environment (GNOME, KDE) is
not running.  With a simple @file{.xsettingsd} configuration file one can avoid
configuring visual settings in different UI toolkits separately.")
    (license license:bsd-3)))

(define-public clipnotify
  (package
    (name "clipnotify")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cdown/clipnotify")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1v3ydm5ljy8z1savmdxrjyx7a5bm5013rzw80frp3qbbjaci0wbg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin"))
                    (doc  (string-append %output "/share/doc/" ,name "-" ,version)))
               (install-file "clipnotify" bin)
               (install-file "README.md" doc)
               #t))))
       #:make-flags
       (list ,(string-append "CC=" (cc-for-target)))
       #:tests? #f))                    ; no test suite
    (inputs
     `(("libx11" ,libx11)
       ("libXfixes" ,libxfixes)))
    (home-page "https://github.com/cdown/clipnotify")
    (synopsis "Notify on new X clipboard events")
    (description "@command{clipnotify} is a simple program that, using the
XFIXES extension to X11, waits until a new selection is available and then
exits.

It was primarily designed for clipmenu, to avoid polling for new selections.

@command{clipnotify} doesn't try to print anything about the contents of the
selection, it just exits when it changes.  This is intentional -- X11's
selection API is verging on the insane, and there are plenty of others who
have already lost their sanity to bring us xclip/xsel/etc.  Use one of those
tools to complement clipnotify.")
    (license license:public-domain)))

(define-public clipmenu
  (let ((commit "bcbe7b144598db4a103f14e8408c4b7327d6d5e1")
        (revision "1"))
    (package
      (name "clipmenu")
      (version (string-append "6.0.1-"
                              revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cdown/clipmenu")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0053j4i14lz5m2bzc5sch5id5ilr1bl196mp8fp0q8x74w3vavs9"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out  (assoc-ref outputs "out"))
                      (bin  (string-append out "/bin"))
                      (doc  (string-append %output "/share/doc/"
                                           ,name "-" ,version)))
                 (install-file "clipdel" bin)
                 (install-file "clipmenu" bin)
                 (install-file "clipmenud" bin)
                 (install-file "README.md" doc)
                 #t)))
           (add-after 'install 'wrap-script
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out               (assoc-ref outputs "out"))
                      (clipnotify        (assoc-ref inputs "clipnotify"))
                      (coreutils-minimal (assoc-ref inputs "coreutils-minimal"))
                      (gawk              (assoc-ref inputs "gawk"))
                      (util-linux        (assoc-ref inputs "util-linux"))
                      (xdotool           (assoc-ref inputs "xdotool"))
                      (xsel              (assoc-ref inputs "xsel")))
                 (for-each
                  (lambda (prog)
                    (wrap-script (string-append out "/bin/" prog)
                      `("PATH" ":" prefix
                        ,(map (lambda (dir)
                                (string-append dir "/bin"))
                              (list clipnotify coreutils-minimal
                                    gawk util-linux xdotool xsel)))))
                  '("clipmenu" "clipmenud" "clipdel")))
               #t))
           (replace 'check
             (lambda* (#:key inputs outputs #:allow-other-keys)
               ;; substitute a shebang appearing inside a string (the test
               ;; file writes this string to a temporary file):
               (substitute* "tests/test-clipmenu"
                 (("#!/usr/bin/env bash")
                  (string-append "#!" (which "bash"))))
               (invoke "tests/test-clipmenu")
               #t)))))
      (inputs
       `(("clipnotify" ,clipnotify)
         ("coreutils-minimal" ,coreutils-minimal)
         ("gawk" ,gawk)
         ("guile" ,guile-3.0) ; for wrap-script
         ("util-linux" ,util-linux)
         ("xdotool" ,xdotool)
         ("xsel" ,xsel)))
      (home-page "https://github.com/cdown/clipmenu")
      (synopsis "Simple clipboard manager using dmenu or rofi and xsel")
      (description "Start @command{clipmenud}, then run @command{clipmenu} to
select something to put on the clipboard.

When @command{clipmenud} detects changes to the clipboard contents, it writes
them out to the cache directory.  @command{clipmenu} reads the cache directory
to find all available clips and launches @command{dmenu} (or @command{rofi},
depending on the value of @code{CM_LAUNCHER}) to let the user select a clip.
After selection, the clip is put onto the PRIMARY and CLIPBOARD X selections.")
      (license license:public-domain))))

(define-public kbdd
  (package
    (name "kbdd")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/qnikst/kbdd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0qkq75grbd4wkx4nlvswgavpijk9ad0pzqyj89a0ayjsbsn36pqy"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("glib" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("dbus-glib" ,dbus-glib)
       ("glib" ,glib)
       ("libx11" ,libx11)))
    (home-page "https://github.com/qnikst/kbdd")
    (synopsis "Per-window keyboard layout switching daemon for X")
    (description "@command{kbdd} is a simple keyboard layout switching
program, which is designed to run in an X11 session and remember
keyboard layouts on a per-window basis.  That can be very handy for a
user of a non-US keyboard who does not want to jump through layouts back
and forth while typing in terminals (mostly in a latin alphabet) and
some kind of chat (in native language).

@command{kbdd} also supports D-Bus signals, which makes it possible to
create layout indicator widgets.")
    (license license:bsd-2)))

(define-public j4-dmenu-desktop
  (package
    (name "j4-dmenu-desktop")
    (version "2.18")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/enkore/j4-dmenu-desktop")
                    (commit (string-append "r" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gxpgifzy0hnpd0ymw3r32amzr32z3bgb90ldjzl438p6h1q0i26"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("catch2" ,catch-framework2)))
    (arguments
     `(#:configure-flags '("-DWITH_GIT_CATCH=off")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-catch
           (lambda _
             (substitute* "CMakeLists.txt"
               (("PATH_SUFFIXES catch") "PATH_SUFFIXES catch2"))
             #t))
         (replace 'check
           (lambda _
             (invoke "./j4-dmenu-tests" "exclude:SearchPath/XDG_DATA_HOME"))))))
    (synopsis "Fast desktop menu")
    (description
     "j4-dmenu-desktop is a replacement for i3-dmenu-desktop.  Its purpose
is to find @file{.desktop} files and offer you a menu to start an application
using @command{dmenu}.")
    (home-page "https://github.com/enkore/j4-dmenu-desktop")
    (license license:gpl3+)))

(define-public wofi
  (package
    (name "wofi")
    (version "1.2.4")
    (source (origin
              (method hg-fetch)
              (uri (hg-reference
                    (url "https://hg.sr.ht/~scoopta/wofi")
                    (changeset (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bnf078fg1kwslzwm1mjxwcqqq3bhk1dzymwfw9gk3brqbxrl75c"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk3" ,gtk+)
       ("wayland" ,wayland)))
    (synopsis "Launcher/menu program for wayland")
    (description
     "Wofi is a launcher/menu program for wlroots based wayland compositors
such as sway, similar to @command{rofi}.")
    (home-page "https://hg.sr.ht/~scoopta/wofi")
    (license license:gpl3+)))

(define-public dex
  (package
    (name "dex")
    (version "0.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/jceb/dex"))
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "03aapcywnz4kl548cygpi25m8adwbmqlmwgxa66v4156ax9dqs86"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:tests? #f))
    (inputs
     `(("python" ,python)))
    (native-inputs
     `(("python-sphinx" ,python-sphinx)))
    (home-page "https://github.com/jceb/dex")
    (synopsis "Execute DesktopEntry files")
    (description
     "@command{dex}, @dfn{DesktopEntry Execution}, is a program to generate
and execute @file{.desktop} files of the Application type.")
    (license license:gpl3+)))

(define-public sx
  (package
    (name "sx")
    (version "2.1.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Earnestly/sx")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0p24ghp1ygvyc2hv81byhxax7491yhcc5priq5ldv07nzl7akagc"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no tests
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "PREFIX=" out)))
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure))))
    (propagated-inputs
     `(("xauth" ,xauth)))
    (home-page "https://github.com/Earnestly/sx")
    (synopsis "Start an xorg server")
    (description
     "@command{sx} is a simple alternative to both @command{xinit} and
@command{startx} for starting an Xorg server.")
    (license license:x11)))

(define-public hsetroot
  (package
    (name "hsetroot")
    (version "1.0.5")
    (home-page "https://github.com/himdel/hsetroot")
    (source (origin
              (method git-fetch)
              (file-name (git-file-name name version))
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (sha256
               (base32
                "1jbk5hlxm48zmjzkaq5946s58rqwg1v1ds2sdyd2ba029hmvr722"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags
       (list
        "CC=gcc"
        (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'install 'mkdir-install-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/bin"))))))))
    (inputs
     `(("libx11" ,libx11)
       ("imlib2" ,imlib2)
       ("libxinerama" ,libxinerama)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Imlib2-based wallpaper changer")
    (description
     "The @command{hsetroot} command composes wallpapers for X.
This package is the fork of hsetroot by Hyriand.")
    (license license:gpl2+)))
