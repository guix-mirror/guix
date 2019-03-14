;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014, 2015, 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2013, 2015, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2015 Alexander I.Grafov <grafov@gmail.com>
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2015 xd1le <elisp.vim@gmail.com>
;;; Copyright © 2015 Florian Paul Schmidt <mista.tapas@gmx.net>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2016, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016, 2017, 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016 Petter <petter@mykolab.ch>
;;; Copyright © 2017 Mekeor Melire <mekeor.melire@gmail.com>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Marek Benc <dusxmt@gmx.com>
;;; Copyright © 2017 Mike Gerwitz <mtg@gnu.org>
;;; Copyright © 2018 Thomas Sigurdsen <tonton@riseup.net>
;;; Copyright © 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Nam Nguyen <namn@berkeley.edu>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages bison)
  #:use-module (ice-9 match))

;; packages outside the x.org system proper

(define-public arandr
  (package
    (name "arandr")
    (version "0.1.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://christian.amsuess.com/tools/" name
                                  "/files/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1i3f1agixxbfy4kxikb2b241p7c2lg73cl9wqfvlwz3q6zf5faxv"))
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
     `(#:python ,python-2     ;incompatible with python 3
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "screenlayout/xrandr.py"
               (("\"xrandr\"") (string-append "\"" (assoc-ref inputs "xrandr")
                                              "/bin/xrandr\"")))
             #t)))
       #:tests? #f)) ;no tests
    (inputs `(("pygtk" ,python2-pygtk)
              ("xrandr" ,xrandr)))
    (native-inputs `(("gettext"           ,gettext-minimal)
                     ("python-docutils"   ,python2-docutils)))
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
  ;; Use latest commit since 1.7 lacks many new features such as the
  ;; autorandr_launcher.
  (let ((commit "b484c0ea9c9a4838278bbd661a7cc384333c1df8"))
    (package
      (name "autorandr")
      (version (git-version "1.7" "1" commit))
      (home-page "https://github.com/phillipberndt/autorandr")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0da17kzsisjv3s993j5idkk1n2d2cvjdn7pngs2b0ic1r2h5z02h"))))
      (build-system python-build-system)
      (native-inputs
       `(("man-db" ,man-db)))
      (inputs
       `(("xrandr" ,xrandr)
         ("libxcb" ,libxcb)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'build 'configure
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "autorandr.py"
                 (("popen\\(\"xrandr") (string-append "popen(\""
                                                      (assoc-ref inputs "xrandr")
                                                      "/bin/xrandr"))
                 (("\\[\"xrandr") (string-append "[\""
                                                 (assoc-ref inputs "xrandr")
                                                 "/bin/xrandr")))
               #t))
           (add-after 'install 'install-contrib
             (lambda* (#:key outputs #:allow-other-keys)
               (invoke "make"
                       (string-append "DESTDIR=" (assoc-ref outputs "out"))
                       "PREFIX="
                       "BASH_COMPLETIONS_DIR=etc/bash_completiond.d"
                       "install_manpage"
                       "install_bash_completion"
                       "install_launcher"))))))
      (synopsis "Auto-detect connected displays and load appropiate setup")
      (description "Autorandr wraps around xrandr to help with X11
multi-screen configuration management.  It allows the user to create profiles
for various multi-screen setups.  Autorandr automatically detects the profiles
that can be activated based on the connected hardware.  Hook scripts can be
used to further tweak the behaviour of the different profiles.")
      (license license:gpl3+))))

(define-public xclip
  (package
    (name "xclip")
    (version "0.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/astrand/xclip.git")
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
    (version "0.8.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://xkbcommon.org/download/" name "-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "136mdq11lrwg6rjmm44lmysxxgb9c35p4sq6k0cd129x82rw9f3s"))))
    (build-system gnu-build-system)
    (inputs
     `(("libx11" ,libx11)
       ("libxcb" ,libxcb)
       ("xkeyboard-config" ,xkeyboard-config)))
    (native-inputs
     `(("bison" ,bison)
       ("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-xkb-config-root="
                            (assoc-ref %build-inputs "xkeyboard-config")
                            "/share/X11/xkb")
             (string-append "--with-x-locale-root="
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
                                "See 'COPYING' in the distribution."))))

(define-public libfakekey
  (package
    (name "libfakekey")
    (version "0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://downloads.yoctoproject.org/releases"
                            "/matchbox/libfakekey/" version "/libfakekey-"
                            version ".tar.bz2"))
        (sha256
         (base32
          "1501l0bflcrhqbf12n7a7cqilvr0w4xawxw0vw75p2940nkl4464"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "AM_LDFLAGS=-lX11")))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
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
    (home-page "http://www.semicomplete.com/projects/xdotool")
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
    (version "0.36.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.cairographics.org/releases/pixman-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1blzrx50ssdv0pn56hcv2v0zw0vrjwj1sx22pkgjls1p9n6rr88w"))
              (patches (search-patches "pixman-CVE-2016-5296.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("libpng" ,libpng)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.pixman.org/")
    (synopsis "Low-level pixel manipulation library")
    (description "Pixman is a low-level software library for pixel
manipulation, providing features such as image compositing and trapezoid
rasterisation.")
    (license license:x11)))

(define-public libdrm
  (package
    (name "libdrm")
    (version "2.4.96")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://dri.freedesktop.org/libdrm/libdrm-"
               version
               ".tar.bz2"))
        (sha256
         (base32
          "14xkip83qgljjaahzq40qgl60j54q7k00la1hbf5kk5lgg7ilmhd"))
        (patches (search-patches "libdrm-symbol-check.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       '(,@(match (%current-system)
             ("armhf-linux"
              '("--enable-exynos-experimental-api"
                "--enable-omap-experimental-api"
                "--enable-etnaviv-experimental-api"
                "--enable-tegra-experimental-api"
                "--enable-freedreno-kgsl"))
             ("aarch64-linux"
              '("--enable-tegra-experimental-api"
                "--enable-etnaviv-experimental-api"
                "--enable-freedreno-kgsl"))
             (_ '())))))
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
    (version "1.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "http://bitmath.org/code/mtdev/mtdev-"
               version ".tar.bz2"))
        (sha256
         (base32
          "0zxs7shzgbalkvlaiibi25bd902rbmkv9n1lww6q8j3ri9qdaxv6"))))
    (build-system gnu-build-system)
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
     "Wmctrl allows to interact with an X window manager that is compatible
with the EWMH/NetWM specification.  It can query the window manager for
information, and request for certain window management actions (resize and
move windows, switch between desktops, etc.).")
    (license license:gpl2+)))

(define-public scrot
  (package
    (name "scrot")
    (version "0.9")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/resurrecting-open-source-projects/scrot.git")
         (commit version)))
       (sha256
        (base32 "1dg0pnmk09p7zlbyxv7d40vf54amrv73y976ds5p7096x6lmlndy"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs
     `(("giblib" ,giblib)
       ("libx11" ,libx11)))
    (home-page "https://github.com/resurrecting-open-source-projects/scrot")
    (synopsis "Command-line screen capture utility for X Window System")
    (description
     "Scrot allows to save a screenshot of a full screen, a window or a part
of the screen selected by mouse.")
    ;; This license removes a clause about X Consortium from the original
    ;; X11 license.
    (license (license:x11-style "file://COPYING"
                                "See 'COPYING' in the distribution."))))

(define-public slop
  (package
    (name "slop")
    (version "7.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/naelstrof/slop.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fgd8a2dqkg64all0f96sca92sdss9r3pzmv5kck46b99z2325z6"))))
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
    (version "5.5.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/naelstrof/maim.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14mfxdm39kc5jk8wysrzx05ag2g4sk9l24i8m5pzqn8j611150v3"))))
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
images inside of images and reports the coordinates, allowing progams
to find buttons, etc, on the screen to click on.")
    (home-page "https://www.hoopajoo.net/projects/xautomation.html")
    (license license:gpl2+)))

(define-public xbanish
  (package
    (name "xbanish")
    (version "1.6")
    (home-page "https://github.com/jcs/xbanish")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (sha256
               (base32
                "0vp8ja68hpmqkl61zyjar3czhmny1hbm74m8f393incfz1ymr3i8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no tests
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
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
    (version "5.56")
    (source (origin
             (method url-fetch)
             (uri (list (string-append "http://sillycycle.com/xlock/"
                                       name "-" version ".tar.xz")
                        ;; Previous releases are moved to a subdirectory.
                        (string-append "http://sillycycle.com/xlock/"
                                       "recent-releases/"
                                       name "-" version ".tar.xz")))
             (sha256
              (base32
               "1dg1n79rnswhxqz36mxnl5lp8p37i9fbibnzzyrqknmvf2s8xpd0"))))
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
    (home-page "http://sillycycle.com/xlockmore.html")
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

(define-public xbindkeys
  (package
    (name "xbindkeys")
    (version "1.8.6")
    (source (origin
              (method url-fetch)
              ;; Download from the savannah mirror list fails
              (uri (string-append
                    "http://www.nongnu.org/xbindkeys/xbindkeys-"
                    version
                    ".tar.gz"))
              (sha256
               (base32
                "060df6d8y727jp1inp7blp44cs8a7jig7vcm8ndsn6gw36z1h3bc"))))
    (build-system gnu-build-system)
    (inputs
     `(("libx11" ,libx11)
       ("guile" ,guile-2.0)))
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
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/baskerville/sxhkd")
             (commit version)))
       (sha256
        (base32 "1cz4vkm7fqd51ly9qjkf5q76kdqdzfhaajgvrs4anz5dyzrdpw68"))))
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
     `(#:make-flags (list (string-append "TERMINFO="
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
                    (url "https://github.com/alols/xcape.git")
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
     "This utility for X.org allows to use modifier key as another key when
pressed and released on its own.  The default behaviour is to generate the
Escape key when Left Control is pressed and released on its own.")
    (license license:gpl3+)))

(define-public libwacom
  (package
    (name "libwacom")
    (version "0.32")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/linuxwacom/libwacom/releases/download/"
                    "libwacom-" version "/libwacom-" version ".tar.bz2"))
              (sha256
               (base32
                "102kz0q7i0bjsnl6yy83vcj2rpir12rs2d4xr0wvhw84rs5sp7bb"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
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
    (version "0.36.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/linuxwacom/xf86-input-wacom/releases/download/"
             name "-" version "/" name "-" version ".tar.bz2"))
       (sha256
        (base32
         "029y8varbricba2dzhzhy0ndd7lbfif411ca8c3wxzni9qmbj1ij"))))
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
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("libdrm" ,libdrm)
       ("libx11" ,libx11)
       ("libxcb" ,libxcb)
       ("libxxf86vm" ,libxxf86vm)
       ("glib" ,glib)))                 ; for Geoclue2 support
    (home-page "https://github.com/jonls/redshift")
    (synopsis "Adjust the color temperature of your screen")
    (description
     "Redshift adjusts the color temperature according to the position of the
sun.  A different color temperature is set during night and daytime.  During
twilight and early morning, the color temperature transitions smoothly from
night to daytime temperature to allow your eyes to slowly adapt.  At night the
color temperature should be set to match the lamps in your room.")
    (license license:gpl3+)))

(define-public xscreensaver
  (package
    (name "xscreensaver")
    (version "5.42")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.jwz.org/xscreensaver/xscreensaver-"
                       version ".tar.gz"))
       (sha256
        (base32
         "1qfbsnj7201d03vf0b2lzxmlcq4kvkvzp48r5gcgsjr17c1sl7a4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ; no check target
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
       ;; FIXME: Remove CFLAGS once our default compiler is GCC6 or later.
       #:make-flags (list "CFLAGS=-std=c99"
                          (string-append "AD_DIR="
                                         (assoc-ref %outputs "out")
                                         "/usr/lib/X11/app-defaults"))))
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
       ("libjpeg" ,libjpeg)
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
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/DaveDavenport/rofi/"
                                  "releases/download/"
                                  version "/rofi-" version ".tar.xz"))
              (sha256
               (base32
                "17faa0rj8vqidrijwx9jrq0c29003n8v3izvc66yssfljgb8kcpj"))))
    (build-system gnu-build-system)
    (inputs
     `(("pango" ,pango)
       ("cairo" ,cairo)
       ("glib" ,glib)
       ("startup-notification" ,startup-notification)
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
     `(#:parallel-tests? #f ; May fail in some circumstances.
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
                    (url "https://github.com/robm/dzen.git")
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
                    (url "https://github.com/dusxmt/nxbelld.git")
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
              (uri (string-append "http://www.ibiblio.org/pub/linux/X11/screensavers/"
                                  name "-" version ".tgz"))
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
    (home-page "http://ibiblio.org/pub/Linux/X11/screensavers/")
    (synopsis "Program launcher for idle X sessions")
    (description "Xautolock monitors input devices under the X Window
System, and launches a program of your choice if there is no activity after
a user-configurable period of time.")
    (license license:gpl2)))

(define-public screen-message
  (package
    (name "screen-message")
    (version "0.25")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.joachim-breitner.de/archive/screen-message"
                    "/screen-message-" version ".tar.gz"))
              (sha256
               (base32
                "1lw955qq5pq010lzmaf32ylj2iprgsri9ih4hx672c3f794ilab0"))))
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
user-defined screensaver.  In effect this allows to automatically lock the
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
                    (url "https://github.com/zoltanp/xrandr-invert-colors.git")
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
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.umaxx.net/dl/sct-"
                       version ".tar.gz"))
       (sha256
        (base32
         "0r57z9ki8pvxhawfxys0v5h85z2x211sqxki0xvk1bga88ryldlv"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list "CC=gcc")
       #:tests? #f ; No tests exist.
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

(define-public wl-clipboard
  (package
    (name "wl-clipboard")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bugaevc/wl-clipboard.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03h6ajcc30w6928bkd4h6xfj4iy2359ww6hdlybq8mr1zwmb2h0q"))))
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
