;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages gnustep)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages libffcall)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (ice-9 match))

(define-public gnustep-make
  (package
    (name "gnustep-make")
    (version "2.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://ftp.gnustep.org/pub/gnustep/core/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1khiygfkz0zhh9b5nybn40g0xnnjxchk24n49hff1bwanszir84h"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f)) ; no check target
    (native-inputs
     (list which))
    (home-page "http://gnustep.org")
    (synopsis "GNUstep make package")
    (description "The makefile package is a simple, powerful and extensible way
to write makefiles for a GNUstep-based project.  It allows the user to write a
project without having to deal with the complex issues associated with
configuration, building, installation, and packaging.  It also allows the user
to easily create cross-compiled binaries.")
    (license gpl3+)))

(define-public windowmaker
  (package
    (name "windowmaker")
    (version "0.95.9")
    (synopsis "NeXTSTEP-like window manager")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/window-maker/wmaker/releases/download/"
                    "wmaker-" version "/WindowMaker-" version ".tar.gz"))
              (sha256
               (base32
                "055pqvlkhipyjn7m6bb3fs4zz9rd1ynzl0mmwbhp05ihc3zmh8zj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 match))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; 'wmaker' wants to invoke 'wmaker.inst' the first time,
             ;; and the 'wmsetbg', so make sure it uses the right ones.
             ;; We can't use a wrapper here because that would pollute
             ;; $PATH in the whole session.
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (substitute* "src/main.c"
                 (("\"wmaker\\.inst")
                  (string-append "\"" bin "/wmaker.inst")))
               (substitute* '("src/defaults.c" "WPrefs.app/Menu.c")
                 (("\"wmsetbg")
                  (string-append "\"" bin "/wmsetbg")))
               ;; Add enough cells to the command character array to
               ;; allow passing our large path to the wmsetbg binary.
               ;; The path to wmsetbg in Guix requires 67 extra characters.
               (substitute* "src/defaults.c"
                 (("len = strlen\\(text\\) \\+ 40;")
                  (string-append "len = strlen(text) + 107;")))
               #t)))
         (add-after 'install 'install-xsession
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (xsessions (string-append out "/share/xsessions")))
               (mkdir-p xsessions)
               (call-with-output-file
                   (string-append xsessions "/windowmaker.desktop")
                 (lambda (port)
                   (format port "~
                    [Desktop Entry]~@
                    Name=Window Maker~@
                    Comment=~a~@
                    Exec=~a/bin/wmaker~@
                    Type=Application~%"
                           (string-map (match-lambda
                                         (#\newline #\space)
                                         (chr chr))
                                       ,synopsis) out))))
             #t))
         (add-after 'install-xsession 'wrap
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               ;; In turn, 'wmaker.inst' wants to invoke 'wmmenugen'
               ;; etc., so make sure everything is in $PATH.
               (wrap-program (string-append bin "/wmaker.inst")
                 `("PATH" ":" prefix (,bin)))
               #t))))))
    (inputs
     `(("libxmu" ,libxmu)
       ("libxft" ,libxft)
       ("libx11" ,libx11)
       ("libxinerama" ,libxinerama)
       ("fontconfig" ,fontconfig)
       ("libjpeg" ,libjpeg-turbo)
       ("giflib" ,giflib)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)))
    (native-inputs
     (list pkg-config))
    (home-page "https://windowmaker.org/")
    (description
     "Window Maker is an X11 window manager originally designed to provide
integration support for the GNUstep Desktop Environment.  In every way
possible, it reproduces the elegant look and feel of the NeXTSTEP user
interface.  It is fast, feature rich, easy to configure, and easy to use.")

    ;; Artwork is distributed under the WTFPL.
    (license gpl2+)))

(define-public wmbattery
  (package
    (name "wmbattery")
    (version "2.54")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://debian/pool/main/w/wmbattery/wmbattery_"
                    version ".orig.tar.gz"))
              (sha256
               (base32
                "1r4n58mwkm69y1pjs7l64hg8r1lpndrzyrfl2rdgd4zi6v0jhyyw"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f)) ; no "check" target
    (inputs
     (list glib libx11 libxext libxpm upower))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.dockapps.net/wmbattery")
    (synopsis "Display laptop battery info")
    (description
     "Wmbattery displays the status of your laptop's battery in a small icon.
This includes if it is plugged in, if the battery is charging, how many minutes
of battery life remain, battery life remaining (with both a percentage and a
graph), and battery status (high - green, low - yellow, or critical - red).")
    (license gpl2)))

(define-public wmnd
  (package
    (name "wmnd")
    (version "0.4.18")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.thregr.org/~wavexx/software/wmnd/releases/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "01s37d8cfpncza1mlw13ar4rcwbrc1vgaj3ifhglmlcnzvvayg0n"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11 libxext libxpm))
    (native-inputs
     (list pkg-config))
    (home-page "http://www.thregr.org/~wavexx/software/wmnd/")
    (synopsis "Network interface monitor")
    (description
     "WMND is a dockapp for monitoring network interfaces under WindowMaker and
other compatible window managers.")
    (license gpl2+)))

(define-public wmcpuload
  (package
    (name "wmcpuload")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://debian/pool/main/w/wmcpuload/"
                    name "_" version ".orig.tar.gz"))
              (sha256
               (base32
                "1334y0axnxydwv05d172f405iljrfakg4kcyg9kmn46v6ywv424g"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11 libxext libxpm))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.dockapps.net/wmcpuload")
    (synopsis "Monitor CPU usage")
    (description
     "Wmcpuload displays the current CPU usage, expressed as a percentile and a
chart, and has an LCD look-alike user interface.  The back-light may be turned
on and off by clicking the mouse button over the application.  If the CPU usage
hits a certain threshold, an alarm-mode will alert you by turning back-light
on.")
    (license gpl2+)))

(define-public wmclock
  (package
    (name "wmclock")
    (version "1.0.16")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://debian/pool/main/w/wmclock/"
                    name "_" version ".orig.tar.gz"))
              (sha256
               (base32
                "1lx276ba8r2yydhmwj1g586jdqg695ad89ng36fr3mb067gvb2rz"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11 libxext libxpm))
    ;; wmclock requires autoreconf to generate its configure script.
    (native-inputs
     (list autoconf automake pkg-config))
    (home-page "https://www.dockapps.net/wmclock")
    (synopsis "Display the date and time")
    (description
     "wmclock is an applet for Window Maker which displays the date and time in
a dockable tile.  It features multiple language support, 24h or 12h time
display, and can run a user-specified program on mouse click.")
    (license gpl2+)))

(define-public wmfire
  (package
    (name "wmfire")
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.improbability.net/"
                                  name "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "101grahd80n97y2dczb629clmcgiavdpbbwy78kk5wgs362m12z3"))
              (patches
               (search-patches "wmfire-update-for-new-gdk-versions.patch"))))
    (build-system gnu-build-system)
    (inputs
     (list gtk+-2 libgtop))
    (native-inputs
     (list pkg-config))
    (home-page "http://www.improbability.net/")
    (synopsis "Display flames to represent resource usage")
    (description
     "wmfire is an applet for Window Maker that can monitor the average cpu
load, or individual cpu load on SMP computers.  Additionally it can monitor the
memory, network load, a file or just be set to show a pretty flame.  On
entering the dock a burning spot replaces the cursor, and after two seconds
symbols to represent the current monitor are \"burnt\" onscreen.  The flame
colour can also be changed.")
    (license gpl2+)))
