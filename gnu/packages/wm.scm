;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2015 Siniša Biđin <sinisa@bidin.eu>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages wm)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (gnu packages linux)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages base)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages asciidoc)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages web)
  #:use-module (guix download)
  #:use-module (guix git-download))

(define-public libconfuse
  (package
    (name "libconfuse")
    (version "2.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://savannah.nongnu.org/download/confuse/"
                                  "confuse-" version ".tar.gz"))
              (sha256
               (base32
                "0y47r2ashz44wvnxdb18ivpmj8nxhw3y9bf7v9w0g5byhgyp89g3"))))
    (build-system gnu-build-system)
    (home-page "http://www.nongnu.org/confuse/")
    (synopsis "Configuration file parser library")
    (description "libconfuse is a configuration file parser library.  It
supports sections and (lists of) values (strings, integers, floats, booleans
or other sections), as well as some other features (such as
single/double-quoted strings, environment variable expansion, functions and
nested include statements).")
    (license isc)))

(define-public i3status
  (package
    (name "i3status")
    (version "2.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://i3wm.org/i3status/i3status-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1qwxbrga2fi5wf742hh9ajwa8b2kpzkjjnhjlz4wlpv21i80kss2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CC=gcc" (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:tests? #f)) ; no test suite
    (inputs
     `(("openlibm" ,openlibm)
       ("libconfuse" ,libconfuse)
       ("libyajl" ,libyajl)
       ("alsa-lib" ,alsa-lib)
       ("wireless-tools" ,wireless-tools)
       ("libcap" ,libcap)
       ("asciidoc" ,asciidoc)))
    (home-page "http://i3wm.org/i3status/")
    (synopsis "Status bar for i3bar, dzen2, xmobar or similar programs")
    (description "i3status is a small program for generating a status bar for
i3bar, dzen2, xmobar or similar programs.  It is designed to be very efficient
by issuing a very small number of system calls, as one generally wants to
update such a status line every second.  This ensures that even under high
load, your status bar is updated correctly.  Also, it saves a bit of energy by
not hogging your CPU as much as spawning the corresponding amount of shell
commands would.")
    (license bsd-3)))

(define-public i3-wm
  (package
    (name "i3-wm")
    (version "4.10.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://i3wm.org/downloads/i3-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1lq7h4w7m0hi31iva8g7yf1sc11ispnknxjdaj9agld4smxqb44j"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CC=gcc" (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:tests? #f)) ; no test suite
    (inputs
     `(("libxcb" ,libxcb)
       ("xcb-util" ,xcb-util)
       ("xcb-util-cursor" ,xcb-util-cursor)
       ("xcb-util-keysyms" ,xcb-util-keysyms)
       ("xcb-util-wm" ,xcb-util-wm)
       ("libxkbcommon" ,libxkbcommon)
       ("libev" ,libev)
       ("libyajl" ,libyajl)
       ("asciidoc" ,asciidoc)
       ("xmlto" ,xmlto)
       ("perl-pod-simple" ,perl-pod-simple)
       ("docbook-xml" ,docbook-xml)
       ("libx11" ,libx11)
       ("pcre" ,pcre)
       ("startup-notification" ,startup-notification)
       ("pango" ,pango)
       ("cairo" ,cairo)))
    (native-inputs
     `(("which" ,which)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (home-page "http://i3wm.org/")
    (synopsis "Improved tiling window manager")
    (description "i3 is a tiling window manager, completely written
from scratch.  i3 is primarily targeted at advanced users and
developers.")
    (license bsd-3)))

(define-public xmonad
  (package
    (name "xmonad")
    (version "0.11.1")
    (synopsis "Tiling window manager")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://hackage.haskell.org/package/xmonad/"
                                  "xmonad-" version ".tar.gz"))
              (sha256
               (base32
                "1pfjssamiwpwjp1qqkm9m9p9s35pv381m0cwg6jxg0ppglibzq1r"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-extensible-exceptions" ,ghc-extensible-exceptions)
       ("ghc-x11" ,ghc-x11)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after
          'install 'install-xsession
          (lambda _
            (let* ((xsessions (string-append %output "/share/xsessions")))
              (mkdir-p xsessions)
              (call-with-output-file
                  (string-append xsessions "/xmonad.desktop")
                (lambda (port)
                  (format port "~
                    [Desktop Entry]~@
                    Name=~a~@
                    Comment=~a~@
                    Exec=~a/bin/xmonad~@
                    Type=Application~%" ,name ,synopsis %output)))))))))
    (home-page "http://xmonad.org")
    (description
     "Xmonad is a tiling window manager for X.  Windows are arranged
automatically to tile the screen without gaps or overlap, maximising screen
use.  All features of the window manager are accessible from the keyboard: a
mouse is strictly optional.  Xmonad is written and extensible in Haskell.
Custom layout algorithms, and other extensions, may be written by the user in
config files.  Layouts are applied dynamically, and different layouts may be
used on each workspace.  Xinerama is fully supported, allowing windows to be
tiled on several screens.")
    (license bsd-3)))
