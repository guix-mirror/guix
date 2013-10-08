;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages qt)
  #:use-module ((guix licenses) #:select (lgpl2.1 x11-style))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages libjpeg)
  #:use-module (gnu packages libpng)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mysql)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
;;   #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg))

(define-public libxkbcommon
  (package
    (name "libxkbcommon")
    (version "0.3.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://xkbcommon.org/download/" name "-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "13mk335r4dhi9qglzbp46ina1wz4qgcp8r7s06iq7j50pf0kb5ww"))))
    (build-system gnu-build-system)
    (inputs
     `(("bison" ,bison)))
    (home-page "http://xkbcommon.org/")
    (synopsis "library to handle keyboard descriptions")
    (description "Xkbcommon is a library to handle keyboard descriptions,
including loading them from disk, parsing them and handling their
state.  It is mainly meant for client toolkits, window systems, and other
system applications; currently that includes Wayland, kmscon, GTK+, Qt,
Clutter, and more.  Despite the name, it is not currently used by anything
X11 (yet).")
    (license (x11-style "file://COPYING"
                        "See 'COPYING' in the distribution."))))

(define-public qt
  (package
    (name "qt")
    (version "5.1.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.qt-project.org/official_releases/qt/"
                                 (string-copy version 0 (string-rindex version #\.))
                                 "/" version
                                 "/single/qt-everywhere-opensource-src-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1xl6n4ai0yfknaiawgyr0cyixk0d2j4262k13mmyj993nlnp81ac"))))
    (build-system gnu-build-system)
    (inputs
     ;; FIXME: Add input ruby once available.
     `(("alsa-lib" ,alsa-lib)
       ("dbus" ,dbus)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glib" ,glib)
       ("icu4c" ,icu4c)
       ;; FIXME: Switch to current libjpeg with qt 5.1.2, see
       ;; https://qt.gitorious.org/qt/qt/commit/2a9ea11f4dea51f9e75036aab8e7a23f0eb4bd1f/diffs
       ("libjpeg" ,libjpeg-8)
       ("libpng" ,libpng)
       ("libx11" ,libx11)
       ("libxi" ,libxi)
       ("libxkbcommon" ,libxkbcommon)
       ("libxrender" ,libxrender)
       ("mesa" ,mesa)
       ("mysql" ,mysql)
       ("openssl" ,openssl)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
;;        ("pulseaudio" ,pulseaudio)
       ("python-wrapper" ,python-wrapper)
       ("xcb-util" ,xcb-util)
       ("xcb-util-image" ,xcb-util-image)
       ("xcb-util-keysyms" ,xcb-util-keysyms)
       ("xcb-util-renderutil" ,xcb-util-renderutil)
       ("xcb-util-wm" ,xcb-util-wm)
       ("zlib" ,zlib)))
    (arguments
     `(#:phases
         (alist-replace
          'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (substitute* '("configure" "qtbase/configure")
                           (("/bin/pwd") (which "pwd")))
              ;; do not pass "--enable-fast-install", which makes the
              ;; configure process fail
              (zero? (system* "./configure"
                              "-verbose"
                              "-prefix" out
                              "-opensource"
                              "-confirm-license"))))
          %standard-phases)))
    (home-page "http://qt-project.org/")
    (synopsis "Cross-platform GUI library")
    (description "Qt is a cross-platform application and UI framework for
developers using C++ or QML, a CSS & JavaScript like language.")
    (license lgpl2.1)))
