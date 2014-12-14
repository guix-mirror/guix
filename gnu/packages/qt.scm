;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
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
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages xorg))

(define-public libxkbcommon
  (package
    (name "libxkbcommon")
    (version "0.5.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://xkbcommon.org/download/" name "-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "176ii5dn2wh74q48sd8ac37ljlvgvp5f506glr96z6ibfhj7igch"))))
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
    (home-page "http://xkbcommon.org/")
    (synopsis "Library to handle keyboard descriptions")
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
    (version "5.3.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.qt-project.org/official_releases/qt/"
                                 (string-copy version 0 (string-rindex version #\.))
                                 "/" version
                                 "/single/qt-everywhere-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1w4v53889kqpwxw45wcqz5bi6zx8xp434jpafk1vlmyb8hrzjnvz"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("mesa" ,mesa)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("dbus" ,dbus)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glib" ,glib)
       ("icu4c" ,icu4c)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libx11" ,libx11)
       ("libxi" ,libxi)
       ("libxkbcommon" ,libxkbcommon)
       ("libxrender" ,libxrender)
       ("mysql" ,mysql)
       ("openssl" ,openssl)
       ("pulseaudio" ,pulseaudio)
       ("python-wrapper" ,python-wrapper)
       ("ruby" ,ruby)
       ("xcb-util" ,xcb-util)
       ("xcb-util-image" ,xcb-util-image)
       ("xcb-util-keysyms" ,xcb-util-keysyms)
       ("xcb-util-renderutil" ,xcb-util-renderutil)
       ("xcb-util-wm" ,xcb-util-wm)
       ("zlib" ,zlib)))
    (native-inputs
      `(("perl" ,perl)
        ("pkg-config" ,pkg-config)))
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
              (zero? (system*
                      "./configure"
                      "-verbose"
                      "-prefix" out
                      "-opensource"
                      "-confirm-license"
                      ;; explicitly link with dbus instead of dlopening it
                      "-dbus-linked"
                      ;; drop special machine instructions not supported
                      ;; on all instances of the target
                      ,@(if (string-prefix? "x86_64"
                                            (or (%current-target-system)
                                                (%current-system)))
                            '()
                            '("-no-sse2"))
                      "-no-sse3"
                      "-no-ssse3"
                      "-no-sse4.1"
                      "-no-sse4.2"
                      "-no-avx"
                      "-no-avx2"
                      "-no-mips_dsp"
                      "-no-mips_dspr2"))))
          %standard-phases)))
    (home-page "http://qt-project.org/")
    (synopsis "Cross-platform GUI library")
    (description "Qt is a cross-platform application and UI framework for
developers using C++ or QML, a CSS & JavaScript like language.")
    (license lgpl2.1)

    ;; Qt 4: 'QBasicAtomicPointer' leads to build failures on MIPS;
    ;; see <http://hydra.gnu.org/build/112828>.
    ;; Qt 5: assembler error; see <http://hydra.gnu.org/build/112526>.
    (supported-systems (delete "mips64el-linux" %supported-systems))))

(define-public qt-4
  (package (inherit qt)
    (version "4.8.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.qt-project.org/official_releases/qt/"
                                 (string-copy version 0 (string-rindex version #\.))
                                 "/" version
                                 "/qt-everywhere-opensource-src-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0b036iqgmbbv37dgwwfihw3mihjbnw3kb5kaisdy0qi8nn8xs54b"))
             (patches (list (search-patch "qt4-tests.patch")))))
    (inputs `(,@(alist-delete "libjpeg" (package-inputs qt))
              ("libjepg" ,libjpeg-8)
              ("libsm" ,libsm)))
    (arguments
     `(#:phases
         (alist-replace
          'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (substitute* '("configure")
                           (("/bin/pwd") (which "pwd")))
              ;; Explicitly link with icui18n, which is dlopened by
              ;; QtCore.so. The LDFLAGS are in fact added to other flags
              ;; determined by the configure phase.
              ;; According to the nix recipe, this may be necessary for
              ;; further libraries (cups, gtk-x11-2.0, libgdk-x11-2.0).
              (setenv "LDFLAGS" "-licui18n")
              ;; do not pass "--enable-fast-install", which makes the
              ;; configure process fail
              (zero? (system*
                      "./configure"
                      "-verbose"
                      "-prefix" out
                      "-opensource"
                      "-confirm-license"
                      ;; explicitly link with dbus instead of dlopening it
                      "-dbus-linked"
                      ;; drop special machine instructions not supported
                      ;; on all instances of the target
                      ,@(if (string-prefix? "x86_64"
                                            (or (%current-target-system)
                                                (%current-system)))
                            '()
                            '("-no-mmx"
                              "-no-3dnow"
                              "-no-sse"
                              "-no-sse2"))
                      "-no-sse3"
                      "-no-ssse3"
                      "-no-sse4.1"
                      "-no-sse4.2"
                      "-no-avx"
                      "-no-neon"))))
          %standard-phases)))))
