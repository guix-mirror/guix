;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module ((guix licenses) #:select (gpl2 gpl3 lgpl2.1 x11-style))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml))

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
    (version "5.4.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.qt-project.org/official_releases/qt/"
                                 (string-copy version 0 (string-rindex version #\.))
                                 "/" version
                                 "/single/qt-everywhere-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "09gay5cimfdb0apy60v7z4r4zkl2vjysdppzihpla8dp2c30fvcc"))
             (patches (list (search-patch "qt5-conflicting-typedefs.patch")
                            (search-patch "qt5-runpath.patch")))
             (snippet
              '(begin
                 ;; Remove broken symlinks.
                 (delete-file "qtwebengine/src/3rdparty/chromium/third_party/\
mesa/src/src/gallium/state_trackers/d3d1x/w32api")
                 (delete-file "qtwebengine/src/3rdparty/chromium/third_party/\
webrtc/tools/e2e_quality/audio/perf")))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("mesa" ,mesa)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("dbus" ,dbus)
       ("cups" ,cups)
       ("expat" ,expat)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glib" ,glib)
       ("icu4c" ,icu4c)
       ("libjpeg" ,libjpeg)
       ("libmng" ,libmng)
       ("libpci" ,pciutils)
       ("libpng" ,libpng)
       ("libx11" ,libx11)
       ("libxcomposite" ,libxcomposite)
       ("libxcursor" ,libxcursor)
       ("libxfixes" ,libxfixes)
       ("libxi" ,libxi)
       ("libxinerama" ,libxinerama)
       ("libxkbcommon" ,libxkbcommon)
       ("libxml2" ,libxml2)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxslt" ,libxslt)
       ("libxtst" ,libxtst)
       ("mtdev" ,mtdev)
       ("mysql" ,mysql)
       ("nss" ,nss)
       ("openssl" ,openssl)
       ("postgresql" ,postgresql)
       ("pulseaudio" ,pulseaudio)
       ("pcre" ,pcre)
       ("sqlite" ,sqlite)
       ("udev" ,eudev)
       ("unixodbc" ,unixodbc)
       ("xcb-util" ,xcb-util)
       ("xcb-util-image" ,xcb-util-image)
       ("xcb-util-keysyms" ,xcb-util-keysyms)
       ("xcb-util-renderutil" ,xcb-util-renderutil)
       ("xcb-util-wm" ,xcb-util-wm)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("gperf" ,gperf)
       ("ninja" ,ninja)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("ruby" ,ruby)
       ("which" ,(@ (gnu packages base) which))))
    (arguments
     `(#:phases
         (alist-replace
          'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (substitute* '("configure" "qtbase/configure")
                (("/bin/pwd") (which "pwd")))
              (substitute* "qtbase/src/corelib/global/global.pri"
                (("/bin/ls") (which "ls")))
              (substitute* "qtwebengine/src/3rdparty/chromium/build/common.gypi"
                (("/bin/echo") (which "echo")))
              (substitute* "qtwebengine/src/3rdparty/chromium/third_party/\
WebKit/Source/build/scripts/scripts.gypi"
                (("/usr/bin/gcc") (which "gcc")))
              (setenv "NINJA_PATH" (which "ninja"))
              ;; do not pass "--enable-fast-install", which makes the
              ;; configure process fail
              (zero? (system*
                      "./configure"
                      "-verbose"
                      "-prefix" out
                      "-opensource"
                      "-confirm-license"
                      "-system-sqlite"
                      ;; explicitly link with openssl instead of dlopening it
                      "-openssl-linked"
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
    (version "4.8.7")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.qt-project.org/official_releases/qt/"
                                 (string-copy version 0 (string-rindex version #\.))
                                 "/" version
                                 "/qt-everywhere-opensource-src-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "183fca7n7439nlhxyg1z7aky0izgbyll3iwakw4gwivy16aj5272"))
             (patches (map search-patch
                           '("qt4-ldflags.patch" "qt4-tests.patch")))))
    (inputs `(,@(alist-delete "libjpeg" (package-inputs qt))
              ("libjepg" ,libjpeg-8)
              ("libsm" ,libsm)))

    ;; Note: there are 37 MiB of examples and a '-exampledir' configure flags,
    ;; but we can't make them a separate output because "out" and "examples"
    ;; would refer to each other.
    (outputs '("out"                             ;112MiB core + 37MiB examples
               "doc"))                           ;280MiB of HTML + code
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace
          'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (doc (assoc-ref outputs "doc")))
              (substitute* '("configure")
                (("/bin/pwd") (which "pwd")))

              (zero? (system*
                      "./configure"
                      "-verbose"
                      "-prefix" out
                      ;; Note: Don't pass '-docdir' since 'qmake' and
                      ;; libQtCore would record its value, thereby defeating
                      ;; the whole point of having a separate output.
                      "-datadir" (string-append out "/share/qt-" ,version
                                                "/data")
                      "-importdir" (string-append out "/lib/qt-4"
                                                  "/imports")
                      "-plugindir" (string-append out "/lib/qt-4"
                                                  "/plugins")
                      "-translationdir" (string-append out "/share/qt-" ,version
                                                       "/translations")
                      "-demosdir"    (string-append out "/share/qt-" ,version
                                                    "/demos")
                      "-examplesdir" (string-append out "/share/qt-" ,version
                                                    "/examples")
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
                      "-no-neon")))))
         (add-after
          'install 'move-doc
          (lambda* (#:key outputs #:allow-other-keys)
            ;; Because of qt4-documentation-path.patch, documentation ends up
            ;; being installed in OUT.  Move it to the right place.
            (let* ((out    (assoc-ref outputs "out"))
                   (doc    (assoc-ref outputs "doc"))
                   (olddoc (string-append out "/doc"))
                   (docdir (string-append doc "/share/doc/qt-" ,version)))
              (mkdir-p (dirname docdir))

              ;; Note: We can't use 'rename-file' here because OUT and DOC are
              ;; different "devices" due to bind-mounts.
              (copy-recursively olddoc docdir)
              (delete-file-recursively olddoc)
              #t))))))))

(define-public python-sip
  (package
    (name "python-sip")
    (version "4.16.5")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append "mirror://sourceforge/pyqt/sip/"
                         "sip-" version "/sip-"
                         version ".tar.gz"))
        (sha256
         (base32
          "11qy1z88py2q7rz68rm7214pbd37538hpcbfj5hhzp5y616a62x0"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python-wrapper)))
    (arguments
     `(#:tests? #f ; no check target
       #:phases
         (alist-replace
          'configure
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (bin (string-append out "/bin"))
                   (include (string-append out "/include"))
                   (python-version
                     (string-take
                       (string-take-right (assoc-ref inputs "python") 5)
                       3))
                   (lib (string-append out "/lib/python"
                                       python-version
                                       "/site-packages")))
              (zero?
                (system* "python" "configure.py"
                         "--bindir" bin
                          "--destdir" lib
                         "--incdir" include))))
          %standard-phases)))
    (home-page "http://www.riverbankcomputing.com/software/sip/intro")
    (synopsis "Python binding creator for C and C++ libraries")
    (description
     "SIP is a tool to create Python bindings for C and C++ libraries.  It
was originally developed to create PyQt, the Python bindings for the Qt
toolkit, but can be used to create bindings for any C or C++ library.

SIP comprises a code generator and a Python module.  The code generator
processes a set of specification files and generates C or C++ code, which
is then compiled to create the bindings extension module.  The SIP Python
module provides support functions to the automatically generated code.")
    ;; There is a choice between a python like license, gpl2 and gpl3.
    ;; For compatibility with pyqt, we need gpl3.
    (license gpl3)))

(define-public python2-sip
  (package (inherit python-sip)
    (name "python2-sip")
    (native-inputs
     `(("python" ,python-2)))))

(define-public python-pyqt
  (package
    (name "python-pyqt")
    (version "5.4")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append "mirror://sourceforge/pyqt/PyQt5/"
                         "PyQt-" version "/PyQt-gpl-"
                         version ".tar.gz"))
        (sha256
         (base32
          "0cbpa63whi8a5akff4pcnfwzpzx7ycac2ynj00ly52m6zbsn80kn"))
       (patches (list (search-patch "pyqt-configure.patch")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python-sip" ,python-sip)
       ("qt" ,qt))) ; for qmake
    (inputs
     `(("python" ,python-wrapper)))
    (arguments
     `(#:phases
         (alist-replace
         'configure
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let* ((out (assoc-ref outputs "out"))
                  (bin (string-append out "/bin"))
                  (sip (string-append out "/share/sip"))
                  (plugins (string-append out "/plugins"))
                  (designer (string-append plugins "/designer"))
                  (qml (string-append plugins "/PyQt5"))
                  (python-version
                    (string-take
                      (string-take-right (assoc-ref inputs "python") 5)
                      3))
                  (lib (string-append out "/lib/python"
                                      python-version
                                      "/site-packages")))
             (zero? (system* "python" "configure.py"
                             "--confirm-license"
                             "--bindir" bin
                             "--destdir" lib
                             "--designer-plugindir" designer
                             "--qml-plugindir" qml
                             "--sipdir" sip))))
         %standard-phases)))
    (home-page "http://www.riverbankcomputing.com/software/pyqt/intro")
    (synopsis "Python bindings for Qt")
    (description
     "PyQt is a set of Python v2 and v3 bindings for the Qt application
framework.  The bindings are implemented as a set of Python modules and
contain over 620 classes.")
    (license gpl3)))

(define-public python2-pyqt
  (package (inherit python-pyqt)
    (name "python2-pyqt")
    (native-inputs
     `(("python-sip" ,python2-sip)
       ("qt" ,qt)))
    (inputs
     `(("python" ,python-2)))))

(define-public python-pyqt-4
  (package (inherit python-pyqt)
    (name "python-pyqt")
    (version "4.11.3")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append "mirror://sourceforge/pyqt/PyQt4/"
                         "PyQt-" version "/PyQt-x11-gpl-"
                         version ".tar.gz"))
        (sha256
         (base32
          "11jnfjw79s0b0qdd9s6kd69w87vf16dhagbhbmwbmrp2vgf80dw5"))))
    (native-inputs
     `(("python-sip" ,python-sip)
       ("qt" ,qt-4)))
    (arguments
     `(#:tests? #f ; no check target
       #:phases
         (alist-replace
         'configure
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let* ((out (assoc-ref outputs "out"))
                  (bin (string-append out "/bin"))
                  (sip (string-append out "/share/sip"))
                  (python-version
                    (string-take
                      (string-take-right (assoc-ref inputs "python") 5)
                      3))
                  (lib (string-append out "/lib/python"
                                      python-version
                                      "/site-packages")))
             (zero? (system* "python" "configure.py"
                             "--confirm-license"
                             "--bindir" bin
                             "--destdir" lib
                             "--sipdir" sip))))
         %standard-phases)))
    (license (list gpl2 gpl3)))) ; choice of either license

(define-public python2-pyqt-4
  (package (inherit python-pyqt-4)
    (name "python2-pyqt")
    (native-inputs
     `(("python-sip" ,python2-sip)
       ("qt" ,qt-4)))
    (inputs
     `(("python" ,python-2)))))
