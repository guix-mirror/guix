;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 ng0 <ng0@n0.is>
;;; Copyright © 2016 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Quiliro <quiliro@fsfla.org>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2018 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-1))

(define-public grantlee
  (package
    (name "grantlee")
    (version "5.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/steveire/grantlee.git")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1dmah2gd6zd4fgz2f4ir11dazqg067hjz8xshhywhfsmavchi626"))))
    (native-inputs
     ;; Optional: lcov and cccc, both are for code coverage
     `(("doxygen" ,doxygen)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtscript" ,qtscript)))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://github.com/steveire/grantlee")
    (synopsis "Libraries for text templating with Qt")
    (description "Grantlee Templates can be used for theming and generation of
other text such as code.  The syntax uses the syntax of the Django template
system, and the core design of Django is reused in Grantlee.")
    (license license:lgpl2.0+)))

(define-public qt
  (package
    (name "qt")
    (version "5.11.3")
    (outputs '("out" "examples"))
    (source (origin
             (method url-fetch)
             (uri
               (string-append
                 "http://download.qt.io/official_releases/qt/"
                 (version-major+minor version)
                 "/" version
                 "/single/qt-everywhere-src-"
                 version ".tar.xz"))
             (sha256
              (base32
               "0kgzy32s1fr22fxxfhcyncfryb3qxrznlr737r4y5khk4xj1g545"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                ;; Remove qtwebengine, which relies on a bundled copy of
                ;; chromium. Not only does it fail compilation in qt 5.5:
                ;;    3rdparty/chromium/ui/gfx/codec/jpeg_codec.cc:362:10:
                ;;    error: cannot convert ‘bool’ to ‘boolean’ in return
                ;; it might also pose security problems.
                ;; Alternatively, we could use the "-skip qtwebengine"
                ;; configuration option.
                (delete-file-recursively "qtwebengine")
                ;; The following snippets are copied from their mondular-qt counterparts.
                (for-each
                  (lambda (dir)
                    (delete-file-recursively (string-append "qtbase/src/3rdparty/" dir)))
                  (list "double-conversion" "freetype" "harfbuzz-ng"
                        "libpng" "libjpeg" "pcre2" "sqlite" "xcb"
                        "xkbcommon" "zlib"))
                (for-each
                  (lambda (dir)
                    (delete-file-recursively dir))
                  (list "qtimageformats/src/3rdparty"
                        "qtmultimedia/examples/multimedia/spectrum/3rdparty"
                        "qtwayland/examples"
                        "qtscxml/tests/3rdparty"
                        "qtcanvas3d/examples/canvas3d/3rdparty"))
                ;; Tests depend on this example, which depends on the 3rd party code.
                (substitute* "qtmultimedia/examples/multimedia/multimedia.pro"
                  (("spectrum") "#"))
                (substitute* "qtxmlpatterns/tests/auto/auto.pro"
                  (("qxmlquery") "# qxmlquery")
                  (("xmlpatterns ") "# xmlpatterns"))
                (substitute* "qtwebglplugin/tests/plugins/platforms/platforms.pro"
                  (("webgl") "# webgl"))
                (substitute* "qtscxml/tests/auto/auto.pro"
                  (("scion") "#"))
                (substitute* "qtnetworkauth/tests/auto/auto.pro"
                  (("oauth1 ") "# oauth1 "))
                (substitute* "qtremoteobjects/tests/auto/qml/qml.pro"
                  (("integration") "# integration")
                  (("usertypes") "# usertypes"))
                #t))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("mesa" ,mesa)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("bluez" ,bluez)
       ("cups" ,cups)
       ("dbus" ,dbus)
       ("double-conversion" ,double-conversion)
       ("expat" ,expat)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glib" ,glib)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("harfbuzz" ,harfbuzz)
       ("icu4c" ,icu4c)
       ("jasper" ,jasper)
       ("libinput" ,libinput-minimal)
       ("libjpeg" ,libjpeg)
       ("libmng" ,libmng)
       ("libpci" ,pciutils)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("libwebp" ,libwebp)
       ("libx11" ,libx11)
       ("libxcomposite" ,libxcomposite)
       ("libxcursor" ,libxcursor)
       ("libxext" ,libxext)
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
       ("mariadb" ,mariadb)
       ("nss" ,nss)
       ("openssl" ,openssl)
       ("postgresql" ,postgresql)
       ("pulseaudio" ,pulseaudio)
       ("pcre2" ,pcre2)
       ("sqlite" ,sqlite-with-column-metadata)
       ("udev" ,eudev)
       ("unixodbc" ,unixodbc)
       ("wayland" ,wayland)
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
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("ruby" ,ruby)
       ("vulkan-headers" ,vulkan-headers)
       ("which" ,(@ (gnu packages base) which))))
    (arguments
     `(#:parallel-build? #f ; Triggers race condition in qtbase module on Hydra.
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-bin-sh
           (lambda _
             (substitute* '("qtbase/configure"
                            "qtbase/mkspecs/features/qt_functions.prf"
                            "qtbase/qmake/library/qmakebuiltins.cpp")
                          (("/bin/sh") (which "sh")))
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out      (assoc-ref outputs "out"))
                   (examples (assoc-ref outputs "examples")))
               (substitute* '("configure" "qtbase/configure")
                 (("/bin/pwd") (which "pwd")))
               (substitute* "qtbase/src/corelib/global/global.pri"
                 (("/bin/ls") (which "ls")))
               ;; do not pass "--enable-fast-install", which makes the
               ;; configure process fail
               (invoke
                 "./configure"
                 "-verbose"
                 "-prefix" out
                 "-docdir" (string-append out "/share/doc/qt5")
                 "-headerdir" (string-append out "/include/qt5")
                 "-archdatadir" (string-append out "/lib/qt5")
                 "-datadir" (string-append out "/share/qt5")
                 "-examplesdir" (string-append
                                  examples "/share/doc/qt5/examples") ; 151MiB
                 "-opensource"
                 "-confirm-license"

                 ;; These features require higher versions of Linux than the
                 ;; minimum version of the glibc.  See
                 ;; src/corelib/global/minimum-linux_p.h.  By disabling these
                 ;; features Qt5 applications can be used on the oldest
                 ;; kernels that the glibc supports, including the RHEL6
                 ;; (2.6.32) and RHEL7 (3.10) kernels.
                 "-no-feature-getentropy"  ; requires Linux 3.17
                 "-no-feature-renameat2"   ; requires Linux 3.16

                 ;; Do not build examples; for the time being, we
                 ;; prefer to save the space and build time.
                 "-no-compile-examples"
                 ;; Most "-system-..." are automatic, but some use
                 ;; the bundled copy by default.
                 "-system-sqlite"
                 "-system-harfbuzz"
                 "-system-pcre"
                 ;; explicitly link with openssl instead of dlopening it
                 "-openssl-linked"
                 ;; explicitly link with dbus instead of dlopening it
                 "-dbus-linked"
                 ;; don't use the precompiled headers
                 "-no-pch"
                 ;; drop special machine instructions not supported
                 ;; on all instances of the target
                 ,@(if (string-prefix? "x86_64"
                                       (or (%current-target-system)
                                           (%current-system)))
                       '()
                       '("-no-sse2"))
                 "-no-mips_dsp"
                 "-no-mips_dspr2"))))
           (add-after 'install 'patch-mkspecs
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (archdata (string-append out "/lib/qt5"))
                      (mkspecs (string-append archdata "/mkspecs"))
                      (qt_config.prf (string-append
                                      mkspecs "/features/qt_config.prf")))
                 ;; For each Qt module, let `qmake' uses search paths in the
                 ;; module directory instead of all in QT_INSTALL_PREFIX.
                 (substitute* qt_config.prf
                   (("\\$\\$\\[QT_INSTALL_HEADERS\\]")
                    "$$clean_path($$replace(dir, mkspecs/modules, ../../include/qt5))")
                   (("\\$\\$\\[QT_INSTALL_LIBS\\]")
                    "$$clean_path($$replace(dir, mkspecs/modules, ../../lib))")
                   (("\\$\\$\\[QT_HOST_LIBS\\]")
                    "$$clean_path($$replace(dir, mkspecs/modules, ../../lib))")
                   (("\\$\\$\\[QT_INSTALL_BINS\\]")
                    "$$clean_path($$replace(dir, mkspecs/modules, ../../bin))"))

                 ;; Searches Qt tools in the current PATH instead of QT_HOST_BINS.
                 (substitute* (string-append mkspecs "/features/qt_functions.prf")
                   (("cmd = \\$\\$\\[QT_HOST_BINS\\]/\\$\\$2")
                    "cmd = $$system(which $${2}.pl 2>/dev/null || which $${2})"))

                 ;; Resolve qmake spec files within qtbase by absolute paths.
                 (substitute*
                     (map (lambda (file)
                            (string-append mkspecs "/features/" file))
                          '("device_config.prf" "moc.prf" "qt_build_config.prf"
                            "qt_config.prf" "winrt/package_manifest.prf"))
                   (("\\$\\$\\[QT_HOST_DATA/get\\]") archdata)
                   (("\\$\\$\\[QT_HOST_DATA/src\\]") archdata))
                 #t)))
           (add-after 'unpack 'patch-paths
             ;; Use the absolute paths for dynamically loaded libs, otherwise
             ;; the lib will be searched in LD_LIBRARY_PATH which typically is
             ;; not set in guix.
             (lambda* (#:key inputs #:allow-other-keys)
               ;; libresolve
               (let ((glibc (assoc-ref inputs ,(if (%current-target-system)
                                                   "cross-libc" "libc"))))
                 (substitute* '("qtbase/src/network/kernel/qdnslookup_unix.cpp"
                                "qtbase/src/network/kernel/qhostinfo_unix.cpp")
                   (("^\\s*(lib.setFileName\\(QLatin1String\\(\")(resolv\"\\)\\);)" _ a b)
                  (string-append a glibc "/lib/lib" b))))
               ;; X11/locale (compose path)
               (substitute* "qtbase/src/plugins/platforminputcontexts/compose/generator/qtablegenerator.cpp"
                 ;; Don't search in /usr/…/X11/locale, …
                 (("^\\s*m_possibleLocations.append\\(QStringLiteral\\(\"/usr/.*/X11/locale\"\\)\\);" line)
                  (string-append "// " line))
                 ;; … but use libx11's path
                 (("^\\s*(m_possibleLocations.append\\(QStringLiteral\\()X11_PREFIX \"(/.*/X11/locale\"\\)\\);)" _ a b)
                  (string-append a "\"" (assoc-ref inputs "libx11") b)))
               ;; libGL
               (substitute* "qtbase/src/plugins/platforms/xcb/gl_integrations/xcb_glx/qglxintegration.cpp"
                 (("^\\s*(QLibrary lib\\(QLatin1String\\(\")(GL\"\\)\\);)" _ a b)
                  (string-append a (assoc-ref inputs "mesa") "/lib/lib" b)))
               ;; libXcursor
               (substitute* "qtbase/src/plugins/platforms/xcb/qxcbcursor.cpp"
                 (("^\\s*(QLibrary xcursorLib\\(QLatin1String\\(\")(Xcursor\"\\), 1\\);)" _ a b)
                  (string-append a (assoc-ref inputs "libxcursor") "/lib/lib" b))
                 (("^\\s*(xcursorLib.setFileName\\(QLatin1String\\(\")(Xcursor\"\\)\\);)" _ a b)
                  (string-append a (assoc-ref inputs "libxcursor") "/lib/lib" b)))
               #t)))))
      (native-search-paths
       (list (search-path-specification
              (variable "QMAKEPATH")
              (files '("lib/qt5")))
             (search-path-specification
              (variable "QML2_IMPORT_PATH")
              (files '("lib/qt5/qml")))
             (search-path-specification
              (variable "QT_PLUGIN_PATH")
              (files '("lib/qt5/plugins")))
             (search-path-specification
              (variable "XDG_DATA_DIRS")
              (files '("share")))
             (search-path-specification
              (variable "XDG_CONFIG_DIRS")
              (files '("etc/xdg")))))
      (home-page "https://www.qt.io/")
      (synopsis "Cross-platform GUI library")
      (description "Qt is a cross-platform application and UI framework for
  developers using C++ or QML, a CSS & JavaScript like language.")
      (license (list license:lgpl2.1 license:lgpl3))

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
             (patches (search-patches "qt4-ldflags.patch"))
             (modules '((guix build utils)))
             (snippet
              ;; Remove webkit module, which is not built.
              '(begin (delete-file-recursively "src/3rdparty/webkit")
                      #t))))
    (inputs
     `(,@(fold alist-delete
               (package-inputs qt)
               '("harfbuzz" "libjpeg"))
       ("libjpeg" ,libjpeg-8)
       ("libsm" ,libsm)))
    (native-inputs
     `(,@(fold alist-delete
               (package-native-inputs qt)
               '("vulkan-headers"))))

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
              (substitute* "src/corelib/global/global.pri"
                (("/bin/ls") (which "ls")))

              (invoke
                "./configure"
                "-verbose"
                "-prefix" out
                "-nomake" "examples demos"
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
                ;; Skip the webkit module; it fails to build on armhf
                ;; and, apart from that, may pose security risks.
                "-no-webkit"
                ;; don't use the precompiled headers
                "-no-pch"
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
                "-no-avx"))))
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

(define-public qtbase
  (package
    (name "qtbase")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "071yc9iz14qs4s8yvrwllyfdzp5yjxsdpvbjxdrf0g5q69vqigy6"))
             ;; Use TZDIR to avoid depending on package "tzdata".
             (patches (search-patches "qtbase-use-TZDIR.patch"
                                      "qtbase-old-kernel.patch"))
             (modules '((guix build utils)))
             (snippet
               ;; corelib uses bundled harfbuzz, md4, md5, sha3
              '(begin
                (with-directory-excursion "src/3rdparty"
                  (for-each delete-file-recursively
                            (list "double-conversion" "freetype" "harfbuzz-ng"
                                  "libpng" "libjpeg" "pcre2" "sqlite" "xcb"
                                  "xkbcommon" "zlib"))
                  #t)))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("mesa" ,mesa)
       ("which" ,(@ (gnu packages base) which))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("cups" ,cups)
       ("dbus" ,dbus)
       ("double-conversion" ,double-conversion)
       ("eudev" ,eudev)
       ("expat" ,expat)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glib" ,glib)
       ("harfbuzz" ,harfbuzz)
       ("icu4c" ,icu4c)
       ("libinput" ,libinput-minimal)
       ("libjpeg" ,libjpeg)
       ("libmng" ,libmng)
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
       ("mariadb" ,mariadb)
       ("nss" ,nss)
       ("openssl" ,openssl)
       ("pcre2" ,pcre2)
       ("postgresql" ,postgresql)
       ("pulseaudio" ,pulseaudio)
       ("sqlite" ,sqlite-with-column-metadata)
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
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("vulkan-headers" ,vulkan-headers)
       ("ruby" ,ruby)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-bin-sh
           (lambda _
             (substitute* '("config.status"
                            "configure"
                            "mkspecs/features/qt_functions.prf"
                            "qmake/library/qmakebuiltins.cpp")
                          (("/bin/sh") (which "sh")))
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "configure"
                 (("/bin/pwd") (which "pwd")))
               (substitute* "src/corelib/global/global.pri"
                 (("/bin/ls") (which "ls")))
               ;; The configuration files for other Qt5 packages are searched
               ;; through a call to "find_package" in Qt5Config.cmake, which
               ;; disables the use of CMAKE_PREFIX_PATH via the parameter
               ;; "NO_DEFAULT_PATH". Re-enable it so that the different
               ;; components can be installed in different places.
               (substitute* (find-files "." ".*\\.cmake")
                 (("NO_DEFAULT_PATH") ""))
               ;; do not pass "--enable-fast-install", which makes the
               ;; configure process fail
               (invoke
                 "./configure"
                 "-verbose"
                 "-prefix" out
                 "-docdir" (string-append out "/share/doc/qt5")
                 "-headerdir" (string-append out "/include/qt5")
                 "-archdatadir" (string-append out "/lib/qt5")
                 "-datadir" (string-append out "/share/qt5")
                 "-examplesdir" (string-append
                                  out "/share/doc/qt5/examples")
                 "-opensource"
                 "-confirm-license"

                 ;; These features require higher versions of Linux than the
                 ;; minimum version of the glibc.  See
                 ;; src/corelib/global/minimum-linux_p.h.  By disabling these
                 ;; features Qt5 applications can be used on the oldest
                 ;; kernels that the glibc supports, including the RHEL6
                 ;; (2.6.32) and RHEL7 (3.10) kernels.
                 "-no-feature-getentropy"  ; requires Linux 3.17

                 ;; Do not build examples; if desired, these could go
                 ;; into a separate output, but for the time being, we
                 ;; prefer to save the space and build time.
                 "-no-compile-examples"
                 ;; Most "-system-..." are automatic, but some use
                 ;; the bundled copy by default.
                 "-system-sqlite"
                 "-system-harfbuzz"
                 "-system-pcre"
                 ;; explicitly link with openssl instead of dlopening it
                 "-openssl-linked"
                 ;; explicitly link with dbus instead of dlopening it
                 "-dbus-linked"
                 ;; don't use the precompiled headers
                 "-no-pch"
                 ;; drop special machine instructions that do not have
                 ;; runtime detection
                 ,@(if (string-prefix? "x86_64"
                                       (or (%current-target-system)
                                           (%current-system)))
                     '()
                     '("-no-sse2"))
                 "-no-mips_dsp"
                 "-no-mips_dspr2"))))
         (add-after 'install 'patch-mkspecs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (archdata (string-append out "/lib/qt5"))
                    (mkspecs (string-append archdata "/mkspecs"))
                    (qt_config.prf (string-append
                                    mkspecs "/features/qt_config.prf")))
               ;; For each Qt module, let `qmake' uses search paths in the
               ;; module directory instead of all in QT_INSTALL_PREFIX.
               (substitute* qt_config.prf
                 (("\\$\\$\\[QT_INSTALL_HEADERS\\]")
                  "$$clean_path($$replace(dir, mkspecs/modules, ../../include/qt5))")
                 (("\\$\\$\\[QT_INSTALL_LIBS\\]")
                  "$$clean_path($$replace(dir, mkspecs/modules, ../../lib))")
                 (("\\$\\$\\[QT_HOST_LIBS\\]")
                  "$$clean_path($$replace(dir, mkspecs/modules, ../../lib))")
                 (("\\$\\$\\[QT_INSTALL_BINS\\]")
                  "$$clean_path($$replace(dir, mkspecs/modules, ../../bin))"))

               ;; Searches Qt tools in the current PATH instead of QT_HOST_BINS.
               (substitute* (string-append mkspecs "/features/qt_functions.prf")
                 (("cmd = \\$\\$\\[QT_HOST_BINS\\]/\\$\\$2")
                  "cmd = $$system(which $${2}.pl 2>/dev/null || which $${2})"))

               ;; Resolve qmake spec files within qtbase by absolute paths.
               (substitute*
                   (map (lambda (file)
                          (string-append mkspecs "/features/" file))
                        '("device_config.prf" "moc.prf" "qt_build_config.prf"
                          "qt_config.prf" "winrt/package_manifest.prf"))
                 (("\\$\\$\\[QT_HOST_DATA/get\\]") archdata)
                 (("\\$\\$\\[QT_HOST_DATA/src\\]") archdata))
               #t)))
         (add-after 'unpack 'patch-paths
           ;; Use the absolute paths for dynamically loaded libs, otherwise
           ;; the lib will be searched in LD_LIBRARY_PATH which typically is
           ;; not set in guix.
           (lambda* (#:key inputs #:allow-other-keys)
             ;; libresolve
             (let ((glibc (assoc-ref inputs ,(if (%current-target-system)
                                                 "cross-libc" "libc"))))
               (substitute* '("src/network/kernel/qdnslookup_unix.cpp"
                              "src/network/kernel/qhostinfo_unix.cpp")
                 (("^\\s*(lib.setFileName\\(QLatin1String\\(\")(resolv\"\\)\\);)" _ a b)
                (string-append a glibc "/lib/lib" b))))
             ;; X11/locale (compose path)
             (substitute* "src/plugins/platforminputcontexts/compose/generator/qtablegenerator.cpp"
               ;; Don't search in /usr/…/X11/locale, …
               (("^\\s*m_possibleLocations.append\\(QStringLiteral\\(\"/usr/.*/X11/locale\"\\)\\);" line)
                (string-append "// " line))
               ;; … but use libx11's path
               (("^\\s*(m_possibleLocations.append\\(QStringLiteral\\()X11_PREFIX \"(/.*/X11/locale\"\\)\\);)" _ a b)
                (string-append a "\"" (assoc-ref inputs "libx11") b)))
             ;; libGL
             (substitute* "src/plugins/platforms/xcb/gl_integrations/xcb_glx/qglxintegration.cpp"
               (("^\\s*(QLibrary lib\\(QLatin1String\\(\")(GL\"\\)\\);)" _ a b)
                (string-append a (assoc-ref inputs "mesa") "/lib/lib" b)))
             ;; libXcursor
             (substitute* "src/plugins/platforms/xcb/qxcbcursor.cpp"
               (("^\\s*(QLibrary xcursorLib\\(QLatin1String\\(\")(Xcursor\"\\), 1\\);)" _ a b)
                (string-append a (assoc-ref inputs "libxcursor") "/lib/lib" b))
               (("^\\s*(xcursorLib.setFileName\\(QLatin1String\\(\")(Xcursor\"\\)\\);)" _ a b)
                (string-append a (assoc-ref inputs "libxcursor") "/lib/lib" b)))
             #t)))))
    (native-search-paths
     (list (search-path-specification
            (variable "QMAKEPATH")
            (files '("lib/qt5")))
           (search-path-specification
            (variable "QML2_IMPORT_PATH")
            (files '("lib/qt5/qml")))
           (search-path-specification
            (variable "QT_PLUGIN_PATH")
            (files '("lib/qt5/plugins")))
           (search-path-specification
            (variable "XDG_DATA_DIRS")
            (files '("share")))
           (search-path-specification
            (variable "XDG_CONFIG_DIRS")
            (files '("etc/xdg")))))
    (home-page "https://www.qt.io/")
    (synopsis "Cross-platform GUI library")
    (description "Qt is a cross-platform application and UI framework for
developers using C++ or QML, a CSS & JavaScript like language.")
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public qtsvg
  (package (inherit qtbase)
    (name "qtsvg")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "14a4rprbj9f9rhixbk7143xdz34d7d39xh9v2sc1w43q9sf2rsi1"))))
    (propagated-inputs `())
    (native-inputs `(("perl" ,perl)))
    (inputs
     `(("mesa" ,mesa)
       ("qtbase" ,qtbase)
       ("zlib" ,zlib)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'configure-qmake
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (qtbase (assoc-ref inputs "qtbase"))
                    (tmpdir (string-append (getenv "TMPDIR")))
                    (qmake (string-append tmpdir "/qmake"))
                    (qt.conf (string-append tmpdir "/qt.conf")))
               ;; Use qmake with a customized qt.conf to override install
               ;; paths to $out.
               (symlink (which "qmake") qmake)
               (setenv "PATH" (string-append tmpdir ":" (getenv "PATH")))
               (with-output-to-file qt.conf
                 (lambda ()
                   (format #t "[Paths]
Prefix=~a
ArchData=lib/qt5
Data=share/qt5
Documentation=share/doc/qt5
Headers=include/qt5
Libraries=lib
LibraryExecutables=lib/qt5/libexec
Binaries=bin
Tests=tests
Plugins=lib/qt5/plugins
Imports=lib/qt5/imports
Qml2Imports=lib/qt5/qml
Translations=share/qt5/translations
Settings=etc/xdg
Examples=share/doc/qt5/examples
HostPrefix=~a
HostData=lib/qt5
HostBinaries=bin
HostLibraries=lib

[EffectiveSourcePaths]
HostPrefix=~a
HostData=lib/qt5
" out out qtbase)))
               #t)))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Valid QT_BUILD_PARTS variables are:
             ;; libs tools tests examples demos docs translations
             (invoke "qmake" "QT_BUILD_PARTS = libs tools tests")))
         (add-before 'check 'set-display
           (lambda _
             ;; make Qt render "offscreen", required for tests
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (synopsis "Qt module for displaying SVGs")
    (description "The QtSvg module provides classes for displaying the
 contents of SVG files.")))

(define-public qtimageformats
  (package (inherit qtsvg)
    (name "qtimageformats")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0zq8igsjyyhxsjr43vpaasrqjw3x0g6rwqf8kaz7y9vs7ny63ch4"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 (delete-file-recursively "src/3rdparty")
                 #t))))
    (native-inputs `())
    (inputs
     `(("jasper" ,jasper)
       ("libmng" ,libmng)
       ("libtiff" ,libtiff)
       ("libwebp" ,libwebp)
       ("mesa" ,mesa)
       ("qtbase" ,qtbase)
       ("zlib" ,zlib)))
    (synopsis "Additional Image Format plugins for Qt")
    (description "The QtImageFormats module contains plugins for adding
support for MNG, TGA, TIFF and WBMP image formats.")))

(define-public qtx11extras
  (package (inherit qtsvg)
    (name "qtx11extras")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "11fd2mc20qmnyv1vqhaqad2q6m0i4lmkr432rmqvpkgphpkfp7pr"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (native-inputs `(("perl" ,perl)))
    (inputs
     `(("mesa" ,mesa)
       ("qtbase" ,qtbase)))
    (synopsis "Qt Extras for X11")
    (description "The QtX11Extras module includes the library to access X11
from within Qt 5.")))

(define-public qtxmlpatterns
  (package (inherit qtsvg)
    (name "qtxmlpatterns")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1vhfvgi39miqsx3iq7c9sii2sykq0yfng69b70i0smr20zihpl4b"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'disable-network-tests
             (lambda _ (substitute* "tests/auto/auto.pro"
                         (("qxmlquery") "# qxmlquery")
                         (("xmlpatterns ") "# xmlpatterns"))
               #t))))))
    (native-inputs `(("perl" ,perl)))
    (inputs `(("qtbase" ,qtbase)))
    (synopsis "Qt XML patterns module")
    (description "The QtXmlPatterns module is a XQuery and XPath engine for
XML and custom data models.  It contains programs such as xmlpatterns and
xmlpatternsvalidator.")))

(define-public qtdeclarative
  (package (inherit qtsvg)
    (name "qtdeclarative")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1rhsf9bma2zwwpixk2fsg31x7c1pmsk144npypgc9w86swhkc9lf"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("qtsvg" ,qtsvg)
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (inputs
     `(("mesa" ,mesa)
       ("qtbase" ,qtbase)))
    (synopsis "Qt QML module (Quick 2)")
    (description "The Qt QML module provides a framework for developing
applications and libraries with the QML language.  It defines and implements the
language and engine infrastructure, and provides an API to enable application
developers to extend the QML language with custom types and integrate QML code
with JavaScript and C++.")))

(define-public qtconnectivity
  (package (inherit qtsvg)
    (name "qtconnectivity")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0amks3qad31i7cha85kvcaxvlmmgkc3gm4jdkw2p02ixxfygr30l"))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("qtdeclarative" ,qtdeclarative)))
    (inputs
     `(("bluez" ,bluez)
       ("qtbase" ,qtbase)))
    (synopsis "Qt Connectivity module")
    (description "The Qt Connectivity modules provides modules for interacting
with Bluetooth and NFC.")))

(define-public qtwebsockets
  (package (inherit qtsvg)
    (name "qtwebsockets")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1ffmapfy68xwwbxbg19ng6b5h8v42cf78s21j7rgq49gm70r0402"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (native-inputs
     `(("perl" ,perl)
       ("qtdeclarative" ,qtdeclarative)))
    (inputs `(("qtbase" ,qtbase)))
    (synopsis "Qt Web Sockets module")
    (description "WebSocket is a web-based protocol designed to enable two-way
communication between a client application and a remote host.  The Qt
WebSockets module provides C++ and QML interfaces that enable Qt applications
to act as a server that can process WebSocket requests, or a client that can
consume data received from the server, or both.")))

(define-public qtsensors
  (package (inherit qtsvg)
    (name "qtsensors")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0n88c8xi9pbyh7q1pcqv4yjv6nx62abflj8qgfr4qzb0sp8m6mx7"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:parallel-tests? _ #f) #f) ; can lead to race condition
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'fix-tests
             (lambda _
               (substitute* "tests/auto/qsensorgestures_gestures/tst_sensorgestures_gestures.cpp"
                 (("2000") "5000")      ;lengthen test timeout
                 (("QTest::newRow(\"twist\") << \"twist\"") "")) ;failing test
               #t))))))
    (native-inputs
     `(("perl" ,perl)
       ("qtdeclarative" ,qtdeclarative)))
    (inputs `(("qtbase" ,qtbase)))
    (synopsis "Qt Sensors module")
    (description "The Qt Sensors API provides access to sensor hardware via QML
and C++ interfaces.  The Qt Sensors API also provides a motion gesture
recognition API for devices.")))

(define-public qtmultimedia
  (package (inherit qtsvg)
    (name "qtmultimedia")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0h9wx86zj20n4xc3qnml0i360x2dc1yd2z2af1flj8fwyzppi03j"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 (delete-file-recursively
                   "examples/multimedia/spectrum/3rdparty")
                 ;; We also prevent the spectrum example from being built.
                 (substitute* "examples/multimedia/multimedia.pro"
                   (("spectrum") "#"))
                 #t))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (invoke "qmake" "QT_BUILD_PARTS = libs tools tests"
                         (string-append "QMAKE_LFLAGS_RPATH=-Wl,-rpath," out "/lib -Wl,-rpath,")
                         (string-append "PREFIX=" out)))))))
       ((#:tests? _ #f) #f)))           ; TODO: Enable the tests
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("qtdeclarative" ,qtdeclarative)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("mesa" ,mesa)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase)
       ;; Gstreamer is needed for the mediaplayer plugin
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)))
    (synopsis "Qt Multimedia module")
    (description "The Qt Multimedia module provides set of APIs to play and
record media, and manage a collection of media content.  It also contains a
set of plugins for interacting with pulseaudio and GStreamer.")))

(define-public qtwayland
  (package (inherit qtsvg)
    (name "qtwayland")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1chz4wchgkzd45h143i5hkqg0whcgdbj37gkg7j4kj31whllzjb2"))
             (modules '((guix build utils)))
             (snippet
               ;; The examples try to build and cause the build to fail
              '(begin
                 (delete-file-recursively "examples")
                 #t))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'check 'set-ld-library-path
             ;; <https://lists.gnu.org/archive/html/guix-devel/2017-09/msg00019.html>
             ;;
             ;; Make the uninstalled libQt5WaylandClient.so.5 available to the
             ;; wayland platform plugin.
             (lambda _
               (setenv "LD_LIBRARY_PATH" (string-append (getcwd) "/lib"))
               #t))))))
    (native-inputs
     `(("glib" ,glib)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("qtdeclarative" ,qtdeclarative)))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("libx11" ,libx11)
       ("libxcomposite" ,libxcomposite)
       ("libxext" ,libxext)
       ("libxkbcommon" ,libxkbcommon)
       ("libxrender" ,libxrender)
       ("mesa" ,mesa)
       ("mtdev" ,mtdev)
       ("qtbase" ,qtbase)
       ("wayland" ,wayland)))
    (synopsis "Qt Wayland module")
    (description "The Qt Wayland module provides the QtWayland client and
compositor libraries.")))

(define-public qtserialport
  (package (inherit qtsvg)
    (name "qtserialport")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1nkbfsxzgicwns3k11hhzjxy2hgrigj8xcw2by0jc1j71mnmxi4n"))))
    (native-inputs `(("perl" ,perl)))
    (inputs
     `(("qtbase" ,qtbase)
       ("eudev" ,eudev)))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'patch-dlopen-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/serialport/qtudev_p.h"
               ;; Use the absolute paths for dynamically loaded libs,
               ;; otherwise the lib will be searched in LD_LIBRARY_PATH which
               ;; typically is not set in guix.
               (("^\\s*(udevLibrary->setFileNameAndVersion\\(QStringLiteral\\(\")(udev\"\\),\\s*[0-9]+\\);)" _ a b)
                (string-append a (assoc-ref inputs "eudev") "/lib/lib" b)))
             #t))))))
    (synopsis "Qt Serial Port module")
    (description "The Qt Serial Port module provides the library for
interacting with serial ports from within Qt.")))

(define-public qtserialbus
  (package (inherit qtsvg)
    (name "qtserialbus")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0vf12jk1ma0v0dlpliw1x9i04iaik1kjkiaby7gaxm2abprxwr2n"))))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtserialport" ,qtserialport)))
    (synopsis "Qt Serial Bus module")
    (description "The Qt Serial Bus API provides classes and functions to
access the various industrial serial buses and protocols, such as CAN, ModBus,
and others.")))

(define-public qtwebchannel
  (package (inherit qtsvg)
    (name "qtwebchannel")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1wrdawlqvcw84h8q52mvbjhp1vkd6fhz6c8ijlg9rw0s3fj4y99w"))))
    (native-inputs
     `(("perl" ,perl)
       ("qtdeclarative" ,qtdeclarative)
       ("qtwebsockets" ,qtwebsockets)))
    (inputs `(("qtbase" ,qtbase)))
    (synopsis "Web communication library for Qt")
    (description "The Qt WebChannel module enables peer-to-peer communication
between the host (QML/C++ application) and the client (HTML/JavaScript
application).  The transport mechanism is supported out of the box by the two
popular web engines, Qt WebKit 2 and Qt WebEngine.")))

(define-public qtwebglplugin
  (package (inherit qtsvg)
    (name "qtwebglplugin")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0wqz8lycmi7pffzy0pz5960w109lbk4mkbw0l1lh64avl6clq7b9"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'disable-network-tests
             (lambda _ (substitute* "tests/plugins/platforms/platforms.pro"
                         (("webgl") "# webgl"))
               #t))))))
    (native-inputs '())
    (inputs
     `(("mesa" ,mesa)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtwebsockets" ,qtwebsockets)))
    (synopsis "QPA plugin for running an application via a browser using
streamed WebGL commands")
    (description "Qt back end that uses WebGL for rendering. It allows Qt
applications (with some limitations) to run in a web browser that supports
WebGL.  WebGL is a JavaScript API for rendering 2D and 3D graphics within any
compatible web browser without the use of plug-ins.  The API is similar to
OpenGL ES 2.0 and can be used in HTML5 canvas elements")))

(define-public qtwebview
  (package (inherit qtsvg)
    (name "qtwebview")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1njmn1n03dp4md8cz58cq2z6bsxd8nwlw0238zmavh7px3jzc9kh"))))
    (native-inputs
     `(("perl" ,perl)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (synopsis "Display web content in a QML application")
    (description "Qt WebView provides a way to display web content in a QML
application without necessarily including a full web browser stack by using
native APIs where it makes sense.")))

(define-public qtlocation
  (package (inherit qtsvg)
    (name "qtlocation")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1sq0f41jwmsimv9a1wl2nk5nifjppm5j92rr4n4s7qwnnjjrir2q"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (native-inputs
     `(("perl" ,perl)
       ("qtdeclarative" ,qtdeclarative)
       ("qtquickcontrols" ,qtquickcontrols)
       ("qtserialport" ,qtserialport)))
    (inputs
     `(("icu4c" ,icu4c)
       ("openssl" ,openssl)
       ("qtbase" ,qtbase)
       ("zlib" ,zlib)))
    (synopsis "Qt Location and Positioning modules")
    (description "The Qt Location module provides an interface for location,
positioning and geolocation plugins.")))

(define-public qttools
  (package (inherit qtsvg)
    (name "qttools")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "13lzdxxi02yhvx4mflhisl6aqv2fiss5m804cqccd1wvp8dyh1f2"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (native-inputs
     `(("perl" ,perl)
       ("qtdeclarative" ,qtdeclarative)
       ("vulkan-headers" ,vulkan-headers)))
    (inputs
     `(("mesa" ,mesa)
       ("qtbase" ,qtbase)))
    (synopsis "Qt Tools and Designer modules")
    (description "The Qt Tools module provides a set of applications to browse
the documentation, translate applications, generate help files and other stuff
that helps in Qt development.")))

(define-public qtscript
  (package (inherit qtsvg)
    (name "qtscript")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "027cvggbcvwyz76cn1bl1zvqg0nq7iica1b7yx7xyy0hb36g715v"))
             (patches (search-patches "qtscript-disable-tests.patch"))))
    (native-inputs
     `(("perl" ,perl)
       ("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)))
    (synopsis "Qt Script module")
    (description "Qt provides support for application scripting with ECMAScript.
The following guides and references cover aspects of programming with
ECMAScript and Qt.")))

(define-public qtquickcontrols
  (package (inherit qtsvg)
    (name "qtquickcontrols")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0dvmy31qbl76yy0j5y8m7mvnmqyg2c01fmlkn0snvc5h5ah5skjf"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (synopsis "Qt Quick Controls and other Quick modules")
    (description "The QtScript module provides classes for making Qt
applications scriptable.  This module provides a set of extra components that
can be used to build complete interfaces in Qt Quick.")))

(define-public qtquickcontrols2
  (package (inherit qtsvg)
    (name "qtquickcontrols2")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "11nhpb0xckv5jjkqj5szr94c2rvyjwr89ch58hh64nsqaav30mpl"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (synopsis "Qt Quick Controls 2 and other Quick 2 modules")
    (description "The Qt Quick Controls 2 module contains the Qt Labs Platform
module that provides platform integration: native dialogs, menus and menu bars,
and tray icons.  It falls back to Qt Widgets when a native implementation is
not available.")))

(define-public qtgraphicaleffects
  (package (inherit qtsvg)
    (name "qtgraphicaleffects")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1qjpdzkamf27cg5n1wsf0zk939lcgppgydfjzap9s4fxzj1nkn0l"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (synopsis "Qt Graphical Effects module")
    (description "The Qt Graphical Effects module provides a set of QML types
for adding visually impressive and configurable effects to user interfaces.
Effects are visual items that can be added to Qt Quick user interface as UI
components.  The API consists of over 20 effects provided as separate QML
types.  The effects cover functional areas such as blending, masking, blurring,
coloring, and many more.")))

(define-public qtdeclarative-render2d
  ;; As of Qt-5.8.0 this module has been merged into qtdeclarative
  (package (inherit qtsvg)
    (name "qtdeclarative-render2d")
    (version "5.7.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0zwch9vn17f3bpy300jcfxx6cx9qymk5j7khx0x9k1xqid4166c3"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 (delete-file-recursively "tools/opengldummy/3rdparty")
                 #t))))
    (native-inputs `())
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (synopsis "Qt Declarative Render module")
    (description "The Qt Declarative Render 2D module provides a Raster
backend for QtQuick scene graph.")
    (properties `((superseded . ,qtdeclarative)))))

(define-public qtgamepad
  (package (inherit qtsvg)
    (name "qtgamepad")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1k222cx18zq48sfna91hmy427qzk2n2xz3dlyz59iyz72k6915g9"))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("libxrender" ,libxrender)
       ("sdl2" ,sdl2)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (synopsis "Qt Gamepad module")
    (description "The Qt Gamepad module is an add-on library that enables Qt
applications to support the use of gamepad hardware and in some cases remote
control equipment.  The module provides both QML and C++ interfaces.  The
primary target audience are embedded devices with fullscreen user interfaces,
and mobile applications targeting TV-like form factors.")))

(define-public qtscxml
  (package (inherit qtsvg)
    (name "qtscxml")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1mv8mz36v34dckrzy5r41mq3sqznbalrhndk3avz2154xmkjf5qk"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 (delete-file-recursively "tests/3rdparty")
                 ;; the scion test refers to the bundled 3rd party test code.
                 (substitute* "tests/auto/auto.pro"
                   (("scion") "#"))
                 #t))))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (synopsis "Qt SCXML module")
    (description "The Qt SCXML module provides functionality to create state
machines from SCXML files.  This includes both dynamically creating state
machines (loading the SCXML file and instantiating states and transitions) and
generating a C++ file that has a class implementing the state machine.  It
also contains functionality to support data models and executable content.")))

(define-public qtpurchasing
  (package (inherit qtsvg)
    (name "qtpurchasing")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1fd0gxdj5mrh81iwimq1243i3n47sqv9ik8nslahfh0q3dsx7k8n"))))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (synopsis "Qt Purchasing module")
    (description "The Qt Purchasing module provides and in-app API for
purchasing goods and services.")))

(define-public qtcanvas3d
  (package (inherit qtsvg)
    (name "qtcanvas3d")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0f110z7cmkzns9k00aa5zhzq2fpybfxkd7gdlwzcbhc8hn20986m"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 (delete-file-recursively "examples/canvas3d/3rdparty")
                 #t))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
      ;; Building the tests depends on the bundled 3rd party javascript files,
      ;; and the test phase fails to import QtCanvas3D, causing the phase to
      ;; fail, so we skip building them for now.
      ((#:phases phases)
       `(modify-phases ,phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "qmake" "QT_BUILD_PARTS = libs tools"
                       (string-append "PREFIX=" out)))))))
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (native-inputs `())
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (synopsis "Qt Canvas 3D module")
    (description "The Qt Canvas 3D module provides a way to make WebGL-like 3D
drawing calls from Qt Quick JavaScript.")))

(define-public qtcharts
  (package (inherit qtsvg)
    (name "qtcharts")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1p4m1nkbbxlkwmbmasx5r83skzssmlcgfzyvj30x2dyrqkmz7627"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (synopsis "Qt Charts module")
    (description "The Qt Charts module provides a set of easy to use chart
components.  It uses the Qt Graphics View Framework, therefore charts can be
easily integrated to modern user interfaces.  Qt Charts can be used as QWidgets,
QGraphicsWidget, or QML types. Users can easily create impressive graphs by
selecting one of the charts themes.")
    (license license:gpl3)))

(define-public qtdatavis3d
  (package (inherit qtsvg)
    (name "qtdatavis3d")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1kqwr3avcvcyy4i28vjgxk1bsjj9011zr668hsk1zrjxnnwjwdl3"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (synopsis "Qt Data Visualization module")
    (description "The Qt Data Visualization module provides a way to visualize
data in 3D as bar, scatter, and surface graphs. It is especially useful for
visualizing depth maps and large quantities of rapidly changing data, such as
data received from multiple sensors. The look and feel of graphs can be
customized by using themes or by adding custom items and labels to them.")
    (license license:gpl3)))

(define-public qtnetworkauth
  (package (inherit qtsvg)
    (name "qtnetworkauth")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0dd35698wzg89975vi2ijl2lym495fjizsl03mjixsjnvb1x0q50"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'remove-failing-test
             (lambda _
               ;; These tests can't find their test data.
               (substitute* "tests/auto/auto.pro"
                 (("oauth1 ") "# oauth1 "))
               #t))))))
    (inputs
     `(("qtbase" ,qtbase)))
    (synopsis "Qt Network Authorization module")
    (description "The Qt Network Authorization module provides an
implementation of OAuth and OAuth2 authenticathon methods for Qt.")))

(define-public qtremoteobjects
  (package (inherit qtsvg)
    (name "qtremoteobjects")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1d3jzsxfyjhgb6wj9iv1388bv7j6pi08346nmkm1c1a4iykhc0zp"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'remove-failing-test
             (lambda _
               ;; This test can't find its imports.
               (substitute* "tests/auto/qml/qml.pro"
                 (("integration") "# integration")
                 (("usertypes") "# usertypes"))
               #t))))))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (synopsis "Qt Remote Objects module")
    (description "The Qt Remote Objects module is an @dfn{inter-process
communication} (IPC) module developed for Qt.  The idea is to extend existing
Qt's functionalities to enable an easy exchange of information between
processes or computers.")))

(define-public qtspeech
  (package (inherit qtsvg)
    (name "qtspeech")
    (version "5.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "158p7zqd0vg55gf88jzc3d4f7649ihh80k0m1q46m2yp6fpdjbxr"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:tests? _ #f) #f))) ; TODO: Enable the tests
    (inputs
     `(("qtbase" ,qtbase)))
    (native-inputs
     `(("perl" ,perl)
       ("qtdeclarative" ,qtdeclarative)
       ("qtmultimedia" ,qtmultimedia)
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (synopsis "Qt Speech module")
    (description "The Qt Speech module enables a Qt application to support
accessibility features such as text-to-speech, which is useful for end-users
who are visually challenged or cannot access the application for whatever
reason.  The most common use case where text-to-speech comes in handy is when
the end-user is driving and cannot attend the incoming messages on the phone.
In such a scenario, the messaging application can read out the incoming
message.")))

(define-public python-sip
  (package
    (name "python-sip")
    (version "4.19.13")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append "mirror://sourceforge/pyqt/sip/"
                         "sip-" version "/sip-" version ".tar.gz"))
        (sha256
         (base32
          "0pniq03jk1n5bs90yjihw3s3rsmjd8m89y9zbnymzgwrcl2sflz3"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python-wrapper)))
    (arguments
     `(#:tests? #f ; no check target
       #:modules ((srfi srfi-1)
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (include (string-append out "/include"))
                    (python (assoc-ref inputs "python"))
                    (python-version
                      (last (string-split python #\-)))
                    (python-major+minor
                      (string-join
                        (take (string-split python-version #\.) 2)
                        "."))
                    (lib (string-append out "/lib/python"
                                        python-major+minor
                                        "/site-packages")))
               (invoke "python" "configure.py"
                       "--bindir" bin
                       "--destdir" lib
                       "--incdir" include)))))))
    (home-page "https://www.riverbankcomputing.com/software/sip/intro")
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
    (license license:gpl3)))

(define-public python2-sip
  (package (inherit python-sip)
    (name "python2-sip")
    (native-inputs
     `(("python" ,python-2)))))

(define-public python-pyqt
  (package
    (name "python-pyqt")
    (version "5.11.3")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append "mirror://sourceforge/pyqt/PyQt5/"
                         "PyQt-" version "/PyQt5_gpl-"
                         version ".tar.gz"))
        (sha256
         (base32
          "0wqh4srqkcc03rvkwrcshaa028psrq58xkys6npnyhqxc0apvdf9"))
       (patches (search-patches "pyqt-configure.patch"
                                "pyqt-public-sip.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("qtbase" ,qtbase))) ; for qmake
    (propagated-inputs
     `(("python-sip" ,python-sip)))
    (inputs
     `(("python" ,python-wrapper)
       ("qtbase" ,qtbase)
       ("qtconnectivity" ,qtconnectivity)
       ("qtdeclarative" ,qtdeclarative)
       ("qtlocation" ,qtlocation)
       ("qtmultimedia" ,qtmultimedia)
       ("qtsensors" ,qtsensors)
       ("qtserialport" ,qtserialport)
       ("qtsvg" ,qtsvg)
       ("qttools" ,qttools)
       ("qtwebchannel" ,qtwebchannel)
       ("qtwebkit" ,qtwebkit)
       ("qtwebsockets" ,qtwebsockets)
       ("qtx11extras" ,qtx11extras)
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (arguments
     `(#:modules ((srfi srfi-1)
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (sip (string-append out "/share/sip"))
                    (plugins (string-append out "/plugins"))
                    (designer (string-append plugins "/designer"))
                    (qml (string-append plugins "/PyQt5"))
                    (python (assoc-ref inputs "python"))
                    (python-version
                      (last (string-split python #\-)))
                    (python-major+minor
                      (string-join
                        (take (string-split python-version #\.) 2)
                        "."))
                    (lib (string-append out "/lib/python"
                                        python-major+minor
                                        "/site-packages"))
                    (stubs (string-append lib "/PyQt5")))
               (invoke "python" "configure.py"
                       "--confirm-license"
                       "--bindir" bin
                       "--destdir" lib
                       "--designer-plugindir" designer
                       "--qml-plugindir" qml
                       ; Where to install the PEP 484 Type Hints stub
                       ; files. Without this the stubs are tried to be
                       ; installed into the python package's
                       ; site-package directory, which is read-only.
                       "--stubsdir" stubs
                       "--sipdir" sip)))))))
    (home-page "https://www.riverbankcomputing.com/software/pyqt/intro")
    (synopsis "Python bindings for Qt")
    (description
     "PyQt is a set of Python v2 and v3 bindings for the Qt application
framework.  The bindings are implemented as a set of Python modules and
contain over 620 classes.")
    (license license:gpl3)))

;; XXX: This is useful because qtwebkit does not build reliably at this time.
;; Ultimately, it would be nicer to have a more modular set of python-pyqt-*
;; packages that could be used together.
(define-public python-pyqt-without-qtwebkit
  (package (inherit python-pyqt)
    (name "python-pyqt-without-qtwebkit")
    (inputs
     (alist-delete "qtwebkit" (package-inputs python-pyqt)))))

(define-public python2-pyqt
  (package (inherit python-pyqt)
    (name "python2-pyqt")
    (native-inputs
     `(("python-sip" ,python2-sip)
       ("qtbase" ,qtbase)))
    (inputs
     `(("python" ,python-2)
       ("python2-enum34" ,python2-enum34)
       ,@(alist-delete "python" (package-inputs python-pyqt))))))

(define-public python2-pyqt-4
  (package (inherit python-pyqt)
    (name "python2-pyqt")
    (version "4.12")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append "mirror://sourceforge/pyqt/PyQt4/"
                         "PyQt-" version "/PyQt4_gpl_x11-"
                         version ".tar.gz"))
        (sha256
         (base32
          "1nw8r88a5g2d550yvklawlvns8gd5slw53yy688kxnsa65aln79w"))))
    (native-inputs
     `(("python-sip" ,python2-sip)
       ("qt" ,qt-4)))
    (inputs `(("python" ,python-2)))
    (arguments
     `(#:tests? #f ; no check target
       #:modules ((srfi srfi-1)
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (sip (string-append out "/share/sip"))
                    (python (assoc-ref inputs "python"))
                    (python-version
                      (last (string-split python #\-)))
                    (python-major+minor
                      (string-join
                        (take (string-split python-version #\.) 2)
                        "."))
                    (lib (string-append out "/lib/python"
                                        python-major+minor
                                        "/site-packages")))
               (invoke "python" "configure.py"
                       "--confirm-license"
                       "--bindir" bin
                       "--destdir" lib
                       "--sipdir" sip)))))))
    (license (list license:gpl2 license:gpl3)))) ; choice of either license

(define-public qscintilla
  (package
    (name "qscintilla")
    (version "2.10.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/pyqt/QScintilla2/"
                                  "QScintilla-" version "/QScintilla_gpl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1swjr786w04r514pry9pn32ivza4il1cg35s60qy39cwc175pka6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (chdir "Qt4Qt5")
               (substitute* "qscintilla.pro"
                 (("\\$\\$\\[QT_INSTALL_LIBS\\]")
                  (string-append out "/lib"))
                 (("\\$\\$\\[QT_INSTALL_HEADERS\\]")
                  (string-append out "/include"))
                 (("\\$\\$\\[QT_INSTALL_TRANSLATIONS\\]")
                  (string-append out "/translations"))
                 (("\\$\\$\\[QT_INSTALL_DATA\\]")
                  (string-append out "/lib/qt$${QT_MAJOR_VERSION}"))
                 (("\\$\\$\\[QT_HOST_DATA\\]")
                 (string-append out "/lib/qt$${QT_MAJOR_VERSION}")))
               (invoke "qmake")))))))
    (native-inputs `(("qtbase" ,qtbase)))
    (home-page "https://www.riverbankcomputing.co.uk/software/qscintilla/intro")
    (synopsis "Qt port of the Scintilla C++ editor control")
    (description "QScintilla is a port to Qt of Neil Hodgson's Scintilla C++
editor control.  QScintilla includes features especially useful when editing
and debugging source code.  These include support for syntax styling, error
indicators, code completion and call tips.")
    (license license:gpl3+)))

(define-public python-qscintilla
  (package (inherit qscintilla)
    (name "python-qscintilla")
    (arguments
     `(#:configure-flags
       (list "--pyqt=PyQt5"
             (string-append "--pyqt-sipdir="
                            (assoc-ref %build-inputs "python-pyqt")
                            "/share/sip")
             (string-append "--qsci-incdir="
                            (assoc-ref %build-inputs "qscintilla")
                            "/include")
             (string-append "--qsci-libdir="
                            (assoc-ref %build-inputs "qscintilla")
                            "/lib"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs configure-flags #:allow-other-keys)
             (let ((out    (assoc-ref outputs "out"))
                   (python (assoc-ref inputs "python")))
               (chdir "Python")
               (apply invoke "python3" "configure.py"
                      configure-flags)
               ;; Install to the right directory
               (substitute* '("Makefile"
                              "Qsci/Makefile")
                 (("\\$\\(INSTALL_ROOT\\)/gnu/store/[^/]+") out)
                 (((string-append python "/lib"))
                  (string-append out "/lib")))
               ;; And fix the installed.txt file
               (substitute* "installed.txt"
                 (("/gnu/store/[^/]+") out)))
             #t)))))
    (inputs
     `(("qscintilla" ,qscintilla)
       ("python" ,python)
       ("python-pyqt" ,python-pyqt)))
    (description "QScintilla is a port to Qt of Neil Hodgson's Scintilla C++
editor control.  QScintilla includes features especially useful when editing
and debugging source code.  These include support for syntax styling, error
indicators, code completion and call tips.

This package provides the Python bindings.")))

;; PyQt only looks for modules in its own directory.  It ignores environment
;; variables such as PYTHONPATH, so we need to build a union package to make
;; it work.
(define-public python-pyqt+qscintilla
  (package (inherit python-pyqt)
    (name "python-pyqt+qscintilla")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
                   (use-modules (ice-9 match)
                                (guix build union))
                   (match %build-inputs
                     (((names . directories) ...)
                      (union-build (assoc-ref %outputs "out")
                                   directories)
                      #t)))))
    (inputs
     `(("python-pyqt" ,python-pyqt)
       ("python-qscintilla" ,python-qscintilla)))
    (synopsis "Union of PyQt and the Qscintilla extension")
    (description
     "This package contains the union of PyQt and the Qscintilla extension.")))

(define-public qtkeychain
  (package
    (name "qtkeychain")
    (version "0.9.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/frankosterfeld/qtkeychain/")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0h4wgngn2yl35hapbjs24amkjfbzsvnna4ixfhn87snjnq5lmjbc"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:tests? #f ; No tests included
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-qt-trans-dir
           (lambda _
             (substitute* "CMakeLists.txt"
              (("\\$\\{qt_translations_dir\\}")
               "${CMAKE_INSTALL_PREFIX}/share/qt5/translations"))
             #t)))))
    (home-page "https://github.com/frankosterfeld/qtkeychain")
    (synopsis "Qt API to store passwords")
    (description
      "QtKeychain is a Qt library to store passwords and other secret data
securely.  It will not store any data unencrypted unless explicitly requested.")
    (license license:bsd-3)))

(define-public qwt
  (package
    (name "qwt")
    (version "6.1.4")
    (source
      (origin
        (method url-fetch)
        (uri
         (string-append "mirror://sourceforge/qwt/qwt/"
                        version "/qwt-" version ".tar.bz2"))
        (sha256
         (base32 "1navkcnmn0qz8kzsyqmk32d929zl72l0b580w1ica7z5559j2a8m"))))
  (build-system gnu-build-system)
  (inputs
   `(("qtbase" ,qtbase)
     ("qtsvg" ,qtsvg)
     ("qttools" ,qttools)))
  (arguments
   `(#:phases
     (modify-phases %standard-phases
       (replace 'configure
         (lambda* (#:key outputs #:allow-other-keys)
           (let* ((out (assoc-ref outputs "out"))
                  (docdir (string-append out "/share/doc/qwt"))
                  (incdir (string-append out "/include/qwt"))
                  (pluginsdir (string-append out "/lib/qt5/plugins/designer"))
                  (featuresdir (string-append out "/lib/qt5/mkspecs/features")))
             (substitute* '("qwtconfig.pri")
               (("^(\\s*QWT_INSTALL_PREFIX)\\s*=.*" _ x)
                (format #f "~a = ~a\n" x out))
               (("^(QWT_INSTALL_DOCS)\\s*=.*" _ x)
                (format #f "~a = ~a\n" x docdir))
               (("^(QWT_INSTALL_HEADERS)\\s*=.*" _ x)
                (format #f "~a = ~a\n" x incdir))
               (("^(QWT_INSTALL_PLUGINS)\\s*=.*" _ x)
                (format #f "~a = ~a\n" x pluginsdir))
               (("^(QWT_INSTALL_FEATURES)\\s*=.*" _ x)
                (format #f "~a = ~a\n" x featuresdir)))
             (substitute* '("doc/doc.pro")
               ;; We'll install them in the 'install-man-pages' phase.
               (("^unix:doc\\.files.*") ""))
             (invoke "qmake"))))
       (add-after 'install 'install-man-pages
         (lambda* (#:key outputs #:allow-other-keys)
           (let* ((out (assoc-ref outputs "out"))
                  (man (string-append out "/share/man")))
             ;; Remove some incomplete manual pages.
             (for-each delete-file (find-files "doc/man/man3" "^_tmp.*"))
             (mkdir-p man)
             (copy-recursively "doc/man" man)
             #t))))))
  (home-page "http://qwt.sourceforge.net")
  (synopsis "Qt widgets for plots, scales, dials and other technical software
GUI components")
  (description
   "The Qwt library contains widgets and components which are primarily useful
for technical and scientific purposes.  It includes a 2-D plotting widget,
different kinds of sliders, and much more.")
  (license
   (list
    ;; The Qwt license is LGPL2.1 with some exceptions.
    (license:non-copyleft "http://qwt.sourceforge.net/qwtlicense.html")
    ;; textengines/mathml/qwt_mml_document.{cpp,h} is dual LGPL2.1/GPL3 (either).
    license:lgpl2.1 license:gpl3))))

(define-public qtwebkit
  (package
    (name "qtwebkit")
    (version "5.212.0-alpha2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/annulen/webkit/releases/download/"
                            name "-" version "/" name "-" version ".tar.xz"))
        (sha256
         (base32
          "12lg7w00d8wsj672s1y5z5gm0xdcgs16nas0b5bgq4byavg03ygq"))
        (patches (search-patches "qtwebkit-pbutils-include.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2.7)
       ("ruby" ,ruby)
       ("bison" ,bison)
       ("flex" ,flex)
       ("gperf" ,gperf)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("icu" ,icu4c)
       ("glib" ,glib)
       ("gst-plugins-base" ,gst-plugins-base)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libwebp" ,libwebp)
       ("sqlite" ,sqlite)
       ("fontconfig" ,fontconfig)
       ("libxrender" ,libxrender)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtlocation" ,qtlocation)
       ("qtmultimedia" ,qtmultimedia)
       ("qtsensors" ,qtsensors)
       ("qtwebchannel" ,qtwebchannel)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("libx11" ,libx11)
       ("libxcomposite" ,libxcomposite)))
    (arguments
     `(#:tests? #f ; no apparent tests; it might be necessary to set
                   ; ENABLE_API_TESTS, see CMakeLists.txt

       ;; Parallel builds fail due to a race condition:
       ;; <https://bugs.gnu.org/34062>.
       #:parallel-build? #f

       #:configure-flags (list ;"-DENABLE_API_TESTS=TRUE"
                               "-DPORT=Qt"
                               "-DUSE_LIBHYPHEN=OFF"
                               "-DUSE_SYSTEM_MALLOC=ON"
                               ;; XXX: relative dir installs to build dir?
                               (string-append "-DECM_MKSPECS_INSTALL_DIR="
                                              %output "/lib/qt5/mkspecs/modules")
                               ;; Sacrifice a little speed in order to link
                               ;; libraries and test executables in a
                               ;; reasonable amount of memory.
                               "-DCMAKE_SHARED_LINKER_FLAGS=-Wl,--no-keep-memory"
                               "-DCMAKE_EXE_LINKER_FLAGS=-Wl,--no-keep-memory")))
    (home-page "https://www.webkit.org")
    (synopsis "Web browser engine and classes to render and interact with web
content")
    (description "QtWebKit provides a Web browser engine that makes it easy to
embed content from the World Wide Web into your Qt application.  At the same
time Web content can be enhanced with native controls.")
    (license license:lgpl2.1+)))

(define-public dotherside
  (package
    (name "dotherside")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/filcuc/DOtherSide/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0azq7qlsrfdwbd6qsi7d3c1knn42qw0r47g43xf7clwbinapswpz"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (home-page "https://filcuc.github.io/DOtherSide/index.html")
    (synopsis "C language library for creating bindings for the Qt QML language")
    (description
     "DOtherSide is a C language library for creating bindings for the
QT QML language.  The following features are implementable from
a binding language:
@itemize
@item Creating custom QObject
@item Creating custom QAbstractListModels
@item Creating custom properties, signals and slots
@item Creating from QML QObject defined in the binded language
@item Creating from Singleton QML QObject defined in the binded language
@end itemize\n")
    (license license:lgpl3)))                    ;version 3 only (+ exception)

;; There have been no public releases yet.
(define-public qtcolorwidgets
  (let ((commit "a95f72e935fe9e046061a1d1c3930cbfbcb533e0")
        (revision "1"))
    (package
      (name "qtcolorwidgets")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/mattia.basaglia/Qt-Color-Widgets")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0dkiwlqh2gwhlp78c1fmchj3shl4p9inspcl96ya5aa8mn6kydy8"))))
      (build-system cmake-build-system)
      (arguments `(#:tests? #f)) ; There are no tests
      (native-inputs
       `(("qttools" ,qttools)))
      (inputs
       `(("qtbase" ,qtbase)))
      (home-page "https://gitlab.com/mattia.basaglia/Qt-Color-Widgets")
      (synopsis "Color management widgets")
      (description "QtColorWidgets provides a Qt color dialog that is more
user-friendly than the default @code{QColorDialog} and several other
color-related widgets.")
      ;; Includes a license exception for combining with GPL2 code.
      (license license:lgpl3+))))
