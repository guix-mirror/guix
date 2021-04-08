;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Quiliro <quiliro@fsfla.org>
;;; Copyright © 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Mike Rosset <mike.rosset@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2020 TomZ <tomz@freedommail.ch>
;;; Copyright © 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Brendan Tildesley <mail@brendan.scot>
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
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-1))

(define-public qite
  (let ((commit "75fb3b6bbd5c6a5a8fc35e08a6efbfb588ed546a")
        (revision "74"))
    (package
      (name "qite")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/Ri0n/qite")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0jmmgy9pvk9hwwph1nwy7hxhczy8drhl4ymhnjjn6yx7bckssvsq"))))
      (build-system qt-build-system)
      (arguments
       `(#:tests? #f                    ; no target
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "libqite")
               #t)))))
      (inputs
       `(("qtbase" ,qtbase)
         ("qtmultimedia" ,qtmultimedia)))
      (home-page "https://github.com/Ri0n/qite/")
      (synopsis "Qt Interactive Text Elements")
      (description "Qite allows to manage interactive elements on QTextEdit.")
      (license license:asl2.0))))

(define-public qt5ct
  (package
    (name "qt5ct")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/qt5ct/qt5ct-" version ".tar.bz2"))
       (sha256
        (base32 "1lnx4wqk87lbr6lqc64w5g5ppjjv75kq2r0q0bz9gfpryzdw8xxg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; No target
       #:imported-modules
       (,@%gnu-build-system-modules
        (guix build cmake-build-system)
        (guix build qt-build-system))
       #:modules
       ((guix build gnu-build-system)
        ((guix build qt-build-system)
         #:prefix qt:)
        (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "qt5ct.pro"
               (("\\$\\$\\[QT_INSTALL_BINS\\]/lrelease")
                (string-append (assoc-ref inputs "qttools")
                               "/bin/lrelease")))
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (invoke "qmake"
                       (string-append "PREFIX=" out)
                       (string-append "BINDIR=" out "/bin")
                       (string-append "DATADIR=" out "/share")
                       (string-append "PLUGINDIR=" out "/lib/qt5/plugins")))
             #t))
         (add-after 'install 'qt-wrap
           (assoc-ref qt:%standard-phases 'qt-wrap)))))
    (native-inputs
     `(("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)))
    (synopsis "Qt5 Configuration Tool")
    (description "Qt5CT is a program that allows users to configure Qt5
settings (such as icons, themes, and fonts) in desktop environments or
window managers, that don't provide Qt integration by themselves.")
    (home-page "https://qt5ct.sourceforge.io/")
    (license license:bsd-2)))

(define-public materialdecoration
  (let ((commit "6a5de23f2e5162fbee39d16f938473ff970a2ec0")
        (revision "9"))
    (package
      (name "materialdecoration")
      (version
       (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/lirios/materialdecoration.git")
           (commit commit)))
         (file-name
          (git-file-name name version))
         (sha256
          (base32 "1zdrcb39fhhmn76w8anv1dnspz26pdl6izmj1mlm02aza4y8ffp4"))
         (modules '((guix build utils)
                    (ice-9 ftw)
                    (srfi srfi-1)))
         (snippet
          `(begin
             (delete-file-recursively "cmake/3rdparty")))))
      (build-system qt-build-system)
      (arguments
       `(#:tests? #f                    ; No target
         #:configure-flags
         (list
          (string-append "-DCMAKE_CXX_FLAGS=-I"
                         (assoc-ref %build-inputs "qtbase")
                         "/include/qt5/QtXkbCommonSupport/"
                         ,(package-version qtbase)))))
      (native-inputs
       `(("cmake-shared" ,cmake-shared)
         ("extra-cmake-modules" ,extra-cmake-modules)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("qtbase" ,qtbase)
         ("qtwayland" ,qtwayland)
         ("wayland" ,wayland)
         ("xkbcommon" ,libxkbcommon)))
      (synopsis "Material Decoration for Qt")
      (description "MaterialDecoration is a client-side decoration for Qt
applications on Wayland.")
      (home-page "https://github.com/lirios/materialdecoration")
      (license license:lgpl3+))))

(define-public grantlee
  (package
    (name "grantlee")
    (version "5.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/steveire/grantlee")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "02dyqxjyxiqxrlz5g7v9ly8f095vs3iha39l75q6s8axs36y01lq"))))
    (native-inputs
     ;; Optional: lcov and cccc, both are for code coverage
     `(("doxygen" ,doxygen)))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
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
    (license license:lgpl2.1+)))

(define (qt5-urls component version)
  "Return a list of URLs for VERSION of the Qt5 COMPONENT."
  ;; We can't use a mirror:// scheme because these URLs are not exact copies:
  ;; the layout differs between them.
  (list (string-append "https://download.qt.io/official_releases/qt/"
                       (version-major+minor version) "/" version
                       "/submodules/" component "-everywhere-src-"
                       version ".tar.xz")
        (string-append "https://download.qt.io/archive/qt/"
                       (version-major+minor version) "/" version
                       "/submodules/" component "-everywhere-src-"
                       version ".tar.xz")
        (let ((directory (string-append "qt5" (string-drop component 2))))
          (string-append "http://sources.buildroot.net/" directory "/"
                         component "-everywhere-src-" version ".tar.xz"))
        (string-append "https://distfiles.macports.org/qt5/"
                       component "-everywhere-src-" version ".tar.xz")))

(define-public qtbase
  (package
    (name "qtbase")
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "1y70libf2x52lpbqvhz10lpk7nyl1ajjwzjxly9pjdpfj4jsv7wh"))
             ;; Use TZDIR to avoid depending on package "tzdata".
             (patches (search-patches "qtbase-use-TZDIR.patch"
                                      "qtbase-moc-ignore-gcc-macro.patch"
                                      "qtbase-absolute-runpath.patch"))
             (modules '((guix build utils)))
             (snippet
               ;; corelib uses bundled harfbuzz, md4, md5, sha3
              '(begin
                (with-directory-excursion "src/3rdparty"
                  (for-each delete-file-recursively
                            (list "double-conversion" "freetype" "harfbuzz-ng"
                                  "libpng" "libjpeg" "pcre2" "sqlite" "xcb"
                                  "zlib"))
                  #t)))))
    (build-system gnu-build-system)
    (outputs '("out" "debug"))
    (propagated-inputs
     `(("mesa" ,mesa)
       ;; Use which the package, not the function
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
       ("libjpeg" ,libjpeg-turbo)
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
       ("mariadb-dev" ,mariadb "dev")
       ("nss" ,nss)
       ("openssl" ,openssl)
       ("pcre2" ,pcre2)
       ("postgresql" ,postgresql)
       ("pulseaudio" ,pulseaudio)
       ("sqlite" ,sqlite)
       ("unixodbc" ,unixodbc)
       ("xcb-util" ,xcb-util)
       ("xcb-util-image" ,xcb-util-image)
       ("xcb-util-keysyms" ,xcb-util-keysyms)
       ("xcb-util-renderutil" ,xcb-util-renderutil)
       ("xcb-util-wm" ,xcb-util-wm)
       ("xdg-utils" ,xdg-utils)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("gperf" ,gperf)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
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
         (add-after 'configure 'patch-xdg-open
           (lambda _
             (substitute* '("src/platformsupport/services/genericunix/qgenericunixservices.cpp")
                          (("^.*const char \\*browsers.*$" all)
                           (string-append "*browser = QStringLiteral(\""
                                          (which "xdg-open")
                                          "\"); return true; \n" all)))
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

                 ;; Later stripped into the :debug output.
                 "-force-debug-info"

                 ;; These features require higher versions of Linux than the
                 ;; minimum version of the glibc.  See
                 ;; src/corelib/global/minimum-linux_p.h.  By disabling these
                 ;; features Qt5 applications can be used on the oldest
                 ;; kernels that the glibc supports, including the RHEL6
                 ;; (2.6.32) and RHEL7 (3.10) kernels.
                 "-no-feature-getentropy"  ; requires Linux 3.17
                 "-no-feature-renameat2"   ; requires Linux 3.16

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
         (add-after 'patch-mkspecs 'patch-prl-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Insert absolute references to the qtbase libraries because
               ;; QT_INSTALL_LIBS does not always resolve correctly, depending
               ;; on context.  See <https://bugs.gnu.org/38405>
               (substitute* (find-files (string-append out "/lib") "\\.prl$")
                 (("\\$\\$\\[QT_INSTALL_LIBS\\]")
                  (string-append out "/lib")))
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

;; qt used to refer to the monolithic Qt 5.x package
(define-deprecated qt qtbase)

(define-public qtsvg
  (package (inherit qtbase)
    (name "qtsvg")
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "0pjqrdmd1991x9h4rl8sf81pkd89hfd5h1a2gp3fjw96pk0w5hwb"))))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "1msk8a0z8rr16hkp2fnv668vf6wayiydqgc2mcklaa04rv3qb0mz"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 (delete-file-recursively "src/3rdparty")
                 #t))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'fix-build
             (lambda _
               (substitute* "src/plugins/imageformats/jp2/qjp2handler.cpp"
                 (("^#include <jasper/jasper.h>")
                  "#include <jasper/jasper.h>\n#include <QtCore/qmath.h>"))
               #t))))))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "0gkfzj195v9flwljnqpdz3a532618yn4h2577nlsai56x4p7053h"))))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "1ypj5jpa31rlx8yfw3y9jia212lfnxvnqkvygs6ihjf3lxi23skn"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:tests? _ #f) #f) ; TODO: Enable the tests
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'disable-network-tests
             (lambda _ (substitute* "tests/auto/auto.pro"
                         (("qxmlquery") "# qxmlquery")
                         (("xmlpatterns ") "# xmlpatterns"))
               #t))))))
    (native-inputs `(("perl" ,perl)
                     ("qtdeclarative" ,qtdeclarative)))
    (inputs `(("qtbase" ,qtbase)))
    (synopsis "Qt XML patterns module")
    (description "The QtXmlPatterns module is a XQuery and XPath engine for
XML and custom data models.  It contains programs such as xmlpatterns and
xmlpatternsvalidator.")))

(define-public qtdeclarative
  (package (inherit qtsvg)
    (name "qtdeclarative")
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "0lancdn7y0lrlmyn5cbdm0izd5yprvd5n77nhkb7a3wl2sbx0066"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:tests? _ #f) #f)             ;TODO: Enable the tests
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'build 'fix-qt5core-install-prefix
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 ;; The Qt5Core install prefix is set to qtbase, but qmlcachegen
                 ;; is provided by qtdeclarative.
                 (substitute*
                     "lib/cmake/Qt5QuickCompiler/Qt5QuickCompilerConfig.cmake"
                   (("\\$\\{_qt5Core_install_prefix\\}") out)))
               #t))))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("python-wrapper" ,python-wrapper)
       ("qtsvg" ,qtsvg)
       ("vulkan-headers" ,vulkan-headers)))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "185zci61ip1wpjrygcw2m6v55lvninc0b8y2p3jh6qgpf5w35003"))))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "0gr399fn5n8j3m9d3vv01vcbr1cb7pw043j04cnnxzrlvn2jvd50"))))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "0fa81r7bn1mf9ynwsx524a55dx1q0jb4vda6j48ssb4lx7wi201z"))))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "1xbd6kc7i0iablqdkvfrajpi32cbq7j6ajbfyyyalcai1s0mhdqc"))
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
       ("python" ,python)
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "1ddfx4nak16xx0zh1kl836zxvpbixmmjyplsmfmg65pqkwi34dqr"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'disable-failing-tests
             (lambda _
               ;; FIXME: tst_seatv4::animatedCursor() fails for no good
               ;; reason and breaks these two tests.
               (substitute* "tests/auto/client/seatv4/tst_seatv4.cpp"
                 (((string-append "QVERIFY\\(!cursorSurface\\(\\)->"
                                  "m_waitingFrameCallbacks\\.empty\\(\\)\\);"))
                  "")
                 (("QTRY_COMPARE\\(bufferSpy\\.count\\(\\), 1\\);")
                  ""))
               #t))
           (add-before 'check 'set-test-environment
             (lambda _
               ;; Do not fail just because /etc/machine-id is missing.
               (setenv "DBUS_FATAL_WARNINGS" "0")
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
       ("vulkan-headers" ,vulkan-headers)
       ("wayland" ,wayland)))
    (synopsis "Qt Wayland module")
    (description "The Qt Wayland module provides the QtWayland client and
compositor libraries.")))

(define-public qtserialport
  (package (inherit qtsvg)
    (name "qtserialport")
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "17gp5qzg4wdg8qlxk2p3mh8x1vk33rf33wic3fy0cws193bmkiar"))))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "125x6756fjpldqy6wbw6cg7ngjh2016aiq92bchh719z1mf7xsxf"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'unpack 'patch-libsocketcan-reference
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((libcansocket (assoc-ref inputs "libsocketcan"))
                      (libcansocket.so (string-append libcansocket
                                                      "/lib/libsocketcan.so")))
                 (substitute* "src/plugins/canbus/socketcan/libsocketcan.cpp"
                   (("QStringLiteral\\(\"socketcan\"\\)")
                    (format #f "QStringLiteral(~s)" libcansocket.so)))
                 #t)))))))
    (inputs
     `(("libsocketcan" ,libsocketcan)
       ("qtbase" ,qtbase)
       ("qtserialport" ,qtserialport)))
    (synopsis "Qt Serial Bus module")
    (description "The Qt Serial Bus API provides classes and functions to
access the various industrial serial buses and protocols, such as CAN, ModBus,
and others.")))

(define-public qtwebchannel
  (package (inherit qtsvg)
    (name "qtwebchannel")
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "1h9y634phvvk557mhmf9z4lmxr41rl8x9mqy2lzp31mk8ffffzqj"))))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "0ihlnhv8ldkqz82v3j7j22lrhk17b6ghra8sx85y2agd2ysq5rw1"))))
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
       ("qtwebsockets" ,qtwebsockets)
       ("zlib" ,zlib)))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "1rw1wibmbxlj6xc86qs3y8h42al1vczqiksyxzaylxs9gqb4d7xy"))))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-everywhere-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "184jychnlfhplpwc5cdcsapwljgwvzk5qpf3val4kpq8w44wnkwq"))))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "1k618f7v6jaj0ygy8d7jvgb8zjr47sn55kiskbdkkizp3z7d12f1"))))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "0gk74hk488k9ldacxbxcranr3arf8ifqg8kz9nm1rgdgd59p36d2"))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "1dczakl868mg0lnwpf082jjc5976ycn879li1vqlgw5ihirzp4y3"))))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "06c9vrwvbjmzapmfa25y34lgjkzg57xxbm92nr6wkv5qykjnq6v7"))))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "1r6zfc0qga2ax155js7c8y5rx6vgayf582s921j09mb797v6g3gc"))))
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

(define-public qtgamepad
  (package (inherit qtsvg)
    (name "qtgamepad")
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "0p07bg93fdfn4gr2kv38qgnws5znhswajrxdfs8xc9l3i7vi2xn7"))))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "1p5771b9hnpchfcdgy0zkhwg09a6xq88934aggp0rij1k85mkfb0"))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "09rjx53519dfk4qj2gbn3vlxyriasyb747wpg1p11y7jkwqhs4l7"))))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))
    (synopsis "Qt Purchasing module")
    (description "The Qt Purchasing module provides and in-app API for
purchasing goods and services.")))

(define-public qtcharts
  (package (inherit qtsvg)
    (name "qtcharts")
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "049x7z8zcp9jixmdv2fjscy2ggpd6za9hkdbb2bqp2mxjm0hwxg0"))))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "1zdn3vm0nfy9ny7c783aabp3mhlnqhi9fw2rljn7ibbksmsnasi2"))))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "11fdgacv4syr8bff2vdw7rb0dg1gcqpdf37hm3pn31d6z91frhpw"))))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "1hngbp0vkr35rpsrac7b9vx6f360v8v2g0fffzm590l8j2ybd0b7"))))
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
               ;; disable failing tests: they need network
               (substitute* "tests/auto/auto.pro"
                 (("integration_multiprocess proxy_multiprocess integration_external restart")
                   "integration_multiprocess"))
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
    (version "5.15.2")
    (source (origin
             (method url-fetch)
             (uri (qt5-urls name version))
             (sha256
              (base32
               "1xc3x3ghnhgchsg1kgj156yg69wn4rwjx8r28i1jd05hxjggn468"))))

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

(define-public qtspell
  (package
    (name "qtspell")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/manisandro/qtspell")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1081makirjxixz44ghwz362vgnk5wcks6ni6w01pl667x8wggsd2"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ;no test
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     `(("enchant" ,enchant)
       ("qtbase" ,qtbase)))
    (home-page "https://github.com/manisandro/qtspell")
    (synopsis "Spell checking for Qt text widgets")
    (description
     "QtSpell adds spell-checking functionality to Qt's text widgets,
using the Enchant spell-checking library.")
    ;; COPYING file specify GPL3, but source code files all refer to GPL2+.
    (license license:gpl2+)))

(define-public qtwebengine
  (package
    (inherit qtsvg)
    (name "qtwebengine")
    (version (package-version qtbase))
    (source
     (origin
       (method url-fetch)
       (uri (qt5-urls name version))
       (sha256
        (base32
         "1q4idxdm81sx102xc12ixj0xpfx52d6vwvs3jpapnkyq8c7cmby8"))
       (modules '((ice-9 ftw)
                  (ice-9 match)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (guix build utils)))
       (snippet
        '(begin
           (let ((preserved-third-party-files
                  '("base/third_party/double_conversion"
                    "base/third_party/cityhash"
                    "base/third_party/cityhash_v103"
                    "base/third_party/dynamic_annotations"
                    "base/third_party/icu"
                    "base/third_party/libevent"
                    "base/third_party/nspr"
                    "base/third_party/superfasthash"
                    "base/third_party/symbolize"
                    "base/third_party/xdg_mime"
                    "base/third_party/xdg_user_dirs"
                    "net/third_party/mozilla_security_manager"
                    "net/third_party/nss"
                    "net/third_party/quiche"
                    "net/third_party/uri_template"
                    "third_party/abseil-cpp"
                    "third_party/angle"
                    "third_party/angle/src/common/third_party/base"
                    "third_party/angle/src/common/third_party/smhasher"
                    "third_party/angle/src/common/third_party/xxhash"
                    "third_party/angle/src/third_party/compiler"
                    "third_party/axe-core"
                    "third_party/blink"
                    "third_party/boringssl"
                    "third_party/boringssl/src/third_party/fiat"
                    "third_party/breakpad"
                    "third_party/brotli"
                    "third_party/ced"
                    "third_party/cld_3"
                    "third_party/closure_compiler"
                    "third_party/crashpad"
                    "third_party/crashpad/crashpad/third_party/lss"
                    "third_party/crashpad/crashpad/third_party/zlib"
                    "third_party/crc32c"
                    "third_party/dav1d"
                    "third_party/dawn"
                    "third_party/devtools-frontend"
                    "third_party/devtools-frontend/src/front_end/third_party/fabricjs"
                    "third_party/devtools-frontend/src/front_end/third_party/lighthouse"
                    "third_party/devtools-frontend/src/front_end/third_party/wasmparser"
                    "third_party/devtools-frontend/src/third_party/axe-core"
                    "third_party/emoji-segmenter"
                    "third_party/ffmpeg"
                    "third_party/googletest"
                    "third_party/harfbuzz-ng/utils"
                    "third_party/hunspell"
                    "third_party/iccjpeg"
                    "third_party/icu"
                    "third_party/inspector_protocol"
                    "third_party/jinja2"
                    "third_party/jsoncpp"
                    "third_party/jstemplate"
                    "third_party/khronos"
                    "third_party/leveldatabase"
                    "third_party/libaddressinput"
                    "third_party/libgifcodec"
                    "third_party/libjingle_xmpp"
                    "third_party/libjpeg_turbo"
                    "third_party/libpng"
                    "third_party/libsrtp"
                    "third_party/libsync"
                    "third_party/libudev"
                    "third_party/libvpx"
                    "third_party/libwebm"
                    "third_party/libwebp"
                    "third_party/libxml"
                    "third_party/libxslt"
                    "third_party/libyuv"
                    "third_party/lss"
                    "third_party/mako"
                    "third_party/markupsafe"
                    "third_party/mesa_headers"
                    "third_party/metrics_proto"
                    "third_party/modp_b64"
                    "third_party/nasm"
                    "third_party/one_euro_filter"
                    "third_party/opus"
                    "third_party/ots"
                    "third_party/pdfium"
                    "third_party/pdfium/third_party/agg23"
                    "third_party/pdfium/third_party/base"
                    "third_party/pdfium/third_party/freetype"
                    "third_party/pdfium/third_party/lcms"
                    "third_party/pdfium/third_party/libopenjpeg20"
                    "third_party/pdfium/third_party/skia_shared"
                    "third_party/perfetto"
                    "third_party/pffft"
                    "third_party/ply"
                    "third_party/polymer"
                    "third_party/protobuf"
                    "third_party/protobuf/third_party/six"
                    "third_party/pyjson5"
                    "third_party/re2"
                    "third_party/rnnoise"
                    "third_party/skia"
                    "third_party/skia/include/third_party/skcms/skcms.h"
                    "third_party/skia/include/third_party/vulkan"
                    "third_party/skia/third_party/skcms"
                    "third_party/skia/third_party/vulkanmemoryallocator"
                    "third_party/smhasher"
                    "third_party/snappy"
                    "third_party/sqlite"
                    "third_party/usb_ids"
                    "third_party/usrsctp"
                    "third_party/web-animations-js"
                    "third_party/webrtc"
                    "third_party/webrtc/common_audio/third_party/fft4g"
                    "third_party/webrtc/common_audio/third_party/spl_sqrt_floor"
                    "third_party/webrtc/modules/third_party/fft"
                    "third_party/webrtc/modules/third_party/g711"
                    "third_party/webrtc/modules/third_party/g722"
                    "third_party/webrtc/rtc_base/third_party/base64"
                    "third_party/webrtc/rtc_base/third_party/sigslot"
                    "third_party/webrtc_overrides"
                    "third_party/widevine/cdm/widevine_cdm_common.h"
                    "third_party/widevine/cdm/widevine_cdm_version.h"
                    "third_party/woff2"
                    "third_party/yasm"
                    "third_party/zlib"
                    "url/third_party/mozilla"
                    "v8/src/third_party/utf8-decoder"
                    "v8/src/third_party/valgrind"
                    "v8/src/third_party/siphash"
                    "v8/third_party/v8/builtins"
                    "v8/third_party/inspector_protocol"))
                 (protected (make-regexp "\\.(gn|gyp)i?$")))
             (define preserved-club
               (map (lambda (member)
                      (string-append "./" member))
                    preserved-third-party-files))
             (define (empty? dir)
               (equal? (scandir dir) '("." "..")))
             (define (third-party? file)
               (string-contains file "third_party/"))
             (define (useless? file)
               (any (cute string-suffix? <> file)
                    '(".zip" ".so" ".dll" ".exe" ".jar")))
             (define (parents child)
               ;; Return all parent directories of CHILD up to and including
               ;; the closest "third_party".
               (let* ((dirs (match (string-split child #\/)
                              ((dirs ... last) dirs)))
                      (closest (list-index (lambda (dir)
                                             (string=? "third_party" dir))
                                           (reverse dirs)))
                      (delim (- (length dirs) closest)))
                 (fold (lambda (dir prev)
                         (cons (string-append (car prev) "/" dir)
                               prev))
                       (list (string-join (list-head dirs delim) "/"))
                       (list-tail dirs delim))))
             (define (remove-loudly file)
               (format #t "deleting ~a...~%" file)
               (force-output)
               (delete-file file))
             (define (delete-unwanted-files child stat flag base level)
               (match flag
                 ((or 'regular 'symlink 'stale-symlink)
                  (when (third-party? child)
                    (unless (or (member child preserved-club)
                                (any (cute member <> preserved-club)
                                     (parents child))
                                (regexp-exec protected child))
                      (remove-loudly child)))
                  (when (and (useless? child) (file-exists? child))
                    (remove-loudly child))
                  #t)
                 ('directory-processed
                  (when (empty? child)
                    (rmdir child))
                  #t)
                 (_ #t)))

             (with-directory-excursion "src/3rdparty"
               ;; TODO: Try removing "gn" too for future versions of qtwebengine.
               (delete-file-recursively "ninja")

               (with-directory-excursion "chromium"
                 ;; Delete bundled software and binaries that were not explicitly
                 ;; preserved above.
                 (nftw "." delete-unwanted-files 'depth 'physical)

                 ;; Assert that each preserved item is present to catch removals.
                 (for-each (lambda (third-party)
                             (unless (file-exists? third-party)
                               (error (format #f "~s does not exist!~%" third-party))))
                           preserved-club)

                 ;; Use relative header locations instead of hard coded ones.
                 (substitute*
                     "base/third_party/dynamic_annotations/dynamic_annotations.c"
                   (("base/third_party/valgrind") "valgrind"))
                 (substitute*
                     "third_party/breakpad/breakpad/src/common/linux/libcurl_wrapper.h"
                   (("third_party/curl") "curl"))
                 (substitute*
                     '("components/viz/common/gpu/vulkan_context_provider.h"
                       "components/viz/common/resources/resource_format_utils_vulkan.h"
                       "gpu/config/gpu_util.cc")
                   (("third_party/vulkan/include/")
                    ""))

                 ;; Replace Google Analytics bundle with an empty file and hope
                 ;; no one notices.
                 (mkdir-p "third_party/analytics")
                 (call-with-output-file
                     "third_party/analytics/google-analytics-bundle.js"
                   (lambda (port)
                     (const #t)))))
             ;; Do not enable support for loading the Widevine DRM plugin.
             (substitute* "src/buildtools/config/common.pri"
               (("enable_widevine=true")
                "enable_widevine=false"))
             #t)))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("gperf" ,gperf)
       ("ninja" ,ninja)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python-2" ,python-2)
       ("python-six" ,python2-six)
       ("ruby" ,ruby)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("atk" ,atk)
       ("cups-minimal" ,cups-minimal)
       ("curl" ,curl)
       ("dbus" ,dbus)
       ("ffmpeg" ,ffmpeg)
       ("fontconfig" ,fontconfig)
       ("harbuzz" ,harfbuzz)
       ("icu4c" ,icu4c)
       ("jsoncpp" ,jsoncpp)
       ("lcms" ,lcms)
       ("libcap" ,libcap)
       ("libevent" ,libevent)
       ("libgcrypt" ,libgcrypt)
       ("libjpeg" ,libjpeg-turbo)
       ("libvpx" ,libvpx)
       ("libwebp" ,libwebp)
       ("libx11" ,libx11)
       ("libxcb" ,libxcb)
       ("libxcomposite" ,libxcomposite)
       ("libxcursor" ,libxcursor)
       ("libxi" ,libxi)
       ("libxkbcommon" ,libxkbcommon)
       ;; FIXME: libxml2 needs to built with icu support though it links to
       ;; libxml2 configure summary still states "Checking for compatible
       ;; system libxml2... no"
       ("libxml2" ,libxml2)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxslt" ,libxslt)
       ("libxtst" ,libxtst)
       ("mesa" ,mesa)
       ("minizip" ,minizip)
       ("nss" ,nss)
       ("opus" ,opus)
       ("pciutils" ,pciutils)
       ("protobuf" ,protobuf)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtmultimedia" ,qtmultimedia)
       ("qtwebchannel" ,qtwebchannel)
       ("re2" ,re2)
       ("snappy" ,snappy)
       ("udev" ,eudev)
       ("valgrind" ,valgrind)
       ("vulkan-headers" ,vulkan-headers)
       ("xcb-util" ,xcb-util)))
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'configure 'substitute-source
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (nss (assoc-ref inputs "nss"))
                     (udev (assoc-ref inputs "udev")))
                 ;; Qtwebengine is not installed into the same prefix as
                 ;; qtbase.  Some qtbase QTLibraryInfo constants will not
                 ;; work.  Replace with the full path to the qtwebengine
                 ;; translations and locales in the store.
                 (substitute* "src/core/web_engine_library_info.cpp"
                   (("QLibraryInfo::location\\(QLibraryInfo::TranslationsPath\\)")
                    (string-append "QLatin1String(\"" out "/share/qt5/translations\")"))
                   (("QLibraryInfo::location\\(QLibraryInfo::DataPath\\)")
                    (string-append "QLatin1String(\"" out "/share/qt5\")")))
                 ;; Substitute full dynamic library path for nss.
                 (substitute* "src/3rdparty/chromium/crypto/nss_util.cc"
                   (("libnssckbi.so")
                    (string-append nss "/lib/nss/libnssckbi.so")))
                 ;; Substitute full dynamic library path for udev.
                 (substitute* "src/3rdparty/chromium/device/udev_linux/udev1_loader.cc"
                   (("libudev.so.1")
                    (string-append udev "/lib/libudev.so.1")))
                 #t)))
           (add-before 'configure 'set-env
             (lambda _
               ;; Avoids potential race conditions.
               (setenv "PYTHONDONTWRITEBYTECODE" "1")
               (setenv "NINJAFLAGS"
                       (string-append "-k1" ;less verbose build output
                                      ;; Respect the '--cores' option of 'guix build'.
                                      " -j" (number->string (parallel-job-count))))
               #t))
           (replace 'configure
             (lambda _
               ;; Valid QT_BUILD_PARTS variables are:
               ;; libs tools tests examples demos docs translations
               (invoke "qmake" "QT_BUILD_PARTS = libs tools" "--"
                       "--webengine-printing-and-pdf=no"
                       "--webengine-ffmpeg=system"
                       "--webengine-icu=system"
                       "--webengine-pepper-plugins=no")))))
       ;; Tests are disabled due to "Could not find QtWebEngineProcess error"
       ;; It's possible this can be fixed by setting QTWEBENGINEPROCESS_PATH
       ;; before running tests.
       ((#:tests? _ #f) #f)))
    (native-search-paths
     (list (search-path-specification
            (file-type 'regular)
            (separator #f)
            (variable "QTWEBENGINEPROCESS_PATH")
            (files '("lib/qt5/libexec/QtWebEngineProcess")))))
    (home-page "https://wiki.qt.io/QtWebEngine")
    (synopsis "Qt WebEngine module")
    (description "The Qt5WebEngine module provides support for web applications
using the Chromium browser project.  The Chromium source code has Google services
and binaries removed, and adds modular support for using system libraries.")
    (license license:lgpl2.1+)))

(define-public python-sip
  (package
    (name "python-sip")
    (version "5.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (list (pypi-uri "sip" version)
                   (string-append "https://www.riverbankcomputing.com/static/"
                                  "Downloads/sip/" version
                                  "/sip-" version ".tar.gz")))
        (sha256
         (base32
          "1idaivamp1jvbbai9yzv471c62xbqxhaawccvskaizihkd0lq0jx"))))
    (build-system python-build-system)
    (native-inputs
     `(("python" ,python-wrapper)))
    (propagated-inputs
     `(("python-toml" ,python-toml)
       ("python-packaging" ,python-packaging)))
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

(define-public python-sip-4
  (package
    (inherit python-sip)
    (name "python-sip")
    (version "4.19.24")
    (source
      (origin
        (method url-fetch)
        (uri (list (pypi-uri "sip" version)
                   (string-append "https://www.riverbankcomputing.com/static/"
                                  "Downloads/sip/" version
                                  "/sip-" version ".tar.gz")))
        (sha256
         (base32
          "1ra15vb5i9gkg2vdvh16cq9x2mmzw1yi3xphxs8q34q1pf83gkgd"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python-wrapper)))
    (propagated-inputs `())
    (arguments
     `(#:tests? #f ; no check target
       #:imported-modules ((guix build python-build-system)
                           ,@%gnu-build-system-modules)
       #:modules ((srfi srfi-1)
                  ((guix build python-build-system) #:select (python-version))
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (include (string-append out "/include"))
                    (python (assoc-ref inputs "python"))
                    (lib (string-append out "/lib/python"
                                        (python-version python)
                                        "/site-packages")))
               (invoke "python" "configure.py"
                       "--bindir" bin
                       "--destdir" lib
                       "--incdir" include)))))))
    (license license:gpl3)))

(define-public python-pyqt
  (package
    (name "python-pyqt")
    (version "5.15.2")
    (source
      (origin
        (method url-fetch)
        ;; PyPI is the canonical distribution point of PyQt.  Older
        ;; releases are available from the web site.
        (uri (list (pypi-uri "PyQt5" version)
                   (string-append "https://www.riverbankcomputing.com/static/"
                                  "Downloads/PyQt5/" version "/PyQt5-"
                                  version ".tar.gz")))
        (file-name (string-append "PyQt5-" version ".tar.gz"))
        (sha256
         (base32
          "1z74295i69cha52llsqffzhb5zz7qnbjc64h8qg21l91jgf0harp"))
        (patches (search-patches "pyqt-configure.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("qtbase" ,qtbase))) ; for qmake
    (propagated-inputs
     `(("python-sip" ,python-sip)
       ("python-pyqt5-sip" ,python-pyqt5-sip)))
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
                  ((guix build python-build-system) #:select (python-version))
                  ,@%gnu-build-system-modules)
       #:imported-modules ((guix build python-build-system)
                           ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         ;; When building python-pyqtwebengine, <qprinter.h> can not be
         ;; included.  Here we substitute the full path to the header in the
         ;; store.
         (add-before 'configure 'substitute-source
           (lambda* (#:key inputs  #:allow-other-keys)
             (let* ((qtbase (assoc-ref inputs "qtbase"))
                    (qtprinter.h (string-append "\"" qtbase "/include/qt5/QtPrintSupport/qprinter.h\"")))
               (substitute* "sip/QtPrintSupport/qprinter.sip"
                 (("<qprinter.h>")
                  qtprinter.h))
               #t)))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (sip (string-append out "/share/sip"))
                    (plugins (string-append out "/lib/qt5/plugins"))
                    (designer (string-append plugins "/designer"))
                    (qml (string-append plugins "/PyQt5"))
                    (python (assoc-ref inputs "python"))
                    (lib (string-append out "/lib/python"
                                        (python-version python)
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

(define-public python-pyqt5-sip
  (package
    (name "python-pyqt5-sip")
    (version "12.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyQt5_sip" version))
       (sha256
        (base32
         "1gg032ys4pccwkdzmdryadc9a4lq85nr05pag9swrsdykbdl9s9h"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;; No test code.
    (home-page "https://www.riverbankcomputing.com/software/sip/")
    (synopsis "Sip module support for PyQt5")
    (description "Sip module support for PyQt5")
    (license license:lgpl2.1+)))

(define-public python-pyqtwebengine
  (package
    (name "python-pyqtwebengine")
    (version "5.15.2")
    (source
     (origin
       (method url-fetch)
       ;; The newest releases are only available on PyPI.  Older ones
       ;; are mirrored at the upstream home page.
       (uri (list (pypi-uri "PyQtWebEngine" version)
                  (string-append "https://www.riverbankcomputing.com/static"
                                 "/Downloads/PyQtWebEngine/" version
                                 "/PyQtWebEngine-" version ".tar.gz")))
       (sha256
        (base32
         "0d56ak71r14w4f9r96vaj34qcn2rbln3s6ildvvyc707fjkzwwjd"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python)
       ("python-sip" ,python-sip)
       ;; qtbase is required for qmake
       ("qtbase" ,qtbase)))
    (inputs
     `(("python" ,python-wrapper)
       ("python-sip" ,python-sip)
       ("python-pyqt" ,python-pyqt)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtdeclarative" ,qtdeclarative)
       ("qtwebchannel" ,qtwebchannel)
       ("qtwebengine" ,qtwebengine)))
    (arguments
     `(#:modules ((srfi srfi-1)
                  ((guix build python-build-system) #:select (python-version))
                  ,@%gnu-build-system-modules)
       #:imported-modules ((guix build python-build-system)
                           ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (sipdir (string-append out "/share/sip"))
                    (pyqt-sipdir (string-append
                                  (assoc-ref inputs "python-pyqt") "/share/sip"))
                    (python (assoc-ref inputs "python"))
                    (lib (string-append out "/lib/python"
                                        (python-version python)
                                        "/site-packages/PyQt5"))
                    (stubs (string-append lib "/PyQt5")))

               (mkdir-p sipdir)
               (invoke "python" "configure.py"
                       "-w"
                       "--no-dist-info"
                       "--destdir" lib
                       "--no-qsci-api"
                       "--stubsdir" stubs
                       "--sipdir" sipdir
                       "--pyqt-sipdir" pyqt-sipdir))))
         ;; Because this has a different prefix than python-pyqt then we need
         ;; to make this a namespace of it's own
         (add-after 'install 'make-namespace
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((__init__.py (string-append
                                  (assoc-ref outputs "out")
                                  "/lib/python"
                                  (python-version (assoc-ref inputs "python"))
                                  "/site-packages/PyQt5/__init__.py")))
               (with-output-to-file __init__.py
                 (lambda _ (display "
from pkgutil import extend_path
__path__ = extend_path(__path__, __name__)
")))
               #t))))))
    (home-page "https://www.riverbankcomputing.com/software/pyqtwebengine/intro")
    (synopsis "Python bindings for QtWebEngine")
    (description
     "PyQtWebEngine is a set of Python bindings for The Qt Company's Qt
WebEngine libraries.  The bindings sit on top of PyQt5 and are implemented as a
set of three modules.  Prior to v5.12 these bindings were part of PyQt
itself.")
    (license license:gpl3)))

;; XXX: This is useful because qtwebkit does not build reliably at this time.
;; Ultimately, it would be nicer to have a more modular set of python-pyqt-*
;; packages that could be used together.
(define-public python-pyqt-without-qtwebkit
  (package/inherit python-pyqt
    (name "python-pyqt-without-qtwebkit")
    (inputs
     (alist-delete "qtwebkit" (package-inputs python-pyqt)))))

(define-public python-pyqt-builder
  (package
   (name "python-pyqt-builder")
   (version "1.9.0")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "PyQt-builder" version))
     (sha256
      (base32
       "0nh0054c54ji3sm6d268fccf0y5f613spswwgwqd3rnn816hnljl"))))
   (build-system python-build-system)
   (inputs
    `(("python-sip" ,python-sip)))
   (home-page "https://www.riverbankcomputing.com/static/Docs/PyQt-builder/")
   (synopsis "PEP 517 compliant PyQt build system")
   (description "PyQt-builder is a tool for generating Python bindings for C++
libraries that use the Qt application framework.  The bindings are built on
top of the PyQt bindings for Qt.  PyQt-builder is used to build PyQt itself.")
   ;; Either version 2 or 3, but no other version. See the file
   ;; 'pyqtbuild/builder.py' in the source distribution for more information.
   (license (list license:gpl2 license:gpl3))))

(define-public python-qtpy
  (package
    (name "python-qtpy")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "QtPy" version))
       (sha256
          (base32
           "13cw8l7zrhbdi03k1wl1pg9xdl4ahdfa7yz8gd0f23sxnm22rdrd"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pyside2" ,python-pyside-2)))
    (arguments
     `(;; Not all supported bindings are packaged. Especially PyQt4.
       #:tests? #f))
    (home-page "https://github.com/spyder-ide/qtpy")
    (synopsis
     "Qt bindings (PyQt5, PyQt4 and PySide) and additional custom QWidgets")
    (description
     "Provides an abstraction layer on top of the various Qt bindings
(PyQt5, PyQt4 and PySide) and additional custom QWidgets.")
    (license license:expat)))

(define-public qscintilla
  (package
    (name "qscintilla")
    (version "2.11.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.riverbankcomputing.com/static"
                                  "/Downloads/QScintilla/" version
                                  "/QScintilla-" version ".tar.gz"))
              (sha256
               (base32
                "19r0vpqb4m9bqwxmjp9w6x0hgahkrg7zryk78hwgplj7vdbn0d77"))))
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
  (package/inherit qscintilla
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
  (package/inherit python-pyqt
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

(define-public qtsolutions
  (let ((commit "9568abd142d581b67b86a5f63d823a34b0612702")
        (revision "53"))
    (package
      (name "qtsolutions")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/qtproject/qt-solutions")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "17fnmassflm3vxi0krpr6fff368jy38cby31a48rban4nqqmgx7n"))
         (modules '((guix build utils)
                    (ice-9 ftw)
                    (srfi srfi-1)))
         (snippet
          ;; Unvendor QtLockFile from QtSingleApplication.
          '(begin
             (with-directory-excursion "qtsingleapplication/src"
               (for-each delete-file
                         (find-files "." "qtlockedfile.*\\.(h|cpp)"))
                 (substitute* "qtsingleapplication.pri"
                   ;; Add include path of LockedFile.
                   (("INCLUDEPATH \\+=")
                    "INCLUDEPATH += ../../qtlockedfile/src")
                   ;; Link library of LockedFile.
                   (("LIBS \\+=")
                    "LIBS += -lQtSolutions_LockedFile"))
                 (substitute* '("qtlocalpeer.h" "qtlocalpeer.cpp")
                   (("#include \"qtlockedfile.*\\.cpp\"") "")
                   ;; Unwrap namespace added in the vendoring process.
                   (("QtLP_Private::QtLockedFile")
                    "QtLockedFile")))
             #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; No target
         #:imported-modules
         ((guix build copy-build-system)
          ,@%gnu-build-system-modules)
         #:modules
         (((guix build copy-build-system) #:prefix copy:)
          (guix build gnu-build-system)
          (guix build utils))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-source
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* (find-files "." "common.pri")
                 ;; Remove unnecessary prefixes/suffixes in library names.
                 (("qt5") "qt")
                 (("-head") ""))
               ;; Disable building of examples.
               (substitute* (find-files "." "\\.pro$")
                 (("SUBDIRS\\+=examples") ""))
               ;; Fix deprecated functions.
               (substitute* "qtsoap/src/qtsoap.cpp"
                 (("toAscii") "toUtf8"))
               #t))
           (replace 'configure
             (lambda _
               (for-each (lambda (solution)
                           (with-directory-excursion solution
                             (invoke "./configure" "-library")
                             (invoke "qmake")))
                         '("qtlockedfile" "qtpropertybrowser" "qtservice"
                           "qtsingleapplication" "qtsoap"))
               #t))
           (replace 'build
             (lambda _
               (for-each (lambda (solution)
                           (with-directory-excursion solution
                             (invoke "make")))
                         '("qtlockedfile" "qtpropertybrowser" "qtservice"
                           "qtsingleapplication" "qtsoap"))
               #t))
           (replace 'install
             (lambda args
               (for-each (lambda (solution)
                           (with-directory-excursion solution
                             (apply
                              (assoc-ref copy:%standard-phases 'install)
                              #:install-plan
                              '(("src" "include" #:include-regexp ("\\.h$"))
                                ("lib" "lib"))
                              args)))
                         '("qtlockedfile" "qtpropertybrowser" "qtservice"
                           "qtsingleapplication" "qtsoap")))))))
      (inputs
       `(("qtbase" ,qtbase)))
      (synopsis "Collection of Qt extensions")
      (description "QtSolutions is a set of components extending Qt.
@itemize
@item QtLockedFile: A class that extends QFile with advisory locking functions.
@item QtPropertyBrowser: A framework that enables the user to edit a set of
properties.
@item QtService: A helper for writing services such as Unix daemons.
@item QtSingleApplication: A component that provides support for applications
that can be only started once per user.
@item QtSoap: A component that provides basic web service support with version
1.1 of the SOAP protocol.
@end itemize\n")
      (home-page "https://doc.qt.io/archives/qq/qq09-qt-solutions.html")
      (license (list license:bsd-3
                     ;; QScriptParser and QScriptGrammar specifically allow
                     ;; redistribution under GPL3 or LGPL2.1
                     license:gpl3 license:lgpl2.1)))))

(define-public qwt
  (package
    (name "qwt")
    (version "6.1.5")
    (source
      (origin
        (method url-fetch)
        (uri
         (string-append "mirror://sourceforge/qwt/qwt/"
                        version "/qwt-" version ".tar.bz2"))
        (sha256
         (base32 "0hf0mpca248xlqn7xnzkfj8drf19gdyg5syzklvq8pibxiixwxj0"))))
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
    (version "5.212.0-alpha4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/annulen/webkit/releases/download/"
                            "qtwebkit-" version "/qtwebkit-" version ".tar.xz"))
        (sha256
         (base32
          "1rm9sjkabxna67dl7myx9d9vpdyfxfdhrk9w7b94srkkjbd2d8cw"))
        (patches (search-patches "qtwebkit-pbutils-include.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python)
       ("ruby" ,ruby)
       ("bison" ,bison)
       ("flex" ,flex)
       ("gperf" ,gperf)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("icu" ,icu4c)
       ("glib" ,glib)
       ("gst-plugins-base" ,gst-plugins-base)
       ("libjpeg" ,libjpeg-turbo)
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
    ;; Building QtWebKit takes around 13 hours on an AArch64 machine.  Give some
    ;; room for slower or busy hardware.
    (properties '((timeout . 64800)))   ;18 hours

    ;; XXX: This consumes too much RAM to successfully build on AArch64 (e.g.,
    ;; SoftIron OverDrive with 8 GiB of RAM), so instead of wasting resources,
    ;; disable it on non-Intel platforms.
    (supported-systems '("x86_64-linux" "i686-linux"))

    (license license:lgpl2.1+)))

(define-public dotherside
  (package
    (name "dotherside")
    (version "0.6.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/filcuc/DOtherSide")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "09fz6v8rp28997f235yaifj8p4vvsyv45knc1iivgdvx7msgcd0m"))))
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

(define-public python-shiboken-2
  (package
    (name "python-shiboken-2")
    (version "5.14.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.qt.io/official_releases"
                                  "/QtForPython/pyside2/PySide2-" version
                                  "-src/pyside-setup-opensource-src-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "08lhqm0n3fjqpblcx9rshsp8g3bvf7yzbai5q99bly2wa04y6b83"))))
    (build-system cmake-build-system)
    (inputs
     `(("clang-toolchain" ,clang-toolchain)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("python-wrapper" ,python-wrapper)
       ("qtbase" ,qtbase)
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (arguments
     `(#:tests? #f
       ;; FIXME: Building tests fails
       #:configure-flags '("-DBUILD_TESTS=off")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-shiboken-dir-only
           (lambda _ (chdir "sources/shiboken2") #t))
         (add-before 'configure 'make-files-writable-and-update-timestamps
           (lambda _
             ;; The build scripts need to modify some files in
             ;; the read-only source directory, and also attempts
             ;; to create Zip files which fails because the Zip
             ;; format does not support timestamps before 1980.
             (let ((circa-1980 (* 10 366 24 60 60)))
               (for-each (lambda (file)
                           (make-file-writable file)
                           (utime file circa-1980 circa-1980))
                         (find-files ".")))
             #t))
         (add-before 'configure 'set-build-env
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((llvm (assoc-ref inputs "clang-toolchain")))
               (setenv "CLANG_INSTALL_DIR" llvm)
               #t))))))
    (home-page "https://wiki.qt.io/Qt_for_Python")
    (synopsis
     "Shiboken generates bindings for C++ libraries using CPython source code")
    (description
     "Shiboken generates bindings for C++ libraries using CPython source code")
    (license
     (list
      ;; The main code is GPL3 or LGPL3.
      ;; Examples are BSD-3.
      license:gpl3
      license:lgpl3
      license:bsd-3))))

(define-public python-pyside-2
  (package
    (name "python-pyside-2")
    (version (package-version python-shiboken-2))
    (source (package-source python-shiboken-2))
    (build-system cmake-build-system)
    (inputs
     `(("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("clang-toolchain" ,clang-toolchain)
       ("qtbase" ,qtbase)
       ("qtdatavis3d" ,qtdatavis3d)
       ("qtlocation" ,qtlocation)
       ("qtmultimedia" ,qtmultimedia)
       ("qtquickcontrols" ,qtquickcontrols)
       ("qtscript" ,qtscript)
       ("qtscxml" ,qtscxml)
       ("qtsensors" ,qtsensors)
       ("qtspeech" ,qtspeech)
       ("qtsvg" ,qtsvg)
       ("qtwebchannel" ,qtwebchannel)
       ("qtwebsockets" ,qtwebsockets)
       ("qtx11extras" ,qtx11extras)
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (native-inputs
     `(("cmake" ,cmake-minimal)
       ("python-shiboken-2" ,python-shiboken-2)
       ("python" ,python-wrapper)
       ("qttools" ,qttools)
       ("which" ,which)))
    (arguments
     `(#:tests? #f
       ;; FIXME: Building tests fail.
       #:configure-flags
       (list "-DBUILD_TESTS=FALSE"
             (string-append "-DPYTHON_EXECUTABLE="
                            (assoc-ref %build-inputs "python")
                            "/bin/python"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'go-to-source-dir
           (lambda _ (chdir "sources/pyside2") #t))
         (add-before 'configure 'set-clang-dir
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((clang (assoc-ref inputs "clang-toolchain")))
               (setenv "CLANG_INSTALL_DIR" clang)
               #t))))))
    (home-page "https://wiki.qt.io/Qt_for_Python")
    (synopsis
     "The Qt for Python product enables the use of Qt5 APIs in Python applications")
    (description
     "The Qt for Python product enables the use of Qt5 APIs in Python
applications.  It lets Python developers utilize the full potential of Qt,
using the PySide2 module.  The PySide2 module provides access to the
individual Qt modules such as QtCore, QtGui,and so on.  Qt for Python also
comes with the Shiboken2 CPython binding code generator, which can be used to
generate Python bindings for your C or C++ code.")
    (license (list
              license:lgpl3
              ;;They state that:
              ;; this file may be used under the terms of the GNU General
              ;; Public License version 2.0 or (at your option) the GNU
              ;; General Public license version 3 or any later version
              ;; approved by the KDE Free Qt Foundation.
              ;; Thus, it is currently v2 or v3, but no "+".
              license:gpl3
              license:gpl2))))

(define-public python-pyside-2-tools
  (package
    (name "python-pyside-2-tools")
    (version (package-version python-shiboken-2))
    (source (package-source python-shiboken-2))
    (build-system cmake-build-system)
    (inputs
     `(("python-pyside-2" ,python-pyside-2)
       ("python-shiboken-2" ,python-shiboken-2)
       ("qtbase" ,qtbase)))
    (native-inputs
     `(("python" ,python-wrapper)))
    (arguments
     `(#:tests? #f
       #:configure-flags
       (list "-DBUILD_TESTS=off"
             (string-append "-DPYTHON_EXECUTABLE="
                            (assoc-ref %build-inputs "python")
                            "/bin/python"))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'go-to-source-dir
                    (lambda _ (chdir "sources/pyside2-tools") #t)))))
    (home-page "https://wiki.qt.io/Qt_for_Python")
    (synopsis
     "Contains command line tools for PySide2")
    (description
     "Contains lupdate, rcc and uic tools for PySide2")
    (license license:gpl2)))

(define-public libqglviewer
  (package
    (name "libqglviewer")
    (version "2.7.2")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "http://libqglviewer.com/src/libQGLViewer-"
                              version ".tar.gz"))
              (sha256
               (base32
                "023w7da1fyn2z69nbkp2rndiv886zahmc5cmira79zswxjfpklp2"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no check target
       #:make-flags
       (list (string-append "PREFIX="
                            (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke (cons "qmake" make-flags)))))))
    (native-inputs
     `(("qtbase" ,qtbase)
       ("qttools" ,qttools)))
    (inputs
     `(("glu" ,glu)))
    (home-page "http://libqglviewer.com")
    (synopsis "Qt-based C++ library for the creation of OpenGL 3D viewers")
    (description
     "@code{libQGLViewer} is a C++ library based on Qt that eases the creation
of OpenGL 3D viewers.

It provides some of the typical 3D viewer functionalities, such as the
possibility to move the camera using the mouse, which lacks in most of the
other APIs.  Other features include mouse manipulated frames, interpolated
keyFrames, object selection, stereo display, screenshot saving and much more.
It can be used by OpenGL beginners as well as to create complex applications,
being fully customizable and easy to extend.")
    ;; According to LICENSE, either version 2 or version 3 of the GNU GPL may
    ;; be used.
    (license (list license:gpl2 license:gpl3))))


(define-public soqt
  (let ((commit-ref "fb8f655632bb9c9c60e0ff9fa69a5ba22d3ff99d")
        (revision "1"))
    (package
    (name "soqt")
    (version (git-version "1.6.0" revision commit-ref))
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/coin3d/soqt")
               (commit commit-ref)
               (recursive? #t)))
        (file-name (git-file-name name version))
        (sha256
          (base32 "16vikb3fy8rmk10sg5g0gy2c343hi3x7zccsga90ssnkzpq6m032"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f)) ; There are no tests
    (native-inputs
      `(("pkg-config" ,pkg-config)
        ("cmake" ,cmake)))
    (inputs
      `(("qtbase" ,qtbase)
        ("coin3D" ,coin3D-4)))
    (home-page "https://github.com/coin3d/soqt")
    (synopsis "Qt GUI component toolkit library for Coin")
    (description "SoQt is a Qt GUI component toolkit library for Coin.  It is
also compatible with SGI and TGS Open Inventor, and the API is based on the API
of the InventorXt GUI component toolkit.")
    (license license:bsd-3))))
