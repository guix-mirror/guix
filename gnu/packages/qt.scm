;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 ng0 <ng0@we.make.ritual.n0.is>
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
  #:use-module (guix build utils)
  #:use-module (guix build-system cmake)
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
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages databases)
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

(define-public qt
  (package
    (name "qt")
    (version "5.6.1-1")
    (source (origin
             (method url-fetch)
             (uri
               (string-append
                 "http://download.qt.io/official_releases/qt/"
                 (version-major+minor version)
                 "/" version
                 "/single/qt-everywhere-opensource-src-"
                 version ".tar.xz"))
             (sha256
               (base32
                 "1nrn2wivjwdxc9q03gpsi336gcl9l2axi0xjbzsha5v6akmsf26f"))
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
                ;; Remove one of the two bundled harfbuzz copies in addition
                ;; to passing "-system-harfbuzz".
                (delete-file-recursively "qtbase/src/3rdparty/harfbuzz-ng")
                ;; Remove the bundled sqlite copy in addition to
                ;; passing "-system-sqlite".
                (delete-file-recursively "qtbase/src/3rdparty/sqlite")))))
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
       ("harfbuzz" ,harfbuzz)
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
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("ruby" ,ruby)
       ("which" ,(@ (gnu packages base) which))))
    (arguments
     `(;; FIXME: Disabling parallel building is a quick hack to avoid the
       ;; failure described in
       ;; https://lists.gnu.org/archive/html/guix-devel/2016-01/msg00837.html
       ;; A more structural fix is needed.
       #:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-bin-sh
           (lambda _
             (substitute* '("qtbase/config.status"
                            "qtbase/configure"
                            "qtbase/mkspecs/features/qt_functions.prf"
                            "qtbase/qmake/library/qmakebuiltins.cpp")
                          (("/bin/sh") (which "sh")))
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* '("configure" "qtbase/configure")
                 (("/bin/pwd") (which "pwd")))
               (substitute* "qtbase/src/corelib/global/global.pri"
                 (("/bin/ls") (which "ls")))
               ;; do not pass "--enable-fast-install", which makes the
               ;; configure process fail
               (zero? (system*
                       "./configure"
                       "-verbose"
                       "-prefix" out
                       "-opensource"
                       "-confirm-license"
                       ;; Do not build examples; if desired, these could go
                       ;; into a separate output, but for the time being, we
                       ;; prefer to save the space and build time.
                       "-nomake" "examples"
                       ;; Most "-system-..." are automatic, but some use
                       ;; the bundled copy by default.
                       "-system-sqlite"
                       "-system-harfbuzz"
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
                       "-no-mips_dspr2"))))))))
    (home-page "http://qt-project.org/")
    (synopsis "Cross-platform GUI library")
    (description "Qt is a cross-platform application and UI framework for
developers using C++ or QML, a CSS & JavaScript like language.")
    (license license:lgpl2.1)

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
              '(delete-file-recursively "src/3rdparty/webkit"))))
    (inputs `(,@(alist-delete "harfbuzz"
                              (alist-delete "libjpeg" (package-inputs qt)))
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
                      ;; Skip the webkit module; it fails to build on armhf
                      ;; and, apart from that, may pose security risks.
                      "-no-webkit"
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
                      "-no-avx")))))
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
    (version "5.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0ip6xnizsn269r4s1nq9lkx8cdxkjqr1fidwrj3sa8xb7h96syry"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                ;; Remove one of the two bundled harfbuzz copies in addition
                ;; to passing "-system-harfbuzz".
                (delete-file-recursively "src/3rdparty/harfbuzz-ng")
                ;; Remove the bundled sqlite copy in addition to
                ;; passing "-system-sqlite".
                (delete-file-recursively "src/3rdparty/sqlite")))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("mesa" ,mesa)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("cups" ,cups)
       ("dbus" ,dbus)
       ("eudev" ,eudev)
       ("expat" ,expat)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glib" ,glib)
       ("harfbuzz" ,harfbuzz)
       ("icu4c" ,icu4c)
       ("libinput" ,libinput)
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
       ("mysql" ,mysql)
       ("nss" ,nss)
       ("openssl" ,openssl)
       ("pcre" ,pcre)
       ("postgresql" ,postgresql)
       ("pulseaudio" ,pulseaudio)
       ("sqlite" ,sqlite)
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
       ("ruby" ,ruby)
       ("which" ,(@ (gnu packages base) which))))
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
               (zero? (system*
                       "./configure"
                       "-verbose"
                       "-prefix" out
                       "-opensource"
                       "-confirm-license"
                       ;; Do not build examples; if desired, these could go
                       ;; into a separate output, but for the time being, we
                       ;; prefer to save the space and build time.
                       "-nomake" "examples"
                       ;; Most "-system-..." are automatic, but some use
                       ;; the bundled copy by default.
                       "-system-sqlite"
                       "-system-harfbuzz"
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
                       "-no-mips_dspr2")))))
         (add-after 'install 'patch-qt_config.prf
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (qt_config.prf (string-append
                                    out "/mkspecs/features/qt_config.prf")))
               ;; For each Qt module, let `qmake' uses search paths in the
               ;; module directory instead of all in QT_INSTALL_PREFIX.
               (substitute* qt_config.prf
                 (("\\$\\$\\[QT_INSTALL_HEADERS\\]")
                  "$$replace(dir, mkspecs/modules, include)")
                 (("\\$\\$\\[QT_INSTALL_LIBS\\]")
                  "$$replace(dir, mkspecs/modules, lib)")
                 (("\\$\\$\\[QT_HOST_LIBS\\]")
                  "$$replace(dir, mkspecs/modules, lib)")
                 (("\\$\\$\\[QT_INSTALL_PLUGINS\\]")
                  "$$replace(dir, mkspecs/modules, plugins)")
                 (("\\$\\$\\[QT_INSTALL_LIBEXECS\\]")
                  "$$replace(dir, mkspecs/modules, libexec)")
                 (("\\$\\$\\[QT_INSTALL_BINS\\]")
                  "$$replace(dir, mkspecs/modules, bin)")
                 (("\\$\\$\\[QT_INSTALL_IMPORTS\\]")
                  "$$replace(dir, mkspecs/modules, imports)")
                 (("\\$\\$\\[QT_INSTALL_QML\\]")
                  "$$replace(dir, mkspecs/modules, qml)"))
               #t))))))
    (native-search-paths
     (list (search-path-specification
            (variable "QMAKEPATH")
            (files '("")))
           (search-path-specification
            (variable "QML2_IMPORT_PATH")
            (files '("qml")))
           (search-path-specification
            (variable "QT_PLUGIN_PATH")
            (files '("plugins")))
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
    (version "5.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "10fqrlqkiq83xhx79g8d2sjy7hjdnp28067z8f4byj7db81rzy51"))))
    (propagated-inputs `())
    (native-inputs `(("perl" ,perl)))
    (inputs
     `(("mesa" ,mesa)
       ("qtbase" ,qtbase)
       ("zlib" ,zlib)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (zero? (system* "qmake" (string-append "PREFIX=" out))))))
         (add-before 'install 'fix-Makefiles
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out    (assoc-ref outputs "out"))
                   (qtbase (assoc-ref inputs "qtbase")))
               (substitute* (find-files "." "Makefile")
                            (((string-append "INSTALL_ROOT)" qtbase))
                             (string-append "INSTALL_ROOT)" out)))))))))))

(define-public qtimageformats
  (package (inherit qtsvg)
    (name "qtimageformats")
    (version "5.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1rb27x7i2pmvsck6wax2cg31gqpzaakciy45wm5l3lcl86j48czg"))))
    (native-inputs `())
    (inputs
     `(("libmng" ,libmng)
       ("libtiff" ,libtiff)
       ("libwebp" ,libwebp)
       ("mesa" ,mesa)
       ("qtbase" ,qtbase)
       ("zlib" ,zlib)))))

(define-public qtx11extras
  (package (inherit qtsvg)
    (name "qtx11extras")
    (version "5.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1yrkn8pqdbvbqykas3wx1vdfimhjkgx3s5jgdxib9dgmgyx6vjzw"))))
    (native-inputs `(("perl" ,perl)))
    (inputs
     `(("mesa" ,mesa)
       ("qtbase" ,qtbase)))))

(define-public qtxmlpatterns
  (package (inherit qtsvg)
    (name "qtxmlpatterns")
    (version "5.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "02z2qxamslg6sphnaykjcjfpypq4b69pb586s43vw4fplm72m21q"))))
    (native-inputs `(("perl" ,perl)))
    (inputs `(("qtbase" ,qtbase)))))

(define-public qtdeclarative
  (package (inherit qtsvg)
    (name "qtdeclarative")
    (version "5.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1x7rij423g5chlfd2kix54f393vxwjvdfsn1c7sybqmfycwn5pl6"))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("qtsvg" ,qtsvg)
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (inputs
     `(("mesa" ,mesa)
       ("qtbase" ,qtbase)))))

(define-public qtconnectivity
  (package (inherit qtsvg)
    (name "qtconnectivity")
    (version "5.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "00r7lc1w3snfp2qfqmviqzv0cw16zd8m1sfpvxvpl65yqmzcli4q"))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("qtdeclarative" ,qtdeclarative)))
    (inputs
     `(("bluez" ,bluez)
       ("qtbase" ,qtbase)))))

(define-public qtwebsockets
  (package (inherit qtsvg)
    (name "qtwebsockets")
    (version "5.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0hwb2l7iwf4wf7l95dli8j3b7h0nffp56skfg1x810kzj0df26vl"))))
    (native-inputs
     `(("perl" ,perl)
       ("qtdeclarative" ,qtdeclarative)))
    (inputs `(("qtbase" ,qtbase)))))

(define-public qtsensors
  (package (inherit qtsvg)
    (name "qtsensors")
    (version "5.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1gii6wg2xd3bkb86y5hgpmwcpl04xav030zscpl6fhscl9kcqg98"))))
    (native-inputs
     `(("perl" ,perl)
       ("qtdeclarative" ,qtdeclarative)))
    (inputs `(("qtbase" ,qtbase)))))

(define-public qtmultimedia
  (package (inherit qtsvg)
    (name "qtmultimedia")
    (version "5.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0ndmhiflmyr144nq8drd5njsdi282ixsm4730q5n0ji2v9dp1bh5"))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("qtdeclarative" ,qtdeclarative)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("mesa" ,mesa)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase)))))

(define-public qtwayland
  (package (inherit qtsvg)
    (name "qtwayland")
    (version "5.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "04dynjcr6gxi3hcqdf688a4hkabi2l17slpcx9k0f3dxygwcgf96"))))
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
       ("wayland" ,wayland)))))

(define-public qtserialport
  (package (inherit qtsvg)
    (name "qtserialport")
    (version "5.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0rc2l14s59qskp16wqlkizfai32s41qlm7a86r3qahx28gc51qaw"))))
    (native-inputs `(("perl" ,perl)))
    (inputs
     `(("qtbase" ,qtbase)
       ("eudev" ,eudev)))))

(define-public qtwebchannel
  (package (inherit qtsvg)
    (name "qtwebchannel")
    (version "5.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "05lqfidlh1ahdd1j9y20p2037qbcq51zkdzj2m8fwhn7ghbwvd1s"))))
    (native-inputs
     `(("perl" ,perl)
       ("qtdeclarative" ,qtdeclarative)
       ("qtwebsockets" ,qtwebsockets)))
    (inputs `(("qtbase" ,qtbase)))))

(define-public qtlocation
  (package (inherit qtsvg)
    (name "qtlocation")
    (version "5.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0rd898gndn41jrp78203lxd94ybfv693l0qg0myag4r46ikk69vh"))))
    (native-inputs
     `(("perl" ,perl)
       ("qtdeclarative" ,qtdeclarative)
       ("qtquickcontrols" ,qtquickcontrols)
       ("qtserialport" ,qtserialport)))
    (inputs `(("qtbase" ,qtbase)))))

(define-public qttools
  (package (inherit qtsvg)
    (name "qttools")
    (version "5.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "004m9l7bgh7qnncbyl3d5fkggdrqx58ib21xv4hflvvarxrssibg"))))
    (native-inputs
     `(("perl" ,perl)
       ("qtdeclarative" ,qtdeclarative)))
    (inputs
     `(("mesa" ,mesa)
       ("qtbase" ,qtbase)))))

(define-public qtscript
  (package (inherit qtsvg)
    (name "qtscript")
    (version "5.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0040890p5ilyrmcpndz1hhp08x2ms5gw4lp4n5iax2a957yy2i4w"))))
    (native-inputs
     `(("perl" ,perl)
       ("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)))))

(define-public qtquickcontrols
  (package (inherit qtsvg)
    (name "qtquickcontrols")
    (version "5.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0cpcrmz9n5b4bgmshmk093lirl9xwqb23inchnai1zqg21vrmqfq"))))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))))

(define-public qtquickcontrols2
  (package (inherit qtsvg)
    (name "qtquickcontrols2")
    (version "5.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0i8h933vhvx1bmniqdx0idg6vk82w9byd3dq0bb2phwjg5vv1xb3"))))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))))

(define-public qtgraphicaleffects
  (package (inherit qtsvg)
    (name "qtgraphicaleffects")
    (version "5.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qt.io/official_releases/qt/"
                                 (version-major+minor version) "/" version
                                 "/submodules/" name "-opensource-src-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1rwdjg5mk6xpadmxfq64xfp573zp5lrj9illb9105ra5wff565n8"))))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)))))

(define-public python-sip
  (package
    (name "python-sip")
    (version "4.18")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append "mirror://sourceforge/pyqt/sip/"
                         "sip-" version "/sip-" version ".tar.gz"))
        (sha256
         (base32
          "1dlw4kyiwd9bzmd1djm79c121r219abaz86lvizdk6ksq20mrp7i"))))
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
               (zero?
                 (system* "python" "configure.py"
                          "--bindir" bin
                          "--destdir" lib
                          "--incdir" include))))))))
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
    (version "5.6")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append "mirror://sourceforge/pyqt/PyQt5/"
                         "PyQt-" version "/PyQt5_gpl-"
                         version ".tar.gz"))
        (sha256
         (base32
          "1qgh42zsr9jppl9k7fcdbhxcd1wrb7wyaj9lng9nxfa19in1lj1f"))
       (patches (search-patches "pyqt-configure.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python-sip" ,python-sip)
       ("qtbase" ,qtbase))) ; for qmake
    (inputs
     `(("python" ,python-wrapper)))
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
                                        "/site-packages")))
               (zero? (system* "python" "configure.py"
                               "--confirm-license"
                               "--bindir" bin
                               "--destdir" lib
                               "--designer-plugindir" designer
                               "--qml-plugindir" qml
                               "--sipdir" sip))))))))
    (home-page "https://www.riverbankcomputing.com/software/pyqt/intro")
    (synopsis "Python bindings for Qt")
    (description
     "PyQt is a set of Python v2 and v3 bindings for the Qt application
framework.  The bindings are implemented as a set of Python modules and
contain over 620 classes.")
    (license license:gpl3)))

(define-public python2-pyqt
  (package (inherit python-pyqt)
    (name "python2-pyqt")
    (native-inputs
     `(("python-sip" ,python2-sip)
       ("qtbase" ,qtbase)))
    (inputs
     `(("python" ,python-2)))))

(define-public python-pyqt-5.5
  (package (inherit python-pyqt)
    (version "5.5")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append "mirror://sourceforge/pyqt/PyQt5/"
                         "PyQt-" version "/PyQt-gpl-"
                         version ".tar.gz"))
        (sha256
         (base32
          "056qmkv02wdcfblqdaxiswrgn4wa88sz22i1x58dpb1iniavplfd"))
       (patches (search-patches "pyqt-configure.patch"))))
    (native-inputs
     `(("python-sip" ,python-sip)
       ("qt" ,qt)))))

(define-public python2-pyqt-5.5
  (package (inherit python-pyqt-5.5)
    (name "python2-pyqt")
    (native-inputs
     `(("python-sip" ,python2-sip)
       ("qt" ,qt)))
    (inputs
     `(("python" ,python-2)))))

(define-public python-pyqt-4
  (package (inherit python-pyqt)
    (name "python-pyqt")
    (version "4.11.4")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append "mirror://sourceforge/pyqt/PyQt4/"
                         "PyQt-" version "/PyQt-x11-gpl-"
                         version ".tar.gz"))
        (sha256
         (base32
          "01zlviy5lq8g6db84wnvvpsrfnip9lbcpxagsyqa6as3jmsff7zw"))))
    (native-inputs
     `(("python-sip" ,python-sip)
       ("qt" ,qt-4)))
    (arguments
     `(#:tests? #f ; no check target
       #:modules ((srfi srfi-1)
                  ,@%gnu-build-system-modules)
       #:phases
         (alist-replace
         'configure
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
             (zero? (system* "python" "configure.py"
                             "--confirm-license"
                             "--bindir" bin
                             "--destdir" lib
                             "--sipdir" sip))))
         %standard-phases)))
    (license (list license:gpl2 license:gpl3)))) ; choice of either license

(define-public python2-pyqt-4
  (package (inherit python-pyqt-4)
           (name "python2-pyqt-4")
           (native-inputs
            `(("python-sip" ,python2-sip)
              ("qt" ,qt-4)))
           (inputs
            `(("python" ,python-2)))))

(define-public qtkeychain
  (package
    (name "qtkeychain")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/frankosterfeld/qtkeychain/"
                            "archive/v" version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0fka5q5cdzlf79igcjgbnb2smvwbwfasqawkzkbr34whispgm6lz"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("qttools" ,qttools)))
    (inputs
     `(("qtbase" ,qtbase)))
    (arguments
     `(#:tests? #f ; No tests included
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'set-qt-trans-dir
           (lambda _
             (substitute* "CMakeLists.txt"
              (("\\$\\{qt_translations_dir\\}")
               "${CMAKE_INSTALL_PREFIX}/share/qt/translations")))))))
    (home-page "https://github.com/frankosterfeld/qtkeychain")
    (synopsis "Qt API to store passwords")
    (description
      "QtKeychain is a Qt library to store passwords and other secret data
securely.  It will not store any data unencrypted unless explicitly requested.")
    (license license:bsd-3)))
