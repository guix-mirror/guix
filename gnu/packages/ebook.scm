;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017, 2019, 2020, 2021 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020, 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2021 la snesne <lasnesne@lagunposprasihopre.org>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
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

(define-module (gnu packages ebook)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages music)
  #:use-module (gnu packages pantheon)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages wxwidgets))

(define-public chmlib
  (package
    (name "chmlib")
    (version "0.40")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.jedrea.com/chmlib/chmlib-"
                                 version ".tar.bz2"))
             (sha256
               (base32
                "18zzb4x3z0d7fjh1x5439bs62dmgsi4c1pg3qyr7h5gp1i5xcj9l"))
             (patches (search-patches "chmlib-inttypes.patch"))))
    (build-system gnu-build-system)
    (home-page "http://www.jedrea.com/chmlib/")
    (synopsis "Library for CHM files")
    (description "CHMLIB is a library for dealing with ITSS/CHM format files.")
    (license license:lgpl2.1+)))

(define-public python-pychm
  (package
    (name "python-pychm")
    (version "0.8.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pychm" version))
       (sha256
        (base32
         "0wpn9ijlsmrpyiwg3drmgz4dms1i1i347adgqw37bkrh3vn6yq16"))))
    (build-system python-build-system)
    (inputs
     (list chmlib))
    (home-page "https://github.com/dottedmag/pychm")
    (synopsis "Handle CHM files")
    (description "This package provides a Python module for interacting
with Microsoft Compiled HTML (CHM) files")
    (license license:gpl2+)))

(define-public calibre
  (package
    (name "calibre")
    (version "5.21.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://download.calibre-ebook.com/"
                            version "/calibre-"
                            version ".tar.xz"))
        (sha256
         (base32
          "0mq2w8blq6ykaml812axakwkqcw85qcpfwijdikn7kvbrhnnp2s5"))
        (modules '((guix build utils)))
        (snippet
          '(begin
             ;; Unbundle python2-odfpy.
             (delete-file-recursively "src/odf")
             ;; Disable test that attempts to load it.
             (substitute* "setup/test.py"
               ((".*SRC, 'odf'.*")
                ""))

             ;; Remove unneeded resources.
             (delete-file "resources/mozilla-ca-certs.pem")
             (delete-file "resources/calibre-portable.bat")
             (delete-file "resources/calibre-portable.sh")
             #t))
        (patches (search-patches "calibre-fix-zeroconf.patch"
                                 "calibre-no-updates-dialog.patch"
                                 "calibre-remove-test-sqlite.patch" ; TODO: fix test.
                                 "calibre-remove-test-unrar.patch"))))
    (build-system python-build-system)
    (native-inputs
     (list pkg-config
           qtbase-5 ; for qmake
           python-flake8
           python-pyqt-builder
           xdg-utils))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("font-liberation" ,font-liberation)
       ("glib" ,glib)
       ("hunspell" ,hunspell)
       ("hyphen" ,hyphen)
       ("icu4c" ,icu4c)
       ("libmtp" ,libmtp)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg-turbo)
       ("libjxr" ,libjxr)
       ("libusb" ,libusb)
       ("openssl" ,openssl)
       ("optipng" ,optipng)
       ("podofo" ,podofo)
       ("poppler" ,poppler)
       ("python-apsw" ,python-apsw)
       ("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-cchardet" ,python-cchardet)
       ("python-css-parser" ,python-css-parser)
       ("python-cssselect" ,python-cssselect)
       ("python-dateutil" ,python-dateutil)
       ("python-dbus" ,python-dbus)
       ("python-dnspython" ,python-dnspython-1.16)
       ("python-dukpy" ,python-dukpy)
       ("python-feedparser" ,python-feedparser)
       ("python-html2text" ,python-html2text)
       ("python-html5-parser" ,python-html5-parser)
       ("python-html5lib" ,python-html5lib)
       ("python-lxml" ,python-lxml)
       ("python-markdown" ,python-markdown)
       ("python-mechanize" ,python-mechanize)
       ;; python-msgpack is needed for the network content server to work.
       ("python-msgpack" ,python-msgpack)
       ("python-netifaces" ,python-netifaces)
       ("python-odfpy" ,python-odfpy)
       ("python-pillow" ,python-pillow)
       ("python-psutil" ,python-psutil)
       ("python-py7zr" ,python-py7zr)
       ("python-pychm" ,python-pychm)
       ("python-pycryptodome" ,python-pycryptodome)
       ("python-pygments" ,python-pygments)
       ("python-pyqt-without-qtwebkit" ,python-pyqt-without-qtwebkit)
       ("python-pyqtwebengine" ,python-pyqtwebengine)
       ("python-regex" ,python-regex)
       ("python-speechd" ,speech-dispatcher)
       ("python-zeroconf" ,python-zeroconf)
       ("qtwebengine" ,qtwebengine)
       ("sqlite" ,sqlite)))
    (arguments
     `(;; Calibre is using setuptools by itself, but the setup.py is not
       ;; compatible with the shim wrapper (taken from pip) we are using.
       #:use-setuptools? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "src/calibre/linux.py"
               ;; We can't use the uninstaller in Guix. Don't build it.
               (("self\\.create_uninstaller()") ""))
             #t))
         (add-after 'patch-source-shebangs 'patch-more-shebangs
           (lambda _
             ;; Patch various inline shebangs.
             (substitute* '("src/calibre/gui2/preferences/tweaks.py"
                            "src/calibre/gui2/dialogs/custom_recipes.py"
                            "setup/install.py"
                            "setup/linux-installer.sh")
               (("#!/usr/bin/env python")
                (string-append "#!" (which "python")))
               (("#!/bin/sh")
                (string-append "#!" (which "sh"))))
             #t))
         (add-after 'unpack 'dont-load-remote-icons
           (lambda _
             (substitute* "setup/plugins_mirror.py"
               (("href=\"//calibre-ebook.com/favicon.ico\"")
                "href=\"favicon.ico\""))
             #t))
         (add-before 'build 'configure
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((podofo (assoc-ref inputs "podofo"))
                  (pyqt (assoc-ref inputs "python-pyqt-without-qtwebkit"))
                  (python-sip (assoc-ref inputs "python-sip"))
                  (out (assoc-ref outputs "out")))
              (substitute* "setup/build.py"
                (("\\[tool.sip.bindings.pictureflow\\]")
                 "[tool.sip.bindings.pictureflow]
tags = [\"WS_X11\"]")
                (("\\[tool.sip.project\\]")
                 (string-append "[tool.sip.project]
sip-include-dirs = [\"" pyqt "/share/sip" "\"]")))
              (substitute* "src/calibre/ebooks/pdf/pdftohtml.py"
                (("PDFTOHTML = 'pdftohtml'")
                 (string-append "PDFTOHTML = \"" (assoc-ref inputs "poppler")
                                "/bin/pdftohtml\"")))
              ;; get_exe_path looks in poppler's output for these binaries. Make
              ;; it not do that.
              (substitute* "src/calibre/utils/img.py"
                (("get_exe_path..jpegtran..") (string-append "'" (which "jpegtran") "'"))
                (("get_exe_path..cjpeg..") (string-append "'" (which "cjpeg") "'"))
                (("get_exe_path..optipng..") (string-append "'" (which "optipng") "'"))
                (("get_exe_path..JxrDecApp..") (string-append "'" (which "JxrDecApp") "'")))
              ;; Calibre thinks we are installing desktop files into a home
              ;; directory, but here we butcher the script in to installing
              ;; to calibres /share directory.
              (setenv "XDG_DATA_HOME" (string-append out "/share"))
              (substitute* "src/calibre/linux.py"
                (("'~/.local/share'") "''"))

              ;; 'python setup.py rapydscript' uses QtWebEngine, which
              ;; needs to create temporary files in $HOME.
              (setenv "HOME" "/tmp")

              ;; XXX: QtWebEngine will fail if no fonts are available.  This
              ;; can likely be removed when fontconfig has been patched to
              ;; include TrueType fonts by default.
              (symlink (string-append (assoc-ref inputs "font-liberation")
                                      "/share/fonts")
                       "/tmp/.fonts")

              (setenv "PODOFO_INC_DIR" (string-append podofo "/include/podofo"))
              (setenv "PODOFO_LIB_DIR" (string-append podofo "/lib"))
              ;; This informs the tests we are a continuous integration
              ;; environment and thus have no networking.
              (setenv "CI" "true")
              ;; The Qt test complains about being unable to load all image plugins, and I
              ;; notice the available plugins list it shows lacks 'svg'. Adding qtsvg doesn't
              ;; fix it, so I'm not sure how to fix it.  TODO: Fix test and remove this.
              (setenv "SKIP_QT_BUILD_TEST" "true")
              #t)))
         (add-after 'install 'install-rapydscript
           (lambda* (#:key inputs #:allow-other-keys)
              ;; Unset so QtWebengine doesn't dump temporary files here.
             (unsetenv "XDG_DATA_HOME")
             (invoke "python" "setup.py" "rapydscript")
             #t))
         (add-after 'install 'install-man-pages
           (lambda* (#:key outputs #:allow-other-keys)
             (copy-recursively
              "man-pages"
              (string-append (assoc-ref outputs "out") "/share/man"))
             #t))
         ;; The font TTF files are used in some miscellaneous tests, so we
         ;; unbundle them here to avoid patching the tests.
         (add-after 'install 'unbundle-font-liberation
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((font-dest (string-append (assoc-ref outputs "out")
                                             "/share/calibre/fonts/liberation"))
                   (font-src (string-append (assoc-ref inputs "font-liberation")
                                            "/share/fonts/truetype")))
               (delete-file-recursively font-dest)
               (symlink font-src font-dest))
             #t))
         ;; Make run-time dependencies available to the binaries.
         (add-after 'wrap 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (qtwebengine (assoc-ref inputs "qtwebengine")))
               (with-directory-excursion (string-append out "/bin")
                 (for-each
                  (lambda (binary)
                    (wrap-program binary
                      ;; Make QtWebEngineProcess available.
                      `("QTWEBENGINEPROCESS_PATH" =
                        ,(list (string-append
                                qtwebengine
                                "/lib/qt5/libexec/QtWebEngineProcess")))))
                  ;; Wrap all the binaries shipping with the package, except
                  ;; for the wrappings created during the 'wrap standard
                  ;; phase.  This extends existing .calibre-real wrappers
                  ;; rather than create ..calibre-real-real-s.  For more
                  ;; information see: https://issues.guix.gnu.org/43249.
                  (find-files "." (lambda (file stat)
                                    (not (wrapped-program? file)))))))
             #t)))))
    (home-page "https://calibre-ebook.com/")
    (synopsis "E-book library management software")
    (description "Calibre is an e-book library manager.  It can view, convert
and catalog e-books in most of the major e-book formats.  It can also talk
to many e-book reader devices.  It can go out to the Internet and fetch
metadata for books.  It can download newspapers and convert them into
e-books for convenient reading.")
    ;; Calibre is largely GPL3+, but includes a number of components covered
    ;; by other licenses. See COPYRIGHT for more details.
    (license (list license:gpl3+
                   license:gpl2+
                   license:lgpl2.1+
                   license:lgpl2.1
                   license:bsd-3
                   license:expat
                   license:zpl2.1
                   license:asl2.0
                   license:public-domain
                   license:silofl1.1
                   license:cc-by-sa3.0))))

(define-public ebook-tools
  (package
    (name "ebook-tools")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/ebook-tools/ebook-tools/"
                           version "/ebook-tools-" version ".tar.gz"))
       (sha256
        (base32
         "1bi7wsz3p5slb43kj7lgb3r6lb91lvb6ldi556k4y50ix6b5khyb"))))
    (arguments
     `(#:tests? #f)) ; No 'test' target
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libzip libxml2))
    (home-page "http://ebook-tools.sourceforge.net")
    (synopsis "Tools and library for dealing with various ebook file formats")
    (description "This package provides command-line tools and a library for
accessing and converting various ebook file formats.")
    (license license:expat)))

(define-public inkbox
  (package
    (name "inkbox")
    (version "1.7")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://alpinekobox.ddns.net/InkBox/inkbox/")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "126cqn0ixcn608lv2hd9f7zmzj4g448bnpxc7wv9cvg83qqajh5n"))))
    (build-system qt-build-system)
    (arguments
     '(#:tests? #f                      ; no test suite
       #:make-flags
       (list (string-append "PREFIX="
                            (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prefix-opt
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "inkbox.pro"
               (("/opt/\\$\\$\\{TARGET\\}") (string-append (assoc-ref outputs "out"))))))
         (replace 'configure
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke (cons "qmake" make-flags)))))))
    (native-inputs
     (list qtbase-5))
    (home-page "https://alpinekobox.ddns.net/InkBox/inkbox/")
    (synopsis "EBook reader")
    (description "This package provides InkBox eBook reader.")
    (license license:gpl3)))

(define-public liblinebreak
  (package
    (name "liblinebreak")
    (version "2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/vimgadgets"
                                  "/liblinebreak/" version
                                  "/liblinebreak-" version ".tar.gz"))
              (sha256
               (base32
                "1f36dbq7nc77lln1by2n1yl050g9dc63viawhs3gc3169mavm36x"))))
    (build-system gnu-build-system)
    (home-page "http://vimgadgets.sourceforge.net/liblinebreak/")
    (synopsis "Library for detecting where linebreaks are allowed in text")
    (description "@code{liblinebreak} is an implementation of the line
breaking algorithm as described in Unicode 6.0.0 Standard Annex 14,
Revision 26.  It breaks lines that contain Unicode characters.  It is
designed to be used in a generic text renderer.")
    (license license:zlib)))

(define-public fbreader
  (package
    (name "fbreader")
    (version "0.99.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/geometer/FBReader")
                     (commit (string-append version "-freebsdport"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c0s4silpax74kwfz3dfmzn4lkv6jsyb800vfak166vii0hvbv3d"))
              (patches (search-patches "fbreader-curl-7.62.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("curl" ,curl)
       ("expat" ,expat)
       ("fribidi" ,fribidi)
       ("glib" ,glib)
       ("gtk+-2" ,gtk+-2)
       ("libjpeg" ,libjpeg-turbo)
       ("liblinebreak" ,liblinebreak)
       ("libxft" ,libxft)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (native-inputs
     `(("gcc@5" ,gcc-5)
       ("pkg-config" ,pkg-config)))
    (arguments
     `(#:tests? #f ; No tests exist.
       #:make-flags `("CC=gcc" "TARGET_ARCH=desktop" "UI_TYPE=gtk"
                      "TARGET_STATUS=release"
                      ,(string-append "INSTALLDIR="
                                      (assoc-ref %outputs "out"))
                      ,(string-append "LDFLAGS=-Wl,-rpath="
                                      (assoc-ref %outputs "out") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'augment-CPLUS_INCLUDE_PATH
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Hide the default GCC from CPLUS_INCLUDE_PATH to prevent a header
             ;; conflict with the GCC provided in native-inputs.
             (let ((gcc (assoc-ref inputs "gcc")))
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join
                        (delete (string-append gcc "/include/c++")
                                (string-split (getenv "CPLUS_INCLUDE_PATH")
                                              #\:))
                        ":"))
               #t)))
         (delete 'configure)
         (add-after 'unpack 'fix-install-locations
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "fbreader/desktop/Makefile"
                 (("/usr") out))
               #t))))))
    (home-page "https://fbreader.org/")
    (synopsis "E-Book reader")
    (description "@code{fbreader} is an E-Book reader.  It supports the
following formats:

@enumerate
@item CHM
@item Docbook
@item FB2
@item HTML
@item OEB
@item PDB
@item RTF
@item TCR
@item TXT
@item XHTML
@end enumerate")
    (license license:gpl2+)))

(define-public cozy
  (package
    (name "cozy")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/geigi/cozy")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hifzzhhf0ww6iar9gswjfndy3i54s6jc41zaazlx4scc7r6fhs0"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-desktop-file
           (lambda _
             (substitute* "data/com.github.geigi.cozy.desktop"
               (("Exec=com.github.geigi.cozy") "Exec=cozy"))))
         (add-after 'install 'patch-executable-name
           (lambda* (#:key outputs #:allow-other-keys)
             (with-directory-excursion
                 (string-append (assoc-ref outputs "out") "/bin")
               (rename-file "com.github.geigi.cozy" "cozy"))))
         (add-after 'wrap 'wrap-libs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out               (assoc-ref outputs "out"))
                    (pylib             (string-append
                                        out "/lib/python"
                                        ,(version-major+minor
                                          (package-version python))
                                        "/site-packages"))
                    (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                    (gst-plugin-path   (getenv "GST_PLUGIN_SYSTEM_PATH"))
                    (libmagic-path     (string-append
                                        (assoc-ref %build-inputs "file")
                                        "/lib"))
                    (python-path     (getenv "GUIX_PYTHONPATH")))
               (wrap-program (string-append out "/bin/cozy")
                 `("LD_LIBRARY_PATH" ":" prefix (,libmagic-path))
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))
                 `("GUIX_PYTHONPATH" ":" prefix (,python-path ,pylib)))))))))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     (list file
           granite
           gsettings-desktop-schemas
           gst-libav
           gst-plugins-bad
           gst-plugins-good
           gst-plugins-ugly
           gtk+
           libdazzle
           libgee
           libhandy
           python-distro
           python-gst
           python-mutagen
           python-packaging
           python-peewee
           python-pycairo
           python-pygobject
           python-pytz
           python-requests))
    (home-page "https://cozy.geigi.de/")
    (synopsis "Modern audiobook player using GTK+")
    (description
     "Cozy is a modern audiobook player written in GTK+.

Some of the current features:

@itemize
@item Import your audiobooks into Cozy to browse them comfortably
@item Sort your audio books by author, reader & name
@item Remembers your playback position
@item Sleep timer
@item Playback speed control
@item Search your library
@item Offline mode
@item Add multiple storage locations
@item Drag & Drop to import new audio books
@item Support for DRM free mp3, m4a (aac, ALAC, …), flac, ogg, opus, wav files
@item Mpris integration (Media keys & playback info for desktop environment)
@end itemize")
    ;; TODO: Unbundle python-inject.
    (license (list license:gpl3+ ;cozy
                   license:asl2.0)))) ;python-inject (bundled dependency)

(define-public xchm
  (package
    (name "xchm")
    (version "1.33")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rzvncj/xCHM"
                                  "/releases/download/"
                                  version "/xchm-" version ".tar.gz"))
              (sha256
               (base32
                "0an09shap2wj9gzj5fsw5sc2i6paq3kc3mc52fnwg2bb2dan5qxk"))))
    (build-system gnu-build-system)
    (inputs
     (list wxwidgets chmlib))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/rzvncj/xCHM")
    (synopsis "CHM file viewer")
    (description "xCHM is a graphical CHM file viewer.  It is a frontend to
the CHM library CHMLIB.")
    (license license:gpl2+)))

(define-public libmobi
  (package
    (name "libmobi")
    (version "0.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bfabiszewski/libmobi/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0yps72cm609xn2k7alflkdhp9kgr1w7zzyxjygz0n1kqrdcplihh"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (inputs
     (list zlib libxml2))
    (home-page "https://github.com/bfabiszewski/libmobi/")
    (synopsis "C library for handling MOBI formats")
    (description "Libmobi is a C library for handling MOBI ebook
format documents, with the following features:

@itemize
@item reading and parsing:
@itemize
@item some older text Palmdoc formats (pdb),
@item Mobipocket files (prc, mobi),
@item newer MOBI files including KF8 format (azw, azw3),
@item Replica Print files (azw4)
@end itemize
@item recreating source files using indices
@item reconstructing references (links and embedded) in html files
@item reconstructing source structure that can be fed back to kindlegen
@item reconstructing dictionary markup (orth, infl tags)
@item writing back loaded documents
@item metadata editing
@item handling encrypted documents
@end itemize\n")
    (license license:lgpl3+)))
