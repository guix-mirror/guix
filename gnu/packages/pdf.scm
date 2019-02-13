;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015, 2016, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2016 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017, 2018 Rene Saavedra <pacoon@protonmail.com>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages pdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public poppler
  (package
   (name "poppler")
   (version "0.72.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://poppler.freedesktop.org/poppler-"
                                version ".tar.xz"))
            (sha256
             (base32
              "0lfs1b1jfamxl13zbl5n448dqvl9n8frbv8180y7b7kfyaw7wx61"))))
   (build-system cmake-build-system)
   ;; FIXME:
   ;;  use libcurl:        no
   (inputs `(("fontconfig" ,fontconfig)
             ("freetype" ,freetype)
             ("libjpeg" ,libjpeg-turbo)
             ("libpng" ,libpng)
             ("libtiff" ,libtiff)
             ("lcms" ,lcms)
             ("openjpeg" ,openjpeg)
             ("zlib" ,zlib)

             ;; To build poppler-glib (as needed by Evince), we need Cairo and
             ;; GLib.  But of course, that Cairo must not depend on Poppler.
             ("cairo" ,(package (inherit cairo)
                         (inputs (alist-delete "poppler"
                                               (package-inputs cairo)))))))
   (propagated-inputs
    ;; As per poppler-cairo and poppler-glib.pc.
    ;; XXX: Ideally we'd propagate Cairo too, but that would require a
    ;; different solution to the circular dependency mentioned above.
    `(("glib" ,glib)))
   (native-inputs
      `(("pkg-config" ,pkg-config)
        ("glib" ,glib "bin")                      ; glib-mkenums, etc.
        ("gobject-introspection" ,gobject-introspection)))
   (arguments
    `(#:tests? #f ; no test data provided with the tarball
      #:configure-flags
      (let* ((out (assoc-ref %outputs "out"))
             (lib (string-append out "/lib")))
        (list "-DENABLE_XPDF_HEADERS=ON" ; to install header files
              "-DENABLE_ZLIB=ON"
              (string-append "-DCMAKE_INSTALL_LIBDIR=" lib)
              (string-append "-DCMAKE_INSTALL_RPATH=" lib)))))
   (synopsis "PDF rendering library")
   (description
    "Poppler is a PDF rendering library based on the xpdf-3.0 code base.")
   (license license:gpl2+)
   (home-page "https://poppler.freedesktop.org/")))

(define-public poppler-data
  (package
    (name "poppler-data")
    (version "0.4.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://poppler.freedesktop.org/poppler-data"
                                  "-" version ".tar.gz"))
              (sha256
               (base32
                "04i0wgdkn5lhda8cyxd1ll4a2p41pwqrwd47n9mdpl7cx5ypx70z"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no test suite
       #:make-flags (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         ;; The package only provides some data files, so there is nothing to
         ;; build.
         (delete 'configure)
         (delete 'build))))
    (synopsis "Poppler encoding files for rendering of CJK and Cyrillic text")
    (description "This package provides optional encoding files for Poppler.
When present, Poppler is able to correctly render CJK and Cyrillic text.")
    (home-page (package-home-page poppler))
    ;; See COPYING in the source distribution for more information about
    ;; the licensing.
    (license (list license:bsd-3
                   license:gpl2))))

(define-public poppler-qt4
  (package/inherit poppler
   (name "poppler-qt4")
   (inputs `(("qt-4" ,qt-4)
             ,@(package-inputs poppler)))
   (synopsis "Qt4 frontend for the Poppler PDF rendering library")))

(define-public poppler-qt5
  (package/inherit poppler
   (name "poppler-qt5")
   (inputs `(("qtbase" ,qtbase)
             ,@(package-inputs poppler)))
   (synopsis "Qt5 frontend for the Poppler PDF rendering library")))

(define-public python-poppler-qt5
  (package
    (name "python-poppler-qt5")
    (version "0.24.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-poppler-qt5" version))
        (sha256
         (base32
          "0l69llw1fzwz8y90q0qp9q5pifbrqjjbwii7di54dwghw5fc6w1r"))))
    (build-system python-build-system)
    (arguments
     `(;; There are no tests.  The check phase just causes a rebuild.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "setup.py"
               ;; This check always fails, so disable it.
               (("if not check_qtxml\\(\\)")
                "if True")
               ;; Enable C++11, which is needed because of Qt5.
               (("\\*\\*ext_args" line)
                (string-append "extra_compile_args=['-std=gnu++11'], " line)))
             ;; We need to pass an extra flag here.  This cannot be in
             ;; configure-flags because it should not be passed for the
             ;; installation phase.
             ((@@ (guix build python-build-system) call-setuppy)
              "build_ext" (list (string-append "--pyqt-sip-dir="
                                               (assoc-ref inputs "python-pyqt")
                                               "/share/sip")) #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("python-sip" ,python-sip)
       ("python-pyqt" ,python-pyqt)
       ("poppler-qt5" ,poppler-qt5)
       ("qtbase" ,qtbase)))
    (home-page "https://pypi.python.org/pypi/python-poppler-qt5")
    (synopsis "Python bindings for Poppler-Qt5")
    (description
     "This package provides Python bindings for the Qt5 interface of the
Poppler PDF rendering library.")
    (license license:lgpl2.1+)))

(define-public libharu
  (package
   (name "libharu")
   (version "2.3.0")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/libharu/libharu.git")
                   (commit (string-append
                            "RELEASE_"
                            (string-join (string-split version #\.) "_")))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "15s9hswnl3qqi7yh29jyrg0hma2n99haxznvcywmsp8kjqlyg75q"))))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags
      (list (string-append "--with-zlib="
                           (assoc-ref %build-inputs "zlib"))
            (string-append "--with-png="
                           (assoc-ref %build-inputs "libpng")))))
   (inputs
    `(("zlib" ,zlib)
      ("libpng" ,libpng)))
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("libtool" ,libtool)))
   (home-page "http://libharu.org/")
   (synopsis "Library for generating PDF files")
   (description
    "libHaru is a library for generating PDF files.  libHaru does not support
reading and editing of existing PDF files.")
   (license license:zlib)))

(define-public xpdf
  (package
   (name "xpdf")
   (version "3.04")
   (source (origin
            (method url-fetch)
            (uri (string-append "ftp://ftp.foolabs.com/pub/xpdf/xpdf-"
                                version ".tar.gz"))
            (sha256 (base32
                     "1rbp54mr3z2x3a3a1qmz8byzygzi223vckfam9ib5g1sfds0qf8i"))))
   (build-system gnu-build-system)
   (inputs `(("freetype" ,freetype)
             ("gs-fonts" ,gs-fonts)
             ("lesstif" ,lesstif)
             ("libpaper" ,libpaper)
             ("libx11" ,libx11)
             ("libxext" ,libxext)
             ("libxp" ,libxp)
             ("libxpm" ,libxpm)
             ("libxt" ,libxt)
             ("libpng" ,libpng)
             ("zlib" ,zlib)))
   (arguments
    `(#:tests? #f ; there is no check target
      #:parallel-build? #f ; build fails randomly on 8-way machines
      #:configure-flags
        (list (string-append "--with-freetype2-includes="
                             (assoc-ref %build-inputs "freetype")
                             "/include/freetype2"))
      #:phases
      (modify-phases %standard-phases
        (replace 'install
          (lambda* (#:key outputs inputs #:allow-other-keys #:rest args)
            (let* ((install (assoc-ref %standard-phases 'install))
                   (out (assoc-ref outputs "out"))
                   (xpdfrc (string-append out "/etc/xpdfrc"))
                   (gs-fonts (assoc-ref inputs "gs-fonts")))
              (apply install args)
              (substitute* xpdfrc
                (("/usr/local/share/ghostscript/fonts")
                 (string-append gs-fonts "/share/fonts/type1/ghostscript"))
                (("#fontFile") "fontFile")))
            #t)))))
   (synopsis "Viewer for PDF files based on the Motif toolkit")
   (description
    "Xpdf is a viewer for Portable Document Format (PDF) files.")
   (license license:gpl3) ; or gpl2, but not gpl2+
   (home-page "http://www.foolabs.com/xpdf/")))

(define-public zathura-cb
  (package
    (name "zathura-cb")
    (version "0.1.8")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura-cb/download/zathura-cb-"
                              version ".tar.xz"))
              (sha256
               (base32
                "1i6cf0vks501cggwvfsl6qb7mdaf3sszdymphimfvnspw810faj5"))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("libarchive" ,libarchive)
              ("zathura" ,zathura)))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f                      ; package does not contain tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-plugin-directory
           ;; Something of a regression in 0.1.8: the new Meson build system
           ;; now hard-codes an incorrect plugin directory.  Fix it.
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "meson.build"
               (("(install_dir:).*" _ key)
                (string-append key
                               "'" (assoc-ref outputs "out") "/lib/zathura'\n")))
             #t)))))
    (home-page "https://pwmt.org/projects/zathura-cb/")
    (synopsis "Comic book support for zathura (libarchive backend)")
    (description "The zathura-cb plugin adds comic book support to zathura
using libarchive.")
    (license license:zlib)))

(define-public zathura-ps
  (package
    (name "zathura-ps")
    (version "0.2.6")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura-ps/download/zathura-ps-"
                              version ".tar.xz"))
              (sha256
               (base32
                "0wygq89nyjrjnsq7vbpidqdsirjm6iq4w2rijzwpk2f83ys8bc3y"))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("libspectre" ,libspectre)
              ("zathura" ,zathura)))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f                      ; package does not contain tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-plugin-directory
           ;; Something of a regression in 0.2.6: the new Meson build system
           ;; now hard-codes an incorrect plugin directory.  Fix it.
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "meson.build"
               (("(install_dir:).*" _ key)
                (string-append key
                               "'" (assoc-ref outputs "out") "/lib/zathura'\n")))
             #t)))))
    (home-page "https://pwmt.org/projects/zathura-ps/")
    (synopsis "PS support for zathura (libspectre backend)")
    (description "The zathura-ps plugin adds PS support to zathura
using libspectre.")
    (license license:zlib)))

(define-public zathura-djvu
  (package
    (name "zathura-djvu")
    (version "0.2.8")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura-djvu/download/zathura-djvu-"
                              version ".tar.xz"))
              (sha256
               (base32
                "0axkv1crdxn0z44whaqp2ibkdqcykhjnxk7qzms0dp1b67an9rnh"))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("djvulibre" ,djvulibre)
       ("zathura" ,zathura)))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f                      ; package does not contain tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-plugin-directory
           ;; Something of a regression in 0.2.8: the new Meson build system
           ;; now hard-codes an incorrect plugin directory.  Fix it.
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "meson.build"
               (("(install_dir:).*" _ key)
                (string-append key
                               "'" (assoc-ref outputs "out") "/lib/zathura'\n")))
             #t)))))
    (home-page "https://pwmt.org/projects/zathura-djvu/")
    (synopsis "DjVu support for zathura (DjVuLibre backend)")
    (description "The zathura-djvu plugin adds DjVu support to zathura
using the DjVuLibre library.")
    (license license:zlib)))

(define-public zathura-pdf-mupdf
  (package
    (name "zathura-pdf-mupdf")
    (version "0.3.4")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura-pdf-mupdf"
                              "/download/zathura-pdf-mupdf-" version ".tar.xz"))
              (sha256
               (base32
                "166d5nz47ixzwj4pixsd5fd9qvjf5v34cdqi3p72vr23pswk2hyn"))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("jbig2dec" ,jbig2dec)
       ("libjpeg" ,libjpeg)
       ("mujs" ,mujs)
       ("mupdf" ,mupdf)
       ("openjpeg" ,openjpeg)
       ("openssl" ,openssl)
       ("zathura" ,zathura)))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f                      ; package does not contain tests
       #:configure-flags (list (string-append "-Dplugindir="
                                              (assoc-ref %outputs "out")
                                              "/lib/zathura")
                               "-Dlink-external=true")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'add-mujs-to-dependencies
           (lambda _
             ;; Add mujs to the 'build_dependencies'.
             (substitute* "meson.build"
               (("^  libopenjp2 = dependency.*" x)
                (string-append x "  mujs = cc.find_library('mujs')\n"))
               (("^    libopenjp2")
                "    libopenjp2, mujs")))))))
    (home-page "https://pwmt.org/projects/zathura-pdf-mupdf/")
    (synopsis "PDF support for zathura (mupdf backend)")
    (description "The zathura-pdf-mupdf plugin adds PDF support to zathura
by using the @code{mupdf} rendering library.")
    (license license:zlib)))

(define-public zathura-pdf-poppler
  (package
    (name "zathura-pdf-poppler")
    (version "0.2.9")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura-pdf-poppler/download/zathura-pdf-poppler-"
                              version ".tar.xz"))
              (sha256
               (base32
                "1p4jcny0jniygns78mcf0nlm298dszh49qpmjmackrm6dq8hc25y"))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("poppler" ,poppler)
       ("zathura" ,zathura)))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f                      ; package does not include tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-plugin-directory
           ;; Something of a regression in 0.2.9: the new Meson build system
           ;; now hard-codes an incorrect plugin directory.  Fix it.
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "meson.build"
               (("(install_dir:).*" _ key)
                (string-append key
                               "'" (assoc-ref outputs "out") "/lib/zathura'\n")))
             #t)))))
    (home-page "https://pwmt.org/projects/zathura-pdf-poppler/")
    (synopsis "PDF support for zathura (poppler backend)")
    (description "The zathura-pdf-poppler plugin adds PDF support to zathura
by using the poppler rendering engine.")
    (license license:zlib)))

(define-public zathura
  (package
    (name "zathura")
    (version "0.4.3")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura/download/zathura-"
                              version ".tar.xz"))
              (sha256
               (base32
                "0hgx5x09i6d0z45llzdmh4l348fxh1y102sb1w76f2fp4r21j4ky"))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("gettext" ,gettext-minimal)
                     ("glib:bin" ,glib "bin")

                     ;; For building documentation.
                     ("python-sphinx" ,python-sphinx)

                     ;; For tests.
                     ("check" ,check)
                     ("xorg-server" ,xorg-server-for-tests)))
    (inputs `(("sqlite" ,sqlite)))
    ;; Listed in 'Requires.private' of 'zathura.pc'.
    (propagated-inputs `(("cairo" ,cairo)
                         ("girara" ,girara)))
    (native-search-paths
     (list (search-path-specification
            (variable "ZATHURA_PLUGINS_PATH")
            (files '("lib/zathura")))))
    (build-system meson-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'check 'start-xserver
                    ;; Tests require a running X server.
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((xorg-server (assoc-ref inputs "xorg-server"))
                            (display ":1"))
                        (setenv "DISPLAY" display)

                        ;; On busy machines, tests may take longer than
                        ;; the default of four seconds.
                        (setenv "CK_DEFAULT_TIMEOUT" "20")

                        ;; Don't fail due to missing '/etc/machine-id'.
                        (setenv "DBUS_FATAL_WARNINGS" "0")
                        (zero? (system (string-append xorg-server "/bin/Xvfb "
                                                      display " &")))))))))
    (home-page "https://pwmt.org/projects/zathura/")
    (synopsis "Lightweight keyboard-driven PDF viewer")
    (description "Zathura is a customizable document viewer.  It provides a
minimalistic interface and an interface that mainly focuses on keyboard
interaction.")
    (license license:zlib)))

(define-public podofo
  (package
    (name "podofo")
    (version "0.9.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/podofo/podofo/" version
                                  "/podofo-" version ".tar.gz"))
              (sha256
               (base32
                "0wj0y4zcmj4q79wrn3vv3xq4bb0vhhxs8yifafwy9f2sjm83c5p9"))
              (patches (search-patches "podofo-cmake-3.12.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("cppunit" ,cppunit)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libjpeg" ,libjpeg)
       ("libtiff" ,libtiff)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("libpng" ,libpng)
       ("lua" ,lua-5.1)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (arguments
     `(#:configure-flags '("-DPODOFO_BUILD_SHARED=ON"
                           "-DPODOFO_BUILD_STATIC=ON")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((freetype (assoc-ref inputs "freetype")))
               ;; Look for freetype include files in the correct place.
               (substitute* "cmake/modules/FindFREETYPE.cmake"
                 (("/usr/local") freetype)))
             #t)))))
    (home-page "http://podofo.sourceforge.net")
    (synopsis "Tools to work with the PDF file format")
    (description
     "PoDoFo is a C++ library and set of command-line tools to work with the
PDF file format.  It can parse PDF files and load them into memory, and makes
it easy to modify them and write the changes to disk.  It is primarily useful
for applications that wish to do lower level manipulation of PDF, such as
extracting content or merging files.")
    (license license:lgpl2.0+)))

(define-public mupdf
  (package
    (name "mupdf")
    (version "1.14.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://mupdf.com/downloads/archive/"
                            name "-" version "-source.tar.xz"))
        (sha256
         (base32
          "1psnz02w5p7wc1s1ma7vvjmkjfy641xvsh9ykaqzkk84dflnjgk0"))
        (modules '((guix build utils)))
        (snippet
         ;; We keep lcms2 since it is different than our lcms.
         '(begin
            (for-each
              (lambda (dir)
                (delete-file-recursively (string-append "thirdparty/" dir)))
              '("curl" "freeglut" "freetype" "harfbuzz" "jbig2dec"
                "libjpeg" "mujs" "openjpeg" "zlib"))
                #t))))
    (build-system gnu-build-system)
    (inputs
      `(("curl" ,curl)
        ("freeglut" ,freeglut)
        ("freetype" ,freetype)
        ("harfbuzz" ,harfbuzz)
        ("jbig2dec" ,jbig2dec)
        ("libjpeg" ,libjpeg)
        ("libx11" ,libx11)
        ("libxext" ,libxext)
        ("mujs" ,mujs)
        ("openjpeg" ,openjpeg)
        ("openssl" ,openssl)
        ("zlib" ,zlib)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (arguments
      '(#:tests? #f ; no check target
        #:make-flags (list "CC=gcc"
                           "XCFLAGS=-fpic"
                           "USE_SYSTEM_LIBS=yes"
                           "USE_SYSTEM_MUJS=yes"
                           (string-append "prefix=" (assoc-ref %outputs "out")))
        #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (home-page "https://mupdf.com")
    (synopsis "Lightweight PDF viewer and toolkit")
    (description
      "MuPDF is a C library that implements a PDF and XPS parsing and
rendering engine.  It is used primarily to render pages into bitmaps,
but also provides support for other operations such as searching and
listing the table of contents and hyperlinks.

The library ships with a rudimentary X11 viewer, and a set of command
line tools for batch rendering @command{pdfdraw}, rewriting files
@command{pdfclean}, and examining the file structure @command{pdfshow}.")
    (license (list license:agpl3+
                   license:bsd-3 ; resources/cmaps
                   license:x11 ; thirdparty/lcms2
                   license:silofl1.1 ; resources/fonts/{han,noto,sil,urw}
                   license:asl2.0)))) ; resources/fonts/droid

(define-public qpdf
  (package
   (name "qpdf")
   (version "8.4.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/qpdf/qpdf/" version
                                "/qpdf-" version ".tar.gz"))
            (sha256
             (base32
              "1864p952m8vzxk6v500a42psbqj2g2gyli3d3zj6h33hzwxqy09r"))
            (modules '((guix build utils)))
            (snippet
             ;; Replace shebang with the bi-lingual shell/Perl trick to remove
             ;; dependency on Perl.
             '(begin
                (substitute* "qpdf/fix-qdf"
                  (("#!/usr/bin/env perl")
                   "\
eval '(exit $?0)' && eval 'exec perl -wS \"$0\" ${1+\"$@\"}'
  & eval 'exec perl -wS \"$0\" $argv:q'
    if 0;\n"))
                #t))))
   (build-system gnu-build-system)
   (arguments
    `(#:disallowed-references (,perl)
      #:phases
      (modify-phases %standard-phases
        (add-before 'configure 'patch-paths
          (lambda _
            (substitute* "make/libtool.mk"
              (("SHELL=/bin/bash")
               (string-append "SHELL=" (which "bash"))))
            (substitute* (append
                          '("qtest/bin/qtest-driver")
                          (find-files "." "\\.test"))
              (("/usr/bin/env") (which "env")))
            #t)))))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("perl" ,perl)))
   (propagated-inputs
    ;; In Requires.private of libqpdf.pc.
    `(("libjpeg-turbo" ,libjpeg-turbo)
      ("zlib" ,zlib)))
   (synopsis "Command-line tools and library for transforming PDF files")
   (description
    "QPDF is a command-line program that does structural, content-preserving
transformations on PDF files.  It could have been called something like
pdf-to-pdf.  It includes support for merging and splitting PDFs and to
manipulate the list of pages in a PDF file.  It is not a PDF viewer or a
program capable of converting PDF into other formats.")
   ;; Prior to the 7.0 release, QPDF was licensed under Artistic 2.0.
   ;; Users can still choose to use the old license at their option.
   (license (list license:asl2.0 license:clarified-artistic))
   (home-page "http://qpdf.sourceforge.net/")))

(define-public xournal
  (package
    (name "xournal")
    (version "0.4.8.2016")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/xournal/xournal/" version
                           "/xournal-" version ".tar.gz"))
       (sha256
        (base32
         "09i88v3wacmx7f96dmq0l3afpyv95lh6jrx16xzm0jd1szdrhn5j"))))
    (build-system gnu-build-system)
    (inputs
     `(("gtk" ,gtk+-2)
       ("pango" ,pango)
       ("poppler" ,poppler)
       ("glib" ,glib)
       ("libgnomecanvas" ,libgnomecanvas)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://xournal.sourceforge.net/")
    (synopsis "Notetaking using a stylus")
    (description
     "Xournal is an application for notetaking, sketching, keeping a journal
using a stylus.")
    (license license:gpl2+)))

(define-public python-reportlab
  (package
    (name "python-reportlab")
    (version "3.5.13")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "reportlab" version))
              (sha256
               (base32
                "1wxgcj46rm83qz97i8ygvd59bks60kr6vvnz12ygw640z58ff5k1"))))
    (build-system python-build-system)
    (arguments
     '(;; FIXME: There is one test failure, but it does not cause the
       ;; build to fail. No time to investigate right now.
       #:test-target "tests"))
    (inputs
     `(("freetype" ,freetype)))
    (propagated-inputs
     `(("python-pillow" ,python-pillow)))
    (home-page "https://www.reportlab.com")
    (synopsis "Python library for generating PDFs and graphics")
    (description "This is the ReportLab PDF Toolkit.  It allows rapid creation
of rich PDF documents, and also creation of charts in a variety of bitmap and
vector formats.")
    (license license:bsd-3)))

(define-public python2-reportlab
  (package-with-python2 python-reportlab))

(define-public impressive
  (package
    (name "impressive")
    (version "0.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/impressive/Impressive/"
                    version "/Impressive-" version ".tar.gz"))
              (sha256
               (base32
                "0zaqq3yvd296mfr5bxpj2hqlk7vrb0rsbgd4dc1l5ag46giqvivx"))))
    (build-system python-build-system)

    ;; TODO: Add dependency on pdftk.
    (inputs `(("python2-pygame" ,python2-pygame)
              ("python2-pillow" ,python2-pillow)
              ("sdl" ,sdl)
              ("xpdf" ,xpdf)))

    (arguments
     `(#:python ,python-2
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (delete 'configure)
                  (delete 'check)
                  (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; There's no 'setup.py' so install things manually.
                      (let* ((out  (assoc-ref outputs "out"))
                             (bin  (string-append out "/bin"))
                             (man1 (string-append out "/share/man/man1"))
                             (sdl  (assoc-ref inputs "sdl"))
                             (xpdf (assoc-ref inputs "xpdf")))
                        (mkdir-p bin)
                        (copy-file "impressive.py"
                                   (string-append bin "/impressive"))
                        (wrap-program (string-append bin "/impressive")
                          `("LIBRARY_PATH" ":" prefix ;for ctypes
                            (,(string-append sdl "/lib")))
                          `("PATH" ":" prefix     ;for pdftoppm
                            (,(string-append xpdf "/bin"))))
                        (install-file "impressive.1" man1)
                        #t))))))
    (home-page "http://impressive.sourceforge.net")
    (synopsis "PDF presentation tool with visual effects")
    (description
     "Impressive is a tool to display PDF files that provides visual effects
such as smooth alpha-blended slide transitions.  It provides additional tools
such as zooming, highlighting an area of the screen, and a tool to navigate
the PDF pages.")
    (license license:gpl2)))

(define-public fbida
  (package
    (name "fbida")
    (version "2.12")
    (home-page "https://www.kraxel.org/blog/linux/fbida/")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.kraxel.org/releases/fbida/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0bw224vb7jh0lrqaf4jgxk48xglvxs674qcpj5y0axyfbh896cfk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-ldconfig
           (lambda _
             (substitute* "mk/Autoconf.mk"
               (("/sbin/ldconfig -p") "echo lib")) #t))
         (delete 'configure))
        #:tests? #f
        #:make-flags (list "CC=gcc"
                           (string-append "prefix=" (assoc-ref %outputs "out")))))
    (inputs `(("libjpeg" ,libjpeg)
              ("curl" ,curl)
              ("libtiff" ,libtiff)
              ("libudev" ,eudev)
              ("libwebp" ,libwebp)
              ("libdrm" ,libdrm)
              ("imagemagick" ,imagemagick)
              ("giflib" ,giflib)
              ("glib" ,glib)
              ("cairo-xcb" ,cairo-xcb)
              ("freetype" ,freetype)
              ("fontconfig" ,fontconfig)
              ("libexif" ,libexif)
              ("mesa" ,mesa)
              ("libepoxy" ,libepoxy)
              ("libpng" ,libpng)
              ("poppler" ,poppler)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (synopsis "Framebuffer and drm-based image viewer")
    (description
      "fbida contains a few applications for viewing and editing images on
the framebuffer.")

    (license license:gpl2+)))

(define-public pdf2svg
  (package
    (name "pdf2svg")
    (version "0.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dawbarton/pdf2svg.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14ffdm4y26imq99wjhkrhy9lp33165xci1l5ndwfia8hz53bl02k"))))
    (build-system gnu-build-system)
    (inputs
     `(("cairo" ,cairo)
       ("poppler" ,poppler)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.cityinthesky.co.uk/opensource/pdf2svg/")
    (synopsis "PDF to SVG converter")
    (description "@command{pdf2svg} is a simple command-line PDF to SVG
converter using the Poppler and Cairo libraries.")
    (license license:gpl2+)))

(define-public python-pypdf2
  (package
    (name "python-pypdf2")
    (version "1.26.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "PyPDF2" version))
              (sha256
               (base32
                "11a3aqljg4sawjijkvzhs3irpw0y67zivqpbjpm065ha5wpr13z2"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'patch-test-suite
          (lambda _
            ;; The text-file needs to be opened in binary mode for Python 3,
            ;; so patch in the "b"
            (substitute* "Tests/tests.py"
              (("pdftext_file = open\\(.* 'crazyones.txt'\\), 'r" line)
               (string-append line "b")))
            #t))
         (replace 'check
           (lambda _
             (invoke "python" "-m" "unittest" "Tests.tests"))))))
    (home-page "http://mstamy2.github.com/PyPDF2")
    (synopsis "Pure Python PDF toolkit")
    (description "PyPDF2 is a pure Python PDF library capable of:

@enumerate
@item extracting document information (title, author, …)
@item splitting documents page by page
@item merging documents page by page
@item cropping pages
@item merging multiple pages into a single page
@item encrypting and decrypting PDF files
@end enumerate

By being pure Python, it should run on any Python platform without any
dependencies on external libraries.  It can also work entirely on
@code{StringIO} objects rather than file streams, allowing for PDF
manipulation in memory.  It is therefore a useful tool for websites that
manage or manipulate PDFs.")
    (license license:bsd-3)))

(define-public python2-pypdf2
  (package-with-python2 python-pypdf2))

(define-public python2-pypdf
  (package
    (name "python2-pypdf")
    (version "1.13")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyPdf" version))
              (sha256
               (base32
                "0fqfvamir7k41w84c73rghzkiv891gdr17q5iz4hgbf6r71y9v9s"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f  ; no tests
       #:python ,python-2))
    (home-page "http://pybrary.net/pyPdf/")
    (synopsis "Pure Python PDF toolkit")
    (description "PyPDF2 is a pure Python PDF toolkit.

Note: This module isn't maintained anymore.  For new projects please use
python-pypdf2 instead.")
    (license license:bsd-3)))

(define-public pdfposter
  (package
    (name "pdfposter")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pdftools.pdfposter" version ".tar.bz2"))
              (sha256
               (base32
                "1i9jqawf279va089ykicglcq4zlsnwgcnsdzaa8vnm836lqhywma"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f  ; no test suite, only for visual control
       #:python ,python-2))
    (inputs
     ;; pdfposter 0.6.0 still uses the old pyPdf
     `(("python2-pypdf" ,python2-pypdf)))
    (home-page "https://pythonhosted.org/pdftools.pdfposter/")
    (synopsis "Scale and tile PDF images/pages to print on multiple pages")
    (description "@command{pdfposter} can be used to create a large poster by
building it from multple pages and/or printing it on large media.  It expects
as input a PDF file, normally printing on a single page.  The output is again
a PDF file, maybe containing multiple pages together building the poster.  The
input page will be scaled to obtain the desired size.

This is much like @command{poster} does for Postscript files, but working with
PDF.  Since sometimes @command{poster} does not like your files converted from
PDF.  Indeed @command{pdfposter} was inspired by @command{poster}.")
    (license license:gpl3+)))

(define-public pdfgrep
  (package
    (name "pdfgrep")
    (version "2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pdfgrep.org/download/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "1fia10djcxxl7n9jw2prargw4yzbykk6izig2443ycj9syhxrwqf"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libgcrypt" ,libgcrypt)
       ("pcre" ,pcre)
       ("poppler" ,poppler)))
    (home-page "https://pdfgrep.org")
    (synopsis "Command-line utility to search text in PDF files")
    (description
     "Pdfgrep searches in pdf files for strings matching a regular expression.
Support some GNU grep options as file name output, page number output,
optional case insensitivity, count occurrences, color highlights and search in
multiple files.")
    (license license:gpl2+)))

(define-public pdfpc
  (package
    (name "pdfpc")
    (version "4.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pdfpc/pdfpc.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1rmsrpf5vlqhnyyrhq8apndny88ld2qvfjx6258653pqbimv7mx5"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))          ; no test target
    (inputs
     `(("cairo" ,cairo)
       ("gtk+" ,gtk+)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("libgee" ,libgee)
       ("poppler" ,poppler)
       ("pango" ,pango)
       ("vala" ,vala)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://pdfpc.github.io/")
    (synopsis "Presenter console with multi-monitor support for PDF files")
    (description
     "pdfpc is a presentation viewer application which uses multi-monitor
output to provide meta information to the speaker during the presentation.  It
is able to show a normal presentation window on one screen, while showing a
more sophisticated overview on the other one providing information like a
picture of the next slide, as well as the left over time till the end of the
presentation.  The input files processed by pdfpc are PDF documents.")
    (license license:gpl2+)))

(define-public paps
  (let ((commit "37e6ca1cd96d751bbbff5539d795c90d657289a5")
        (revision "1"))
    (package
      (name "paps")
      ;; The last release was in 2015, but since then there have been security
      ;; bug fixes.
      (version (git-version "0.7.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dov/paps.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1ilcyjqdynxsd2p8dnn8h4592dwf531x9pbkxa1w09hkcdn7hgwc"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'do-not-run-configure-script-during-bootstrap
             (lambda _
               (substitute* "autogen.sh"
                 (("^./configure") "#"))
               #t)))))
      (inputs
       `(("pango" ,pango)))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("gettext" ,gettext-minimal)
         ("glib" ,glib "bin")
         ("intltool" ,intltool)
         ("pkg-config" ,pkg-config)))
      (home-page "https://github.com/dov/paps")
      (synopsis "Pango to PostScript converter")
      (description
       "Paps reads a UTF-8 encoded file and generates a PostScript language
rendering of the file.  The rendering is done by creating outline curves
through the Pango @code{ft2} backend.")
      (license license:lgpl2.0+))))
