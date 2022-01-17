;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015, 2016, 2018, 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016, 2017, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2016, 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017, 2018 Rene Saavedra <pacoon@protonmail.com>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Ben Sturmfels <ben@sturm.com.au>
;;; Copyright © 2019,2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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
  #:use-module (guix build-system qt)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages fonts)
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
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public extractpdfmark
  (package
    (name "extractpdfmark")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/trueroad/extractpdfmark")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14aa6zly53j8gx5d32caiabk2j4b102xha0v9149yahz6kbn5b80"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'start-xorg-server
           ;; The test suite wants to write to /homeless-shelter
           (lambda _ (setenv "HOME" (getcwd)))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("ghostscript" ,ghostscript)
       ("pkg-config" ,pkg-config)
       ("texlive" ,texlive-tiny)))
    (inputs
     (list poppler))
    (home-page "https://github.com/trueroad/extractpdfmark")
    (synopsis "Extract page mode and named destinations as PDFmark from PDF")
    (description
     "PDFmarks is a technique that accompanies PDF, and that is used to store
metadata such as author or title, but also structural information such as
bookmarks or hyperlinks.

When Ghostscript reads the main PDF generated by the TeX system with embedded
PDF files and outputs the final PDF, the PDF page mode and name targets
etc. are not preserved.  Therefore, when you open the final PDF, it is not
displayed correctly.  Also, remote PDF links do not work correctly.

This program is able to extract the page mode and named targets as PDFmark
from PDF.  In this way, you can obtain embedded PDF files that have kept this
information.")
    (license license:gpl3)))

(define-public flyer-composer
  (package
    (name "flyer-composer")
    (version "1.0rc2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flyer-composer" version))
       (sha256
        (base32 "17igqb5dlcgcq4nimjw6cf9qgz6a728zdx1d0rr90r2z0llcchsv"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ;; TODO
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (qtbase (assoc-ref inputs "qtbase"))
                    (qml "/lib/qt5/qml"))
               (wrap-program (string-append out "/bin/flyer-composer-gui")
                 `("QT_PLUGIN_PATH" ":" =
                   (,(string-append qtbase "/lib/qt5/plugins")))
                 `("QT_QPA_PLATFORM_PLUGIN_PATH" ":" =
                   (,(string-append qtbase "/lib/qt5/plugins/platforms"))))
               #t))))))
    (inputs
     (list python-pypdf2 python-pyqt python-poppler-qt5 qtbase-5))
    (home-page "http://crazy-compilers.com/flyer-composer")
    (synopsis "Rearrange PDF pages to print as flyers on one sheet")
    (description "@command{flyer-composer} can be used to prepare one- or
two-sided flyers for printing on one sheet of paper.

Imagine you have designed a flyer in A6 format and want to print it using your
A4 printer.  Of course, you want to print four flyers on each sheet.  This is
where Flyer Composer steps in, creating a PDF which holds your flyer four
times.  If you have a second page, Flyer Composer can arrange it the same way
- even if the second page is in a separate PDF file.

This package contains both the command line tool and the gui too.")
    (license license:agpl3+)))

(define-public flyer-composer-cli
  (package/inherit flyer-composer
    (name "flyer-composer-cli")
    (arguments
     `(#:tests? #f ;; TODO
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-gui
           (lambda _
             (delete-file-recursively "flyer_composer/gui")
             (substitute* "setup.cfg"
               (("^\\s+flyer-composer-gui\\s*=.*") ""))
             #t)))))
    (inputs
     `(("python-pypdf2" ,python-pypdf2)))
    (description "@command{flyer-composer} can be used to prepare one- or
two-sided flyers for printing on one sheet of paper.

Imagine you have designed a flyer in A6 format and want to print it using your
A4 printer.  Of course, you want to print four flyers on each sheet.  This is
where Flyer Composer steps in, creating a PDF which holds your flyer four
times.  If you have a second page, Flyer Composer can arrange it the same way
- even if the second page is in a separate PDF file.

This package contains only the command line tool.  If you like to use the gui,
please install the @code{flyer-composer-gui} package.")))

(define-public poppler
  (package
   (name "poppler")
   (version "21.07.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://poppler.freedesktop.org/poppler-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1m54hsi8z6c13jdbjwz55flkra1mahmkw2igavbf8p86d2gv4sp2"))))
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
             ("cairo" ,cairo-sans-poppler)))
   (propagated-inputs
    ;; As per poppler-cairo and poppler-glib.pc.
    ;; XXX: Ideally we'd propagate Cairo too, but that would require a
    ;; different solution to the circular dependency mentioned above.
    (list glib))
   (native-inputs
      (list pkg-config
            `(,glib "bin") ; glib-mkenums, etc.
            gobject-introspection))
   (arguments
    `(#:tests? #f                      ;no test data provided with the tarball
      #:configure-flags
      (let* ((out (assoc-ref %outputs "out"))
             (lib (string-append out "/lib")))
        (list "-DENABLE_UNSTABLE_API_ABI_HEADERS=ON" ;to install header files
              "-DENABLE_ZLIB=ON"
              "-DENABLE_BOOST=OFF"      ;disable Boost to save size
              (string-append "-DCMAKE_INSTALL_LIBDIR=" lib)
              (string-append "-DCMAKE_INSTALL_RPATH=" lib)))
      ,@(if (%current-target-system)
            `(#:phases
              (modify-phases %standard-phases
                (add-after 'unpack 'set-PKG_CONFIG
                  (lambda _
                    (setenv "PKG_CONFIG" ,(pkg-config-for-target))))))
            '())))
   (synopsis "PDF rendering library")
   (description
    "Poppler is a PDF rendering library based on the xpdf-3.0 code base.")
   (license license:gpl2+)
   (home-page "https://poppler.freedesktop.org/")))

(define-public poppler-data
  (package
    (name "poppler-data")
    (version "0.4.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://poppler.freedesktop.org/poppler-data"
                                  "-" version ".tar.gz"))
              (sha256
               (base32
                "137h4m48gc4v0srnr0gkwaqna6kfdqpy5886if5gjfmh3g6hbv1c"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no test suite
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

(define-public poppler-qt5
  (package/inherit poppler
   (name "poppler-qt5")
   (inputs `(("qtbase" ,qtbase-5)
             ,@(package-inputs poppler)))
   (synopsis "Qt5 frontend for the Poppler PDF rendering library")))

(define-public python-poppler-qt5
  (package
    (name "python-poppler-qt5")
    (version "21.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-poppler-qt5" version))
        (sha256
         (base32
          "0b82gm4i75q5v19kfbq0h4y0b2vcwr2213zkhxh6l0h45kdndmxd"))
       (patches (search-patches "python-poppler-qt5-fix-build.patch"))))
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
                "if True"))
             ;; We need to pass an extra flag here.  This cannot be in
             ;; configure-flags because it should not be passed for the
             ;; installation phase.
             ((@@ (guix build python-build-system) call-setuppy)
              "build_ext" (list (string-append "--pyqt-sip-dir="
                                               (assoc-ref inputs "python-pyqt")
                                               "/share/sip")) #t))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list python-sip-4 python-pyqt poppler-qt5 qtbase-5))
    (home-page "https://pypi.org/project/python-poppler-qt5/")
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
                   (url "https://github.com/libharu/libharu")
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
    (list zlib libpng))
   (native-inputs
    (list autoconf automake libtool))
   (home-page "http://libharu.org/")
   (synopsis "Library for generating PDF files")
   (description
    "libHaru is a library for generating PDF files.  libHaru does not support
reading and editing of existing PDF files.")
   (license license:zlib)))

(define-public xpdf
  (package
   (name "xpdf")
   (version "4.03")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "https://dl.xpdfreader.com/xpdf-" version ".tar.gz"))
      (sha256
       (base32 "0ip81c9vy0igjnasl9iv2lz214fb01vvvdzbvjmgwc63fi1jgr0g"))))
   (build-system cmake-build-system)
   (inputs (list cups freetype libpng qtbase-5 zlib))
   (arguments
    `(#:tests? #f))                   ; there is no check target
   (synopsis "Viewer for PDF files based on the Motif toolkit")
   (description
    "Xpdf is a viewer for Portable Document Format (PDF) files.")
   (license license:gpl3)             ; or gpl2, but not gpl2+
   (home-page "https://www.xpdfreader.com/")))

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
    (native-inputs (list pkg-config))
    (inputs (list libarchive zathura))
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
    (native-inputs (list pkg-config))
    (inputs (list libspectre zathura))
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
    (version "0.2.9")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura-djvu/download/zathura-djvu-"
                              version ".tar.xz"))
              (sha256
               (base32
                "0062n236414db7q7pnn3ccg5111ghxj3407pn9ri08skxskgirln"))))
    (native-inputs (list pkg-config))
    (inputs
     (list djvulibre zathura))
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
    (version "0.3.6")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura-pdf-mupdf"
                              "/download/zathura-pdf-mupdf-" version ".tar.xz"))
              (sha256
               (base32
                "1r3v37k9fl2rxipvacgxr36llywvy7n20a25h3ajlyk70697sa66"))))
    (native-inputs (list pkg-config))
    (inputs
     `(("jbig2dec" ,jbig2dec)
       ("libjpeg" ,libjpeg-turbo)
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
         (add-after 'unpack 'remove-libmupdfthird.a-requirement
           (lambda _
             ;; Ignore a missing (apparently superfluous) static library.
             (substitute* "meson.build"
               ((".*mupdfthird.*") ""))
             #t))
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
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura-pdf-poppler/download/zathura-pdf-poppler-"
                              version ".tar.xz"))
              (sha256
               (base32
                "1vfl4vkyy3rf39r1sqaa7y8113bgkh2bkfq3nn2inis9mrykmk6m"))))
    (native-inputs (list pkg-config))
    (inputs
     (list poppler zathura))
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
    (version "0.4.8")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/zathura/download/zathura-"
                              version ".tar.xz"))
              (sha256
               (base32
                "1nr0ym1mi2afk4ycdf1ppmkcv7i7hyzwn4p3r4m0j2qm3nvaiami"))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("gettext" ,gettext-minimal)
                     ("glib:bin" ,glib "bin")

                     ;; For building documentation.
                     ("python-sphinx" ,python-sphinx)

                     ;; For building icons.
                     ("librsvg" ,librsvg)

                     ;; For tests.
                     ("check" ,check)
                     ("xorg-server" ,xorg-server-for-tests)))
    (inputs (list sqlite))
    ;; Listed in 'Requires.private' of 'zathura.pc'.
    (propagated-inputs (list cairo girara))
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
    (version "0.9.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/podofo/podofo/" version
                                  "/podofo-" version ".tar.gz"))
              (sha256
               (base32
                "1f0yvkx6nf99fp741w2y706d8bs9824x1z2gqm3rdy5fv8bfgwkw"))))
    (build-system cmake-build-system)
    (native-inputs
     (list cppunit pkg-config))
    (inputs
     `(("libjpeg" ,libjpeg-turbo)
       ("libtiff" ,libtiff)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("libpng" ,libpng)
       ("lua" ,lua-5.1)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (arguments
     `(#:configure-flags
       (list "-DPODOFO_BUILD_SHARED=ON")
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
    (version "1.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://mupdf.com/downloads/archive/"
                           "mupdf-" version "-source.tar.xz"))
       (sha256
        (base32 "1i98xqgnzp168hnnhradl8658qsif06wlbvcglz0mmh8wi1rkwrq"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove bundled software.
           (let* ((keep (list "extract"
                              "lcms2")) ; different from our lcms2 package
                  (from "thirdparty")
                  (kept (string-append from "~temp")))
             (mkdir-p kept)
             (for-each (lambda (file) (rename-file (string-append from "/" file)
                                              (string-append kept "/" file)))
                       keep)
             (delete-file-recursively from)
             (rename-file kept from))
           #t))))
    (build-system gnu-build-system)
    (inputs
      `(("curl" ,curl)
        ("freeglut" ,freeglut)
        ("freetype" ,freetype)
        ("gumbo-parser" ,gumbo-parser)
        ("harfbuzz" ,harfbuzz)
        ("jbig2dec" ,jbig2dec)
        ("libjpeg" ,libjpeg-turbo)
        ("libx11" ,libx11)
        ("libxext" ,libxext)
        ("mujs" ,mujs)
        ("openjpeg" ,openjpeg)
        ("openssl" ,openssl)
        ("zlib" ,zlib)))
    (native-inputs
      (list pkg-config))
    (arguments
      `(#:tests? #f                     ; no check target
        #:make-flags (list "verbose=yes"
                           (string-append "CC=" ,(cc-for-target))
                           "XCFLAGS=-fpic"
                           "USE_SYSTEM_LIBS=yes"
                           "USE_SYSTEM_MUJS=yes"
                           "shared=yes"
                           ;; Even with the linkage patch we must fix RUNPATH.
                           (string-append "LDFLAGS=-Wl,-rpath="
                                          (assoc-ref %outputs "out") "/lib")
                           (string-append "prefix=" (assoc-ref %outputs "out")))
        #:phases (modify-phases %standard-phases
                   (delete 'configure)))) ; no configure script
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
   (version "10.0.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/qpdf/qpdf/" version
                                "/qpdf-" version ".tar.gz"))
            (sha256
             (base32
              "0yw2cpw7ygfd6jlgpwbi8vsnvv9p55zxp9h17x77z2qq733pf8jx"))))
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
    (list pkg-config perl))
   (propagated-inputs
    ;; In Requires.private of libqpdf.pc.
    (list libjpeg-turbo zlib))
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

(define-public qpdfview
  (package
    (name "qpdfview")
    (version "0.4.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://launchpad.net/qpdfview/"
                           "trunk/" version "/+download/"
                           "qpdfview-" version ".tar.gz"))
       (sha256
        (base32 "0v1rl126hvblajnph2hkansgi0s8vjdc5yxrm4y3faa0lxzjwr6c"))
       (patches (search-patches "qpdfview-qt515-compat.patch"))))
    (build-system qt-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list cups
           djvulibre
           libspectre
           poppler-qt5
           qtbase-5
           qtsvg))
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "qpdfview.pri"
               (("/usr") (assoc-ref outputs "out")))
             (invoke "qmake" "qpdfview.pro"))))))
    (home-page "https://launchpad.net/qpdfview")
    (synopsis "Tabbed document viewer")
    (description "@command{qpdfview} is a document viewer for PDF, PS and DJVU
files.  It uses the Qt toolkit and features persistent per-file settings,
configurable toolbars and shortcuts, continuous and multi‐page layouts,
SyncTeX support, and rudimentary support for annotations and forms.")
    (license license:gpl2+)))

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
     (list gtk+-2 pango poppler glib libgnomecanvas))
    (native-inputs
     (list pkg-config))
    (home-page "http://xournal.sourceforge.net/")
    (synopsis "Notetaking using a stylus")
    (description
     "Xournal is an application for notetaking, sketching, keeping a journal
using a stylus.")
    (license license:gpl2+)))

(define-public xournalpp
  (package
    (name "xournalpp")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xournalpp/xournalpp")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ldf58l5sqy52x5dqfpdjdh7ldjilj9mw42jzsl5paxg0md2k0hl"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DENABLE_CPPUNIT=ON") ;enable tests
       #:imported-modules ((guix build glib-or-gtk-build-system)
                           ,@%cmake-build-system-modules)
       #:modules (((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build cmake-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-permissions-on-po-files
           (lambda _
             ;; Make sure 'msgmerge' can modify the PO files.
             (for-each make-file-writable
                       (find-files "." "\\.po$"))))
         ;; Fix path to addr2line utility, which the crash reporter uses.
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/util/Stacktrace.cpp"
               ;; Match only the commandline.
               (("\"addr2line ")
                (string-append "\"" (which "addr2line") " ")))))
         (add-after 'install 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     `(("cppunit" ,cppunit)
       ("gettext" ,gettext-minimal)
       ("help2man" ,help2man)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list alsa-lib
           gtk+
           librsvg
           libsndfile
           libxml2
           libzip
           lua
           poppler
           portaudio
           texlive-bin))
    (home-page "https://github.com/xournalpp/xournalpp")
    (synopsis "Handwriting notetaking software with PDF annotation support")
    (description "Xournal++ is a hand note taking software written in
C++ with the target of flexibility, functionality and speed.  Stroke
recognizer and other parts are based on Xournal code.

Xournal++ features:

@itemize
@item Support for Pen pressure, e.g., Wacom Tablet
@item Support for annotating PDFs
@item Fill shape functionality
@item PDF Export (with and without paper style)
@item PNG Export (with and without transparent background)
@item Map different tools / colors etc. to stylus buttons /
mouse buttons
@item Sidebar with Page Previews with advanced page sorting, PDF
Bookmarks and Layers (can be individually hidden, editing layer can be
selected)
@item enhanced support for image insertion
@item Eraser with multiple configurations
@item LaTeX support
@item bug reporting, autosave, and auto backup tools
@item Customizeable toolbar, with multiple configurations, e.g., to
optimize toolbar for portrait / landscape
@item Page Template definitions
@item Shape drawing (line, arrow, circle, rectangle)
@item Shape resizing and rotation
@item Rotation snapping every 45 degrees
@item Rect snapping to grid
@item Audio recording and playback alongside with handwritten notes
@item Multi Language Support, Like English, German, Italian...
@item Plugins using LUA Scripting
@end itemize")
    (license license:gpl2+)))

(define-public python-reportlab
  (package
    (name "python-reportlab")
    (version "3.5.42")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "reportlab" version))
              (sha256
               (base32
                "0i17qgm7gzy7pzp240mkpsx9rn8rr67jh5npp5bylv3sd41g48cw"))))
    (build-system python-build-system)
    (arguments
     '(;; FIXME: There is one test failure, building the pdf manual from source,
       ;; but it does not cause the build to fail.
       #:test-target "tests"
       #:configure-flags (list "--use-system-libart")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libart (assoc-ref inputs "libart-lgpl"))
                   (freetype (assoc-ref inputs "freetype"))
                   (dlt1 (assoc-ref inputs "font-curve-files")))
               (substitute* "setup.py"
                 (("/usr/include/libart-\\*")
                  (string-append libart "/include/libart-2.0"))
                 (("/usr/include/freetype2")
                  (string-append freetype "/include"))
                 (("http://www.reportlab.com/ftp/pfbfer-20180109.zip")
                  (string-append "file://" dlt1)))
               #t))))))
    (inputs
     `(("freetype" ,freetype)
       ("libart-lgpl" ,libart-lgpl)
       ("font-curve-files"
        ,(origin
           (method url-fetch)
           (uri "http://www.reportlab.com/ftp/pfbfer-20180109.zip")
           (sha256
            (base32
             "1v0gy4mbx02ys96ssx89420y0njknlrxs2bx64bv4rp8a0al66w5"))))))
    (propagated-inputs
     (list python-pillow))
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
    (version "0.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/impressive/Impressive/"
                    version "/Impressive-" version ".tar.gz"))
              (sha256
               (base32
                "1r7ihv41awnlnlry1kymb8fka053wdhzibfwcarn78rr3vs338vl"))))
    (build-system python-build-system)

    ;; TODO: Add dependency on pdftk.
    (inputs (list python2-pygame python2-pillow sdl xpdf))

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

(define-public img2pdf
  (package
    (name "img2pdf")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "img2pdf" version))
       (sha256
        (base32 "17z0bn8kihiyqjd1m5jr80m7ry06l1qn0l8v918xg5gs7q2calcf"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pikepdf python-pillow
           `(,python "tk")))
    (home-page "https://gitlab.mister-muffin.de/josch/img2pdf")
    (synopsis "Convert images to PDF via direct JPEG inclusion")
    (description
     "img2pdf converts images to PDF via direct JPEG inclusion.  That
conversion is lossless: the image embedded in the PDF has the exact same color
information for every pixel as the input.")
    (license license:lgpl3)))

(define-public fbida
  (package
    (name "fbida")
    (version "2.14")
    (home-page "https://www.kraxel.org/blog/linux/fbida/")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.kraxel.org/releases/fbida/"
                                  "fbida-" version ".tar.gz"))
              (sha256
               (base32
                "0f242mix20rgsqz1llibhsz4r2pbvx6k32rmky0zjvnbaqaw1dwm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-FHS-file-names
           (lambda _
             (substitute* "mk/Autoconf.mk"
               (("/bin/echo") "echo")
               (("/sbin/ldconfig -p") "echo lib")) #t))
         (add-before 'build 'set-fcommon
           (lambda _
             (setenv "CFLAGS" "-fcommon")))
         (delete 'configure))
        #:tests? #f
        #:make-flags
        (list (string-append "CC=" ,(cc-for-target))
              (string-append "prefix=" (assoc-ref %outputs "out")))))
    (inputs `(("libjpeg" ,libjpeg-turbo)
              ("curl" ,curl)
              ("libtiff" ,libtiff)
              ("libudev" ,eudev)
              ("libwebp" ,libwebp)
              ("libdrm" ,libdrm)
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
    (native-inputs (list pkg-config))
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
                    (url "https://github.com/dawbarton/pdf2svg")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14ffdm4y26imq99wjhkrhy9lp33165xci1l5ndwfia8hz53bl02k"))))
    (build-system gnu-build-system)
    (inputs
     (list cairo poppler))
    (native-inputs
     (list pkg-config))
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

(define-public pdfarranger
  (package
    (name "pdfarranger")
    (version "1.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jeromerobert/pdfarranger")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18bpnnwjx72d5ps06dr89mkixiwzc9hf5gr75k8qcnrkshl038v2"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f                      ;no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-for-typelib
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (program (string-append out "/bin/pdfarranger")))
               (wrap-program program
                 `("GI_TYPELIB_PATH" ":" prefix
                   (,(getenv "GI_TYPELIB_PATH"))))))))))
    (native-inputs
     (list intltool python-distutils-extra))
    (inputs
     (list gtk+ poppler))
    (propagated-inputs
     (list img2pdf
           python-dateutil
           python-pikepdf
           python-pycairo
           python-pygobject))
    (home-page "https://github.com/jeromerobert/pdfarranger")
    (synopsis "Merge, split and re-arrange pages from PDF documents")
    (description
     "PDF Arranger is a small application which allows one to merge or split
PDF documents and rotate, crop and rearrange their pages using an interactive
and intuitive graphical interface.

PDF Arranger was formerly known as PDF-Shuffler.")
    (license license:gpl3+)))

(define-public pdfposter
  (package
    (name "pdfposter")
    (version "0.7.post1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pdftools.pdfposter" version))
              (sha256
               (base32
                "0c1avpbr9q53yzq5ar2x485rmp9d0l3z27aham32bg7gplzd7w0j"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))  ; test-suite not included in source archive
    (inputs
     (list python-pypdf2))
    (home-page "https://pythonhosted.org/pdftools.pdfposter/")
    (synopsis "Scale and tile PDF images/pages to print on multiple pages")
    (description "@command{pdfposter} can be used to create a large poster by
building it from multiple pages and/or printing it on large media.  It expects
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
     (list pkg-config))
    (inputs
     (list libgcrypt pcre poppler))
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
    (version "4.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pdfpc/pdfpc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bmy51w6ypz927hxwp5g7wapqvzqmsi3w32rch6i3f94kg1152ck"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f          ; no test target
       #:phases
       (modify-phases %standard-phases
         ;; This is really a bug in Vala.
         ;; https://github.com/pdfpc/pdfpc/issues/594
         (add-after 'unpack 'fix-vala-API-conflict
           (lambda _
             (substitute* "src/classes/action/movie.vala"
               (("info.from_caps\\(caps\\)")
                "Gst.Video.info_from_caps(out info, caps)")))))))
    (inputs
     `(("cairo" ,cairo)
       ("discount" ,discount) ; libmarkdown
       ("gtk+" ,gtk+)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("json-glib" ,json-glib)
       ("libgee" ,libgee)
       ("poppler" ,poppler)
       ("pango" ,pango)
       ("vala" ,vala)
       ("webkitgtk" ,webkitgtk-with-libsoup2)))
    (native-inputs
     (list pkg-config))
    (home-page "https://pdfpc.github.io/")
    (synopsis "Presenter console with multi-monitor support for PDF files")
    (description
     "pdfpc is a presentation viewer application which uses multi-monitor
output to provide meta information to the speaker during the presentation.  It
is able to show a normal presentation window on one screen, while showing a
more sophisticated overview on the other one providing information like a
picture of the next slide, as well as the left over time till the end of the
presentation.  The input files processed by pdfpc are PDF documents.")
    (license license:gpl3+)))

(define-public paps
  (package
    (name "paps")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/dov/paps/releases/download/v"
                           version "/paps-" version ".tar.gz"))
       (sha256
        (base32 "1z1w1fg2bvb8p92n1jlpqp3n9mq42szb2mqhh4xqmmnmfcdkpi9s"))))
    (build-system gnu-build-system)
    (inputs
     (list pango))
    (native-inputs
     (list intltool pkg-config))
    (home-page "https://github.com/dov/paps")
    (synopsis "Pango to PostScript converter")
    (description
     "Paps reads a UTF-8 encoded file and generates a PostScript language
rendering of the file through the Pango Cairo back end.")
    (license license:lgpl2.0+)))

(define-public stapler
  (package
    (name "stapler")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stapler" version))
       (sha256
        (base32
         "0b2lbm3f79cdxcsagwhzihbzwahjabxqmbws0c8ki25gpdnygdd7"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-more-itertools-version-requirement
           (lambda _
             ;; Tests require an version of the more-itertools module older
             ;; than the one we have packaged.
             (substitute* "setup.py"
               (("more-itertools>=2\\.2,<6\\.0\\.0") "more-itertools>=2.2"))
             #t)))))
    (propagated-inputs
     (list python-more-itertools python-pypdf2))
    (home-page "https://github.com/hellerbarde/stapler")
    (synopsis "PDF manipulation tool")
    (description "Stapler is a pure Python alternative to PDFtk, a tool for
manipulating PDF documents from the command line.  It supports

@itemize
@item cherry-picking pages and concatenating them into a new file
@item splitting a PDF document into single pages each in its own file
@item merging PDF documents with their pages interleaved
@item displaying metadata in a PDF document
@item displaying the mapping between logical and physical page numbers
@end itemize")
    (license license:bsd-3)))

(define-public weasyprint
  (package
    (name "weasyprint")
    (version "52.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FelixSchwarz/WeasyPrint")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0rcj9yah3bp6bbvkmny3w4csx4l5v49lc7mrk29g0x77qnwswjy7"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-library-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((fontconfig (assoc-ref inputs "fontconfig"))
                   (glib (assoc-ref inputs "glib"))
                   (pango (assoc-ref inputs "pango"))
                   (pangoft2 (assoc-ref inputs "pangoft2")))
               (substitute* "weasyprint/fonts.py"
                 (("'fontconfig'")
                  (format #f "'~a/lib/libfontconfig.so'" fontconfig))
                 (("'pangoft2-1.0'")
                  (format #f "'~a/lib/libpangoft2-1.0.so'" pango)))
               (substitute* "weasyprint/text.py"
                 (("'gobject-2.0'")
                  (format #f "'~a/lib/libgobject-2.0.so'" glib))
                 (("'pango-1.0'")
                  (format #f "'~a/lib/libpango-1.0.so'" pango))
                 (("'pangocairo-1.0'")
                  (format #f "'~a/lib/libpangocairo-1.0.so'" pango)))
               #t)))
         (add-after 'unpack 'disable-linters
           ;; Their check fails; none of our business.
           (lambda _
             (substitute* "setup.cfg"
               ((".*pytest-flake8.*") "")
               ((".*pytest-isort.*") "")
               (("--flake8") "")
               (("--isort") ""))
             #t))
         (add-before 'check 'register-dejavu-font
           (lambda* (#:key inputs #:allow-other-keys)
             ;; TODO: fix FreeType so that fonts found in XDG_DATA_DIRS are
             ;; honored.
             (let* ((HOME "/tmp")
                    (dejavu (assoc-ref inputs "font-dejavu"))
                    (fonts-dir (string-append HOME "/.fonts")))
               (setenv "HOME" HOME)
               (mkdir-p fonts-dir)
               (symlink (string-append dejavu "/share/fonts/truetype")
                        (string-append fonts-dir "/truetype"))
               (invoke "fc-cache" "-rv")))))))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("glib" ,glib)
       ("pango" ,pango)))
    (propagated-inputs
     (list gdk-pixbuf
           python-cairocffi
           python-cairosvg
           python-cffi
           python-cssselect2
           python-html5lib
           python-pyphen
           python-tinycss2))
    (native-inputs
     (list font-dejavu ;tests depend on it
           python-pytest-cov python-pytest-runner))
    (home-page "https://weasyprint.org/")
    (synopsis "Document factory for creating PDF files from HTML")
    (description "WeasyPrint helps web developers to create PDF documents.  It
turns simple HTML pages into gorgeous statistical reports, invoices, tickets,
etc.

From a technical point of view, WeasyPrint is a visual rendering engine for
HTML and CSS that can export to PDF and PNG.  It aims to support web standards
for printing.

It is based on various libraries but not on a full rendering engine like
WebKit or Gecko.  The CSS layout engine is written in Python, designed for
pagination, and meant to be easy to hack on.  Weasyprint can also be used as a
python library.

Keywords: html2pdf, htmltopdf")
    (license license:bsd-3)))
