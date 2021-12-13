;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020, 2021 Guillaume Le Vaillant <glv@posteo.net>
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

(define-module (gnu packages djvu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public djvulibre
  (package
    (name "djvulibre")
    (version "3.5.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/djvu/DjVuLibre/"
                           (string-replace-substring version "." ",")
                           "/djvulibre-" version ".tar.gz"))
       (sha256
        (base32 "0manxn1ly5n8nqamv47hz7akxi6v0rzwc9j1c3x99vngrjlr5qw2"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; This bundles software (e.g., zlib) and is entirely superfluous.
           (delete-file-recursively "win32")
           #t))))
    (build-system gnu-build-system)
    (native-inputs
     ;; The 3.5.28 release tarball isn't bootstrapped.
     (list autoconf automake libtool))
    (inputs
     (list libjpeg-turbo libtiff zlib))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'bootstrap 'make-reproducible
           (lambda _
             ;; Ensure there are no timestamps in .svgz files.
             (substitute* "desktopfiles/Makefile.am"
               (("gzip") "gzip -n"))
             #t)))))
    (home-page "http://djvu.sourceforge.net/")
    (synopsis "Implementation of DjVu, the document format")
    (description "DjVuLibre is an implementation of DjVu,
including viewers, browser plugins, decoders, simple encoders, and
utilities.")
    (license license:gpl2+)))

(define-public djview
  (package
    (name "djview")
    (version "4.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/djvu/djview-git")
             (commit (string-append "release." version))))
       (sha256
        (base32 "0mn9ywjbc7iga50lbjclrk892g0x0rap0dmb6ybzjyarybdhhcxp"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config qttools))
    (inputs
     (list djvulibre glib libxt libtiff qtbase-5))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-desktop-file
           ;; Executable is "djview", not "djview4".
           (lambda _
             (substitute* "desktopfiles/djvulibre-djview4.desktop"
               (("Exec=djview4 %f") "Exec=djview %f"))
             #t))
         (add-after 'unpack 'make-files-writable
           (lambda _
             (for-each make-file-writable
                       (find-files "."))
             #t)))))
    (home-page "http://djvu.sourceforge.net/djview4.html")
    (synopsis "Viewer for the DjVu image format")
    (description "DjView is a standalone viewer for DjVu files.

Its features include navigating documents, zooming and panning page images,
producing and displaying thumbnails, displaying document outlines, searching
documents for particular words in the hidden text layer, copying hidden text
to the clipboard, saving pages and documents as bundled or indirect multi-page
files, and printing page and documents.

The viewer can simultaneously display several pages using a side-by-side or
a continuous layout.")
    (license license:gpl2+)))

(define-public pdf2djvu
  (package
    (name "pdf2djvu")
    (version "0.9.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/jwilk/pdf2djvu/releases/download/" version
             "/pdf2djvu-" version ".tar.xz"))
       (sha256
        (base32 "0c595yziz81c9izf9s5sskd00qmgz2n1hp2vdcgg0dx81g3xfidb"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("python2" ,python-2)
       ("python2-nose" ,python2-nose)))
    (inputs
     `(("djvulibre" ,djvulibre)
       ("exiv2" ,exiv2)
       ("graphicsmagick" ,graphicsmagick)
       ("poppler" ,poppler)
       ("poppler-data" ,poppler-data)
       ("util-linux-lib" ,util-linux "lib"))) ; for libuuid
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             (substitute* "tests/test-xmp-broken.py"
               ;; Error message changed in recent versions of XML parser
               (("XML parsing failure")
                "Error in XMLValidator"))))
         (add-before 'check 'set-home-for-tests
           (lambda _
             (setenv "HOME" "/tmp"))))))
    (synopsis "PDF to DjVu converter")
    (description
     "@code{pdf2djvu} creates DjVu files from PDF files.
It is able to extract:
@itemize
@item graphics,
@item text layer,
@item hyperlinks,
@item document outline (bookmarks),
@item metadata (including XMP metadata).
@end itemize\n")
    (home-page "https://jwilk.net/software/pdf2djvu")
    (license license:gpl2)))

(define-public djvu2pdf
  (package
    (name "djvu2pdf")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://0x2a.at/site/projects/djvu2pdf/djvu2pdf-"
                           version ".tar.gz"))
       (sha256
        (base32 "0v2ax30m7j1yi4m02nzn9rc4sn4vzqh5vywdh96r64j4pwvn5s5g"))))
    (build-system gnu-build-system)
    (inputs
     (list djvulibre
           gawk
           ghostscript
           grep
           ncurses
           which))
    (arguments
     `(#:tests? #f ; No test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((djvulibre (assoc-ref inputs "djvulibre"))
                   (gawk (assoc-ref inputs "gawk"))
                   (ghostscript (assoc-ref inputs "ghostscript"))
                   (grep (assoc-ref inputs "grep"))
                   (ncurses (assoc-ref inputs "ncurses"))
                   (which (assoc-ref inputs "which")))
               (substitute* "djvu2pdf"
                 (("awk")
                  (string-append gawk "/bin/awk"))
                 (("ddjvu")
                  (string-append djvulibre "/bin/ddjvu"))
                 (("djvudump")
                  (string-append djvulibre "/bin/djvudump"))
                 (("grep")
                  (string-append grep "/bin/grep"))
                 (("gs")
                  (string-append ghostscript "/bin/gs"))
                 (("tput ")
                  (string-append ncurses "/bin/tput "))
                 (("which")
                  (string-append which "/bin/which"))))
             #t))
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref %outputs "out")))
               (install-file "djvu2pdf"
                             (string-append out "/bin"))
               (install-file "djvu2pdf.1.gz"
                             (string-append out "/share/man/man1"))
               #t))))))
    (synopsis "DjVu to PDF converter")
    (description "This is a small tool to convert DjVu files to PDF files.")
    (home-page "https://0x2a.at/site/projects/djvu2pdf/")
    (license license:gpl2+)))

(define-public minidjvu
  (package
    (name "minidjvu")
    (version "0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/minidjvu/minidjvu/"
                           version "/minidjvu-" version ".tar.gz"))
       (sha256
        (base32 "0jmpvy4g68k6xgplj9zsl6brg6vi81mx3nx2x9hfbr1f4zh95j79"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (inputs
     (list libjpeg-turbo libtiff zlib))
    (arguments
     '(#:configure-flags '("--disable-static")
       #:parallel-build? #f
       #:tests? #f ; No test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda _
             (substitute* "Makefile.in"
               (("/usr/bin/gzip")
                "gzip"))
             #t))
         (add-before 'install 'make-lib-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib"))
             #t)))))
    (synopsis "Black and white DjVu encoder")
    (description
     "@code{minidjvu} is a multipage DjVu encoder and single page
encoder/decoder.  It doesn't support colors or grayscales, just black
and white.")
    (home-page "https://sourceforge.net/projects/minidjvu/")
    (license license:gpl2)))

(define-public djvusmooth
  (package
    (name "djvusmooth")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/jwilk/djvusmooth/releases/download/" version
             "/djvusmooth-" version ".tar.gz"))
       (sha256
        (base32 "0z403cklvxzz0qaczgv83ax0nknrd9h8micp04j9kjfdxk2sgval"))))
    (build-system python-build-system)
    (inputs
     (list djvulibre python2-djvulibre python2-subprocess32
           python2-wxpython))
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "lib/djvused.py"
               (("djvused_path = 'djvused'")
                (string-append "djvused_path = '"
                               (assoc-ref inputs "djvulibre")
                               "/bin/djvused'"))))))))
    (synopsis "Graphical editor for DjVu documents")
    (description
     "@code{djvusmooth} is a graphical editor for DjVu_ documents.
It is able to:
@itemize
@item edit document metadata,
@item edit document outline (bookmarks),
@item add, remove or edit hyperlinks,
@item correct occasional errors in the hidden text layer.
@end itemize\n")
    (home-page "https://jwilk.net/software/djvusmooth")
    (license license:gpl2)))

(define-public didjvu
  (package
    (name "didjvu")
    (version "0.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/jwilk/didjvu/releases/download/" version
             "/didjvu-" version ".tar.gz"))
       (sha256
        (base32 "0xyrnk8d2khi7q1zr28gjkjq6frz4mkb5jdl8821yzf12k7c8pbv"))))
    (build-system gnu-build-system)
    (native-inputs
     (list python2-nose))
    (inputs
     `(("djvulibre" ,djvulibre)
       ("minidjvu" ,minidjvu)
       ("python" ,python-2)
       ("python2-gamera" ,python2-gamera)
       ("python2-pillow" ,python2-pillow)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build python-build-system))
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'check 'disable-failing-test
           (lambda _
             (substitute* "tests/test_ipc.py"
               ;; test_wait_signal gets stuck forever
               (("yield self\\._test_signal, name")
                "return True")
               ;; test_path fails to find a file it should have created
               (("path = os\\.getenv\\('PATH'\\)\\.split\\(':'\\)")
                "return True"))
             (substitute* "tests/test_timestamp.py"
               ;; test_timezones fails with:
               ;;   '2009-12-18T21:25:14Z' != '2009-12-18T22:25:14+01:00'
               (("@fork_isolation")
                "return True"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "make"
                       "DESTDIR="
                       (string-append "PREFIX=" out)
                       "install"))))
         (add-after 'install 'wrap-python
           (assoc-ref python:%standard-phases 'wrap))
         (add-after 'wrap-python 'wrap-path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (djvulibre (assoc-ref inputs "djvulibre")))
               (wrap-program (string-append out "/bin/didjvu")
                 `("PATH" ":" prefix (,(string-append djvulibre "/bin"))))))))))
    (synopsis "DjVu encoder with foreground/background separation")
    (description
     "@code{didjvu} uses the @code{Gamera} framework to separate the foreground
and background layers of images, which can then be encoded into a DjVu file.")
    (home-page "https://jwilk.net/software/didjvu")
    (license license:gpl2)))

(define-public ocrodjvu
  (package
    (name "ocrodjvu")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/jwilk/ocrodjvu/releases/download/" version
             "/ocrodjvu-" version ".tar.xz"))
       (sha256
        (base32 "09w9rqr7z2jd5kwp178zz2yrsc82mxs7gksipg92znxzgzhmw2ng"))))
    (build-system gnu-build-system)
    (native-inputs
     (list libxml2 python2-nose python2-pillow))
    (inputs
     `(("djvulibre" ,djvulibre)
       ("ocrad" ,ocrad)
       ("python" ,python-2)
       ("python2-djvulibre" ,python2-djvulibre)
       ("python2-html5lib" ,python2-html5lib)
       ("python2-lxml" ,python2-lxml)
       ("python2-pyicu" ,python2-pyicu)
       ("python2-subprocess32" ,python2-subprocess32)
       ("tesseract-ocr" ,tesseract-ocr)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build python-build-system))
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'check 'disable-failing-test
           (lambda _
             (substitute* "tests/test_ipc.py"
               ;; test_wait_signal gets stuck forever
               (("yield self\\._test_signal, name")
                "return True")
               ;; test_path fails to find a file it should have created
               (("path = os\\.getenv\\('PATH'\\)\\.split\\(':'\\)")
                "return True"))
             ;; Disable tests with tesseract. They can't work without
             ;; the language files that must downloaded by the final user
             ;; as they are not packaged in Guix.
             (substitute* "tests/ocrodjvu/test.py"
               (("engines = stdout\\.getvalue\\(\\)\\.splitlines\\(\\)")
                "engines = ['ocrad']"))
             (substitute* "tests/ocrodjvu/test_integration.py"
               (("engines = 'tesseract', 'cuneiform', 'gocr', 'ocrad'")
                "engines = 'ocrad'"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "make"
                       "DESTDIR="
                       (string-append "PREFIX=" out)
                       "install"))))
         (add-after 'install 'wrap-python
           (assoc-ref python:%standard-phases 'wrap))
         (add-after 'wrap-python 'wrap-path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (djvulibre (assoc-ref inputs "djvulibre"))
                   (ocrad (assoc-ref inputs "ocrad"))
                   (tesseract (assoc-ref inputs "tesseract-ocr")))
               (for-each (lambda (file)
                           (wrap-program (string-append out "/bin/" file)
                             `("PATH" ":" prefix
                               (,(string-append djvulibre "/bin:"
                                                ocrad "/bin:"
                                                tesseract "/bin")))))
                         '("djvu2hocr"
                           "hocr2djvused"
                           "ocrodjvu"))))))))
    (synopsis "Program to perform OCR on DjVu files")
    (description
     "@code{ocrodjvu} is a wrapper for OCR systems, that allows you to perform
OCR on DjVu files.")
    (home-page "https://jwilk.net/software/ocrodjvu")
    (license license:gpl2)))
