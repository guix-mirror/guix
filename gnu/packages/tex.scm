;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages tex)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system texlive)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages base)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:autoload   (gnu packages texinfo) (texinfo)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:hide (zip)))

(define texlive-extra-src
  (origin
    (method url-fetch)
    (uri "ftp://tug.org/historic/systems/texlive/2016/texlive-20160523-extra.tar.xz")
    (sha256 (base32
              "0q4a92zmwhn4ry6xgrp4k8wq11ax2sg9rg9yrsrdkr719y0x887a"))))

(define texlive-texmf-src
  (origin
    (method url-fetch)
    (uri "ftp://tug.org/historic/systems/texlive/2016/texlive-20160523b-texmf.tar.xz")
    (patches (search-patches "texlive-texmf-CVE-2016-10243.patch"))
    (patch-flags '("-p2"))
    (sha256 (base32
              "1dv8vgfzpczqw82hv9g7a8djhhyzywljmrarlcyy6g2qi5q51glr"))))

(define-public texlive-bin
  (package
   (name "texlive-bin")
   (version "2016")
   (source
    (origin
     (method url-fetch)
      (uri "ftp://tug.org/historic/systems/texlive/2016/texlive-20160523b-source.tar.xz")
      (sha256 (base32
               "1v91vahxlxkdra0qz3f132vvx5d9cx2jy84yl1hkch0agyj2rcx8"))))
   (build-system gnu-build-system)
   (inputs
    `(("texlive-extra-src" ,texlive-extra-src)
      ("cairo" ,cairo)
      ("fontconfig" ,fontconfig)
      ("fontforge" ,fontforge)
      ("freetype" ,freetype)
      ("gd" ,gd)
      ("gmp" ,gmp)
      ("ghostscript" ,ghostscript)
      ("graphite2" ,graphite2)
      ("harfbuzz" ,harfbuzz)
      ("icu4c" ,icu4c)
      ("libpaper" ,libpaper)
      ("libpng" ,libpng)
      ("libxaw" ,libxaw)
      ("libxt" ,libxt)
      ("mpfr" ,mpfr)
      ("perl" ,perl)
      ("pixman" ,pixman)
      ("poppler" ,poppler)
      ("potrace" ,potrace)
      ("python" ,python-2) ; incompatible with Python 3 (print syntax)
      ("ruby" ,ruby)
      ("tcsh" ,tcsh)
      ("teckit" ,teckit)
      ("zlib" ,zlib)
      ("zziplib" ,zziplib)))
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (arguments
    `(#:out-of-source? #t
      #:configure-flags
       `("--disable-native-texlive-build"
         "--with-system-cairo"
         "--with-system-freetype2"
         "--with-system-gd"
         "--with-system-gmp"
         "--with-system-graphite2"
         "--with-system-harfbuzz"
         "--with-system-icu"
         "--with-system-libgs"
         "--with-system-libpaper"
         "--with-system-libpng"
         "--with-system-mpfr"
         "--with-system-pixman"
         "--with-system-poppler"
         "--with-system-potrace"
         "--with-system-teckit"
         "--with-system-xpdf"
         "--with-system-zlib"
         "--with-system-zziplib")

      ;; Disable tests on mips64 to cope with a failure of luajiterr.test.
      ;; XXX FIXME fix luajit properly on mips64.
      #:tests? ,(not (string-prefix? "mips64" (or (%current-target-system)
                                                  (%current-system))))
      #:phases
       (modify-phases %standard-phases
         (add-after 'install 'postint
           (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (texlive-extra (assoc-ref inputs "texlive-extra-src"))
                    (unpack (assoc-ref %standard-phases 'unpack))
                    (patch-source-shebangs
                      (assoc-ref %standard-phases 'patch-source-shebangs)))
               ;; Create symbolic links for the latex variants and their
               ;; man pages.
               (with-directory-excursion (string-append out "/bin/")
                 (for-each symlink
                 '("pdftex" "pdftex"   "xetex"   "luatex")
                 '("latex"  "pdflatex" "xelatex" "lualatex")))
               (with-directory-excursion (string-append share "/man/man1/")
                 (symlink "luatex.1" "lualatex.1"))
               ;; Unpack texlive-extra and install tlpkg.
               (mkdir "texlive-extra")
               (with-directory-excursion "texlive-extra"
                 (apply unpack (list #:source texlive-extra))
                 (apply patch-source-shebangs (list #:source texlive-extra))
                 (system* "mv" "tlpkg" share))))))))
   (synopsis "TeX Live, a package of the TeX typesetting system")
   (description
    "TeX Live provides a comprehensive TeX document production system.
It includes all the major TeX-related programs, macro packages, and fonts
that are free software, including support for many languages around the
world.

This package contains the binaries.")
   (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))
   (home-page "https://www.tug.org/texlive/")))

(define-public texlive-dvips
  (package
    (name "texlive-dvips")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/dvips"))
                    (revision %texlive-revision)))
              (sha256
               (base32
                "0fcy2hpapbj01ncpjj3v39yhr0jjxb6rm13qaxjjw66s3vydxls1"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/dvips")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "http://www.ctan.org/pkg/dvips")
    (synopsis "DVI to PostScript drivers")
    (description "This package provides files needed for converting DVI files
to PostScript.")
    ;; Various free software licenses apply to individual files.
    (license (list license:lppl1.3c+
                   license:expat
                   license:lgpl3+))))

(define-public texlive-generic-unicode-data
  (package
    (name "texlive-generic-unicode-data")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/generic/unicode-data"))
                    (revision %texlive-revision)))
              (sha256
               (base32
                "0ivrhp6jz31pl4z841g4ws41lmvdiwz4sslmhf02inlib79gz6r2"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/generic/unicode-data")))
             (mkdir-p target)
             (copy-recursively (assoc-ref %build-inputs "source") target)
             #t))))
    (home-page "http://www.ctan.org/pkg/unicode-data")
    (synopsis "Unicode data and loaders for TeX")
    (description "This bundle provides generic access to Unicode Consortium
data for TeX use.  It contains a set of text files provided by the Unicode
Consortium which are currently all from Unicode 8.0.0, with the exception of
@code{MathClass.txt} which is not currently part of the Unicode Character
Database.  Accompanying these source data are generic TeX loader files
allowing this data to be used as part of TeX runs, in particular in building
format files.  Currently there are two loader files: one for general character
set up and one for initializing XeTeX character classes as has been carried
out to date by @code{unicode-letters.tex}. ")
    (license license:lppl1.3c+)))

(define-public texlive-generic-dehyph-exptl
  (package
    (name "texlive-generic-dehyph-exptl")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/generic/dehyph-exptl"))
                    (revision %texlive-revision)))
              (sha256
               (base32
                "1l9wgv99qq0ysvlxqpj4g8bl0dywbzra4g8m2kmpg2fb0i0hczap"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/generic/dehyph-exptl")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "http://projekte.dante.de/Trennmuster/WebHome")
    (synopsis "Hyphenation patterns for German")
    (description "The package provides experimental hyphenation patterns for
the German language, covering both traditional and reformed orthography.  The
patterns can be used with packages Babel and hyphsubst from the Oberdiek
bundle.")
    ;; Hyphenation patterns are under the Expat license; documentation is
    ;; under LPPL.
    (license (list license:expat license:lppl))))

(define-public texlive-generic-tex-ini-files
  (package
    (name "texlive-generic-tex-ini-files")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/generic/tex-ini-files"))
                    (revision %texlive-revision)))
              (sha256
               (base32
                "1wh42n1lmzcvi3g6mm31nm3yd5ha5bl260xqc444jg1m9fdp3wz5"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/generic/tex-ini-files")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "http://ctan.org/pkg/tex-ini-files")
    (synopsis "Files for creating TeX formats")
    (description "This bundle provides a collection of model \".ini\" files
for creating TeX formats.  These files are commonly used to introduced
distribution-dependent variations in formats.  They are also used to
allow existing format source files to be used with newer engines, for example
to adapt the plain e-TeX source file to work with XeTeX and LuaTeX.")
    (license license:public-domain)))

(define-public texlive-generic-hyph-utf8
  (package
    (name "texlive-generic-hyph-utf8")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/generic/hyph-utf8"))
                    (revision %texlive-revision)))
              (sha256
               (base32
                "0ghizcz7ps16dzfqf66wwg5i181assc6qsm0g7g5dbmp909931vi"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/generic/hyph-utf8")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "http://ctan.org/pkg/hyph-utf8")
    (synopsis "Hyphenation patterns expressed in UTF-8")
    (description "Modern native UTF-8 engines such as XeTeX and LuaTeX need
hyphenation patterns in UTF-8 format, whereas older systems require
hyphenation patterns in the 8-bit encoding of the font in use (such encodings
are codified in the LaTeX scheme with names like OT1, T2A, TS1, OML, LY1,
etc).  The present package offers a collection of conversions of existing
patterns to UTF-8 format, together with converters for use with 8-bit fonts in
older systems.  Since hyphenation patterns for Knuthian-style TeX systems are
only read at iniTeX time, it is hoped that the UTF-8 patterns, with their
converters, will completely supplant the older patterns.")
    ;; Individual files each have their own license.  Most of these files are
    ;; independent hyphenation patterns.
    (license (list license:lppl1.0+
                   license:lppl1.2+
                   license:lppl1.3
                   license:lppl1.3+
                   license:lppl1.3a+
                   license:lgpl2.1
                   license:lgpl2.1+
                   license:lgpl3+
                   license:gpl2+
                   license:gpl3+
                   license:mpl1.1
                   license:asl2.0
                   license:expat
                   license:bsd-3
                   license:cc0
                   license:public-domain
                   license:wtfpl2))))

(define-public texlive-metafont-base
  (package
    (name "texlive-metafont-base")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/metafont"))
                    (revision %texlive-revision)))
              (sha256
               (base32
                "1yl4n8cn5xqk2nc22zgzq6ymd7bhm6xx1mz3azip7i3ki4bhb5q5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no test target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((cwd (getcwd)))
               (setenv "MFINPUTS"
                       (string-append cwd "/base:"
                                      cwd "/misc:"
                                      cwd "/roex:"
                                      cwd "/feynmf:"
                                      cwd "/mfpic:"
                                      cwd "/config")))
             (mkdir "build")
             (with-directory-excursion "build"
               (zero? (system* "inimf" "mf.mf")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (base (string-append out "/share/texmf-dist/web2c"))
                    (mf   (string-append out "/share/texmf-dist/metafont/base")))
               (mkdir-p base)
               (mkdir-p mf)
               (install-file "build/mf.base" base)
               (copy-recursively "base" mf)
               #t))))))
    (native-inputs
     `(("texlive-bin" ,texlive-bin)))
    (home-page "http://www.ctan.org/pkg/metafont")
    (synopsis "Metafont base files")
    (description "This package provides the Metafont base files needed to
build fonts using the Metafont system.")
    (license license:knuth)))

(define-public texlive-fonts-cm
  (package
    (name "texlive-fonts-cm")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/fonts/source/public/cm"))
                    (revision %texlive-revision)))
              (sha256
               (base32
                "045k5b9rdmbxpy1a3006l1x96z1rd18vg3cwrvnld9bqybw5qz44"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (srfi srfi-26))
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((mf (assoc-ref inputs "texlive-metafont-base")))
               ;; Tell mf where to find mf.base
               (setenv "MFBASES" (string-append mf "/share/texmf-dist/web2c"))
               ;; Tell mf where to look for source files
               (setenv "MFINPUTS"
                       (string-append (getcwd) ":"
                                      mf "/share/texmf-dist/metafont/base")))
             (mkdir "build")
             (every (lambda (font)
                      (format #t "building font ~a\n" font)
                      (zero? (system* "mf" "-progname=mf"
                                      "-output-directory=build"
                                      (string-append "\\"
                                                     "mode:=ljfour; "
                                                     "mag:=1; "
                                                     "batchmode; "
                                                     "input "
                                                     (basename font ".mf")))))
                    (find-files "." "cm(.*[0-9]+.*|inch)\\.mf$"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (tfm (string-append
                          out "/share/texmf-dist/fonts/tfm/public/cm"))
                    (mf  (string-append
                          out "/share/texmf-dist/fonts/source/public/cm")))
               (for-each (cut install-file <> tfm)
                         (find-files "build" "\\.*"))
               (for-each (cut install-file <> mf)
                         (find-files "." "\\.mf"))
               #t))))))
    (native-inputs
     `(("texlive-bin" ,texlive-bin)
       ("texlive-metafont-base" ,texlive-metafont-base)))
    (home-page "http://www.ctan.org/pkg/cm")
    (synopsis "Computer Modern fonts for TeX")
    (description "This package provides the Computer Modern fonts by Donald
Knuth.  The Computer Modern font family is a large collection of text,
display, and mathematical fonts in a range of styles, based on Monotype Modern
8A.")
    (license license:knuth)))

(define-public texlive-fonts-knuth-lib
  (package
    (name "texlive-fonts-knuth-lib")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/fonts/source/public/knuth-lib"))
                    (revision %texlive-revision)))
              (sha256
               (base32
                "0in9aqyi8jkyf9d16z0li50z5fpwj1iwgwm83gmvwqcf7chfs04y"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((mf (assoc-ref inputs "texlive-metafont-base")))
               ;; Tell mf where to find mf.base
               (setenv "MFBASES"
                       (string-append mf "/share/texmf-dist/web2c"))
               ;; Tell mf where to look for source files
               (setenv "MFINPUTS"
                       (string-append (getcwd) ":"
                                      mf "/share/texmf-dist/metafont/base")))
             (mkdir "build")
             (zero? (system* "mf" "-progname=mf"
                             "-output-directory=build"
                             (string-append "\\"
                                            "mode:=ljfour; "
                                            "mag:=1; "
                                            "batchmode; "
                                            "input manfnt")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (tfm (string-append
                          out "/share/texmf-dist/fonts/tfm/public/knuth-lib"))
                    (mf  (string-append
                          out "/share/texmf-dist/fonts/source/public/knuth-lib")))
               (for-each (cut install-file <> tfm)
                         (find-files "build" "\\.*"))
               (for-each (cut install-file <> mf)
                         (find-files "." "\\.mf"))
               #t))))))
    (native-inputs
     `(("texlive-bin" ,texlive-bin)
       ("texlive-metafont-base" ,texlive-metafont-base)))
    (home-page "https://www.ctan.org/pkg/knuth-lib")
    (synopsis "Small library of METAFONT sources")
    (description "This is a collection of core TeX and METAFONT macro files
from Donald Knuth, including the plain format, plain base, and the MF logo
fonts.")
    (license license:knuth)))

(define-public texlive-fonts-latex
  (package
    (name "texlive-fonts-latex")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/fonts/source/public/latex-fonts"))
                    (revision %texlive-revision)))
              (sha256
               (base32
                "0ypsm4xv9cw0jckk2qc7gi9hcmhf31mrg56pz3llyx3yd9vq2lps"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (srfi srfi-26))
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((mf (assoc-ref inputs "texlive-metafont-base")))
               ;; Tell mf where to find mf.base
               (setenv "MFBASES" (string-append mf "/share/texmf-dist/web2c"))
               ;; Tell mf where to look for source files
               (setenv "MFINPUTS"
                       (string-append (getcwd) ":"
                                      mf "/share/texmf-dist/metafont/base:"
                                      (assoc-ref inputs "texlive-fonts-cm")
                                      "/share/texmf-dist/fonts/source/public/cm")))
             (mkdir "build")
             (every (lambda (font)
                      (format #t "building font ~a\n" font)
                      (zero? (system* "mf" "-progname=mf"
                                      "-output-directory=build"
                                      (string-append "\\"
                                                     "mode:=ljfour; "
                                                     "mag:=1; "
                                                     "batchmode; "
                                                     "input " font))))
                    '("icmcsc10" "icmex10" "icmmi8" "icmsy8" "icmtt8"
                      "ilasy8" "ilcmss8" "ilcmssb8" "ilcmssi8"
                      "lasy5" "lasy6" "lasy7" "lasy8" "lasy9" "lasy10" "lasyb10"
                      "lcircle10" "lcirclew10" "lcmss8" "lcmssb8" "lcmssi8"
                      "line10" "linew10"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (tfm (string-append
                          out "/share/texmf-dist/fonts/tfm/public/latex-fonts"))
                    (mf  (string-append
                          out "/share/texmf-dist/fonts/source/public/latex-fonts")))
               (for-each (cut install-file <> tfm)
                         (find-files "build" "\\.*"))
               (for-each (cut install-file <> mf)
                         (find-files "." "\\.mf"))
               #t))))))
    (native-inputs
     `(("texlive-bin" ,texlive-bin)
       ("texlive-metafont-base" ,texlive-metafont-base)
       ("texlive-fonts-cm" ,texlive-fonts-cm)))
    (home-page "http://www.ctan.org/pkg/latex-fonts")
    (synopsis "Collection of fonts used in LaTeX distributions")
    (description "This is a collection of fonts for use with standard LaTeX
packages and classes. It includes invisible fonts (for use with the slides
class), line and circle fonts (for use in the picture environment) and LaTeX
symbol fonts.")
    (license license:lppl1.2+)))

;; This provides etex.src which is needed to build various formats, including
;; luatex.fmt and pdflatex.fmt
(define-public texlive-tex-plain
  (package
    (name "texlive-tex-plain")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/plain"))
                    (revision %texlive-revision)))
              (sha256
               (base32
                "0mjgl3gscn3ps29yjambz1j9fg81ynnncb96vpprwx4xsijhsns0"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/plain")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/plain")
    (synopsis "Plain TeX format and supporting files")
    (description
     "Contains files used to build the Plain TeX format, as described in the
TeXbook, together with various supporting files (some also discussed in the
book).")
    (license license:knuth)))

(define-public texlive-latex-base
  (let ((texlive-dir
         (lambda (dir hash)
           (origin
             (method svn-fetch)
             (uri (svn-reference
                   (url (string-append "svn://www.tug.org/texlive/tags/"
                                       %texlive-tag "/Master/texmf-dist/"
                                       dir))
                   (revision %texlive-revision)))
             (sha256 (base32 hash))))))
    (package
      (name "texlive-latex-base")
      (version (number->string %texlive-revision))
      (source (origin
                (method svn-fetch)
                (uri (texlive-ref "latex" "base"))
                (sha256
                 (base32
                  "1h9pir2hz6i9avc4lrl733p3zf4rpkg8537x1zdbhs91hvhikw9k"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (ice-9 match)
                    (srfi srfi-1)
                    (srfi srfi-26))
         #:tests? #f                    ; no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda* (#:key inputs #:allow-other-keys)
               ;; Find required fonts
               (setenv "TFMFONTS"
                       (string-append (assoc-ref inputs "texlive-fonts-cm")
                                      "/share/texmf-dist/fonts/tfm/public/cm:"
                                      (assoc-ref inputs "texlive-fonts-latex")
                                      "/share/texmf-dist/fonts/tfm/public/latex-fonts:"
                                      (assoc-ref inputs "texlive-fonts-knuth-lib")
                                      "/share/texmf-dist/fonts/tfm/public/knuth-lib"))
               (setenv "TEXINPUTS"
                       (string-append
                        (getcwd) ":"
                        (getcwd) "/build:"
                        (string-join
                         (append-map (match-lambda
                                       ((_ . dir)
                                        (find-files dir
                                                    (lambda (_ stat)
                                                      (eq? 'directory (stat:type stat)))
                                                    #:directories? #t
                                                    #:stat stat)))
                                     inputs)
                         ":")))

               ;; Create an empty texsys.cfg, because latex.ltx wants to include
               ;; it.  This file must exist and it's fine if it's empty.
               (with-output-to-file "texsys.cfg"
                 (lambda _ (format #t "%")))

               (mkdir "build")
               (mkdir "web2c")
               (and (zero? (system* "luatex" "-ini" "-interaction=batchmode"
                                    "-output-directory=build"
                                    "unpack.ins"))
                    (zero? (system* "tex" "-ini" "-interaction=batchmode"
                                    "-output-directory=web2c"
                                    "tex.ini"))
                    ;; LaTeX, pdfetex/pdftex, and XeTeX require e-TeX, which
                    ;; is enabled only in extended mode (activated with a
                    ;; leading asterisk).  We should not use luatex here,
                    ;; because that would make the generated format files
                    ;; incompatible with any other TeX engine.
                    (every
                     (lambda (format)
                       (zero? (system* "latex" "-ini" "-interaction=batchmode"
                                       "-output-directory=web2c"
                                       "-translate-file=cp227.tcx"
                                       (string-append "*" format ".ini"))))
                     '("latex"
                       "pdflatex"
                       "pdfetex"))
                    (every
                     (lambda (format)
                       (zero? (system* format "-ini" "-interaction=batchmode"
                                       "-output-directory=web2c"
                                       (string-append "*" format ".ini"))))
                     '("xetex"
                       "xelatex"))
                    (every
                     (lambda (format)
                       (zero? (system* "luatex" "-ini" "-interaction=batchmode"
                                       "-output-directory=web2c"
                                       (string-append format ".ini"))))
                     '("dviluatex" "dvilualatex" "luatex" "lualatex")))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (target (string-append
                               out "/share/texmf-dist/tex/latex/base"))
                      (web2c (string-append
                              out "/share/texmf-dist/web2c")))
                 (mkdir-p target)
                 (mkdir-p web2c)
                 (for-each delete-file (find-files "." "\\.(log|aux)$"))
                 (for-each (cut install-file <> target)
                           (find-files "build" ".*"))
                 (for-each (cut install-file <> web2c)
                           (find-files "web2c" ".*"))
                 ;; pdftex is really just the same as pdfetex, but since it
                 ;; doesn't have its own format file, we need to copy it.
                 (copy-file "web2c/pdfetex.fmt"
                            (string-append web2c "/pdftex.fmt"))
                 #t))))))
      (native-inputs
       `(("texlive-bin" ,texlive-bin)
         ("texlive-generic-unicode-data" ,texlive-generic-unicode-data)
         ("texlive-generic-dehyph-exptl" ,texlive-generic-dehyph-exptl)
         ("texlive-generic-tex-ini-files" ,texlive-generic-tex-ini-files)
         ("texlive-latex-latexconfig"
          ,(texlive-dir "tex/latex/latexconfig/"
                        "1zb3j49cj8p75yph6c8iysjp7qbdvghwf0mn9j0l7qq3qkbz2xaf"))
         ("texlive-generic-hyph-utf8" ,texlive-generic-hyph-utf8)
         ("texlive-generic-hyphen"
          ,(texlive-dir "tex/generic/hyphen/"
                        "0xim36wybw2625yd0zwlp9m2c2xrcybw58gl4rih9nkph0wqwwhd"))
         ("texlive-generic-ruhyphen"
          ,(texlive-dir "tex/generic/ruhyphen/"
                        "14rjkpl4zkjqs13rcf9kcd24mn2kx7i1jbdwxq8ds94bi66ylzsd"))
         ("texlive-generic-ukrhyph"
          ,(texlive-dir "tex/generic/ukrhyph/"
                        "1cfwdg2rhbayl3w0x1xqd36d45zbc96f029myp13s7cb6kbmbppv"))
         ("texlive-generic-config"
          ,(texlive-dir "tex/generic/config/"
                        "19vj088p4kkp6xll0141m4kl6ssgdzhs3g10i232khb07aqiag8s"))
         ("texlive-tex-plain" ,texlive-tex-plain)
         ("texlive-fonts-cm" ,texlive-fonts-cm)
         ("texlive-fonts-latex" ,texlive-fonts-latex)
         ("texlive-fonts-knuth-lib" ,texlive-fonts-knuth-lib)))
      (home-page "http://www.ctan.org/pkg/latex-base")
      (synopsis "Base sources of LaTeX")
      (description
       "This bundle comprises the source of LaTeX itself, together with several
packages which are considered \"part of the kernel\".  This bundle, together
with the required packages, constitutes what every LaTeX distribution should
contain.")
      (license license:lppl1.3c+))))

(define-public texlive-latex-filecontents
  (package
    (name "texlive-latex-filecontents")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "filecontents"))
              (sha256
               (base32
                "0swkbxv8vg0yizadfnvrwjb4cj0pn34v9wm6v7wqq903fdav7k7q"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/filecontents"))
    (home-page "http://www.ctan.org/pkg/filecontents")
    (synopsis "Extended filecontents and filecontents* environments")
    (description
     "LaTeX2e's @code{filecontents} and @code{filecontents*} environments
enable a LaTeX source file to generate external files as it runs through
LaTeX.  However, there are two limitations of these environments: they refuse
to overwrite existing files, and they can only be used in the preamble of a
document.  The filecontents package removes these limitations, letting you
overwrite existing files and letting you use @code{filecontents} /
@code{filecontents*} anywhere.")
    (license license:lppl1.3c+)))

(define-public texlive-generic-ifxetex
  (package
    (name "texlive-generic-ifxetex")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "generic" "ifxetex"))
              (sha256
               (base32
                "0w2xj7n0szavj329kds09q626szkc378p3w0sk022q0ln4ksz86d"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "generic/ifxetex"
       #:tex-format "xelatex"))
    (inputs
     `(("texlive-latex-filecontents" ,texlive-latex-filecontents)))
    (home-page "http://www.ctan.org/pkg/ifxetex")
    (synopsis "Am I running under XeTeX?")
    (description
     "This is a simple package which provides an @code{\\ifxetex} conditional,
so that other code can determine that it is running under XeTeX.  The package
requires the etexe-TeX extensions to the TeX primitive set.")
    (license license:lppl1.3c+)))

(define-public texlive-latex-fancyvrb
  (package
    (name "texlive-latex-fancyvrb")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "fancyvrb"))
              (sha256
               (base32
                "03l7140y031rr14h02i4z9zqsfvrbn7wzwxbjsrjcgrk6sdr71wv"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/fancyvrb"
       ;; We exclude "fvrb-ex" to avoid a dependency on texlive-luaotfload and
       ;; thus texlive-luatex-lualibs.
       #:build-targets '("fancyvrb.ins")))
    (home-page "http://www.ctan.org/pkg/fancyvrb")
    (synopsis "Sophisticated verbatim text")
    (description
     "This package provides tools for the flexible handling of verbatim text
including: verbatim commands in footnotes; a variety of verbatim environments
with many parameters; ability to define new customized verbatim environments;
save and restore verbatim text and environments; write and read files in
verbatim mode; build \"example\" environments (showing both result and
verbatim source).")
    (license license:lppl1.0+)))

(define-public texlive-latex-graphics
  (package
    (name "texlive-latex-graphics")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "graphics"))
              (sha256
               (base32
                "07azyn0b1s49vbdlr6dmygrminxp72ndl24j1091hiiccvrjq3xc"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/graphics"
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-config
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((cfg (assoc-ref inputs "graphics-cfg"))
                   (target (string-append (assoc-ref outputs "out")
                                          "/share/texmf-dist/tex/latex/graphics-cfg")))
               (mkdir-p target)
               (install-file (string-append cfg "/graphics.cfg") target)
               (install-file (string-append cfg "/color.cfg")    target)
               #t)))
         (add-after 'install 'install-defs
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((def (assoc-ref inputs "graphics-def"))
                   (target (string-append (assoc-ref outputs "out")
                                          "/share/texmf-dist/tex/latex/graphics-def")))
               (mkdir-p target)
               (copy-recursively def target)
               #t))))))
    (native-inputs
     `(("graphics-cfg"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/latex3/graphics-cfg.git")
                 (commit "19d1238af17df376cd46333b229579b0f7f3a41f")))
           (sha256
            (base32
             "12kbgbm52gmmgn8zajb74s8n5rvnxcfdvs3iyj8vcw5vrsw5i6mh"))))
       ("graphics-def"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/tex/latex/graphics-def"))
                 (revision %texlive-revision)))
           (sha256
            (base32
             "0gi4qv6378nl84s8n1yx3hjqvv7r4lza7hpbymbl5rzwgw8qrnyb"))))))
    (home-page "http://www.ctan.org/pkg/latex-graphics")
    (synopsis "LaTeX standard graphics bundle")
    (description
     "This is a collection of LaTeX packages for producing color, including
graphics (e.g. PostScript) files, and rotation and scaling of text in LaTeX
documents.  It comprises the packages color, graphics, graphicx, trig, epsfig,
keyval, and lscape.")
    ;; The configuration files are released under CC0.
    (license (list license:lppl1.3c+
                   license:cc0))))

(define-public texlive-latex-xcolor
  (package
    (name "texlive-latex-xcolor")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "xcolor"))
              (sha256
               (base32
                "01n613s7bcrd2n4jfawm0k4nn2ny3aaifp2jjfif3lz4sbv31494"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/xcolor"))
    (home-page "http://www.ctan.org/pkg/xcolor")
    (synopsis "Driver-independent color extensions for LaTeX and pdfLaTeX")
    (description
     "The package starts from the basic facilities of the colorcolor package,
and provides easy driver-independent access to several kinds of color tints,
shades, tones, and mixes of arbitrary colors.  It allows a user to select a
document-wide target color model and offers complete tools for conversion
between eight color models.  Additionally, there is a command for alternating
row colors plus repeated non-aligned material (like horizontal lines) in
tables.")
    (license license:lppl1.2+)))

(define-public texlive-latex-hyperref
  (package
    (name "texlive-latex-hyperref")
    (version "6.84a2")
    ;; The sources in the TeX Live SVN repository do not contain hluatex.dtx,
    ;; so we fetch the release from GitHub.
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ho-tex/hyperref/"
                                  "archive/release-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1d3rmjgzh0025a1dza55zb6nzzlgd1y9snwx45wq1c1vf42m79h2"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/hyperref"))
    (home-page "http://www.ctan.org/pkg/hyperref")
    (synopsis "Extensive support for hypertext in LaTeX")
    (description
     "The hyperref package is used to handle cross-referencing commands in
LaTeX to produce hypertext links in the document.  The package provides
backends for the special set defined for HyperTeX DVI processors; for embedded
pdfmark commands for processing by Acrobat Distiller (dvips and dvipsone); for
dviwindo; for PDF control within pdfTeX and dvipdfm; for TeX4ht; and for VTeX
pdf and HTML backends.  The package is distributed with the backref and
nameref packages, which make use of the facilities of hyperref.")
    (license license:lppl1.3+)))

(define-public texlive-latex-oberdiek
  (package
    (name "texlive-latex-oberdiek")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "oberdiek"))
              (sha256
               (base32
                "0aswvsxgsn709xmvpcg50d2xl7vcy1ckdxb9c1cligqqfjjvviqf"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/oberdiek"
       #:phases
       (modify-phases %standard-phases
         ;; "ifpdf.ins" is not generated, so we cannot process it.
         (add-after 'unpack 'do-not-process-ifpdf.ins
           (lambda _
             (substitute* "oberdiek.ins"
               (("\\\\batchinput\\{ifpdf.ins\\}") ""))
             #t)))))
    (home-page "http://www.ctan.org/pkg/oberdiek")
    (synopsis "Bundle of packages submitted by Heiko Oberdiek")
    (description
     "The bundle comprises various LaTeX packages, providing among others:
better accessibility support for PDF files; extensible chemists reaction
arrows; record information about document class(es) used; and many more.")
    (license license:lppl1.3+)))

(define-public texlive-latex-tools
  (package
    (name "texlive-latex-tools")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "tools"))
              (sha256
               (base32
                "052a0pch2k5zls5jlay9xxcf93rw3i60a2x28y3ip3rhbsv3xgiz"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/tools"
       #:build-targets '("tools.ins")))
    (home-page "http://www.ctan.org/pkg/latex-tools")
    (synopsis "LaTeX standard tools bundle")
    (description
     "This package is a collection of (variously) simple tools provided as
part of the LaTeX required tools distribution, comprising the following
packages: afterpage, array, bm, calc, dcolumn, delarray, enumerate, fileerr,
fontsmpl, ftnright, hhline, indentfirst, layout, longtable, multicol,
rawfonts, showkeys, somedefs, tabularx, theorem, trace, varioref, verbatim,
xr, and xspace.")
    (license license:lppl1.3+)))

(define-public texlive-latex-url
  (package
    (name "texlive-latex-url")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/url"))
                    (revision %texlive-revision)))
              (sha256
               (base32
                "184s2543cwia5l7iibhlkl1ffbncfhjpv5p56zq0c15by5sghlac"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/url")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/url")
    (synopsis "Verbatim with URL-sensitive line breaks")
    (description "The command @code{\\url} is a form of verbatim command that
allows linebreaks at certain characters or combinations of characters, accepts
reconfiguration, and can usually be used in the argument to another command.
The command is intended for email addresses, hypertext links,
directories/paths, etc., which normally have no spaces, so by default the
package ignores spaces in its argument.  However, a package option allows
spaces, which is useful for operating systems where spaces are a common part
of file names.")
    ;; The license header states that it is under LPPL version 2 or later, but
    ;; the latest version is 1.3c.
    (license license:lppl1.3c+)))

(define-public texlive-latex-l3kernel
  (package
    (name "texlive-latex-l3kernel")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "l3kernel"))
              (sha256
               (base32
                "0r0wfk594j8wkdqhh21haimwsfq8x5jch4ldm21hkzk5dnmvpbg6"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/l3kernel"))
    (home-page "http://www.ctan.org/pkg/l3kernel")
    (synopsis "LaTeX3 programmers’ interface")
    (description
     "The l3kernel bundle provides an implementation of the LaTeX3
programmers’ interface, as a set of packages that run under LaTeX 2e.  The
interface provides the foundation on which the LaTeX3 kernel and other future
code are built: it is an API for TeX programmers.  The packages are set up so
that the LaTeX3 conventions can be used with regular LaTeX 2e packages.")
    (license license:lppl1.3c+)))

(define-public texlive-latex-l3packages
  (package
    (name "texlive-latex-l3packages")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "l3packages"))
              (sha256
               (base32
                "16jplkvzdysfssijq9l051nsks65c2nrarsl17k8gjhc28yznj8y"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/l3packages"
       #:phases
       (modify-phases %standard-phases
         ;; All package sources are in sub-directories, so we need to add them
         ;; to TEXINPUTS.
         (add-after 'unpack 'set-TEXINPUTS
           (lambda _
             (let ((cwd (getcwd)))
               (setenv "TEXINPUTS"
                       (string-append cwd "/l3keys2e:"
                                      cwd "/xparse:"
                                      cwd "/xfrac:"
                                      cwd "/xfp:"
                                      cwd "/xtemplate")))
             #t)))))
    (inputs
     `(("texlive-latex-l3kernel" ,texlive-latex-l3kernel)))
    (home-page "http://www.ctan.org/pkg/l3packages")
    (synopsis "High-level LaTeX3 concepts")
    (description
     "This bundle holds prototype implementations of concepts for a LaTeX
designer interface, to be used with the experimental LaTeX kernel as
programming tools and kernel sup­port.  Packages provided in this release are:

@enumerate
@item l3keys2e, which makes the facilities of the kernel module l3keys
  available for use by LaTeX 2e packages;
@item xfrac, which provides flexible splitlevel fractions;
@item xparse, which provides a high-level interface for declaring document
  commands; and
@item xtemplate, which provides a means of defining generic functions using a
  key-value syntax.
@end enumerate\n")
    (license license:lppl1.3c+)))

(define-public texlive-latex-fontspec
  (package
    (name "texlive-latex-fontspec")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "fontspec"))
              (sha256
               (base32
                "1rx43y5xmjqvc27pjdnmqwp4pcw3czcfd6nfpmzc1gnqfl1hlc0q"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/fontspec"
       #:build-targets '("fontspec.dtx")))
    (inputs
     `(("texlive-latex-l3kernel" ,texlive-latex-l3kernel)))
    (home-page "http://www.ctan.org/pkg/fontspec")
    (synopsis "Advanced font selection in XeLaTeX and LuaLaTeX")
    (description
     "Fontspec is a package for XeLaTeX and LuaLaTeX.  It provides an
automatic and unified interface to feature-rich AAT and OpenType fonts through
the NFSS in LaTeX running on XeTeX or LuaTeX engines.  The package requires
the l3kernel and xparse bundles from the LaTeX 3 development team.")
    (license license:lppl1.3+)))

;; The SVN directory contains little more than a dtx file that generates three
;; of the many lua files that should be installed as part of this package.
;; This is why we take the release from GitHub instead.
(define-public texlive-luatex-lualibs
  (package
    (name "texlive-luatex-lualibs")
    (version "2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/lualatex/lualibs/"
                                  "releases/download/v"
                                  version "/lualibs.zip"))
              (sha256
               (base32
                "1xx9blvrmx9hyhrl345lpai9m6xxnw997261a1ahn1bm5r2j5fqy"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "DESTDIR="
                            (assoc-ref %outputs "out")
                            "/share/texmf-dist"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("texlive-bin" ,texlive-bin)
       ("unzip" ,unzip)
       ("zip" ,zip)))
    (home-page "https://github.com/lualatex/lualibs")
    (synopsis "Lua modules for general programming (in the (La)TeX world)")
    (description
     "Lualibs is a collection of Lua modules useful for general programming.
The bundle is based on Lua modules shipped with ConTeXt, and made available in
this bundle for use independent of ConTeXt.")
    ;; GPL version 2 only
    (license license:gpl2)))

(define-public texlive-latex-amsmath
  (package
    (name "texlive-latex-amsmath")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "amsmath"))
              (sha256
               (base32
                "178ywjpdlv78qmfzqdyn6gy14620zjsn2q9wap76fbr9s4hw6dba"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/amsmath"))
    (home-page "http://www.ctan.org/pkg/amsmath")
    (synopsis "AMS mathematical facilities for LaTeX")
    (description
     "This is the principal package in the AMS-LaTeX distribution.  It adapts
for use in LaTeX most of the mathematical features found in AMS-TeX; it is
highly recommended as an adjunct to serious mathematical typesetting in LaTeX.
When amsmath is loaded, AMS-LaTeX packages @code{amsbsyamsbsy} (for bold
symbols), @code{amsopnamsopn} (for operator names) and
@code{amstextamstext} (for text embedded in mathematics) are also loaded.
This package is part of the LaTeX required distribution; however, several
contributed packages add still further to its appeal; examples are
@code{empheqempheq}, which provides functions for decorating and highlighting
mathematics, and @code{ntheoremntheorem}, for specifying theorem (and similar)
definitions.")
    (license license:lppl1.3c+)))

(define-public texlive-latex-amscls
  (package
    (name "texlive-latex-amscls")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "amscls"))
              (sha256
               (base32
                "0jmcr37mcdi7drczppvr6lmz5d5yd9m67ii79gp2nglg1xpw934j"))))
    (build-system texlive-build-system)
    (arguments
     `(#:tex-directory "latex/amscls"))
    (home-page "http://www.ctan.org/pkg/amscls")
    (synopsis "AMS document classes for LaTeX")
    (description
     "This bundle contains three AMS classes: @code{amsartamsart} (for writing
articles for the AMS), @code{amsbookamsbook} (for books) and
@code{amsprocamsproc} (for proceedings), together with some supporting
material.  The material is made available as part of the AMS-LaTeX
distribution.")
    (license license:lppl1.3c+)))

(define-public texlive-latex-babel
  (package
    (name "texlive-latex-babel")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "babel"))
              (sha256
               (base32
                "1n3i5adsyy7jw0imnzrm2i8wkf73i3mjk9h3ic8cb9cd19i4r9r3"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/babel"
       #:phases
       (modify-phases %standard-phases
         ;; This package tries to produce babel.aux twice but refuses to
         ;; overwrite the first one.
         (add-before 'build 'fix-ins
           (lambda _
             (substitute* "babel.ins"
               (("askonceonly") "askforoverwritefalse"))
             #t)))))
    (home-page "http://www.ctan.org/pkg/babel")
    (synopsis "Multilingual support for Plain TeX or LaTeX")
    (description
     "The package manages culturally-determined typographical (and other)
rules, and hyphenation patterns for a wide range of languages.  A document may
select a single language to be supported, or it may select several, in which
case the document may switch from one language to another in a variety of
ways.  Babel uses contributed configuration files that provide the detail of
what has to be done for each language.  Users of XeTeX are advised to use the
polyglossia package rather than Babel.")
    (license license:lppl1.3+)))

(define-public texlive-latex-cyrillic
  (package
    (name "texlive-latex-cyrillic")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "cyrillic"))
              (sha256
               (base32
                "1mdhl35hwas68ki56qqngzar37dwv4mm64l2canihr255bz34lbv"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/cyrillic"))
    (home-page "http://www.ctan.org/pkg/latex-cyrillic")
    (synopsis "Support for Cyrillic fonts in LaTeX")
    (description
     "This bundle of macros files provides macro support (including font
encoding macros) for the use of Cyrillic characters in fonts encoded under the
T2* and X2 encodings. These encodings cover (between them) pretty much every
language that is written in a Cyrillic alphabet.")
    (license license:lppl1.3c+)))

(define-public texlive-latex-psnfss
  (package
    (name "texlive-latex-psnfss")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "psnfss"))
              (sha256
               (base32
                "1920dcq8613yzprasbg80fh4fcjcidvvl54wkx438nimyxcri7qz"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/psnfss"))
    (home-page "http://www.ctan.org/pkg/psnfss")
    (synopsis "Font support for common PostScript fonts")
    (description
     "The PSNFSS collection includes a set of files that provide a complete
working setup of the LaTeX font selection scheme (NFSS2) for use with common
PostScript fonts.  It covers the so-called \"Base\" fonts (which are built
into any Level 2 PostScript printing device and the Ghostscript interpreter)
and a number of free fonts.  It provides font definition files, macros and
font metrics.  The bundle as a whole is part of the LaTeX required set of
packages.")
    (license license:lppl1.2+)))

(define-public texlive-union
  (lambda* (#:optional (packages '()))
    "Return 'texlive-union' package which is a union of PACKAGES and the
standard LaTeX packages."
    (let ((default-packages
            (list texlive-bin
                  texlive-dvips
                  texlive-fonts-cm
                  texlive-fonts-latex
                  texlive-metafont-base
                  texlive-latex-base
                  ;; LaTeX packages from the "required" set.
                  texlive-latex-amsmath
                  texlive-latex-amscls
                  texlive-latex-babel
                  texlive-latex-cyrillic
                  texlive-latex-graphics
                  texlive-latex-psnfss
                  texlive-latex-tools)))
      (package
        (name "texlive-union")
        (version (number->string %texlive-revision))
        (source #f)
        (build-system trivial-build-system)
        (arguments
         '(#:modules ((guix build union)
                      (guix build utils)
                      (guix build texlive-build-system)
                      (guix build gnu-build-system)
                      (guix build gremlin)
                      (guix elf))
           #:builder
           (begin
             (use-modules (ice-9 match)
                          (srfi srfi-26)
                          (guix build union)
                          (guix build utils)
                          (guix build texlive-build-system))
             (let* ((out       (assoc-ref %outputs "out"))
                    (texmf.cnf (string-append out "/share/texmf-dist/web2c/texmf.cnf")))
               ;; Build a modifiable union of all inputs (but exclude bash)
               (match (filter (match-lambda
                                ((name . _)
                                 (not (string=? "bash" name))))
                              %build-inputs)
                 (((names . directories) ...)
                  (union-build (assoc-ref %outputs "out")
                               directories
                               #:create-all-directories? #t)))

               ;; The configuration file "texmf.cnf" is provided by the
               ;; "texlive-bin" package.  We take it and override only the
               ;; setting for TEXMFROOT and TEXMF.  This file won't be consulted
               ;; by default, though, so we still need to set TEXMFCNF.
               (substitute* texmf.cnf
                 (("^TEXMFROOT = .*")
                  (string-append "TEXMFROOT = " out "/share\n"))
                 (("^TEXMF = .*")
                  "TEXMF = $TEXMFROOT/share/texmf-dist\n"))
               (setenv "PATH" (string-append (assoc-ref %build-inputs "bash")
                                             "/bin"))
               (for-each
                (cut wrap-program <>
                     `("TEXMFCNF" ":" = (,(dirname texmf.cnf)))
                     `("TEXMF"    ":" = (,(string-append out "/share/texmf-dist"))))
                (find-files (string-append out "/bin") ".*"))
               #t))))
        (inputs
         `(("bash" ,bash)
           ,@(map (lambda (package)
                    (list (package-name package) package))
                  (append default-packages packages))))
        (home-page (package-home-page texlive-bin))
        (synopsis "Union of TeX Live packages")
        (description "This package provides a subset of the TeX Live
distribution.")
        (license (fold (lambda (package result)
                         (match (package-license package)
                           ((lst ...)
                            (append lst result))
                           ((? license:license? license)
                            (cons license result))))
                       '()
                       (append default-packages packages)))))))

(define-public texlive-tiny
  (package
    (inherit (texlive-union))
    (name "texlive-tiny")
    (description "This is a very limited subset of the TeX Live distribution.
It includes little more than the required set of LaTeX packages.")))

(define-public texlive-latex-natbib
  (package
    (name "texlive-latex-natbib")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "natbib"))
              (sha256
               (base32
                "0aqliq0nwblxyrzhwhv77pnmk7qh2y3prgq7z7qhwcbgz5kisld7"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/natbib"))
    (home-page "http://www.ctan.org/pkg/natbib")
    (synopsis "Flexible bibliography support")
    (description
     "This bundle provides a package that implements both author-year and
numbered references, as well as much detailed of support for other
bibliography use.  Also provided are versions of the standard BibTeX styles
that are compatible with @code{natbib}: @code{plainnat}, @code{unsrtnat},
@code{abbrnat}.  The bibliography styles produced by @code{custom-bib} are
designed from the start to be compatible with @code{natbib}.")
    (license license:lppl)))

(define-public texlive-latex-seminar
  (package
    (name "texlive-latex-seminar")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/seminar"))
                    (revision %texlive-revision)))
              (sha256
               (base32
                "0y4i651b75y6006n03x8n86bsqvjsailvvz9bhzy51dzsznqidq0"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/seminar"))
    (home-page "http://www.ctan.org/pkg/seminar")
    (synopsis "Make overhead slides")
    ;; TODO: This package may need fancybox and xcomment at runtime.
    (description
     "This package provides a class that produces overhead
slides (transparencies), with many facilities.  Seminar is not nowadays
reckoned a good basis for a presentation — users are advised to use more
recent classes such as powerdot or beamer, both of which are tuned to
21st-century presentation styles.")
    (license license:lppl1.2+)))

(define-public texlive-latex-hyperref
  (package
    (name "texlive-latex-hyperref")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "hyperref"))
              (sha256
               (base32
                "03arf3xvz1jsbvlpgc5qxbxbl9wmk8k09cn6b8gv9pzgpjy4vx4j"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/hyperref"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-hluatex.def
           (lambda _
             ;; This depends on hluatex.dtx, which does not exist and is
             ;; nowhere to be found in the sources of the TeX Live
             ;; distribution.
             (substitute* "hyperref.ins"
               (("\\\\file\\{hluatex.def\\}.*") ""))
             #t)))))
    ;; The package depends on the kvoptions, ltxcmds, and refcount packages,
    ;; which are part of the oberdiek bundle.
    (inputs
     `(("texlive-latex-oberdiek" ,texlive-latex-oberdiek)))
    (home-page "http://www.ctan.org/pkg/hyperref")
    (synopsis "Extensive support for hypertext in LaTeX")
    (description
     "The @code{hyperref} package is used to handle cross-referencing commands
in LaTeX to produce hypertext links in the document.  The package provides
backends for the @code{\\special} set defined for HyperTeX DVI processors; for
embedded @code{pdfmark} commands for processing by Acrobat
Distiller (@code{dvips} and Y&Y's @code{dvipsone}); for Y&Y's @code{dviwindo};
for PDF control within pdfTeX and @code{dvipdfm}; for TeX4ht; and for VTeX's
pdf and HTML backends.  The package is distributed with the @code{backref} and
@code{nameref} packages, which make use of the facilities of @code{hyperref}.")
    (license license:lppl1.3+)))

(define-public texlive-tex-texinfo
  (package
    (name "texlive-tex-texinfo")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/texinfo"))
                    (revision %texlive-revision)))
              (sha256
               (base32
                "09zj2w3lx0y6i2syfjjgizahf86z301dw8p37ln6syfhqhzqdz46"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/texinfo")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "http://www.ctan.org/pkg/texinfo")
    (synopsis "TeX macros to handle Texinfo files")
    (description
     "Texinfo is the preferred format for documentation in the GNU project;
the format may be used to produce online or printed output from a single
source.  The Texinfo macros may be used to produce printable output using TeX;
other programs in the distribution offer online interactive use (with
hypertext linkages in some cases).")
    (license license:gpl3+)))

(define texlive-texmf
  (package
   (name "texlive-texmf")
   (version "2016")
   (source texlive-texmf-src)
   (build-system gnu-build-system)
   (inputs
    `(("texlive-bin" ,texlive-bin)
      ("lua" ,lua)
      ("perl" ,perl)
      ("python" ,python-2) ; incompatible with Python 3 (print syntax)
      ("ruby" ,ruby)
      ("tcsh" ,tcsh)))
   (arguments
    `(#:modules ((guix build gnu-build-system)
                 (guix build utils)
                 (srfi srfi-26))

      ;; This package takes 4 GiB, which we can't afford to distribute from
      ;; our servers.
      #:substitutable? #f

      #:phases
        (modify-phases (map (cut assq <> %standard-phases)
                            '(set-paths unpack patch-source-shebangs))
          (add-after 'patch-source-shebangs 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((share (string-append (assoc-ref outputs "out") "/share")))
                (mkdir-p share)
                (system* "mv" "texmf-dist" share))))
          (add-after 'install 'texmf-config
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (share (string-append out "/share"))
                     (texmfroot (string-append share "/texmf-dist/web2c"))
                     (texmfcnf (string-append texmfroot "/texmf.cnf"))
                     (texlive-bin (assoc-ref inputs "texlive-bin"))
                     (texbin (string-append texlive-bin "/bin"))
                     (tlpkg (string-append texlive-bin "/share/tlpkg")))
                ;; Register SHARE as TEXMFROOT in texmf.cnf.
                (substitute* texmfcnf
                  (("TEXMFROOT = \\$SELFAUTOPARENT")
                   (string-append "TEXMFROOT = " share))
                  (("TEXMFLOCAL = \\$SELFAUTOGRANDPARENT/texmf-local")
                   "TEXMFLOCAL = $SELFAUTODIR/share/texmf-local")
                  (("!!\\$TEXMFLOCAL") "$TEXMFLOCAL"))
                ;; Register paths in texmfcnf.lua, needed for context.
                (substitute* (string-append texmfroot "/texmfcnf.lua")
                  (("selfautodir:") out)
                  (("selfautoparent:") (string-append share "/")))
                ;; Set path to TeXLive Perl modules
                (setenv "PERL5LIB"
                        (string-append (getenv "PERL5LIB") ":" tlpkg))
                ;; Configure the texmf-dist tree; inspired from
                ;; http://slackbuilds.org/repository/13.37/office/texlive/
                (setenv "PATH" (string-append (getenv "PATH") ":" texbin))
                (setenv "TEXMFCNF" texmfroot)
                (system* "updmap-sys" "--nohash" "--syncwithtrees")
                (system* "mktexlsr")
                (system* "fmtutil-sys" "--all")))))))
   (properties `((max-silent-time . 9600))) ; don't time out while grafting
   (synopsis "TeX Live, a package of the TeX typesetting system")
   (description
    "TeX Live provides a comprehensive TeX document production system.
It includes all the major TeX-related programs, macro packages, and fonts
that are free software, including support for many languages around the
world.

This package contains the complete tree of texmf-dist data.")
   (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))
   (home-page "https://www.tug.org/texlive/")))

(define-public texlive
  (package
   (name "texlive")
   (version "2016")
   (source #f)
   (build-system trivial-build-system)
   (inputs `(("bash" ,bash) ; for wrap-program
             ("texlive-bin" ,texlive-bin)
             ("texlive-texmf" ,texlive-texmf)))
   (native-search-paths
    (list (search-path-specification
           (variable "TEXMFLOCAL")
           (files '("share/texmf-local")))))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
        ;; Build the union of texlive-bin and texlive-texmf, but take the
        ;; conflicting subdirectory share/texmf-dist from texlive-texmf.
        (begin
          (use-modules (guix build utils))
          (let ((out (assoc-ref %outputs "out"))
                (bin (assoc-ref %build-inputs "texlive-bin"))
                (texmf (assoc-ref %build-inputs "texlive-texmf"))
                (bash (assoc-ref %build-inputs "bash")))
               (mkdir out)
               (with-directory-excursion out
                 (for-each
                   (lambda (name)
                     (symlink (string-append bin "/" name) name))
                   '("include" "lib"))
                 (mkdir "bin")
                 (with-directory-excursion "bin"
                   (setenv "PATH" (string-append bash "/bin"))
                   (for-each
                     (lambda (name)
                       (symlink name (basename name))
                       (wrap-program
                         (basename name)
                         `("TEXMFCNF" =
                           (,(string-append texmf "/share/texmf-dist/web2c")))))
                     (find-files (string-append bin "/bin/") "")))
                 (mkdir "share")
                 (with-directory-excursion "share"
                   (for-each
                     (lambda (name)
                       (symlink (string-append bin "/share/" name) name))
                     '("info" "man" "tlpkg"))
                   (for-each
                     (lambda (name)
                       (symlink (string-append texmf "/share/" name) name))
                     '("texmf-dist" "texmf-var"))))))))
   (synopsis "TeX Live, a package of the TeX typesetting system")
   (description
    "TeX Live provides a comprehensive TeX document production system.
It includes all the major TeX-related programs, macro packages, and fonts
that are free software, including support for many languages around the
world.

This package contains the complete TeX Live distribution.")
   (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))
   (home-page "https://www.tug.org/texlive/")))


;; texlive-texmf-minimal is a pruned, small version of the texlive tree,
;; in particular dropping documentation and fonts.  It weighs in at 470 MiB
;; instead of 4 GiB.
(define texlive-texmf-minimal
  (package (inherit texlive-texmf)
   (name "texlive-texmf-minimal")
   (arguments
    (substitute-keyword-arguments
     (package-arguments texlive-texmf)
     ((#:modules modules)
      `((ice-9 ftw)
        (srfi srfi-1)
        ,@modules))
     ((#:phases phases)
      `(modify-phases ,phases
         (add-after 'unpack 'prune
           (lambda _
             (define (delete subdir exclude)
               "Delete all files and directories in SUBDIR except for those
given in the list EXCLUDE."
               (with-directory-excursion subdir
                 (for-each delete-file-recursively
                           (lset-difference equal?
                                            (scandir ".")
                                            (append '("." "..")
                                                    exclude)))))
             (with-directory-excursion "texmf-dist"
               (for-each delete-file-recursively
                         '("doc" "source" "tex4ht"))
               ;; Delete all subdirectories of "fonts", except for "tfm" and
               ;; any directories named "cm".
               (delete "fonts" '("afm" "map" "pk" "source" "tfm" "type1"))
               (delete "fonts/afm" '("public"))
               (delete "fonts/afm/public" '("amsfonts"))
               (delete "fonts/afm/public/amsfonts" '("cm"))
               (delete "fonts/map" '("dvips"))
               (delete "fonts/map/dvips" '("cm"))
               (delete "fonts/source" '("public"))
               (delete "fonts/source/public" '("cm"))
               (delete "fonts/tfm" '("public"))
               (delete "fonts/type1" '("public"))
               (delete "fonts/type1/public" '("amsfonts"))
               (delete "fonts/type1/public/amsfonts" '("cm")))
             #t))))))
   (description
    "TeX Live provides a comprehensive TeX document production system.
It includes all the major TeX-related programs, macro packages, and fonts
that are free software, including support for many languages around the
world.

This package contains a small subset of the texmf-dist data.")))


;; texlive-minimal is the same as texlive, but using texlive-texmf-minimal
;; instead of the full texlive-texmf. It can be used, for instance, as a
;; native input to packages that need texlive to build their documentation.
(define-public texlive-minimal
  (package (inherit texlive)
   (name "texlive-minimal")
   (inputs
    `(("texlive-texmf" ,texlive-texmf-minimal)
      ,@(alist-delete "texlive-texmf" (package-inputs texlive))))
   (native-search-paths
    (list (search-path-specification
           (variable "TEXMFLOCAL")
           (files '("share/texmf-local")))))
   (description
    "TeX Live provides a comprehensive TeX document production system.
It includes all the major TeX-related programs, macro packages, and fonts
that are free software, including support for many languages around the
world.

This package contains a small working part of the TeX Live distribution.")))

(define-public perl-text-bibtex
  (package
    (name "perl-text-bibtex")
    (version "0.77")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AM/AMBS/Text-BibTeX-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0kkfx8skk763pivz6h2ffy2zdp1lvy6d5sz0kjaj0mdbjffvnnb4"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'add-output-directory-to-rpath
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "inc/MyBuilder.pm"
               (("-Lbtparse" line)
                (string-append "-Wl,-rpath="
                               (assoc-ref outputs "out") "/lib " line)))
             #t))
         (add-after 'unpack 'install-libraries-to-/lib
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Build.PL"
               (("lib64") "lib"))
             #t)))))
    (native-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)
       ("perl-config-autoconf" ,perl-config-autoconf)
       ("perl-extutils-libbuilder" ,perl-extutils-libbuilder)
       ("perl-module-build" ,perl-module-build)))
    (home-page "http://search.cpan.org/dist/Text-BibTeX")
    (synopsis "Interface to read and parse BibTeX files")
    (description "@code{Text::BibTeX} is a Perl library for reading, parsing,
and processing BibTeX files.  @code{Text::BibTeX} gives you access to the data
at many different levels: you may work with BibTeX entries as simple field to
string mappings, or get at the original form of the data as a list of simple
values (strings, macros, or numbers) pasted together.")
    (license license:perl-license)))

(define-public biber
  (package
    (name "biber-next")
    (version "2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/plk/biber/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "158smzgjhjvyabdv97si5q88zjj5l8j1zbfnddvzy6fkpfhskgkp"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-programs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (perl5lib (getenv "PERL5LIB")))
               (wrap-program (string-append out "/bin/biber")
                 `("PERL5LIB" ":" prefix
                   (,(string-append perl5lib ":" out
                                    "/lib/perl5/site_perl")))))
             #t)))))
    (inputs
     `(("perl-autovivification" ,perl-autovivification)
       ("perl-class-accessor" ,perl-class-accessor)
       ("perl-data-dump" ,perl-data-dump)
       ("perl-data-compare" ,perl-data-compare)
       ("perl-data-uniqid" ,perl-data-uniqid)
       ("perl-datetime-format-builder" ,perl-datetime-format-builder)
       ("perl-datetime-calendar-julian" ,perl-datetime-calendar-julian)
       ("perl-file-slurp" ,perl-file-slurp)
       ("perl-ipc-cmd" ,perl-ipc-cmd)
       ("perl-ipc-run3" ,perl-ipc-run3)
       ("perl-list-allutils" ,perl-list-allutils)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-mozilla-ca" ,perl-mozilla-ca)
       ("perl-regexp-common" ,perl-regexp-common)
       ("perl-log-log4perl" ,perl-log-log4perl)
       ;; We cannot use perl-unicode-collate here, because otherwise the
       ;; hardcoded hashes in the tests would differ.  See
       ;; https://mail-archive.com/debian-bugs-dist@lists.debian.org/msg1469249.html
       ;;("perl-unicode-collate" ,perl-unicode-collate)
       ("perl-unicode-normalize" ,perl-unicode-normalize)
       ("perl-unicode-linebreak" ,perl-unicode-linebreak)
       ("perl-encode-eucjpascii" ,perl-encode-eucjpascii)
       ("perl-encode-jis2k" ,perl-encode-jis2k)
       ("perl-encode-hanextra" ,perl-encode-hanextra)
       ("perl-xml-libxml" ,perl-xml-libxml)
       ("perl-xml-libxml-simple" ,perl-xml-libxml-simple)
       ("perl-xml-libxslt" ,perl-xml-libxslt)
       ("perl-xml-writer" ,perl-xml-writer)
       ("perl-sort-key" ,perl-sort-key)
       ("perl-text-csv" ,perl-text-csv)
       ("perl-text-csv-xs" ,perl-text-csv-xs)
       ("perl-text-roman" ,perl-text-roman)
       ("perl-uri" ,perl-uri)
       ("perl-text-bibtex" ,perl-text-bibtex)
       ("perl-libwww" ,perl-libwww)
       ("perl-lwp-protocol-https" ,perl-lwp-protocol-https)
       ("perl-business-isbn" ,perl-business-isbn)
       ("perl-business-issn" ,perl-business-issn)
       ("perl-business-ismn" ,perl-business-ismn)
       ("perl-lingua-translit" ,perl-lingua-translit)))
    (native-inputs
     `(("perl-config-autoconf" ,perl-config-autoconf)
       ("perl-extutils-libbuilder" ,perl-extutils-libbuilder)
       ("perl-module-build" ,perl-module-build)
       ;; for tests
       ("perl-file-which" ,perl-file-which)
       ("perl-test-more" ,perl-test-most) ; FIXME: "more" would be sufficient
       ("perl-test-differences" ,perl-test-differences)))
    (home-page "http://biblatex-biber.sourceforge.net/")
    (synopsis "Backend for the BibLaTeX citation management tool")
    (description "Biber is a BibTeX replacement for users of biblatex.  Among
other things it comes with full Unicode support.")
    (license license:artistic2.0)))

;; Our version of texlive comes with biblatex 3.4, which is only compatible
;; with biber 2.5 according to the compatibility matrix in the biber
;; documentation.
(define-public biber-2.5
  (package (inherit biber)
    (name "biber")
    (version "2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/plk/biber/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "163sd343wkrzwnvj2003m2j0kz517jmjr4savw6f8bjxhj8fdrqv"))))
    (arguments
     (substitute-keyword-arguments (package-arguments biber)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'check 'delete-failing-test
             (lambda _
               (delete-file "t/sort-order.t")
               #t))))))
    (inputs
     `(("perl-date-simple" ,perl-date-simple)
       ,@(package-inputs biber)))))

(define-public rubber
  (package
    (name "rubber")
    (version "1.1")
    (source (origin
             (method url-fetch)
             (uri (list (string-append "https://launchpad.net/rubber/trunk/"
                                       version "/+download/rubber-"
                                       version ".tar.gz")
                        (string-append "http://ebeffara.free.fr/pub/rubber-"
                                       version ".tar.gz")))
             (sha256
              (base32
               "1xbkv8ll889933gyi2a5hj7hhh216k04gn8fwz5lfv5iz8s34gbq"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f))                    ; no `check' target
    (inputs `(("texinfo" ,texinfo)
              ("python" ,python-2) ; incompatible with Python 3 (print syntax)
              ("which" ,which)))
    (home-page "https://launchpad.net/rubber")
    (synopsis "Wrapper for LaTeX and friends")
    (description
     "Rubber is a program whose purpose is to handle all tasks related to the
compilation of LaTeX documents.  This includes compiling the document itself,
of course, enough times so that all references are defined, and running BibTeX
to manage bibliographic references.  Automatic execution of dvips to produce
PostScript documents is also included, as well as usage of pdfLaTeX to produce
PDF documents.")
    (license license:gpl2+)))

(define-public texmaker
  (package
    (name "texmaker")
    (version "4.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.xm1math.net/texmaker/texmaker-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "056njk6j8wma23mlp7xa3rgfaxx0q8ynwx8wkmj7iy0b85p9ds9c"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Qt has its own configuration utility.
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (zero? (system* "qmake"
                               (string-append "PREFIX=" out)
                               (string-append "DESKTOPDIR=" out
                                              "/share/applications")
                               (string-append "ICONDIR=" out "/share/pixmaps")
                               "texmaker.pro"))))))))
    (inputs
     `(("poppler-qt5" ,poppler-qt5)
       ("qtbase" ,qtbase)
       ("qtscript" ,qtscript)
       ("qtwebkit" ,qtwebkit)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.xm1math.net/texmaker/")
    (synopsis "LaTeX editor")
    (description "Texmaker is a program that integrates many tools needed to
develop documents with LaTeX, in a single application.")
    (license license:gpl2+)))


(define-public teximpatient
  (package
    (name "teximpatient")
    (version "2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/" name "/" name "-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0h56w22d99dh4fgld4ssik8ggnmhmrrbnrn1lnxi1zr0miphn1sd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         ;; Unfortunately some mistakes have been made in packaging.
         ;; Work around them here ...
         (replace 'unpack
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((srcdir "teximpatient-2.4"))
               (system* "tar" "-xzf" (assoc-ref inputs "source")
                        (string-append "--one-top-level=" srcdir))
               (delete-file (string-append srcdir "/book.pdf"))
               (install-file (car
                              (find-files
                               (assoc-ref inputs "automake")
                               "^install-sh$"))
                             srcdir)
               (chdir srcdir)))))))
    (native-inputs
     `(("texlive" ,texlive)
       ("automake" ,automake)))
    (home-page "https://www.gnu.org/software/teximpatient/")
    (synopsis "Book on TeX, plain TeX and Eplain")
    (description "@i{TeX for the Impatient} is a ~350 page book on TeX,
plain TeX, and Eplain, originally written by Paul Abrahams, Kathryn Hargreaves,
and Karl Berry.")
    (license license:fdl1.3+)))
