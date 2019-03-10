;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system texlive)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
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
    (uri "ftp://tug.org/historic/systems/texlive/2018/texlive-20180414-extra.tar.xz")
    (sha256 (base32
             "0a83kymxc8zmlxjb0y1gf6mx7qnf0hxffwkivwh5yh138y2rfhsv"))))

(define texlive-texmf-src
  (origin
    (method url-fetch)
    (uri "ftp://tug.org/historic/systems/texlive/2018/texlive-20180414-texmf.tar.xz")
    (sha256 (base32
             "1b8zigzg8raxkhhzphcmynf84rbdbj2ym2qkz24v8n0qx82zmqms"))))

(define-public texlive-bin
  (package
   (name "texlive-bin")
   (version "20180414")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "ftp://tug.org/historic/systems/texlive/2018/"
                          "texlive-" version "-source.tar.xz"))
      (sha256
       (base32
        "0khyi6h015r2zfqgg0a44a2j7vmr1cy42knw7jbss237yvakc07y"))
      (patches
       (list
        ;; This is required for compatibility with Poppler 0.64.0 and to fix a
        ;; segmentation fault in dvipdfm-x from XeTeX, and also contains a fix
        ;; for CVE-2018-17407.
        (origin
          (method url-fetch)
          (uri (string-append "http://www.linuxfromscratch.org/patches/blfs/"
                              "svn/texlive-" version "-source-upstream_fixes-2.patch"))
          (file-name "texlive-poppler-compat.patch")
          (sha256
           (base32
            "04sxy1qv9y575mxwyg3y7rx7mh540pfjqx7yni7ncb5wjbq9pq1a")))
        (search-patch "texlive-bin-luatex-poppler-compat.patch")
        (search-patch "texlive-bin-pdftex-poppler-compat.patch")
        (search-patch "texlive-bin-xetex-poppler-compat.patch")))))
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
         ;; XXX: This is needed because recent Poppler requires C++11 or later.
         ;; Remove after switch to GCC >= 6.
         "CXXFLAGS=-std=gnu++11"
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

      ;; Disable tests on mips64/aarch64 to cope with a failure of luajiterr.test.
      ;; XXX FIXME fix luajit properly on mips64 and aarch64.
      #:tests? ,(let ((s (or (%current-target-system)
                             (%current-system))))
                  (not (or (string-prefix? "aarch64" s)
                           (string-prefix? "mips64" s))))
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'configure-ghostscript-executable
          ;; ps2eps.pl uses the "gswin32c" ghostscript executable on Windows,
          ;; and the "gs" ghostscript executable on Unix. It detects Unix by
          ;; checking for the existence of the /usr/bin directory. Since
          ;; GuixSD does not have /usr/bin, it is also detected as Windows.
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "utils/ps2eps/ps2eps-src/bin/ps2eps.pl"
              (("gswin32c") "gs"))
            (substitute* "texk/texlive/linked_scripts/epstopdf/epstopdf.pl"
              (("\"gs\"")
               (string-append "\"" (assoc-ref inputs "ghostscript") "/bin/gs\"")))
            #t))
        (add-after 'unpack 'use-code-for-new-poppler
          (lambda _
            (copy-file "texk/web2c/pdftexdir/pdftoepdf-newpoppler.cc"
                       "texk/web2c/pdftexdir/pdftoepdf.cc")
            (copy-file "texk/web2c/pdftexdir/pdftosrc-newpoppler.cc"
                       "texk/web2c/pdftexdir/pdftosrc.cc")
            #t))
        (add-after 'use-code-for-new-poppler 'use-code-for-even-newer-poppler
          (lambda _
            ;; Adjust for deprecated types in Poppler 0.73.
            (substitute* (append
                          (find-files "texk/web2c/luatexdir/" "\\.(cc|w)$")
                          '("texk/web2c/pdftexdir/pdftosrc.cc"))
              (("Guint") "unsigned int")
              (("Guchar") "unsigned char"))
            #t))
        (add-after 'unpack 'disable-failing-test
          (lambda _
            ;; FIXME: This test fails on 32-bit architectures since Glibc 2.28:
            ;; <https://bugzilla.redhat.com/show_bug.cgi?id=1631847>.
            (substitute* "texk/web2c/omegafonts/check.test"
              (("^\\./omfonts -ofm2opl \\$srcdir/tests/check tests/xcheck \\|\\| exit 1")
               "./omfonts -ofm2opl $srcdir/tests/check tests/xcheck || exit 77"))
            #t))
        (add-after 'install 'postint
          (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
            (let* ((out (assoc-ref outputs "out"))
                   (share (string-append out "/share"))
                   (texlive-extra (assoc-ref inputs "texlive-extra-src"))
                   (unpack (assoc-ref %standard-phases 'unpack))
                   (patch-source-shebangs
                    (assoc-ref %standard-phases 'patch-source-shebangs)))
              (substitute* (string-append share "/texmf-dist/web2c/texmf.cnf")
                ;; Don't truncate lines.
                (("^error_line = .*$") "error_line = 254\n")
                (("^half_error_line = .*$") "half_error_line = 238\n")
                (("^max_print_line = .*$") "max_print_line = 1000\n"))
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
                (invoke "mv" "tlpkg" share))
              ;; texlua shebangs are not patched by the patch-source-shebangs
              ;; phase because the texlua executable does not exist at that
              ;; time.
              (setenv "PATH" (string-append (getenv "PATH") ":" out "/bin"))
              (with-directory-excursion out
                (patch-source-shebangs))))))))
   (native-search-paths
    (list (search-path-specification
           (variable "TEXMF")
           (files '("share/texmf-dist"))
           (separator #f))
          (search-path-specification
           (variable "TEXMFCNF")
           (files '("share/texmf-dist/web2c"))
           (separator #f))))
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
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1ky6wc173jhf0b33lhyb4r3bx1d4bmiqkn6y1hxn92kwjdzl42p7"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((root (string-append (assoc-ref %outputs "out")
                                     "/share/texmf-dist"))
                (dvips (string-append root "/dvips"))
                (maps  (string-append root "/fonts/map/dvips"))
                (encs  (string-append root "/fonts/enc/dvips/base")))
           (mkdir-p dvips)
           (copy-recursively (assoc-ref %build-inputs "source") dvips)
           (mkdir-p maps)
           (copy-recursively (assoc-ref %build-inputs "dvips-font-maps") maps)
           (mkdir-p encs)
           (copy-recursively (assoc-ref %build-inputs "dvips-base-enc") encs)
           #t))))
    (native-inputs
     `(("dvips-font-maps"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/map/dvips"))
                 (revision %texlive-revision)))
           (file-name (string-append "dvips-font-maps-" version "-checkout"))
           (sha256
            (base32
             "0nxvfbb5vsvakiw0iviicghdc2sxk05cj056ilqnpa62fffc36a6"))))
       ("dvips-base-enc"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/enc/dvips/base"))
                 (revision %texlive-revision)))
           (file-name (string-append "dvips-base-enc-" version "-checkout"))
           (sha256
            (base32
             "1xnf6ms0h87r55kxik4vicwr1907scj789lhqflqns8svvsli5iy"))))))
    (home-page "https://www.ctan.org/pkg/dvips")
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
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0r1v16jyfpz6dwqsgm6b9jcj4kf2pkzc9hg07s6fx9g8ba8qglih"))))
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
    (home-page "https://www.ctan.org/pkg/unicode-data")
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
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "03yj1di9py92drp6gpfva6q69vk2iixr79r7cp7ja570s3pr0m33"))))
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
              (file-name (string-append name "-" version "-checkout"))
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
    (home-page "https://www.ctan.org/pkg/tex-ini-files")
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
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1alnn9cd60m2c12vym9f9q22ap1ngywxpkjl9dk472why44g1dmy"))))
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
    (home-page "https://ctan.org/pkg/hyph-utf8")
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
              (file-name (string-append name "-" version "-checkout"))
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
               (invoke "inimf" "mf.mf"))))
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
    (home-page "https://www.ctan.org/pkg/metafont")
    (synopsis "Metafont base files")
    (description "This package provides the Metafont base files needed to
build fonts using the Metafont system.")
    (license license:knuth)))

(define-public texlive-tex-fontinst-base
  (package
    (name "texlive-tex-fontinst-base")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/fontinst/base"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "12gnb8hc45p47pqn31msvi4mpr3wxbbbf2k4xhmshjqykwzlx508"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/fontinst/base")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/fontinst")
    (synopsis "Tools for converting and installing fonts for TeX and LaTeX")
    (description "This package provides TeX macros for converting Adobe Font
Metric files to TeX metric and virtual font format.  Fontinst helps mainly
with the number crunching and shovelling parts of font installation.  This
means in practice that it creates a number of files which give the TeX
metrics (and related information) for a font family that TeX needs to do any
typesetting in these fonts.")
    (license license:lppl1.1+)))

(define-public texlive-fontname
  (package
    (name "texlive-fontname")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/fonts/map/fontname"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "05rbn7z30xawd3n6w7c3ijp2yc67ga220jgqmkla9pd9wx185rzq"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/fonts/map/fontname")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/fontname")
    (synopsis "Scheme for naming fonts in TeX")
    (description "This is Fontname, a naming scheme for (the base part of)
external TeX font filenames.  This makes at most eight-character names
from (almost) arbitrarily complex font names, thus helping portability of TeX
documents.")
    (license license:public-domain)))

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
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0vfjhidr9pha613h8mfhnpcpvld6ahdfb449918fpsfs93cppkyj"))))
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
             (mkdir-p "pk/ljfour/public/cm/dpi600")
             (for-each (lambda (font)
                         (format #t "building font ~a\n" font)
                         (invoke "mf" "-progname=mf"
                                 "-output-directory=build"
                                 (string-append "\\"
                                                "mode:=ljfour; "
                                                "mag:=1+0/600; "
                                                "batchmode; "
                                                "input "
                                                (basename font ".mf")))
                         (invoke "gftopk"
                                 (string-append "build/"
                                                (basename font ".mf") ".600gf")
                                 (string-append "pk/ljfour/public/cm/dpi600/"
                                                (basename font ".mf") ".pk")))
                       (find-files "." "cm(.*[0-9]+.*|inch)\\.mf$"))
             #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (fonts (string-append out "/share/texmf-dist/fonts/"))
                    (pk    (string-append fonts "pk"))
                    (tfm   (string-append fonts "tfm/public/cm"))
                    (mf    (string-append fonts "source/public/cm"))
                    (type1 (string-append fonts "type1/public/amsfonts/cm")))
               (for-each (cut install-file <> tfm)
                         (find-files "build" "\\.*"))
               (for-each (cut install-file <> mf)
                         (find-files "." "\\.mf"))
               (copy-recursively "pk" pk)
               (mkdir-p type1)
               (copy-recursively (assoc-ref inputs "cm-type1") type1)
               #t))))))
    (native-inputs
     `(("texlive-bin" ,texlive-bin)
       ("texlive-metafont-base" ,texlive-metafont-base)
       ("cm-type1"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/type1/public/amsfonts/cm"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-type1-" version "-checkout"))
           (sha256
            (base32
             "12jyl9jp3hidifa4l5pmi47p71d5mb5kj5rknxkygilix8yz2iy6"))))))
    (home-page "https://www.ctan.org/pkg/cm")
    (synopsis "Computer Modern fonts for TeX")
    (description "This package provides the Computer Modern fonts by Donald
Knuth.  The Computer Modern font family is a large collection of text,
display, and mathematical fonts in a range of styles, based on Monotype Modern
8A.")
    (license license:knuth)))

(define-public texlive-fonts-cm-super
  (package
    (name "texlive-fonts-cm-super")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/cm-super"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0ybb4gi2rblzpb6wfzm2wk7dj3y2jnmkzsla7mz7g3zc12y4r2b9"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (ice-9 match))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match))
         (let ((root (string-append (assoc-ref %outputs "out")
                                    "/share/texmf-dist/"))
               (pkgs '(("source"         . "tex/latex/cm-super")
                       ("cm-super-afm"   . "fonts/afm/public/cm-super")
                       ("cm-super-type1" . "fonts/type1/public/cm-super")
                       ("cm-super-enc"   . "fonts/enc/dvips/cm-super")
                       ("cm-super-map"   . "fonts/map/dvips/cm-super")
                       ("cm-super-vtex"  . "fonts/map/vtex/cm-super"))))
           (for-each (match-lambda
                       ((pkg . dir)
                        (let ((target (string-append root dir)))
                          (mkdir-p target)
                          (copy-recursively (assoc-ref %build-inputs pkg)
                                            target))))
                     pkgs)
           #t))))
    (native-inputs
     `(("cm-super-vtex"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/map/vtex/cm-super"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-map-vtex-" version "-checkout"))
           (sha256
            (base32
             "14c9allsgfv6za9wznz4cxqxwz5nsmj8rnwvxams8fhs5rvglxqi"))))
       ("cm-super-afm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/afm/public/cm-super"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-afm-" version "-checkout"))
           (sha256
            (base32
             "048ih65f2nghdabdar2p957c4s2spgllmy2gxdscddwqpnmd26yn"))))
       ("cm-super-type1"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/type1/public/cm-super"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-type1-" version "-checkout"))
           (sha256
            (base32
             "1140swk3w2ka0y4zdsq6pdifrdanb281q71p5gngbbjxdxjxf4qx"))))
       ("cm-super-map"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/map/dvips/cm-super"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-map-" version "-checkout"))
           (sha256
            (base32
             "10r6xqbwf9wk3ylg7givwyrw1952zydc6p7fw29zjf8ijl0lndim"))))
       ("cm-super-enc"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/enc/dvips/cm-super"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-enc-" version "-checkout"))
           (sha256
            (base32
             "1pgksy96gfgyjxfhs2k04bgg7nr7i128y01kjcahr7n38080h4ij"))))))
    (home-page "https://www.ctan.org/pkg/cm-super")
    (synopsis "Computer Modern Super family of fonts")
    (description "The CM-Super family provides Adobe Type 1 fonts that replace
the T1/TS1-encoded Computer Modern (EC/TC), T1/TS1-encoded Concrete,
T1/TS1-encoded CM bright and LH Cyrillic fonts (thus supporting all European
languages except Greek), and bringing many ameliorations in typesetting
quality.  The fonts exhibit the same metrics as the METAFONT-encoded
originals.")
    ;; With font exception
    (license license:gpl2+)))

(define-public texlive-fonts-lm
  (package
    (name "texlive-fonts-lm")
    (version "2.004")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.gust.org.pl/projects/e-foundry/"
                                  "latin-modern/download/lm" version "bas.zip"))
              (sha256
               (base32
                "0z2s253y751m2ci5aw8nq0sf2kyg9hpimv2gyixkch9d07m2b9wp"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((root (string-append (assoc-ref %outputs "out")
                                    "/share/texmf-dist/")))
           (mkdir-p root)
           (with-directory-excursion root
             (invoke (string-append (assoc-ref %build-inputs "unzip")
                                    "/bin/unzip")
                     (assoc-ref %build-inputs "source")))
           #t))))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://www.gust.org.pl/projects/e-foundry/latin-modern/")
    (synopsis "Latin Modern family of fonts")
    (description "The Latin Modern fonts are derived from the famous Computer
Modern fonts designed by Donald E. Knuth and described in Volume E of his
Computers & Typesetting series.")
    ;; The GUST font license (GFL) is legally identical to the LaTeX Project
    ;; Public License (LPPL), version 1.3c or later, but comes with an
    ;; additional but not legally binding clause.
    (license license:lppl1.3c+)))

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
              (file-name (string-append name "-" version "-checkout"))
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
             (for-each (lambda (font)
                         (format #t "building font ~a\n" font)
                         (invoke "mf" "-progname=mf"
                                 "-output-directory=build"
                                 (string-append "\\"
                                                "mode:=ljfour; "
                                                "mag:=1; "
                                                "batchmode; "
                                                "input " font)))
                       (find-files "." "(manfnt|logo.+)\\.mf$"))
             #t))
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
              (file-name (string-append name "-" version "-checkout"))
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
             (for-each (lambda (font)
                         (format #t "building font ~a\n" font)
                         (invoke "mf" "-progname=mf"
                                 "-output-directory=build"
                                 (string-append "\\"
                                                "mode:=ljfour; "
                                                "mag:=1; "
                                                "batchmode; "
                                                "input " font)))
                       '("icmcsc10" "icmex10" "icmmi8" "icmsy8" "icmtt8"
                         "ilasy8" "ilcmss8" "ilcmssb8" "ilcmssi8"
                         "lasy5" "lasy6" "lasy7" "lasy8" "lasy9" "lasy10" "lasyb10"
                         "lcircle10" "lcirclew10" "lcmss8" "lcmssb8" "lcmssi8"
                         "line10" "linew10"))
             #t))
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
    (home-page "https://www.ctan.org/pkg/latex-fonts")
    (synopsis "Collection of fonts used in LaTeX distributions")
    (description "This is a collection of fonts for use with standard LaTeX
packages and classes. It includes invisible fonts (for use with the slides
class), line and circle fonts (for use in the picture environment) and LaTeX
symbol fonts.")
    (license license:lppl1.2+)))

(define-public texlive-latex-mflogo
  (package
    (name "texlive-latex-mflogo")
    (version (number->string %texlive-revision))
    (source
     (origin
       (method svn-fetch)
       (uri (texlive-ref "latex" "mflogo"))
       (sha256
        (base32
         "15i2ib6nvhf31g1b92c6njf7n0g29znlq7hbfp9ii7qabhcwwvrj"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/mflogo"))
    (home-page "http://www.ctan.org/pkg/mflogo")
    (synopsis "LaTeX support for Metafont logo fonts")
    (description
     "This package provides LaTeX and font definition files to access the
Knuthian mflogo fonts described in The Metafontbook and to typeset Metafont
logos in LaTeX documents.")
    (license license:lppl)))

(define-public texlive-fonts-mflogo-font
  (package
    (name "texlive-fonts-mflogo-font")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/fonts/type1/hoekwater/mflogo-font"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "09fsxfpiyxjljkrb52b197728bjnkcnv3bdwm4hl6hf23mbmqadf"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (ice-9 match))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match))
         (let ((root (string-append (assoc-ref %outputs "out")
                                    "/share/texmf-dist/"))
               (pkgs '(("source"    . "fonts/type1/hoekwater/mflogo-font")
                       ("afm"       . "fonts/afm/hoekwater/mflogo-font")
                       ("fonts-map" . "fonts/map/dvips/mflogo-font"))))
           (for-each (match-lambda
                       ((pkg . dir)
                        (let ((target (string-append root dir)))
                          (mkdir-p target)
                          (copy-recursively (assoc-ref %build-inputs pkg)
                                            target))))
                     pkgs)
           #t))))
    (native-inputs
     `(("afm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/afm/hoekwater/mflogo-font"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-afm-" version "-checkout"))
           (sha256
            (base32
             "0bx1mfhhzsk9gj1pha36q2rk0jd0y285qm62zgvdvzzzlfnk8sdb"))))
       ("fonts-map"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/map/dvips/mflogo-font/"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-fonts-map-" version "-checkout"))
           (sha256
            (base32
             "044xrrpl8hnvj55cx2ql1ib1bcyr33nzanx5nkwxpai7fb7pg4y6"))))))
    (home-page "https://www.ctan.org/pkg/mflogo-font")
    (synopsis "Metafont logo font")
    (description
     "These fonts were created in METAFONT by Knuth, for his own publications.
At some stage, the letters P and S were added, so that the METAPOST logo could
also be expressed.  The fonts were originally issued (of course) as METAFONT
source; they have since been autotraced and reissued in Adobe Type 1 format by
Taco Hoekwater.")
    (license license:knuth)))

(define-public texlive-fonts-amsfonts
  (package
    (name "texlive-fonts-amsfonts")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/fonts/source/public/amsfonts"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "07h20rvpbdb4k72hzmjkyb29426zr9wxsfp6yd4ajbbpd3vx8grb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 match)
                  (srfi srfi-1)
                  (srfi srfi-26))
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((mf (assoc-ref inputs "texlive-union"))
                   (cwd (getcwd)))
               ;; Make METAFONT reproducible
               (setenv "SOURCE_DATE_EPOCH" "1")
               ;; Tell mf where to find mf.base
               (setenv "MFBASES" (string-append mf "/share/texmf-dist/web2c"))
               ;; Tell mf where to look for source files
               (setenv "MFINPUTS"
                       (string-append cwd ":"
                                      cwd "/cmextra:"
                                      cwd "/cyrillic:"
                                      cwd "/dummy:"
                                      cwd "/symbols:"
                                      mf "/share/texmf-dist/metafont/base:"
                                      (assoc-ref inputs "texlive-fonts-cm")
                                      "/share/texmf-dist/fonts/source/public/cm")))
             (mkdir "build")
             (for-each (lambda (font)
                         (format #t "building font ~a\n" (basename font ".mf"))
                         (with-directory-excursion (dirname font)
                           (invoke "mf" "-progname=mf"
                                   "-output-directory=../build"
                                   (string-append "\\"
                                                  "mode:=ljfour; "
                                                  "mag:=1; "
                                                  "nonstopmode; "
                                                  "input "
                                                  (getcwd) "/"
                                                  (basename font ".mf")))))
                       (find-files "." "[0-9]+\\.mf$"))

             ;; There are no metafont sources for the Euler fonts, so we
             ;; convert the afm files instead.
             (mkdir "build/euler")
             (for-each (lambda (font)
                         (format #t "converting afm font ~a\n" (basename font ".afm"))
                         (invoke "afm2tfm" font
                                 (string-append "build/euler/"
                                                (basename font ".tfm"))))
                       (find-files (assoc-ref inputs "amsfonts-afm")
                                   "\\.afm$"))

             ;; Frustratingly, not all fonts can be created this way.  To
             ;; generate eufm8.tfm, for example, we first scale down
             ;; eufm10.afm to eufm8.pl, and then generate the tfm file from
             ;; the pl file.
             (with-directory-excursion "build/euler"
               (setenv "TEXINPUTS"
                       (string-append (getcwd) "//:"
                                      (assoc-ref inputs "amsfonts-afm") "//:"
                                      (assoc-ref inputs "texlive-union") "//"))
               (for-each (match-lambda
                           (((target-base target-size)
                             (source-base source-size))
                            (let ((factor (number->string
                                           (truncate/ (* 1000 target-size)
                                                      source-size))))
                              (invoke "tex"
                                      "-interaction=scrollmode"
                                      (string-append "\\input fontinst.sty "
                                                     "\\transformfont{" target-base "}"
                                                     "{\\scalefont{" factor "}"
                                                     "{\\fromafm{" source-base "}}} "
                                                     "\\bye")))
                            (invoke "pltotf"
                                    (string-append target-base ".pl")
                                    (string-append target-base ".tfm"))
                            (delete-file (string-append target-base ".pl"))))

                         '((("eufm8" 8) ("eufm10" 10))

                           (("eufb6" 6) ("eufb7" 7))
                           (("eufb8" 8) ("eufb10" 10))
                           (("eufb9" 9) ("eufb10" 10))

                           (("eufm6" 6) ("eufb7" 7))
                           (("eufm9" 9) ("eufb10" 10))

                           (("eurb6" 6) ("eurb7" 7))
                           (("eurb8" 8) ("eurb10" 10))
                           (("eurb9" 9) ("eurb10" 10))

                           (("eurm6" 6) ("eurm7" 7))
                           (("eurm8" 8) ("eurm10" 10))
                           (("eurm9" 9) ("eurm10" 10)))))
             #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (root (string-append out "/share/texmf-dist/fonts/"))
                    (pkgs '(("amsfonts-afm"   . "afm/public/amsfonts")
                            ("amsfonts-type1" . "type1/public/amsfonts")
                            ("amsfonts-map"   . "map/dvips/amsfonts"))))
               (for-each (match-lambda
                           ((pkg . dir)
                            (let ((target (string-append root dir)))
                              (mkdir-p target)
                              (copy-recursively (assoc-ref inputs pkg)
                                                target))))
                         pkgs)
               (copy-recursively (assoc-ref inputs "amsfonts-plain")
                                 (string-append out "/share/texmf-dist/tex/plain/amsfonts"))
               (let* ((tfm (string-append root "tfm/public/amsfonts"))
                      (mf  (string-append root "source/public/amsfonts")))
                 (copy-recursively "build" tfm)
                 (for-each (cut install-file <> mf)
                           (find-files "." "\\.mf"))
                 #t)))))))
    (native-inputs
     `(("texlive-union" ,(texlive-union (list texlive-tex-fontinst-base
                                              texlive-fonts-cm
                                              texlive-metafont-base)))
       ("amsfonts-plain"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/tex/plain/amsfonts"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-plain-" version "-checkout"))
           (sha256
            (base32
             "1hi8c9rkfb6395sxf7fhkr91xygfg8am1hqij9g3h2c7qx3714qp"))))
       ("amsfonts-map"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/map/dvips/amsfonts"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-map-" version "-checkout"))
           (sha256
            (base32
             "1lrj3bd9ybj4aawzlygc6qvakbrwc5s0mc5n9rpic331frv3axfs"))))
       ("amsfonts-type1"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/type1/public/amsfonts"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-type1-" version "-checkout"))
           (sha256
            (base32
             "1zfz33vn6gm19njy74n8wmn7sljrimfhwns5z8qqhxqfh1g4qip2"))))
       ("amsfonts-afm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/afm/public/amsfonts"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-afm-" version "-checkout"))
           (sha256
            (base32
             "1fifzkaihmjgchnk7dmw0c23k0cz999dxnc78ivmqvgi1dhx0iv8"))))))
    (home-page "https://www.ctan.org/pkg/amsfonts")
    (synopsis "TeX fonts from the American Mathematical Society")
    (description
     "This package provides an extended set of fonts for use in mathematics,
including: extra mathematical symbols; blackboard bold letters (uppercase
only); fraktur letters; subscript sizes of bold math italic and bold Greek
letters; subscript sizes of large symbols such as sum and product; added sizes
of the Computer Modern small caps font; cyrillic fonts (from the University of
Washington); Euler mathematical fonts.  All fonts are provided as Adobe Type 1
files, and all except the Euler fonts are provided as Metafont source.  The
distribution also includes the canonical Type 1 versions of the Computer
Modern family of fonts.  The Euler fonts are supported by separate packages;
details can be found in the documentation.")
    (license license:silofl1.1)))

(define-public texlive-latex-amsfonts
  (package
    (name "texlive-latex-amsfonts")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "amsfonts"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0slzfv5h2m03b2xvm2sasznz4azh6rgi069z161dja3l8rln79hm"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/amsfonts"))
    (native-inputs
     `(("texlive-fonts-cm" ,texlive-fonts-cm)
       ("texlive-metafont-base" ,texlive-metafont-base)))
    (home-page "https://www.ctan.org/pkg/amsfonts")
    (synopsis "TeX fonts from the American Mathematical Society")
    (description
     "This package provides basic LaTeX support for the symbol fonts provides
by the amsfonts package.  It provides @code{amsfonts.sty}, with names of
individual symbols defined in @code{amssymb.sty}.")
    (license license:silofl1.1)))

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
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1xknlb3gcw6jjqh97bhghxi594bzpj1zfzzfsrr9pvr9s1bx7dnf"))))
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
             (file-name (string-append "texlive-generic-"
                                       (last (string-split
                                               (string-drop-right dir 1) #\/))
                                       "-" (number->string %texlive-revision)
                                       "-checkout"))
             (sha256 (base32 hash))))))
    (package
      (name "texlive-latex-base")
      (version (number->string %texlive-revision))
      (source (origin
                (method svn-fetch)
                (uri (texlive-ref "latex" "base"))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "17bqrzzjz16k52sc7ydl4vw7ddy2z3g0p1xsk2c35h1ynq9h3wwm"))))
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
                         (map (match-lambda ((_ . dir) dir)) inputs)
                         "//:")))

               ;; Create an empty texsys.cfg, because latex.ltx wants to include
               ;; it.  This file must exist and it's fine if it's empty.
               (with-output-to-file "texsys.cfg"
                 (lambda _ (format #t "%")))

               (mkdir "build")
               (mkdir "web2c")
               (invoke "luatex" "-ini" "-interaction=batchmode"
                       "-output-directory=build" "unpack.ins")
               (invoke "tex" "-ini" "-interaction=batchmode"
                       "-output-directory=web2c" "tex.ini")
               ;; LaTeX, pdfetex/pdftex, and XeTeX require e-TeX, which
               ;; is enabled only in extended mode (activated with a
               ;; leading asterisk).  We should not use luatex here,
               ;; because that would make the generated format files
               ;; incompatible with any other TeX engine.
               (for-each (lambda (format)
                           (invoke "latex" "-ini" "-interaction=batchmode"
                                   "-output-directory=web2c"
                                   "-translate-file=cp227.tcx"
                                   (string-append "*" format ".ini")))
                         '("latex"
                           "pdflatex"
                           "pdfetex"))
               (for-each (lambda (format)
                           (invoke format "-ini" "-interaction=batchmode"
                                   "-output-directory=web2c"
                                   (string-append "*" format ".ini")))
                         '("xetex"
                           "xelatex"))
               (for-each (lambda (format)
                           (invoke "luatex" "-ini" "-interaction=batchmode"
                                   "-output-directory=web2c"
                                   (string-append format ".ini")))
                         '("dviluatex" "dvilualatex" "luatex" "lualatex"))
               #t))
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (target (string-append
                               out "/share/texmf-dist/tex/latex/base"))
                      (web2c (string-append
                              out "/share/texmf-dist/web2c"))
                      (support-files (assoc-ref inputs "texlive-latex-base-support-files")))
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
                 ;; "source" is missing the support files as per doc/latex/base/manifest.txt.
                 ;; FIXME: We are probably not packaging this right.
                 (for-each (lambda (file)
                             (install-file
                              (string-append support-files "/" file)
                              target))
                           '("ltxguide.cls" "ltnews.cls" "minimal.cls" "idx.tex"
                             "lablst.tex" "testpage.tex" "ltxcheck.tex"))
                 ;; Install configurations
                 (copy-recursively
                  (assoc-ref inputs "texlive-latex-latexconfig")
                  (string-append out "/share/texmf-dist/tex/latex/latexconfig"))
                 (copy-recursively
                  (assoc-ref inputs "texlive-generic-config")
                  (string-append out "/share/texmf-dist/tex/generic/config"))
                 (copy-recursively
                  (assoc-ref inputs "texlive-generic-hyphen")
                  (string-append out "/share/texmf-dist/tex/generic/hyphen"))
                 (copy-recursively
                  (assoc-ref inputs "texlive-generic-ruhyphen")
                  (string-append out "/share/texmf-dist/tex/generic/ruhyphen"))
                 (copy-recursively
                  (assoc-ref inputs "texlive-generic-ukrhyph")
                  (string-append out "/share/texmf-dist/tex/generic/ukrhyph"))
                 #t))))))
      (native-inputs
       `(("texlive-bin" ,texlive-bin)
         ("texlive-generic-unicode-data" ,texlive-generic-unicode-data)
         ("texlive-generic-dehyph-exptl" ,texlive-generic-dehyph-exptl)
         ("texlive-generic-tex-ini-files" ,texlive-generic-tex-ini-files)
         ("texlive-latex-latexconfig"
          ,(texlive-dir "tex/latex/latexconfig/"
                        "1zb3j49cj8p75yph6c8iysjp7qbdvghwf0mn9j0l7qq3qkbz2xaf"))
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
                        "1v90iihy112q93zdpblpdk8zv8rf99fgslsg06s1sxm27zjm9nap"))
         ("texlive-latex-base-support-files"
          ,(origin
             (method svn-fetch)
             (uri (svn-reference
                   (url (string-append "svn://www.tug.org/texlive/tags/"
                                       %texlive-tag "/Master/texmf-dist/"
                                       "/tex/latex/base"))
                   (revision %texlive-revision)))
             (file-name (string-append name "-" version "-checkout"))
             (sha256
              (base32
               "18wy8dlcw8adl6jzqwbg54pdwlhs8hilnfvqbw6ikj6y3zhqkj7q"))))
         ("texlive-tex-plain" ,texlive-tex-plain)
         ("texlive-fonts-cm" ,texlive-fonts-cm)
         ("texlive-fonts-latex" ,texlive-fonts-latex)
         ("texlive-fonts-knuth-lib" ,texlive-fonts-knuth-lib)))
      (propagated-inputs
       `(("texlive-generic-hyph-utf8" ,texlive-generic-hyph-utf8)))
      (home-page "https://www.ctan.org/pkg/latex-base")
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
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0swkbxv8vg0yizadfnvrwjb4cj0pn34v9wm6v7wqq903fdav7k7q"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/filecontents"))
    (home-page "https://www.ctan.org/pkg/filecontents")
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
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0w2xj7n0szavj329kds09q626szkc378p3w0sk022q0ln4ksz86d"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "generic/ifxetex"
       #:tex-format "xelatex"))
    (inputs
     `(("texlive-latex-filecontents" ,texlive-latex-filecontents)))
    (home-page "https://www.ctan.org/pkg/ifxetex")
    (synopsis "Am I running under XeTeX?")
    (description
     "This is a simple package which provides an @code{\\ifxetex} conditional,
so that other code can determine that it is running under XeTeX.  The package
requires the e-TeX extensions to the TeX primitive set.")
    (license license:lppl1.3c+)))

(define-public texlive-generic-epsf
  (package
    (name "texlive-generic-epsf")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/generic/epsf"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "14w3j81ympyvg8hkk9i1xgr8a0gfnfsb2ki8qqsk5pa051za1xcy"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/generic/epfs")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/epsf")
    (synopsis "Simple macros for EPS inclusion")
    (description
     "This package provides the original (and now obsolescent) graphics
inclusion macros for use with dvips, still widely used by Plain TeX users (in
particular).  For LaTeX users, the package is nowadays (rather strongly)
deprecated in favour of the more sophisticated standard LaTeX latex-graphics
bundle of packages.  (The latex-graphics bundle is also available to Plain TeX
users, via its Plain TeX version.)")
    (license license:public-domain)))

(define-public texlive-latex-fancyvrb
  (package
    (name "texlive-latex-fancyvrb")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "fancyvrb"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "03l7140y031rr14h02i4z9zqsfvrbn7wzwxbjsrjcgrk6sdr71wv"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/fancyvrb"
       #:tex-format "latex"))
    (home-page "https://www.ctan.org/pkg/fancyvrb")
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
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0nlfhn55ax89rcvpkrl9570671b62kcr4c9l5ch3w5zw9vmi00dz"))))
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
           (file-name (string-append "graphics-cfg-"
                                     (number->string %texlive-revision)
                                     "-checkout"))
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
           (file-name (string-append "graphics-def-"
                                     (number->string %texlive-revision)
                                     "-checkout"))
           (sha256
            (base32
             "17zpcgrfsr29g1dkz9np1qi63kjv7gb12rg979c6dai6qksbr3vq"))))))
    (home-page "https://www.ctan.org/pkg/latex-graphics")
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
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "01n613s7bcrd2n4jfawm0k4nn2ny3aaifp2jjfif3lz4sbv31494"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/xcolor"))
    (home-page "https://www.ctan.org/pkg/xcolor")
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
    (propagated-inputs
     `(("texlive-latex-oberdiek" ,texlive-latex-oberdiek) ; for ltxcmds.sty
       ("texlive-latex-url" ,texlive-latex-url)))
    (home-page "https://www.ctan.org/pkg/hyperref")
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

(define-public texlive-latex-oberdiek
  (package
    (name "texlive-latex-oberdiek")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "oberdiek"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1m9fg8ddhpsl1212igr9a9fmj012lv780aghjn6fpidg2wqrffmn"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/oberdiek"
       #:build-targets '("oberdiek.ins")
       #:phases
       (modify-phases %standard-phases
         ;; "ifpdf.ins" is not generated, so we need to process the dtx file.
         (add-after 'unpack 'do-not-process-ifpdf.ins
           (lambda _
             (substitute* "oberdiek.ins"
               (("ifpdf.ins") "ifpdf.dtx"))
             #t)))))
    (propagated-inputs
     `(("texlive-generic-ifxetex" ,texlive-generic-ifxetex)))
    (home-page "https://www.ctan.org/pkg/oberdiek")
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
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0vj7h1fgf1396h4qjdc2m07y08i54gbbfrxl8y327cn3r1n093s6"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/tools"
       #:build-targets '("tools.ins")))
    (home-page "https://www.ctan.org/pkg/latex-tools")
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
              (file-name (string-append name "-" version "-checkout"))
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
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0p3fsxap1ilwjz356aq4s5ygwvdscis8bh93g8klf8mhrd8cr2jy"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/l3kernel"))
    (home-page "https://www.ctan.org/pkg/l3kernel")
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
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0pyx0hffiyss363vv7fkrcdiaf7p099xnq0mngzqc7v8v9q849hs"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/l3packages"
       ;; build-targets must be specified manually since they are in
       ;; sub-directories.
       #:build-targets '("l3keys2e.ins" "xparse.ins" "xfrac.ins" "xfp.ins" "xtemplate.ins")
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
                                      cwd "/xtemplate"
                                      ;; The terminating ":" is required to include the
                                      ;; l3kernel input as well.
                                      ":")))
             #t)))
       ))
    (propagated-inputs
     `(("texlive-latex-l3kernel" ,texlive-latex-l3kernel)))
    (home-page "https://www.ctan.org/pkg/l3packages")
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
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1p0mkn6iywl0k4m9cx3hnhylpi499inisff3f72pcf349baqsnvq"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/fontspec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-default-fontspec.cfg
           (lambda* (#:key outputs #:allow-other-keys)
             (with-output-to-file
                 (string-append (assoc-ref outputs "out")
                                "/share/texmf-dist/tex/latex/fontspec/fontspec.cfg")
               (lambda _
                 (display "\
%%% FONTSPEC.CFG %%%
%
% This configuration file sets up TeX Ligatures by default for all fonts loaded
% with `\\setmainfont` and `\\setsansfont`.
%
% In addition, `\\setmonofont` has default features to enforce \"monospace\"
% settings with regard to space stretchability and shrinkability.

\\defaultfontfeatures
 [\\rmfamily,\\sffamily]
 {Ligatures=TeX}

\\defaultfontfeatures
 [\\ttfamily]
 {WordSpace={1,0,0},
  HyphenChar=None,
  PunctuationSpace=WordSpace}
")))
             #t)))))
    (propagated-inputs
     `(("texlive-latex-l3packages" ,texlive-latex-l3packages)))
    (home-page "https://www.ctan.org/pkg/fontspec")
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
              (file-name (string-append name "-" version ".zip"))
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

(define-public texlive-luatex-luaotfload
  (package
    (name "texlive-luatex-luaotfload")
    (version "2.8-fix-2")
    ;; The release tarball does not contain all source files.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lualatex/luaotfload.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0l5l7iq3dxcxl65qaghcpjg27yd9iw1sxa8pnd7xlvlm09dhfdnf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "DESTDIR="
                            (assoc-ref %outputs "out")
                            "/share/texmf-dist")
             "all")
       #:parallel-build? #f ; not supported
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "doc/Makefile"
               (("rst2man") "rst2man.py")
               ;; Don't build the PDF.  This requires more of LaTeX.
               (("\\$\\(DOCPDF\\)") ""))

             (substitute* "Makefile"
               ;; We don't build the PDF, so don't attempt to install it.
               (("cp \\$\\(RESOURCES\\) \\$\\(DOCPDF\\)")
                "cp $(RESOURCES)")
               (("= \\$\\(DOCPDF\\)") "= ")
               ;; Fix name of fontloader file
               (("^LOADER.*= \\$\\(BUILDDIR\\)/fontloader-\\$\\(shell date \\+%F\\).lua")
                "LOADER = $(BUILDDIR)/fontloader.lua"))

             (mkdir "build")

             ;; Don't download this file.
             (copy-file (assoc-ref inputs "glyphlist")
                        "build/glyphlist.txt")

             ;; Don't use git
             (let ((notes
                    `((committer . "Philipp Gesang <phg@phi-gamma.net>")
                      (description . ,version)
                      (loader . "fontloader.lua")
                      (revision . "ad480924393fffa2896156e1a32c22f5c61120dd")
                      (timestamp . "2019-01-01 00:00:00 +0000"))))
               (substitute* "scripts/mkstatus"
                 (("local notes.*=.*")
                  (string-append "local notes = {"
                                 (string-join
                                  (map (lambda (entry)
                                         (format "[\"~a\"]=\"~a\","
                                                 (symbol->string (car entry))
                                                 (cdr entry)))
                                       notes))
                                 "}"))))
             #t)))))
    (native-inputs
     `(("zip" ,zip)
       ("unzip" ,unzip)
       ("graphviz" ,graphviz)
       ("lualatex" ,(texlive-union (list texlive-luatex-lualibs
                                         texlive-context-base)))
       ("python-docutils" ,python-docutils)
       ("glyphlist"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://raw.githubusercontent.com/adobe-type-tools/"
                               "agl-aglfn/b2a04cb906f9257cc06a2fe0ad4b3d663bc02136/"
                               "glyphlist.txt"))
           (sha256
            (base32 "1s6svfw23rqzdvflv8frgd4xrwvrmsj8szwzqgcd39dp9rpjafjp"))))))
    (propagated-inputs
     `(("texlive-luatex-lualibs" ,texlive-luatex-lualibs)))
    (home-page "https://github.com/lualatex/luaotfload")
    (synopsis "OpenType font loader for LuaTeX")
    (description
     "Luaotfload is an adaptation of the ConTeXt font loading system for the
Plain and LaTeX formats.  It allows OpenType fonts to be loaded with font
features accessible using an extended font request syntax while providing
compatibilitywith XeTeX.  By indexing metadata in a database it facilitates
loading fonts by their proper names instead of file names.")
    ;; GPL version 2 only
    (license license:gpl2)))

(define-public texlive-latex-amsmath
  (package
    (name "texlive-latex-amsmath")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "amsmath"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0arvk7gn32mshnfdad5qkgf3i1arxq77k3vq7wnpm4nwnrzclxal"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/amsmath"))
    (home-page "https://www.ctan.org/pkg/amsmath")
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
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0c2j9xh4qpi0x1vvcxdjxq6say0zhyr569fryi5cmhp8bclh4kca"))))
    (build-system texlive-build-system)
    (arguments
     `(#:tex-directory "latex/amscls"))
    (home-page "https://www.ctan.org/pkg/amscls")
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
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0yhlfiz3fjc8jd46f1zrjj4jig48l8rrzh8cmd8ammml8z9a01z6"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "generic/babel"
       #:phases
       (modify-phases %standard-phases
         ;; This package tries to produce babel.aux twice but refuses to
         ;; overwrite the first one.
         (add-before 'build 'fix-ins
           (lambda _
             (substitute* "babel.ins"
               (("askonceonly") "askforoverwritefalse"))
             #t)))))
    (home-page "https://www.ctan.org/pkg/babel")
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

(define-public texlive-generic-babel-english
  (package
    (name "texlive-generic-babel-english")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "generic" "babel-english"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1s404wbx91z5w65hm024kyl4h56zsa096irx18vsx8jvlmwsr5wc"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "generic/babel-english"))
    (home-page "https://www.ctan.org/pkg/babel-english")
    (synopsis "Babel support for English")
    (description
     "This package provides the language definition file for support of
English in @code{babel}.  Care is taken to select British hyphenation patterns
for British English and Australian text, and default (\"american\") patterns
for Canadian and USA text.")
    (license license:lppl1.3+)))

(define-public texlive-generic-babel-german
  (package
    (name "texlive-generic-babel-german")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "generic" "babel-german"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0h47s67gnhdaxfgbf8pirp5vw4z6rrhxl8zav803yjxka0096i3y"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "generic/babel-german"))
    (home-page "https://www.ctan.org/pkg/babel-german")
    (synopsis "Babel support for German")
    (description
     "This package provides the language definition file for support of German
in @code{babel}.  It provides all the necessary macros, definitions and
settings to typeset German documents.  The bundle includes support for the
traditional and reformed German orthography as well as for the Austrian and
Swiss varieties of German.")
    (license license:lppl1.3+)))

(define-public texlive-latex-cyrillic
  (package
    (name "texlive-latex-cyrillic")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "cyrillic"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "083xbwg7hrnlv47fkwvz8yjb830bhxx7y0mq7z7nz2f96y2ldr6b"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/cyrillic"))
    (home-page "https://www.ctan.org/pkg/latex-cyrillic")
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
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1920dcq8613yzprasbg80fh4fcjcidvvl54wkx438nimyxcri7qz"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/psnfss"))
    (home-page "https://www.ctan.org/pkg/psnfss")
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

;; For user profiles
(define-public texlive-base
  (let ((default-packages
          (list texlive-bin
                texlive-dvips
                texlive-fontname
                texlive-fonts-cm
                texlive-fonts-latex
                texlive-metafont-base
                texlive-latex-base
                ;; LaTeX packages from the "required" set.
                texlive-latex-amsmath
                texlive-latex-amscls
                texlive-latex-babel
                texlive-generic-babel-english
                texlive-latex-cyrillic
                texlive-latex-graphics
                texlive-latex-psnfss
                texlive-latex-tools)))
    (package
      (name "texlive-base")
      (version (number->string %texlive-revision))
      (source #f)
      (build-system trivial-build-system)
      (arguments
       '(#:builder
         (begin (mkdir (assoc-ref %outputs "out")))))
      (propagated-inputs
       (map (lambda (package)
              (list (package-name package) package))
            default-packages))
      (home-page (package-home-page texlive-bin))
      (synopsis "TeX Live base packages")
      (description "This is a very limited subset of the TeX Live distribution.
It includes little more than the required set of LaTeX packages.")
      (license (fold (lambda (package result)
                       (match (package-license package)
                         ((lst ...)
                          (append lst result))
                         ((? license:license? license)
                          (cons license result))))
                     '()
                     default-packages)))))

;; For use in package definitions only
(define-public texlive-union
  (lambda* (#:optional (packages '()))
    "Return 'texlive-union' package which is a union of PACKAGES and the
standard LaTeX packages."
    (let ((default-packages (match (package-propagated-inputs texlive-base)
                              (((labels packages) ...) packages))))
      (package (inherit texlive-base)
        (name "texlive-union")
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
                               #:create-all-directories? #t
                               #:log-port (%make-void-port "w"))))

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
                     `("TEXMFCNF" ":" suffix (,(dirname texmf.cnf)))
                     `("TEXMF"    ":" suffix (,(string-append out "/share/texmf-dist"))))
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

;; For use in package definitions only
(define-public texlive-tiny
  (package
    (inherit (texlive-union))
    (name "texlive-tiny")
    (description "This is a very limited subset of the TeX Live distribution.
It includes little more than the required set of LaTeX packages.")))

(define-public texlive-latex-amsrefs
  (package
    (name "texlive-latex-amsrefs")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "amsrefs"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "15i4k479dwrpr0kspmm70g1yn4p3dkh0whyzmr93hph9bggnh1i1"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/amsrefs"))
    (home-page "https://www.ctan.org/pkg/amsrefs")
    (synopsis "LaTeX-based replacement for BibTeX")
    (description
     "Amsrefs is a LaTeX package for bibliographies that provides an archival
data format similar to the format of BibTeX database files, but adapted to
make direct processing by LaTeX easier.  The package can be used either in
conjunction with BibTeX or as a replacement for BibTeX.")
    (license license:lppl1.3+)))

(define-public texlive-latex-bigfoot
  (package
    (name "texlive-latex-bigfoot")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "bigfoot"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "092g8alnsdwlgl1isdnqrr32l161994295kadr1n05d81xgj5wnv"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/bigfoot"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-generated-file
           (lambda _
             (for-each delete-file (find-files "." "\\.drv$"))
             #t)))))
    (home-page "https://www.ctan.org/pkg/bigfoot")
    (synopsis "Footnotes for critical editions")
    (description
     "This package aims to provide a one-stop solution to requirements for
footnotes.  It offers: Multiple footnote apparatus superior to that of
@code{manyfoot}.  Footnotes can be formatted in separate paragraphs, or be run
into a single paragraph (this choice may be selected per footnote series);
Things you might have expected (such as @code{\\verb}-like material in
footnotes, and color selections over page breaks) now work.  Note that the
majority of the bigfoot package's interface is identical to that of
@code{manyfoot}; users should seek information from that package's
documentation.  The bigfoot bundle also provides the @code{perpage} and
@code{suffix} packages.")
    (license license:gpl2+)))

(define-public texlive-latex-blindtext
  (package
    (name "texlive-latex-blindtext")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "blindtext"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1jrja9b1pzdh9zgv1jh807w4xijqja58n2mqny6dkwicv8qfgbfg"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/blindtext"))
    (home-page "https://www.ctan.org/pkg/blindtext")
    (synopsis "Producing 'blind' text for testing")
    (description
     "The package provides the commands @code{\\blindtext} and
@code{\\Blindtext} for creating \"blind\" text useful in testing new classes
and packages, and @code{\\blinddocument}, @code{\\Blinddocument} for creating
an entire random document with sections, lists, mathematics, etc.  The package
supports three languages, @code{english}, @code{(n)german} and @code{latin};
the @code{latin} option provides a short \"lorem ipsum\" (for a fuller \"lorem
ipsum\" text, see the @code{lipsum} package).")
    (license license:lppl)))

(define-public texlive-latex-dinbrief
  (package
    (name "texlive-latex-dinbrief")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "dinbrief"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0lb0kiy8fxzl6cnhcw1sggy6jrjvcd6kj1kkw3k9lkimm388yjz6"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/dinbrief"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-generated-file
           (lambda _
             (delete-file "dinbrief.drv")
             #t))
         (add-after 'unpack 'fix-encoding-error
           (lambda _
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               (substitute* "dinbrief.dtx"
                 (("zur Verf.+ung. In der Pr\"aambel")
                  "zur Verf\"ung. In der Pr\"aambel")))
             #t)))))
    (home-page "https://www.ctan.org/pkg/dinbrief")
    (synopsis "German letter DIN style")
    (description
     "This package implements a document layout for writing letters according
to the rules of DIN (Deutsches Institut für Normung, German standardisation
institute).  A style file for LaTeX 2.09 (with limited support of the
features) is part of the package.  Since the letter layout is based on a
German standard, the user guide is written in German, but most macros have
English names from which the user can recognize what they are used for.  In
addition there are example files showing how letters may be created with the
package.")
    (license license:lppl)))

(define-public texlive-latex-draftwatermark
  (package
    (name "texlive-latex-draftwatermark")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "draftwatermark"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1zyl2pcz2x529gzj5m93a1s4ipymdabf7qdjl3l1673pizd4hfyv"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/draftwatermark"))
    (home-page "https://www.ctan.org/pkg/draftwatermark")
    (synopsis "Put a grey textual watermark on document pages")
    (description
     "This package provides a means to add a textual, light grey watermark on
every page or on the first page of a document.  Typical usage may consist in
writing words such as DRAFT or CONFIDENTIAL across document pages.  The
package performs a similar function to that of @code{draftcopy}, but its
implementation is output device independent, and made very simple by relying
on everypage.")
    (license license:lppl1.3+)))

(define-public texlive-latex-environ
  (package
    (name "texlive-latex-environ")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "environ"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "06h28b26dyjkj9shksphgqfv4130jfkwhbw737hxn7d3yvdfffyd"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/environ"))
    (home-page "https://www.ctan.org/pkg/environ")
    (synopsis "New interface for environments in LaTeX")
    (description
     "This package provides the @code{\\collect@@body} command (as in
@code{amsmath}), as well as a @code{\\long} version @code{\\Collect@@Body},
for collecting the body text of an environment.  These commands are used to
define a new author interface to creating new environments.")
    (license license:lppl)))

(define-public texlive-latex-eqparbox
  (package
    (name "texlive-latex-eqparbox")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "eqparbox"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1ib5xdwcj5wk23wgk41m2hdcjr1dzrs4l3wwnpink9mlapz12wjs"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/eqparbox"))
    (home-page "https://www.ctan.org/pkg/eqparbox")
    (synopsis "Create equal-widthed parboxes")
    (description
     "LaTeX users sometimes need to ensure that two or more blocks of text
occupy the same amount of horizontal space on the page.  To that end, the
@code{eqparbox} package defines a new command, @code{\\eqparbox}, which works
just like @code{\\parbox}, except that instead of specifying a width, one
specifies a tag.  All @code{eqparbox}es with the same tag---regardless of
where they are in the document---will stretch to fit the widest
@code{eqparbox} with that tag.  This simple, equal-width mechanism can be used
for a variety of alignment purposes, as is evidenced by the examples in
@code{eqparbox}'s documentation.  Various derivatives of @code{\\eqparbox} are
also provided.")
    (license license:lppl1.3+)))

(define-public texlive-latex-expdlist
  (package
    (name "texlive-latex-expdlist")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "expdlist"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1x7byk6x10njir3y9rm56glhdzrxwqag7gsnw2sqn1czlq525w7r"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/expdlist"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-generated-file
           (lambda _
             (for-each delete-file
                       (find-files "." "\\.drv$"))
             #t)))))
    (home-page "https://www.ctan.org/pkg/expdlist")
    (synopsis "Expanded description environments")
    (description
     "The package provides additional features for the LaTeX
@code{description} environment, including adjustable left margin.  The package
also allows the user to \"break\" a list (for example, to interpose a comment)
without affecting the structure of the list (this works for @code{itemize} and
@code{enumerate} lists, and numbered lists remain in sequence).")
    (license license:lppl)))

(define-public texlive-latex-filemod
  (package
    (name "texlive-latex-filemod")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/filemod"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0vpxilfw69xv78f03g0j0zw0bw4qcn36whqp8phcq48qk1ax2kr2"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/filemod")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/filemod")
    (synopsis "Provide file modification times, and compare them")
    (description
     "This package provides macros to read and compare the modification dates
of files.  The files may be @code{.tex} files, images or other files (as long
as they can be found by LaTeX).  It uses the @code{\\pdffilemoddate} primitive
of pdfLaTeX to find the file modification date as PDF date string, parses the
string and returns the value to the user.  The package will also work for DVI
output with recent versions of the LaTeX compiler which uses pdfLaTeX in DVI
mode.  The functionality is provided by purely expandable macros or by faster
but non-expandable ones.")
    (license license:lppl1.3+)))

(define-public texlive-latex-ifplatform
  (package
    (name "texlive-latex-ifplatform")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "ifplatform"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "157pplavvm2z97b3jl4x41w11k6q9wgy074mfg0dwmsx5lm328jy"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/ifplatform"))
    (home-page "https://www.ctan.org/pkg/ifplatform")
    (synopsis "Conditionals to test which platform is being used")
    (description
     "This package uses the (La)TeX extension @code{-shell-escape} to
establish whether the document is being processed on a Windows or on a
Unix-like system, or on Cygwin (Unix environment over a Windows system).
Booleans provided are: @code{\\ifwindows}, @code{\\iflinux}, @code{\\ifmacosx}
and @code{\\ifcygwin}.  The package also preserves the output of @code{uname}
on a Unix-like system, which may be used to distinguish between various
classes of systems.")
    (license license:lppl)))

(define-public texlive-latex-natbib
  (package
    (name "texlive-latex-natbib")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "natbib"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0aqliq0nwblxyrzhwhv77pnmk7qh2y3prgq7z7qhwcbgz5kisld7"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/natbib"))
    (home-page "https://www.ctan.org/pkg/natbib")
    (synopsis "Flexible bibliography support")
    (description
     "This bundle provides a package that implements both author-year and
numbered references, as well as much detailed of support for other
bibliography use.  Also provided are versions of the standard BibTeX styles
that are compatible with @code{natbib}: @code{plainnat}, @code{unsrtnat},
@code{abbrnat}.  The bibliography styles produced by @code{custom-bib} are
designed from the start to be compatible with @code{natbib}.")
    (license license:lppl)))

(define-public texlive-latex-psfrag
  (package
    (name "texlive-latex-psfrag")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "psfrag"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1dxbl5il7wbbsp0v45vk884xi1192wxw03849pb1g5q4x808n352"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/psfrag"))
    (home-page "https://www.ctan.org/pkg/psfrag")
    (synopsis "Replace strings in encapsulated PostScript figures")
    (description
     "This package allows LaTeX constructions (equations, picture
environments, etc.) to be precisely superimposed over Encapsulated PostScript
figures, using your own favorite drawing tool to create an EPS figure and
placing simple text \"tags\" where each replacement is to be placed, with
PSfrag automatically removing these tags from the figure and replacing them
with a user specified LaTeX construction, properly aligned, scaled, and/or
rotated.")
    (license (license:fsf-free "file://psfrag.dtx"))))

(define-public texlive-latex-xkeyval
  (package
    (name "texlive-latex-xkeyval")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "xkeyval"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0wancavix39j240pd8m9cgmwsijwx6jd6n54v8wg0x2rk5m44myp"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/xkeyval"))
    (home-page "https://www.ctan.org/pkg/xkeyval")
    (synopsis "Macros for defining and setting keys")
    (description
     "This package is an extension of the @code{keyval} package and offers
more flexible macros for defining and setting keys.  The package provides a
pointer and a preset system.  Furthermore, it supplies macros to allow class
and package options to contain options of the @code{key=value} form.  A LaTeX
kernel patch is provided to avoid premature expansions of macros in class or
package options.  A specialized system for setting @code{PSTricks} keys is
provided by the @code{pst-xkey} package.")
    (license license:lppl1.3+)))

(define-public texlive-latex-pstool
  (package
    (name "texlive-latex-pstool")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/pstool"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1h816jain8c9nky75kk8pmmwj5b4yf9dpqvdvi2l6jhfj5iqkzr8"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/pstool")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (propagated-inputs
     `(("texlive-latex-bigfoot" ,texlive-latex-bigfoot) ; for suffix
       ("texlive-latex-filemod" ,texlive-latex-filemod)
       ("texlive-latex-graphics" ,texlive-latex-graphics)
       ("texlive-latex-ifplatform" ,texlive-latex-ifplatform)
       ("texlive-latex-l3kernel" ,texlive-latex-l3kernel) ; for expl3
       ("texlive-latex-oberdiek" ,texlive-latex-oberdiek)
       ("texlive-latex-psfrag" ,texlive-latex-psfrag)
       ("texlive-latex-tools" ,texlive-latex-tools) ; for shellesc
       ("texlive-latex-trimspaces" ,texlive-latex-trimspaces)
       ("texlive-latex-xkeyval" ,texlive-latex-xkeyval)))
    (home-page "https://www.ctan.org/pkg/pstool")
    (synopsis "Process PostScript graphics within pdfLaTeX documents")
    (description
     "This is a package for processing PostScript graphics with @code{psfrag}
labels within pdfLaTeX documents.  Every graphic is compiled individually,
drastically speeding up compilation time when only a single figure needs
re-processing.")
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
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0y4i651b75y6006n03x8n86bsqvjsailvvz9bhzy51dzsznqidq0"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/seminar")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/seminar")
    (synopsis "Make overhead slides")
    ;; TODO: This package may need fancybox and xcomment at runtime.
    (description
     "This package provides a class that produces overhead
slides (transparencies), with many facilities.  Seminar is not nowadays
reckoned a good basis for a presentation — users are advised to use more
recent classes such as powerdot or beamer, both of which are tuned to
21st-century presentation styles.")
    (license license:lppl1.2+)))

(define-public texlive-latex-trimspaces
  (package
    (name "texlive-latex-trimspaces")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "trimspaces"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0da00lb32am4g63mn96625wg48p3pj3spx79lajrk17d549apwqa"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/trimspaces"
       #:tex-format "latex"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-bug
           (lambda _
             ;; The "ins" file refers to the wrong source file.
             (substitute* "trimspaces.ins"
               (("pstool.tex") "trimspaces.tex"))
             #t)))))
    (inputs
     `(("texlive-latex-filecontents" ,texlive-latex-filecontents)))
    (home-page "https://www.ctan.org/pkg/trimspaces")
    (synopsis "Trim spaces around an argument or within a macro")
    (description
     "This very short package allows you to expandably remove spaces around a
token list (commands are provided to remove spaces before, spaces after, or
both); or to remove surrounding spaces within a macro definition, or to define
space-stripped macros.")
    (license license:lppl)))

(define-public texlive-latex-capt-of
  (package
    (name "texlive-latex-capt-of")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/capt-of"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1y2s50f6lz0jx2748lj3iy56hrpcczgnbzmvphxv7aqndyyamd4x"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/capt-of")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/capt-of")
    (synopsis "Captions on more than floats")
    (description
     "This package defines a command @code{\\captionof} for putting a caption
to something that's not a float.")
    (license license:lppl)))

(define-public texlive-latex-doi
  (package
    (name "texlive-latex-doi")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/doi"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0378rdmrgr2lzbfi4qqy4dfpj5im20diyd8z8b9m4mlg05r7wgnb"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/doi")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/doi")
    (synopsis "Create correct hyperlinks for DOI numbers")
    (description
     "You can hyperlink DOI numbers to doi.org.  However, some publishers have
elected to use nasty characters in their DOI numbering scheme (@code{<},
@code{>}, @code{_} and @code{;} have all been spotted).  This will either
upset LaTeX, or your PDF reader.  This package contains a single user-level
command @code{\\doi{}}, which takes a DOI number, and creates a correct
hyperlink to the target of the DOI.")
    ;; Any version of the LPPL.
    (license license:lppl1.3+)))

(define-public texlive-latex-etoolbox
  (package
    (name "texlive-latex-etoolbox")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/etoolbox"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1agmq6bf8wzcd77n20ng8bl4kh69cg5f6sjniii7bcw4llhd3nc8"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/etoolbox")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/etoolbox")
    (synopsis "e-TeX tools for LaTeX")
    (description
     "This package is a toolbox of programming facilities geared primarily
towards LaTeX class and package authors.  It provides LaTeX frontends to some
of the new primitives provided by e-TeX as well as some generic tools which
are not strictly related to e-TeX but match the profile of this package.  The
package provides functions that seem to offer alternative ways of implementing
some LaTeX kernel commands; nevertheless, the package will not modify any part
of the LaTeX kernel.")
    (license license:lppl1.3+)))

(define-public texlive-latex-fncychap
  (package
    (name "texlive-latex-fncychap")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/fncychap"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0fdk84dbicfjfprkz6vk15x36mvlhaw9isjmgkc56jp2khwjswwq"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/fncychap")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/fncychap")
    (synopsis "Seven predefined chapter heading styles")
    (description
     "This package provides seven predefined chapter heading styles.  Each
style can be modified using a set of simple commands.  Optionally one can
modify the formatting routines in order to create additional chapter
headings.")
    (license license:lppl1.3+)))

(define-public texlive-latex-framed
  (package
    (name "texlive-latex-framed")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/framed"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "14a4ydqsvp3vcfavl21jrv0ybiqypaaqzg2q2cs3rzkandg7w98x"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/framed")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/framed")
    (synopsis "Framed or shaded regions that can break across pages")
    (description
     "The package creates three environments: @code{framed}, which puts an
ordinary frame box around the region, @code{shaded}, which shades the region,
and @code{leftbar}, which places a line at the left side.  The environments
allow a break at their start (the @code{\\FrameCommand} enables creation of a
title that is “attached” to the environment); breaks are also allowed in the
course of the framed/shaded matter.  There is also a command
@code{\\MakeFramed} to make your own framed-style environments.")
    ;; The header states: "These macros may be freely transmitted, reproduced,
    ;; or modified for any purpose provided that this notice is left intact."
    (license (license:fsf-free "file://framed.sty"))))

(define-public texlive-latex-g-brief
  (package
    (name "texlive-latex-g-brief")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "g-brief"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0sikazkg0dpkcpzlbqw8qzxr81paf2f443vsrh14jnw7s4gswvc5"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/g-brief"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-generated-file
           (lambda _
             (delete-file "g-brief.drv")
             #t)))))
    (home-page "https://www.ctan.org/pkg/g-brief")
    (synopsis "Letter document class")
    (description
     "This package is designed for formatting formless letters in German; it
can also be used for English (by those who can read the documentation).  There
are LaTeX 2.09 @code{documentstyle} and LaTeX 2e class files for both an
\"old\" and a \"new\" version of g-brief.")
    (license license:lppl)))

(define-public texlive-latex-galois
  (package
    (name "texlive-latex-galois")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "galois"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0d4l0msk8j5pi95xnmm9wygv1vbpkwkv5amx9l0km86cs79jpp1h"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/galois"))
    (home-page "https://www.ctan.org/pkg/galois")
    (synopsis "Typeset Galois connections")
    (description
     "The package deals with connections in two-dimensional style, optionally
in colour.")
    (license license:lppl)))

(define-public texlive-latex-gcite
  (package
    (name "texlive-latex-gcite")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "gcite"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "03g9by54yrypn599y98r1xh7qw0bbbmpzq0bfwpj6j5q5rkl1mfa"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/gcite"))
    (home-page "https://www.ctan.org/pkg/gcite")
    (synopsis "Citations in a reader-friendly style")
    (description
     "The package allows citations in the German style, which is considered by
many to be particularly reader-friendly.  The citation provides a small amount
of bibliographic information in a footnote on the page where each citation is
made.  It combines a desire to eliminate unnecessary page-turning with the
look-up efficiency afforded by numeric citations.  The package makes use of
BibLaTeX, and is considered experimental.")
    (license license:lppl1.3+)))

(define-public texlive-latex-geometry
  (package
    (name "texlive-latex-geometry")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "geometry"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0yw6bjfgsli3s1dldsgb7mkr7lnk329cgdjbgs8z2xn59pmmdsn4"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/geometry"))
    (propagated-inputs
     `(("texlive-latex-oberdiek" ,texlive-latex-oberdiek))) ;for ifpdf
    (home-page "https://www.ctan.org/pkg/geometry")
    (synopsis "Flexible and complete interface to document dimensions")
    (description
     "This package provides an easy and flexible user interface to customize
page layout, implementing auto-centering and auto-balancing mechanisms so that
the users have only to give the least description for the page layout.  The
package knows about all the standard paper sizes, so that the user need not
know what the nominal \"real\" dimensions of the paper are, just its standard
name (such as a4, letter, etc.).  An important feature is the package's
ability to communicate the paper size it's set up to the output.")
    (license license:lppl)))

(define-public texlive-latex-mdwtools
  (package
    (name "texlive-latex-mdwtools")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "mdwtools"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0caxs74hla28hc67csf5i5ahadx97w8vxh3mdmsprxbpd1mr7ssg"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/mdwtools"))
    (home-page "https://www.ctan.org/pkg/mdwtools")
    (synopsis "Miscellaneous tools by Mark Wooding")
    (description
     "This collection of tools includes: @code{atsupport} for short commands
starting with @code{@@}, macros to sanitize the OT1 encoding of the
@code{cmtt} fonts; a @code{doafter} command; improved @code{footnote} support;
@code{mathenv} for various alignment in maths; list handling; @code{mdwmath}
which adds some minor changes to LaTeX maths; a rewrite of LaTeX's tabular and
array environments; verbatim handling; and syntax diagrams.")
    (license license:gpl3+)))

(define-public texlive-latex-polyglossia
  (package
    (name "texlive-latex-polyglossia")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "polyglossia"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "03ma58z3ypsbp7zgkzb1ylpn2ygr27cxzkf042ns0rif4g8s491f"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/polyglossia"))
    (home-page "https://www.ctan.org/pkg/polyglossia")
    (synopsis "Alternative to babel for XeLaTeX and LuaLaTeX")
    (description
     "This package provides a complete Babel replacement for users of LuaLaTeX
and XeLaTeX; it relies on the @code{fontspec} package, version 2.0 at least.")
    (license license:lppl1.3+)))

(define-public texlive-latex-supertabular
  (package
    (name "texlive-latex-supertabular")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "supertabular"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "14b2bc7cqz4ckxxycim9sw6jkrr1pahivm1rdbpz5k6hl967w1s3"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/supertabular"))
    (home-page "https://www.ctan.org/pkg/supertabular")
    (synopsis "Multi-page tables package")
    (description
     "This package was a predecessor of @code{longtable}; the newer
package (designed on quite different principles) is easier to use and more
flexible, in many cases, but supertabular retains its usefulness in a few
situations where longtable has problems.")
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
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "06cf821y1j7jdg93pb41ayigrfwgn0y49d7w1025zlijjxi6bvjp"))))
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
    (home-page "https://www.ctan.org/pkg/texinfo")
    (synopsis "TeX macros to handle Texinfo files")
    (description
     "Texinfo is the preferred format for documentation in the GNU project;
the format may be used to produce online or printed output from a single
source.  The Texinfo macros may be used to produce printable output using TeX;
other programs in the distribution offer online interactive use (with
hypertext linkages in some cases).")
    (license license:gpl3+)))

(define-public texlive-latex-upquote
  (package
    (name "texlive-latex-upquote")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "upquote"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0d1050i973wnxigy0xpky5l7vn4ff7ldhkjpdqsw5s653gagwixp"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/upquote"))
    (home-page "https://www.ctan.org/pkg/upquote")
    (synopsis "Show \"realistic\" quotes in verbatim")
    (description
     "Typewriter-style fonts are best for program listings, but Computer
Modern Typewriter prints @code{`} and @code{'} as bent opening and closing
single quotes.  Other fonts, and most programming languages, print @code{`} as
a grave accent and @code{'} upright; @code{'} is used both to open and to
close quoted strings.  The package switches the typewriter font to Computer
Modern Typewriter in OT1 encoding, and modifies the behaviour of
@code{verbatim}, @code{verbatim*}, @code{\\verb}, and @code{\\verb*} to print
in the expected way.  It does this regardless of other fonts or encodings in
use, so long as the package is loaded after the other fonts were.  The package
does not affect @code{\\tt}, @code{\\texttt}, etc.")
    (license license:lppl1.2+)))

(define-public texlive-latex-anysize
  (package
    (name "texlive-latex-anysize")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/anysize"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "19khwqjlvznc955sijhww3c4zbb0053rvzwv9nz738qknq7y18vb"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/anysize")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/anysize")
    (synopsis "Simple package to set up document margins")
    (description
     "This is a simple package to set up document margins.  This package is
considered obsolete; alternatives are the @code{typearea} package from the
@code{koma-script} bundle, or the @code{geometry} package.")
    (license license:public-domain)))

(define-public texlive-latex-appendix
  (package
    (name "texlive-latex-appendix")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "appendix"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0rxfpr8vq3brwx5rc7qn91ixlp9zva4zrms8a579fqa1g5yva7vg"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/appendix"))
    (home-page "https://www.ctan.org/pkg/appendix")
    (synopsis "Extra control of appendices")
    (description
     "The appendix package provides various ways of formatting the titles of
appendices.  Also (sub)appendices environments are provided that can be used,
for example, for per chapter/section appendices.  An @code{appendices}
environment is provided which can be used instead of the @code{\\appendix}
command.")
    (license license:lppl)))

(define-public texlive-latex-changebar
  (package
    (name "texlive-latex-changebar")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "changebar"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "05x15ilynqrl448h8l6qiraygamdldlngz89a2bw7kg74fym14ch"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/changebar"))
    (home-page "https://www.ctan.org/pkg/changebar")
    (synopsis "Generate changebars in LaTeX documents")
    (description
     "Identify areas of text to be marked with changebars with the
@code{\\cbstart} and @code{\\cbend} commands; the bars may be coloured.  The
package uses 'drivers' to place the bars; the available drivers can work with
@code{dvitoln03}, @code{dvitops}, @code{dvips}, the emTeX and TeXtures DVI
drivers, and VTeX and pdfTeX.")
    (license license:lppl)))

(define-public texlive-latex-cmap
  (package
    (name "texlive-latex-cmap")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/cmap"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1s1rv6zgw105w2j6ffhnk914qrix87y1ndzri1q72g2kbr91zlbg"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/cmap")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.tug.org/svn/texlive/tags/texlive-2017.1/\
Master/texmf-dist/tex/latex/cmap/")
    (synopsis "CMap support for PDF files")
    (description
     "This package embeds CMap tables into PDF files to make search and
copy-and-paste functions work properly.")
    (license license:lppl)))

(define-public texlive-latex-colortbl
  (package
    (name "texlive-latex-colortbl")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "colortbl"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "190pmq8la2rq07xry8bn8z8yywzxv6fqyqaj7yjfj5rgw6x0mas8"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/colortbl"))
    (home-page "https://www.ctan.org/pkg/colortbl")
    (synopsis "Add colour to LaTeX tables")
    (description
     "This package allows rows, columns, and even individual cells in LaTeX
tables to be coloured.")
    (license license:lppl)))

(define-public texlive-latex-fancybox
  (package
    (name "texlive-latex-fancybox")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/fancybox"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0smmnaad2q8qwicay1frri990lv65l0k8cwzsvdsyp3jk8kp042w"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/fancybox")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/fancybox")
    (synopsis "Variants of \\fbox and other games with boxes")
    (description
     "This package provides variants of @code{\\fbox}: @code{\\shadowbox},
@code{\\doublebox}, @code{\\ovalbox}, @code{\\Ovalbox}, with helpful tools for
using box macros and flexible verbatim macros.  You can box mathematics,
floats, center, flushleft, and flushright, lists, and pages.")
    (license license:lppl1.2+)))

(define-public texlive-latex-fancyhdr
  (package
    (name "texlive-latex-fancyhdr")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/fancyhdr"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1xsnzx7vgdfh9zh2m7bjz6bgdpxsgb1kyc19p50vhs34x5nbgsnr"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/fancyhdr")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/fancyhdr")
    (synopsis "Extensive control of page headers and footers in LaTeX2e")
    (description
     "The package provides extensive facilities, both for constructing headers
and footers, and for controlling their use (for example, at times when LaTeX
would automatically change the heading style in use).")
    (license license:lppl)))

(define-public texlive-latex-float
  (package
    (name "texlive-latex-float")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "float"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0nbl7wylkv22fcdv4p8byhhj575fli6jnqjpkhrkbv8dzwah84nq"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/float"))
    (home-page "https://www.ctan.org/pkg/float")
    (synopsis "Improved interface for floating objects")
    (description
     "This package improves the interface for defining floating objects such
as figures and tables.  It introduces the boxed float, the ruled float and the
plaintop float.  You can define your own floats and improve the behaviour of
the old ones.  The package also provides the @code{H} float modifier option of
the obsolete @code{here} package.  You can select this as automatic default
with @code{\\floatplacement{figure}{H}}.")
    (license license:lppl)))

(define-public texlive-latex-footmisc
  (package
    (name "texlive-latex-footmisc")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "footmisc"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "03x61wwql8nh6zrqiiiq3rb0x7m3pn48c606zapy19y21fybwdxs"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/footmisc"))
    (home-page "https://www.ctan.org/pkg/footmisc")
    (synopsis "Range of footnote options")
    (description
     "This is a collection of ways to change the typesetting of footnotes.
The package provides means of changing the layout of the footnotes themselves,
a way to number footnotes per page, to make footnotes disappear in a
\"moving\" argument, and to deal with multiple references to footnotes from
the same place.  The package also has a range of techniques for labelling
footnotes with symbols rather than numbers.")
    (license license:lppl1.3+)))

(define-public texlive-latex-listings
  (package
    (name "texlive-latex-listings")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "listings"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1nsn9wp3wl12b36c0sqrim33lf33cr5wky0h4ncnw8lvqgm7h8wf"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/listings"
       #:build-targets '("listings.ins")))
    (home-page "https://www.ctan.org/pkg/listings")
    (synopsis "Typeset source code listings using LaTeX")
    (description
     "The package enables the user to typeset programs (programming code)
within LaTeX; the source code is read directly by TeX---no front-end processor
is needed.  Keywords, comments and strings can be typeset using different
styles.  Support for @code{hyperref} is provided.")
    (license license:lppl1.3+)))

(define-public texlive-latex-jknapltx
  (package
    (name "texlive-latex-jknapltx")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/jknapltx"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0m034x72f2g07icr50gacyxfb9g1lz2rmqh4kqr1qjb421x2kds9"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/jknapltx")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/jknappen")
    (synopsis "Miscellaneous packages by Joerg Knappen")
    (description
     "This package provides miscellaneous macros by Joerg Knappen, including:
represent counters in greek; Maxwell's non-commutative division;
@code{latin1jk}, @code{latin2jk} and @code{latin3jk}, which are
@code{inputenc} definition files that allow verbatim input in the respective
ISO Latin codes; blackboard bold fonts in maths; use of RSFS fonts in maths;
extra alignments for @code{\\parboxes}; swap Roman and Sans fonts;
transliterate semitic languages; patches to make (La)TeX formulae embeddable
in SGML; use maths minus in text as appropriate; simple Young tableaux.")
    (license license:gpl2)))

(define-public texlive-fonts-ec
  (package
    (name "texlive-fonts-ec")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/fonts/source/jknappen/ec/"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "12av65fbz9xiashm09c9m1fj1mijxls5xspd7652ry1n5s0nixy4"))))
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
             (for-each (lambda (font)
                         (format #t "building font ~a\n" font)
                         (invoke "mf" "-progname=mf"
                                 "-output-directory=build"
                                 (string-append "\\"
                                                "mode:=ljfour; "
                                                "mag:=1; "
                                                "batchmode; "
                                                "input " (basename font ".mf"))))
                       (find-files "." "[0-9]+\\.mf$"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (tfm (string-append
                          out "/share/texmf-dist/fonts/tfm/jknappen/ec"))
                    (mf  (string-append
                          out "/share/texmf-dist/fonts/source/jknappen/ec")))
               (for-each (cut install-file <> tfm)
                         (find-files "build" "\\.*"))
               (for-each (cut install-file <> mf)
                         (find-files "." "\\.mf"))
               #t))))))
    (native-inputs
     `(("texlive-bin" ,texlive-bin)
       ("texlive-metafont-base" ,texlive-metafont-base)
       ("texlive-fonts-cm" ,texlive-fonts-cm)))
    (home-page "https://www.ctan.org/pkg/ec")
    (synopsis "Computer modern fonts in T1 and TS1 encodings")
    (description
     "The EC fonts are European Computer Modern Fonts, supporting the complete
LaTeX T1 encoding defined at the 1990 TUG conference hold at Cork/Ireland.
These fonts are intended to be stable with no changes being made to the tfm
files.  The set also contains a Text Companion Symbol font, called @code{tc},
featuring many useful characters needed in text typesetting, for example
oldstyle digits, currency symbols (including the newly created Euro symbol),
the permille sign, copyright, trade mark and servicemark as well as a copyleft
sign, and many others.  Recent releases of LaTeX2e support the EC fonts.  The
EC fonts supersede the preliminary version released as the DC fonts.  The
fonts are available in (traced) Adobe Type 1 format, as part of the
@code{cm-super} bundle.  The other Computer Modern-style T1-encoded Type 1
set, Latin Modern, is not actually a direct development of the EC set, and
differs from the EC in a number of particulars.")
    (license (license:fsf-free "https://www.tug.org/svn/texlive/tags/\
texlive-2017.1/Master/texmf-dist/doc/fonts/ec/copyrite.txt"))))

(define-public texlive-fonts-adobe-times
  (package
    (name "texlive-fonts-adobe-times")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/fonts/type1/urw/times/"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "15vzyr7favkv1mj00qxr03s89kw78nd066fh69by93272g8p5sgd"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (ice-9 match))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match))
         (let ((root (string-append (assoc-ref %outputs "out")
                                    "/share/texmf-dist/"))
               (pkgs '(("source"        . "fonts/type1/urw/times")

                       ("times-afm"     . "fonts/afm/adobe/times")
                       ("times-tfm"     . "fonts/tfm/adobe/times")
                       ("times-vf"      . "fonts/vf/adobe/times")

                       ("urw-afm"       . "fonts/afm/urw/times")
                       ("urw35vf-tfm"   . "fonts/tfm/urw35vf/times")
                       ("urw35vf-vf"    . "fonts/vf/urw35vf/times")

                       ("times-tex"     . "tex/latex/times")
                       ("dvips"         . "dvips/times")
                       ("fonts-map"     . "fonts/map/dvips/times"))))
           (for-each (match-lambda
                       ((pkg . dir)
                        (let ((target (string-append root dir)))
                          (mkdir-p target)
                          (copy-recursively (assoc-ref %build-inputs pkg)
                                            target))))
                     pkgs)
           #t))))
    (native-inputs
     `(("times-afm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/afm/adobe/times"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-afm-" version "-checkout"))
           (sha256
            (base32
             "1k7h6vihfc6ri2lq9ggnq2g4zq3qcgq1vd0hr486g9cqrdpys6cy"))))
       ("times-tfm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/tfm/adobe/times"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-tfm-" version "-checkout"))
           (sha256
            (base32
             "1hbgkjnf5xyganbznwpwszvr3iyk4bzb0ys4hd8ybawp60paadrr"))))
       ("times-vf"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/vf/adobe/times"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-vf-" version "-checkout"))
           (sha256
            (base32
             "18rfspnwdw9r81dy18lb4w96d09b6c4g7y80azwylalkhwdf2lfp"))))
       ("urw-afm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/afm/urw/times"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-urw-afm-" version "-checkout"))
           (sha256
            (base32
             "0g0xpsyn6634g0b4rpd420v7i4gkz3zr12vcy2b8csbcscjvwri5"))))
       ("urw35vf-tfm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/tfm/urw35vf/times"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-urw35vf-tfm-" version "-checkout"))
           (sha256
            (base32
             "0a4idlvpaqd0ypqgy1xw0rpx8q23bvssg8xq757zzn3zikj0w7pr"))))
       ("urw35vf-vf"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/vf/urw35vf/times"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-urw35vf-vf-" version "-checkout"))
           (sha256
            (base32
             "05mppwxd4c5x0yw50gca726f0ylc1rk8jf0jjkrriixq6rnw03di"))))
       ("times-tex"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/tex/latex/times"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-tex-" version "-checkout"))
           (sha256
            (base32
             "1gmd0x7c3vkvfzgmrsp4866rcdbyimfk3bjr91zaadc41r1i8xrp"))))
       ("dvips"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/dvips/times/"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-dvips-" version "-checkout"))
           (sha256
            (base32
             "1fvqpgqi7bp2q76nf5kmlhsdijxw65arqfy3ax3djwih3yg12mp0"))))
       ("fonts-map"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/map/dvips/times/"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-fonts-map-" version "-checkout"))
           (sha256
            (base32
             "12f00gzs2zgllkm59qdhw2xxj7lvg3p256232f1l275z3pldfqqi"))))))
    (home-page "https://ctan.org/pkg/urw-base35")
    (synopsis "URW Base 35 font pack for LaTeX")
    (description
     "This package provides a drop-in replacements for the Times font from
Adobe's basic set.")
    ;; No license version specified.
    (license license:gpl3+)))

(define-public texlive-fonts-adobe-palatino
  (package
    (name "texlive-fonts-adobe-palatino")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/fonts/type1/urw/palatino/"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "18dw5260c6fy7acxaqwrg3hw04kg63ijq4lkn56q5pa2g6nyylrp"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (ice-9 match))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match))
         (let ((root (string-append (assoc-ref %outputs "out")
                                    "/share/texmf-dist/"))
               (pkgs '(("source"        . "fonts/type1/urw/palatino")

                       ("palatino-afm"  . "fonts/afm/adobe/palatino")
                       ("palatino-tfm"  . "fonts/tfm/adobe/palatino")
                       ("palatino-vf"   . "fonts/vf/adobe/palatino")

                       ("urw-afm"       . "fonts/afm/urw/palatino")
                       ("urw35vf-tfm"   . "fonts/tfm/urw35vf/palatino")
                       ("urw35vf-vf"    . "fonts/vf/urw35vf/palatino")

                       ("palatino-tex"  . "tex/latex/palatino")
                       ("dvips"         . "dvips/palatino")
                       ("fonts-map"     . "fonts/map/dvips/palatino"))))
           (for-each (match-lambda
                       ((pkg . dir)
                        (let ((target (string-append root dir)))
                          (mkdir-p target)
                          (copy-recursively (assoc-ref %build-inputs pkg)
                                            target))))
                     pkgs)
           #t))))
    (native-inputs
     `(("palatino-afm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/afm/adobe/palatino"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-afm-" version "-checkout"))
           (sha256
            (base32
             "0pxizay730cx7rb9y5bqq9dn1zxx3arc33rmdsn7l29pc51flmmi"))))
       ("palatino-tfm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/tfm/adobe/palatino"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-tfm-" version "-checkout"))
           (sha256
            (base32
             "1w1vm0sk9kpsy14yhyf1v1q3c6b97cgbba74g578bcwjlh810mg0"))))
       ("palatino-vf"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/vf/adobe/palatino"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-vf-" version "-checkout"))
           (sha256
            (base32
             "1maqfis8hpybcn9lmm8r2b1g56620lfpsncg0742c3kkjd6dh97h"))))
       ("urw-afm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/afm/urw/palatino"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-urw-afm-" version "-checkout"))
           (sha256
            (base32
             "0gk0xwy1fs2si5kb1j3dzgm52c8sagv32gd9dmw88m7sgh5qkd87"))))
       ("urw35vf-tfm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/tfm/urw35vf/palatino"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-urw35vf-tfm-" version "-checkout"))
           (sha256
            (base32
             "19aq3xwfg7vkf1qzjdxgcvcdqwpvpavq3l25y64xni72qx0kmppz"))))
       ("urw35vf-vf"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/vf/urw35vf/palatino"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-urw35vf-vf-" version "-checkout"))
           (sha256
            (base32
             "1lkn4p6zimrs0ah6mxsang4bicp8j7xzl016529a3f168an7mdmj"))))
       ("palatino-tex"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/tex/latex/palatino"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-tex-" version "-checkout"))
           (sha256
            (base32
             "0ng9w7i0p1nb51amla32jj86vx6p84m6qc7asam3g4x8w5jf7s27"))))
       ("dvips"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/dvips/palatino/"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-dvips-" version "-checkout"))
           (sha256
            (base32
             "1pdbkfmhx4kk3brh5lg6fyl9ad2kbjmkrhgcx84klnlhq01mfdhb"))))
       ("fonts-map"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/map/dvips/palatino/"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-fonts-map-" version "-checkout"))
           (sha256
            (base32
             "0rg13hyp652hp3gnrj5pbyb84zkqmyi1qnm8c6spcyaq8pm06l0d"))))))
    (home-page "https://ctan.org/pkg/urw-base35")
    (synopsis "URW Base 35 font pack for LaTeX")
    (description
     "This package provides a drop-in replacements for the Palatino font from
Adobe's basic set.")
    ;; No license version specified.
    (license license:gpl3+)))

(define-public texlive-fonts-adobe-zapfding
  (package
    (name "texlive-fonts-adobe-zapfding")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/fonts/type1/urw/zapfding/"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1sp3jblg3khp0yj121blvhph6ib09919kyrsk5x2lg258yypqyis"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (ice-9 match))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match))
         (let ((root (string-append (assoc-ref %outputs "out")
                                    "/share/texmf-dist/"))
               (pkgs '(("source"      . "fonts/type1/urw/zapfding")
                       ("zapf-afm"    . "fonts/afm/adobe/zapfding")
                       ("zapf-tfm"    . "fonts/tfm/adobe/zapfding")
                       ("urw-afm"     . "fonts/afm/urw/zapfding")
                       ("urw35vf-tfm" . "fonts/tfm/urw35vf/zapfding")

                       ("zapf-tex"    . "tex/latex/zapfding")
                       ("dvips"       . "dvips/zapfding")
                       ("fonts-map"   . "fonts/map/dvips/zapfding"))))
           (for-each (match-lambda
                       ((pkg . dir)
                        (let ((target (string-append root dir)))
                          (mkdir-p target)
                          (copy-recursively (assoc-ref %build-inputs pkg)
                                            target))))
                     pkgs)
           #t))))
    (native-inputs
     `(("zapf-afm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/afm/adobe/zapfding"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-afm-" version "-checkout"))
           (sha256
            (base32
             "0qvl4w1bfcpiakkd8rvkism46qnvzj9w7x4r8z9m0y7mspbkblyr"))))
       ("zapf-tfm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/tfm/adobe/zapfding"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-tfm-" version "-checkout"))
           (sha256
            (base32
             "1i8mh9xsl8l4cgsg3nl4ha9q6m55j122riclaxsvkc5ka83432qm"))))
       ("urw-afm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/afm/urw/zapfding"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-urw-afm-" version "-checkout"))
           (sha256
            (base32
             "0m4qndqh7ji723ff82c5c1q8ziqvblbaip7vx05vnl15fqbsnfx1"))))
       ("urw35vf-tfm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/tfm/urw35vf/zapfding"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-urw35vf-tfm-" version "-checkout"))
           (sha256
            (base32
             "167g2x6mpjfqh0w1fhjbw14qcx6ridrj2zm1bd8bi0l2d7phj28m"))))
       ("zapf-tex"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/tex/latex/zapfding"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-tex-" version "-checkout"))
           (sha256
            (base32
             "0hp7i8f6nbrg7irrwc8fd7n1hrzjysa84d6iyivwlc65v9p7lmd0"))))
       ("dvips"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/dvips/zapfding/"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-dvips-" version "-checkout"))
           (sha256
            (base32
             "1f18sc4qwxykd786zhn6szcrycqvpvfhlcim71aamxmwghakd7fa"))))
       ("fonts-map"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/map/dvips/zapfding/"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-fonts-map-" version "-checkout"))
           (sha256
            (base32
             "17kwxmdrgz2fb072hx57a3pidcrhbgayphx11zyld2hv9149pkyl"))))))
    (home-page "https://ctan.org/pkg/urw-base35")
    (synopsis "URW Base 35 font pack for LaTeX")
    (description
     "This package provides a drop-in replacements for the Zapfding font from
Adobe's basic set.")
    ;; No license version specified.
    (license license:gpl3+)))

(define-public texlive-fonts-rsfs
  (package
    (name "texlive-fonts-rsfs")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/fonts/source/public/rsfs/"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0r12pn02r4a955prcvq0048nifh86ihlcgvw3pppqqvfngv34l5h"))))
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
             (for-each (lambda (font)
                         (format #t "building font ~a\n" font)
                         (invoke "mf" "-progname=mf"
                                 "-output-directory=build"
                                 (string-append "\\"
                                                "mode:=ljfour; "
                                                "mag:=1; "
                                                "batchmode; "
                                                "input " (basename font ".mf"))))
                       (find-files "." "[0-9]+\\.mf$"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (tfm (string-append
                          out "/share/texmf-dist/fonts/tfm/public/rsfs"))
                    (mf  (string-append
                          out "/share/texmf-dist/fonts/source/public/rsfs")))
               (for-each (cut install-file <> tfm)
                         (find-files "build" "\\.*"))
               (for-each (cut install-file <> mf)
                         (find-files "." "\\.mf"))
               #t))))))
    (native-inputs
     `(("texlive-bin" ,texlive-bin)
       ("texlive-metafont-base" ,texlive-metafont-base)
       ("texlive-fonts-cm" ,texlive-fonts-cm)))
    (home-page "https://www.ctan.org/pkg/rsfs")
    (synopsis "Ralph Smith's Formal Script font")
    (description
     "The fonts provide uppercase formal script letters for use as symbols in
scientific and mathematical typesetting (in contrast to the informal script
fonts such as that used for the calligraphic symbols in the TeX maths symbol
font).  The fonts are provided as Metafont source, and as derived Adobe Type 1
format.  LaTeX support, for using these fonts in mathematics, is available via
one of the packages @code{calrsfs} and @code{mathrsfs}.")
    (license (license:fsf-free "http://mirrors.ctan.org/fonts/rsfs/README"))))

(define-public texlive-latex-eso-pic
  (package
    (name "texlive-latex-eso-pic")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "eso-pic"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1xvmms28mvvfpks9x7lfya2xhh5k8jy3qnlih1mzcnf156xnb89z"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/eso-pic"))
    (home-page "https://www.ctan.org/pkg/eso-pic")
    (synopsis "Add picture commands (or backgrounds) to every page")
    (description
     "The package adds one or more user commands to LaTeX's @code{shipout}
routine, which may be used to place the output at fixed positions.  The
@code{grid} option may be used to find the correct places.")
    (license license:lppl1.3+)))

(define-public texlive-latex-eepic
  (package
    (name "texlive-latex-eepic")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/eepic"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1c68gvh021pvybg07apsd2xhq2ljbg80kq94wh71drdga3c2zqjw"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/eepic")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/eepic")
    (synopsis "Extensions to epic and the LaTeX drawing tools")
    (description
     "Extensions to @code{epic} and the LaTeX picture drawing environment,
include the drawing of lines at any slope, the drawing of circles in any
radii, and the drawing of dotted and dashed lines much faster with much less
TeX memory, and providing several new commands for drawing ellipses, arcs,
splines, and filled circles and ellipses.  The package uses @code{tpic}
@code{\\special} commands.")
    (license license:public-domain)))

(define-public texlive-latex-enumitem
  (package
    (name "texlive-latex-enumitem")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/enumitem"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0q24b1bkdi9l6bw787bpggww83jh2vj8955aw2m5yccqbx4vgr5r"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/enumitem")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/enumitem")
    (synopsis "Customize basic list environments")
    (description
     "This package is intended to ease customizing the three basic list
environments: @code{enumerate}, @code{itemize} and @code{description}.  It
extends their syntax to allow an optional argument where a set of parameters
in the form @code{key=value} are available, for example:
@code{\\begin{itemize}[itemsep=1ex,leftmargin=1cm]}.")
    (license license:lppl1.3+)))

(define-public texlive-latex-multirow
  (package
    (name "texlive-latex-multirow")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "multirow"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0qlxy47f1f8plgch3jqfsnrdgpyz20sz46yp33i2jwvf9hvfczf0"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/multirow"))
    (home-page "https://www.ctan.org/pkg/multirow")
    (synopsis "Create tabular cells spanning multiple rows")
    (description
     "The package provides tools for creating tabular cells spanning multiple
rows.  It has a lot of flexibility, including an option for specifying an
entry at the \"natural\" width of its text.")
    (license license:lppl1.3+)))

(define-public texlive-latex-overpic
  (package
    (name "texlive-latex-overpic")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/overpic"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1rpx4ibjncj5416rg19v0xjbj3z9avhfdfn2gzp8r8sz9vz25c6g"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/overpic")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/overpic")
    (synopsis "Combine LaTeX commands over included graphics")
    (description
     "The @code{overpic} environment is a cross between the LaTeX
@code{picture} environment and the @code{\\includegraphics} command of
@code{graphicx}.  The resulting picture environment has the same dimensions as
the included graphic.  LaTeX commands can be placed on the graphic at defined
positions; a grid for orientation is available.")
    (license license:lppl1.0+)))

(define-public texlive-latex-parskip
  (package
    (name "texlive-latex-parskip")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/parskip"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "14r6h9hqb0qgccxj5l1208694fx8sb8avmgzps36lsbbpszl7i7m"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/parskip")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/parskip")
    (synopsis "Layout with zero \\parindent, non-zero \\parskip")
    (description
     "Simply changing @code{\\parskip} and @code{\\parindent} leaves a layout
that is untidy; this package (though it is no substitute for a properly
designed class) helps alleviate this untidiness.")
    (license license:lppl)))

(define-public texlive-latex-pdfpages
  (package
    (name "texlive-latex-pdfpages")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "pdfpages"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0s4izcah7im67889qz4d26pcfpasmm35sj1rw4ragkkdk3rlbbbd"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/pdfpages"))
    (home-page "https://www.ctan.org/pkg/pdfpages")
    (synopsis "Include PDF documents in LaTeX")
    (description
     "This package simplifies the inclusion of external multi-page PDF
documents in LaTeX documents.  Pages may be freely selected and it is possible
to put several logical pages onto each sheet of paper.  Furthermore a lot of
hypertext features like hyperlinks and article threads are provided.  The
package supports pdfTeX (pdfLaTeX) and VTeX.  With VTeX it is even possible to
use this package to insert PostScript files, in addition to PDF files.")
    (license license:lppl1.3+)))

(define-public texlive-fonts-stmaryrd
  (package
    (name "texlive-fonts-stmaryrd")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "fonts" "stmaryrd"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "08pn4ca3vl6qm9l3wm5h5iyjsrg411kkm1yana329xwg2j14s9n6"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/stmaryrd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-ins
           (lambda _
             (substitute* "stmaryrd.ins"
               (("^%% LaTeX2e.*") "\\input docstrip\n")
               (("fontdef\\}\\}" line)
                (string-append line "\n\\endbatchfile")))
             #t)))))
    (home-page "https://www.ctan.org/pkg/stmaryrd")
    (synopsis "St Mary Road symbols for theoretical computer science")
    (description
     "The fonts were originally distributed as Metafont sources only, but
Adobe Type 1 versions are also now available.  Macro support is provided for
use under LaTeX; the package supports the @code{only} option (provided by the
@code{somedefs} package) to restrict what is loaded, for those who don't need
the whole font.")
    (license license:lppl)))

(define-public texlive-latex-subfigure
  (package
    (name "texlive-latex-subfigure")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "subfigure"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "15spcl5wb7w269qd6y596vp4yi8sa5ppcx8w4z2i9kyp02r3a0yb"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/subfigure"))
    (home-page "https://www.ctan.org/pkg/subfigure")
    (synopsis "Figures divided into subfigures")
    (description
     "This (deprecated) package provides support for the manipulation and
reference of small or \"sub\" figures and tables within a single figure or
table environment.  It is convenient to use this package when your subfigures
are to be separately captioned, referenced, or are to be included in the
List-of-Figures.  A new @code{\\subfigure} command is introduced which can be
used inside a figure environment for each subfigure.  An optional first
argument is used as the caption for that subfigure.  The package is now
considered obsolete: it was superseded by @code{subfig}, but users may find
the more recent @code{subcaption} package more satisfactory.")
    (license license:lppl)))

(define-public texlive-latex-tabulary
  (package
    (name "texlive-latex-tabulary")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "tabulary"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1adkdx2zkk42g82nqf57lv1nc1z7kwl13jmy8vpcsizsa0xdnx9n"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/tabulary"))
    (home-page "https://www.ctan.org/pkg/tabulary")
    (synopsis "Tabular with variable width columns balanced")
    (description
     "The package defines a @code{tabular*}-like environment, @code{tabulary},
taking a \"total width\" argument as well as the column specifications.  The
environment uses column types @code{L}, @code{C}, @code{R} and @code{J} for
variable width columns (@code{\\raggedright}, @code{\\centering},
@code{\\raggedleft}, and normally justified).  In contrast to
@code{tabularx}'s @code{X} columns, the width of each column is weighted
according to the natural width of the widest cell in the column.")
    (license license:lppl)))

(define-public texlive-latex-threeparttable
  (package
    (name "texlive-latex-threeparttable")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/threeparttable"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "10vy9k150w2lviw8h22s2mcykff38xci653m5823s2vv44pwbmzq"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/threeparttable")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/threeparttable")
    (synopsis "Tables with captions and notes all the same width")
    (description
     "This package facilitates tables with titles (captions) and notes.  The
title and notes are given a width equal to the body of the table (a
@code{tabular} environment).  By itself, a @code{threeparttable} does not
float, but you can put it in a @code{table} or a @code{table*} or some other
environment.")
    (license (license:fsf-free "file://threeparttable.sty"))))

(define-public texlive-fonts-txfonts
  (package
    (name "texlive-fonts-txfonts")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/txfonts"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0jl921qdphg8i7bkfprackn3xd4gmvxckc526nmzqsmahqkavgg2"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (ice-9 match))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match))
         (let ((root (string-append (assoc-ref %outputs "out")
                                    "/share/texmf-dist/"))
               (pkgs '(("source"        . "tex/latex/txfonts")
                       ("txfonts-vf"    . "fonts/tfm/public/txfonts")
                       ("txfonts-afm"   . "fonts/afm/public/txfonts")
                       ("txfonts-tfm"   . "fonts/tfm/public/txfonts")
                       ("txfonts-type1" . "fonts/type1/public/txfonts")
                       ("txfonts-enc"   . "fonts/enc/dvips/txfonts")
                       ("txfonts-map"   . "fonts/map/dvips/txfonts"))))
           (for-each (match-lambda
                       ((pkg . dir)
                        (let ((target (string-append root dir)))
                          (mkdir-p target)
                          (copy-recursively (assoc-ref %build-inputs pkg)
                                            target))))
                     pkgs)
           #t))))
    (native-inputs
     `(("txfonts-tfm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/tfm/public/txfonts"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-tfm-" version "-checkout"))
           (sha256
            (base32
             "12ffmbrp48ap35qa3b4mi6ckif9q2vf7972jxh5dc1yzykhla2xv"))))
       ("txfonts-vf"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/vf/public/txfonts"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-vf-" version "-checkout"))
           (sha256
            (base32
             "04acyfdwvxpfx4l2xh2bpzdmpvwdf2pzbs7a236b0xckz2jvc1ci"))))
       ("txfonts-afm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/afm/public/txfonts"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-afm-" version "-checkout"))
           (sha256
            (base32
             "1705klz51pnqzcs89s3521b84b6c89wlczflsh0vci66nl155yis"))))
       ("txfonts-type1"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/type1/public/txfonts"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-type1-" version "-checkout"))
           (sha256
            (base32
             "0ajwr7zb6ch3gxd0g8p2i4llhy2wr9a9saz6jq6hm6fxf4pgl5h3"))))
       ("txfonts-map"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/map/dvips/txfonts"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-map-" version "-checkout"))
           (sha256
            (base32
             "0kamr8a9x24jakas3v09dgv7kkpybj3i7qv4vz1iyypqr6kk1raj"))))
       ("txfonts-enc"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/enc/dvips/txfonts"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-enc-" version "-checkout"))
           (sha256
            (base32
             "1bal5fhw0xlhl37ayv8vlnqnsn1y82kadzfjhbgr223blspp4zsj"))))))
    (home-page "https://www.ctan.org/pkg/threeparttable")
    (synopsis "Times-like fonts in support of mathematics")
    (description
     "Txfonts supplies virtual text roman fonts using Adobe Times (or URW
NimbusRomNo9L) with some modified and additional text symbols in the OT1, T1,
and TS1 encodings; maths alphabets using Times/URW Nimbus; maths fonts
providing all the symbols of the Computer Modern and AMS fonts, including all
the Greek capital letters from CMR; and additional maths fonts of various
other symbols.

The set is complemented by a sans-serif set of text fonts, based on
Helvetica/NimbusSanL, and a monospace set.

All the fonts are in Type 1 format (AFM and PFB files), and are supported by
TeX metrics (VF and TFM files) and macros for use with LaTeX.")
    ;; Any version of the GPL with font exception.
    (license license:gpl3+)))

(define-public texlive-fonts-iwona
  (package
    (name "texlive-fonts-iwona")
    (version "0.995b")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://jmn.pl/pliki/Iwona-tex-"
                                  (string-map (lambda (c)
                                                (if (char=? c #\.)
                                                    #\_ c))
                                              version)
                                  ".zip"))
              (sha256
               (base32
                "13684iqx5granpc5rfvqnmyvdpgpbr1x9y7i7y7bcaq0qxv7ph1x"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/"))
               (unzip  (string-append (assoc-ref %build-inputs "unzip")
                                      "/bin/unzip")))
           (invoke unzip (assoc-ref %build-inputs "source"))
           (mkdir-p target)
           (copy-recursively "iwona" target)
           #t))))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://jmn.pl/en/kurier-i-iwona/")
    (synopsis "Sans-serif typeface for TeX")
    (description "Iwona is a two-element sans-serif typeface. It was created
as an alternative version of the Kurier typeface, which was designed in 1975
for a diploma in typeface design at the Warsaw Academy of Fine Arts under the
supervision of Roman Tomaszewski.  Kurier was designed for linotype
typesetting of newspapers and similar periodicals. The Iwona fonts are an
alternative version of the Kurier fonts.  The difference lies in the absence
of ink traps which typify the Kurier font.")
    (license license:gfl1.0)))

(define-public texlive-latex-titlesec
  (package
    (name "texlive-latex-titlesec")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/titlesec"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "04nmkhqx6jxcxx9a30zbcd5smxi5fd0cbp132bki7fnvhspnhg21"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/titlesec")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/titlesec")
    (synopsis "Select alternative section titles")
    (description
     "This package provides an interface to sectioning commands for selection
from various title styles, e.g. for marginal titles and to change the font of
all headings with a single command, also providing simple one-step page
styles.  It also includes a package to change the page styles when there are
floats in a page.  You may assign headers/footers to individual floats, too.")
    (license license:lppl)))

(define-public texlive-latex-type1cm
  (package
    (name "texlive-latex-type1cm")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "type1cm"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1lvxrqfwcwa4p31zyfm80gr05v8c28xybv5ri79zi2ngz6834z12"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/type1cm"))
    (home-page "https://www.ctan.org/pkg/type1cm")
    (synopsis "Arbitrary size font selection in LaTeX")
    (description
     "LaTeX, by default, restricts the sizes at which you can use its default
computer modern fonts, to a fixed set of discrete sizes (effectively, a set
specified by Knuth).  The @code{type1cm} package removes this restriction;
this is particularly useful when using scalable versions of the CM
fonts (Bakoma, or the versions from BSR/Y&Y, or True Type versions from Kinch,
PCTeX, etc.).  In fact, since modern distributions will automatically generate
any bitmap font you might need, @code{type1cm} has wider application than just
those using scalable versions of the fonts.  Note that the LaTeX distribution
now contains a package @code{fix-cm},f which performs the task of
@code{type1cm}, as well as doing the same job for T1- and TS1-encoded
@code{ec} fonts.")
    (license license:lppl)))

(define-public texlive-latex-lh
  (package
    (name "texlive-latex-lh")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "lh"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "00gdiwh3sfhh1iimjhpja7lm7k4vzqzql2irgwnpz94qvh25zwi5"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/lh"))
    (home-page "https://www.ctan.org/pkg/lh")
    (synopsis "Cyrillic fonts that support LaTeX standard encodings")
    (description
     "The LH fonts address the problem of the wide variety of alphabets that
are written with Cyrillic-style characters.  The fonts are the original basis
of the set of T2* and X2 encodings that are now used when LaTeX users need to
write in Cyrillic languages.  Macro support in standard LaTeX encodings is
offered through the latex-cyrillic and t2 bundles, and the package itself
offers support for other (more traditional) encodings.  The fonts, in the
standard T2* and X2 encodings are available in Adobe Type 1 format, in the
CM-Super family of fonts.  The package also offers its own LaTeX support for
OT2 encoded fonts, CM bright shaped fonts and Concrete shaped fonts.")
    (license license:lppl)))

(define-public texlive-metapost
  (package
    (name "texlive-metapost")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/metapost"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0sf18pc6chgy26p9bxxn44xcqhzjrfb53jxjr2y7l3jb6xllhblq"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/metapost")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/metapost")
    (synopsis "Create scalable illustrations")
    (description
     "MetaPost uses a language based on that of Metafont to produce precise
technical illustrations.  Its output is scalable PostScript or SVG, rather
than the bitmaps Metafont creates.")
    (license license:lppl)))

(define-public texlive-latex-acmart
  (package
    (name "texlive-latex-acmart")
    (version "1.45")
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "acmart"))
              (sha256
               (base32
                "10zs8ga88ksypv1v4p6mynmfa7749q2hgxlr4shnwfjd9wrb421q"))
              (file-name (string-append name "-" version "-checkout"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/acmart"))
    (home-page "https://www.ctan.org/pkg/acmart")
    (synopsis "Class for typesetting publications of ACM")
    (description
     "This package provides a class for typesetting publications of the
Association for Computing Machinery (ACM).")
    (license license:lppl1.3+)))

(define-public texlive-latex-varwidth
  (package
    (name "texlive-latex-varwidth")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/varwidth"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1bmz9ap0ffyg7qry2xi7lki06qx4809w028xvk88cl66h7p46g52"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/varwidth")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/varwidth")
    (synopsis "Variable-width minipage")
    (description
     "The @code{varwidth} environment is superficially similar to
@code{minipage}, but the specified width is just a maximum value — the box may
get a narrower “natural” width.")
    (license license:lppl)))

(define-public texlive-latex-wasysym
  (package
    (name "texlive-latex-wasysym")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "wasysym"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1sgwbfwjjf70g54hh93gsd9jp9nm67w6n74x9d72a56n07jbk5hv"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/wasysym"))
    (home-page "https://www.ctan.org/pkg/wasysym")
    (synopsis "LaTeX support file to use the WASY2 fonts")
    (description
     "The wasy2WASY2 (Waldi Symbol) font by Roland Waldi provides many glyphs
like male and female symbols and astronomical symbols, as well as the complete
lasy font set and other odds and ends.  The wasysym package implements an easy
to use interface for these symbols.")
    (license license:lppl)))

(define-public texlive-latex-wrapfig
  (package
    (name "texlive-latex-wrapfig")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/wrapfig"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "16xpyl0csmmwndz1xhzqfg9l0zcsnqxslsixsqkwd4zsvfj30sv4"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/wrapfig")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/wrapfig")
    (synopsis "Produces figures which text can flow around")
    (description
     "This package allows figures or tables to have text wrapped around them.
It does not work in combination with list environments, but can be used in a
@code{parbox} or @code{minipage}, and in two-column format.")
    (license license:lppl)))

(define-public texlive-latex-ucs
  (package
    (name "texlive-latex-ucs")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/ucs"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0rrxwi60wmz5dfjifl4fwk66plf7wix85qnhfv4ylvmj6qi6hw37"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/ucs")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/ucs")
    (synopsis "Extended UTF-8 input encoding support for LaTeX")
    (description
     "The bundle provides the @code{ucs} package, and @code{utf8x.def},
together with a large number of support files.  The @code{utf8x.def}
definition file for use with @code{inputenc} covers a wider range of Unicode
characters than does @code{utf8.def} in the LaTeX distribution.  The package
provides facilities for efficient use of its large sets of Unicode characters.
Glyph production may be controlled by various options, which permits use of
non-ASCII characters when coding mathematical formulae.  Note that the bundle
previously had an alias “unicode”; that alias has now been withdrawn, and no
package of that name now exists.")
    (license license:lppl1.3+)))

(define-public texlive-latex-preview
  (package
    (name "texlive-latex-preview")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "preview"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1hpsk4yp08qvbl43kqiv0hhwxv3gcqqxcpahyv6ch2b38pbj4bh6"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/preview"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-generated-file
           (lambda _
             (delete-file "preview.drv")
             #t)))))
    (home-page "https://www.ctan.org/pkg/preview")
    (synopsis "Extract bits of a LaTeX source for output")
    (description
     "The main purpose of the preview package is the extraction of selected
elements from a LaTeX source, like formulas or graphics, into separate
pages of a DVI file.  A flexible and convenient interface allows it to
specify what commands and constructs should be extracted.  This works
with DVI files postprocessed by either Dvips and Ghostscript or
dvipng, but it also works when you are using PDFTeX for generating PDF
files.")
    (license license:gpl3+)))

(define-public texlive-latex-acronym
  (package
    (name "texlive-latex-acronym")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "acronym"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0jmasg40bk53zdd2jc8nc18jvdai3p2wmamy7hwli8gls4nf25qp"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/acronym"))
    (home-page "https://www.ctan.org/pkg/acronym")
    (synopsis "Expand acronyms at least once")
    (description
     "This package ensures that all acronyms used in the text are spelled out
in full at least once.  It also provides an environment to build a list of
acronyms used.  The package is compatible with PDF bookmarks.  The package
requires the suffix package, which in turn requires that it runs under
e-TeX.")
    (license license:lppl1.3+)))

(define-public texlive-generic-pdftex
  (package
    (name "texlive-generic-pdftex")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/generic/pdftex"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0k68zmqzs4qvrqxdwsrawbjb14hxqjfamq649azvai0jjxdpkljd"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/generic/pdftex"))
               (target-map (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/fonts/map/pdftex")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           (mkdir-p target-map)
           (copy-recursively (assoc-ref %build-inputs "pdftex-map") target-map)
           #t))))
    (native-inputs
     `(("pdftex-map"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/map/pdftex"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-map-" version "-checkout"))
           (sha256
            (base32
             "18jvcm0vwpg6wwzijvnb92xx78la45kkh71k6l44425krp2vnwm0"))))))
    (home-page "https://www.ctan.org/pkg/pdftex")
    (synopsis "TeX extension for direct creation of PDF")
    (description
     "This package provides an extension of TeX which can be configured to
directly generate PDF documents instead of DVI.")
    (license license:gpl2+)))

(define texlive-texmf
  (package
   (name "texlive-texmf")
   (version "20180414")
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
          (add-after 'unpack 'unset-environment-variables
            (lambda _
              (unsetenv "TEXMF")
              (unsetenv "TEXMFCNF")
              #t))
          (add-after 'patch-source-shebangs 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((share (string-append (assoc-ref outputs "out") "/share")))
                (mkdir-p share)
                (invoke "mv" "texmf-dist" share))))
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
                (invoke "updmap-sys" "--nohash" "--syncwithtrees")
                (invoke "mktexlsr")
                (invoke "fmtutil-sys" "--all")))))))
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
   (version "20180414")
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
                     '("texmf-dist" "texmf-var"))))
               #t))))
   (synopsis "TeX Live, a package of the TeX typesetting system")
   (description
    "TeX Live provides a comprehensive TeX document production system.
It includes all the major TeX-related programs, macro packages, and fonts
that are free software, including support for many languages around the
world.

This package contains the complete TeX Live distribution.")
   (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))
   (home-page "https://www.tug.org/texlive/")))

(define-public perl-text-bibtex
  (package
    (name "perl-text-bibtex")
    (version "0.85")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AM/AMBS/Text-BibTeX-"
                           version ".tar.gz"))
       (sha256
        (base32
         "036kxgbn1jf70pfm2lmjlzjwnhbkd888fp5lyvmkjpdd15gla18h"))))
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
    (home-page "https://metacpan.org/release/Text-BibTeX")
    (synopsis "Interface to read and parse BibTeX files")
    (description "@code{Text::BibTeX} is a Perl library for reading, parsing,
and processing BibTeX files.  @code{Text::BibTeX} gives you access to the data
at many different levels: you may work with BibTeX entries as simple field to
string mappings, or get at the original form of the data as a list of simple
values (strings, macros, or numbers) pasted together.")
    (license license:perl-license)))

(define-public biber
  (package
    (name "biber")
    (version "2.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/plk/biber/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              ;; TODO: Patch awaiting inclusion upstream (see:
              ;; https://github.com/plk/biber/issues/239).
              (patches (search-patches "biber-fix-encoding-write.patch"))
              (sha256
               (base32
                "1g1hi6zvf2hmrjly1sidjaxy5440gfqm4p7p3n7kayshnjsmlskx"))))
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
       ("perl-file-slurper" ,perl-file-slurper)
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
    (version "5.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.xm1math.net/texmaker/texmaker-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0y81mjm89b99pr9svcwpaf4iz2q9pc9hjas5kiwd1pbgl5vqskm9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Qt has its own configuration utility.
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "qmake"
                       (string-append "PREFIX=" out)
                       (string-append "DESKTOPDIR=" out "/share/applications")
                       (string-append "ICONDIR=" out "/share/pixmaps")
                       (string-append "METAINFODIR=" out "/share/metainfo")
                       "texmaker.pro")))))))
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
              (method url-fetch/tarbomb)
              (uri (string-append "mirror://gnu/" name "/" name "-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0h56w22d99dh4fgld4ssik8ggnmhmrrbnrn1lnxi1zr0miphn1sd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are none
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-packaging-error
           (lambda* (#:key inputs #:allow-other-keys)
             ;; This file should have been part of the tarball.
             (install-file (car
                            (find-files
                             (assoc-ref inputs "automake")
                             "^install-sh$"))
                           ".")
             ;; Remove generated file.
             (delete-file "book.pdf")
             #t)))))
    (native-inputs
     `(("texlive" ,(texlive-union (list texlive-latex-amsfonts
                                        texlive-fonts-amsfonts
                                        texlive-fonts-adobe-palatino
                                        texlive-fonts-adobe-zapfding
                                        texlive-fonts-knuth-lib
                                        texlive-fonts-mflogo-font
                                        texlive-generic-pdftex)))
       ("automake" ,automake)))
    (home-page "https://www.gnu.org/software/teximpatient/")
    (synopsis "Book on TeX, plain TeX and Eplain")
    (description "@i{TeX for the Impatient} is a ~350 page book on TeX,
plain TeX, and Eplain, originally written by Paul Abrahams, Kathryn Hargreaves,
and Karl Berry.")
    (license license:fdl1.3+)))

(define-public lyx
  (package
    (name "lyx")
    (version "2.3.2-2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.lyx.org/pub/lyx/stable/"
                                  (version-major+minor version) ".x/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0vr0qwis6rhind6azfa270hqxci7rj8qb1kk5x6lm80mc34nvrqi"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "3rdparty")
                  #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags `("-DLYX_USE_QT=QT5"
                           "-DLYX_EXTERNAL_BOOST=1"
                           "-DLYX_INSTALL=1"
                           "-DLYX_RELEASE=1"
                           ,(string-append "-DLYX_INSTALL_PREFIX="
                                           (assoc-ref %outputs "out")
                                           ;; Exact name and level is necessary.
                                           "/lyx" ,(version-major+minor version)))
       #:phases
       (modify-phases %standard-phases
         ;; See ;; https://www.lyx.org/trac/changeset/3a123b90af838b08680471d87170c38e56787df9/lyxgit
         (add-after 'unpack 'fix-compilation-with-boost-1.69
           (lambda _
             (substitute* "src/support/FileName.cpp"
               (("^template struct boost::detail::crc_table_t.*") ""))
             #t))
         (add-after 'unpack 'patch-python
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("src/support/os.cpp")
               (("\"python ")
                (string-append "\""
                               (assoc-ref inputs "python")
                               "/bin/python ")))
             #t))
         (add-after 'patch-python 'patch-desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "lib/lyx.desktop.in"
               (("Exec=")
                (string-append "Exec="
                               (assoc-ref outputs "out")
                               "/")))
             #t))
         (add-before 'check 'setenv-check
           (lambda _
             ;; Create missing file that would cause tests to fail.
             (with-output-to-file (string-append "../lyx-"
                                                 ,version
                                                 "/src/tests/check_layout.cmake")
               (const #t))
             (setenv (string-append "LYX_DIR_"
                                    (string-join
                                      (string-split
                                        ,(version-major+minor version) #\-)) "x")
                     (string-append (getcwd) "/../lyx-" ,version "/lib"))
             #t))
         (add-after 'install 'install-symlinks
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/bin"))
               (symlink (string-append "../lyx" ,(version-major+minor version)
                                       "/bin/lyx" ,(version-major+minor version))
                        (string-append out "/bin/lyx" ,(version-major+minor version)))
               #t))))))
    (inputs
     `(("boost" ,boost)
       ("hunspell" ,hunspell)           ; Note: Could also use aspell instead.
       ("libx11" ,libx11)
       ("mythes" ,mythes)
       ("python" ,python-2)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("texlive" ,(texlive-union (list texlive-fonts-ec)))))
    ;; FIXME: Python 3.7.0 cannot be used because the test infrastructure
    ;; "requires a bytes-like object, not 'str'".  This may be fixed with
    ;; upgrades to Python.
    (native-inputs
     `(("python" ,python-2)
       ("pkg-config" ,pkg-config)
       ("bc" ,bc)))
    (home-page "https://www.lyx.org/")
    (synopsis "Document preparation system with GUI")
    (description "LyX is a document preparation system.  It excels at letting
you create complex technical and scientific articles with mathematics,
cross-references, bibliographies, indexes, etc.  It is very good for working
with documents of any length in which the usual processing abilities are
required: automatic sectioning and pagination, spell checking and so forth.")
    (license license:gpl2+)))

(define-public texlive-latex-media9
  (package
    (name "texlive-latex-media9")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/media9"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0lhb2h5hxjq9alpk4r3gvg21pwyifs4ah6hqzp92k55mkp1xv73j"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/media9")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/media9")
    (synopsis "Multimedia inclusion package with Adobe Reader-9/X compatibility")
    (description
     "The package provides an interface to embed interactive Flash (SWF) and 3D
objects (Adobe U3D & PRC), as well as video and sound files or streams in the
popular MP4, FLV and MP3 formats into PDF documents with Acrobat-9/X
compatibility.  Playback of multimedia files uses the built-in Flash Player of
Adobe Reader and does, therefore, not depend on external plug-ins.  Flash Player
supports the efficient H.264 codec for video compression.

The package is based on the RichMedia Annotation, an Adobe addition to the PDF
specification.  It replaces the now obsolete @code{movie15} package.")
    (license license:lppl)))

(define-public texlive-latex-ocgx2
  (package
    (name "texlive-latex-ocgx2")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/ocgx2"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0zp00jg058djx8xp0xqwas92y3j97clkyd8z6pqr890yqy06myqb"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/ogcx2")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/ocgx2")
    (synopsis "Provide OCG (Optional Content Groups) support within a PDF document")
    (description
     "This package provides OCG (Optional Content Groups) support within a PDF
document.

It re-implements the functionality of the @code{ocg}, @code{ocgx}, and
@code{ocg-p} packages and adds support for all known engines and back-ends
including:

@itemize
@item LaTeX → dvips → @code{ps2pdf}/Distiller
@item (Xe)LaTeX(x) → @code{dvipdfmx}
@item pdfLaTeX and LuaLaTeX .
@end itemize

It also ensures compatibility with the @code{media9} and @code{animate} packages.")
    (license license:lppl)))

(define-public texlive-latex-ms
  (package
    (name "texlive-latex-ms")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "ms"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0m4wx3yjb5al1qsv995z8fii8xxy96mcfihbnlx43lpgayiwz35s"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/ms"
       #:tex-format "latex"))
    (home-page "https://ctan.org/pkg/ms")
    (synopsis "Various LATEX packages by Martin Schröder")
    (description
     "A bundle of LATEX packages by Martin Schröder; the collection comprises:

@itemize
@item @command{count1to}, make use of fixed TEX counters;
@item @command{everysel}, set commands to execute every time a font is selected;
@item @command{everyshi}, set commands to execute whenever a page is shipped out;
@item @command{multitoc}, typeset the table of contents in multiple columns;
@item @command{prelim2e}, mark typeset pages as preliminary; and
@item @command{ragged2e}, typeset ragged text and allow hyphenation.
@end itemize\n")
    (license license:lppl1.3c+)))

(define-public texlive-latex-needspace
  (package
    (name "texlive-latex-needspace")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "needspace"))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0kw80f5jh4gdpa2ka815abza3gr5z8b929w0745vrlc59pl0017y"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/needspace"
       #:tex-format "latex"))
    (inputs
     `(("texlive-latex-filecontents" ,texlive-latex-filecontents)))
    (home-page "https://www.ctan.org/pkg/needspace")
    (synopsis "Insert pagebreak if not enough space")
    (description
     "Provides commands to disable pagebreaking within a given vertical
space.  If there is not enough space between the command and the bottom of the
page, a new page will be started.")
    (license license:lppl)))

(define-public texlive-latex-changepage
  (package
    (name "texlive-latex-changepage")
    (version (number->string %texlive-revision))
    (source
     (origin
       (method svn-fetch)
       (uri (texlive-ref "latex" "changepage"))
       (sha256
        (base32
         "1rpw8xg5p4jsyh236jma9dz3l29wjx4062f154b3wak5yjcxyxyb"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/changepage"
       #:tex-format "latex"))
    (inputs
     `(("texlive-latex-filecontents" ,texlive-latex-filecontents)))
    (home-page "https://www.ctan.org/pkg/changepage")
    (synopsis "Margin adjustment and detection of odd/even pages")
    (description
     "The package provides commands to change the page layout in the middle of
a document, and to robustly check for typesetting on odd or even pages.
Instructions for use are at the end of the file.  The package is an extraction
of code from the @code{memoir} class, whose user interface it shares.")
    (license license:lppl1.3+)))

(define-public texlive-latex-eukdate
  (package
    (name "texlive-latex-eukdate")
    (version (number->string %texlive-revision))
    (source
     (origin
       (method svn-fetch)
       (uri (svn-reference
             (url (string-append "svn://www.tug.org/texlive/tags/"
                                 %texlive-tag "/Master/texmf-dist/"
                                 "/tex/latex/eukdate"))
             (revision %texlive-revision)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "18xan116l8w47v560bkw6nbhkrml7g04xrlzk3jrpc7qsyf3n5fz"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/eukdate")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/eukdate")
    (synopsis "UK format dates, with weekday")
    (description
     "The package is used to change the format of @code{\\today}’s date,
including the weekday, e.g., \"Saturday, 26 June 2008\", the 'UK format', which
is preferred in many parts of the world, as distinct from that which is used in
@code{\\maketitle} of the article class, \"June 26, 2008\", the 'US format'.")
    (license license:lppl)))

(define-public texlive-generic-ulem
  (package
    (name "texlive-generic-ulem")
    (version (number->string %texlive-revision))
    (source
     (origin
       (method svn-fetch)
       (uri (svn-reference
             (url (string-append "svn://www.tug.org/texlive/tags/"
                                 %texlive-tag "/Master/texmf-dist/"
                                 "/tex/generic/ulem"))
             (revision %texlive-revision)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "1rzdniqq9zk39w8ch8ylx3ywh2mj87s4ivchrsk2b8nx06jyn797"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/generic/ulem")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/ulem")
    (synopsis "Underline text in TeX")
    (description
     "The package provides an @code{\\ul} (underline) command which will break
over line ends; this technique may be used to replace @code{\\em} (both in that
form and as the @code{\\emph} command), so as to make output look as if it comes
from a typewriter.  The package also offers double and wavy underlining, and
striking out (line through words) and crossing out (/// over words).")
    (license license:lppl1.3c+)))

(define-public texlive-latex-pgf
  (package
    (name "texlive-latex-pgf")
    (version (number->string %texlive-revision))
    (source
     (origin
       (method svn-fetch)
       (uri (svn-reference
             (url (string-append "svn://www.tug.org/texlive/tags/"
                                 %texlive-tag "/Master/texmf-dist/"
                                 "/tex/latex/pgf"))
             (revision %texlive-revision)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "1dq8p10pz8wn0vx412m7d7d5gj1syxly3yqdqvf7lv2xl8zndn5h"))))
    (build-system trivial-build-system)
    (native-inputs
     `(("texlive-latex-pgf-generic"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
             (url (string-append "svn://www.tug.org/texlive/tags/"
                                 %texlive-tag "/Master/texmf-dist/"
                                 "/tex/generic/pgf"))
             (revision %texlive-revision)))
           (file-name (string-append "texlive-latex-pgf-generic" version "-checkout"))
           (sha256
            (base32
             "0xkxw26sjzr5npjpzpr28yygwdbhzpdd0hsk80gjpidhcxmz393i"))))))
    (propagated-inputs
     `(("texlive-latex-xcolor" ,texlive-latex-xcolor)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target-generic (string-append (assoc-ref %outputs "out")
                                              "/share/texmf-dist/tex/generic/pgf"))
               (target-latex (string-append (assoc-ref %outputs "out")
                                            "/share/texmf-dist/tex/latex/pgf")))
           (mkdir-p target-generic)
           (mkdir-p target-latex)
           (copy-recursively (assoc-ref %build-inputs "texlive-latex-pgf-generic") target-generic)
           (copy-recursively (assoc-ref %build-inputs "source") target-latex)
           #t))))
    (home-page "https://www.ctan.org/pkg/tikz")
    (synopsis "Create PostScript and PDF graphics in TeX")
    (description
     "PGF is a macro package for creating graphics.  It is platform- and
format-independent and works together with the most important TeX backend
drivers, including pdfTeX and dvips.  It comes with a user-friendly syntax layer
called TikZ.

Its usage is similar to pstricks and the standard picture environment.  PGF
works with plain (pdf-)TeX, (pdf-)LaTeX, and ConTeXt.  Unlike pstricks, it can
produce either PostScript or PDF output.")
    (license license:lppl1.3c+)))

(define-public texlive-latex-koma-script
  (package
    (name "texlive-latex-koma-script")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/koma-script"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0nqwf0sr4mf3v9gqa6apv6ml2xhcdwax0vgyf12a672g7rpdyvgm"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (ice-9 match))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match))
         (let ((root (string-append (assoc-ref %outputs "out")
                                    "/share/texmf-dist/"))
               (pkgs '(("source" . "tex/latex/koma-script"))))
           (for-each (match-lambda
                       ((pkg . dir)
                        (let ((target (string-append root dir)))
                          (mkdir-p target)
                          (copy-recursively (assoc-ref %build-inputs pkg)
                                            target))))
                     pkgs)
           #t))))
    (home-page "https://www.ctan.org/pkg/koma-script")
    (synopsis "Bundle of versatile classes and packages")
    (description
     "The KOMA-Script bundle provides replacements for the article, report, and
book classes with emphasis on typography and versatility.  There is also a
letter class.

The bundle also offers:

@itemize
@item a package for calculating type areas in the way laid down by the
typographer Jan Tschichold,
@item packages for easily changing and defining page styles,
@item a package scrdate for getting not only the current date but also the name
of the day, and
@item a package scrtime for getting the current time.
@end itemize

All these packages may be used not only with KOMA-Script classes but also with
the standard classes.

Since every package has its own version number, the version number quoted only
refers to the version of scrbook, scrreprt, scrartcl, scrlttr2 and
typearea (which are the main parts of the bundle).")
    (license license:lppl1.3+)))

(define-public texlive-generic-listofitems
  (package
    (name "texlive-generic-listofitems")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/generic/listofitems"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0hs28fc0v2l92ad9las9b8xcckyrdrwmyhcx1yzmbr6s7s6nvsx8"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/generic/listofitems")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/listofitems")
    (synopsis "Grab items in lists using user-specified seperation character")
    (description
     "This package allows one to capture all the items of a list, for which
the parsing character has been selected by the user, and to access any of
these items with a simple syntax.")
    (license license:lppl1.3c+)))

(define-public texlive-latex-readarray
  (package
    (name "texlive-latex-readarray")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/readarray"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0c53k180ivn1n7fz3ngvd2w1i5dw3kxml0n64vhki88xsylz7lxp"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/readarray")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (propagated-inputs
     `(("texlive-generic-listofitems" ,texlive-generic-listofitems)))
    (home-page "https://www.ctan.org/pkg/readarray")
    (synopsis "Read, store and recall array-formatted data")
    (description
     "This package allows the user to input formatted data into elements of a
2-D or 3-D array and to recall that data at will by individual cell number.
The data can be but need not be numerical in nature.  It can be, for example,
formatted text.")
    (license license:lppl1.3)))

(define-public texlive-latex-verbatimbox
  (package
    (name "texlive-latex-verbatimbox")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/verbatimbox"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0qh1cgvfs463zsi2pjg490gj0mkjfdpfc381j10cbb5la304psna"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/verbatimbox")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (propagated-inputs
     `(("texlive-latex-readarray" ,texlive-latex-readarray)))
    (home-page "https://www.ctan.org/pkg/verbatimbox")
    (synopsis "Deposit verbatim text in a box")
    (description
     "The package provides a @code{verbbox} environment to place its contents
into a globally available box, or into a box specified by the user.  The
global box may then be used in a variety of situations (for example, providing
a replica of the @code{boxedverbatim} environment itself).  A valuable use is
in places where the standard @code{verbatim} environment (which is based on a
@code{trivlist}) may not appear.")
    (license license:lppl1.3+)))

(define-public texlive-latex-examplep
  (package
    (name "texlive-latex-examplep")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/examplep"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0fsvvmz68ij0zwfzrny6x13d92grxr4ap59lxgah4smbkccd6s27"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/examplep")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/examplep")
    (synopsis "Verbatim phrases and listings in LaTeX")
    (description
     "Examplep provides sophisticated features for typesetting verbatim source
code listings, including the display of the source code and its compiled LaTeX
or METAPOST output side-by-side, with automatic width detection and enabled
page breaks (in the source), without the need for specifying the source twice.
Special care is taken that section, page and footnote numbers do not interfere
with the main document.  For typesetting short verbatim phrases, a replacement
for the @code{\\verb} command is also provided in the package, which can be
used inside tables and moving arguments such as footnotes and section
titles.")
    ;; No version of the GPL is specified.
    (license license:gpl3+)))

(define-public texlive-generic-xypic
  (package
    (name "texlive-generic-xypic")
    (version (number->string %texlive-revision))
    (source
     (origin
       (method svn-fetch)
       (uri (svn-reference
             (url (string-append "svn://www.tug.org/texlive/tags/"
                                 %texlive-tag "/Master/texmf-dist/"
                                 "/tex/generic/xypic"))
             (revision %texlive-revision)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "1g5cyxwdfznq4lk9zl6fkjkapmhmwd2cm4m5aibxj20qgwnaggfz"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/generic/xypic")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/xypic")
    (synopsis "Flexible diagramming macros for TeX")
    (description
     "A package for typesetting a variety of graphs and diagrams with TeX.
Xy-pic works with most formats (including LaTeX, AMS-LaTeX, AMS-TeX, and plain
TeX).")
    (license license:gpl3+)))

(define-public texlive-fonts-xypic
  (package
    (name "texlive-fonts-xypic")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/fonts/source/public/xypic"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0p20v1257kwsqnrk98cdhhiz2viv8l3ly4xay4by0an3j37m9xs3"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (ice-9 match))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match))
         (let ((root (string-append (assoc-ref %outputs "out")
                                    "/share/texmf-dist/"))
               (pkgs '(("source" . "fonts/source/public/xypic")
                       ("xypic-afm" . "fonts/afm/public/xypic")
                       ("xypic-type1" . "fonts/type1/public/xypic")
                       ("xypic-enc" . "fonts/enc/dvips/xypic"))))
           (for-each (match-lambda
                       ((pkg . dir)
                        (let ((target (string-append root dir)))
                          (mkdir-p target)
                          (copy-recursively (assoc-ref %build-inputs pkg)
                                            target))))
                     pkgs)
           #t))))
    (native-inputs
     `(("xypic-afm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/afm/public/xypic"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-afm-" version "-checkout"))
           (sha256
            (base32
             "149xdijxp8lw3s0qv2aqxxxyyn748z57dpr596rjvkqdffpnsddh"))))
       ("xypic-type1"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/type1/public/xypic"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-type1-" version "-checkout"))
           (sha256
            (base32
             "1bln89wib7g3hcv2jny3qi6jb73k9d2vbgx3wnnjwp3ryg0846if"))))
       ("xypic-enc"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/enc/dvips/xypic"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-enc-" version "-checkout"))
           (sha256
            (base32
             "0yi8vms3203l3p5slnhrrlzzp0f0jw77fkcvcaicrz2vmw9z99x7"))))))
    (home-page "https://www.ctan.org/pkg/xypic")
    (synopsis "Fonts for XY-pic")
    (description "This package provides the XY-pic fonts.")
    (license license:gpl3+)))

(define-public texlive-bibtex
  (package
    (name "texlive-bibtex")
    (version (number->string %texlive-revision))
    (source
     (origin
       (method svn-fetch)
       (uri (svn-reference
             (url (string-append "svn://www.tug.org/texlive/tags/"
                                 %texlive-tag "/Master/texmf-dist/"
                                 "/bibtex"))
             (revision %texlive-revision)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "0hnbs0s1znbn32hfcsyijl39z81sdb00jf092a4blqz421qs2mbv"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/bibtex")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/bibtex")
    (synopsis "Process bibliographies for LaTeX")
    (description
     "BibTeX allows the user to store his citation data in generic form, while
printing citations in a document in the form specified by a BibTeX style, to
be specified in the document itself (one often needs a LaTeX citation-style
package, such as @command{natbib} as well).")
    (license license:knuth)))

(define-public texlive-fonts-charter
  (package
    (name "texlive-fonts-charter")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/fonts/type1/bitstrea/charter"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0yvib45xxff3jm5270zij4q888pivbc18cqs7lz4pqfhn1am4wnv"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (ice-9 match))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match))
         (let ((root (string-append (assoc-ref %outputs "out")
                                    "/share/texmf-dist/"))
               (pkgs '(("source" . "fonts/type1/bitstrea/charter")
                       ("charter-afm" . "fonts/afm/bitstrea/charter")
                       ("charter-tfm" . "fonts/tfm/bitstrea/charter"))))
           (for-each (match-lambda
                       ((pkg . dir)
                        (let ((target (string-append root dir)))
                          (mkdir-p target)
                          (copy-recursively (assoc-ref %build-inputs pkg)
                                            target))))
                     pkgs)
           #t))))
    (native-inputs
     `(("charter-afm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/afm/bitstrea/charter"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-afm-" version "-checkout"))
           (sha256
            (base32
             "02nbkqrlr3vypnzslmr7dxg1353mmc0rl4ynx0s6qbvf313fq76a"))))
       ("charter-tfm"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/fonts/tfm/bitstrea/charter"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-tfm-" version "-checkout"))
           (sha256
            (base32
             "0j7ci9vprivbhac70aq0z7m23hqcpx1g0i3wp1k0h8ilhimj80xk"))))))
    (home-page "https://www.ctan.org/pkg/charter")
    (synopsis "Charter fonts for TeX")
    (description "A commercial text font donated for the common good.  Support
for use with LaTeX is available in @code{freenfss}, part of
@command{psnfss}. ")
    (license (license:non-copyleft (string-append "http://mirrors.ctan.org/"
                                                  "fonts/charter/readme.charter")))))

(define-public texlive-context-base
  (package
    (name "texlive-context-base")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/context/base"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0rlx4qqijms1n64gjx475kvip8l322fh7v17zkmwp1l1g0w3vlyz"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/context/case")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)
           #t))))
    (home-page "https://www.ctan.org/pkg/context")
    (synopsis "Full featured, parameter driven macro package for TeX")
    (description "A full featured, parameter driven macro package, which fully
supports advanced interactive documents.  See the ConTeXt garden for a wealth
of support information.")
    (license license:gpl2+)))

(define-public texlive-latex-beamer
  (package
    (name "texlive-latex-beamer")
    (version (number->string %texlive-revision))
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/texmf-dist/"
                                        "/tex/latex/beamer"))
                    (revision %texlive-revision)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "09y3qwbj0nckshvg9afgwcv9v3zdif1d7bnpzrggsa1fbr80mgk2"))))
    (build-system trivial-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((target (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/latex/beamer"))
               (docs   (string-append (assoc-ref %outputs "doc")
                                      "/share/texmf-dist/doc/latex/beamer/")))
           (mkdir-p target)
           (copy-recursively (assoc-ref %build-inputs "source") target)

           (mkdir-p docs)
           (copy-recursively (assoc-ref %build-inputs "docs") docs)
           #t))))
    (propagated-inputs
     `(("texlive-latex-hyperref" ,texlive-latex-hyperref)
       ("texlive-latex-oberdiek" ,texlive-latex-oberdiek)
       ("texlive-latex-etoolbox" ,texlive-latex-etoolbox)
       ("texlive-latex-pgf" ,texlive-latex-pgf)))
    (native-inputs
     `(("docs"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/doc/latex/beamer"))
                 (revision %texlive-revision)))
           (file-name (string-append name "-" version "-checkout"))
           (sha256
            (base32
             "102b18b9nw9dicqqgjwx0srh1mav8vh9wdvwayn741niza9hac23"))))))
    (home-page "https://www.ctan.org/pkg/beamer")
    (synopsis "LaTeX class for producing presentations and slides")
    (description "The beamer LaTeX class can be used for producing slides.
The class works in both PostScript and direct PDF output modes, using the
@code{pgf} graphics system for visual effects.  Content is created in the
@code{frame} environment, and each frame can be made up of a number of slides
using a simple notation for specifying material to appear on each slide within
a frame.  Short versions of title, authors, institute can also be specified as
optional parameters.  Whole frame graphics are supported by plain frames.  The
class supports @code{figure} and @code{table} environments, transparency
effects, varying slide transitions and animations.")
    ;; Code is dual licensed under GPLv2+ or LPPL1.3c+; documentation is
    ;; dual-licensed under either FDLv1.3+ or LPPL1.3c+.
    (license (list license:lppl1.3c+ license:gpl2+ license:fdl1.3+))))

(define-public texlive-latex-xmpincl
  (package
    (name "texlive-latex-xmpincl")
    (version (number->string %texlive-revision))
    (source
     (origin
       (method svn-fetch)
       (uri (texlive-ref "latex" "xmpincl"))
       (sha256
        (base32
         "0lq3dfb4fsw955gjwllnk7cg00ciq5mva64mlpbva6g2jz117734"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/xmpincl"))
    (home-page "http://www.ctan.org/pkg/xmpincl")
    (synopsis "Include eXtensible Metadata Platform data in pdfLaTeX")
    (description
     "The XMP (eXtensible Metadata platform) is a framework to add metadata to
digital material to enhance the workflow in publication.  The essence is that
the metadata is stored in an XML file, and this XML stream is then embedded in
the file to which it applies.")
    (license license:gpl3+)))

(define-public texlive-latex-pdfx
  (package
    (name "texlive-latex-pdfx")
    (version (number->string %texlive-revision))
    (source
     (origin
       (method svn-fetch)
       (uri (texlive-ref "latex" "pdfx"))
       (sha256
        (base32
         "0ikxg8yzq78hy5b9x13d4nah46d0yvmwlqmdri06pygbx116dcac"))))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/pdfx"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-encoding
           (lambda _
             (substitute* "pdfx.dtx"
               (("    .+umaczy") "umaczy"))
             #t))
         (add-before 'install 'install-tex-files
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((target (string-append (assoc-ref outputs "out")
                                          "/share/texmf-dist/tex/latex/pdfx")))
               (mkdir-p target)
               (copy-recursively (assoc-ref inputs "texlive-tex-pdfx") target)
               ;; Install the generated version in the "install" phase.
               (delete-file (string-append target "/pdfx.sty"))
               #t))))))
    (propagated-inputs
     `(("texlive-generic-pdftex" ,texlive-generic-pdftex)))
    (native-inputs
     `(("texlive-tex-pdfx"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/tex/latex/pdfx"))
                 (revision %texlive-revision)))
           (file-name (string-append "texlive-tex-latex-pdfx-" version "-checkout"))
           (sha256
            (base32
             "14j1zsvqc59ims3sk34v6km8db6cimks28y5fcxcr5mi2ykvj4vf"))))))
    (home-page "https://www.ctan.org/pkg/pdfx")
    (synopsis "PDF/X and PDF/A support for pdfTeX, LuaTeX and XeTeX")
    (description
     "This package helps LaTeX users to create PDF/X, PFD/A and other
standards-compliant PDF documents with pdfTeX, LuaTeX and XeTeX.")
    (license license:lppl1.2+)))
