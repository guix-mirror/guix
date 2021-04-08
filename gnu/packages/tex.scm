;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2018, 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020, 2021 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Leo Le Bouter <lle-bout@zaclys.net>
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
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
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
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages texinfo)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:hide (zip)))

(define* (simple-texlive-package name locations hash
                                 #:key trivial?)
  "Return a template for a simple TeX Live package with the given NAME,
downloading from a list of LOCATIONS in the TeX Live repository, and expecting
the provided output HASH.  If TRIVIAL? is provided, all files will simply be
copied to their outputs; otherwise the TEXLIVE-BUILD-SYSTEM is used."
  (define with-documentation?
    (and trivial?
         (any (lambda (location)
                (string-prefix? "/doc" location))
              locations)))
  (package
    (name name)
    (version (number->string %texlive-revision))
    (source (texlive-origin name version
                            locations hash))
    (outputs (if with-documentation?
                 '("out" "doc")
                 '("out")))
    (build-system (if trivial?
                      gnu-build-system
                      texlive-build-system))
    (arguments
     (let ((copy-files
            `(lambda* (#:key outputs inputs #:allow-other-keys)
               (let (,@(if with-documentation?
                           `((doc (string-append (assoc-ref outputs "doc")
                                                 "/share/texmf-dist/")))
                           '())
                     (out (string-append (assoc-ref outputs "out")
                                         "/share/texmf-dist/")))
                 ,@(if with-documentation?
                       '((mkdir-p doc)
                         (copy-recursively
                          (string-append (assoc-ref inputs "source") "/doc")
                          (string-append doc "/doc")))
                       '())
                 (mkdir-p out)
                 (copy-recursively (assoc-ref inputs "source") out)
                 ,@(if with-documentation?
                       '((delete-file-recursively (string-append out "/doc")))
                       '())
                 #t))))
       (if trivial?
           `(#:tests? #f
             #:phases
             (modify-phases %standard-phases
               (delete 'configure)
               (replace 'build (const #t))
               (replace 'install ,copy-files)))
           `(#:phases
             (modify-phases %standard-phases
               (add-after 'install 'copy-files ,copy-files))))))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license #f)))

(define hyph-utf8-scripts
  (origin
    (method svn-fetch)
    (uri (texlive-ref "generic" "hyph-utf8"))
    (file-name (string-append "hyph-utf8-scripts-"
                              (number->string %texlive-revision)
                              "-checkout"))
    (sha256
     (base32
      "0lk7shx768sxvgr85y8bnmmnj8x4bbkgpxrz3z8jp8avi33prw83"))))

(define (texlive-hyphen-package name code locations hash)
  "Return a TeX Live hyphenation package with the given NAME, using source
files from LOCATIONS with expected checksum HASH.  CODE is not currently in use."
  (let ((parent (simple-texlive-package
                 name locations hash #:trivial? #t)))
    (package
      (inherit parent)
      (arguments
       (substitute-keyword-arguments (package-arguments parent)
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'build
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (let* ((out (assoc-ref outputs "out"))
                        (root (string-append out "/share/texmf-dist"))
                        (patterns
                         (string-append root "/tex/generic/hyph-utf8/patterns/txt/"))
                        (loaders
                         (string-append root "/tex/generic/hyph-utf8/loadhyph"))
                        (ptex
                         (string-append root "/tex/generic/hyph-utf8/patterns/ptex"))
                        (quote
                         (string-append root "/tex/generic/hyph-utf8/patterns/quote")))
                   (mkdir "scripts")
                   (copy-recursively
                    (assoc-ref inputs "hyph-utf8-scripts") "scripts")

                   ;; Prepare target directories
                   (mkdir-p patterns)
                   (mkdir-p loaders)
                   (mkdir-p ptex)
                   (mkdir-p quote)

                   ;; Generate plain patterns
                   (with-directory-excursion "scripts"
                     (substitute* "lib/tex/hyphen/path.rb"
                       (("^([[:blank:]]+)TeXROOT = .*" _ indent)
                        (string-append indent "TeXROOT = \""
                                       (getcwd) "/..\"\n")))

                     (substitute* "generate-plain-patterns.rb"
                       ;; Ruby 2 does not need this.
                       (("require 'unicode'") "")
                       ;; Write directly to the output directory
                       (("File\\.join\\(PATH::TXT")
                        (string-append "File.join(\"" patterns "\""))
                       (("File\\.join\\(PATH::QUOTE")
                        (string-append "File.join(\"" quote "\"")))
                     (invoke "ruby" "generate-plain-patterns.rb")

                     ;; Build pattern loaders
                     (substitute* "generate-pattern-loaders.rb"
                       (("File\\.join\\(PATH::LOADER")
                        (string-append "File.join(\"" loaders "\"")))

                     (invoke "ruby" "generate-pattern-loaders.rb")

                     ;; Build ptex patterns
                     (substitute* "generate-ptex-patterns.rb"
                       (("File\\.join\\(PATH::PTEX")
                        (string-append "File.join(\"" ptex "\"")))
                     (invoke "ruby" "generate-ptex-patterns.rb")))))))))
      (native-inputs
       `(("ruby" ,ruby)
         ("ruby-hydra" ,ruby-hydra)
         ("hyph-utf8-scripts" ,hyph-utf8-scripts)))
      (home-page "https://ctan.org/pkg/hyph-utf8"))))

(define texlive-extra-src
  (origin
    (method url-fetch)
    (uri "ftp://tug.org/historic/systems/texlive/2019/texlive-20190410-extra.tar.xz")
    (sha256 (base32
             "13ncf2an4nlqv18lki6y2p6pcsgs1i54zqkhfwprax5j53bk70j8"))))

(define texlive-texmf-src
  (origin
    (method url-fetch)
    (uri "ftp://tug.org/historic/systems/texlive/2019/texlive-20190410-texmf.tar.xz")
    (sha256 (base32
             "00n4qh9fj8v9zzy3y488hpfq1g3dnnh72y4yjsaikfcqpi59gv62"))))

(define-public texlive-bin
  (package
   (name "texlive-bin")
   (version "20190410")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "ftp://tug.org/historic/systems/texlive/2019/"
                          "texlive-" version "-source.tar.xz"))
      (sha256
       (base32
        "1dfps39q6bdr1zsbp9p74mvalmy3bycihv19sb9c6kg30kprz8nj"))
      (patches
       (let ((arch-patch
              (lambda (name revision hash)
                (origin
                  (method url-fetch)
                  (uri (string-append "https://git.archlinux.org/svntogit/packages.git"
                                      "/plain/trunk/" name "?h=packages/texlive-bin"
                                      "&id=" revision))
                  (file-name (string-append "texlive-bin-" name))
                  (sha256 (base32 hash)))))
             (arch-revision "49d7fe25e5ea63f136ebc20270c1d8fc9b00041c"))
         (list
          (arch-patch "pdftex-poppler0.76.patch" arch-revision
                      "03vc88dz37mjjyaspzv0fik2fp5gp8qv82114869akd1dhszbaax")
          (search-patch "texlive-bin-poppler-0.83.patch")
          (arch-patch "texlive-poppler-0.84.patch" arch-revision
                      "1ia6cr99krk4ipx4hdi2qdb98bh2h26mckjlpxdzrjnfhlnghksa")
          (search-patch "texlive-bin-poppler-0.86.patch"))))
      (modules '((guix build utils)
                 (ice-9 ftw)))
      (snippet
       '(begin
          (with-directory-excursion "libs"
            (let ((preserved-directories '("." ".." "lua53" "luajit")))
              ;; Delete bundled software, except Lua which cannot easily be
              ;; used as an external dependency.
              (for-each delete-file-recursively
                        (scandir "."
                                 (lambda (file)
                                   (and (not (member file preserved-directories))
                                        (eq? 'directory (stat:type (stat file)))))))))
          ;; TODO: Unbundle stuff in texk/dvisvgm/dvisvgm-src/libs too.
          #t))))
   (build-system gnu-build-system)
   (inputs
    `(("texlive-extra-src" ,texlive-extra-src)
      ("texlive-scripts"
       ,(origin
          (method svn-fetch)
          (uri (svn-reference
                (url (string-append "svn://www.tug.org/texlive/tags/"
                                    %texlive-tag "/Master/texmf-dist/"
                                    "/scripts/texlive"))
                (revision %texlive-revision)))
          (file-name (string-append "texlive-scripts-"
                                    (number->string %texlive-revision)
                                    "-checkout"))
          (sha256
           (base32
            "1cj04svl8bpfwjr4gqfcc04rmklz3aggrxvgj7q5bxrh7c7g18xh"))))
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
       '("--disable-static"
         "--disable-native-texlive-build"
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
         "--with-system-zziplib"
         ;; LuaJIT is not ported to powerpc64le* yet.
         ,@(if (string-prefix? "powerpc64le" (or (%current-target-system)
                                                 (%current-system)))
               '("--disable-luajittex"
                 "--disable-mfluajit")
               '()))

      ;; Disable tests on some architectures to cope with a failure of
      ;; luajiterr.test.
      ;; XXX FIXME fix luajit properly on these architectures.
      #:tests? ,(let ((s (or (%current-target-system)
                             (%current-system))))
                  (not (or (string-prefix? "aarch64" s)
                           (string-prefix? "mips64" s)
                           (string-prefix? "powerpc64le" s))))

      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'configure-ghostscript-executable
          ;; ps2eps.pl uses the "gswin32c" ghostscript executable on Windows,
          ;; and the "gs" ghostscript executable on Unix. It detects Unix by
          ;; checking for the existence of the /usr/bin directory. Since
          ;; Guix System does not have /usr/bin, it is also detected as Windows.
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "utils/ps2eps/ps2eps-src/bin/ps2eps.pl"
              (("gswin32c") "gs"))
            (substitute* "texk/texlive/linked_scripts/epstopdf/epstopdf.pl"
              (("\"gs\"")
               (string-append "\"" (assoc-ref inputs "ghostscript") "/bin/gs\"")))
            #t))
        (add-after 'unpack 'use-code-for-new-poppler
          (lambda _
            (copy-file "texk/web2c/pdftexdir/pdftoepdf-poppler0.76.0.cc"
                       "texk/web2c/pdftexdir/pdftoepdf.cc")
            (copy-file "texk/web2c/pdftexdir/pdftosrc-poppler0.76.0.cc"
                       "texk/web2c/pdftexdir/pdftosrc.cc")
            #t))
        (add-after 'unpack 'patch-dvisvgm-build-files
          (lambda _
            ;; XXX: Ghostscript is detected, but HAVE_LIBGS is never set, so
            ;; the appropriate linker flags are not added.
            (substitute* "texk/dvisvgm/configure"
              (("^have_libgs=yes" all)
               (string-append all "\nHAVE_LIBGS=1")))
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
              (let ((scripts (string-append share "/texmf-dist/scripts/texlive/")))
                (mkdir-p scripts)
                (copy-recursively (assoc-ref inputs "texlive-scripts") scripts)
                ;; Make sure that fmtutil can find its Perl modules.
                (substitute* (string-append scripts "fmtutil.pl")
                  (("\\$TEXMFROOT/") (string-append share "/"))))

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


(define texlive-docstrip
  (package
    (inherit (simple-texlive-package
              "texlive-docstrip"
              (list "/tex/latex/base/docstrip.tex")
              (base32
               "1f9sx1lp7v34zwm186msf03q2h28rrg0lh65z59zc0cvqffs6dvb")
              #:trivial? #t))
    (home-page "https://www.ctan.org/texlive")
    (synopsis "Utility to strip documentation from TeX files.")
    (description "This package provides the docstrip utility to strip
documentation from TeX files.  It is part of the LaTeX base.")
    (license license:lppl1.3+)))

(define-public texlive-unicode-data
  (package
    (inherit (simple-texlive-package
              "texlive-unicode-data"
              (list "/tex/generic/unicode-data/"
                    "/doc/generic/unicode-data/")
              (base32
               "0zy4v9y667cka5fi4dnc6x500907812y7pcaf63s5qxi8l7khxxy")
              #:trivial? #t))
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

(define-public texlive-generic-unicode-data
  (deprecated-package "texlive-generic-unicode-data" texlive-unicode-data))

(define-public texlive-hyphen-base
  (package
    (inherit (simple-texlive-package
              "texlive-hyphen-base"
              (list "/tex/generic/config/language.dat"
                    "/tex/generic/config/language.dat.lua"
                    "/tex/generic/config/language.def"
                    "/tex/generic/config/language.us"
                    "/tex/generic/config/language.us.def"
                    "/tex/generic/config/language.us.lua"
                    "/tex/generic/hyphen/dumyhyph.tex"
                    "/tex/generic/hyphen/hyphen.tex"
                    "/tex/generic/hyphen/hypht1.tex"
                    "/tex/generic/hyphen/zerohyph.tex")
              (base32
               "0f19nml4hdx9lh7accqdk1b9ismwfm2523l5zsc4kb4arysgcakz")
              #:trivial? #t))
    (home-page "https://tug.org/texlive/")
    (synopsis "Core hyphenation support files")
    (description "This package includes Knuth's original @file{hyphen.tex},
@file{zerohyph.tex} to disable hyphenation, @file{language.us} which starts
the autogenerated files @file{language.dat} and @file{language.def} (and
default versions of those), etc.")
    (license license:knuth)))

(define-public texlive-dvips
  (package
    (inherit (simple-texlive-package
              "texlive-dvips"
              (list "/doc/man/man1/afm2tfm.1"
                    "/doc/man/man1/dvips.1"
                    "/dvips/base/"
                    "/dvips/config/"
                    "/fonts/enc/dvips/base/"
                    "/tex/generic/dvips/")
              (base32
               "1qr7h0ahycmz5wmpv54glfss9jqdmmyymj6kim626d1c8v9bmg86")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/dvips")
    (synopsis "DVI to PostScript drivers")
    (description "This package provides files needed for converting DVI files
to PostScript.")
    (license license:lppl)))

(define-public texlive-tex-ini-files
  (package
    (inherit (simple-texlive-package
              "texlive-tex-ini-files"
              (list "/tex/generic/tex-ini-files/")
              (base32
               "0q1g62jg0qiqslm93ycvm30bw8ydmssjdshzsnzl7n2vpd62qfi2")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/tex-ini-files")
    (synopsis "Files for creating TeX formats")
    (description "This bundle provides a collection of model \".ini\" files
for creating TeX formats.  These files are commonly used to introduced
distribution-dependent variations in formats.  They are also used to
allow existing format source files to be used with newer engines, for example
to adapt the plain e-TeX source file to work with XeTeX and LuaTeX.")
    (license license:public-domain)))

(define-public texlive-generic-tex-ini-files
  (deprecated-package "texlive-generic-tex-ini-files" texlive-tex-ini-files))

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

(define-public texlive-fontinst
  (let ((template (simple-texlive-package
                   "texlive-fontinst"
                   (list "/doc/fonts/fontinst/"
                         "/doc/man/man1/fontinst.1"
                         "/doc/man/man1/fontinst.man1.pdf"

                         ;; This is used to build parts of
                         ;; /tex/fontinst/{base,misc}/ and
                         ;; /tex/latex/fontinst/fontdoc.sty.
                         "/source/fontinst/base/"

                         ;; These are not generated.
                         "/tex/fontinst/base/bbox.sty"
                         "/tex/fontinst/base/multislot.sty"
                         "/tex/fontinst/misc/glyphbox.mtx"
                         "/tex/fontinst/misc/glyphoff.mtx"
                         "/tex/fontinst/misc/glyphon.mtx"
                         "/tex/fontinst/misc/kernoff.mtx"
                         "/tex/fontinst/misc/kernon.mtx"

                         "/tex/fontinst/latinetx/"
                         "/tex/fontinst/latinmtx/"
                         "/tex/fontinst/mathmtx/"
                         "/tex/fontinst/smblmtx/"

                         "/scripts/texlive/fontinst.sh")
                   (base32
                    "0lprwib7n2ygfxvrw675vhif7ghyip2x6k70kqs9syp8lqxiizf8")
                   #:trivial? #t)))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:modules _ '())
          '((guix build gnu-build-system)
            (guix build utils)
            (ice-9 match)))
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'build
               (lambda* (#:key inputs #:allow-other-keys)
                 (setenv "TEXINPUTS"
                         (string-append (getcwd) "//:"
                                        (getcwd) "/source/fontinst/base//:"
                                        (assoc-ref inputs "texlive-docstrip") "//"))
                 (mkdir "build")
                 (invoke "tex" "-ini" "-interaction=scrollmode"
                         "-output-directory=build"
                         "fontinst.ins")))
             ;; Since we're using docstrip without LaTeX we can't set \UseTDS
             ;; or \BaseDirectory, so the generated files are just dumped in
             ;; the "build" directory.
             (add-after 'install 'install-generated-files
               (lambda* (#:key outputs #:allow-other-keys)
                 (let* ((out (assoc-ref outputs "out"))
                        (root (string-append out "/share/texmf-dist")))
                   (for-each (match-lambda
                               ((dir files ...)
                                (for-each (lambda (file)
                                            (install-file
                                             (string-append "build/" file)
                                             (string-append root dir)))
                                          files)))
                             '(("/tex/fontinst/base"
                                "fontinst.sty"
                                "cfntinst.sty"
                                "xfntinst.sty"
                                "finstmsc.sty"
                                "fontinst.ini")
                               ("/tex/fontinst/misc"
                                "csc2x.tex"
                                "csckrn2x.tex"
                                "osf2x.tex")
                               ("/tex/latex/fontinst"
                                "fontdoc.sty")))
                   #t)))))))
      (native-inputs
       `(("texlive-bin" ,texlive-bin)
         ("texlive-docstrip" ,texlive-docstrip)))
      (home-page "https://www.ctan.org/pkg/fontinst")
      (synopsis "Tools for converting and installing fonts for TeX and LaTeX")
      (description "This package provides TeX macros for converting Adobe Font
Metric files to TeX metric and virtual font format.  Fontinst helps mainly
with the number crunching and shovelling parts of font installation.  This
means in practice that it creates a number of files which give the TeX
metrics (and related information) for a font family that TeX needs to do any
typesetting in these fonts.")
      (license license:lppl1.1+))))

(define-public texlive-tex-fontinst-base
  (deprecated-package "texlive-tex-fontinst-base" texlive-fontinst))

(define-public texlive-fontname
  (package
    (inherit (simple-texlive-package
              "texlive-fontname"
              (list "/doc/fonts/fontname/fontname.texi"
                    "/fonts/map/fontname/")
              (base32
               "0h5im5rnhycrrkd6z10f17m2caa8lv594wf482b68qjmnxfrqnxj")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/fontname")
    (synopsis "Scheme for naming fonts in TeX")
    (description "This is Fontname, a naming scheme for (the base part of)
external TeX font filenames.  This makes at most eight-character names
from (almost) arbitrarily complex font names, thus helping portability of TeX
documents.")
    (license license:public-domain)))

(define-public texlive-cm
  (let ((template (simple-texlive-package
                   "texlive-cm"
                   (list "/fonts/source/public/cm/"
                         "/fonts/map/dvips/cm/cmtext-bsr-interpolated.map"
                         "/doc/fonts/cm/")
                   (base32
                    "09mvl94qrwlb9b4pkigi151l256v3djhwl4m5lgvk6yhn5y75zrp")
                   #:trivial? #t)))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:modules modules '())
          '((guix build gnu-build-system)
            (guix build utils)
            (srfi srfi-26)))
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'build
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((mf (assoc-ref inputs "texlive-metafont-base")))
                   ;; Tell mf where to find mf.base
                   (setenv "MFBASES" (string-append mf "/share/texmf-dist/web2c"))
                   ;; Tell mf where to look for source files
                   (setenv "MFINPUTS"
                           (string-append (getcwd) "/fonts/source/public/cm/:"
                                          mf "/share/texmf-dist/metafont/base")))
                 (for-each make-file-writable
                           (cons "fonts/source/public/cm/"
                                 (find-files "fonts/source/public/cm/" ".*")))
                 (let ((build (string-append (getcwd) "/build"))
                       (pkdir (string-append (getcwd) "/pk/ljfour/public/cm/dpi600")))
                   (mkdir-p pkdir)
                   (mkdir-p build)
                   (with-directory-excursion "fonts/source/public/cm/"
                     (for-each (lambda (font)
                                 (format #t "building font ~a\n" font)
                                 (invoke "mf" "-progname=mf"
                                         (string-append "-output-directory=" build)
                                         (string-append "\\"
                                                        "mode:=ljfour; "
                                                        "mag:=1+0/600; "
                                                        "scrollmode; "
                                                        "input "
                                                        (basename font ".mf")))
                                 (invoke "gftopk"
                                         (string-append build "/"
                                                        (basename font ".mf") ".600gf")
                                         (string-append pkdir "/"
                                                        (basename font ".mf") ".pk")))
                               (find-files "." "cm(.*[0-9]+.*|inch)\\.mf$"))))
                 #t))
             (add-after 'install 'install-generated-fonts
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (let* ((out    (assoc-ref outputs "out"))
                        (fonts  (string-append out "/share/texmf-dist/fonts/"))
                        (pk     (string-append fonts "pk"))
                        (tfm    (string-append fonts "tfm/public/cm")))
                   (for-each (cut install-file <> tfm)
                             (find-files "build" "\\.*"))
                   (copy-recursively "pk" pk)
                   #t)))))))
      (native-inputs
       `(("texlive-bin" ,texlive-bin)
         ("texlive-metafont-base" ,texlive-metafont-base)))
      (home-page "https://www.ctan.org/pkg/cm")
      (synopsis "Computer Modern fonts for TeX")
      (description "This package provides the Computer Modern fonts by Donald
Knuth.  The Computer Modern font family is a large collection of text,
display, and mathematical fonts in a range of styles, based on Monotype Modern
8A.")
      (license license:knuth))))

(define-public texlive-fonts-cm
  (deprecated-package "texlive-fonts-cm" texlive-cm))

(define-public texlive-cm-super
  (let ((template (simple-texlive-package
                   "texlive-cm-super"
                   (list "/doc/fonts/cm-super/"
                         "/dvips/cm-super/"
                         "/fonts/afm/public/cm-super/"
                         "/fonts/enc/dvips/cm-super/"
                         "/fonts/map/dvips/cm-super/"
                         "/fonts/map/vtex/cm-super/"
                         "/fonts/type1/public/cm-super/"
                         "/tex/latex/cm-super/")
                   (base32
                    "1k3afl0x0bqbr5mnawbnp7rr2126dwn0vwnxzibm9ggvzqilnkm6")
                   #:trivial? #t)))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'reset-gzip-timestamps)))))
      (home-page "https://www.ctan.org/pkg/cm-super")
      (synopsis "Computer Modern Super family of fonts")
      (description "The CM-Super family provides Adobe Type 1 fonts that replace
the T1/TS1-encoded Computer Modern (EC/TC), T1/TS1-encoded Concrete,
T1/TS1-encoded CM bright and LH Cyrillic fonts (thus supporting all European
languages except Greek), and bringing many ameliorations in typesetting
quality.  The fonts exhibit the same metrics as the METAFONT-encoded
originals.")
      ;; With font exception
      (license license:gpl2+))))

(define-public texlive-fonts-cm-super
  (deprecated-package "texlive-fonts-cm-super" texlive-cm-super))

(define-public texlive-courier
  (package
    (inherit (simple-texlive-package
              "texlive-courier"
              (list "/dvips/courier/"
                    "/fonts/afm/adobe/courier/"
                    "/fonts/afm/urw/courier/"
                    "/fonts/map/dvips/courier/"
                    "/fonts/tfm/adobe/courier/"
                    "/fonts/tfm/urw35vf/courier/"
                    "/fonts/type1/adobe/courier/"
                    "/fonts/type1/urw/courier/"
                    "/fonts/vf/adobe/courier/"
                    "/fonts/vf/urw35vf/courier/"
                    "/tex/latex/courier/"
                    "/tex4ht/ht-fonts/alias/adobe/courier/"
                    "/tex4ht/ht-fonts/unicode/adobe/courier/")
              (base32
               "1hfgisdi7mjf5156ax6d6zsbiq42zkmp1x5lg17rgzf61k6d6z1y")
              #:trivial? #t))
    (home-page "https://ctan.org/pkg/urw-base35")
    (synopsis "URW Base 35 font pack for LaTeX")
    (description "This package provides a drop-in replacements for the Courier
font from Adobe's basic set.")
    ;; No license version specified.
    (license license:gpl3+)))

(define-public texlive-lm
  (package
    (inherit (simple-texlive-package
              "texlive-lm"
              (list "/doc/fonts/lm/"
                    "/fonts/afm/public/lm/"
                    "/fonts/enc/dvips/lm/"
                    "/fonts/map/dvipdfm/lm/"
                    "/fonts/map/dvips/lm/"
                    "/fonts/opentype/public/lm/"
                    "/fonts/tfm/public/lm/"
                    "/fonts/type1/public/lm/"
                    "/tex/latex/lm/")
              (base32
               "0i1hwr8rp0jqyvs4qyplrirscd4w7lsgwsncyv3yzy80bsa56jq5")
              #:trivial? #t))
    (home-page "http://www.gust.org.pl/projects/e-foundry/latin-modern/")
    (synopsis "Latin Modern family of fonts")
    (description "The Latin Modern fonts are derived from the famous Computer
Modern fonts designed by Donald E. Knuth and described in Volume E of his
Computers & Typesetting series.")
    ;; The GUST font license (GFL) is legally identical to the LaTeX Project
    ;; Public License (LPPL), version 1.3c or later, but comes with an
    ;; additional but not legally binding clause.
    (license license:lppl1.3c+)))

(define-public texlive-fonts-lm
  (deprecated-package "texlive-fonts-lm" texlive-lm))

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
                                      (assoc-ref inputs "texlive-cm")
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
       ("texlive-cm" ,texlive-cm)))
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
       (file-name (string-append name "-" version "-checkout"))
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

(define-public texlive-mflogo-font
  (package
    (inherit (simple-texlive-package
              "texlive-mflogo-font"
              (list "/doc/fonts/mflogo-font/README"
                    "/fonts/afm/hoekwater/mflogo-font/"
                    "/fonts/map/dvips/mflogo-font/"
                    "/fonts/type1/hoekwater/mflogo-font/")
              (base32
               "094mknjv8ki2pvj1zin0f1z4f1w12g0cfqjiqcsawjsry4yfrmbg")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/mflogo-font")
    (synopsis "Metafont logo font")
    (description
     "These fonts were created in METAFONT by Knuth, for his own publications.
At some stage, the letters P and S were added, so that the METAPOST logo could
also be expressed.  The fonts were originally issued (of course) as METAFONT
source; they have since been autotraced and reissued in Adobe Type 1 format by
Taco Hoekwater.")
    (license license:knuth)))

(define-public texlive-fonts-mflogo-font
  (deprecated-package "texlive-fonts-mflogo-font" texlive-mflogo-font))

(define-public texlive-amsfonts
  (let ((template (simple-texlive-package
                   "texlive-amsfonts"
                   (list "/source/latex/amsfonts/"
                         "/fonts/source/public/amsfonts/"
                         "/fonts/type1/public/amsfonts/"
                         "/fonts/afm/public/amsfonts/"
                         "/fonts/map/dvips/amsfonts/"
                         "/tex/plain/amsfonts/"
                         "/doc/fonts/amsfonts/")
                   (base32
                    "15q70nkjf8wqzbd5ivcdx3i2sdgqxjb38q0qn9a2qw9i0qcnx6zw"))))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:build-targets _ #t)
          '(list "amsfonts.ins"))
         ((#:tex-directory _ #t)
          "latex/amsfonts")
         ((#:modules modules '())
          `((guix build texlive-build-system)
            (guix build utils)
            (ice-9 match)
            (srfi srfi-1)
            (srfi srfi-26)))
         ((#:phases phases)
          `(modify-phases ,phases
             (add-before 'build 'build-fonts
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((mf  (assoc-ref inputs "texlive-union"))
                       (src (string-append (getcwd) "/fonts/source/public/amsfonts/")))
                   ;; Make METAFONT reproducible
                   (setenv "SOURCE_DATE_EPOCH" "1")
                   ;; Tell mf where to find mf.base
                   (setenv "MFBASES" (string-append mf "/share/texmf-dist/web2c"))
                   ;; Tell mf where to look for source files
                   (setenv "MFINPUTS"
                           (string-append src ":"
                                          src "/cmextra:"
                                          src "/cyrillic:"
                                          src "/dummy:"
                                          src "/symbols:"
                                          mf "/share/texmf-dist/metafont/base:"
                                          (assoc-ref inputs "texlive-cm")
                                          "/share/texmf-dist/fonts/source/public/cm")))
                 (let ((build (string-append (getcwd) "/build-fonts")))
                   (mkdir-p build)
                   (with-directory-excursion "fonts/source/public/amsfonts"
                     (for-each (lambda (font)
                                 (format #t "building font ~a\n" (basename font ".mf"))
                                 (with-directory-excursion (dirname font)
                                   (invoke "mf" "-progname=mf"
                                           (string-append "-output-directory=" build)
                                           (string-append "\\"
                                                          "mode:=ljfour; "
                                                          "mag:=1; "
                                                          "nonstopmode; "
                                                          "input "
                                                          (getcwd) "/"
                                                          (basename font ".mf")))))
                               (find-files "." "[0-9]+\\.mf$"))))

                 ;; There are no metafont sources for the Euler fonts, so we
                 ;; convert the afm files instead.
                 (let ((build (string-append (getcwd) "/build-fonts/euler")))
                   (mkdir build)
                   (with-directory-excursion "fonts/afm/public/amsfonts/"
                     (for-each (lambda (font)
                                 (format #t "converting afm font ~a\n" (basename font ".afm"))
                                 (invoke "afm2tfm" font
                                         (string-append build "/"
                                                        (basename font ".tfm"))))
                               (find-files "." "\\.afm$")))

                   ;; Frustratingly, not all fonts can be created this way.  To
                   ;; generate eufm8.tfm, for example, we first scale down
                   ;; eufm10.afm to eufm8.pl, and then generate the tfm file from
                   ;; the pl file.
                   (setenv "TEXINPUTS"
                           (string-append build "//:"
                                          (getcwd) "/fonts/afm/public/amsfonts//:"
                                          (getcwd) "/source/latex/amsfonts//:"
                                          (assoc-ref inputs "texlive-union") "//"))
                   (with-directory-excursion build
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
                                 (("eurm9" 9) ("eurm10" 10))))))
                 #t))
             (add-after 'install 'install-generated-fonts
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (copy-recursively "build-fonts"
                                   (string-append
                                    (assoc-ref outputs "out")
                                    "/share/texmf-dist/fonts/tfm/public/amsfonts"))
                 #t))))))
      (native-inputs
       `(("texlive-union" ,(texlive-union (list texlive-tex-fontinst-base
                                                texlive-cm
                                                texlive-metafont-base)))))
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
      (license license:silofl1.1))))

(define-public texlive-fonts-amsfonts
  (deprecated-package "texlive-fonts-amsfonts" texlive-amsfonts))

(define-public texlive-latex-amsfonts
  (deprecated-package "texlive-latex-amsfonts" texlive-amsfonts))

(define-public texlive-mkpattern
  (package
    (inherit (simple-texlive-package
              "texlive-mkpattern"
              (list "/doc/plain/mkpattern/README"
                    "/doc/plain/mkpattern/mkpatdoc.tex"
                    "/doc/plain/mkpattern/mkpatter.pdf"
                    "/doc/plain/mkpattern/mkpattern-exmpl.tex"
                    "/tex/plain/mkpattern/mkpatter.tex")
              (base32
               "0sxnkbcc802jl3fj56x9hvg978bpv15lhrwj0aykb4syq29l47ga")
              #:trivial? #t))
    (home-page "https://ctan.org/pkg/mkpattern")
    (synopsis "Utility for making hyphenation patterns")
    (description "Mkpattern is a general purpose program for the generation of
hyphenation patterns, with definition of letter sets and template-like
constructions.  It also provides an easy way to handle different input and
output encodings, and features generation of clean UTF-8 patterns.")
    (license license:lppl)))

;; This provides etex.src which is needed to build various formats, including
;; luatex.fmt and pdflatex.fmt
(define-public texlive-etex
  (let ((template (simple-texlive-package
                   "texlive-etex"
                   (list "/doc/etex/base/"
                         "/doc/man/man1/etex.1"
                         "/doc/man/man1/etex.man1.pdf"
                         "/tex/plain/etex/"
                         "/fonts/source/public/etex/")
                   (base32
                    "1qv6vxm5a8pw38gas3i69ivmsn79zj2yq5n5vdmh0rzic5hw2hmc")
                   #:trivial? #t)))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:phases phases)
          `(modify-phases ,phases
             ;; Build tfm font.
             (replace 'build
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((mf (assoc-ref inputs "texlive-metafont-base")))
                   ;; Tell mf where to find mf.base
                   (setenv "MFBASES" (string-append mf "/share/texmf-dist/web2c"))
                   ;; Tell mf where to look for source files
                   (setenv "MFINPUTS"
                           (string-append (getcwd)
                                          "/fonts/source/public/etex/:"
                                          mf "/share/texmf-dist/metafont/base:"
                                          (assoc-ref inputs "texlive-cm")
                                          "/share/texmf-dist/fonts/source/public/cm")))
                 (invoke "mf" "-progname=mf"
                         (string-append "\\"
                                        "mode:=ljfour; "
                                        "mag:=1; "
                                        "scrollmode; "
                                        "input xbmc10"))
                 #t))
             (add-after 'install 'install-font
               (lambda* (#:key outputs #:allow-other-keys)
                 (install-file
                  "xbmc10.tfm"
                  (string-append (assoc-ref outputs "out")
                                 "/share/texmf-dist/fonts/tfm/public/etex/"))
                 #t))))))
      (native-inputs
       `(("texlive-bin" ,texlive-bin)
         ("texlive-metafont-base" ,texlive-metafont-base)
         ("texlive-cm" ,texlive-cm)))
      (home-page "https://www.ctan.org/pkg/etex")
      (synopsis "Extended version of TeX")
      (description
       "This package provides an extended version of TeX (which is capable of
running as if it were TeX unmodified).  E-TeX has been specified by the LaTeX
team as the engine for the development of LaTeX2e; as a result, LaTeX
programmers may assume e-TeX functionality.  The pdftex engine directly
incorporates the e-TeX extensions.")
      (license license:knuth))))

(define-public texlive-tex-plain
  (package
    (inherit (simple-texlive-package
              "texlive-tex-plain"
              (list "/tex/plain/")
              (base32
               "1m4qpaszwfv7j8a85rlwl7rs4iv5nlj67c1vvn6ysly72h9gjydb")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/plain")
    (synopsis "Plain TeX format and supporting files")
    (description
     "This package contains files used to build the Plain TeX format, as
described in the TeXbook, together with various supporting files (some also
discussed in the book).")
    (license license:knuth)))

(define-public texlive-helvetic
  (package
    (inherit (simple-texlive-package
              "texlive-helvetic"
              (list "/dvips/helvetic/"
                    "/fonts/afm/adobe/helvetic/"
                    "/fonts/afm/urw/helvetic/"
                    "/fonts/map/dvips/helvetic/"
                    "/fonts/tfm/adobe/helvetic/"
                    "/fonts/tfm/monotype/helvetic/"
                    "/fonts/tfm/urw35vf/helvetic/"
                    "/fonts/type1/urw/helvetic/"
                    "/fonts/vf/adobe/helvetic/"
                    "/fonts/vf/monotype/helvetic/"
                    "/fonts/vf/urw35vf/helvetic/"
                    "/tex/latex/helvetic/")
              (base32
               "0c3f1ly7y6404z0akbfbbfql13sz717v0n0g69qjpr69hi4n0nsl")
              #:trivial? #t))
    (home-page "https://ctan.org/pkg/urw-base35")
    (synopsis "URW Base 35 font pack for LaTeX")
    (description "This package provides a drop-in replacements for the Helvetica
font from Adobe's basic set.")
    ;; No license version specified.
    (license license:gpl3+)))

(define-public texlive-hyphen-afrikaans
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-afrikaans" "af"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-af.tex")
              (base32
               "1k9k27a27bbrb0gz36191w32l2v6d3zbdh8zhrp4l3ild2pj3n4l")))
    (synopsis "Hyphenation patterns for Afrikaans")
    (description "The package provides hyphenation patterns for the Afrikaans
language.")
    (license license:lppl1.3+)))

(define-public texlive-hyphen-ancientgreek
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-ancientgreek" "grc"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-grc.tex"
                    "/tex/generic/hyphen/grahyph5.tex"
                    "/tex/generic/hyphen/ibyhyph.tex")
              (base32
               "01326lb6z0s8krcfgs8i1pnjfrm4gr33rc53gy80f63qbv4ssxrw")))
    (synopsis "Hyphenation patterns for ancient Greek")
    (description "The package provides hyphenation patterns for ancient
Greek.")
    (license license:lppl1.3+)))

(define-public texlive-hyphen-armenian
  (let ((template (texlive-hyphen-package
                   "texlive-hyphen-armenian" "hy"
                   (list "/tex/generic/hyph-utf8/patterns/tex/hyph-hy.tex")
                   (base32
                    "0hzny0npynsb07syxrpbfa5pkpj8r0j51pj64yxyfl1c0bak1fwp"))))
    (package
      (inherit template)
      (synopsis "Hyphenation patterns for Armenian")
      (description "The package provides hyphenation patterns for the Armenian
language.")
      ;; Any version of the LGPL.
      (license license:lgpl3+))))

(define-public texlive-hyphen-basque
  (let ((template (texlive-hyphen-package
                    "texlive-hyphen-basque" "eu"
                    (list "/tex/generic/hyph-utf8/patterns/tex/hyph-eu.tex")
                    (base32
                     "15w969g1jqzn68l2b2lzf7iv7g3kil02aba3if6cag3qcnq92ra9"))))
    (package
      (inherit template)
      (synopsis "Hyphenation patterns for Basque")
      (description "The package provides hyphenation patterns for the Basque
language.")
      ;; Similar to Unicode license.
      (license (license:fsf-free
                "/tex/generic/hyph-utf8/patterns/tex/hyph-eu.tex")))))

(define-public texlive-hyphen-belarusian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-belarusian" "be"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-be.tex")
              (base32
               "0ppm12wndaxv9da62dwkbnk7w9nijikn6jkc97m76xis338g2h02")))
    (synopsis "Hyphenation patterns for Belarusian")
    (description "The package provides hyphenation patterns for the Belarusian
language.")
    (license license:expat)))

(define-public texlive-hyphen-bulgarian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-bulgarian" "bg"
              (list "/doc/generic/hyph-utf8/bg/azbukaExtended.pdf"
                    "/doc/generic/hyph-utf8/bg/azbukaExtended.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-bg.tex")
              (base32
               "0ngrgw2rmipxss76rgfk62x9nnsgwmaxxna2jqxxhybai3q39mx5")))
    (synopsis "Hyphenation patterns for Bulgarian")
    (description "The package provides hyphenation patterns for the Bulgarian
language in T2A and UTF-8 encodings.")
    (license (license:non-copyleft
              "file:///tex/generic/hyph-utf8/patterns/tex/hyph-bg.tex"
              "Ancestral BSD variant"))))

(define-public texlive-hyphen-catalan
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-catalan" "ca"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-ca.tex")
              (base32
               "10zzlfz5v8d9csg85ibpp2vfvmpqa56vbl85qy5gws099vygpayg")))
    (synopsis "Hyphenation patterns for Catalan")
    (description "The package provides hyphenation patterns for Catalan in
T1/EC and UTF-8 encodings.")
    (license license:lppl1.0+)))

(define-public texlive-hyphen-chinese
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-chinese" "zh-latn-pinyin"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-zh-latn-pinyin.tex")
              (base32
               "1j68mry2zy91m1kbzwhin5q2jajf6xh48npdds8wvp1sqmzih2a3")))
    (synopsis "Hyphenation patterns for unaccented Chinese pinyin")
    (description "The package provides hyphenation patterns for unaccented
Chinese pinyin T1/EC and UTF-8 encodings.")
    (license license:gpl2+)))

(define-public texlive-hyphen-churchslavonic
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-churchslavonic" "cu"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-cu.tex")
              (base32
               "0fhbwaapq2213msbhgr0d1lw06ihmrqirxj092mn73d8ynl13qlh")))
    (synopsis "Hyphenation patterns for Church Slavonic")
    (description "The package provides hyphenation patterns for Church
Slavonic in UTF-8 encoding.")
    (license license:expat)))

(define-public texlive-hyphen-coptic
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-coptic" "cop"
              (list "/tex/generic/hyph-utf8/patterns/tex-8bit/copthyph.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-cop.tex")
              (base32
               "1jlxxvyfa2aljizaa3qlcxyhqsrb4dawv3q3fbyp2lxz6ag9fy6m")))
    (synopsis "Hyphenation patterns for Coptic")
    (description "The package provides hyphenation patterns for Coptic in
UTF-8 encoding as well as in ASCII-based encoding for 8-bit engines.")
    ;; No explicit license declaration, so we use the project license.
    (license license:lppl)))

(define-public texlive-hyphen-croatian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-croatian" "hr"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-hr.tex")
              (base32
               "12n9r2winai15jc622sqdwclgcs1s68r6vcf7ic8vvq0x9qhwc5v")))
    (synopsis "Hyphenation patterns for Croatian")
    (description "The package provides hyphenation patterns for Croatian in
T1/EC and UTF-8 encodings.")
    (license license:lppl1.0+)))

(define-public texlive-hyphen-czech
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-czech" "cs"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-cs.tex")
              (base32
               "1q37s6p8yfyi3rp1azbz421lg4lr4aiki8m631i4x9rmps89m8iq")))
    (synopsis "Hyphenation patterns for Czech")
    (description "The package provides hyphenation patterns for Czech in T1/EC
and UTF-8 encodings.")
    (license license:gpl2+)))

(define-public texlive-hyphen-danish
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-danish" "da"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-da.tex")
              (base32
               "1vj8nip64rzcrcg3skm4vqad1ggqwgan74znrdns610wjcm1z9qd")))
    (synopsis "Hyphenation patterns for Danish")
    (description "The package provides hyphenation patterns for Danish in
T1/EC and UTF-8 encodings.")
    ;; Either LPPL 1.3 or later, or Expat
    (license (list license:lppl1.3+ license:expat))))

(define-public texlive-hyphen-dutch
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-dutch" "nl"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-nl.tex")
              (base32
               "1bg9g790ksq5cn8qihai6pacmkp9vpf35h4771z361nvwa40l8yk")))
    (synopsis "Hyphenation patterns for Dutch")
    (description "The package provides hyphenation patterns for Dutch in T1/EC
and UTF-8 encodings.")
    (license license:lppl1.0+)))

(define-public texlive-hyphen-english
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-english" '("en-gb" "en-us")
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-en-gb.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-en-us.tex")
              (base32
               "08b3jihjaamcl1pvffi0s47nwavkm66l9mrrmby3l32dfpkprrc5")))
    (synopsis "Hyphenation patterns for American and British English")
    (description "The package provides additional hyphenation patterns for
American and British English in ASCII encoding.")
    (license (license:non-copyleft
              "file:///tex/generic/hyph-utf8/patterns/tex/hyph-en-us.tex"
              "FSF all permissive license"))))

(define-public texlive-hyphen-esperanto
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-esperanto" "eo"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-eo.tex")
              (base32
               "1503kzn9bk4mm4ba35cka2hm8rz0v3j5l30v5rrsd4rqgpibcgic")))
    (synopsis "Hyphenation patterns for Esperanto")
    (description "The package provides hyphenation patterns for Esperanto ISO
Latin 3 and UTF-8 encodings.")
    (license license:lppl1.0+)))

(define-public texlive-hyphen-estonian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-estonian" "et"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-et.tex")
              (base32
               "1rdas2450ib02rwy65i69l86nyc9h15bl07xbbwhmhxfnj8zj4v8")))
    (synopsis "Hyphenation patterns for Estonian")
    (description "The package provides hyphenation patterns for Estonian in
T1/EC and UTF-8 encodings.")
    ;; Dual licensed under either license.
    (license (list license:lppl1.3+ license:expat))))

(define-public texlive-hyphen-ethiopic
  (let ((template (texlive-hyphen-package
                   "texlive-hyphen-ethiopic" "mul-ethi"
                   (list "/tex/generic/hyph-utf8/patterns/tex/hyph-mul-ethi.tex")
                   (base32
                    "1b93fc6j4aybh0pgq23hsn1njm6asf7sfz803fbj3ai0whsxd10l"))))
    (package
      (inherit template)
      (synopsis "Hyphenation patterns for Ethiopic scripts")
      (description "The package provides hyphenation patterns for languages
written using the Ethiopic script for Unicode engines.  They are not supposed
to be linguistically relevant in all cases and should, for proper typography,
be replaced by files tailored to individual languages.")
      (license license:expat))))

(define-public texlive-hyphen-finnish
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-finnish" "fi"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-fi.tex")
              (base32
               "1f72b4ydb4zddvw2i004948khmwzigxkdkwfym5v1kkq0183sfpj")))
    (synopsis "Hyphenation patterns for Finnish")
    (description "The package provides hyphenation patterns for Finnish in
T1/EC and UTF-8 encodings.")
    (license license:public-domain)))

(define-public texlive-hyphen-french
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-french" "fr"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-fr.tex")
              (base32
               "0jc3kqys6cxjw8x8pzjln7z78l8s7f5rlyrkv7dzr1kiwnwilk9d")))
    (synopsis "Hyphenation patterns for French")
    (description "The package provides hyphenation patterns for French in
T1/EC and UTF-8 encodings.")
    (license license:expat)))

(define-public texlive-hyphen-friulan
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-friulan" "fur"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-fur.tex")
              (base32
               "1dlnh8slpf50mryxv7zzbx08xp54zkdfs1j7y37ipwbrajvd740f")))
    (synopsis "Hyphenation patterns for Friulan")
    (description "The package provides hyphenation patterns for Friulan in
ASCII encodings.")
    (license license:lppl1.3+)))

(define-public texlive-hyphen-galician
  (let ((template (texlive-hyphen-package
                   "texlive-hyphen-galician" "gl"
                   (list "/tex/generic/hyph-utf8/patterns/tex/hyph-gl.tex")
                   (base32
                    "13zx2r3nrxdr025g2lxrph0ga6wf7cs8dxixn4fhbl6xr1cx028g"))))
    (package
      (inherit template)
      (synopsis "Hyphenation patterns for Galician")
      (description "The package provides hyphenation patterns for Galician in
T1/EC and UTF-8 encodings.")
      (license license:lppl1.3))))

(define-public texlive-hyphen-georgian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-georgian" "ka"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-ka.tex")
              (base32
               "0l0hk7ka04fr8x11nnw95x151cxyycy0fph772m3a3p8qk4x9wp7")))
    (synopsis "Hyphenation patterns for Georgian")
    (description "The package provides hyphenation patterns for Georgian in
T8M, T8K, and UTF-8 encodings.")
    (license license:lppl1.3+)))

(define-public texlive-hyphen-german
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-german" '("de-1901" "de-1996" "de-ch-1901")
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-de-1901.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-de-1996.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-de-ch-1901.tex"
                    "/tex/generic/dehyph/dehyphn.tex"
                    "/tex/generic/dehyph/dehypht.tex"
                    "/tex/generic/dehyph/dehyphtex.tex"
                    "/tex/generic/dehyph/README")
              (base32
               "0wp5by5kkf4ac6li5mbppqzw11500wa7f22p5vpz3m1kwd15zavw")))
    (synopsis "Hyphenation patterns for German")
    (description "This package provides hyphenation patterns for German in
T1/EC and UTF-8 encodings, for traditional and reformed spelling, including
Swiss German.")
    ;; The patterns are released under the Expat license; the dehyph* files
    ;; are released under the LPPL version 1 or later.
    (license (list license:expat license:lppl1.0+))))

(define-public texlive-hyphen-greek
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-greek" '("el-monoton" "el-polyton")
              (list "/doc/generic/elhyphen/"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-el-monoton.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-el-polyton.tex"
                    "/tex/generic/hyphen/grmhyph5.tex"
                    "/tex/generic/hyphen/grphyph5.tex")
              (base32
               "1qyr6m1nh6d4wj68616cfxv4wjpiy1w2rlldxlx2ajzba381w3hf")))
    (synopsis "Hyphenation patterns for Greek")
    (description "This package provides hyphenation patterns for Modern Greek
in monotonic and polytonic spelling in LGR and UTF-8 encodings.")
    (license license:lppl)))

(define-public texlive-hyphen-hungarian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-hungarian" "hu"
              (list "/doc/generic/huhyphen/"
                    "/doc/generic/hyph-utf8/hu/"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-hu.tex")
              (base32
               "1j1b8kksg9r8nmjyjvvz8fr3hgcrjj6jlybf9p06nwrrwm2r8j8f")))
    (synopsis "Hyphenation patterns for Hungarian")
    (description "This package provides hyphenation patterns for Hungarian in
T1/EC and UTF-8 encodings.")
    ;; Any of these licenses
    (license (list license:gpl2 license:lgpl2.1+ license:mpl1.1))))

(define-public texlive-hyphen-icelandic
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-icelandic" "is"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-is.tex")
              (base32
               "1m9xj41csj3ldym09d82zjbd3345sg2z10d8pxpvhgibf97mb66h")))
    (synopsis "Hyphenation patterns for Icelandic")
    (description "This package provides hyphenation patterns for Icelandic in
T1/EC and UTF-8 encodings.")
    (license license:lppl1.2+)))

(define-public texlive-hyphen-indic
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-indic"
              '("as" "bn" "gu" "hi" "kn" "ml" "mr" "or" "pa" "ta" "te")
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-as.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-bn.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-gu.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-hi.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-kn.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-ml.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-mr.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-or.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-pa.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-ta.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-te.tex")
              (base32
               "02d2kcd3lpk95fykjwhzw9s2a1s2w1skz8h2mmszrz979d1xzhpm")))
    (synopsis "Indic hyphenation patterns")
    (description "This package provides hyphenation patterns for Assamese,
Bengali, Gujarati, Hindi, Kannada, Malayalam, Marathi, Oriya, Panjabi, Tamil
and Telugu for Unicode engines.")
    (license license:expat)))

(define-public texlive-hyphen-indonesian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-indonesian" "id"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-id.tex")
              (base32
               "1r62w02rf0i4z0jgij54d16qjbj0zyfwm9dwdkqka76jrivij83q")))
    (synopsis "Indonesian hyphenation patterns")
    (description "This package provides hyphenation patterns for
Indonesian (Bahasa Indonesia) in ASCII encoding.  They are probably also
usable for Malay (Bahasa Melayu).")
    (license license:gpl2)))

(define-public texlive-hyphen-interlingua
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-interlingua" "ia"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-ia.tex")
              (base32
               "0a9na20vjnzhgjbicaxay0jk4rm5zg1rjyiswr377mjhd9mx5cg3")))
    (synopsis "Interlingua hyphenation patterns")
    (description "This package provides hyphenation patterns for Interlingua
in ASCII encoding.")
    (license license:lppl1.3+)))

(define-public texlive-hyphen-irish
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-irish" "ga"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-ga.tex")
              (base32
               "1h1l9jzkpsb91nyhz6s6c9jfrbz8jx5ip8vyq3dkz0rl6g960i6b")))
    (synopsis "Irish hyphenation patterns")
    (description "This package provides hyphenation patterns for
Irish (Gaeilge) in T1/EC and UTF-8 encodings.")
    ;; Either of these licenses
    (license (list license:gpl2+ license:expat))))

(define-public texlive-hyphen-italian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-italian" "it"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-it.tex")
              (base32
               "03c7jiqslfxvl3gbdx79hggbvrfi2l4z2bnwxc0na8f8lkp1m787")))
    (synopsis "Italian hyphenation patterns")
    (description "This package provides hyphenation patterns for Italian in
ASCII encoding.  Compliant with the Recommendation UNI 6461 on hyphenation
issued by the Italian Standards Institution (Ente Nazionale di Unificazione
UNI).")
    (license license:lppl1.3+)))

(define-public texlive-hyphen-kurmanji
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-kurmanji" "kmr"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-kmr.tex")
              (base32
               "01ylbsi5wymrdrxr9b28nmjmcj72mdhqr657lwsb6m9aj33c9ql6")))
    (synopsis "Kurmanji hyphenation patterns")
    (description "This package provides hyphenation patterns for
Kurmanji (Northern Kurdish) as spoken in Turkey and by the Kurdish diaspora in
Europe, in T1/EC and UTF-8 encodings.")
    (license license:lppl1.3)))

(define-public texlive-hyphen-latin
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-latin" '("la-x-classic" "la-x-liturgic" "la")
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-la-x-classic.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-la-x-liturgic.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-la.tex")
              (base32
               "0rxg8a4s5cpj8vlkz5a74a036axda5jqgvr3f9aj2cc2x9f2f3w9")))
    (synopsis "Liturgical Latin hyphenation patterns")
    (description "This package provides hyphenation patterns for Latin in
T1/EC and UTF-8 encodings, mainly in modern spelling (u when u is needed and v
when v is needed), medieval spelling with the ligatures @code{\\ae} and
@code{\\oe} and the (uncial) lowercase 'v' written as a 'u' is also supported.
Apparently there is no conflict between the patterns of modern Latin and those
of medieval Latin.  It also includes hyphenation patterns for the Classical
Latin in T1/EC and UTF-8 encodings.  Classical Latin hyphenation patterns are
different from those of 'plain' Latin, the latter being more adapted to modern
Latin.  It also provides hyphenation patterns for the Liturgical Latin in
T1/EC and UTF-8 encodings.")
    ;; Either of these licenses
    (license (list license:lppl1.0+ license:expat))))

(define-public texlive-hyphen-latvian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-latvian" "lv"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-lv.tex")
              (base32
               "00jf8xma4ldz0zpqwma97k9q3j0mqx7qdj6b7baph3n5xgc24aaw")))
    (synopsis "Latvian hyphenation patterns")
    (description "This package provides hyphenation patterns for Latvian in
L7X and UTF-8 encodings.")
    ;; Either of these licenses.
    (license (list license:gpl2 license:lgpl2.1))))

(define-public texlive-hyphen-lithuanian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-lithuanian" "lt"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-lt.tex")
              (base32
               "1kfq7j2ajg6nj952s1ygd520sj9z9kl0bqvd291a36ni2b1frzgd")))
    (synopsis "Lithuanian hyphenation patterns")
    (description "This package provides hyphenation patterns for Lithuanian in
L7X and UTF-8 encodings.")
    ;; "Do ... whatever ... as long as you respect the copyright"; as part of
    ;; the hyph-utf8 package we choose the LPPL license.
    (license license:lppl)))

(define-public texlive-hyphen-mongolian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-mongolian" '("mn-cyrl-x-lmc" "mn-cyrl")
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-mn-cyrl-x-lmc.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-mn-cyrl.tex")
              (base32
               "1y1b91ihrdl9bad3rxlsfjpd9wmyd5zzgci3qv9w8qqk33jxhwya")))
    (synopsis "Mongolian hyphenation patterns in Cyrillic script")
    (description "This package provides hyphenation patterns for Mongolian in
T2A, LMC and UTF-8 encodings.")
    ;; Either of these licenses
    (license (list license:lppl1.3+ license:expat))))

(define-public texlive-hyphen-norwegian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-norwegian" '("nb" "nn" "no")
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-nb.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-nn.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-no.tex")
              (base32
               "08gbwj64p4fckm199k52yp5lx65h9f4wwdkvl4pv4aa7k370jq9y")))
    (synopsis "Norwegian Bokmal and Nynorsk hyphenation patterns")
    (description "This package provides hyphenation patterns for Norwegian
Bokmal and Nynorsk in T1/EC and UTF-8 encodings.")
    (license (license:non-copyleft
              "/tex/generic/hyph-utf8/patterns/tex/hyph-no.tex"
              "FSF All permissive license"))))

(define-public texlive-hyphen-occitan
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-occitan" "oc"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-oc.tex")
              (base32
               "0vhjbq2nr58vhqwwky3cwx4dqiwjmmfwp81rb65mfpf0m8yypdfg")))
    (synopsis "Occitan hyphenation patterns")
    (description "This package provides hyphenation patterns for Occitan in
T1/EC and UTF-8 encodings.  They are supposed to be valid for all the Occitan
variants spoken and written in the wide area called 'Occitanie' by the French.
It ranges from the Val d'Aran within Catalunya, to the South Western Italian
Alps encompassing the southern half of the French pentagon.")
    (license license:lppl1.0+)))

(define-public texlive-hyphen-pali
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-pali" "pi"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-pi.tex")
              (base32
               "1fak853s4ijdqgrnhwymaq1lh8jab3qfyxapdmf6qpg6bqd20kxq")))
    (synopsis "Panjabi hyphenation patterns")
    (description "This package provides hyphenation patterns for Panjabi in
T1/EC encoding.")
    ;; Can be used with either license.
    (license (list license:expat license:lgpl3+ license:gpl3+))))

(define-public texlive-hyphen-piedmontese
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-piedmontese" "pms"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-pms.tex")
              (base32
               "0xva3l2gwzkqw1sz64k5g5iprhdyr27w1mv8rxp8x62i5y3aqr1k")))
    (synopsis "Piedmontese hyphenation patterns")
    (description "This package provides hyphenation patterns for Piedmontese
in ASCII encoding.  Compliant with 'Gramatica dla lengua piemonteisa' by
Camillo Brero.")
    (license license:lppl1.3+)))

(define-public texlive-hyphen-polish
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-polish" "pl"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-pl.tex")
              (base32
               "1c22g99isxapv4xjrmsw24hhp1xb83wbgcxyd8j24mxdnizywxzm")))
    (synopsis "Polish hyphenation patterns")
    (description "This package provides hyphenation patterns for Polish in QX
and UTF-8 encodings.")
    ;; No differing license declared, so we choose the project license.
    (license license:lppl)))

(define-public texlive-hyphen-portuguese
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-portuguese" "pt"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-pt.tex")
              (base32
               "00rkjy4p7893zs940bq3s4hp7al0skgxqggj5qfax0bx8karf30b")))
    (synopsis "Portuguese hyphenation patterns")
    (description "This package provides hyphenation patterns for Portuguese in
T1/EC and UTF-8 encodings.")
    (license license:bsd-3)))

(define-public texlive-hyphen-romanian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-romanian" "ro"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-ro.tex")
              (base32
               "1ykb5v7ip6p3n34wq8qypfyrap4gg946by5rsl6ab0k5gv6ypsbf")))
    (synopsis "Romanian hyphenation patterns")
    (description "This package provides hyphenation patterns for Romanian in
T1/EC and UTF-8 encodings.")
    ;; No differing license declared, so we choose the project license.
    (license license:lppl)))

(define-public texlive-hyphen-romansh
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-romansh" "rm"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-rm.tex")
              (base32
               "0a1q9p6sp5n6a9w6xhwk03vmkrrmnh2md7g1k4qhnf0dc4h7dy9r")))
    (synopsis "Romansh hyphenation patterns")
    (description "This package provides hyphenation patterns for Romansh in
ASCII encodings.  They are supposed to comply with the rules indicated by the
Lia Rumantscha (Romansh language society).")
    (license license:lppl1.3+)))

(define-public texlive-hyphen-russian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-russian" "ru"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-ru.tex")
              (base32
               "00sy7qh5f8ryxw36fwbyd1yi2hxhv7hmk99yp7dwh73n4mxv6lpl")))
    (synopsis "Russian hyphenation patterns")
    (description "This package provides hyphenation patterns for Russian in
T2A and UTF-8 encodings.")
    (license license:lppl1.2+)))

(define-public texlive-hyphen-sanskrit
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-sanskrit" "sa"
              (list "/doc/generic/hyph-utf8/sa/hyphenmin.txt"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-sa.tex")
              (base32
               "0gi2qk0wf388h9n25gzhv0cdz67ph83wal8h3iz2sqnpdjsw8kpc")))
    (synopsis "Sanskrit hyphenation patterns")
    (description "This package provides hyphenation patterns for Sanskrit and
Prakrit in longdesc transliteration, and in Devanagari, Bengali, Kannada,
Malayalam longdesc and Telugu scripts for Unicode engines.")
    ;; "You may freely use, copy, modify and/or distribute this file."
    (license (license:non-copyleft
              "file:///tex/generic/hyph-utf8/patterns/tex/hyph-sa.tex"))))

(define-public texlive-hyphen-serbian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-serbian" '("sh-cyrl" "sh-latn" "sr-cyrl")
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-sh-cyrl.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-sh-latn.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-sr-cyrl.tex")
              (base32
               "0pwc9z0m5y6acq1vqm0da9akg156jbhxzvsfp2f8bsz5b99y5z45")))
    (synopsis "Serbian hyphenation patterns")
    (description "This package provides hyphenation patterns for Serbian in
T1/EC, T2A and UTF-8 encodings.")
    ;; Any version of the GPL.
    (license license:gpl3+)))

(define-public texlive-hyphen-slovak
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-slovak" "sk"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-sk.tex")
              (base32
               "0ppp53bbclp5c8wvx748krvrp5y5053khgkjnnv966a90fvp3vgd")))
    (synopsis "Slovak hyphenation patterns")
    (description "This package provides hyphenation patterns for Slovak in
T1/EC and UTF-8 encodings.")
    (license license:gpl2+)))

(define-public texlive-hyphen-slovenian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-slovenian" "sl"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-sl.tex")
              (base32
               "02n8l9yf4hqyhbpsc1n6b2mggy09z6lq4dcb8ndiwawb6h0mp7s4")))
    (synopsis "Slovenian hyphenation patterns")
    (description "This package provides hyphenation patterns for Slovenian in
T1/EC and UTF-8 encodings.")
    ;; Either license
    (license (list license:lppl1.0+ license:expat))))

(define-public texlive-hyphen-spanish
  (package
    ;; The source files "eshyph-make.lua" and "eshyph.src" are provided to
    ;; generate obsolete hyphenation patterns, which aren't included in a
    ;; default TeX Live distribution, so we don't include them either.
    (inherit (texlive-hyphen-package
              "texlive-hyphen-spanish" "es"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-es.tex")
              (base32
               "1h3yg9vcq0lf7hxv0ahkqmyg269dxjs8m2mz8sgz5l1fxmvahvaj")))
    (synopsis "Hyphenation patterns for Spanish")
    (description "The package provides hyphenation patterns for Spanish in
T1/EC and UTF-8 encodings.")
    (license license:expat)))

(define-public texlive-hyphen-swedish
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-swedish" "sv"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-sv.tex")
              (base32
               "1n7incy7n24pix1q2i8c3h7i78zpql5ayhskavlmy6mhd7ayncaw")))
    (synopsis "Swedish hyphenation patterns")
    (description "This package provides hyphenation patterns for Swedish in
T1/EC and UTF-8 encodings.")
    (license license:lppl1.2+)))

(define-public texlive-hyphen-thai
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-thai" "th"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-th.tex")
              (base32
               "00gxcs4jfqifd5cnrjipn77m73fmpw2qms4lp216jj3kz4a7h9kf")))
    (synopsis "Thai hyphenation patterns")
    (description "This package provides hyphenation patterns for Thai in LTH
and UTF-8 encodings.")
    (license license:lppl1.3+)))

(define-public texlive-hyphen-turkish
  (let ((template (texlive-hyphen-package
                   "texlive-hyphen-turkish" "tr"
                   (list "/tex/generic/hyph-utf8/patterns/tex/hyph-tr.tex")
                   (base32
                    "04sihjgpm31i5bi67rrfp15w3imn7hxwwk70v0vhx053ghxy72vh"))))
    (package
      (inherit template)
      (synopsis "Hyphenation patterns for Turkish")
      (description "The package provides hyphenation patterns for Turkish in
T1/EC and UTF-8 encodings.  The patterns for Turkish were first produced for
the Ottoman Texts Project in 1987 and were suitable for both Modern Turkish
and Ottoman Turkish in Latin script, however the required character set didn't
fit into EC encoding, so support for Ottoman Turkish had to be dropped to keep
compatibility with 8-bit engines.")
      (license license:lppl1.0+))))

(define-public texlive-hyphen-turkmen
  (let ((template (texlive-hyphen-package
                   "texlive-hyphen-turkmen" "tk"
                   (list "/tex/generic/hyph-utf8/patterns/tex/hyph-tk.tex")
                   (base32
                    "0g5ip2lw9g47s61mv3cypswc6qm7zy9c4iqq4h19ysvds81adzkr"))))
    (package
      (inherit template)
      (synopsis "Hyphenation patterns for Turkmen")
      (description "The package provides hyphenation patterns for Turkmen in
T1/EC and UTF-8 encodings.")
      (license license:expat))))

(define-public texlive-hyphen-ukrainian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-ukrainian" "uk"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-uk.tex")
              (base32
               "0fbfhx1fmbshxr4ihsjaqgx251h69h7i288p8gh3w6ysgxr53p60")))
    (synopsis "Ukrainian hyphenation patterns")
    (description "This package provides hyphenation patterns for Ukrainian in
T2A and UTF-8 encodings.")
    ;; No version specified
    (license license:lppl)))

(define-public texlive-hyphen-uppersorbian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-uppersorbian" "hsb"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-hsb.tex")
              (base32
               "0x0051wph3sqmzzw6prvjy6bp7gn02rbmys1bmbc210jk3pkylfj")))
    (synopsis "Upper Sorbian hyphenation patterns")
    (description "This package provides hyphenation patterns for Upper Sorbian
in T1/EC and UTF-8 encodings.")
    (license license:lppl1.3a+)))

(define-public texlive-hyphen-welsh
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-welsh" "cy"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-cy.tex")
              (base32
               "1bpxp3jiifdw7waw2idz5j9xgi3526nkxm8mbmsspr4mlf2xyr76")))
    (synopsis "Welsh hyphenation patterns")
    (description "This package provides hyphenation patterns for Welsh in
T1/EC and UTF-8 encodings.")
    ;; Either license
    (license (list license:lppl1.0+ license:expat))))

(define-public texlive-hyph-utf8
  (package
    (inherit (simple-texlive-package
              "texlive-hyph-utf8"
              (list "/source/generic/hyph-utf8/"
                    "/source/luatex/hyph-utf8/"
                    "/doc/luatex/hyph-utf8/"
                    "/tex/luatex/hyph-utf8/etex.src"
                    ;; Used to extract luatex-hyphen.lua
                    "/tex/latex/base/docstrip.tex"

                    ;; Documentation; we can't use the whole directory because
                    ;; it includes files from other packages.
                    "/doc/generic/hyph-utf8/CHANGES"
                    "/doc/generic/hyph-utf8/HISTORY"
                    "/doc/generic/hyph-utf8/hyph-utf8.pdf"
                    "/doc/generic/hyph-utf8/hyph-utf8.tex"
                    "/doc/generic/hyph-utf8/hyphenation-distribution.pdf"
                    "/doc/generic/hyph-utf8/hyphenation-distribution.tex"
                    "/doc/generic/hyph-utf8/img/miktex-languages.png"
                    "/doc/generic/hyph-utf8/img/texlive-collection.png")
              (base32
               "1bar5mc808ch20anhqrdxcwiych359qsvr7mggxpg2l2kq5xdyq0")))
    (outputs '("out" "doc"))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are none
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 match))
       #:make-flags
       (list "-C" "source/luatex/hyph-utf8/"
             (string-append "DO_TEX = tex --interaction=nonstopmode '&tex' $<")
             (string-append "RUNDIR =" (assoc-ref %outputs "out") "/share/texmf-dist/tex/luatex/hyph-utf8/")
             (string-append "DOCDIR =" (assoc-ref %outputs "doc") "/share/texmf-dist/doc/luatex/hyph-utf8/")
             ;; hyphen.cfg is neither included nor generated, so let's only build the lua file.
             (string-append "UNPACKED = $(NAME).lua"))
       #:phases
       (modify-phases %standard-phases
         ;; TeX isn't usable at this point, so we first need to generate the
         ;; tex.fmt.
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Target directories must exist.
             (mkdir-p (string-append (assoc-ref %outputs "out")
                                     "/share/texmf-dist/tex/luatex/hyph-utf8/"))
             (mkdir-p (string-append (assoc-ref %outputs "doc")
                                     "/share/texmf-dist/doc/luatex/hyph-utf8/"))

             ;; We cannot build the documentation because that requires a
             ;; fully functional pdflatex, which depends on this package.
             (substitute* "source/luatex/hyph-utf8/Makefile"
               (("all: .*") "all: $(RUNFILES)\n"))

             ;; Find required fonts for building tex.fmt
             (setenv "TFMFONTS"
                     (string-append (assoc-ref inputs "texlive-cm")
                                    "/share/texmf-dist/fonts/tfm/public/cm:"
                                    (assoc-ref inputs "texlive-fonts-knuth-lib")
                                    "/share/texmf-dist/fonts/tfm/public/knuth-lib"))
             ;; ...and find all tex files in this environment.
             (setenv "TEXINPUTS"
                     (string-append
                      (getcwd) ":"
                      (string-join
                       (map (match-lambda ((_ . dir) dir)) inputs)
                       "//:")))

             ;; Generate tex.fmt.
             (let ((where "source/luatex/hyph-utf8"))
               (mkdir-p where)
               (with-directory-excursion where
                 (invoke "tex" "-ini"
                         (string-append (assoc-ref inputs "texlive-tex-plain")
                                        "/share/texmf-dist/tex/plain/config/tex.ini"))))))
         (add-before 'build 'build-loaders-and-converters
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((root (string-append (assoc-ref outputs "out")
                                         "/share/texmf-dist"))
                    (conv
                     (string-append root
                                    "/tex/generic/hyph-utf8/conversions")))

               ;; Build converters
               (mkdir-p conv)
               (with-directory-excursion "source/generic/hyph-utf8"
                 (substitute* "generate-converters.rb"
                   (("\\$path_root=File.*")
                    (string-append "$path_root=\"" root "\"\n"))
                   ;; Avoid error with newer Ruby.
                   (("#1\\{%") "#1{%%"))
                 (invoke "ruby" "generate-converters.rb"))
               #t)))
         (replace 'install
           (lambda* (#:key source outputs #:allow-other-keys)
             (let ((doc (assoc-ref outputs "doc"))
                   (out (assoc-ref outputs "out")))
               (mkdir-p doc)
               (copy-recursively
                (string-append source "/doc")
                (string-append doc "/doc"))
               (install-file
                (string-append source "/tex/luatex/hyph-utf8/etex.src")
                (string-append out "/share/texmf-dist/tex/luatex/hyph-utf8/")))
             #t)))))
    (native-inputs
     `(("ruby" ,ruby)
       ("texlive-bin" ,texlive-bin)
       ;; The following packages are needed for build "tex.fmt", which we need
       ;; for a working "tex".
       ("texlive-tex-plain" ,texlive-tex-plain)
       ("texlive-cm" ,texlive-cm)
       ("texlive-fonts-knuth-lib" ,texlive-fonts-knuth-lib)
       ("texlive-hyphen-base" ,texlive-hyphen-base)))
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

(define-public texlive-generic-hyph-utf8
  (deprecated-package "texlive-generic-hyph-utf8" texlive-hyph-utf8))

(define-public texlive-dehyph-exptl
  (package
    (inherit (simple-texlive-package
              "texlive-dehyph-exptl"
              (list "/tex/generic/dehyph-exptl/"
                    "/doc/generic/dehyph-exptl/")
              (base32
               "1fnqc63gz8gvdyfz45bx8dxn1r1rwrypahs3bqd2vlc8ff76xp86")
              #:trivial? #t))
    (propagated-inputs
     `(("texlive-hyphen-base" ,texlive-hyphen-base)
       ("texlive-hyph-utf8" ,texlive-hyph-utf8)))
    (home-page "http://projekte.dante.de/Trennmuster/WebHome")
    (synopsis "Hyphenation patterns for German")
    (description "The package provides experimental hyphenation patterns for
the German language, covering both traditional and reformed orthography.  The
patterns can be used with packages Babel and hyphsubst from the Oberdiek
bundle.")
    ;; Hyphenation patterns are under the Expat license; documentation is
    ;; under LPPL.
    (license (list license:expat license:lppl))))

(define-public texlive-generic-dehyph-exptl
  (deprecated-package "texlive-generic-dehyph-exptl" texlive-dehyph-exptl))

(define-public texlive-ukrhyph
  (package
    (inherit (simple-texlive-package
              "texlive-ukrhyph"
              (list "/doc/generic/ukrhyph/"
                    "/tex/generic/ukrhyph/")
              (base32
               "01ma274sixcrbpb7fpqkxwfvrnzfj2srv9b4a42rfnph1pdql74z")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/ukrhyph")
    (synopsis "Hyphenation patterns for Ukrainian")
    (description "The package provides a range of hyphenation patterns for
Ukrainian, depending on the encoding of the output font including the standard
T2A.")
    (license license:lppl)))

(define-public texlive-ruhyphen
  (let ((template (simple-texlive-package
                   "texlive-ruhyphen"
                   (list "/source/generic/ruhyphen/"
                         "/tex/generic/ruhyphen/")
                   (base32
                    "18n1bqhh8jv765vz3a3fjwffy7m71vhwx9yq8zl0p5j7p72q9qcn")
                   #:trivial? #t)))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'build
               (lambda _
                 (let ((cwd (getcwd)))
                   ;; Remove generated files.
                   (for-each delete-file
                             (find-files "tex/generic/ruhyphen/"
                                         "^cyry.*.tex$"))
                   (substitute* "source/generic/ruhyphen/Makefile"
                     (("./mkcyryo") (string-append cwd "/source/generic/ruhyphen/mkcyryo")))
                   (with-directory-excursion "tex/generic/ruhyphen"
                     (invoke "make" "-f"
                             (string-append cwd "/source/generic/ruhyphen/Makefile"))))))))))
      (native-inputs
       `(("coreutils" ,coreutils)
         ("gawk" ,gawk)
         ("sed" ,sed)
         ("grep" ,grep)
         ("perl" ,perl)))
      (home-page "https://www.ctan.org/pkg/ruhyphen")
      (synopsis "Hyphenation patterns for Russian")
      (description "The package provides a collection of Russian hyphenation
patterns supporting a number of Cyrillic font encodings, including T2,
UCY (Omega Unicode Cyrillic), LCY, LWN (OT2), and koi8-r.")
      (license license:lppl))))

(define-public texlive-kpathsea
  (package
    (inherit (simple-texlive-package
              "texlive-kpathsea"
              (list "/web2c/amiga-pl.tcx"
                    "/web2c/cp1250cs.tcx"
                    "/web2c/cp1250pl.tcx"
                    "/web2c/cp1250t1.tcx"
                    "/web2c/cp227.tcx"
                    "/web2c/cp852-cs.tcx"
                    "/web2c/cp852-pl.tcx"
                    "/web2c/cp8bit.tcx"
                    "/web2c/empty.tcx"
                    "/web2c/fmtutil.cnf"
                    "/web2c/il1-t1.tcx"
                    "/web2c/il2-cs.tcx"
                    "/web2c/il2-pl.tcx"
                    "/web2c/il2-t1.tcx"
                    "/web2c/kam-cs.tcx"
                    "/web2c/kam-t1.tcx"
                    "/web2c/macce-pl.tcx"
                    "/web2c/macce-t1.tcx"
                    "/web2c/maz-pl.tcx"
                    "/web2c/mktex.cnf"
                    "/web2c/mktex.opt"
                    "/web2c/mktexdir"
                    "/web2c/mktexdir.opt"
                    "/web2c/mktexnam"
                    "/web2c/mktexnam.opt"
                    "/web2c/mktexupd"
                    "/web2c/natural.tcx"
                    "/web2c/tcvn-t5.tcx"
                    "/web2c/viscii-t5.tcx")
              (base32
               "191i8n3g46p53bb9dkx2ggwpzy7skgg0pbklsrpx8x4ayd86wcaf")
              #:trivial? #t))
    (home-page "https://www.tug.org/texlive/")
    (synopsis "Files related to the path searching library for TeX")
    (description "Kpathsea is a library and utility programs which provide
path searching facilities for TeX file types, including the self-locating
feature required for movable installations, layered on top of a general search
mechanism.  This package provides supporting files.")
    (license license:lgpl3+)))

(define-public texlive-latexconfig
  (package
    (inherit (simple-texlive-package
              "texlive-latexconfig"
              (list "/tex/latex/latexconfig/")
              (base32
               "1wa7yhdpnz1nyidwgli68fyr33jn951bnniqrih5lj98k09rqc3h")
              #:trivial? #t))
    (home-page "https://www.tug.org/")
    (synopsis "Configuration files for LaTeX-related formats")
    (description "The package provides configuration files for LaTeX-related
formats.")
    (license license:lppl)))

(define-public texlive-latex-base
  (let ((template (simple-texlive-package
                   "texlive-latex-base"
                   (list "/doc/latex/base/"
                         "/source/latex/base/"
                         ;; Almost all files in /tex/latex/base are generated, but
                         ;; these are not:
                         "/tex/latex/base/idx.tex"
                         "/tex/latex/base/lablst.tex"
                         "/tex/latex/base/ltnews.cls"
                         "/tex/latex/base/ltxcheck.tex"
                         "/tex/latex/base/ltxguide.cls"
                         "/tex/latex/base/minimal.cls"
                         "/tex/latex/base/sample2e.tex"
                         "/tex/latex/base/small2e.tex"
                         "/tex/latex/base/testpage.tex"
                         "/tex/latex/base/texsys.cfg")
                   (base32
                    "0m0gjb4hbsf2iqkkx3px4f28r2scjvsjv4zb2whkbnb44apyw1f0")
                   #:trivial? #t)))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:modules modules '())
          '((guix build gnu-build-system)
            (guix build utils)
            (ice-9 match)
            (srfi srfi-26)))
         ((#:phases phases)
          `(modify-phases ,phases
             ;; The literal tab in the dtx file is translated to the string
             ;; "^^I" in the generated Lua file, which causes a syntax error.
             (add-after 'unpack 'fix-lua-sources
               (lambda _
                 (substitute* "source/latex/base/ltluatex.dtx"
                   (("	") "  "))
                 #t))
             (replace 'build
               (lambda* (#:key inputs #:allow-other-keys)
                 ;; Find required fonts
                 (setenv "TFMFONTS"
                         (string-join
                          (map (match-lambda
                                 ((pkg-name . dir)
                                  (string-append
                                   (assoc-ref inputs pkg-name)
                                   "/share/texmf-dist/fonts/tfm/public"
                                   dir)))
                               '(("texlive-etex" . "/etex")
                                 ("texlive-cm" . "/cm")
                                 ("texlive-fonts-latex" . "/latex-fonts")
                                 ("texlive-fonts-knuth-lib" . "/knuth-lib")))
                          ":"))
                 (let ((cwd (getcwd)))
                   (setenv "TEXINPUTS"
                           (string-append
                            cwd "//:"
                            cwd "/source/latex/base//:"
                            cwd "/build:"
                            (string-join
                             (map (match-lambda ((_ . dir) dir)) inputs)
                             "//:"))))

                 ;; This is the actual build step.
                 (mkdir "build")
                 (invoke "tex" "-ini" "-interaction=scrollmode"
                         "-output-directory=build" "unpack.ins")

                 ;; XXX: We can't build all formats at this point, nor are they
                 ;; part of the LaTeX base, so we disable them.  Actually, we
                 ;; should be running this all in a profile hook, so that only
                 ;; selected formats and hyphenation patterns are included, but it
                 ;; takes long and TeX Live isn't designed to be modular like
                 ;; that.  Everything operates on a shared directory, which we
                 ;; would only have at profile generation time.
                 (let ((disabled-formats
                        '("aleph aleph" "lamed aleph" "uptex uptex" "euptex euptex"
                          "eptex eptex" "ptex ptex" "pdfxmltex pdftex" "platex eptex"
                          "csplain pdftex" "mf mf-nowin" "mex pdftex" "pdfmex pdftex"
                          "luacsplain luatex"
                          ,@(if (string-prefix? "powerpc64le"
                                                (or (%current-target-system)
                                                    (%current-system)))
                              '("luajittex") '())
                          "cont-en xetex" "cont-en pdftex" "pdfcsplain xetex"
                          "pdfcsplain pdftex" "pdfcsplain luatex" "cslatex pdftex"
                          "mptopdf pdftex" "uplatex euptex" "jadetex pdftex"
                          "amstex pdftex" "pdfcslatex pdftex" "lollipop tex"
                          "xmltex pdftex" "pdfjadetex pdftex" "eplain pdftex"
                          "texsis pdftex" "mltex pdftex" "utf8mex pdftex")))
                   (mkdir "web2c")
                   (install-file (string-append
                                  (assoc-ref inputs "texlive-kpathsea")
                                  "/share/texmf-dist/web2c/fmtutil.cnf")
                                 "web2c")
                   (make-file-writable "web2c/fmtutil.cnf")
                   (substitute* "web2c/fmtutil.cnf"
                     (((string-append "^(" (string-join disabled-formats "|") ")") m)
                      (string-append "#! " m))))
                 (invoke "fmtutil-sys" "--all"
                         "--fmtdir=web2c"
                         (string-append "--cnffile=web2c/fmtutil.cnf"))
                 ;; We don't actually want to install it.
                 (delete-file "web2c/fmtutil.cnf")
                 #t))
             (add-after 'install 'install-more
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (let* ((out (assoc-ref outputs "out"))
                        (root (string-append out "/share/texmf-dist"))
                        (target (string-append root "/tex/latex/base"))
                        (web2c (string-append root "/web2c"))
                        (makeindex (string-append root "/makeindex/latex")))
                   (for-each delete-file (find-files "." "\\.(log|aux)$"))

                   ;; The usedir directive in docstrip.ins is ignored, so these
                   ;; two files end up in the wrong place.  Move them.
                   (mkdir-p makeindex)
                   (for-each (lambda (file)
                               (install-file file makeindex)
                               (delete-file file))
                             '("build/gglo.ist"
                               "build/gind.ist"))
                   (for-each (cut install-file <> target)
                             (find-files "build" ".*"))
                   (for-each (cut install-file <> web2c)
                             (find-files "web2c" ".*"))
                   #t)))))))
      (native-inputs
       `(("texlive-bin" ,texlive-bin)
         ("texlive-tex-ini-files" ,texlive-tex-ini-files)
         ("texlive-tex-plain" ,texlive-tex-plain)
         ("texlive-kpathsea" ,texlive-kpathsea)
         ("texlive-cm" ,texlive-cm)
         ("texlive-fonts-latex" ,texlive-fonts-latex)
         ("texlive-fonts-knuth-lib" ,texlive-fonts-knuth-lib)
         ("texlive-luatexconfig"
          ,(texlive-origin
            "texlive-luatexconfig" (number->string %texlive-revision)
            (list "/tex/generic/config/luatex-unicode-letters.tex"
                  "/tex/generic/config/luatexiniconfig.tex"
                  "/web2c/texmfcnf.lua")
            (base32
             "1gi87wy12r8w8fhx9ajcid382dmqzf6b9070b5nndvbbjrvhwf23")))))
      (propagated-inputs
       `(("texlive-dehyph-exptl" ,texlive-dehyph-exptl)
         ("texlive-etex" ,texlive-etex)
         ("texlive-hyph-utf8" ,texlive-hyph-utf8)
         ("texlive-hyphen-base" ,texlive-hyphen-base)
         ("texlive-hyphen-afrikaans" ,texlive-hyphen-afrikaans)
         ("texlive-hyphen-ancientgreek" ,texlive-hyphen-ancientgreek)
         ("texlive-hyphen-armenian" ,texlive-hyphen-armenian)
         ("texlive-hyphen-basque" ,texlive-hyphen-basque)
         ("texlive-hyphen-belarusian" ,texlive-hyphen-belarusian)
         ("texlive-hyphen-bulgarian" ,texlive-hyphen-bulgarian)
         ("texlive-hyphen-catalan" ,texlive-hyphen-catalan)
         ("texlive-hyphen-chinese" ,texlive-hyphen-chinese)
         ("texlive-hyphen-churchslavonic" ,texlive-hyphen-churchslavonic)
         ("texlive-hyphen-coptic" ,texlive-hyphen-coptic)
         ("texlive-hyphen-croatian" ,texlive-hyphen-croatian)
         ("texlive-hyphen-czech" ,texlive-hyphen-czech)
         ("texlive-hyphen-danish" ,texlive-hyphen-danish)
         ("texlive-hyphen-dutch" ,texlive-hyphen-dutch)
         ("texlive-hyphen-english" ,texlive-hyphen-english)
         ("texlive-hyphen-esperanto" ,texlive-hyphen-esperanto)
         ("texlive-hyphen-estonian" ,texlive-hyphen-estonian)
         ("texlive-hyphen-ethiopic" ,texlive-hyphen-ethiopic)
         ("texlive-hyphen-finnish" ,texlive-hyphen-finnish)
         ("texlive-hyphen-french" ,texlive-hyphen-french)
         ("texlive-hyphen-friulan" ,texlive-hyphen-friulan)
         ("texlive-hyphen-galician" ,texlive-hyphen-galician)
         ("texlive-hyphen-georgian" ,texlive-hyphen-georgian)
         ("texlive-hyphen-german" ,texlive-hyphen-german)
         ("texlive-hyphen-greek" ,texlive-hyphen-greek)
         ("texlive-hyphen-hungarian" ,texlive-hyphen-hungarian)
         ("texlive-hyphen-icelandic" ,texlive-hyphen-icelandic)
         ("texlive-hyphen-indic" ,texlive-hyphen-indic)
         ("texlive-hyphen-indonesian" ,texlive-hyphen-indonesian)
         ("texlive-hyphen-interlingua" ,texlive-hyphen-interlingua)
         ("texlive-hyphen-irish" ,texlive-hyphen-irish)
         ("texlive-hyphen-italian" ,texlive-hyphen-italian)
         ("texlive-hyphen-kurmanji" ,texlive-hyphen-kurmanji)
         ("texlive-hyphen-latin" ,texlive-hyphen-latin)
         ("texlive-hyphen-latvian" ,texlive-hyphen-latvian)
         ("texlive-hyphen-lithuanian" ,texlive-hyphen-lithuanian)
         ("texlive-hyphen-mongolian" ,texlive-hyphen-mongolian)
         ("texlive-hyphen-norwegian" ,texlive-hyphen-norwegian)
         ("texlive-hyphen-occitan" ,texlive-hyphen-occitan)
         ("texlive-hyphen-pali" ,texlive-hyphen-pali)
         ("texlive-hyphen-piedmontese" ,texlive-hyphen-piedmontese)
         ("texlive-hyphen-polish" ,texlive-hyphen-polish)
         ("texlive-hyphen-portuguese" ,texlive-hyphen-portuguese)
         ("texlive-hyphen-romanian" ,texlive-hyphen-romanian)
         ("texlive-hyphen-romansh" ,texlive-hyphen-romansh)
         ("texlive-hyphen-russian" ,texlive-hyphen-russian)
         ("texlive-hyphen-sanskrit" ,texlive-hyphen-sanskrit)
         ("texlive-hyphen-serbian" ,texlive-hyphen-serbian)
         ("texlive-hyphen-slovak" ,texlive-hyphen-slovak)
         ("texlive-hyphen-slovenian" ,texlive-hyphen-slovenian)
         ("texlive-hyphen-spanish" ,texlive-hyphen-spanish)
         ("texlive-hyphen-swedish" ,texlive-hyphen-swedish)
         ("texlive-hyphen-thai" ,texlive-hyphen-thai)
         ("texlive-hyphen-turkish" ,texlive-hyphen-turkish)
         ("texlive-hyphen-turkmen" ,texlive-hyphen-turkmen)
         ("texlive-hyphen-ukrainian" ,texlive-hyphen-ukrainian)
         ("texlive-hyphen-uppersorbian" ,texlive-hyphen-uppersorbian)
         ("texlive-hyphen-welsh" ,texlive-hyphen-welsh)
         ("texlive-unicode-data" ,texlive-unicode-data)
         ("texlive-ukrhyph" ,texlive-ukrhyph)
         ("texlive-ruhyphen" ,texlive-ruhyphen)
         ("texlive-latexconfig" ,texlive-latexconfig)))
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
                "1h78zw0vhldx478zs4v86ajg7vpkysd1kg3npc480qqls3q6ba40"))))
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

(define-public texlive-epsf
  (package
    (inherit (simple-texlive-package
              "texlive-epsf"
              (list "/doc/generic/epsf/"
                    "/tex/generic/epsf/")
              (base32
               "03jcf0kqh47is965d2590miwj7d5kif3c4mgsnvkyl664jzjkh92")
              #:trivial? #t))
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

(define-public texlive-generic-epsf
  (deprecated-package "texlive-generic-epsf" texlive-epsf))

(define-public texlive-latex-fancyvrb
  (package
    (inherit (simple-texlive-package
              "texlive-latex-fancyvrb"
              (list "/doc/latex/fancyvrb/README"
                    "/tex/latex/fancyvrb/")
              (base32
               "1dwkcradz9nwpjwmv1sjzn77lvw25ypr0rrgmf1kd8pd2mw7dxcn")
              #:trivial? #t))
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

(define-public texlive-graphics-def
  (package
    (inherit (simple-texlive-package
              "texlive-graphics-def"
              (list "/doc/latex/graphics-def/README.md"
                    "/tex/latex/graphics-def/")
              (base32
               "0zrbn9cwfnnrrl3b2zsd74ldksp9jwpvjh7z93ild1m75crpb39a")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/latex-graphics")
    (synopsis "Color and graphics option files")
    (description
     "This bundle is a combined distribution consisting of @file{dvips.def},
@file{pdftex.def}, @file{luatex.def}, @file{xetex.def}, @file{dvipdfmx.def},
and @file{dvisvgm.def} driver option files for the LaTeX graphics and color
packages.")
    (license license:lppl1.3c+)))

(define-public texlive-graphics-cfg
  (package
    (inherit (simple-texlive-package
              "texlive-graphics-cfg"
              (list "/doc/latex/graphics-cfg/README.md"
                    "/tex/latex/graphics-cfg/")
              (base32
               "00n63adb2laf43lzix39xl68aq0k5k80mmrw602w99w5n7f96gsf")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/latex-graphics")
    (synopsis "Sample configuration files for LaTeX color and graphics")
    (description
     "This bundle includes @file{color.cfg} and @file{graphics.cfg} files that
set default \"driver\" options for the color and graphics packages.")
    (license license:public-domain)))

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
    (arguments '(#:tex-directory "latex/graphics"))
    (propagated-inputs
     `(("texlive-graphics-cfg" ,texlive-graphics-cfg)
       ("texlive-graphics-def" ,texlive-graphics-def)))
    (home-page "https://www.ctan.org/pkg/latex-graphics")
    (synopsis "LaTeX standard graphics bundle")
    (description
     "This is a collection of LaTeX packages for producing color, including
graphics (e.g. PostScript) files, and rotation and scaling of text in LaTeX
documents.  It comprises the packages color, graphics, graphicx, trig, epsfig,
keyval, and lscape.")
    (license license:lppl1.3c+)))

(define-public texlive-xcolor
  (let ((template (simple-texlive-package
                   "texlive-xcolor"
                   (list "/doc/latex/xcolor/"
                         "/source/latex/xcolor/")
                   (base32
                    "12q6spmpxg30alhvarjmxzigmz7lazapbrb0mc4vhbn6n1sdz7pp"))))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:tex-directory _ #t)
          "latex/xcolor")
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'chdir
               (lambda _ (chdir "source/latex/xcolor") #t))
             (add-after 'install 'move-files
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((share (string-append (assoc-ref outputs "out")
                                             "/share/texmf-dist")))
                   (mkdir-p (string-append share "/dvips/xcolor"))
                   (rename-file (string-append share "/tex/latex/xcolor/xcolor.pro")
                                (string-append share "/dvips/xcolor/xcolor.pro"))
                   #t)))))))
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
      (license license:lppl1.2+))))

(define-public texlive-latex-xcolor
  (deprecated-package "texlive-latex-xcolor" texlive-xcolor))

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
                "0pi2d6gsddcs9wprdbar46s91kdc5fxl1m79g7xrbccsx8s9xbml"))))
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
                "1wqvn4z0s92h5iqzrvxw7hinzp95avjk9v8lnqbqr4kz6nv4xb9l"))))
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

(define-public texlive-url
  (package
    (inherit (simple-texlive-package
              "texlive-url"
              (list "/doc/latex/url/"
                    "/tex/latex/url/")
              (base32
               "184m40wgnx939ky2hbxnj0v9aak023ldrhgffp0lgyk9wdqpxlqg")
              #:trivial? #t))
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

(define-public texlive-latex-url
  (deprecated-package "texlive-latex-url" texlive-url))

(define-public texlive-tetex
  (package
    (inherit (simple-texlive-package
              "texlive-tetex"
              (list "/dvips/tetex/"
                    "/fonts/enc/dvips/tetex/"
                    "/fonts/map/dvips/tetex/")
              (base32
               "1si3as8mwi8837965djlw6jhwwzsp3r1hkflvdxv2avx9vb45hjb")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/tetex")
    (synopsis "Font maps originally from teTeX")
    (description "This package provides font maps that were originally part of
the now obsolete teTeX distributions but are still used at the core of the TeX
Live distribution.")
    (license license:public-domain)))

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
                "0s77z2cbv841l45qrpf0s8qhzfa4wi689lg7zkw88qg18nzvy0ly"))))
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
                "1cv4fk9pabh7mkxhfsdmh4k8xpmzg1psgcsvd11c869m7n3a629h"))))
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
                "1223cw029n6zff7pqpwbsq1x8v3w63smczkmnybqxkw5h2za8gbz"))))
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
                    (url "https://github.com/lualatex/luaotfload")
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
                "0qgk2332dacsxn1z95qzp35gbs7wrzl1ipjdhnmk1r897msm4sf5"))))
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
                "1rwqq841i1rxywymzwkw0cw2yhgvxwjx5mgygfasvypwrwll6f6s"))))
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
                "129f9w41cb6yyrr6kpv3zz9ml6334hyq1wcz7j9jn47p0hlxqfk8"))))
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
                texlive-cm
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
                texlive-latex-tools
                texlive-tetex)))
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
                          (ice-9 popen)
                          (srfi srfi-26)
                          (guix build union)
                          (guix build utils)
                          (guix build texlive-build-system))
             (let* ((out       (assoc-ref %outputs "out"))
                    (texmf.cnf (string-append out "/share/texmf-dist/web2c/texmf.cnf")))
               ;; Build a modifiable union of all inputs (but exclude bash and
               ;; the updmap.cfg file)
               (match (filter (match-lambda
                                ((name . _)
                                 (not (member name '("bash"
                                                     "coreutils"
                                                     "sed"
                                                     "updmap.cfg")))))
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
               (setenv "PATH" (string-append
                               (assoc-ref %build-inputs "bash") "/bin:"
                               (assoc-ref %build-inputs "coreutils") "/bin:"
                               (assoc-ref %build-inputs "sed") "/bin:"
                               (string-append out "/bin")))
               (for-each
                (cut wrap-program <>
                     `("TEXMFCNF" ":" suffix (,(dirname texmf.cnf)))
                     `("TEXMF"    ":" suffix (,(string-append out "/share/texmf-dist"))))
                (find-files (string-append out "/bin") ".*"))

               ;; Remove invalid maps from config file.
               (let ((web2c (string-append out "/share/texmf-config/web2c/"))
                     (maproot (string-append out "/share/texmf-dist/fonts/map/")))
                 (mkdir-p web2c)
                 (copy-file
                  (assoc-ref %build-inputs "updmap.cfg")
                  (string-append web2c "updmap.cfg"))
                 (make-file-writable (string-append web2c "updmap.cfg"))

                 (let* ((port (open-pipe* OPEN_WRITE "updmap-sys"
                                          "--syncwithtrees"
                                          "--nohash"
                                          (string-append "--cnffile=" web2c "updmap.cfg"))))
                   (display "Y\n" port)
                   (when (not (zero? (status:exit-val (close-pipe port))))
                     (error "failed to filter updmap.cfg")))
                 ;; Generate maps.
                 (invoke "updmap-sys"
                         (string-append "--cnffile=" web2c "updmap.cfg")
                         (string-append "--dvipdfmxoutputdir="
                                        maproot "dvipdfmx/updmap/")
                         (string-append "--dvipsoutputdir="
                                        maproot "dvips/updmap/")
                         (string-append "--pdftexoutputdir="
                                        maproot "pdftex/updmap/"))
                 ;; Having this file breaks all file lookups later.
                 (delete-file (string-append out "/share/texmf-dist/ls-R")))
               #t))))
        (inputs
         `(("bash" ,bash)
           ,@(map (lambda (package)
                    (list (package-name package) package))
                  (append default-packages packages))))
        (native-inputs
         `(("coreutils" ,coreutils)
           ("sed" ,sed)
           ("updmap.cfg"
            ,(origin
               (method url-fetch)
               (uri (string-append "https://tug.org/svn/texlive/tags/"
                                   %texlive-tag "/Master/texmf-dist/web2c/updmap.cfg"
                                   "?revision=" (number->string %texlive-revision)))
               (file-name (string-append "updmap.cfg-"
                                         (number->string %texlive-revision)))
               (sha256
                (base32
                 "0faqknqxs80qp9ywk0by5k85s0yalg97c4lja4q56lsyblrr4j7i"))))))
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

(define-public texlive-tipa
  (package
    (inherit (simple-texlive-package
              "texlive-tipa"
              (list "/tex4ht/ht-fonts/alias/tipa/"
                    "/doc/fonts/tipa/"
                    "/fonts/map/dvips/tipa/"
                    "/fonts/source/public/tipa/"
                    "/fonts/tfm/public/tipa/"
                    "/fonts/type1/public/tipa/"
                    "/tex/latex/tipa/")
              (base32
               "1h53dscv8z6fsa0ax5dys3x11b2f1g60l22hpdf7c88cvdhcs8sn")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/tipa")
    (synopsis "Fonts and macros for IPA phonetics characters")
    (description "These fonts are considered the \"ultimate answer\" to IPA
typesetting.  The encoding of these 8-bit fonts has been registered as LaTeX
standard encoding T3, and the set of addendum symbols as encoding
TS3. \"Times-like\" Adobe Type 1 versions are provided for both the T3 and the
TS3 fonts.")
    (license license:lppl)))

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

(define-public texlive-filemod
  (package
    (inherit (simple-texlive-package
              "texlive-filemod"
              (list "/doc/latex/filemod/"
                    "/tex/latex/filemod/"
                    "/tex/generic/filemod/")
              (base32
               "1snsj7kblkj1ig3x3845lsypz7ab04lf0dcpdh946xakgjnz4fb5")
              #:trivial? #t))
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

(define-public texlive-latex-filemod
  (deprecated-package "texlive-latex-filemod" texlive-filemod))

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

(define-public texlive-pstool
  (package
    (inherit (simple-texlive-package
              "texlive-pstool"
              (list "/doc/latex/pstool/"
                    "/tex/latex/pstool/")
              (base32
               "12clzcw2cl7g2chr2phgmmiwxw4859cln1gbx1wgp8bl9iw590nc")
              #:trivial? #t))
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

(define-public texlive-latex-pstool
  (deprecated-package "texlive-latex-pstool" texlive-pstool))

(define-public texlive-seminar
  (package
    (inherit (simple-texlive-package
              "texlive-seminar"
              (list "/doc/latex/seminar/"
                    "/tex/latex/seminar/")
              (base32
               "1clgw5xy867khzfn8d210rc5hsw5s7r0pznhk84niybvw4zc7r3f")
              #:trivial? #t))
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

(define-public texlive-latex-seminar
  (deprecated-package "texlive-latex-seminar" texlive-seminar))

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

(define-public texlive-doi
  (package
    (inherit (simple-texlive-package
              "texlive-doi"
              (list "/doc/latex/doi/README.md"
                    "/tex/latex/doi/")
              (base32
               "18z9922lqb3hliqn95h883fndqs4lgyi5yqbnq2932ya0imc3j7h")
              #:trivial? #t))
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

(define-public texlive-latex-doi
  (deprecated-package "texlive-latex-doi" texlive-doi))

(define-public texlive-etoolbox
  (package
    (inherit (simple-texlive-package
              "texlive-etoolbox"
              (list "/doc/latex/etoolbox/"
                    "/tex/latex/etoolbox/")
              (base32
               "1cc1vw1ach55g4ff4x30by8k1mg01w199ccxvn72f5khlnnxial0")
              #:trivial? #t))
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

(define-public texlive-latex-etoolbox
  (deprecated-package "texlive-latex-etoolbox" texlive-etoolbox))

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
                "1br4kv9y17cvngp83ykpvy7gy3jqfan5plk7sggcgbdfhndi5dsr"))))
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
                "0a8f38c2ds1flxcr0apdpyaaz3k6fyalz6dkbrmcv9srjc40mh3n"))))
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
                "0na7v4hsyx5s67cpjj2dbnq8j67k8lln6b19hmj631gfs27slss1"))))
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
                "16jy02m089m7n6v9vbfi4xjgngc1fnvsmmppk8axfwzbhdky3c9c"))))
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
package uses @code{drivers} to place the bars; the available drivers can work
with @code{dvitoln03}, @code{dvitops}, @code{dvips}, the emTeX and TeXtures DVI
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
                "16vd99p01a0y30xr5yf1z2j5da9x8gy21vb30wk08jh31zffbaqj"))))
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
                "0hrwspqkqfahxyzzsnjyrxlgxj06zw1f3636gx76pvl4xhvdj1cj"))))
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
                "1fbrhqj22vzakn30j71fc41l8nliqbv1dmxm0zlwi2qjjbq6fwav"))))
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
                                      (assoc-ref inputs "texlive-cm")
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
       ("texlive-cm" ,texlive-cm)))
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
texlive-2019.3/Master/texmf-dist/doc/fonts/ec/copyrite.txt"))))

;; FIXME: the fonts should be built from source, but running "tex aefonts.tex"
;; fails with obscure TeX-typical error messages.
(define-public texlive-ae
  (package
    (inherit (simple-texlive-package
              "texlive-ae"
              (list "/doc/fonts/ae/"
                    "/source/fonts/ae/"
                    "/fonts/tfm/public/ae/"
                    "/fonts/vf/public/ae/"
                    "/tex/latex/ae/")
              (base32
               "1xkzg381y0avdq381r2m990wp27czkdff0qkvsp2n5q62yc0bdsw")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/ae")
    (synopsis "Virtual fonts for T1 encoded CMR-fonts")
    (description
     "This package provides a set of virtual fonts which emulates T1 coded
fonts using the standard CM fonts.  The package name, AE fonts, supposedly
stands for \"Almost European\".  The main use of the package was to produce
PDF files using Adobe Type 1 versions of the CM fonts instead of bitmapped EC
fonts.  Note that direct substitutes for the bitmapped EC fonts are available,
via the CM-super, Latin Modern and (in a restricted way) CM-LGC font sets.")
    (license license:lppl1.3+)))

(define-public texlive-inconsolata
  (package
    (inherit (simple-texlive-package
              "texlive-inconsolata"
              (list "/doc/fonts/inconsolata/"
                    "/fonts/enc/dvips/inconsolata/"
                    "/fonts/map/dvips/inconsolata/"
                    "/fonts/opentype/public/inconsolata/"
                    "/fonts/tfm/public/inconsolata/"
                    "/fonts/type1/public/inconsolata/"
                    "/tex/latex/inconsolata/")
              (base32
               "1a77w26m4c4j0202s1qkikz7ha6cxlv8zxhzi9s3l0x1l2pl7cr2")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/inconsolata")
    (synopsis "Monospaced font with support files for use with TeX")
    (description
     "Inconsolata is a monospaced font designed by Raph Levien.  This package
contains the font (in both Adobe Type 1 and OpenType formats) in regular and
bold weights, with additional glyphs and options to control slashed zero,
upright quotes and a shapelier lower-case L, plus metric files for use with
TeX, and LaTeX font definition and other relevant files.")
    (license (list license:lppl1.3+
                   license:silofl1.1
                   license:asl2.0))))

(define-public texlive-times
  (package
    (inherit (simple-texlive-package
              "texlive-times"
              (list "/dvips/times/"
                    "/fonts/afm/adobe/times/"
                    "/fonts/afm/urw/times/"
                    "/fonts/tfm/adobe/times/"
                    "/fonts/tfm/urw35vf/times/"
                    "/fonts/type1/urw/times/"
                    "/fonts/vf/adobe/times/"
                    "/fonts/vf/urw35vf/times/"
                    "/fonts/map/dvips/times/"
                    "/tex/latex/times/")
              (base32
               "13g41a7vbkvsf7ki9dgl7qm100w382mnlqkcngwgl3axp6s5s8l0")
              #:trivial? #t))
    (home-page "https://ctan.org/pkg/urw-base35")
    (synopsis "URW Base 35 font pack for LaTeX")
    (description
     "This package provides a drop-in replacements for the Times font from
Adobe's basic set.")
    ;; No license version specified.
    (license license:gpl3+)))

(define-public texlive-fonts-adobe-times
  (deprecated-package "texlive-fonts-adobe-times" texlive-times))

(define-public texlive-palatino
  (package
    (inherit (simple-texlive-package
              "texlive-palatino"
              (list "/dvips/palatino/"
                    "/fonts/afm/adobe/palatino/"
                    "/fonts/afm/urw/palatino/"
                    "/fonts/tfm/adobe/palatino/"
                    "/fonts/tfm/urw35vf/palatino/"
                    "/fonts/type1/urw/palatino/"
                    "/fonts/vf/adobe/palatino/"
                    "/fonts/vf/urw35vf/palatino/"

                    "/fonts/map/dvips/palatino/"
                    "/tex/latex/palatino/")
              (base32
               "12jc0av7v99857jigmva47qaxyllhpzsnqis10n0qya2kz44xf22")
              #:trivial? #t))
    (home-page "https://ctan.org/pkg/urw-base35")
    (synopsis "URW Base 35 font pack for LaTeX")
    (description
     "This package provides a drop-in replacements for the Palatino font from
Adobe's basic set.")
    ;; No license version specified.
    (license license:gpl3+)))

(define-public texlive-fonts-adobe-palatino
  (deprecated-package "texlive-fonts-adobe-palatino" texlive-palatino))

(define-public texlive-zapfding
  (package
    (inherit (simple-texlive-package
              "texlive-zapfding"
              (list "/dvips/zapfding/"
                    "/fonts/afm/adobe/zapfding/"
                    "/fonts/afm/urw/zapfding/"
                    "/fonts/tfm/adobe/zapfding/"
                    "/fonts/tfm/urw35vf/zapfding/"
                    "/fonts/type1/urw/zapfding/"
                    "/fonts/map/dvips/zapfding/"
                    "/tex/latex/zapfding/")
              (base32
               "17mls8wilz9api9ivsbcczpiqp1f39qy8wa6ajssi8zhnc5lq7zn")
              #:trivial? #t))
    (home-page "https://ctan.org/pkg/urw-base35")
    (synopsis "URW Base 35 font pack for LaTeX")
    (description
     "This package provides a drop-in replacements for the Zapfding font from
Adobe's basic set.")
    ;; No license version specified.
    (license license:gpl3+)))

(define-public texlive-fonts-adobe-zapfding
  (deprecated-package "texlive-fonts-adobe-zapfding" texlive-zapfding))

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
                                      (assoc-ref inputs "texlive-cm")
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
       ("texlive-cm" ,texlive-cm)))
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
                "0y2y08kr3w6asm9lblj9yywqmhaal36fq71zzcbfsc7cvwf641q7"))))
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
                "1vm9xp67hzif0pqab4r3ialf0cyhi0fa4p8kxgp1ymcf85pqip14"))))
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
                "07vbcp6avdwldr870cwf65av2s9lfyzcpp8gpld53yw6lcxgaipj"))))
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
                "1pr6ym3ad7x14ng7gmhsmywh3685d2cnm5qgyrqbigng2r6fcc1k"))))
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
                "0j1fhm1m9k6rz80lmch3x44g20y9nm4abaaf8czb0q8hzwlx5aq5"))))
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

(define-public texlive-txfonts
  (package
    (inherit (simple-texlive-package
              "texlive-txfonts"
              (list "/doc/fonts/txfonts/"

                    "/fonts/afm/public/txfonts/"
                    "/fonts/tfm/public/txfonts/"
                    "/fonts/type1/public/txfonts/"
                    "/fonts/vf/public/txfonts/"

                    "/fonts/map/dvips/txfonts/"
                    "/fonts/enc/dvips/txfonts/"
                    "/tex/latex/txfonts/")
              (base32
               "017zjas5y1zlyq0iy4x6mv1qbz23xcy3y5xs0crj6zdnfvnccqgp")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/txfonts")
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

(define-public texlive-fonts-txfonts
  (deprecated-package "texlive-fonts-txfonts" texlive-txfonts))

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
typesetting of newspapers and similar periodicals.  The Iwona fonts are an
alternative version of the Kurier fonts.  The difference lies in the absence
of ink traps which typify the Kurier font.")
    (license license:gfl1.0)))

(define-public texlive-jknappen
  (package
    (inherit (simple-texlive-package
              "texlive-jknappen"
              (list "/fonts/source/jknappen/"
                    "/fonts/tfm/jknappen/"
                    "/fonts/source/jknappen/"
                    "/fonts/tfm/jknappen/"
                    "/tex4ht/ht-fonts/alias/jknappen/"
                    "/tex4ht/ht-fonts/unicode/jknappen/")
              (base32
               "1v5wk5il41bddzr81h5636h3cjz0w1qann5g9garsb7qfch9fhws")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/jknappen")
    (synopsis "Miscellaneous packages by Jörg Knappen")
    (description
     "This package contains a collection of macros by Jörg Knappen:
@table @code
@item greekctr
New counterstyles @code{\\greek} and @code{\\Greek}.
@item holtpolt
Non-commutative fractions
@item latin1jk
@itemx latin2jk
@itemx latin3jk
Inputenc definition files that allow verbatim input in the respective ISO
Latin codes.
@item mathbol
Blackboard bold fonts for use in maths.
@item mathrsfs
Mathematical script letters, as traditionally used in physics for Lagrangian,
Hamiltonian, path integral measures, etc.
@item parboxx
New alignment options for parboxen at top and bottom of the box.
@item sans
Interchanges the roles of sans serif and roman fonts throughout the document.
@item semtrans
Support for special latin letters and diacritics used in transliteration of
semitic languages
@item smartmn
Intelligent hyphen/minus, which guesses whether to render as hyphen or minus.
@item sgmlcmpt
Commands replacing the characters <, >, and &.
@item tccompat
A compatibility package for users of the older versions of the textcomp package.
@item young
Simple Young tableaux.
@end table")
    (license license:gpl2)))                    ;per the 00readme_txt file.

(define-public texlive-libertine
  (package
    (inherit (simple-texlive-package
              "texlive-libertine"
              (list "/doc/fonts/libertine/"

                    "/fonts/enc/dvips/libertine/"
                    "/fonts/map/dvips/libertine/"
                    "/fonts/opentype/public/libertine/"
                    "/fonts/tfm/public/libertine/"
                    "/fonts/type1/public/libertine/"
                    "/fonts/vf/public/libertine/"

                    "/tex/latex/libertine/")
              (base32
               "1v2vimpfi0b08sd79dadrck7wkpa0bphiakwyvhjc3ygq9k5bp8k")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/libertine")
    (synopsis "Use Linux Libertine and Biolinum fonts with LaTeX")
    (description
     "The package provides the Libertine and Biolinum fonts in both Type 1 and
OTF styles, together with support macros for their use.  Monospaced and
display fonts, and the \"keyboard\" set are also included, in OTF style, only.
The @code{mweights} package is used to manage the selection of font weights.
The package supersedes both the @code{libertineotf} and the
@code{libertine-legacy} packages.")
    (license (list license:gpl2+        ; with font exception
                   license:silofl1.1
                   license:lppl))))

(define-public texlive-dejavu
  (package
    (inherit (simple-texlive-package
              "texlive-dejavu"
              (list "/doc/fonts/dejavu/"

                    "/fonts/enc/dvips/dejavu/"
                    "/fonts/map/dvips/dejavu/"

                    "/fonts/afm/public/dejavu/"
                    "/fonts/tfm/public/dejavu/"
                    "/fonts/truetype/public/dejavu/"
                    "/fonts/type1/public/dejavu/"
                    "/fonts/vf/public/dejavu/"

                    "/tex/latex/dejavu/")
              (base32
               "0y4qf5jl0xncah9nkcaalmy69wwq02n3j895zp71n2p0nfi24aka")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/libertine")
    (synopsis "LaTeX support for the DejaVu fonts")
    (description
     "The package contains LaTeX support for the DejaVu fonts, which are
derived from the Vera fonts but contain more characters and styles.  The fonts
are included in the original TrueType format, and in converted Type 1 format.
The (currently) supported encodings are: OT1, T1, IL2, TS1, T2*, X2, QX, and
LGR.  The package doesn't (currently) support mathematics.")
    (license license:lppl)))

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

(define-public texlive-marvosym
  (package
    (inherit (simple-texlive-package
              "texlive-marvosym"
              (list "/doc/fonts/marvosym/"
                    "/fonts/afm/public/marvosym/"
                    "/fonts/map/dvips/marvosym/"
                    "/fonts/tfm/public/marvosym/"
                    "/fonts/truetype/public/marvosym/"
                    "/fonts/type1/public/marvosym/"
                    "/tex/latex/marvosym/")
              (base32
               "0m3bbg06cia8ni86fjhvb7x4a5qcxgnpqcvicfms91w2px9ysc46")
              #:trivial? #t))
    (home-page "https://martinvogel.de/blog/index.php?\
/archives/131-Marvosym.ttf.html")
    (synopsis "Martin Vogel's Symbols (marvosym) font")
    (description "The Martin Vogel’s Symbols fonts (marvosym) contains the
Euro currency symbol as defined by the European commission, along with symbols
for structural engineering, symbols for steel cross-sections, astronomy
signs (sun, moon, planets), the 12 signs of the zodiac, scissor symbols, CE
sign and others.  This package contains both the original TrueType font and
the derived Type 1 font, together with support files for TeX (LaTeX).")
    (license (list license:lppl          ;for TeX support files
                   license:silofl1.1)))) ;for fonts

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
                "1xyd57c8z1xi0kbqpbad61flcazz68i9ssxrag0gjvci3irxi8xh"))))
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
    (version "1.60")
    (source (origin
              (method svn-fetch)
              (uri (texlive-ref "latex" "acmart"))
              (sha256
               (base32
                "18rl67p2zhngskisnhv78mksv8q8q658l6igkswzswldixmkpphq"))
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

(define-public texlive-wasy
  (package
    (inherit (simple-texlive-package
              "texlive-wasy"
              (list "/fonts/source/public/wasy/"
                    "/fonts/tfm/public/wasy/"
                    "/tex/plain/wasy/"
                    "/doc/fonts/wasy/")
              (base32
               "10dxbqgv42niybj65hj6y47x8lsl83m48rgw2v2s50k05wbghwbm")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/wasy")
    (synopsis "Waldi symbol fonts")
    (description "This package provides the @code{wasy} (Waldi symbol) fonts,
in the Metafont and Adobe Type 1 formats.  Support under LaTeX is provided by
the @code{wasysym} package.")
    (license license:public-domain)))

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
    (synopsis "LaTeX support file to use the @code{wasy} fonts")
    (description
     "The @code{wasy} (Waldi Symbol) font by Roland Waldi provides many glyphs like
male and female symbols and astronomical symbols, as well as the complete
@code{lasy} font set and other odds and ends.  The @code{wasysym} package
implements an easy to use interface for these symbols.")
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
             "03rfif2631pgd8g1ar4xblcdh078kky7fvw3kfsj5a47rxxgicp2"))))))
    (home-page "https://www.ctan.org/pkg/pdftex")
    (synopsis "TeX extension for direct creation of PDF")
    (description
     "This package provides an extension of TeX which can be configured to
directly generate PDF documents instead of DVI.")
    (license license:gpl2+)))

(define texlive-texmf
  (package
   (name "texlive-texmf")
   (version "20190410")
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
   (version "20190410")
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
    ;; Note: When updating Biber, make sure it matches our BibLaTeX version by
    ;; checking the Biber/BibLaTeX compatibility matrix in the BibLaTeX manual
    ;; at <https://ctan.org/pkg/biblatex>.
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
              (patches (search-patches "biber-fix-encoding-write.patch"
                                       "biber-sortinithash.patch"))
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
    (version "1.5.1")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "https://launchpad.net/rubber/trunk/"
                                        version "/+download/rubber-"
                                        version ".tar.gz")
                         (string-append "http://ebeffara.free.fr/pub/rubber-"
                                        version ".tar.gz")))
              (sha256
               (base32
                "178dmrp0mza5gqjiqgk6dqs0c10s0c517pk6k9pjbam86vf47a1p"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f ; no `check' target
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; texlive is required to build the PDF documentation; do not
             ;; build it.
             (invoke "python" "setup.py" "build" "--pdf=False" "install"
                     (string-append "--prefix=" (assoc-ref outputs "out"))))))))
    (native-inputs `(("texinfo" ,texinfo)))
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
    (version "5.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.xm1math.net/texmaker/texmaker-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1qnh5g8zkjpjmw2l8spcynpfgs3wpcfcla5ms2kkgvkbdlzspqqx"))))
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
     `(("texlive" ,(texlive-union (list texlive-amsfonts
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
    (version "2.3.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.lyx.org/pub/lyx/stable/"
                                  (version-major+minor version) ".x/"
                                  "lyx-" version ".tar.xz"))
              (sha256
               (base32
                "0y7sx804ral14py5jwmb3icvyd6rsw806dfclw0qx28r6iix5gn6"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "3rdparty")
                  #t))))
    (build-system qt-build-system)
    (arguments
     `(#:configure-flags `("-DLYX_USE_QT=QT5"
                           "-DLYX_EXTERNAL_BOOST=1"
                           "-DLYX_INSTALL=1"
                           "-DLYX_RELEASE=1"
                           "-DLYX_PROGRAM_SUFFIX=OFF"
                           ,(string-append "-DLYX_INSTALL_PREFIX="
                                           (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-python
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("lib/configure.py"
                            "src/support/ForkedCalls.cpp"
                            "src/support/Systemcall.cpp"
                            "src/support/os.cpp"
                            "src/support/filetools.cpp")
               (("\"python ")
                (string-append "\""
                               (assoc-ref inputs "python")
                               "/bin/python3 ")))
             #t))
         (add-after 'unpack 'add-missing-test-file
           (lambda _
             ;; Create missing file that would cause tests to fail.
             (with-output-to-file "src/tests/check_layout.cmake"
               (const #t))
             #t)))))
    (inputs
     `(("boost" ,boost)
       ("hunspell" ,hunspell)           ; Note: Could also use aspell instead.
       ("libx11" ,libx11)
       ("mythes" ,mythes)
       ("python" ,python)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("texlive" ,(texlive-union (list texlive-fonts-ec)))))
    (native-inputs
     `(("python" ,python)
       ("pkg-config" ,pkg-config)))
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
                "0s86v2b6b1vky1svmmn8pn0l2gz3v280mvjbr2d9l2sjyarlgz9w"))))
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
                "1yhp51w8yr10c10pc9196q7hlw80brzqinnqbjw81d0sf2p0llc5"))))
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
       (file-name (string-append name "-" version "-checkout"))
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
         "1dscrgwyr71vgx35mzb316xl669arzagfgq50fdv3nxga63959b3"))))
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
             "0hk5x2j15n4pps279cmkbjl1dvhasq3mbhna5xdvp2qgh635ahks"))))))
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
                "1vz9zg7s5w52xr323zgglzprfrvba2zvyzf6b8vrdf4wdghlpv4z"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
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
                "1x4wnpca97rnbvvg6wjmbkxxvnfva274q9ahzx746b435q93z3i1"))))
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
    (synopsis "Grab items in lists using user-specified separation character")
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

(define-public texlive-xypic
  (let ((template (simple-texlive-package
                   "texlive-xypic"
                   (list "/doc/generic/xypic/"
                         "/dvips/xypic/xy389dict.pro"
                         "/fonts/enc/dvips/xypic/"
                         "/fonts/map/dvips/xypic/xypic.map"

                         "/fonts/source/public/xypic/"
                         "/fonts/afm/public/xypic/"
                         "/fonts/tfm/public/xypic/"
                         "/fonts/type1/public/xypic/"
                         "/tex/generic/xypic/")
                   (base32
                    "09b51bbm189xh7039h5n8nmab5nn2bybhh26qjn08763m80zdhjg")
                   #:trivial? #t)))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'reset-gzip-timestamps)))))
      (home-page "https://www.ctan.org/pkg/xypic")
      (synopsis "Flexible diagramming macros")
      (description "This is a package for typesetting a variety of graphs and
diagrams with TeX.  Xy-pic works with most formats (including LaTeX,
AMS-LaTeX, AMS-TeX, and plain TeX).  The distribution includes Michael Barr's
@code{diag} package, which was previously distributed stand-alone.")
      (license license:gpl3+))))

(define-public texlive-fonts-xypic
  (deprecated-package "texlive-fonts-xypic" texlive-xypic))

(define-public texlive-generic-xypic
  (deprecated-package "texblive-generic-xypic" texlive-xypic))

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
         "1wijqq605cbhn2bdaryby3xpkwmnk9ixcrjn5zwlfrxbgfblzfmz"))))
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

(define-public texlive-charter
  (package
    (inherit (simple-texlive-package
              "texlive-charter"
              (list "/doc/fonts/charter/readme.charter"
                    "/fonts/afm/bitstrea/charter/"
                    "/fonts/tfm/bitstrea/charter/"
                    "/fonts/type1/bitstrea/charter/"
                    "/fonts/vf/bitstrea/charter/")
              (base32
               "09l5ymgz48s3hyn776l01g3isk3dnhrj1vdavdw4qq4kfxxpqdn9")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/charter")
    (synopsis "Charter fonts for TeX")
    (description "This package provides a copy of the Charter Type-1 fonts
which Bitstream contributed to the X consortium, renamed for use with TeX.
Support for use with LaTeX is available in @code{freenfss}, part of
@command{psnfss}. ")
    (license (license:non-copyleft
              "http://mirrors.ctan.org/fonts/charter/readme.charter"))))

(define-public texlive-fonts-charter
  (deprecated-package "texlive-fonts-charter" texlive-charter))

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
                "0d7d74giz5knvj4rj6mbzd6c05mwg9jrxab86jxdqbc3jy7cl4kz"))))
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

(define-public texlive-beamer
  (package
    (inherit (simple-texlive-package
              "texlive-beamer"
              (list "/doc/latex/beamer/"
                    "/tex/latex/beamer/")
              (base32
               "1fqzbkmw2kfxihab8j4dadc3v68xap6v2ghpp2064fna47xlwy1c")
              #:trivial? #t))
    (propagated-inputs
     `(("texlive-latex-hyperref" ,texlive-latex-hyperref)
       ("texlive-latex-oberdiek" ,texlive-latex-oberdiek)
       ("texlive-latex-etoolbox" ,texlive-latex-etoolbox)
       ("texlive-latex-pgf" ,texlive-latex-pgf)))
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

(define-public texlive-latex-beamer
  (deprecated-package "texlive-latex-beamer" texlive-beamer))

(define-public texlive-latex-xmpincl
  (package
    (name "texlive-latex-xmpincl")
    (version (number->string %texlive-revision))
    (source
     (origin
       (method svn-fetch)
       (uri (texlive-ref "latex" "xmpincl"))
       (file-name (string-append name "-" version "-checkout"))
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
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "18294h0cr05fs424m3x6aq24z5hf5zmiflalkj4kvpmsyyqqsj74"))))
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
             "171ffvpkj2fab4ljcxv3l6l5c8ga8zavdhmhfq07id8zyyr619ip"))))))
    (home-page "https://www.ctan.org/pkg/pdfx")
    (synopsis "PDF/X and PDF/A support for pdfTeX, LuaTeX and XeTeX")
    (description
     "This package helps LaTeX users to create PDF/X, PDF/A and other
standards-compliant PDF documents with pdfTeX, LuaTeX and XeTeX.")
    (license license:lppl1.2+)))

(define-public texlive-ydoc
  (let ((template (simple-texlive-package
                   "texlive-ydoc"
                   (list "/doc/latex/ydoc/"
                         "/source/latex/ydoc/")
                   (base32
                    "0ckcpy1b8v1fk3qc8qkxgiag2wc0qzxm6bgksv000m4m1hsi2g8b")
                   #:trivial? #f)))
    (package
      (inherit template)
      (outputs '("out" "doc"))
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:tex-directory _ #t)
          "latex/ydoc")
         ((#:build-targets _ #t)
          ''("ydoc.dtx"))
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'chdir
               (lambda _ (chdir "source/latex/ydoc") #t))
             (add-after 'copy-files 'move-files
               (lambda* (#:key outputs #:allow-other-keys)
                 (let* ((share (string-append (assoc-ref outputs "out")
                                              "/share/texmf-dist"))
                        (target (string-append share "/tex/generic/ydoc"))
                        (doc (string-append (assoc-ref outputs "doc")
                                            "/share/texmf-dist/doc") ))
                   (mkdir-p target)
                   (for-each
                    (lambda (file)
                      (rename-file (string-append share "/tex/latex/ydoc/" file)
                                   (string-append target "/" file)))
                    '("ydocincl.tex" "ydocstrip.tex"))
                   (mkdir-p doc)
                   (rename-file (string-append share "/doc") doc)
                   #t)))))))
      (home-page "http://www.ctan.org/pkg/ydoc")
      (synopsis "Macros for documentation of LaTeX classes and packages")
      (description "The package provides macros and environments to document
LaTeX packages and classes.  It is an (as yet unfinished) alternative to the
@code{ltxdoc} class and the @code{doc} or @code{xdoc} packages.  The aim is to
provide a different layout and more modern styles (using the @code{xcolor},
@code{hyperref} packages, etc.)  This is an alpha release, and should probably
not (yet) be used with other packages, since the implementation might
change.")
      (license license:lppl1.3+))))

(define-public texlive-pstricks
  (let ((template (simple-texlive-package
                   "texlive-pstricks"
                   (list "/doc/generic/pstricks/"
                         "/dvips/pstricks/"
                         "/tex/generic/pstricks/"
                         "/tex/latex/pstricks/")
                   (base32
                    "0sdxdd0qi4sccw9il7d4s7jivs24pq99cdzfnrf0gkqjb1y8s7cl")
                   #:trivial? #t)))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'reset-gzip-timestamps)))))
      (home-page "http://www.ctan.org/pkg/pstricks")
      (synopsis "PostScript macros for TeX")
      (description "PSTricks offers an extensive collection of macros for
generating PostScript that is usable with most TeX macro formats, including
Plain TeX, LaTeX, AMS-TeX, and AMS-LaTeX.  Included are macros for colour,
graphics, pie charts, rotation, trees and overlays.  It has many special
features, including a wide variety of graphics (picture drawing) macros, with
a flexible interface and with colour support.  There are macros for colouring
or shading the cells of tables.")
      (license license:lppl1.3+))))

(define-public texlive-pst-text
  (let ((template (simple-texlive-package
                   "texlive-pst-text"
                   (list "/doc/generic/pst-text/"
                         "/dvips/pst-text/pst-text.pro"
                         "/tex/generic/pst-text/"
                         "/tex/latex/pst-text/")
                   (base32
                    "146fpzd1xlqi94q5r48z8ni8qww713yh6nwkbr9pw27mjrqdadb9")
                   #:trivial? #t)))
    (package
      (inherit template)
      (propagated-inputs
       `(("texlive-pstricks" ,texlive-pstricks)))
      (home-page "http://www.ctan.org/pkg/pst-text")
      (synopsis "Text and character manipulation in PSTricks")
      (description "Pst-text is a PSTricks based package for plotting text along
a different path and manipulating characters.  It includes the functionality
of the old package @code{pst-char}.")
      (license license:lppl))))

(define-public texlive-marginnote
  (let ((template (simple-texlive-package
                   "texlive-marginnote"
                   (list "/source/latex/marginnote/marginnote.dtx")
                   (base32
                    "1vj1k8xm11gjdfj60as42d8lsv3dbzrm5dlgqcfk89d9dzm3k39j"))))
    (package
      (inherit template)
      (home-page "http://www.ctan.org/pkg/marginnote")
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:tex-directory _ '())
          "latex/marginnote")
         ((#:build-targets _ '())
          ''("marginnote.dtx"))
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'chdir
               (lambda _ (chdir "source/latex/marginnote") #t))))))
      (synopsis "Notes in the margin")
      (description "This package provides the command @code{\\marginnote} that
may be used instead of @code{\\marginpar} at almost every place where
@code{\\marginpar} cannot be used, e.g., inside floats, footnotes, or in
frames made with the @code{framed} package.")
      (license license:lppl1.3c+))))

(define-public texlive-iftex
  (let ((template (simple-texlive-package
                   "texlive-iftex"
                   (list "/doc/generic/iftex/"
                         "/tex/generic/iftex/iftex.sty")
                   (base32
                    "089zvw31gby150n1k0zdk2c0q97pgbqs46phxydaqil64b55nnl7")
                   #:trivial? #t)))
    (package
      (inherit template)
      (home-page "http://www.ctan.org/pkg/iftex")
      (synopsis "Determine the currently used TeX engine")
      (description "This package, which works both for Plain TeX and for
LaTeX, defines the @code{\\ifPDFTeX}, @code{\\ifXeTeX}, and @code{\\ifLuaTeX}
conditionals for testing which engine is being used for typesetting.  The
package also provides the @code{\\RequirePDFTeX}, @code{\\RequireXeTeX}, and
@code{\\RequireLuaTeX} commands which throw an error if pdfTeX, XeTeX or
LuaTeX (respectively) is not the engine in use.")
      (license license:lppl1.3+))))

(define-public texlive-tools
  (let ((template (simple-texlive-package
                   "texlive-tools"
                   (list "/doc/latex/tools/"
                         "/source/latex/tools/")
                   (base32
                    "1ivhij7171wvrgcjn4wah84wxwpd21d0chh3zxab4pj067c8d0mh"))))
    (package
      (inherit template)
      (arguments
       (substitute-keyword-arguments (package-arguments template)
         ((#:tex-directory _ '())
          "latex/tools")
         ((#:build-targets _ '())
          ''("tools.ins"))
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'chdir
               (lambda _ (chdir "source/latex/tools") #t))))))
      (home-page "https://www.ctan.org/tex-archive/macros/latex/required/tools/")
      (synopsis "LaTeX standard tools bundle")
      (description "This package provides a collection of simple tools that
are part of the LaTeX required tools distribution, comprising the packages:
@code{afterpage}, @code{array}, @code{bm}, @code{calc}, @code{dcolumn},
@code{delarray}, @code{enumerate}, @code{fileerr}, @code{fontsmpl},
@code{ftnright}, @code{hhline}, @code{indentfirst}, @code{layout},
@code{longtable}, @code{multicol}, @code{rawfonts}, @code{showkeys},
@code{somedefs}, @code{tabularx}, @code{theorem}, @code{trace},
@code{varioref}, @code{verbatim}, @code{xr}, and @code{xspace}.")
      (license license:lppl1.3+))))

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
    (arguments
     '(#:tex-directory "latex/xkeyval"
       #:build-targets '("xkeyval.dtx")
       #:tex-format "latex" ; won't build with luatex
       #:phases
       (modify-phases %standard-phases
         ;; This package cannot be built out of tree as it expects to find
         ;; built files in the working directory.
         (add-before 'build 'fix-build
           (lambda _
             (setenv "TEXINPUTS"
                     (string-append (getcwd) "/build:"))
             (substitute* "xkeyval.dtx"
               (("usepackage\\{xcolor\\}")
                "usepackage[dvips]{xcolor}"))
             #t))
         ;; FIXME: We don't have a package for this font yet.
         (add-after 'unpack 'remove-dependency-on-fourier
           (lambda _
             (substitute* "xkeyval.dtx"
               (("\\\\usepackage\\{fourier\\}") ""))
             #t))
         (add-after 'install 'move-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share/texmf-dist"))
                    (source (string-append share "/tex/latex/xkeyval/"))
                    (target (string-append share "/tex/generic/xkeyval/")))
               (mkdir-p target)
               (for-each (lambda (file)
                           (rename-file (string-append source file)
                                        (string-append target file)))
                         '("keyval.tex"
                           "pst-xkey.tex"
                           "xkeyval.tex"
                           "xkvex1.tex"
                           "xkvex2.tex"
                           "xkvex3.tex"
                           "xkvex4.tex"
                           "xkvtxhdr.tex"
                           "xkvutils.tex"))
               #t))))))
    (native-inputs
     `(("texlive-latex-base" ,texlive-latex-base)
       ("texlive-cm" ,texlive-cm)
       ("texlive-lm" ,texlive-lm)
       ("texlive-url" ,texlive-url)
       ("texlive-graphics-def" ,texlive-graphics-def)
       ("texlive-xcolor" ,texlive-xcolor)
       ("texlive-latex-footmisc" ,texlive-latex-footmisc)
       ("texlive-latex-listings" ,texlive-latex-listings)
       ("texlive-iftex" ,texlive-iftex)
       ("texlive-pstricks" ,texlive-pstricks)
       ("texlive-pst-text" ,texlive-pst-text)
       ("texlive-tools" ,texlive-tools)
       ("texlive-latex-pgf" ,texlive-latex-pgf)))
    (home-page "http://www.ctan.org/pkg/xkeyval")
    (synopsis "Extension of the keyval package")
    (description
     "This package is an extension of the keyval package and offers additional
macros for setting keys and declaring and setting class or package options.
The package allows the programmer to specify a prefix to the name of the
macros it defines for keys, and to define families of key definitions; these
all help use in documents where several packages define their own sets of
keys.")
    (license license:lppl1.3+)))

(define-public texlive-standalone
  (package
    (name "texlive-standalone")
    (version (number->string %texlive-revision))
    (source
     (origin
       (method svn-fetch)
       (uri (texlive-ref "latex" "standalone"))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "192ydxcn8ir96q8qwvnppksmqf5i0p50i0wz6iqazbwmh3dqxpx4"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/standalone"))
    (propagated-inputs
     `(("texlive-latex-xkeyval" ,texlive-latex-xkeyval)))
    (native-inputs
     `(("texlive-ydoc" ,texlive-ydoc)))
    (home-page "http://www.ctan.org/pkg/standalone")
    (synopsis "Compile TeX pictures stand-alone or as part of a document")
    (description "A class and package is provided which allows TeX pictures or
other TeX code to be compiled standalone or as part of a main document.
Special support for pictures with beamer overlays is also provided.  The
package is used in the main document and skips extra preambles in sub-files.
The class may be used to simplify the preamble in sub-files.  By default the
@code{preview} package is used to display the typeset code without margins.
The behaviour in standalone mode may adjusted using a configuration file
@code{standalone.cfg} to redefine the standalone environment.")
    (license license:lppl1.3+)))

(define-public texlive-siunitx
  (package
    (name "texlive-siunitx")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "/source/latex/siunitx/siunitx.dtx"
                   "/doc/latex/siunitx/README.md")
             (base32
              "11kf6znkgw7y5qmw75qk6px6pqf57bwr53q0673zaiyq20lif96c")))
    (build-system texlive-build-system)
    (arguments
     '(#:tex-directory "latex/siunitx"
       #:build-targets '("siunitx.dtx")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "source/latex/siunitx") #t)))))
    (propagated-inputs
     `(("texlive-latex-l3kernel" ,texlive-latex-l3kernel)
       ("texlive-latex-l3packages" ,texlive-latex-l3packages)))
    (home-page "http://www.ctan.org/pkg/siunitx")
    (synopsis "Comprehensive SI units package")
    (description
     "Typesetting values with units requires care to ensure that the combined
mathematical meaning of the value plus unit combination is clear.  In
particular, the SI units system lays down a consistent set of units with rules
on how they are to be used.  However, different countries and publishers have
differing conventions on the exact appearance of numbers (and units).  A
number of LaTeX packages have been developed to provide consistent application
of the various rules.  The @code{siunitx} package takes the best from the
existing packages, and adds new features and a consistent interface.  A number
of new ideas have been incorporated, to fill gaps in the existing provision.
The package also provides backward-compatibility with @code{SIunits},
@code{sistyle}, @code{unitsdef} and @code{units}.  The aim is to have one
package to handle all of the possible unit-related needs of LaTeX users.")
    (license license:lppl1.3c)))

(define-public texlive-booktabs
  (package
    (name "texlive-booktabs")
    (version (number->string %texlive-revision))
    (source
     (origin
       (method svn-fetch)
       (uri (texlive-ref "latex" "booktabs"))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "1dqid48vgh25wmw8xzmx6x3pfgz1y9f0r8aza1yxq2mjny5yf68x"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/booktabs"))
    (home-page "http://www.ctan.org/pkg/booktabs")
    (synopsis "Publication quality tables in LaTeX")
    (description
     "This package enhances the quality of tables in LaTeX, providing extra
commands as well as behind-the-scenes optimisation.  Guidelines are given as
to what constitutes a good table in this context.  The package offers
@code{longtable} compatibility.")
    (license license:lppl1.3+)))

(define-public texlive-csquotes
  (let ((template (simple-texlive-package
                   "texlive-csquotes"
                   (list "/doc/latex/csquotes/"
                         "/tex/latex/csquotes/")
                   (base32
                    "088gvi60d7sdl6fgg68fbz30fnpqc3yrpkx80sfw7vwgar3wm3av")
                   #:trivial? #t)))
    (package
      (inherit template)
      (propagated-inputs
       `(("texlive-etoolbox" ,texlive-etoolbox)))
      (home-page "https://www.ctan.org/pkg/csquotes")
      (synopsis "Context sensitive quotation facilities")
      (description "This package provides advanced facilities for inline and
display quotations.  It is designed for a wide range of tasks ranging from the
most simple applications to the more complex demands of formal quotations.
The facilities include commands, environments, and user-definable 'smart
quotes' which dynamically adjust to their context.  Quotation marks are
switched automatically if quotations are nested and they can be adjusted to
the current language if the babel package is available.  There are additional
facilities designed to cope with the more specific demands of academic
writing, especially in the humanities and the social sciences.  All quote
styles as well as the optional active quotes are freely configurable.")
      (license license:lppl1.3c+))))

(define-public texlive-logreq
  (let ((template (simple-texlive-package
                   "texlive-logreq"
                   (list "/doc/latex/logreq/"
                         "/tex/latex/logreq/")
                   (base32
                    "13difccs3cxlkqlnhw286yb0c7mifrxfd402a2x5wwxv0m1kgfqd")
                   #:trivial? #t)))
    (package
      (inherit template)
      (propagated-inputs
       `(("texlive-etoolbox" ,texlive-etoolbox)))
      (home-page "https://www.ctan.org/pkg/logreq")
      (synopsis "Support for automation of the LaTeX workflow")
      (description "The package helps to automate a typical LaTeX
workflow that involves running LaTeX several times and running tools
such as BibTeX or makeindex.  It will log requests like \"please rerun
LaTeX\" or \"please run BibTeX on file X\" to an external file in a
machine-readable format.  Compiler scripts and integrated LaTeX
editing environments may parse this file to determine the next steps
in the workflow.  In sum, the package will do two things:

@enumerate
@item
enable package authors to use LaTeX commands to issue requests, and

@item
collect all requests from all packages and write them to an external
XML file.
@end enumerate\n")
      (license license:lppl1.3c))))

(define-public texlive-biblatex
  (let ((template (simple-texlive-package
                   "texlive-biblatex"
                   (list "/doc/latex/biblatex/"
                         "/tex/latex/biblatex/")
                   (base32
                    "11kzvny50iklzkamr0rqd5x532q8rxny1xhmf96jl8mzj1d2zmay")
                   #:trivial? #t)))
    (package
      (inherit template)
      (propagated-inputs
       `(("texlive-logreq" ,texlive-logreq)
         ("texlive-url" ,texlive-url)))
      (home-page "https://www.ctan.org/pkg/biblatex")
      (synopsis "Sophisticated bibliographies in LaTeX")
      (description "BibLaTeX is a complete reimplementation of the
bibliographic facilities provided by LaTeX.  Formatting of the
bibliography is entirely controlled by LaTeX macros, facilitating the
design of new bibliography and citation styles.  BibLaTeX uses its own
data backend program \"biber\" to read and process the bibliographic
data.  With biber, the range of features provided by biblatex
includes:

@enumerate
@item
full unicode support,

@item
customisable bibliography labels,

@item
multiple bibliographies in the same document, and

@item
subdivided bibliographies, such as bibliographies per chapter or
section.
@end enumerate\n")
      (license license:lppl1.3c))))

(define-public texlive-todonotes
  (let ((template (simple-texlive-package
                   "texlive-todonotes"
                   (list "/doc/latex/todonotes/"
                         "/tex/latex/todonotes/")
                   (base32
                    "0lvxsskz4bdfxhd59hf77kiq8k4nh2spb66vc6hifdgi21z8r8wm")
                   #:trivial? #t)))
    (package
      (inherit template)
      (propagated-inputs
       `(("texlive-latex-pgf" ,texlive-latex-pgf)
         ("texlive-latex-xkeyval" ,texlive-latex-xkeyval)))
      (home-page "http://www.ctan.org/pkg/todonotes")
      (synopsis "Marking things to do in a LaTeX document")
      (description "The @code{todonotes} package lets the user mark
things to do later, in a simple and visually appealing way.  The
package takes several options to enable customization and finetuning
of the visual appearance.")
      (license license:lppl1.3+))))

(define-public texlive-units
  (let ((template (simple-texlive-package
                   "texlive-units"
                   (list "/doc/latex/units/"
                         "/tex/latex/units/")
                   (base32
                    "1ia1vzy8dp7pdvmawwnmh9lmkajmpnnh62dixrjpb6mnxq118bfd")
                   #:trivial? #t)))
    (package
      (inherit template)
      (home-page "http://www.ctan.org/pkg/units")
      (synopsis "Typeset physical units and fractions")
      (description "@code{units} is a package for typesetting physical
units in a standard-looking way.  The package is based upon
@code{nicefrac}, a package for typing fractions.  @code{nicefrac} is
included in the @code{units} bundle.")
      (license license:gpl3+))))

(define-public texlive-microtype
  (let ((template (simple-texlive-package
                   "texlive-microtype"
                   (list "/doc/latex/microtype/"
                         "/tex/latex/microtype/")
                   (base32
                    "0xmjpzbj4nqmnl5m7xx1bshdk2c8n57rmbvn0j479ypj4wdlq9iy")
                   #:trivial? #t)))
    (package
      (inherit template)
      (home-page "http://www.ctan.org/pkg/microtype")
      (synopsis "Subliminal refinements towards typographical perfection")
      (description "@code{microtype} provides a LaTeX interface to the
micro-typographic extensions that were introduced by pdfTeX and have
since propagated to XeTeX and LuaTeX: most prominently character
protrusion and font expansion, the adjustment of kerning and interword
spacing, hyphenatable letterspacing and the possibility to disable all
or selected ligatures.  These features may be applied to customisable
sets of fonts.  All micro-typographic aspects of the fonts can be
configured in a straight-forward and flexible way.  Settings for
various fonts are provided.  An alternative package
@code{letterspace}, which also works with plain TeX, is included in
the bundle.")
      (license license:lppl1.3c))))

(define-public texlive-caption
  (let ((template (simple-texlive-package
                   "texlive-caption"
                   (list "/doc/latex/caption/"
                         "/tex/latex/caption/")
                   (base32
                    "09gmh8yjj9f5zak8r18g87w9p5jn7flnvmlhxmvdq6992mbdc6hg")
                   #:trivial? #t)))
    (package
      (inherit template)
      (home-page "http://www.ctan.org/pkg/caption")
      (synopsis "Customising captions in floating environments")
      (description "The @code{caption} package provides many ways to
customise the captions in floating environments like figure and table.
Facilities include rotating captions, sideways captions and continued
captions (for tables or figures that come in several parts).  A list
of compatibility notes, for other packages, is provided in the
documentation.  The package also provides the \"caption outside
float\" facility, in the same way that simpler packages like
@code{capt-ofcapt-of} do.  The package supersedes @code{caption2}.
Packages @code{bicaption}, @code{ltcaption}, @code{newfloat},
@code{subcaption} and @code{totalcount} are included in the bundle.")
      (license license:lppl1.3+))))

(define-public texlive-symbol
  (package
    (inherit (simple-texlive-package
              "texlive-symbol"
              (list "/dvips/symbol/"
                    "/fonts/afm/adobe/symbol/"
                    "/fonts/afm/urw/symbol/"
                    "/fonts/tfm/adobe/symbol/"
                    "/fonts/tfm/urw35vf/symbol/"
                    "/fonts/type1/urw/symbol/"
                    "/fonts/map/dvips/symbol/"
                    "/tex/latex/symbol/")
              (base32
               "01xiygb88xwi7rfvh1zrlxzi5pqb5fvylws5zzszg379iz4pyzwj")
              #:trivial? #t))
    (home-page "https://ctan.org/pkg/urw-base35")
    (synopsis "URW Base 35 font pack for LaTeX")
    (description "This package provides a drop-in replacement for the
Symbol font from Adobe's basic set.")
    (license license:gpl2)))

(define-public texlive-mathpazo
  (package
    (inherit (simple-texlive-package
              "texlive-mathpazo"
              (list "/doc/latex/mathpazo/"
                    "/fonts/afm/public/mathpazo/"
                    "/fonts/tfm/public/mathpazo/"
                    "/fonts/type1/public/mathpazo/"
                    "/fonts/vf/public/mathpazo/")
              (base32
               "02in6hdnbnkz216mpy8g6fk3wmlls8nh5982vmg37vhbj77lk0rh")
              #:trivial? #t))
    (home-page "http://www.ctan.org/pkg/mathpazo")
    (synopsis "Fonts to typeset mathematics to match Palatino")
    (description "The Pazo Math fonts are a family of PostScript fonts
suitable for typesetting mathematics in combination with the Palatino
family of text fonts.  The Pazo Math family is made up of five fonts
provided in Adobe Type 1 format.  These contain glyphs that are
usually not available in Palatino and for which Computer Modern looks
odd when combined with Palatino.  These glyphs include the uppercase
Greek alphabet in upright and slanted shapes, the lowercase Greek
alphabet in slanted shape, several mathematical glyphs and the
uppercase letters commonly used to represent various number sets.
LaTeX macro support is provided in package @code{psnfss}.")
    (license license:gpl3+)))

(define-public texlive-fpl
  (package
    (inherit (simple-texlive-package
              "texlive-fpl"
              (list "/doc/fonts/fpl/"
                    "/source/fonts/fpl/"
                    "/fonts/afm/public/fpl/"
                    "/fonts/type1/public/fpl/")
              (base32
               "02gkl516z9kn8xqs269pdkqn37sxm8ib0pcs43s4rs2rhyyl5z68")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/fpl")
    (synopsis "SC and OsF fonts for URW Palladio L")
    (description "The FPL Fonts provide a set of SC/OsF fonts for URW
Palladio L which are compatible with the Palatino SC/OsF fonts from
Adobe.  LaTeX use is enabled by the mathpazo package, which is part of
the @code{psnfss} distribution.")
    ;; Either LPPL version 1.0 or later, or GPL version 2
    (license (list license:lppl1.0+ license:gpl2))))

(define-public texlive-arev
  (package
    (inherit (simple-texlive-package
              "texlive-arev"
              (list "/doc/fonts/arev/"
                    "/fonts/afm/public/arev/"
                    "/fonts/enc/dvips/arev/"
                    "/fonts/map/dvips/arev/"
                    "/fonts/tfm/public/arev/"
                    "/fonts/type1/public/arev/"
                    "/fonts/vf/public/arev/"
                    "/tex/latex/arev/")
              (base32
               "15wkgc48r52mjpymv6l7j9bl99kwxbvg3g1mi3qyq7nfm799dkxy")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/arev")
    (synopsis "Fonts and LaTeX support files for Arev Sans")
    (description "The @code{arev} package provides type 1 fonts,
virtual fonts and LaTeX packages for using Arev Sans in both text and
mathematics.  Arev Sans is a derivative of Bitstream Vera Sans, adding
support for Greek and Cyrillic characters and a few variant letters
appropriate for mathematics.  The font is primarily used in LaTeX for
presentations, particularly when using a computer projector.  Arev
Sans has large x-height, \"open letters\", wide spacing and thick
stems.  The style is very similar to the SliTeX font lcmss but
heavier.  Arev is one of a very small number of sans-font mathematics
support packages.  Others are cmbright, hvmath and kerkis.")
    (license (list license:silofl1.1 ;for Arev Sans
                   license:lppl1.3a  ;for TeX support files
                   license:gpl2))))  ;for ams-mdbch.sty

(define-public texlive-mathdesign
  (package
    (inherit (simple-texlive-package
              "texlive-mathdesign"
              (list "/doc/fonts/mathdesign/"
                    "/dvips/mathdesign/"
                    "/fonts/enc/dvips/mathdesign/"
                    "/fonts/map/dvips/mathdesign/"
                    "/fonts/tfm/public/mathdesign/"
                    "/fonts/type1/public/mathdesign/"
                    "/fonts/vf/public/mathdesign/"
                    "/tex/latex/mathdesign/")
              (base32
               "0jcby2sd0l3ank2drxc0qcf5d1cwa8idzh4g91h4nxk8zrzxj8nr")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/mathdesign")
    (synopsis "Mathematical fonts to fit with particular text fonts")
    (description "The Math Design project offers free mathematical
fonts that match with existing text fonts.  To date, three free font
families are available: Adobe Utopia, URW Garamond and Bitstream
Charter.  Mathdesign covers the whole LaTeX glyph set including AMS
symbols.  Both roman and bold versions of these symbols can be used.
Moreover, there is a choice between three greek fonts (two of them
created by the Greek Font Society).")
    (license license:gpl2+)))

(define-public texlive-bera
  (package
    (inherit (simple-texlive-package
              "texlive-bera"
              (list "/doc/fonts/bera/"
                    "/fonts/afm/public/bera/"
                    "/fonts/map/dvips/bera/"
                    "/fonts/tfm/public/bera/"
                    "/fonts/type1/public/bera/"
                    "/fonts/vf/public/bera/"
                    "/tex/latex/bera/")
              (base32
               "1pkmhhr6ah44xhipjr7nianv03hr4w4bn45xcvp264yw6ymqzqwr")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/bera")
    (synopsis "Bera fonts")
    (description "The @code{bera} package contains the Bera Type 1
fonts and files to use the fonts with LaTeX.  Bera is a set of three
font families: Bera Serif (a slab-serif Roman), Bera Sans (a Frutiger
descendant) and Bera Mono (monospaced/typewriter).  The Bera family is
a repackaging, for use with TeX, of the Bitstream Vera family.")
    (license license:silofl1.1)))

(define-public texlive-fourier
  (package
    (inherit (simple-texlive-package
              "texlive-fourier"
              (list "/doc/fonts/fourier/"
                    "/fonts/afm/public/fourier/"
                    "/fonts/map/dvips/fourier/"
                    "/fonts/tfm/public/fourier/"
                    "/fonts/type1/public/fourier/"
                    "/fonts/vf/public/fourier/"
                    "/tex/latex/fourier/")
              (base32
               "1vs2xdx6f6hd01zlslx3y93g3dsa7k3yhqpnhgkizgjmz0r9ipz1")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/fourier")
    (synopsis "Utopia fonts for LaTeX documents")
    (description "Fourier-GUTenberg is a LaTeX typesetting system
which uses Adobe Utopia as its standard base font.  Fourier-GUTenberg
provides all complementary typefaces needed to allow Utopia based TeX
typesetting including an extensive mathematics set and several other
symbols.  The system is absolutely stand-alone; apart from Utopia and
Fourier no other typefaces are required.  Utopia is a registered
trademark of Adobe Systems Incorporated.")
    (license license:lppl)))

(define-public texlive-utopia
  (package
    (inherit (simple-texlive-package
              "texlive-utopia"
              (list "/doc/fonts/utopia/"
                    "/fonts/afm/adobe/utopia/"
                    "/fonts/tfm/adobe/utopia/"
                    "/fonts/type1/adobe/utopia/"
                    "/fonts/vf/adobe/utopia/")
              (base32
               "113wgkfz4z0ls2grxxfj17l42a1yv9r5ipcd0156xnfsrqvqzxfc")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/utopia")
    (synopsis "Adobe Utopia fonts")
    (description "The Adobe Standard Encoding set of the Utopia font
family, as contributed to the X Consortium.  The set comprises upright
and italic shapes in medium and bold weights.  Macro support and
matching maths fonts are provided by the @code{fourier} and
@code{mathdesign} font packages.")
    (license (license:fsf-free
              "http://mirrors.ctan.org/fonts/utopia/README"))))

(define-public texlive-fontaxes
  (package
    (name "texlive-fontaxes")
    (version "1.0e")
    (source
     (origin
       (method svn-fetch)
       (uri (texlive-ref "latex" "fontaxes"))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "19mhp9l7cjw0sbq55c9lz0l2pffkyhyir3i63jqynifjmglbgkl7"))))
    (build-system texlive-build-system)
    (arguments '(#:tex-directory "latex/fontaxes"))
    (home-page "http://www.ctan.org/pkg/fontaxes")
    (synopsis "Additional font axes for LaTeX")
    (description "The @code{fontaxes} package adds several new font
axes on top of LaTeX's New Font Selection Scheme (NFSS).  In
particular, it splits the shape axis into a primary and a secondary
shape axis and it adds three new axes to deal with the different
figure versions offered by many professional fonts.")
    (license license:lppl1.3+)))

(define-public texlive-mweights
  (package
    (inherit (simple-texlive-package
              "texlive-mweights"
              (list "/doc/latex/mweights/"
                    "/tex/latex/mweights/")
              (base32
               "1k2xclk54q3xgn48hji23q52nivkzgwf0s30bmm6k83f7v57qv8h")
              #:trivial? #t))
    (home-page "https://www.ctan.org/pkg/mweights")
    (synopsis "Support for multiple-weight font packages")
    (description "Many font families available for use with LaTeX are
available at multiple weights.  Many Type 1-oriented support packages
for such fonts re-define the standard @code{\\mddefault} or
@code{\\bfdefault} macros.  This can create difficulties if the weight
desired for one font family is not available for another font family,
or if it differs from the weight desired for another font family.  The
@code{mweights} package provides a solution to these difficulties.")
    (license license:lppl)))
