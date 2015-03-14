;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages texlive)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages tcsh)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages zip)
  #:autoload   (gnu packages texinfo) (texinfo))

(define texlive-extra-src
  (origin
    (method url-fetch)
    (uri "ftp://tug.org/historic/systems/texlive/2014/texlive-20140525-extra.tar.xz")
    (sha256 (base32
              "1zlnjysvxskcy05iva6jfklirwv12wqyn3ia119a7xnqlvhpqz33"))))

(define texlive-texmf-src
  (origin
    (method url-fetch)
    (uri "ftp://tug.org/historic/systems/texlive/2014/texlive-20140525-texmf.tar.xz")
    (sha256 (base32
              "0qsr55ms1278dhmgixs5qqwd4fxhh369ihkki6wgh8xaqm8p48p0"))))

(define-public texlive-bin
  (package
   (name "texlive-bin")
   (version "2014")
   (source
    (origin
     (method url-fetch)
      (uri "ftp://tug.org/historic/systems/texlive/2014/texlive-20140525-source.tar.xz")
      (sha256 (base32
               "1glmaw2jv42grbsn05kay825j66scimjqqc32776bb1356q4xfq8"))))
   (build-system gnu-build-system)
   (inputs
    `(("texlive-extra-src" ,texlive-extra-src)
      ("cairo" ,cairo)
      ("fontconfig" ,fontconfig)
      ("fontforge" ,fontforge)
      ("freetype" ,freetype)
      ("gd" ,gd)
      ("ghostscript" ,ghostscript)
      ("graphite2" ,graphite2)
      ("harfbuzz" ,harfbuzz)
      ("icu4c" ,icu4c)
      ("libpaper" ,libpaper)
      ("libpng" ,libpng)
      ("libxaw" ,libxaw)
      ("libxt" ,libxt)
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
         "--with-system-graphite2"
         "--with-system-harfbuzz"
         "--with-system-icu"
         "--with-system-libgs"
         "--with-system-libpaper"
         "--with-system-libpng"
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
       (alist-cons-after
        'install 'postinst
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
               (system* "mv" "tlpkg" share))))
        %standard-phases)))
   (synopsis "TeX Live, a package of the TeX typesetting system")
   (description
    "TeX Live provides a comprehensive TeX document production system.
It includes all the major TeX-related programs, macro packages, and fonts
that are free software, including support for many languages around the
world.

This package contains the binaries.")
   (license (license:fsf-free "http://tug.org/texlive/copying.html"))
   (home-page "http://www.tug.org/texlive/")))

(define-public texlive-texmf
  (package
   (name "texlive-texmf")
   (version "2014")
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
      #:imported-modules ((guix build gnu-build-system)
                          (guix build utils))
      #:phases
        (alist-cons-before
         'texmf-config 'install
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((share (string-append (assoc-ref outputs "out") "/share")))
             (mkdir-p share)
             (system* "mv" "texmf-dist" share)))
         (alist-cons-after
          'patch-source-shebangs 'texmf-config
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((share (string-append (assoc-ref outputs "out") "/share"))
                   (texmfroot (string-append share "/texmf-dist/web2c"))
                   (texmfcnf (string-append texmfroot "/texmf.cnf"))
                   (texbin (string-append (assoc-ref inputs "texlive-bin")
                            "/bin")))
              ;; Register SHARE as TEXMFROOT in texmf.cnf.
              (substitute* texmfcnf
                (("TEXMFROOT = \\$SELFAUTOPARENT")
                (string-append "TEXMFROOT = " share)))
              ;; Configure the texmf-dist tree; inspired from
              ;; http://slackbuilds.org/repository/13.37/office/texlive/
              (setenv "PATH" (string-append (getenv "PATH") ":" texbin))
              (setenv "TEXMFCNF" texmfroot)
              (system* "updmap-sys" "--nohash" "--syncwithtrees")
              (system* "mktexlsr")
              (system* "fmtutil-sys" "--all")))
          (map (cut assq <> %standard-phases)
               '(set-paths unpack patch-source-shebangs))))))
   (synopsis "TeX Live, a package of the TeX typesetting system")
   (description
    "TeX Live provides a comprehensive TeX document production system.
It includes all the major TeX-related programs, macro packages, and fonts
that are free software, including support for many languages around the
world.

This package contains the complete tree of texmf-dist data.")
   (license (license:fsf-free "http://tug.org/texlive/copying.html"))
   (home-page "http://www.tug.org/texlive/")))

(define-public texlive
  (package
   (name "texlive")
   (version "2014")
   (source #f)
   (build-system trivial-build-system)
   (inputs `(("bash" ,bash) ; for wrap-program
             ("texlive-bin" ,texlive-bin)
             ("texlive-texmf" ,texlive-texmf)))
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
   (license (license:fsf-free "http://tug.org/texlive/copying.html"))
   (home-page "http://www.tug.org/texlive/")))

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
