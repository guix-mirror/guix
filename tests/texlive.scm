;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (test-texlive)
  #:use-module (gnu packages tex)
  #:use-module (guix import texlive)
  #:use-module (guix tests)
  #:use-module (guix tests http)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-26)
  #:use-module (web client)
  #:use-module (ice-9 match))

(test-begin "texlive")

(define %fake-tlpdb
  '(("stricttex"
     . ((name
         . "stricttex")
        (shortdesc
         . "Strictly balanced brackets and numbers in command names")
        (longdesc
         . "This is a small, LuaLaTeX-only package providing you with three,
sometimes useful features: It allows you to make brackets [...]  \"strict\",
meaning that each [ must be balanced by a ]. It allows you to use numbers in
command names, so that you can do stuff like \\newcommand\\pi12{\\pi_{12}}. It
allows you to use numbers and primes in command names, so that you can do
stuff like \\newcommand\\pi'12{\\pi '_{12}}.")
        (docfiles
         . ("texmf-dist/doc/lualatex/stricttex/README.md"
            "texmf-dist/doc/lualatex/stricttex/stricttex.pdf"))
        (runfiles
         . ("texmf-dist/tex/lualatex/stricttex/stricttex.lua"
            "texmf-dist/tex/lualatex/stricttex/stricttex.sty"))
        (catalogue-license . "lppl1.3c")))
    ("texsis"
     . ((name
         . "texsis")
        (shortdesc
         . "Plain TeX macros for Physicists")
        (longdesc
         . "TeXsis is a TeX macro package which provides useful features for
typesetting research papers and related documents. For example, it includes
support specifically for: Automatic numbering of equations, figures, tables
and references; Simplified control of type sizes, line spacing, footnotes,
running headlines and footlines, and tables of contents, figures and tables;
Specialized document formats for research papers, preprints and \"e-prints\",
conference proceedings, theses, books, referee reports, letters, and
memoranda; Simplified means of constructing an index for a book or thesis;
Easy to use double column formatting; Specialized environments for lists,
theorems and proofs, centered or non-justified text, and listing computer
code; Specialized macros for easily constructing ruled tables. TeXsis was
originally developed for physicists, but others may also find it useful. It is
completely compatible with Plain TeX.")
        (depend . ("cm" "hyphen-base" "knuth-lib" "plain" "tex"))
        (docfiles
         . ("texmf-dist/doc/man/man1/texsis.1"
            "texmf-dist/doc/man/man1/texsis.man1.pdf"
            "texmf-dist/doc/otherformats/texsis/base/COPYING"
            "texmf-dist/doc/otherformats/texsis/base/Example.tex"
            "texmf-dist/doc/otherformats/texsis/base/Fonts.tex"
            "texmf-dist/doc/otherformats/texsis/base/INSTALL"
            "texmf-dist/doc/otherformats/texsis/base/Install.tex"
            "texmf-dist/doc/otherformats/texsis/base/MANIFEST"
            "texmf-dist/doc/otherformats/texsis/base/Manual.fgl"
            "texmf-dist/doc/otherformats/texsis/base/Manual.ref"
            "texmf-dist/doc/otherformats/texsis/base/Manual.tbl"
            "texmf-dist/doc/otherformats/texsis/base/Manual.tex"
            "texmf-dist/doc/otherformats/texsis/base/NEWS"
            "texmf-dist/doc/otherformats/texsis/base/README"
            "texmf-dist/doc/otherformats/texsis/base/TXSapxF.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXScover.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSdcol.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSdoc.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSdoc0.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSdocM.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSend.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSenvmt.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSeqns.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSfigs.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSfmts.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSfonts.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSinstl.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSintro.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSletr.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSmisc.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSprns.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSrefs.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSrevs.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSruled.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSsects.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXSsite.000"
            "texmf-dist/doc/otherformats/texsis/base/TXSsymb.doc"
            "texmf-dist/doc/otherformats/texsis/base/TXStags.doc"
            "texmf-dist/doc/otherformats/texsis/base/index.tex"
            "texmf-dist/doc/otherformats/texsis/base/letr"
            "texmf-dist/doc/otherformats/texsis/base/penguin.eps"
            "texmf-dist/doc/otherformats/texsis/base/penguin2.eps"
            "texmf-dist/doc/otherformats/texsis/base/texsis.el"
            "texmf-dist/doc/otherformats/texsis/base/texsis.lsm"))
        (runfiles
         . ("texmf-dist/bibtex/bst/texsis/texsis.bst"
            "texmf-dist/tex/texsis/base/AIP.txs"
            "texmf-dist/tex/texsis/base/CVformat.txs"
            "texmf-dist/tex/texsis/base/Elsevier.txs"
            "texmf-dist/tex/texsis/base/Exam.txs"
            "texmf-dist/tex/texsis/base/Formletr.txs"
            "texmf-dist/tex/texsis/base/IEEE.txs"
            "texmf-dist/tex/texsis/base/PhysRev.txs"
            "texmf-dist/tex/texsis/base/Spanish.txs"
            "texmf-dist/tex/texsis/base/Swedish.txs"
            "texmf-dist/tex/texsis/base/TXSconts.tex"
            "texmf-dist/tex/texsis/base/TXSdcol.tex"
            "texmf-dist/tex/texsis/base/TXSenvmt.tex"
            "texmf-dist/tex/texsis/base/TXSeqns.tex"
            "texmf-dist/tex/texsis/base/TXSfigs.tex"
            "texmf-dist/tex/texsis/base/TXSfmts.tex"
            "texmf-dist/tex/texsis/base/TXSfonts.tex"
            "texmf-dist/tex/texsis/base/TXShead.tex"
            "texmf-dist/tex/texsis/base/TXSinit.tex"
            "texmf-dist/tex/texsis/base/TXSletr.tex"
            "texmf-dist/tex/texsis/base/TXSmacs.tex"
            "texmf-dist/tex/texsis/base/TXSmemo.tex"
            "texmf-dist/tex/texsis/base/TXSprns.tex"
            "texmf-dist/tex/texsis/base/TXSrefs.tex"
            "texmf-dist/tex/texsis/base/TXSruled.tex"
            "texmf-dist/tex/texsis/base/TXSsects.tex"
            "texmf-dist/tex/texsis/base/TXSsite.tex"
            "texmf-dist/tex/texsis/base/TXSsymb.tex"
            "texmf-dist/tex/texsis/base/TXStags.tex"
            "texmf-dist/tex/texsis/base/TXStitle.tex"
            "texmf-dist/tex/texsis/base/Tablebod.txs"
            "texmf-dist/tex/texsis/base/WorldSci.txs"
            "texmf-dist/tex/texsis/base/color.txs"
            "texmf-dist/tex/texsis/base/nuclproc.txs"
            "texmf-dist/tex/texsis/base/printfont.txs"
            "texmf-dist/tex/texsis/base/spine.txs"
            "texmf-dist/tex/texsis/base/texsis.tex"
            "texmf-dist/tex/texsis/base/thesis.txs"
            "texmf-dist/tex/texsis/base/twin.txs"
            "texmf-dist/tex/texsis/config/texsis.ini"))
        (catalogue-license . "lppl")))))

(test-assert "texlive->guix-package"
  ;; Replace network resources with sample data.
  (mock ((guix build svn) svn-fetch
         (lambda* (url revision directory
                       #:key (svn-command "svn")
                       (user-name #f)
                       (password #f)
                       (recursive? #t))
           (mkdir-p directory)
           (with-output-to-file (string-append directory "/foo")
             (lambda ()
               (display "source")))))
        (let ((result (texlive->guix-package "texsis"
                                             #:package-database
                                             (lambda _ %fake-tlpdb))))
          (match result
            (('package
               ('inherit ('simple-texlive-package
                          "texlive-texsis"
                          ('list "bibtex/bst/texsis/"
                                 "doc/man/man1/"
                                 "doc/otherformats/texsis/base/"
                                 "tex/texsis/base/"
                                 "tex/texsis/config/")
                          ('base32 (? string? hash))
                          #:trivial? #t))
               ('propagated-inputs
                ('list 'texlive-cm
                       'texlive-hyphen-base
                       'texlive-knuth-lib
                       'texlive-plain
                       'texlive-tex))
               ('home-page "https://www.tug.org/texlive/")
               ('synopsis "Plain TeX macros for Physicists")
               ('description (? string? description))
               ('license 'lppl))
             #true)
            (_
             (begin
               (format #t "~s~%" result)
               (pk 'fail result #f)))))))

(test-end "texlive")
