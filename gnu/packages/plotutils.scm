;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016-2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages plotutils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module ((guix utils) #:select (target-x86-32?))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages web)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages))

(define-public plotutils
  (package
    (name "plotutils")
    (version "2.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/plotutils/plotutils-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1arkyizn5wbgvbh53aziv3s6lmd3wm9lqzkhxb3hijlp1y124hjg"))
             (modules '((guix build utils)))
             (snippet
              ;; Force the use of libXaw7 instead of libXaw.  When not doing
              ;; that, libplot.la ends up containing just "-lXaw" (without
              ;; "-L/path/to/Xaw"), due to the fact that there is no
              ;; libXaw.la, which forces us to propagate libXaw.
              '(begin
                 (substitute* "configure"
                   (("-lXaw")
                    "-lXaw7"))
                 ;; Use the `png_jmpbuf' accessor, as recommended since libpng
                 ;; 1.4.0 (see:
                 ;; http://www.libpng.org/pub/png/src/libpng-1.2.x-to-1.4.x-summary.txt).
                 (substitute* "libplot/z_write.c"
                   (("png_ptr->jmpbuf")
                    "png_jmpbuf (png_ptr)"))
                 #t))
             (patches
              ;; The test suite fails on some architectures such as i686 (see:
              ;; https://lists.gnu.org/archive/html/bug-plotutils/2016-04/msg00002.html).
              ;; The following Debian patch works around it.
              (search-patches "plotutils-spline-test.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-libplotter"

                   ;; On i686 some tests fail due to excess floating point
                   ;; precision; work around it.  However, libplotter is C++
                   ;; and thus unaffected by CFLAGS, but '-fexcess-precision'
                   ;; is not implemented for C++ as of GCC 10.
                   #$@(if (target-x86-32?)
                          #~("CFLAGS=-g -O2 -fexcess-precision=standard")
                          #~()))

           #:phases
           (if (target-x86-32?)
               #~(modify-phases %standard-phases
                   (add-before 'check 'skip-sloppy-test
                     (lambda _
                       ;; This test reveals a slight difference in the SVG
                       ;; output due to floating point inequalities.  Skip it.
                       (substitute* "test/plot2svg.test"
                         (("^exit .*") "exit 77")))))
               #~%standard-phases)))
    (inputs (list libpng libx11 libxt libxaw))
    (home-page "https://www.gnu.org/software/plotutils/")
    (synopsis "Plotting utilities and library")
    (description
     "GNU Plotutils is a package for plotting and working with 2D graphics.
It includes the C library @code{libplot} and the C++ @code{libplotter} library
for exporting 2D vector graphics in many file formats.  It also has support
for 2D vector graphics animations.  The package also contains command-line
programs for plotting scientific data.")
    (license license:gpl2+)))

(define-public guile-plotutils
  (package
    (name "guile-plotutils")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "https://lonelycactus.com/tarball/"
                                        "guile_plotutils-" version ".tar.gz")
                         (string-append
                          "https://github.com/spk121/guile-plotutils/releases/download/v"
                          version "/guile_plotutils-" version
                          ".tar.gz")))
              (sha256
               (base32
                "0r245z75cdzgzi57fpz84mnyrjq44793zzaaxxrszyxm1d06hc6r"))))
    (build-system gnu-build-system)
    (arguments
     `(#:imported-modules ((guix build guile-build-system)
                           ,@%gnu-build-system-modules)
       #:modules (((guix build guile-build-system)
                   #:select (target-guile-effective-version))
                  (guix build gnu-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'set-library-file-name
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (version (target-guile-effective-version)))
               ;; First install libguile-plotutils.so.
               (invoke "make" "install-guileextensionLTLIBRARIES")

               ;; Then change source files to refer to it.
               (substitute* '("module/plotutils/graph.scm"
                              "module/plotutils/plot.scm")
                 (("\"libguile-plotutils\"")
                  (string-append "\"" out "/lib/guile/" version
                                 "/extensions/libguile-plotutils\"")))))))))
    (native-inputs (list pkg-config texinfo))
    (inputs (list plotutils guile-3.0 zlib))
    (home-page "https://lonelycactus.com/guile-plotutils.html")
    (synopsis "Guile bindings to the GNU Plotutils plotting libraries")
    (description
     "Guile-Plotutils is a Guile binding to the venerable GNU Plotutils
plotting and graphing library.  If you want to make graphs that look like you
went to university in the 1990s, this is the library for you.")
    (license license:gpl3+)))

(define-public guile-charting
  ;; This commit fixes a few things, including Guile 3 support, not available
  ;; in the latest release.
  (let ((commit "75f755b691a9f712f3b956657d01805d6a8a1b98")
        (revision "1"))
    (package
      (name "guile-charting")
      (version (git-version "0.2.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/wingo/guile-charting")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "03049g7wnpyfi0r36ij4a46kc9l45jbanx02iklkjwav2n6jqnnk"))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf automake texinfo pkg-config))
      (inputs (list guile-3.0))
      (propagated-inputs (list guile-cairo))
      (home-page "http://wingolog.org/projects/guile-charting/")
      (synopsis "Create charts and graphs in Guile")
      (description
       "Guile-Charting is a Guile Scheme library to create bar charts and graphs
using the Cairo drawing library.")
      (license license:lgpl2.1+))))

(define-public guile2.2-charting
  (package
    (inherit guile-charting)
    (name "guile2.2-charting")
    (inputs (list guile-2.2))
    (propagated-inputs (list guile2.2-cairo))))

(define-public ploticus
  (package
    (name "ploticus")
    (version "2.42")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/ploticus/ploticus/"
                                  version "/ploticus242_src.tar.gz"))
              (sha256
               (base32
                "1c70cvfvgjh83hj1x21130wb9qfr2rc0x47cxy9kl805yjwy8a9z"))
              (modules '((guix build utils)))
              (snippet
               ;; Install binaries in the right place.
               '(begin
                  (substitute* "src/Makefile"
                    (("INSTALLBIN =.*$")
                     (string-append "INSTALLBIN = $(out)/bin")))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure (lambda _ (chdir "src")))
         (add-before 'install 'make-target-directories
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let ((out (assoc-ref outputs "out")))
                         (mkdir-p (string-append out "/bin"))
                         #t)))
         (add-after 'install 'install-prefabs
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (dir (string-append out
                                                 "/share/ploticus/prefabs"))
                             (bin (string-append out "/bin")))
                        (mkdir-p dir)

                        ;; Install "prefabs".
                        (for-each (lambda (file)
                                    (let ((target
                                           (string-append dir "/"
                                                          (basename file))))
                                      (copy-file file target)))
                                  (find-files "../prefabs" "."))

                        ;; Allow them to be found.
                        (wrap-program (string-append bin "/pl")
                          `("PLOTICUS_PREFABS" ":" = (,dir)))))))))
    (inputs
     (list libpng libx11 zlib))
    (home-page "http://ploticus.sourceforge.net/")
    (synopsis "Command-line tool for producing plots and charts")
    (description
     "Ploticus is a non-interactive software package for producing plots,
charts, and graphics from data.  Ploticus is good for automated or
just-in-time graph generation, handles date and time data nicely, and has
basic statistical capabilities.  It allows significant user control over
colors, styles, options and details.")
    (license license:gpl2+)))

(define-public asymptote
  (package
    (name "asymptote")
    (version "2.77")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/asymptote/"
                           version "/asymptote-" version ".src.tgz"))
       (sha256
        (base32 "0v5r8g3b7f2dxsiba4f3yrgfkigr5nsdhg6jrdsnqrmf6y7dqgdf"))))
    (build-system gnu-build-system)
    ;; Note: The 'asy' binary retains a reference to docdir for use with its
    ;; "help" command in interactive mode, so adding a "doc" output is not
    ;; currently useful.
    (native-inputs
     (list autoconf
           automake
           boost
           emacs-minimal
           ghostscript                  ;for tests
           perl
           rapidjson
           texinfo                      ;for generating documentation
           (texlive-updmap.cfg
            (list texlive-amsfonts
                  texlive-dvips-l3backend
                  texlive-epsf
                  texlive-etoolbox
                  texlive-fonts-ec
                  texlive-generic-infwarerr
                  texlive-generic-kvdefinekeys
                  texlive-grfext
                  texlive-hyperref
                  texlive-latex-base
                  texlive-latex-geometry
                  texlive-latex-graphics
                  texlive-latex-kvoptions
                  texlive-latex-media9
                  texlive-latex-ocgx2
                  texlive-latex-pdftexcmds
                  texlive-oberdiek      ;for ifluatex
                  texlive-latex-parskip
                  texlive-tex-texinfo))))
    (inputs
     (list fftw
           freeglut
           glew
           glm
           gsl
           libgc
           libtirpc
           python
           python-cson
           python-numpy
           python-pyqt
           readline
           zlib))
    (arguments
     (list
      #:modules '((guix build emacs-utils)
                  (guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26))
      #:imported-modules `(,@%gnu-build-system-modules
                           (guix build emacs-utils))
      #:configure-flags
      #~(list (string-append "--enable-gc=" #$(this-package-input "libgc"))
              (string-append "--with-latex=" #$output "/share/texmf/tex/latex")
              (string-append "--with-context="
                             #$output
                             "/share/texmf/tex/context/third"))
      #:phases
      `(modify-phases %standard-phases
         (add-after 'unpack 'locate-tirpc
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (list "configure.ac")
               (("/usr/include/tirpc")
                (search-input-directory inputs "include/tirpc")))))
         (add-after 'unpack 'fix-includes
           (lambda _
             (substitute* (find-files "." "\\.in$")
               (("#include <primitives.h>") "#include \"primitives.h\""))
             (substitute* (find-files "prc" "\\.h$")
               (("#include \"config.h\"") "#include \"../config.h\""))
             (substitute* "prc/oPRCFile.h"
               (("#include \"xstream.h\"") "#include \"../xstream.h\""))
             (substitute* "v3dfile.h"
               (("#include <prc/oPRCFile.h>") "#include \"prc/oPRCFile.h\""))
             (substitute* "LspCpp/LibLsp/lsp/ParentProcessWatcher.cpp"
               (("#include <boost/process.hpp>" all)
                (string-append "#include <algorithm>\n" all)))))
         (replace 'bootstrap
           (lambda _
             (invoke "autoreconf" "-vfi")))
         (add-after 'unpack 'move-info-location
           ;; Build process installs info file in the unusual
           ;; "%out/share/info/asymptote/" location.  Move it to
           ;; "%out/share/info/" so it appears in the top-level directory.
           (lambda _
             (substitute* "doc/png/Makefile.in"
               (("(\\$\\(infodir\\))/asymptote" _ infodir) infodir))
             (substitute* "doc/asymptote.texi"
               (("asymptote/asymptote") "asymptote"))))
         (add-before 'build 'patch-pdf-viewer
           (lambda _
             ;; Default to a free pdf viewer.
             (substitute* "settings.cc"
               (("defaultPDFViewer=\"acroread\"")
                "defaultPDFViewer=\"gv\""))))
         (add-before 'check 'set-HOME
           ;; Some tests require write access to $HOME, otherwise leading to
           ;; "failed to create directory /homeless-shelter/.asy" error.
           (lambda _
             (setenv "HOME" "/tmp")))
         (add-after 'install 'install-Emacs-data
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Install related Emacs libraries into an appropriate location.
             (let* ((out (assoc-ref outputs "out"))
                    (lisp-dir (string-append out "/share/emacs/site-lisp")))
               (for-each (cut install-file <> lisp-dir)
                         (find-files "." "\\.el$"))
               (emacs-generate-autoloads ,name lisp-dir))))
         (add-after 'install-Emacs-data 'wrap-python-script
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make sure 'xasy' runs with the correct PYTHONPATH.
             (let* ((out (assoc-ref outputs "out"))
                    (path (getenv "GUIX_PYTHONPATH")))
               (wrap-program (string-append out "/share/asymptote/GUI/xasy.py")
                 `("GUIX_PYTHONPATH" ":" prefix (,path)))))))))
    (home-page "http://asymptote.sourceforge.net")
    (synopsis "Script-based vector graphics language")
    (description
     "Asymptote is a powerful descriptive vector graphics language for
technical drawing, inspired by MetaPost but with an improved C++-like syntax.
Asymptote provides for figures the same high-quality level of typesetting that
LaTeX does for scientific text.")
    ;; Most source files do not contain license statements, but the README
    ;; contains: "All source files in the Asymptote project, unless explicitly
    ;; noted otherwise, are released under version 3 (or later) of the GNU
    ;; Lesser General Public License"
    (license license:lgpl3+)))
