;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages xfig)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:select (bsd-2))
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages compression))

(define-public xfig
  (package
    (name "xfig")
    (version "3.2.8b")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/mcj/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0fndgbm1mkqb1sn2v2kj3nx9mxj70jbp31y2bjvzcmmkry0q3k5j"))))
    (build-system gnu-build-system)
    (native-inputs
     ;; For tests.
     (list desktop-file-utils ghostscript))
    (inputs
     `(("libxaw3d" ,libxaw3d)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libxpm" ,libxpm)
       ("libx11" ,libx11)
       ("libxt" ,libxt)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'strip-bogus-exec-prefix
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "xfig.desktop"
               ;; The patch-dot-desktop-files phase requires a relative name.
               (("Exec=/usr/bin/xfig") "Exec=xfig"))
             #t)))))
    (home-page "http://mcj.sourceforge.net/")
    (synopsis "Interactive drawing tool")
    (description
     "Xfig is an interactive drawing tool which runs under X Window System.
In xfig, figures may be drawn using objects such as circles, boxes, lines,
spline curves, text, etc.  It is also possible to import images in formats
such as GIF, JPEG, EPSF (PostScript), etc.  Those objects can be created,
deleted, moved or modified.  Attributes such as colors or line styles can be
selected in various ways.  For text, 35 fonts are available.")
    (license bsd-2)))

(define-public transfig
  (package
    (name "transfig")
    (version "3.2.5e")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/mcj/mcj-source/transfig."
                           version ".tar.gz"))
       (sha256
        (base32
         "0i3p7qmg2w8qrad3pn42b0miwarql7yy0gpd49b1bpal6bqsiicf"))))
    (build-system gnu-build-system)
    (native-inputs
     (list imake makedepend))
    (inputs
     `(("xfig"    ,xfig)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng"  ,libpng)
       ("libxpm"  ,libxpm)
       ("libx11"  ,libx11)
       ("zlib"    ,zlib)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((imake (assoc-ref inputs "imake"))
                   (out   (assoc-ref outputs "out")))
               (substitute* '("fig2dev/Imakefile"
                              "transfig/Imakefile")
                 (("XCOMM (BINDIR = )[[:graph:]]*" _ front)
                  (string-append front out "/bin"))
                 (("XCOMM USEINLINE") "USEINLINE")
                 ;; The variable name is deceptive.  The directory is used as an
                 ;; installation path for bitmaps.
                 (("(XFIGLIBDIR =[[:blank:]]*)[[:graph:]]*" _ front)
                  (string-append front out "/lib"))
                 (("(XPMLIBDIR = )[[:graph:]]*" _ front)
                  (string-append front (assoc-ref inputs "libxpm") "/lib"))
                 (("(XPMINC = -I)[[:graph:]]*" _ front)
                  (string-append front (assoc-ref inputs "libxpm") "/include/X11"))
                 (("/usr/local/lib/fig2dev") (string-append out "/lib")))
               ;; The -a argument is required in order to pick up the correct paths
               ;; to several X header files.
               (invoke "xmkmf" "-a")
               (substitute* '("Makefile"
                              "fig2dev/Makefile"
                              "transfig/Makefile")
                 ;; These imake variables somehow remain undefined
                 (("DefaultGcc2[[:graph:]]*Opt") "-O2")
                 ;; Reset a few variable defaults that are set in imake templates
                 ((imake) out)
                 (("(MANPATH = )[[:graph:]]*" _ front)
                  (string-append front out "/share/man"))
                 (("(CONFDIR = )([[:graph:]]*)" _ front default)
                  (string-append front out default)))
               #t)))
         (add-after 'install 'install/doc
           (lambda _
             (invoke "make" "install.man"))))))
    (home-page "http://mcj.sourceforge.net/")
    (synopsis "Create portable LaTeX figures")
    (description
     "Transfig creates a makefile to translate figures described in Fig code
or PIC into a specified LaTeX graphics language.  PIC files are identified by
the suffix \".pic\"; Fig files can be specified either with or without the
suffix \".fig\".  Transfig also creates a TeX macro file appropriate to the
target language.")
    (license bsd-2)))
