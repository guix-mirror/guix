;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages compression))

(define-public xfig
  (package
    (name "xfig")
    (version "3.2.5c")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/mcj/mcj-source/xfig."
                          version ".full.tar.gz"))
      (sha256
       (base32
        "1yd1jclvw5w3ja4jjzr1ysbn8iklh88wq84jn9d1gavrbfbqyqpa"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("imake" ,imake)
       ("makedepend" ,makedepend)
       ("groff" ,groff)))               ;for creating some doc
    (inputs
     `(("libxaw3d" ,libxaw3d)
       ;; Requires libjpeg>=9a, otherwise jmorecfg.h define an enum FALSE that
       ;; conflicts with the FALSE macro from X11/Intrinsic.h
       ("libjpeg"  ,libjpeg)
       ("libpng"   ,libpng)
       ("libxpm"   ,libxpm)
       ("libx11"   ,libx11)
       ("libxmu"   ,libxmu)
       ("libxt"    ,libxt)
       ("zlib"     ,zlib)))
    (arguments
     `(#:tests? #f
       #:phases
       (alist-replace
        'configure
        (lambda* (#:key inputs outputs #:allow-other-keys)
          (let ((imake (assoc-ref inputs "imake"))
                (out   (assoc-ref outputs "out")))
           (substitute* "Imakefile"
             (("XCOMM (BINDIR = )[[:graph:]]*" _ front)
              (string-append front out "/bin"))
             (("(PNGLIBDIR = )[[:graph:]]*" _ front)
              (string-append front (assoc-ref inputs "libpng") "/lib"))
             (("(PNGINC = -I)[[:graph:]]*" _ front)
              (string-append front (assoc-ref inputs "libpng") "/include"))
             (("(JPEGLIBDIR = )[[:graph:]]*" _ front)
              (string-append front (assoc-ref inputs "libjpeg") "/lib"))
             (("(JPEGINC = -I)[[:graph:]]*" _ front)
              (string-append front (assoc-ref inputs "libjpeg") "/include"))
             (("(ZLIBDIR = )[[:graph:]]*" _ front)
              (string-append front (assoc-ref inputs "zlib") "/lib"))
             (("(XPMLIBDIR = )[[:graph:]]*" _ front)
              (string-append front (assoc-ref inputs "libxpm") "/lib"))
             (("(XPMINC = -I)[[:graph:]]*" _ front)
              (string-append front (assoc-ref inputs "libxpm") "/include"))
             (("(XFIGLIBDIR = )[[:graph:]]*" _ front)
              (string-append front out "/lib"))
             (("(XFIGDOCDIR = )[[:graph:]]*" _ front)
              (string-append front out "/share/doc"))
             (("XCOMM USEINLINE") "USEINLINE"))
           ;; The -a argument is required in order to pick up the correct paths
           ;; to several X header files.
           (zero? (system* "xmkmf" "-a"))
           ;; Reset some variables that are inherited from imake templates
           (substitute* "Makefile"
             ;; These imake variables somehow remain undefined
             (("DefaultGcc2[[:graph:]]*Opt") "-O2")
             ;; Reset a few variable defaults that are set in imake templates
             ((imake) out)
             (("(MANPATH = )[[:graph:]]*" _ front)
              (string-append front out "/share/man"))
             (("(CONFDIR = )([[:graph:]]*)" _ front default)
              (string-append front out default)))))
        (alist-cons-after
         'install 'install/libs
         (lambda _
           (zero? (system* "make" "install.libs")))
         (alist-cons-after
          'install 'install/doc
          (lambda _
            (begin
              ;; The Doc/xfig_man.html file is expected by the install.html
              ;; target, but is not present in the tarball, so generate it.
              (use-modules (ice-9 popen))
              (let* ((in  (open-pipe* OPEN_READ
                                      "groff" "-mandoc" "-Thtml"
                                      "Doc/xfig.man"))
                     (out (open-output-file "Doc/xfig_man.html")))
                (begin
                  (dump-port in out)
                  (close-pipe in)
                  (close-port out)))
              (zero? (system* "make" "install.doc"))))
          %standard-phases)))))
    (home-page "http://xfig.org/")
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
     `(("imake" ,imake)
       ("makedepend" ,makedepend)))
    (inputs
     `(("xfig"    ,xfig)
       ("libjpeg" ,libjpeg)
       ("libpng"  ,libpng)
       ("libxpm"  ,libxpm)
       ("libx11"  ,libx11)
       ("zlib"    ,zlib)))
    (arguments
     `(#:tests? #f
       #:phases
       (alist-replace
        'configure
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
           (zero? (system* "xmkmf" "-a"))
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
              (string-append front out default)))))
        (alist-cons-after
         'install 'install/doc
         (lambda _
           (zero? (system* "make" "install.man")))
         %standard-phases))))
    (home-page "http://www.xfig.org/")
    (synopsis "Create portable LaTeX figures")
    (description
     "Transfig creates a makefile to translate figures described in Fig code
or PIC into a specified LaTeX graphics language.  PIC files are identified by
the suffix \".pic\"; Fig files can be specified either with or without the
suffix \".fig\".  Transfig also creates a TeX macro file appropriate to the
target language.")
    (license bsd-2)))
