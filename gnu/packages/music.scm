;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages music)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages linux) ; for alsa-utils
  #:use-module (gnu packages man)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages zip))

(define-public lilypond
  (package
    (name "lilypond")
    (version "2.18.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://download.linuxaudio.org/lilypond/sources/v"
                    (version-major+minor version) "/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "01xs9x2wjj7w9appaaqdhk15r1xvvdbz9qwahzhppfmhclvp779j"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Tests fail with this error:
       ;; Undefined subroutine &main::get_index called at
       ;; ./lilypond-2.18.2/Documentation/lilypond-texi2html.init line 2127.
       #:tests? #f
       #:out-of-source? #t
       #:phases
       (alist-cons-before
        'configure 'prepare-configuration
        (lambda _
          (substitute* "configure"
            (("SHELL=/bin/sh") "SHELL=sh"))
          (setenv "out" "")
          #t)
        %standard-phases)))
    (inputs
     `(("guile" ,guile-1.8)
       ("font-dejavu" ,font-dejavu)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("ghostscript" ,ghostscript)
       ("pango" ,pango)
       ("python" ,python-2)))
    (native-inputs
     `(("bison" ,bison)
       ("perl" ,perl)
       ("flex" ,flex)
       ("fontforge" ,fontforge)
       ("dblatex" ,dblatex)
       ("gettext" ,gnu-gettext)
       ("imagemagick" ,imagemagick)
       ("netpbm" ,netpbm) ;for pngtopnm
       ("texlive" ,texlive) ;metafont and metapost
       ("texinfo" ,texinfo)
       ("texi2html" ,texi2html)
       ("rsync" ,rsync)
       ("pkg-config" ,pkg-config)
       ("zip" ,zip)))
    (home-page "http://www.lilypond.org/")
    (synopsis "Music typesetting")
    (description
     "GNU LilyPond is a music typesetter, which produces high-quality sheet
music.  Music is input in a text file containing control sequences which are
interpreted by LilyPond to produce the final document.  It is extendable with
Guile.")
    (license license:gpl3+)))

(define-public solfege
  (package
    (name "solfege")
    (version "3.22.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnu/solfege/solfege-"
                    version ".tar.xz"))
              (sha256
               (base32
                "1w25rxdbj907nsx285k9nm480pvy12w3yknfh4n1dfv17cwy072i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; xmllint attempts to download DTD
       #:test-target "test"
       #:phases
       (alist-cons-after
        'unpack 'fix-configuration
        (lambda* (#:key inputs #:allow-other-keys)
          (substitute* "default.config"
            (("csound=csound")
             (string-append "csound="
                            (assoc-ref inputs "csound")
                            "/bin/csound"))
            (("/usr/bin/aplay")
             (string-append (assoc-ref inputs "aplay")
                            "/bin/aplay"))
            (("/usr/bin/timidity")
             (string-append (assoc-ref inputs "timidity")
                            "/bin/timidity"))
            (("/usr/bin/mpg123")
             (string-append (assoc-ref inputs "mpg123")
                            "/bin/mpg123"))
            (("/usr/bin/ogg123")
             (string-append (assoc-ref inputs "ogg123")
                            "/bin/ogg123"))))
        (alist-cons-before
         'build 'patch-python-shebangs
         (lambda _
           ;; Two python scripts begin with a Unicode BOM, so patch-shebang
           ;; has no effect.
           (substitute* '("solfege/parsetree.py"
                          "solfege/presetup.py")
             (("#!/usr/bin/python") (string-append "#!" (which "python")))))
         (alist-cons-before
          'build 'add-sitedirs
          ;; .pth files are not automatically interpreted unless the
          ;; directories containing them are added as "sites".  The directories
          ;; are then added to those in the PYTHONPATH.  This is required for
          ;; the operation of pygtk and pygobject.
          (lambda _
            (substitute* "run-solfege.py"
              (("import os")
               "import os, site
for path in [path for path in sys.path if 'site-packages' in path]: site.addsitedir(path)")))
          (alist-cons-before
           'build 'adjust-config-file-prefix
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "run-solfege.py"
               (("prefix = os.path.*$")
                (string-append "prefix = " (assoc-ref outputs "out")))))
           (alist-cons-after
            'install 'wrap-program
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; Make sure 'solfege' runs with the correct PYTHONPATH.  We
              ;; also need to modify GDK_PIXBUF_MODULE_FILE for SVG support.
              (let* ((out (assoc-ref outputs "out"))
                     (path (getenv "PYTHONPATH"))
                     (rsvg (assoc-ref inputs "librsvg"))
                     (pixbuf (find-files rsvg "^loaders\\.cache$")))
                (wrap-program (string-append out "/bin/solfege")
                  `("PYTHONPATH" ":" prefix (,path))
                  `("GDK_PIXBUF_MODULE_FILE" ":" prefix ,pixbuf))))
            %standard-phases)))))))
    (inputs
     `(("python" ,python-2)
       ("pygtk" ,python2-pygtk)
       ("gettext" ,gnu-gettext)
       ("gtk" ,gtk+)
       ;; TODO: Lilypond is optional.  Produces errors at build time:
       ;;   Drawing systems...Error: /undefinedresult in --glyphshow--
       ;; Fontconfig is needed to fix one of the errors, but other similar
       ;; errors remain.
       ;;("lilypond" ,lilypond)
       ("librsvg" ,librsvg) ; needed at runtime for icons
       ("libpng" ,libpng) ; needed at runtime for icons
       ;; players needed at runtime
       ("aplay" ,alsa-utils)
       ("csound" ,csound) ; optional, needed for some exercises
       ("mpg123" ,mpg123)
       ("ogg123" ,vorbis-tools)
       ("timidity" ,timidity++)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("txt2man" ,txt2man)
       ("libxml2" ,libxml2) ; for tests
       ("ghostscript" ,ghostscript)
       ;;("fontconfig" ,fontconfig) ; only needed with lilypond
       ;;("freetype" ,freetype) ; only needed with lilypond
       ("texinfo" ,texinfo)))
    (home-page "https://www.gnu.org/software/solfege/")
    (synopsis "Ear training")
    (description
     "GNU Solfege is a program for practicing musical ear-training.  With it,
you can practice your recognition of various musical intervals and chords.  It
features a statistics overview so you can monitor your progress across several
sessions.  Solfege is also designed to be extensible so you can easily write
your own lessons.")
    (license license:gpl3+)))
