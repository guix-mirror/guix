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
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base) ;libbdf
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages code)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux) ; for alsa-utils
  #:use-module (gnu packages man)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio) ;libsndfile
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages zip))

(define-public hydrogen
  (package
    (name "hydrogen")
    (version "0.9.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/hydrogen/Hydrogen/"
                    (version-prefix version 3) "%20Sources/"
                    "hydrogen-" version ".tar.gz"))
              (sha256
               (base32
                "1fvyp6gfzcqcc90dmaqbm11p272zczz5pfz1z4lj33nfr7z0bqgb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no "check" target
       #:phases
       ;; TODO: Add scons-build-system and use it here.
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'scons-propagate-environment
                    (lambda _
                      ;; By design, SCons does not, by default, propagate
                      ;; environment variables to subprocesses.  See:
                      ;; <http://comments.gmane.org/gmane.linux.distributions.nixos/4969>
                      ;; Here, we modify the Sconstruct file to arrange for
                      ;; environment variables to be propagated.
                      (substitute* "Sconstruct"
                        (("^env = Environment\\(")
                         "env = Environment(ENV=os.environ, "))))
         (replace 'build
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    (let ((out (assoc-ref outputs "out")))
                      (zero? (system* "scons"
                                      (string-append "prefix=" out)
                                      "lrdf=0" ; cannot be found
                                      "lash=1")))))
         (add-before
          'install
          'fix-img-install
          (lambda _
            ;; The whole ./data/img directory is copied to the target first.
            ;; Scons complains about existing files when we try to install all
            ;; images a second time.
            (substitute* "Sconstruct"
              (("os.path.walk\\(\"./data/img/\",install_images,env\\)") ""))
            #t))
         (replace 'install (lambda _ (zero? (system* "scons" "install")))))))
    (native-inputs
     `(("scons" ,scons)
       ("python" ,python-2)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("zlib" ,zlib)
       ("libtar" ,libtar)
       ("alsa-lib" ,alsa-lib)
       ("jack" ,jack-1)
       ("lash" ,lash)
       ;;("lrdf" ,lrdf) ;FIXME: cannot be found by scons
       ("qt" ,qt-4)
       ("libsndfile" ,libsndfile)))
    (home-page "http://www.hydrogen-music.org")
    (synopsis "Drum machine")
    (description
     "Hydrogen is an advanced drum machine for GNU/Linux.  Its main goal is to
enable professional yet simple and intuitive pattern-based drum programming.")
    (license license:gpl2+)))

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

(define-public powertabeditor
  (package
    (name "powertabeditor")
    (version "2.0.0-alpha8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/powertab/powertabeditor/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gaa2x209v3azql8ak3r1n9a9qbxjx2ssirvwdxwklv2lmfqkm82"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove bundled sources for external libraries
                  (delete-file-recursively "external")
                  (substitute* "CMakeLists.txt"
                    (("include_directories\\(\\$\\{PROJECT_SOURCE_DIR\\}/external/.*") "")
                    (("add_subdirectory\\(external\\)") ""))
                  (substitute* "test/CMakeLists.txt"
                    (("include_directories\\(\\$\\{PROJECT_SOURCE_DIR\\}/external/.*") ""))

                  ;; Add install target
                  (substitute* "source/CMakeLists.txt"
                    (("qt5_use_modules")
                     (string-append
                      "install(TARGETS powertabeditor "
                      "RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)\n"
                      "install(FILES data/tunings.json DESTINATION "
                      "${CMAKE_INSTALL_PREFIX}/share/powertabeditor/)\n"
                      "qt5_use_modules")))
                  #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:modules ((guix build cmake-build-system)
                  (guix build utils)
                  (ice-9 match))
       #:configure-flags
       ;; CMake appears to lose the RUNPATH for some reason, so it has to be
       ;; explicitly set with CMAKE_INSTALL_RPATH.
       (list "-DCMAKE_BUILD_WITH_INSTALL_RPATH=TRUE"
             "-DCMAKE_ENABLE_PRECOMPILED_HEADERS=OFF" ; if ON pte_tests cannot be built
             (string-append "-DCMAKE_INSTALL_RPATH="
                            (string-join (map (match-lambda
                                                ((name . directory)
                                                 (string-append directory "/lib")))
                                              %build-inputs) ";")))
       #:phases
       (modify-phases %standard-phases
         (replace
          'check
          (lambda _
            (zero? (system* "bin/pte_tests"
                            ;; Exclude this failing test
                            "~Formats/PowerTabOldImport/Directions"))))
         (add-before
          'configure 'fix-tests
          (lambda _
            ;; Tests cannot be built with precompiled headers
            (substitute* "test/CMakeLists.txt"
              (("cotire\\(pte_tests\\)") ""))
            #t))
         (add-before
          'configure 'remove-third-party-libs
          (lambda* (#:key inputs #:allow-other-keys)
            ;; Link with required static libraries, because we're not
            ;; using the bundled version of withershins.
            (substitute* '("source/CMakeLists.txt"
                           "test/CMakeLists.txt")
              (("target_link_libraries\\((powertabeditor|pte_tests)" _ target)
               (string-append "target_link_libraries(" target " "
                              (assoc-ref inputs "binutils")
                              "/lib/libbfd.a "
                              (assoc-ref inputs "libiberty")
                              "/lib/libiberty.a "
                              "dl")))
            #t)))))
    (inputs
     `(("boost" ,boost)
       ("alsa-lib" ,alsa-lib)
       ("qt" ,qt)
       ("withershins" ,withershins)
       ("libiberty" ,libiberty) ;for withershins
       ("binutils" ,binutils) ;for -lbfd and -liberty (for withershins)
       ("timidity" ,timidity++)
       ("pugixml" ,pugixml)
       ("rtmidi" ,rtmidi)
       ("rapidjson" ,rapidjson)
       ("zlib" ,zlib)))
    (native-inputs
     `(("catch" ,catch-framework)
       ("pkg-config" ,pkg-config)))
    (home-page "http://powertabs.net")
    (synopsis "Guitar tablature editor")
    (description
     "Power Tab Editor 2.0 is the successor to the famous original Power Tab
Editor.  It is compatible with Power Tab Editor 1.7 and Guitar Pro.")
    (license license:gpl3+)))

(define-public setbfree
  (package
    (name "setbfree")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://github.com/pantherb/setBfree/releases/download/v"
                version "/setbfree-" version ".tar.gz"))
              (sha256
               (base32
                "045bgp7qsigpbrhk7qvgvliwiy26sajifwn7f2jvk90ckfqnlw4b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "FONTFILE="
                            (assoc-ref %build-inputs "font-bitstream-vera")
                            "/share/fonts/truetype/VeraBd.ttf"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-CC-variable
                     (lambda _ (setenv "CC" "gcc") #t))
         (delete 'configure))))
    (inputs
     `(("jack" ,jack-1)
       ("lv2" ,lv2)
       ("zita-convolver" ,zita-convolver)
       ("glu" ,glu)
       ("ftgl" ,ftgl)
       ("font-bitstream-vera" ,font-bitstream-vera)))
    (native-inputs
     `(("help2man" ,help2man)
       ("pkg-config" ,pkg-config)))
    (home-page "http://setbfree.org")
    (synopsis "Tonewheel organ")
    (description
     "setBfree is a MIDI-controlled, software synthesizer designed to imitate
the sound and properties of the electromechanical organs and sound
modification devices that brought world-wide fame to the names and products of
Laurens Hammond and Don Leslie.")
    (license license:gpl2+)))

(define-public tuxguitar
  (package
    (name "tuxguitar")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/tuxguitar/TuxGuitar/TuxGuitar-"
                    version "/tuxguitar-src-" version ".tar.gz"))
              (sha256
               (base32
                "1g1yf2gd06fzdhqb8kb8dmdcmr602s9y24f01kyl4940wimgr944"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                         (assoc-ref %outputs "out") "/lib")
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out"))
                          (string-append "SWT_PATH="
                                         (assoc-ref %build-inputs "swt")
                                         "/share/java/swt.jar"))
       #:tests? #f ;no "check" target
       #:parallel-build? #f ;not supported
       #:phases
       (alist-cons-before
        'build 'enter-dir-set-path-and-pass-ldflags
        (lambda* (#:key inputs #:allow-other-keys)
          (chdir "TuxGuitar")
          (substitute* "GNUmakefile"
            (("PROPERTIES\\?=")
             (string-append "PROPERTIES?= -Dswt.library.path="
                            (assoc-ref inputs "swt") "/lib"))
            (("\\$\\(GCJ\\) -o") "$(GCJ) $(LDFLAGS) -o"))
          #t)
        (alist-delete 'configure %standard-phases))))
    (inputs
     `(("swt" ,swt)))
    (native-inputs
     `(("gcj" ,gcj-4.8)
       ("pkg-config" ,pkg-config)))
    (home-page "http://tuxguitar.com.ar")
    (synopsis "Multitrack tablature editor and player")
    (description
     "TuxGuitar is a guitar tablature editor with player support through midi.
It can display scores and multitrack tabs.  TuxGuitar provides various
additional features, including autoscrolling while playing, note duration
management, bend/slide/vibrato/hammer-on/pull-off effects, support for
tuplets, time signature management, tempo management, gp3/gp4/gp5 import and
export.")
    (license license:lgpl2.1+)))
