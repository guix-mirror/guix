;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2016 Al McElrath <hello@yrns.org>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016 John J. Foerch <jjfoerch@earthlink.net>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2017 Rodger Fox <thylakoid@openmailbox.org>
;;; Copyright © 2017, 2018, 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2017, 2018 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 nee <nee.git@hidamari.blue>
;;; Copyright © 2018 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2019 Gabriel Hondet <gabrielhondet@gmail.com>
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
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system scons)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system waf)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system go)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages apr)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base) ;libbdf
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages code)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux) ; for alsa-utils
  #:use-module (gnu packages lirc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages man)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-web)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio) ;libsndfile
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages lua)
  #:use-module ((srfi srfi-1) #:select (last)))

(define-public aria-maestosa
  (package
    (name "aria-maestosa")
    (version "1.4.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/ariamaestosa/ariamaestosa/"
                                  version "/AriaSrc-" version ".tar.bz2"))
              (sha256
               (base32
                "1cs3z6frx2ch7rm5ammx9p0rxcjrbj1vq14hvcbimpaw39rdsn3d"))))
    (build-system scons-build-system)
    (arguments
     `(#:tests? #f  ;no tests
       #:scons-flags
       (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:scons ,scons-python2
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'scons-propagate-environment
           (lambda _
             ;; By design, SCons does not, by default, propagate
             ;; environment variables to subprocesses.  See:
             ;; <http://comments.gmane.org/gmane.linux.distributions.nixos/4969>
             ;; Here, we modify the SConstruct file to arrange for
             ;; environment variables to be propagated.
             (substitute* "SConstruct"
               (("env = Environment\\(\\)")
                "env = Environment(ENV=os.environ)")
               ;; Scons errors out when copying subdirectories from Resources,
               ;; so we move them instead.
               (("Copy") "Move")
               ;; We move the "score" and "Documentation" directories at once,
               ;; so we have to ignore files contained therein.
               (("if \".svn\" in file" line)
                (string-append line
                               " or \"score/\" in file"
                               " or \"Documentation/\" in file")))
             #t))
         (add-after 'install 'fix-directory-permissions
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (chmod (string-append out "/share/Aria/Documentation") #o555)
               (chmod (string-append out "/share/Aria/score") #o555)
               #t))))))
    (inputs
     `(("wxwidgets" ,wxwidgets)
       ("glib" ,glib)
       ("alsa-lib" ,alsa-lib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://ariamaestosa.sourceforge.net/")
    (synopsis "MIDI sequencer and editor")
    (description
     "Aria Maestosa is a MIDI sequencer and editor.  It lets you compose, edit
and play MIDI files with a few clicks in a user-friendly interface offering
score, keyboard, guitar, drum and controller views.")
    (license license:gpl3+)))

;; We don't use the latest release because it depends on Qt4.  Instead we
;; download the sources from the tip of the "qt5" branch.
(define-public clementine
  (let ((commit "4619a4c1ab3b17b13d4b2327ad477912917eaf36")
        (revision "2"))
    (package
      (name "clementine")
      (version (git-version "1.3.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/clementine-player/Clementine.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1hximk3q0p8nw8is5w7215xgkb7k4flnfyr0pdz9svfwvcm05w1i"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    (for-each
                     (lambda (dir)
                       (delete-file-recursively
                        (string-append "3rdparty/" dir)))
                     (list
                      ;; TODO: The following dependencies are still bundled:
                      ;; - "qxt": Appears to be unmaintained upstream.
                      ;; - "qsqlite"
                      ;; - "qtsingleapplication"
                      ;; - "qocoa"
                      ;; - "qtiocompressor"
                      ;; - "gmock": The tests crash when using our googletest
                      ;;   package instead of the bundled gmock.
                      "SPMediaKeyTap"
                      "fancytabwidget"
                      "google-breakpad"
                      "libmygpo-qt"
                      "libmygpo-qt5"
                      "libprojectm"
                      "qtwin"
                      "sha2" ;; Replaced by openssl.
                      "taglib"
                      "tinysvcmdns"))
                    #t))
                (patches (search-patches "clementine-use-openssl.patch"
                                         "clementine-remove-crypto++-dependency.patch"
                                         "clementine-fix-sqlite.patch"))))
      (build-system cmake-build-system)
      (arguments
       '(#:test-target "clementine_test"
         #:configure-flags
         (list ;; Requires unpackaged "projectm"
               "-DENABLE_VISUALISATIONS=OFF"
               ;; Otherwise it may try to download a non-free library at run-time.
               ;; TODO In an origin snippet, remove the code that performs the
               ;; download.
               "-DHAVE_SPOTIFY_DOWNLOADER=FALSE"
               "-DUSE_SYSTEM_SHA2=TRUE")
         #:phases
         (modify-phases %standard-phases
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out             (assoc-ref outputs "out"))
                     (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH")))
                 (wrap-program (string-append out "/bin/clementine")
                   `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path)))
                 #t))))))
      (native-inputs
       `(("gettext" ,gettext-minimal)
         ("pkg-config" ,pkg-config)
         ("qtlinguist" ,qttools)))
      (inputs
       `(("boost" ,boost)
         ("chromaprint" ,chromaprint)
         ("fftw" ,fftw)
         ("glib" ,glib)
         ("glu" ,glu)
         ("gstreamer" ,gstreamer)
         ("gst-plugins-base" ,gst-plugins-base)
         ("gst-plugins-good" ,gst-plugins-good)
         ("gst-libav" ,gst-libav)
         ("libcdio" ,libcdio)
         ("libmygpo-qt" ,libmygpo-qt)
         ;; TODO: Package libgpod.
         ("libmtp" ,libmtp)
         ("libxml2" ,libxml2)
         ("openssl" ,openssl)
         ("protobuf" ,protobuf)
         ("pulseaudio" ,pulseaudio)
         ("qtbase" ,qtbase)
         ("qtx11extras" ,qtx11extras)
         ("sqlite" ,sqlite-with-column-metadata)
         ("sparsehash" ,sparsehash)
         ("taglib" ,taglib)))
      (home-page "https://clementine-player.org")
      (synopsis "Music player and library organizer")
      (description "Clementine is a multiplatform music player.  It is inspired
by Amarok 1.4, focusing on a fast and easy-to-use interface for searching and
playing your music.")
      (license (list
                 ;; clementine and qtiocompressor are under GPLv3.
                 license:gpl3+
                 ;; gmock is under BSD-3.
                 license:bsd-3
                 ;; qxt is under CPL1.0.
                 license:cpl1.0
                 ;; qsqlite and qtsingleapplication are under LGPL2.1+.
                 license:lgpl2.1+
                 ;; qocoa is under MIT and CC by-sa for the icons.
                 license:cc-by-sa3.0)))))

(define-public cmus
  (package
    (name "cmus")
    (version "2.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cmus/cmus.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xd96py21bl869qlv1353zw7xsgq6v5s8szr0ldr63zj5fgc2ps5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; cmus does not include tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; It's an idiosyncratic configure script that doesn't
               ;; understand --prefix=..; it wants prefix=.. instead.
               (invoke "./configure"
                       (string-append "prefix=" out))
               #t))))))
    ;; TODO: cmus optionally supports the following formats, which haven't yet
    ;; been added to Guix:
    ;;
    ;; - Roar, libroar
    ;;
    ;; - DISCID_LIBS, apparently different from cd-discid which is included in
    ;;   Guix.  See <http://sourceforge.net/projects/discid/>
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("ao" ,ao)
       ("ffmpeg" ,ffmpeg)
       ("flac" ,flac)
       ("jack" ,jack-1)
       ("libcddb" ,libcddb)
       ("libcdio-paranoia" ,libcdio-paranoia)
       ("libcue" ,libcue)
       ("libmad" ,libmad)
       ("libmodplug" ,libmodplug)
       ("libmpcdec" ,libmpcdec)
       ("libsamplerate" ,libsamplerate)
       ("libvorbis" ,libvorbis)
       ("ncurses" ,ncurses)
       ("opusfile" ,opusfile)
       ("pulseaudio" ,pulseaudio)
       ("wavpack" ,wavpack)))
     (home-page "https://cmus.github.io/")
     (synopsis "Small console music player")
     (description "Cmus is a small and fast console music player.  It supports
many input formats and provides a customisable Vi-style user interface.")
     (license license:gpl2+)))

(define-public denemo
  (package
    (name "denemo")
    (version "2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/denemo/denemo-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0hggf8c4xcrjcxd5m00r788r7jg7g8ff54w2idfaqpj5j2ix3299"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           ;; Denemo's documentation says to use this command to run its
           ;; testsuite.
           (lambda _
             (invoke "make" "-C" "tests" "check")))
         (add-before 'build 'set-lilypond
           ;; This phase sets the default path for lilypond to its current
           ;; location in the store.
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((lilypond (string-append (assoc-ref inputs "lilypond")
                                             "/bin/lilypond")))
               (substitute* "src/core/prefops.c"
                 (("g_string_new \\(\"lilypond\"\\);")
                  (string-append "g_string_new (\""
                                 lilypond
                                 "\");"))))
             #t))
         (add-after 'install 'correct-filename
           ;; "graft-derivation/shallow" from the (guix grafts) module runs in
           ;; the C locale, expecting file names to be ASCII encoded. This
           ;; phase renames a filename with a Unicode character in it to meet
           ;; the aforementioned condition.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (chdir (string-append
                       out
                       "/share/denemo/templates/instruments/woodwind"))
               (rename-file "Clarinet in B♭.denemo"
                            "Clarinet in Bb.denemo"))
             #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin")         ; for gtester
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("aubio" ,aubio)
       ("evince" ,evince)
       ("fftw" ,fftw)
       ("fluidsynth" ,fluidsynth)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("gtk-doc" ,gtk-doc)
       ("gtksourceview" ,gtksourceview)
       ("guile" ,guile-2.0)
       ("intltool" ,intltool)
       ("librsvg" ,librsvg)
       ("libsndfile" ,libsndfile)
       ("libtool" ,libtool)
       ("libxml2" ,libxml2)
       ("lilypond" ,lilypond)
       ("portaudio" ,portaudio)
       ("portmidi" ,portmidi)
       ("rubberband" ,rubberband)))
    (synopsis "Graphical music notation, front-end to GNU Lilypond")
    (description
     "GNU Denemo is a music notation editor that provides a convenient
interface to the powerful music engraving program Lilypond.  Music can be
typed in using the computer keyboard, played in using a MIDI keyboard, or
even input via a microphone connected to the sound card.  The final product
is publication-quality music notation that is continuously generated in the
background while you work.")
    (home-page "http://www.denemo.org")
    (license license:gpl3+)))

(define-public hydrogen
  (package
    (name "hydrogen")
    (version "1.0.0-beta1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hydrogen-music/hydrogen.git")
                    (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0nv83l70j5bjz2wd6n3a8cq3bmgrvdvg6g2hjhc1g5h6xnbqsh9x"))))
    (build-system cmake-build-system)
    (arguments
     `(#:test-target "tests"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-data-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("/usr/share/pixmaps")
                (string-append (assoc-ref outputs "out")
                               "/share/pixmaps")))
             #t)))))
    (native-inputs
     `(("cppunit" ,cppunit)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("jack" ,jack-1)
       ;; ("ladspa" ,ladspa) ; cannot find during configure
       ("lash" ,lash)
       ("libarchive" ,libarchive)
       ("libsndfile" ,libsndfile)
       ("libtar" ,libtar)
       ("lrdf" ,lrdf)
       ("qtbase" ,qtbase)
       ("qtxmlpatterns" ,qtxmlpatterns)
       ("zlib" ,zlib)))
    (home-page "http://www.hydrogen-music.org")
    (synopsis "Drum machine")
    (description
     "Hydrogen is an advanced drum machine for GNU/Linux.  Its main goal is to
enable professional yet simple and intuitive pattern-based drum programming.")
    (license license:gpl2+)))

(define-public easytag
  (package
    (name "easytag")
    (version "2.4.3")
    (source (origin
             (method url-fetch)
              (uri (string-append "mirror://gnome/sources/easytag/2.4/easytag-"
                     version ".tar.xz"))
             (sha256
              (base32
               "1mbxnqrw1fwcgraa1bgik25vdzvf97vma5pzknbwbqq5ly9fwlgw"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("glib" ,glib "bin")
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("flac" ,flac)
       ("gtk+" ,gtk+)
       ("id3lib" ,id3lib)
       ("libid3tag" ,libid3tag)
       ("libvorbis" ,libvorbis)
       ("opusfile" ,opusfile)
       ("speex" ,speex)
       ("taglib" ,taglib)
       ("wavpack" ,wavpack)
       ("yelp" ,yelp)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'configure-libid3tag
           (lambda* (#:key inputs #:allow-other-keys)
             ;; libid3tag does not provide a .pc file and EasyTAG's configure
             ;; script healivy relies on pkg-config.  Providing a temporary
             ;; local .pc file is easier than patching the configure script.
             (let* ((libid3tag (assoc-ref inputs "libid3tag")))
               (mkdir-p "pkgconfig")
               (with-output-to-file
                 "pkgconfig/id3tag.pc"
                 (lambda _
                   (format #t
                     "prefix=~@*~a~@
                      libdir=${prefix}/lib~@
                      includedir=${prefix}/include~@
                      Libs: -L${libdir} -lid3tag -lz~@
                      Cflags: -I${includedir}~%"
                     libid3tag)))
               (setenv "PKG_CONFIG_PATH"
                 (string-append (getenv "PKG_CONFIG_PATH")
                   ":" (getcwd) "/pkgconfig")))))
         (add-after 'unpack 'patch-makefile
           (lambda _
             (substitute* "Makefile.in"
               ;; The Makefile generates a test-desktop-file-validate.sh
               ;; script with /bin/sh hard-coded.
               (("/bin/sh") (which "sh"))
               ;; Don't create 'icon-theme.cache'.
               (("gtk-update-icon-cache") "true")))))))
    (home-page "https://wiki.gnome.org/Apps/EasyTAG")
    (synopsis "Simple application for viewing and editing tags in audio files")
    (description
      "EasyTAG is an application for viewing and editing tags in audio files.
It supports MP3, MP2, MP4/AAC, FLAC, Ogg Opus, Ogg Speex, Ogg Vorbis,
MusePack, Monkey's Audio, and WavPack files.")
    (license license:gpl2+)))

(define-public extempore
  (package
    (name "extempore")
    (version "0.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/digego/extempore.git")
                    (commit version)))
              (sha256
               (base32
                "12fsp7zkfxb9kykwq46l88kcbbici9arczrrsl4qn87m6vm5349l"))
              (file-name (string-append name "-" version "-checkout"))))
    (build-system cmake-build-system)
    (arguments
     `(;; The default target also includes ahead-of-time compilation of the
       ;; standard libraries.  However, during the "install" phase this would
       ;; happen *again* for unknown reasons.  Hence we only build the
       ;; extempore executable during the build phase.
       #:make-flags '("extempore")
       #:configure-flags '("-DJACK=ON"
                           ;; We want to distribute.
                           "-DIN_TREE=OFF"
                           ;; Don't download any dependencies.
                           "-DBUILD_DEPS=OFF")
       #:modules ((ice-9 match)
                  (guix build cmake-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-directories
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Rewrite default path to runtime directory
             (substitute* "src/Extempore.cpp"
               (("runtimedir \\+= \"runtime\"")
                (string-append "runtimedir = \""
                               (assoc-ref outputs "out")
                               "/lib/extempore/runtime\"")))
             (substitute* "extras/extempore.el"
               (("\\(runtime-directory \\(concat default-directory \"runtime\"\\)\\)")
                (string-append "(runtime-directory \""
                               (assoc-ref outputs "out")
                               "/lib/extempore/runtime"
                               "\")")))
             #t))
         (add-after 'unpack 'link-with-additional-libs
           (lambda _
             ;; The executable must be linked with libffi and zlib.
             (substitute* "CMakeLists.txt"
               (("add_dependencies\\(aot_extended extended_deps\\)") "")
               (("target_link_libraries\\(extempore PRIVATE dl" line)
                (string-append line " ffi z")))
             #t))
         ;; FIXME: AOT compilation of the nanovg bindings fail with the error:
         ;; "Compiler Error  could not bind _nvgLinearGradient"
         (add-after 'unpack 'disable-nanovg
           (lambda _
             (substitute* "CMakeLists.txt"
               (("aotcompile_lib\\(libs/external/nanovg.xtm.*") ""))
             #t))
         ;; FIXME: All examples that are used as tests segfault for some
         ;; unknown reason.
         (add-after 'unpack 'disable-broken-tests
           (lambda _
             (substitute* "CMakeLists.txt"
               (("extempore_add_example_as_test\\(.*") ""))
             #t))
         (add-after 'unpack 'hardcode-external-lib-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (use-modules (ice-9 match))
             (for-each
              (match-lambda
                ((file-name lib pkg-name)
                 (substitute* (string-append "libs/external/" file-name ".xtm")
                   ((lib) (string-append (assoc-ref inputs pkg-name)
                                         "/lib/" lib)))))
              '(("assimp"    "libassimp.so"    "assimp")
                ("portmidi"  "libportmidi.so"  "portmidi")
                ("sndfile"   "libsndfile.so"   "libsndfile")
                ("fft"       "libkiss_fft.so"  "kiss-fft")
                ("stb_image" "libstb_image.so" "stb-image")
                ("nanovg"    "libnanovg.so"    "nanovg")
                ("glext"     "libGL.so"        "mesa")
                ("glfw3"     "libglfw.so"      "glfw")
                ("gl/glcore-directbind"   "libGL.so" "mesa")
                ("gl/glcompat-directbind" "libGL.so" "mesa")))
             #t))
         (add-after 'unpack 'use-own-llvm
          (lambda* (#:key inputs #:allow-other-keys)
            (setenv "EXT_LLVM_DIR" (assoc-ref inputs "llvm"))
            ;; Our LLVM builds shared libraries, so Extempore should use
            ;; those.
            (substitute* "CMakeLists.txt"
              (("CMAKE_STATIC_LIBRARY") "CMAKE_SHARED_LIBRARY"))
            #t))
         (add-after 'unpack 'fix-aot-compilation
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               ;; EXT_SHARE_DIR does not exist before installation, so the
               ;; working directory should be the source directory instead.
               (("WORKING_DIRECTORY \\$\\{EXT_SHARE_DIR\\}")
                "WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}")
               ;; Extempore needs to be told where the runtime is to be found.
               ;; While we're at it we disable automatic tuning for a specific
               ;; CPU to make binary substitution possible.
               (("COMMAND extempore" prefix)
                (string-append prefix " --sharedir " (getcwd)
                               " --mcpu=generic --attr=none")))
             #t)))))
    (inputs
     `(("llvm" ,llvm-for-extempore)
       ("libffi" ,libffi)
       ("jack" ,jack-1)
       ("libsndfile" ,libsndfile)
       ("glfw" ,glfw)
       ("apr" ,apr)
       ("stb-image" ,stb-image-for-extempore)
       ("kiss-fft" ,kiss-fft-for-extempore)
       ("nanovg" ,nanovg-for-extempore)
       ("portmidi" ,portmidi-for-extempore)
       ("assimp" ,assimp)
       ("alsa-lib" ,alsa-lib)
       ("portaudio" ,portaudio)
       ("mesa" ,mesa)
       ("pcre" ,pcre)
       ("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)
       ("emacs" ,emacs-no-x)))
    ;; Extempore refuses to build on architectures other than x86_64
    (supported-systems '("x86_64-linux"))
    (home-page "http://digego.github.io/extempore/")
    (synopsis "Programming environment for live coding of multimedia")
    (description
     "Extempore is a programming language and runtime environment designed
with live programming in mind.  It supports interactive programming in a REPL
style, compiling and binding code just-in-time.  Although Extempore has its
roots in 'live coding' of audiovisual media art, it is suitable for any task
domain where dynamic run-time modifiability and good numerical performance are
required.  Extempore also has strong timing and concurrency semantics, which
are helpful when working in problem spaces where timing is important (such as
audio and video).")
    (license license:bsd-2)))

(define-public klick
  (package
    (name "klick")
    (version "0.12.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://das.nasophon.de/download/klick-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0hmcaywnwzjci3pp4xpvbijnnwvibz7gf9xzcdjbdca910y5728j"))))
    (build-system scons-build-system)
    (arguments
     `(#:scons-flags (list (string-append "PREFIX=" %output))
       #:scons ,scons-python2
       #:tests? #f)) ;no "check" target
    (inputs
     `(("boost" ,boost)
       ("jack" ,jack-1)
       ("libsndfile" ,libsndfile)
       ("libsamplerate" ,libsamplerate)
       ("liblo" ,liblo)
       ("rubberband" ,rubberband)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://das.nasophon.de/klick/")
    (synopsis "Metronome for JACK")
    (description
     "klick is an advanced command-line based metronome for JACK.  It allows
you to define complex tempo maps for entire songs or performances.")
    (license license:gpl2+)))

(define-public gtklick
  (package
    (name "gtklick")
    (version "0.6.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://das.nasophon.de/download/gtklick-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0dq1km6njnzsqdqyf6wzir9g733z0mc9vmxfg2383k3c2a2di6bp"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'add-sitedirs
           ;; .pth files are not automatically interpreted unless the
           ;; directories containing them are added as "sites".  The directories
           ;; are then added to those in the PYTHONPATH.  This is required for
           ;; the operation of pygtk.
           (lambda _
             (substitute* "gtklick/gtklick.py"
               (("import pygtk")
                "import pygtk, site, sys
for path in [path for path in sys.path if 'site-packages' in path]: site.addsitedir(path)"))))
         (add-after 'unpack 'inject-store-path-to-klick
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "gtklick/klick_backend.py"
               (("KLICK_PATH = 'klick'")
                (string-append "KLICK_PATH = '"
                               (assoc-ref inputs "klick")
                               "/bin/klick'")))
             #t)))))
    (inputs
     `(("klick" ,klick)
       ("python2-pyliblo" ,python2-pyliblo)
       ("python2-pygtk" ,python2-pygtk)))
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (home-page "http://das.nasophon.de/gtklick/")
    (synopsis "Simple metronome with an easy-to-use graphical interface")
    (description
     "Gtklick is a simple metronome with an easy-to-use graphical user
interface.  It is implemented as a frontend to @code{klick}.")
    (license license:gpl2+)))

(define-public libgme
  (package
    (name "libgme")
    (version "0.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bitbucket.org/mpyne/game-music-emu/"
                                  "downloads/game-music-emu-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0hkkmxbaas2sirlb5i4r10mgbbiaapjx8pazifabwba23m3wnijh"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f)) ; no check target
    (native-inputs
     `(;; Use gcc-4.9 to work around an internal compiler error that happens
       ;; when using gcc-5.5.0.  FIXME: Try removing this when the default
       ;; compiler is no longer gcc-5.5.0.
       ("gcc" ,gcc-4.9)))
    (home-page "https://bitbucket.org/mpyne/game-music-emu")
    (synopsis "Video game music file playback library")
    (description
     "Game-music-emu is a collection of video game music file emulators that
support the following formats and systems:
@table @code
@item AY
ZX Spectrum/Asmtrad CPC
@item GBS
Nintendo Game Boy
@item GYM
Sega Genesis/Mega Drive
@item HES
NEC TurboGrafx-16/PC Engine
@item KSS
MSX Home Computer/other Z80 systems (doesn't support FM sound)
@item NSF/NSFE
Nintendo NES/Famicom (with VRC 6, Namco 106, and FME-7 sound)
@item SAP
Atari systems using POKEY sound chip
@item SPC
Super Nintendo/Super Famicom
@item VGM/VGZ
Sega Master System/Mark III, Sega Genesis/Mega Drive, BBC Micro
@end table")
    (license (list license:lgpl2.1+
                   ;; demo and player directories are under the Expat license
                   license:expat))))

(define-public lilypond
  (package
    (name "lilypond")
    (version "2.19.80")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://download.linuxaudio.org/lilypond/sources/v"
                    (version-major+minor version) "/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "0lql4q946gna2pl1g409mmmsvn2qvnq2z5cihrkfhk7plcqdny9n"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; out-test/collated-files.html fails
       #:out-of-source? #t
       #:make-flags '("conf=www") ;to generate images for info manuals
       #:configure-flags
       (list "CONFIGURATION=www"
             (string-append "--with-texgyre-dir="
                            (assoc-ref %build-inputs "font-tex-gyre")
                            "/share/fonts/opentype/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-texlive-union
           (lambda _
             ;; FIXME: fonts are not found and have to be generated in HOME.
             (setenv "HOME" "/tmp")
             ;; The test for the "lh" package fails, even though it is among
             ;; the inputs.
             (substitute* "configure"
               (("TEX_FIKPARM=.*") "TEX_FIKPARM=found\n"))
             #t))
         (add-after 'unpack 'fix-path-references
          (lambda _
            (substitute* "scm/backend-library.scm"
              (("\\(search-executable '\\(\"gs\"\\)\\)")
               (string-append "\"" (which "gs") "\""))
              (("\"/bin/sh\"")
               (string-append "\"" (which "sh") "\"")))
            #t))
         (add-before 'configure 'prepare-configuration
          (lambda _
            (substitute* "configure"
              (("SHELL=/bin/sh") "SHELL=sh")
              ;; When checking the fontforge version do not consider the
              ;; version string that's part of the directory.
              (("head -n") "tail -n")
              ;; Also allow for SOURCE_DATE_EPOCH = 0 in fontforge.
              (("20110222") "19700101"))
            (setenv "out" "www")
            (setenv "conf" "www")
            #t))
         (add-after 'install 'install-info
           (lambda _
             (invoke "make"
                     "-j" (number->string (parallel-job-count))
                     "conf=www" "install-info")
             #t)))))
    (inputs
     `(("guile" ,guile-1.8)
       ("font-dejavu" ,font-dejavu)
       ("font-tex-gyre" ,font-tex-gyre)
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
       ("gettext" ,gettext-minimal)
       ("imagemagick" ,imagemagick)
       ("netpbm" ,netpbm) ;for pngtopnm
       ("texlive" ,(texlive-union (list texlive-metapost
                                        texlive-generic-epsf
                                        texlive-latex-lh
                                        texlive-latex-cyrillic)))
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
    (license license:gpl3+)

    ;; On armhf and mips64el, building the documentation sometimes leads to
    ;; more than an hour of silence, so double the max silent time.
    (properties `((max-silent-time . 7200)))))

(define-public non-sequencer
  ;; The latest tagged release is three years old and uses a custom build
  ;; system, so we take the last commit.
  (let ((commit "5ae43bb27c42387052a73e5ffc5d33efb9d946a9")
        (revision "4"))
    (package
      (name "non-sequencer")
      (version (string-append "1.9.5-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "git://git.tuxfamily.org/gitroot/non/non.git")
                      (commit commit)))
                (sha256
                 (base32
                  "1cljkkyi9dxqpqhx8y6l2ja4zjmlya26m26kqxml8gx08vyvddhx"))
                (file-name (string-append name "-" version "-checkout"))))
      (build-system waf-build-system)
      (arguments
       `(#:tests? #f ;no "check" target
         #:configure-flags
         (list "--project=sequencer"
               ;; Disable the use of SSE unless on x86_64.
               ,@(if (not (string-prefix? "x86_64" (or (%current-target-system)
                                                       (%current-system))))
                     '("--disable-sse")
                     '()))
         #:phases
         (modify-phases %standard-phases
           (add-before
            'configure 'set-flags
            (lambda _
              ;; Compile with C++11, required by libsigc++.
              (setenv "CXXFLAGS" "-std=c++11")
              #t)))
         #:python ,python-2))
      (inputs
       `(("jack" ,jack-1)
         ("libsigc++" ,libsigc++)
         ("liblo" ,liblo)
         ("ntk" ,ntk)))
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (home-page "http://non.tuxfamily.org/wiki/Non%20Sequencer")
      (synopsis "Pattern-based MIDI sequencer")
      (description
       "The Non Sequencer is a powerful, lightweight, real-time,
pattern-based MIDI sequencer.  It utilizes the JACK Audio Connection Kit for
MIDI I/O and the NTK GUI toolkit for its user interface.  Everything in Non
Sequencer happens on-line, in real-time.  Music can be composed live, while the
transport is rolling.")
      (license license:gpl2+))))

(define-public non-session-manager
  (package (inherit non-sequencer)
    (name "non-session-manager")
    (arguments
     (substitute-keyword-arguments (package-arguments non-sequencer)
       ((#:configure-flags flags)
        `(cons "--project=session-manager"
               (delete "--project=sequencer" ,flags)))))
    (inputs
     `(("jack" ,jack-1)
       ("liblo" ,liblo)
       ("ntk" ,ntk)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://non.tuxfamily.org/nsm/")
    (synopsis "Audio session management")
    (description
     "The Non Session Manager is an API and an implementation for audio
session management.  NSM clients use a well-specified OSC protocol to
communicate with the session management daemon.")
    (license license:gpl2+)))

(define-public non-mixer
  (package (inherit non-sequencer)
    (name "non-mixer")
    (arguments
     (substitute-keyword-arguments (package-arguments non-sequencer)
       ((#:configure-flags flags)
        `(cons "--project=mixer"
               (delete "--project=sequencer" ,flags)))))
    (inputs
     `(("jack" ,jack-1)
       ("liblo" ,liblo)
       ("ladspa" ,ladspa)
       ("lrdf" ,lrdf)
       ("ntk" ,ntk)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://non.tuxfamily.org/wiki/Non%20Mixer")
    (synopsis "Modular digital audio mixer")
    (description
     "The Non Mixer is a powerful, reliable and fast modular digital audio
mixer.  It utilizes JACK for inter-application audio I/O and the NTK GUI
toolkit for a fast and lightweight user interface.  Non Mixer can be used
alone or in concert with Non Timeline and Non Sequencer to form a complete
studio.")
    (license license:gpl2+)))

(define-public non-timeline
  (package (inherit non-sequencer)
    (name "non-timeline")
    (arguments
     (substitute-keyword-arguments (package-arguments non-sequencer)
       ((#:configure-flags flags)
        `(cons "--project=timeline"
               (delete "--project=sequencer" ,flags)))))
    (inputs
     `(("jack" ,jack-1)
       ("liblo" ,liblo)
       ("libsndfile" ,libsndfile)
       ("ntk" ,ntk)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://non.tuxfamily.org/wiki/Non%20Timeline")
    (synopsis "Modular digital audio timeline arranger")
    (description
     "The Non Timeline is a powerful, reliable and fast modular digital audio
timeline arranger.  It utilizes JACK for inter-application audio I/O and the
NTK GUI toolkit for a fast and lightweight user interface.  Non Timeline can
be used alone or in concert with Non Mixer and Non Sequencer to form a
complete studio.")
    (license license:gpl2+)))

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
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-configuration
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "default.config"
               (("/usr/bin/aplay") "aplay")
               (("/usr/bin/timidity") "timidity")
               (("/usr/bin/mpg123") "mpg123")
               (("/usr/bin/ogg123") "ogg123"))
             #t))
         (add-before 'build 'patch-python-shebangs
           (lambda _
             ;; Two python scripts begin with a Unicode BOM, so patch-shebang
             ;; has no effect.
             (substitute* '("solfege/parsetree.py"
                            "solfege/presetup.py")
               (("#!/usr/bin/python") (string-append "#!" (which "python"))))
             #t))
         (add-before 'build 'add-sitedirs
           ;; .pth files are not automatically interpreted unless the
           ;; directories containing them are added as "sites".  The directories
           ;; are then added to those in the PYTHONPATH.  This is required for
           ;; the operation of pygtk and pygobject.
           (lambda _
             (substitute* "run-solfege.py"
               (("import os")
                "import os, site
for path in [path for path in sys.path if 'site-packages' in path]: site.addsitedir(path)"))
             #t))
         (add-before 'build 'adjust-config-file-prefix
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "run-solfege.py"
               (("prefix = os.path.*$")
                (string-append "prefix = " (assoc-ref outputs "out"))))
             #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make sure 'solfege' runs with the correct PYTHONPATH.
             (let* ((out (assoc-ref outputs "out"))
                    (path (getenv "PYTHONPATH")))
               (wrap-program (string-append out "/bin/solfege")
                 `("PYTHONPATH" ":" prefix (,path))))
             #t)))))
    (inputs
     `(("python" ,python-2)
       ("pygtk" ,python2-pygtk)
       ("gettext" ,gettext-minimal)
       ("gtk" ,gtk+)
       ("lilypond" ,lilypond)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("txt2man" ,txt2man)
       ("libxml2" ,libxml2) ; for tests
       ("ghostscript" ,ghostscript)
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
    (version "2.0.0-alpha10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/powertab/powertabeditor.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1z4fhdp71ck9synr12rg1p6365xnypd8ih40c5icj4si36khvksk"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove bundled sources for external libraries
                  (delete-file-recursively "external")
                  ;; Use only system libraries
                  (substitute* "CMakeLists.txt"
                    (("include\\( PTE_ThirdParty \\)")
                     "\
include(third_party/Qt)
include(third_party/boost)
add_library( Catch INTERFACE IMPORTED )
add_library( rapidjson INTERFACE IMPORTED )"))
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
             (string-append "-DCMAKE_INSTALL_RPATH="
                            (string-join (map (match-lambda
                                                ((name . directory)
                                                 (string-append directory "/lib")))
                                              %build-inputs) ";"))
             "-DPTE_DATA_DIR=share/powertabeditor")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "bin/pte_tests"
                     ;; FIXME: these tests fail
                     "exclude:Actions/EditStaff"
                     "exclude:Formats/PowerTabOldImport/MergeMultiBarRests"
                     "exclude:Score/ViewFilter/FilterRule"
                     "exclude:Score/ViewFilter/ViewFilter"
                     "exclude:Formats/PowerTabOldImport/Directions")
             #t))
         ;; FIXME: This bug has been fixed upstream, but no release has been
         ;; made yet.  See https://github.com/powertab/powertabeditor/issues/257
         (add-after 'unpack 'fix-boost-bug
           (lambda _
             (substitute* "source/score/voiceutils.cpp"
               (("boost::rational<int> duration\\(4, pos.getDurationType\\(\\)\\);")
                "boost::rational<int> duration(4, static_cast<int>(pos.getDurationType()));"))
             #t))
         ;; Fix build with Qt 5.11.
         (add-after 'unpack 'add-missing-headers
           (lambda _
             (substitute* (find-files "source/dialogs/" "\\.h$")
               (("#include <QDialog>" m)
                (string-append m "\n#include <QButtonGroup>")))
             (substitute* "source/widgets/mixer/mixeritem.h"
               (("#include <QWidget>" m)
                (string-append m "\n#include <QStyle>")))
             (substitute* "source/widgets/playback/playbackwidget.h"
               (("#include <QWidget>" m)
                (string-append m "\n#include <QButtonGroup>\n#include <QAction>")))
             #t))
         (add-before 'configure 'remove-third-party-libs
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Link with required static libraries, because we're not
             ;; using the bundled version of withershins.
             ;; Also add pthread for fixing a linker error.
             (substitute* "source/build/CMakeLists.txt"
               (("withershins" line)
                (string-append line "\n"
                               (assoc-ref inputs "binutils")
                               "/lib/libbfd.a\n"
                               (assoc-ref inputs "libiberty")
                               "/lib/libiberty.a\n"
                               "dl\n"
                               "pthread\n"
                               "z\n")))
             #t)))))
    (inputs
     `(("boost" ,boost)
       ("alsa-lib" ,alsa-lib)
       ("qtbase" ,qtbase)
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

(define-public jalv-select
  (package
    (name "jalv-select")
    (version "0.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/brummer10/jalv_select.git")
                    (commit (string-append "V" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gqh768sbvn9ffyx1vqg9i57py9x9v4l65bk6wjsvgga4d7m83k1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'ignore-PATH
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "jalv.select.cpp"
               (("echo \\$PATH.*tr ':'.*xargs ls")
                (string-append "ls -1 " (assoc-ref inputs "jalv") "/bin")))
             (substitute* "jalv.select.h"
               (("gtkmm.h") "gtkmm-2.4/gtkmm.h"))
             #t))
         (add-before 'reset-gzip-timestamps 'make-manpages-writable
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each make-file-writable
                       (find-files (string-append (assoc-ref outputs "out")
                                                  "/share/man")
                                   ".*\\.gz$"))
             #t)))))
    (inputs
     `(("lilv" ,lilv)
       ("lv2" ,lv2)
       ("jalv" ,jalv)
       ("gtkmm" ,gtkmm-2)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/brummer10/jalv_select")
    (synopsis "GUI to select LV2 plugins and run them with jalv")
    (description
     "The jalv.select package provides a graphical user interface allowing
users to select LV2 plugins and run them with jalv.")
    (license license:public-domain)))

(define-public synthv1
  (package
    (name "synthv1")
    (version "0.9.5")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://sourceforge/synthv1/synthv1/" version
                              "/synthv1-" version ".tar.gz"))
              (sha256
               (base32
                "1b9w4cml3cmcg09kh852cahas6l9ks8wl3gzp1az8rzxz4229yg1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:configure-flags
       '("CXXFLAGS=-std=gnu++11")))
    (inputs
     `(("jack" ,jack-1)
       ("lv2" ,lv2)
       ("alsa-lib" ,alsa-lib)
       ("non-session-manager" ,non-session-manager)
       ("liblo" ,liblo)
       ("qtbase" ,qtbase)
       ("qttools" ,qttools)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://synthv1.sourceforge.io")
    (synopsis "Polyphonic subtractive synthesizer")
    (description
     "Synthv1 is an old-school subtractive polyphonic synthesizer with four
oscillators and stereo effects.")
    (license license:gpl2+)))

(define-public drumkv1
  (package
    (name "drumkv1")
    (version "0.9.5")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://sourceforge/drumkv1/drumkv1/" version
                              "/drumkv1-" version ".tar.gz"))
              (sha256
               (base32
                "1azzwqgrrr4zr509sib2jvigfvz5bkwnx82chdadxdwfarai8586"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:configure-flags
       '("CXXFLAGS=-std=gnu++11")))
    (inputs
     `(("jack" ,jack-1)
       ("lv2" ,lv2)
       ("libsndfile" ,libsndfile)
       ("alsa-lib" ,alsa-lib)
       ("non-session-manager" ,non-session-manager)
       ("liblo" ,liblo)
       ("qtbase" ,qtbase)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (home-page "https://drumkv1.sourceforge.io")
    (synopsis "Drum-kit sampler synthesizer with stereo effects")
    (description
     "Drumkv1 is an old-school drum-kit sampler synthesizer with stereo
effects.")
    (license license:gpl2+)))

(define-public samplv1
  (package
    (name "samplv1")
    (version "0.9.5")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://sourceforge/samplv1/samplv1/" version
                              "/samplv1-" version ".tar.gz"))
              (sha256
               (base32
                "0402rdr46za21w34m7ajzbwiiqcd1h0da3b0ldsr2z5g2mrkzxv8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:configure-flags
       '("CXXFLAGS=-std=gnu++11")))
    (inputs
     `(("jack" ,jack-1)
       ("lv2" ,lv2)
       ("libsndfile" ,libsndfile)
       ("alsa-lib" ,alsa-lib)
       ("non-session-manager" ,non-session-manager)
       ("liblo" ,liblo)
       ("qtbase" ,qtbase)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (home-page "https://samplv1.sourceforge.io")
    (synopsis "Polyphonic sampler synthesizer with stereo effects")
    (description
     "Samplv1 is an old-school polyphonic sampler synthesizer with stereo
effects.")
    (license license:gpl2+)))

(define-public padthv1
  (package
    (name "padthv1")
    (version "0.9.5")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://sourceforge/padthv1/padthv1/" version
                              "/padthv1-" version ".tar.gz"))
              (sha256
               (base32
                "0cd1jfb3ynfrsbz8jwfsbvs5liyddxg4zghrvz931qkkqi117hbh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:configure-flags
       '("CXXFLAGS=-std=gnu++11")))
    (inputs
     `(("jack" ,jack-1)
       ("lv2" ,lv2)
       ("alsa-lib" ,alsa-lib)
       ("non-session-manager" ,non-session-manager)
       ("liblo" ,liblo)
       ("fftw" ,fftw)
       ("qtbase" ,qtbase)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (home-page "https://padthv1.sourceforge.io")
    (synopsis "Polyphonic additive synthesizer")
    (description
     "Padthv1 is an old-school polyphonic additive synthesizer with stereo
effects.  Padthv1 is based on the PADsynth algorithm by Paul Nasca, as a
special variant of additive synthesis.")
    (license license:gpl2+)))

(define-public amsynth
  (package
    (name "amsynth")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/amsynth/amsynth/releases/"
                           "download/release-" version
                           "/amsynth-" version ".tar.bz2"))
       (sha256
        (base32
         "1882pfcmf3rqg3vd4qflzkppcv158d748i603spqjbxqi8z7x7w0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-flags
           (lambda _
             ;; Compile with C++11, required by gtkmm.
             (setenv "CXXFLAGS" "-std=c++11")
             #t)))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("jack" ,jack-1)
       ("lv2" ,lv2)
       ("lash" ,lash)
       ("libsndfile" ,libsndfile)
       ("gtk+" ,gtk+-2)
       ("gtkmm" ,gtkmm-2)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (home-page "http://amsynth.github.io")
    (synopsis "Analog modeling synthesizer")
    (description
     "amsynth is an easy-to-use software synthesizer with a classic
subtractive synthesizer topology.  Its features include: dual
oscillators (sine, saw, square, noise) with hard sync; 12 and 24 dB/oct
resonant filters (low-pass, high-pass, band-pass, notch); mono, poly, legato
keyboard modes; dual ADSR envelope generators for filter and amplitude; LFO
which can modulate the oscillators, filter, and amplitude; distortion and
reverb effects.")
    (license license:gpl2+)))

(define-public setbfree
  (package
    (name "setbfree")
    (version "0.8.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pantherb/setBfree")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r2i8cp0gfzqp6scrxwwf7pn7lk2j9kmf8cx7bqjvgfyn251dw3j"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "FONTFILE="
                            (assoc-ref %build-inputs "font-bitstream-vera")
                            "/share/fonts/truetype/VeraBd.ttf")
             ;; Disable unsupported optimization flags on non-x86
             ,@(let ((system (or (%current-target-system)
                                 (%current-system))))
                 (if (or (string-prefix? "x86_64" system)
                         (string-prefix? "i686" system))
                     '()
                     '("OPTIMIZATIONS=-ffast-math -fomit-frame-pointer -O3"))))
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

(define-public beast
  (package
    (name "beast")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://testbit.eu/pub/dists/beast/beast-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1jzzmfwssklzw8fvvil04n8csc0zm99fnd9p2xa7c0xchg37lvhn"))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-build? #f)) ; Race conditions cause build failures
    (inputs
     `(("rapicorn" ,rapicorn)
       ("guile" ,guile-1.8)
       ("python" ,python-2)
       ("libgnomecanvas" ,libgnomecanvas)
       ("libogg" ,libogg)
       ("libmad" ,libmad)
       ("flac" ,flac)
       ("alsa-lib" ,alsa-lib)
       ("libvorbis" ,libvorbis)
       ("gettext" ,gettext-minimal)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib:bin" ,glib "bin")
       ("cython" ,python2-cython)
       ("perl" ,perl)
       ("perl-xml-parser" ,perl-xml-parser)))
    (home-page "https://testbit.eu/wiki/Beast_Home")
    (synopsis "Music composition and modular synthesis environment")
    (description
     "Beast is a music composition and modular synthesis application.  It
supports a wide range of standards in the field, such as MIDI, various audio
file formats and LADSPA modules.  It allows for multitrack editing, real-time
synthesis, 32bit audio rendering, precise timing down to sample granularity,
on-demand and partial loading of wave files, on the fly decoding, stereo
mixing, FFT scopes, MIDI automation and full scriptability in Scheme.")
    (license license:gpl3+)))

(define-public bristol
  (package
    (name "bristol")
    (version "0.60.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/bristol/bristol/"
                                  (version-major+minor version)
                                  "/bristol-" version ".tar.gz"))
              (sha256
               (base32
                "1fi2m4gmvxdi260821y09lxsimq82yv4k5bbgk3kyc3x1nyhn7vx"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-sse-flags
           (lambda* (#:key system #:allow-other-keys)
             (when (not (or (string-prefix? "x86_64" system)
                            (string-prefix? "i686" system)))
               (substitute* "bristol/Makefile.in"
                 (("-msse -mfpmath=sse") "")))
             #t))
         ;; alsa-lib 1.1.x no longer provides iatomic.h.  That's okay because
         ;; bristol actually doesn't use it.
         (add-after 'unpack 'do-not-use-alsa-iatomic
           (lambda _
             (substitute* "libbristolaudio/audioEngineJack.c"
               (("#include <alsa/iatomic.h>") ""))
             #t))
         ;; We know that Bristol has been linked with JACK and we don't have
         ;; ldd, so we can just skip this check.
         (add-after 'unpack 'do-not-grep-for-jack
           (lambda _
             (substitute* "bin/startBristol.in"
               (("ldd `which bristol` | grep jack") "echo guix"))
             #t)))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("jack" ,jack-1)
       ("liblo" ,liblo)
       ("libx11" ,libx11)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://bristol.sourceforge.net/")
    (synopsis "Synthesizer emulator")
    (description
     "Bristol is an emulation package for a number of different 'classic'
synthesizers including additive and subtractive and a few organs.  The
application consists of the engine, which is called bristol, and its own GUI
library called brighton that represents all the emulations.  There are
currently more than twenty different emulations; each does sound different
although the author maintains that the quality and accuracy of each emulation
is subjective.")
    (license license:gpl3+)))

(define-public tuxguitar
  (package
    (name "tuxguitar")
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/tuxguitar/TuxGuitar/TuxGuitar-"
                    version "/tuxguitar-" version "-src.tar.gz"))
              (sha256
               (base32
                "10arfpgm2pw7mn922klklzn05lw5ifqx070shdrar81afmkfbbd9"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "build"
       #:jdk ,icedtea-8
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-dir
           (lambda _ (chdir "TuxGuitar-lib") #t))
         (add-after 'build 'build-libraries
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((initial-classpath (getenv "CLASSPATH"))
                    (build-dir (lambda (dir)
                                 (chdir "..")
                                 (setenv "CLASSPATH"
                                         (string-join (cons initial-classpath
                                                            (find-files (getcwd) "\\.jar$"))
                                                      ":"))
                                 (chdir dir)
                                 (if (file-exists? "build.xml")
                                     ((assoc-ref %standard-phases 'build)
                                      #:build-target "build")
                                     (begin
                                       ;; Generate default build.xml.
                                       ((@@ (guix build ant-build-system) default-build.xml)
                                        (string-append (string-downcase dir) ".jar")
                                        (string-append (assoc-ref outputs "out")
                                                       "/share/java"))
                                       ((assoc-ref %standard-phases 'build)))))))
               (map build-dir '("TuxGuitar-editor-utils"
                                "TuxGuitar-ui-toolkit"
                                "TuxGuitar-ui-toolkit-swt"
                                "TuxGuitar-awt-graphics")))))
         (add-after 'build-libraries 'build-application
           (lambda _
             (chdir "../TuxGuitar")
             ((assoc-ref %standard-phases 'build)
              #:build-target "build")))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (share (string-append out "/share"))
                    (lib   (string-append share "/java"))
                    (swt   (assoc-ref inputs "java-swt")))
               (mkdir-p bin)
               ;; Install all jars.
               (for-each (lambda (file)
                           (install-file file lib))
                         (find-files ".." "\\.jar$"))

               ;; Install all resources.
               (copy-recursively "share" share)

               ;; Create wrapper.
               (call-with-output-file (string-append bin "/tuxguitar")
                 (lambda (port)
                   (let ((classpath (string-join (append (find-files lib "\\.jar$")
                                                         (find-files swt "\\.jar$"))
                                                 ":")))
                     (format
                      port
                      (string-append "#!/bin/sh\n"
                                     (which "java")
                                     " -cp " classpath
                                     " -Dtuxguitar.home.path=" out
                                     " -Dtuxguitar.share.path=" out "/share"
                                     " -Dswt.library.path=" swt "/lib"
                                     " org.herac.tuxguitar.app.TGMainSingleton"
                                     " \"$1\" \"$2\"")))))
               (chmod (string-append bin "/tuxguitar") #o555)
               #t))))))
    (inputs
     `(("java-swt" ,java-swt)))
    (home-page "http://tuxguitar.com.ar/")
    (synopsis "Multitrack tablature editor and player")
    (description
     "TuxGuitar is a guitar tablature editor with player support through midi.
It can display scores and multitrack tabs.  TuxGuitar provides various
additional features, including autoscrolling while playing, note duration
management, bend/slide/vibrato/hammer-on/pull-off effects, support for
tuplets, time signature management, tempo management, gp3/gp4/gp5 import and
export.")
    (license license:lgpl2.1+)))

(define-public pd
  (package
    (name "pd")
    (version "0.49-0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "http://msp.ucsd.edu/Software/pd-"
                              version ".src.tar.gz"))
              (sha256
               (base32
                "18rzqbpgnnvyslap7k0ly87aw1bbxkb0rk5agpr423ibs9slxq6j"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:configure-flags
       (list
        "--enable-jack"
        (string-append "--with-wish=" (string-append
                                       (assoc-ref %build-inputs "tk")
                                       "/bin/wish8.6")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-with-path
           (lambda _
             (substitute* "tcl/pd-gui.tcl"
               (("exec wish ") (string-append "exec " (which "wish8.6") " ")))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("tk" ,tk)
       ("alsa-lib" ,alsa-lib)
       ("jack" ,jack-1)))
    (home-page "http://puredata.info")
    (synopsis "Visual programming language for artistic performances")
    (description
     "Pure Data (aka Pd) is a visual programming language.  Pd enables
musicians, visual artists, performers, researchers, and developers to create
software graphically, without writing lines of code.  Pd is used to process
and generate sound, video, 2D/3D graphics, and interface sensors, input
devices, and MIDI.  Pd can easily work over local and remote networks to
integrate wearable technology, motor systems, lighting rigs, and other
equipment.  Pd is suitable for learning basic multimedia processing and visual
programming methods as well as for realizing complex systems for large-scale
projects.")
    (license license:bsd-3)))

(define-public libpd
  (package
    (name "libpd")
    (version "0.11.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libpd/libpd.git")
                    (commit version)
                    (recursive? #t)))   ; for the 'pure-data' submodule
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1bcg1d9iyf9n37hwwphmih0c8rd1xcqykil5z1cax6xfs76552nk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no tests
       #:make-flags '("CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "make" "install"
                       (string-append "prefix=" out)
                       ;; XXX: Fix the last 2 lines of 'install' target.
                       "LIBPD_IMPLIB=NO"
                       "LIBPD_DEF=NO")))))))
    (home-page "http://libpd.cc/")
    (synopsis "Pure Data as an embeddable audio synthesis library")
    (description
     "Libpd provides Pure Data as an embeddable audio synthesis library.  Its
main purpose is to liberate raw audio rendering from audio and MIDI drivers.")
    (license license:bsd-3)))

(define-public portmidi
  (package
    (name "portmidi")
    (version "217")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/portmedia/portmidi/"
                                  version "/portmidi-src-" version ".zip"))
              (sha256
               (base32
                "03rfsk7z6rdahq2ihy5k13qjzgx757f75yqka88v3gc0pn9ais88"))
              (patches (list (search-patch "portmidi-modular-build.patch")))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; tests cannot be linked
       #:build-type "Release"           ; needed to have PMALSA set
       #:configure-flags
       (list "-DPORTMIDI_ENABLE_JAVA=Off"
             "-DPORTMIDI_ENABLE_TEST=Off") ; tests fail linking
       #:phases
       (modify-phases %standard-phases
         ;; Some packages, e.g., MuseScore, expect "libporttime.so" instead of
         ;; "libportmidi.so".  Distributions get away with it by creating an
         ;; appropriate symlink.
         (add-after 'install 'add-porttime
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (with-directory-excursion lib
                 (symlink "libportmidi.so" "libporttime.so")))
             #t)))))
    (inputs
     `(("alsa-lib" ,alsa-lib)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://portmedia.sourceforge.net/portmidi/")
    (synopsis "Library for MIDI I/O")
    (description
     "PortMidi is a library supporting real-time input and output of MIDI data
using a system-independent interface.")
    (license license:expat)))

(define-public portmidi-for-extempore
  (package (inherit portmidi)
    (name "portmidi-for-extempore")
    (version "217")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/extemporelang/portmidi.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1inriyrjf7xx2b7r54x0vmf9ngyqgr7g5060c22bwkbsgg53apzv"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; no tests
    (native-inputs '())
    ;; Extempore refuses to build on architectures other than x86_64
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/extemporelang/portmidi/")))

(define-public python-pyportmidi
  (package
    (name "python-pyportmidi")
    (version (package-version portmidi))
    (source (package-source portmidi))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-dir
           (lambda _ (chdir "pm_python") #t))
         (add-after 'enter-dir 'fix-setup.py
           (lambda _
             (substitute* "setup.py"
               ;; Use Python 3 syntax
               (("print (\".*\")" _ text)
                (string-append "print(" text ")\n"))
               ;; TODO.txt and CHANGES.txt don't exist
               (("CHANGES =.*") "CHANGES = \"\"\n")
               (("TODO =.*") "TODO = \"\"\n"))
             #t)))))
    (inputs
     `(("portmidi" ,portmidi)
       ("alsa-lib" ,alsa-lib)))
    (native-inputs
     `(("python-cython" ,python-cython)
       ("unzip" ,unzip)))
    (home-page "http://portmedia.sourceforge.net/portmidi/")
    (synopsis "Python bindings to PortMidi")
    (description
     "This package provides Python bindings to the PortMidi library.")
    (license license:expat)))

(define-public frescobaldi
  (package
    (name "frescobaldi")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/wbsoft/frescobaldi/releases/download/v"
                    version "/frescobaldi-" version ".tar.gz"))
              (sha256
               (base32
                "15cqhbjbjikr7ljgiq56bz2gxrr38j8p0f78p2vhyzydaviy9a2z"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; no tests included
    (inputs
     `(("lilypond" ,lilypond)
       ("portmidi" ,portmidi)
       ("python-pyqt" ,python-pyqt)
       ("python-ly" ,python-ly)
       ("python-pyportmidi" ,python-pyportmidi)
       ("poppler" ,poppler)
       ("python-poppler-qt5" ,python-poppler-qt5)
       ("python-sip" ,python-sip)))
    (home-page "http://www.frescobaldi.org/")
    (synopsis "LilyPond sheet music text editor")
    (description
     "Frescobaldi is a LilyPond sheet music text editor with syntax
highlighting and automatic completion.  Among other things, it can render
scores next to the source, can capture input from MIDI or read MusicXML and
ABC files, has a MIDI player for proof-listening, and includes a documentation
browser.")
    (license license:gpl2+)))

(define-public drumstick
  (package
    (name "drumstick")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/drumstick/"
                                  version "/drumstick-" version ".tar.bz2"))
              (sha256
               (base32
                "0kljqyqj7s1i2z52i24x7ail1bywn6dcxxfbad5c59drm8wv94bp"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no test target
       #:configure-flags '("-DLIB_SUFFIX=")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-docbook
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "cmake_admin/CreateManpages.cmake"
               (("http://docbook.sourceforge.net/release/xsl/current/manpages/docbook.xsl")
                (string-append (assoc-ref inputs "docbook-xsl")
                               "/xml/xsl/docbook-xsl-"
                               ,(package-version docbook-xsl)
                               "/manpages/docbook.xsl")))
             #t)))))
    (inputs
     `(("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("alsa-lib" ,alsa-lib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("libxslt" ,libxslt)             ; for xsltproc
       ("docbook-xsl" ,docbook-xsl)
       ("doxygen" ,doxygen)))
    (home-page "http://drumstick.sourceforge.net/")
    (synopsis "C++ MIDI library")
    (description
     "Drumstick is a set of MIDI libraries using C++/Qt5 idioms and style.  It
includes a C++ wrapper around the ALSA library sequencer interface.  A
complementary library provides classes for processing SMF (Standard MIDI
files: .MID/.KAR), Cakewalk (.WRK), and Overture (.OVE) file formats.  A
multiplatform realtime MIDI I/O library is also provided with various output
backends, including ALSA, OSS, Network and FluidSynth.")
    (license license:gpl2+)))

(define-public zynaddsubfx
  (package
    (name "zynaddsubfx")
    (version "3.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/zynaddsubfx/zynaddsubfx/"
                    version "/zynaddsubfx-" version ".tar.bz2"))
              (sha256
               (base32
                "1hfpiqdm337gl4ynkmmp2qss2m5z8mzqzjrbiyg6w1v4js7l9phi"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Move SSE compiler optimization flags from generic target to
         ;; athlon64 and core2 targets, because otherwise the build would fail
         ;; on non-Intel machines.
         (add-after 'unpack 'remove-sse-flags-from-generic-target
          (lambda _
            (substitute* "src/CMakeLists.txt"
              (("-msse -msse2 -mfpmath=sse") "")
              (("-march=(athlon64|core2)" flag)
               (string-append flag " -msse -msse2 -mfpmath=sse")))
            #t)))))
    (inputs
     `(("liblo" ,liblo)
       ("ntk" ,ntk)
       ("mesa" ,mesa)
       ("alsa-lib" ,alsa-lib)
       ("jack" ,jack-1)
       ("fftw" ,fftw)
       ("minixml" ,minixml)
       ("libxpm" ,libxpm)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://zynaddsubfx.sf.net/")
    (synopsis "Software synthesizer")
    (description
     "ZynAddSubFX is a feature heavy realtime software synthesizer.  It offers
three synthesizer engines, multitimbral and polyphonic synths, microtonal
capabilities, custom envelopes, effects, etc.")
    (license license:gpl2)))

(define-public yoshimi
  (package
    (name "yoshimi")
    (version "1.5.10.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/yoshimi/"
                                  (version-major+minor version)
                                  "/yoshimi-" version ".tar.bz2"))
              (sha256
               (base32
                "1rr99qkq80s8l2iv3x4ccxan07m15dvmd5s9b10386bfjbwbya01"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:configure-flags
       (list (string-append "-DCMAKE_INSTALL_DATAROOTDIR="
                            (assoc-ref %outputs "out") "/share"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'enter-dir
           (lambda _ (chdir "src") #t))
         ;; Move SSE compiler optimization flags from generic target to
         ;; athlon64 and core2 targets, because otherwise the build would fail
         ;; on non-Intel machines.
         (add-after 'unpack 'remove-sse-flags-from-generic-target
          (lambda _
            (substitute* "src/CMakeLists.txt"
              (("-msse -msse2 -mfpmath=sse") "")
              (("-march=(athlon64|core2)" flag)
               (string-append flag " -msse -msse2 -mfpmath=sse")))
            #t)))))
    (inputs
     `(("boost" ,boost)
       ("fftwf" ,fftwf)
       ("alsa-lib" ,alsa-lib)
       ("jack" ,jack-1)
       ("fontconfig" ,fontconfig)
       ("minixml" ,minixml)
       ("mesa" ,mesa)
       ("fltk" ,fltk)
       ("lv2" ,lv2)
       ("readline" ,readline)
       ("ncurses" ,ncurses)
       ("cairo" ,cairo)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://yoshimi.sourceforge.net/")
    (synopsis "Multi-paradigm software synthesizer")
    (description
     "Yoshimi is a fork of ZynAddSubFX, a feature-heavy real-time software
synthesizer.  It offers three synthesizer engines, multitimbral and polyphonic
synths, microtonal capabilities, custom envelopes, effects, etc.  Yoshimi
improves on support for JACK features, such as JACK MIDI.")
    (license license:gpl2)))

(define-public libgig
  (package
    (name "libgig")
    (version "4.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.linuxsampler.org/packages/"
                                  "libgig-" version ".tar.bz2"))
              (sha256
               (base32
                "02xx6bqxzgkvrawwnzrnxx1ypk244q4kpwfd58266f9ji8kq18h6"))))
    (build-system gnu-build-system)
    (inputs
     `(("libuuid" ,util-linux)
       ("libsndfile" ,libsndfile)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://linuxsampler.org/libgig/")
    (synopsis "C++ library for working with Gigasampler (.gig) files")
    (description
     "Libgig is a C++ library for loading, modifying existing and creating new
Gigasampler (.gig) files and DLS (Downloadable Sounds) Level 1/2 files, KORG
sample based instruments (.KSF and .KMP files), SoundFont v2 (.sf2) files and
AKAI sampler data.  The package includes a couple of command line tools based
on the library.")
    ;; The library and tools are released under the GPL, except the AKAI
    ;; classes which are released under the LGPL.
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public jack-keyboard
  (package
    (name "jack-keyboard")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/jack-keyboard/jack-keyboard/"
                           version "/jack-keyboard-" version ".tar.gz"))
       (sha256
        (base32
         "0mzmg8aavybcfdlq2yd9d0vscqd6is5p6jzrgfpfm5j3xdcvh2s3"))))
    (build-system gnu-build-system)
    (inputs
     `(("jack" ,jack-1)
       ("lash" ,lash)
       ("gtk+" ,gtk+-2)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://jack-keyboard.sourceforge.net/")
    (synopsis "Virtual MIDI keyboard")
    (description "Jack-keyboard is a virtual MIDI keyboard, a program that
allows you to send JACK MIDI events (i.e. play) using your PC keyboard.")
    (license license:bsd-2)))

(define-public jack-capture
  (package
    (name "jack-capture")
    (version "0.9.73")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kmatheussen/jack_capture.git")
                    (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0jcqky96q8xgya6wqv1p8pj9fkf2wh7ynl67ah7x5bn3basgfclf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f ; there are none
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("gtk+" ,gtk+-2)
       ("jack" ,jack-1)
       ("libogg" ,libogg)
       ("liblo" ,liblo)
       ("lame" ,lame)
       ("libsndfile" ,libsndfile)))
    (home-page "https://github.com/kmatheussen/jack_capture")
    (synopsis "Program for recording sound files with JACK")
    (description "This is a program for recording sound files with JACK.  It
can connect to any JACK port and record the output into a stereo WAV file.")
    (license license:gpl2+)))

(define-public cursynth
  (package
    (name "cursynth")
    (version "1.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/cursynth/cursynth-"
                          version ".tar.gz"))
      (sha256
       (base32 "1dhphsya41rv8z6yqcv9l6fwbslsds4zh1y56zizi39nd996d40v"))
      (patches (search-patches "cursynth-wave-rand.patch"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    ;; TODO: See https://github.com/iyoko/cursynth/issues/4 which currently
    ;; prevents us from using pulseaudio
    (inputs `(("ncurses" ,ncurses)
              ("alsa" ,alsa-lib)))
    (home-page "https://www.gnu.org/software/cursynth/")
    (synopsis "Polyphonic and MIDI subtractive music synthesizer using curses")
    (description "GNU cursynth is a polyphonic synthesizer that runs
graphically in the terminal.  It is built on a full-featured subtractive
synthesis engine.  Notes and parameter changes may be entered via MIDI or the
computer's keyboard.")
    (license license:gpl3+)))

(define-public aj-snapshot
  (package
    (name "aj-snapshot")
    (version "0.9.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/aj-snapshot/"
                                  "aj-snapshot-" version ".tar.bz2"))
              (sha256
               (base32
                "0wilky1g2mb88v2z0520s7sw1dsn10iwanc8id5p6z1xsnhg7b6p"))))
    (build-system gnu-build-system)
    (inputs
     `(("minixml" ,minixml)
       ("jack" ,jack-1)
       ("alsa-lib" ,alsa-lib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://aj-snapshot.sourceforge.net/")
    (synopsis "Snapshot connections between ALSA and JACK clients")
    (description "Aj-snapshot is a small program that can be used to make
snapshots of the connections made between JACK and/or ALSA clients.  Because
JACK can provide both audio and MIDI support to programs, aj-snapshot can
store both types of connections for JACK.  ALSA, on the other hand, only
provides routing facilities for MIDI clients.  Aj-snapshot is meant to be used
from the command line.")
    (license license:gpl3+)))

(define-public qtractor
  (package
    (name "qtractor")
    (version "0.9.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://downloads.sourceforge.net/qtractor/"
                                  "qtractor-" version ".tar.gz"))
              (sha256
               (base32
                "1zsikhqj5xzhw2x3b6pqlmcwz3hxx07lbbif8v3m3j41snzjic22"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))                    ; no "check" target
    (inputs
     `(("qt" ,qtbase)
       ("qtx11extras" ,qtx11extras)
       ("alsa-lib" ,alsa-lib)
       ("jack" ,jack-1)
       ("libsndfile" ,libsndfile)
       ("ladspa" ,ladspa)
       ("lv2" ,lv2)
       ("lilv" ,lilv)
       ("suil" ,suil)
       ("libsamplerate" ,libsamplerate)
       ("libvorbis" ,libvorbis)
       ("libmad" ,libmad)
       ("rubberband" ,rubberband)
       ("liblo" ,liblo)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (home-page "http://qtractor.org/")
    (synopsis "Audio/MIDI multi-track sequencer")
    (description
     "Qtractor is an Audio/MIDI multi-track sequencer application.  It uses
JACK for audio and ALSA sequencer for MIDI as multimedia infrastructures and
follows a traditional multi-track tape recorder control paradigm.")
    (license license:gpl2+)))

(define-public ams-lv2
  ;; Version 1.2.1 built with Python 3.7 raises an error in the waf-script.
  ;; Therefore, we take two more commmits than 1.2.1 that introduce an updated
  ;; waf-script and fix one error.
  (let ((commit "377d166db54a787b48979171c5652d2eb4f1bbb5")
        (revision "1"))
    (package
      (name "ams-lv2")
      (version (git-version "1.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/blablack/ams-lv2.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1ndgxcxjxwidg7436k0nb5clxkyi878k1j999sbbd1gk2fm0kcqm"))))
      (build-system waf-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'remove-sse-flags
             (lambda* (#:key system #:allow-other-keys)
               (unless (or (string-prefix? "x86_64" system)
                              (string-prefix? "i686" system))
                     (substitute* "wscript"
                       (("'-msse', '-mfpmath=sse', ") "")))
               #t)))
         #:tests? #f)) ; no tests
      (inputs
       `(("lv2" ,lv2)
         ("lvtk" ,lvtk)
         ("gtkmm" ,gtkmm-2)
         ("gtk" ,gtk+-2)
         ("cairo" ,cairo)
         ("fftw" ,fftw)))
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (home-page "https://objectivewave.wordpress.com/ams-lv2/")
      (synopsis "Port of Alsa Modular Synth internal modules into LV2")
      (description "This set of LV2 plugins is a port of the internal modules
found in Alsa Modular Synth.  These plugins are used to create modular
synthesizers and contain: VCO, VCF, VCA, LFO, slew limiter, envelopes, sample
and hold, etc.")
      (license license:gpl2))))

(define-public gxtuner
  (package
    (name "gxtuner")
    (version "2.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/brummer10/gxtuner.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fxd2akan2njlr7fpkh84830783qhh1gg7yakswqk5dd466dcn96"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "INCLUDE_L_DIR="
                            (assoc-ref %build-inputs "zita-resampler")
                            "/include/"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     `(("gtk+" ,gtk+)
       ("jack" ,jack-1)
       ("fftwf" ,fftwf)
       ("cairo" ,cairo)
       ("zita-resampler" ,zita-resampler)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/brummer10/gxtuner")
    (synopsis "Guitar tuner")
    (description "GXtuner is a simple guitar tuner for JACK with an
analogue-like user interface.")
    (license license:gpl2+)))

(define-public mod-host
  ;; The last release was in 2014 but since then hundreds of commits have
  ;; been made.
  (let ((commit "1726ad06b11323da7e1aaed690ff8aef91f702b5")
        (revision "3"))
    (package
      (name "mod-host")
      (version (string-append "0.10.6-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/moddevices/mod-host")
                      (commit commit)))
                (sha256
                 (base32
                  "1nrd37c35w6z6ldczgrwmmd9hx1n3zyvcjcgb3mi4cygqdanvspv"))
                (file-name (string-append name "-" version "-checkout"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; no tests included
         #:make-flags
         (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
               "CC=gcc")
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'fix-jack-installation-directory
             (lambda _
               ;; Do not attempt to install files to output of "jack" package.
               (substitute* "Makefile"
                 (("\\$\\(shell pkg-config --variable=libdir jack\\)")
                  "lib"))
               #t)))))
      (inputs
       `(("lilv" ,lilv)
         ("fftw" ,fftw)
         ("fftwf" ,fftwf)
         ("lv2" ,lv2)
         ("jack" ,jack-1)
         ("readline" ,readline)))
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ("python" ,python-2)))
      (home-page "https://github.com/moddevices/mod-host")
      (synopsis "LV2 host for Jack controllable via socket or command line")
      (description "mod-host is an LV2 plugin host for JACK, controllable via
socket or command line.")
      (license license:gpl3+))))

(define-public curseradio
  (let ((commit "1bd4bd0faeec675e0647bac9a100b526cba19f8d")
        (revision "1"))
    (package
      (name "curseradio")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/chronitis/curseradio.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "11bf0jnj8h2fxhpdp498189r4s6b47vy4wripv0z4nx7lxajl88i"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-lxml" ,python-lxml)
       ("python-requests" ,python-requests)
       ("python-pyxdg" ,python-pyxdg)
       ("mpv" ,mpv)))
    (home-page "https://github.com/chronitis/curseradio")
    (synopsis "Command-line Internet radio player")
    (description "Curseradio is a Curses-based radio player that uses a
tune-in sender list from @url{http://opml.radiotime.com}.")
    (license license:expat))))

(define-public pianobar
  (package
    (name "pianobar")
    (version "2016.06.02")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PromyLOPh/pianobar.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "058fbdxp7n35hxwy3b8slfy4pb4n63cb173vfmywqa06wh1dv6f6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:make-flags (list "CC=gcc" "CFLAGS=-std=c99"
                          (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (inputs
     `(("ao" ,ao)
       ("curl" ,curl)
       ("libgcrypt" ,libgcrypt)
       ("json-c" ,json-c)
       ("ffmpeg" ,ffmpeg-3.4)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://6xq.net/projects/pianobar/")
    (synopsis "Console-based pandora.com player")
    (description "pianobar is a console-based music player for the
personalized online radio pandora.com.  It has configurable keys for playing
and managing stations, can be controlled remotely via fifo, and can run
event-based scripts for scrobbling, notifications, etc.")
    (license license:expat)))

(define-public picard
  (package
    (name "picard")
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://musicbrainz.osuosl.org/pub/musicbrainz/"
                    "picard/picard-" version ".tar.gz"))
              (sha256
               (base32
                "0ds3ylpqn717fnzcjrfn05v5xram01bj6n3hwn9igmkd1jgf8vhc"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "picard/const/__init__.py"
               (("pyfpcalc")
                (string-append
                 "pyfpcalc', '"
                 (assoc-ref inputs "chromaprint") "/bin/fpcalc")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "python" "setup.py" "install"
                     (string-append "--prefix=" (assoc-ref outputs "out"))
                     "--root=/"))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (inputs
     `(("chromaprint" ,chromaprint)
       ("python-pyqt" ,python-pyqt)
       ("python-mutagen" ,python-mutagen)))
    (home-page "https://picard.musicbrainz.org/")
    (synopsis "Graphical music tagging application")
    (description
     "MusicBrainz Picard is a music tagging application, supporting multiple
formats, looking up tracks through metadata and audio fingerprints.")
    (license license:gpl2+)))

(define-public python-mutagen
  (package
    (name "python-mutagen")
    (version "1.38")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "mutagen" version))
              (sha256
               (base32
                "0rl7sxn1rcjl48fwga3dqf9f6pzspsny4ngxyf6pp337mrq0z693"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://bitbucket.org/lazka/mutagen")
    (synopsis "Read and write audio tags")
    (description "Mutagen is a Python module to handle audio metadata.  It
supports ASF, FLAC, M4A, Monkey’s Audio, MP3, Musepack, Ogg FLAC, Ogg Speex, Ogg
Theora, Ogg Vorbis, True Audio, WavPack and OptimFROG audio files.  All versions
of ID3v2 are supported, and all standard ID3v2.4 frames are parsed.  It can read
Xing headers to accurately calculate the bitrate and length of MP3s.  ID3 and
APEv2 tags can be edited regardless of audio format.  It can also manipulate Ogg
streams on an individual packet/page level.")
    (license license:gpl2))) ; "later version" never mentioned

(define-public python2-mutagen
  (package-with-python2 python-mutagen))

(define-public python-musicbrainzngs
  (package
    (name "python-musicbrainzngs")
    (version "0.6")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "musicbrainzngs" version))
              (sha256
               (base32
                "1dddarpjawryll2wss65xq3v9q8ln8dan7984l5dxzqx88d2dvr8"))))
    (build-system python-build-system)
    (arguments
     '(;; The tests fail suffer from race conditions:
       ;; https://github.com/alastair/python-musicbrainzngs/issues/211
       #:tests? #f))
    (home-page "https://python-musicbrainzngs.readthedocs.org/")
    (synopsis "Python bindings for MusicBrainz NGS webservice")
    (description "Musicbrainzngs implements Python bindings of the MusicBrainz
web service.  This library can be used to retrieve music metadata from the
MusicBrainz database.")
    ;; 'musicbrainzngs/compat.py' is ISC licensed.
    (license (list license:bsd-2 license:isc))))

(define-public python2-musicbrainzngs
  (package-with-python2 python-musicbrainzngs))

(define-public python-isrcsubmit
  (package
    (name "python-isrcsubmit")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "isrcsubmit" version))
       (sha256
        (base32
         "0jh4cni8qhri6dh83cmp0i0m0384vv0vznlygv49wj9xzh1d99qv"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-discid" ,python-discid)
       ("python-musicbrainzngs" ,python-musicbrainzngs)))
    (home-page "https://github.com/JonnyJD/musicbrainz-isrcsubmit")
    (synopsis "Submit ISRCs from CDs to MusicBrainz")
    (description "@code{isrcsubmit} is a tool to extract @dfn{International
Standard Recording Code} (ISRCs) from audio CDs and submit them to
@url{https://musicbrainz.org/, MusicBrainz}.")
    (license license:gpl3+)))

(define-public python2-pyechonest
  (package
    (name "python2-pyechonest")
    (version "9.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyechonest" version))
              (sha256
               (base32
                "1584nira3rkiman9dm81kdshihmkj21s8navndz2l8spnjwb790x"))))
    (build-system python-build-system)
    (arguments
     `(;; Python 3 is not supported:
       ;; https://github.com/echonest/pyechonest/issues/42
       #:python ,python-2))
    (home-page "https://github.com/echonest/pyechonest")
    (synopsis "Python interface to The Echo Nest APIs")
    (description "Pyechonest is a Python library for the Echo Nest API.  With
Pyechonest you have Python access to the entire set of API methods including:

@enumerate
@item artist - search for artists by name, description, or attribute, and get
back detailed information about any artist including audio, similar artists,
blogs, familiarity, hotttnesss, news, reviews, urls and video.
@item song - search songs by artist, title, description, or attribute (tempo,
duration, etc) and get detailed information back about each song, such as
hotttnesss, audio_summary, or tracks.
@item track - upload a track to the Echo Nest and receive summary information
about the track including key, duration, mode, tempo, time signature along with
detailed track info including timbre, pitch, rhythm and loudness information.
@end enumerate\n")
    (license license:bsd-3)))

(define-public python-pylast
  (package
    (name "python-pylast")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pylast" version))
              (sha256
               (base32
                "0r9h7g8i8l2mgqjwkda3v6prfbkb2im5kap1az9ppmhjm9i4jkcf"))))
    (build-system python-build-system)
    ;; Tests require network access.  See
    ;; https://github.com/pylast/pylast/issues/105
    (arguments '(#:tests? #f))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-pycodestyle" ,python-pycodestyle)
       ("python-mock" ,python-mock)
       ("python-pep8" ,python-pep8)
       ("python-pytest" ,python-pytest)
       ("python-flaky" ,python-flaky)
       ("python-pyflakes" ,python-pyflakes)
       ("python-pyyaml" ,python-pyyaml)))
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page "https://github.com/pylast/pylast")
    (synopsis "Python interface to Last.fm and Libre.fm")
    (description "A Python interface to Last.fm and other API-compatible
websites such as Libre.fm.")
    (license license:asl2.0)))

(define-public python2-pylast
  (package-with-python2 python-pylast))

(define-public instantmusic
  (let ((commit "300891d09c703525215fa5a116b9294af1c923c8")
        (revision "1"))
    (package
      (name "instantmusic")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yask123/Instant-Music-Downloader.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0j7qivaa04bpdz3anmgci5833dgiyfqqwq9fdrpl9m68b34gl773"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-requests" ,python-requests)
       ("eyed3" ,eyed3)
       ("python-beautifulsoup4" ,python-beautifulsoup4)
       ("youtube-dl" ,youtube-dl)))
    (arguments
     '(#:modules ((guix build python-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'change-directory
                    (lambda _
                      (chdir "instantmusic-0.1") #t))
                  (add-before 'check 'fix-file-permissions
                    (lambda _
                      ;; Fix some read-only files that would cause a build failure
                      (for-each (cut chmod <> #o644)
                                (find-files "instantmusic.egg-info"
                                            "PKG-INFO|.*\\.txt"))
                      #t)))))
    (home-page "http://iyask.me/Instant-Music-Downloader/")
    (synopsis "Command-line program to download a song from YouTube")
    (description "InstantMusic downloads a song from YouTube in MP3 format.
Songs can be searched by artist, name or even by a part of the song text.")
    (license license:expat))))

(define-public beets
  (package
    (name "beets")
    (version "1.4.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "beets" version))
              (patches (search-patches "beets-python-3.7-fix.patch"))
              (sha256
               (base32
                "0w3gz69s9gf5ih69d4sddgh7ndj7658m621bp742zldvjakdncrs"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-HOME
           (lambda _
             (setenv "HOME" (string-append (getcwd) "/tmp"))
             #t))
         (add-after 'unpack 'make-python3.7-compatible
           (lambda _
             ;; See <https://github.com/beetbox/beets/issues/2978>.
             (substitute* "beets/autotag/hooks.py"
              (("re\\._pattern_type") "re.Pattern"))
             #t))
         (replace 'check
           (lambda _
             (invoke "nosetests" "-v"))))))
    (native-inputs
     `(("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-flask" ,python-flask)
       ("python-mock" ,python-mock)
       ("python-mpd2" ,python-mpd2)
       ("python-nose" ,python-nose)
       ("python-pathlib" ,python-pathlib)
       ("python-pyxdg" ,python-pyxdg)
       ("python-pylast" ,python-pylast)
       ("python-rarfile" ,python-rarfile)
       ("python-responses" ,python-responses)))
    ;; TODO: Install optional plugins and dependencies.
    (inputs
     `(("python-discogs-client" ,python-discogs-client)
       ("python-jellyfish" ,python-jellyfish)
       ("python-munkres" ,python-munkres)
       ("python-musicbrainzngs" ,python-musicbrainzngs)
       ("python-mutagen" ,python-mutagen)
       ("python-pyyaml" ,python-pyyaml)
       ("python-unidecode" ,python-unidecode)))
    (home-page "http://beets.io")
    (synopsis "Music organizer")
    (description "The purpose of beets is to get your music collection right
once and for all.  It catalogs your collection, automatically improving its
metadata as it goes using the MusicBrainz database.  Then it provides a variety
of tools for manipulating and accessing your music.")
    (license license:expat)))

(define-public milkytracker
  (package
    (name "milkytracker")
    (version "1.02.00")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/milkytracker/MilkyTracker.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05a6d7l98k9i82dwrgi855dnccm3f2lkb144gi244vhk1156n0ca"))
              (modules '((guix build utils)))
              ;; Remove non-FSDG compliant sample songs.
              (snippet
               '(begin
                  (delete-file-recursively "resources/music")
                  (substitute* "CMakeLists.txt"
                    (("add_subdirectory\\(resources/music\\)") ""))
                  #t))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f ; no check target
       ;; This flag ensures that MilkyTracker links with the JACK library.
       #:configure-flags '("-DCMAKE_CXX_FLAGS=-ljack")))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("lhasa" ,lhasa)
       ("jack" ,jack-1)
       ("rtmidi" ,rtmidi)
       ("sdl" ,sdl2)
       ("zlib" ,zlib)
       ("zziplib" ,zziplib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Music tracker for working with .MOD/.XM module files")
    (description "MilkyTracker is a music application for creating .MOD and .XM
module files.  It attempts to recreate the module replay and user experience of
the popular DOS program Fasttracker II, with special playback modes available
for improved Amiga ProTracker 2/3 compatibility.")
    (home-page "http://milkytracker.titandemo.org/")
    ;; 'src/milkyplay' is under Modified BSD, the rest is under GPL3 or later.
    (license (list license:bsd-3 license:gpl3+))))

(define-public schismtracker
  (package
    (name "schismtracker")
    (version "20181223")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/schismtracker/schismtracker.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18k5j10zq39y2q294avdmar87x93k57wqmq8bpz562hdqki2mz1l"))
              (modules '((guix build utils)))
              (snippet
               ;; Remove use of __DATE__ and __TIME__ for reproducibility.
               `(begin
                  (substitute* "schism/version.c"
                    (("Schism Tracker built %s %s.*$")
                     (string-append "Schism Tracker version " ,version "\");")))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'link-libm
           (lambda _ (setenv "LIBS" "-lm") #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("python" ,python)))
    (inputs
     `(("alsa-lib" ,alsa-lib) ; for asound dependency
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("sdl" ,sdl)))
    (home-page "http://schismtracker.org")
    (synopsis "Oldschool sample-based music composition tool")
    (description
     "Schism Tracker is a reimplementation of Impulse Tracker, a program used to
create high quality music without the requirements of specialized, expensive
equipment, and with a unique \"finger feel\" that is difficult to replicate in
part.  The player is based on a highly modified version of the ModPlug engine,
with a number of bugfixes and changes to improve IT playback.")
    (license license:gpl2+)))

(define-public sooperlooper
  (package
    (name "sooperlooper")
    (version "1.7.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://essej.net/sooperlooper/sooperlooper-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0n2gdxw1fx8nxxnpzf4sj0kp6k6zi1yq59cbz6qqzcnsnpnvszbs"))
              (patches (search-patches "sooperlooper-build-with-wx-30.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CXXFLAGS=-std=gnu++11")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'add-sigc++-includes
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((sig (assoc-ref inputs "libsigc++"))
                   (xml (assoc-ref inputs "libxml2"))
                   (cwd (getcwd)))
               (setenv "CPATH"
                       (string-append sig "/include/sigc++-2.0:"
                                      sig "/lib/sigc++-2.0/include:"
                                      xml "/include/libxml2/:"
                                      cwd "/libs/pbd:"
                                      cwd "/libs/midi++")))
             (substitute* '("src/control_osc.hpp"
                            "src/gui/app_frame.hpp"
                            "src/gui/config_panel.hpp"
                            "src/gui/keys_panel.hpp"
                            "src/gui/latency_panel.hpp"
                            "src/gui/main_panel.hpp"
                            "src/gui/midi_bind_panel.hpp"
                            "src/gui/prefs_dialog.hpp")
               (("sigc\\+\\+/object.h")
                "sigc++/sigc++.h"))
             (substitute* '("src/engine.cpp"
                            "src/gui/latency_panel.cpp"
                            "src/gui/looper_panel.cpp"
                            "src/gui/main_panel.cpp")
               (("(\\(| )bind " _ pre)
                (string-append pre "sigc::bind ")))
             #t))
         (add-after 'unpack 'fix-xpm-warnings
           (lambda _
             (substitute* (find-files "." "\\.xpm$")
               (("static char") "static const char"))
             #t)))))
    (inputs
     `(("jack" ,jack-1)
       ("alsa-lib" ,alsa-lib)
       ("wxwidgets" ,wxwidgets-gtk2)
       ("libsndfile" ,libsndfile)
       ("libsamplerate" ,libsamplerate)
       ("liblo" ,liblo)
       ("rubberband" ,rubberband)
       ("libxml2" ,libxml2)
       ("libsigc++" ,libsigc++)
       ("ncurses" ,ncurses)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://essej.net/sooperlooper/")
    (synopsis "Live looping sampler")
    (description
     "SooperLooper is a live looping sampler capable of immediate loop
recording, overdubbing, multiplying, reversing and more. It allows for
multiple simultaneous multi-channel loops limited only by your computer's
available memory.")
    (license license:gpl2+)))

(define-public moc
  (package
    (name "moc")
    (version "2.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.daper.net/pub/soft/"
                                  name "/stable/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "026v977kwb0wbmlmf6mnik328plxg8wykfx9ryvqhirac0aq39pk"))))
    (build-system gnu-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("curl" ,curl)
       ("faad2" ,faad2)
       ("ffmpeg" ,ffmpeg-3.4)
       ("file" ,file)
       ("jack" ,jack-1)
       ("libid3tag" ,libid3tag)
       ("libltdl" ,libltdl)
       ("libmodplug" ,libmodplug)
       ("libmpcdec" ,libmpcdec)
       ("libmad" ,libmad)
       ("libogg" ,libogg)
       ("libvorbis" ,libvorbis)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("sasl" ,cyrus-sasl)
       ("speex" ,speex)
       ("taglib" ,taglib)
       ("wavpack" ,wavpack)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Console audio player designed to be powerful and easy to use")
    (description
     "Music on Console is a console audio player that supports many file
formats, including most audio formats recognized by FFMpeg.")
    (home-page "http://moc.daper.net")
    (license license:gpl2+)))

(define-public midicsv
  (package
    (name "midicsv")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.fourmilab.ch/webtools/midicsv/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vvhk2nf9ilfw0wchmxy8l13hbw9cnpz079nsx5srsy4nnd78nkw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases (delete 'configure))
       #:make-flags (list "CC=gcc" (string-append "INSTALL_DEST=" %output))))
    (synopsis "Convert MIDI files to and from CSV")
    (description
     "Midicsv reads a standard MIDI file and decodes it into a comma-separated
value file (CSV), which preserves all the information in the MIDI file.  The
ASCII CSV file may be loaded into a spreadsheet or database application, or
processed by a program to transform the MIDI data (for example, to key
transpose a composition or extract a track from a multi-track sequence).  A
CSV file in the format created by midicsv may be converted back into a
standard MIDI file with the csvmidi program.")
    (home-page "http://www.fourmilab.ch/webtools/midicsv/")
    (license license:public-domain)))

(define-public gx-guvnor-lv2
  (package
    (name "gx-guvnor-lv2")
    (version "0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/brummer10/GxGuvnor.lv2.git")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1wa5070j40p7f0b3kr259pzm99xb6cf2badr2capayjvgayd6gnm"))))
    (build-system gnu-build-system)
    (arguments
     `(;; The check target is used only to output a warning.
       #:tests? #f
       #:make-flags
       (list (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (substitute* "Makefile"
               (("INSTALL_DIR = .*") "INSTALL_DIR=/lib/lv2\n")
               (("install : all") "install :"))
             #t)))))
    (inputs
     `(("lv2" ,lv2)))
    (home-page "https://github.com/brummer10/GxGuvnor.lv2")
    (synopsis "Overdrive/distortion pedal simulation")
    (description "This package provides the LV2 plugin \"GxGuvnor\", a
simulation of an overdrive or distortion pedal for guitars.")
    ;; The LICENSE file says GPLv3 but the license headers in the files say
    ;; GPLv2 or later.  The whole project is released under GPLv3 or later
    ;; according to https://github.com/brummer10/GxGuvnor.lv2/issues/1
    (license license:gpl3+)))

(define-public gx-vbass-preamp-lv2
  (let ((commit "eb999b0ca0ef4da40a59e458a9ab6e7042b96c99")
        (revision "2"))
    (package (inherit gx-guvnor-lv2)
      (name "gx-vbass-preamp-lv2")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/brummer10/GxVBassPreAmp.lv2")
                      (commit commit)))
                (sha256
                 (base32
                  "0firap073ldw4nrykkd7jvyyj0jbl1nslxyzjj4kswazp99x7d9h"))
                (file-name (string-append name "-" version "-checkout"))))
      (inputs
       `(("lv2" ,lv2)
         ("gtk+" ,gtk+-2)))
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (home-page "https://github.com/brummer10/GxVBassPreAmp.lv2")
      (synopsis "Simulation of the Vox Venue Bass 100 Pre Amp Section")
      (description "This package provides the LV2 plugin \"GxVBassPreAmp\", a
pre-amplifier simulation modelled after the 1984 Vox Venue Bass 100 Pre Amp
Section."))))

(define-public gx-overdriver-lv2
  (let ((commit "ed71801987449414bf3adaa0dbfac68e8775f1ce")
        (revision "1"))
    (package (inherit gx-guvnor-lv2)
      (name "gx-overdriver-lv2")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/brummer10/GxOverDriver.lv2")
                      (commit commit)))
                (sha256
                 (base32
                  "13j614jh525fbkby79nnzwj0z1ac0c9wclyn5pfqvkmx6a7j24r8"))
                (file-name (string-append name "-" version "-checkout"))))
      (home-page "https://github.com/brummer10/GxOverDriver.lv2")
      (synopsis "Overdrive effect with level and tone control")
      (description "This package provides the LV2 plugin \"GxOverDriver\", an
overdrive effect."))))

(define-public gx-tone-mender-lv2
  (let ((commit "b6780b4a3e4782b3ed0e5882d6788f178aed138f")
        (revision "1"))
    (package (inherit gx-guvnor-lv2)
      (name "gx-tone-mender-lv2")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/brummer10/GxToneMender.lv2")
                      (commit commit)))
                (sha256
                 (base32
                  "07qdcfsvv2vdnfnjh91pfgvjdcs5y91nvwfm8c0z8fp6b4bk7a9q"))
                (file-name (string-append name "-" version "-checkout"))))
      (home-page "https://github.com/brummer10/GxToneMender.lv2")
      (synopsis "Clean boost with a 3-knob tonestack")
      (description "This package provides the LV2 plugin \"GxToneMender\", a
clean boost effect with a 3-knob tonestack."))))

(define-public gx-push-pull-lv2
  (let ((commit "7f76ae2068498643ac8671ee0930b13ee3fd8eb5")
        (revision "1"))
    (package (inherit gx-guvnor-lv2)
      (name "gx-push-pull-lv2")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/brummer10/GxPushPull.lv2")
                      (commit commit)))
                (sha256
                 (base32
                  "12f5hwck2irph0gjbj8xy8jqcqdwb8l1hlwf29k0clz52h1jhb5q"))
                (file-name (string-append name "-" version "-checkout"))))
      (home-page "https://github.com/brummer10/GxPushPull.lv2")
      (synopsis "Octave up push pull transistor fuzz simulation")
      (description "This package provides the LV2 plugin \"GxPushPull\", a
simulation of a push pull transistor fuzz effect with added high octave."))))

(define-public gx-suppa-tone-bender-lv2
  (package (inherit gx-guvnor-lv2)
    (name "gx-suppa-tone-bender-lv2")
    (version "0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/brummer10/GxSuppaToneBender.lv2.git")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "01x6bjmllkmvxfzc5xwdix7w021j26js71awv728cxsmkxgqw0zy"))))
    (home-page "https://github.com/brummer10/GxSuppaToneBender.lv2")
    (synopsis "Simulation of the Vox Suppa Tone Bender pedal")
    (description "This package provides the LV2 plugin
\"GxSuppaToneBender\", a simulation modelled after the Vox Suppa Tone Bender
pedal.")))

(define-public gx-saturator-lv2
  (let ((commit "605330f432c94b6eb3f8203cbe472befae959532")
        (revision "3"))
    (package (inherit gx-vbass-preamp-lv2)
      (name "gx-saturator-lv2")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/brummer10/GxSaturator.lv2")
                      (commit commit)))
                (sha256
                 (base32
                  "1w4nvh0rmxrv3s3hmh4fs74f3hc0jn31v00j769j7v68mqr7kawy"))
                (file-name (string-append name "-" version "-checkout"))))
      (home-page "https://github.com/brummer10/GxSaturator.lv2")
      (synopsis "Saturation effect")
      (description "This package provides the LV2 plugin \"GxSaturator\", a
saturation effect."))))

(define-public gx-hyperion-lv2
  (package (inherit gx-guvnor-lv2)
    (name "gx-hyperion-lv2")
    (version "0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/brummer10/GxHyperion.lv2.git")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1vx79s6s9if117y2g0ppdja2sv2wcny6xcfl3j1z4cipahnildxf"))))
    (home-page "https://github.com/brummer10/GxHyperion.lv2")
    (synopsis "Simulation of the Hyperion Fuzz pedal")
    (description "This package provides the LV2 plugin \"GxHyperion\", a
simulation of the Hyperion Fuzz pedal.")))

(define-public gx-voodoo-fuzz-lv2
  (package (inherit gx-guvnor-lv2)
    (name "gx-voodoo-fuzz-lv2")
    (version "0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/brummer10/GxVoodoFuzz.lv2.git")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1v0scphivri1fk4hl20j13f92i48mnx1zsil4hgnadsmm4nsfw43"))))
    (home-page "https://github.com/brummer10/GxVoodoFuzz.lv2")
    (synopsis "Fuzz effect modelled after the Voodoo Lab SuperFuzz")
    (description "This package provides the LV2 plugin \"GxVoodooFuzz\", a
simulation modelled after the Voodoo Lab SuperFuzz pedal.  It's basically a
Bosstone circuit, followed by the tone control of the FoxToneMachine in
parallel with a DarkBooster, followed by a volume control.")))

(define-public gx-super-fuzz-lv2
  (package (inherit gx-guvnor-lv2)
    (name "gx-super-fuzz-lv2")
    (version "0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/brummer10/GxSuperFuzz.lv2.git")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1jlljd9hlgfflbiizq47lv1xbbgjyx3v835mf24zmh1q5zsw4np4"))))
    (home-page "https://github.com/brummer10/GxSuperFuzz.lv2")
    (synopsis "Fuzz effect modelled after the UniVox SuperFuzz")
    (description "This package provides the LV2 plugin \"GxSuperFuzz\", an
analog simulation of the UniVox SuperFuzz pedal.  In this simulation the trim
pot, which is usually in the housing, is exposed as a control parameter.  It
adjusts the amount of harmonics.")))

(define-public gx-vintage-fuzz-master-lv2
  (package (inherit gx-guvnor-lv2)
    (name "gx-vintage-fuzz-master-lv2")
    (version "0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/brummer10/GxVintageFuzzMaster.lv2.git")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "02jb211z8rw2qr5r1z5mdxlqgiw6cbc319xpqplvn6k21c59mskv"))))
    (home-page "https://github.com/brummer10/GxVintageFuzzMaster.lv2")
    (synopsis "Fuzz effect simulation of the vintage Fuzz Master")
    (description "This package provides the LV2 plugin
\"GxVintageFuzzMaster\", a simulation of the vintage Fuzz Master pedal.")))

(define-public gx-slow-gear-lv2
  (let ((commit "5d37e775b0feef1d82feee94e2a7a2d7e57efe2d")
        (revision "3"))
    (package (inherit gx-vbass-preamp-lv2)
      (name "gx-slow-gear-lv2")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/brummer10/GxSlowGear.lv2")
                      (commit commit)))
                (sha256
                 (base32
                  "141mz69zkhk3lm54bb6wgpnghb92zm1ig7fv07240cmhydqji1q1"))
                (file-name (string-append name "-" version "-checkout"))))
      (home-page "https://github.com/brummer10/GxSlowGear.lv2")
      (synopsis "Slow gear audio effect")
      (description "This package provides the LV2 plugin \"GxSlowGear\", a
slow gear audio effect to produce volume swells."))))

(define-public gx-switchless-wah-lv2
  (let ((commit "7b08691203314612999f0ce2328cdc1161cd6665")
        (revision "2"))
    (package (inherit gx-guvnor-lv2)
      (name "gx-switchless-wah-lv2")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/brummer10/GxSwitchlessWah.lv2")
                      (commit commit)))
                (sha256
                 (base32
                  "04jqfpncmrrqn34p21w4v9m2x5a5wsqwbm4f3byxvq4vcibwxzk2"))
                (file-name (string-append name "-" version "-checkout"))))
      (home-page "https://github.com/brummer10/GxSwitchlessWah.lv2")
      (synopsis "Wah emulation with switchless activation")
      (description "This package provides the LV2 plugin \"GxSwitchlessWah\",
a simulation of an analog Wah pedal with switchless activation."))))

(define-public rkrlv2
  (let ((commit "d8c17d3c976107e5c012970276d7cf0aa059f15b")
        (revision "1"))
    (package
      (name "rkrlv2")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ssj71/rkrlv2.git")
                      (commit commit)))
                (sha256
                 (base32
                  "035z068rlafb7ibgqf34ck2lm4fab8yzlx41qh061x2xi4hxkvp0"))
                (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments '(#:tests? #f)) ; there are no tests
      (inputs
       `(("fftw" ,fftw)
         ("libsamplerate" ,libsamplerate)
         ("lv2" ,lv2)))
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (home-page "https://github.com/ssj71/rkrlv2")
      (synopsis "Rakarrack effects ported to LV2 plugins")
      (description "This package provides the Rakarrack effects as LV2
plugins.  The ports are done such that hopefully when Rakarrack gets an active
maintainer these will get merged into the original project.")
      (license license:gpl2))))

(define-public mod-utilities
  (let ((commit "80ea3ea9f52fab7f191671f4810bf90fc955a046")
        (revision "2"))
    (package
      (name "mod-utilities")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/moddevices/mod-utilities.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1v55zmzmlg0ka7341x5lsvb44amy17vk27s669ps1basd1bk5s5v"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; there are no tests
         #:make-flags
         (list (string-append "INSTALL_PATH="
                              (assoc-ref %outputs "out")
                              "/lib/lv2")
               (string-append "PREFIX=" (assoc-ref %outputs "out"))
               "CC=gcc")
         #:phases
         (modify-phases %standard-phases
           (delete 'configure))))
      (inputs
       `(("lv2" ,lv2)))
      (home-page "https://github.com/moddevices/mod-utilities")
      (synopsis "LV2 utility plugins")
      (description "This package provides LV2 audio utility plugins, such as
filters, crossovers, simple gain plugins without zipper noise, switch box
plugins, a switch trigger, a toggle switch, and a peakmeter.")
      (license license:gpl2+))))

(define-public ingen
  (let ((commit "cc4a4db33f4d126a07a4a498e053c5fb9a883be3")
        (revision "2"))
    (package
      (name "ingen")
      (version (string-append "0.0.0-" revision "."
                              (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.drobilla.net/ingen.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1wg47vjw9djn99gbnsl2bcwj4xhdid61m4wrbn2nlp797flj91ic"))))
      (build-system waf-build-system)
      (arguments
       `(#:python ,python-2
         #:tests? #f ; no "check" target
         #:configure-flags (list "--no-webkit")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-wscript
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (substitute* "wscript"
                   ;; FIXME: Our version of lv2specgen.py does not behave as
                   ;; expected.  Maybe this requires a development version of
                   ;; LV2.
                   (("lv2specgen.py") "touch ingen.lv2/ingen.html; echo")
                   ;; Add libraries to RUNPATH.
                   (("^(.+)target.*= 'src/ingen/ingen'," line prefix)
                    (string-append prefix
                                   "linkflags=[\"-Wl,-rpath="
                                   out "/lib" "\"]," line)))
                 (substitute* '("src/wscript"
                                "src/server/wscript")
                   ;; Add libraries to RUNPATH.
                   (("bld.env.PTHREAD_LINKFLAGS" line)
                    (string-append line
                                   " + [\"-Wl,-rpath=" out "/lib" "\"]")))
                 (substitute* "src/client/wscript"
                   ;; Add libraries to RUNPATH.
                   (("^(.+)target.*= 'ingen_client'," line prefix)
                    (string-append prefix
                                   "linkflags=[\"-Wl,-rpath="
                                   out "/lib" "\"]," line)))
                 (substitute* "src/gui/wscript"
                   ;; Add libraries to RUNPATH.
                   (("^(.+)target.* = 'ingen_gui.*" line prefix)
                    (string-append prefix
                                   "linkflags=[\"-Wl,-rpath="
                                   out "/lib" "\"]," line))))
               #t)))))
      (inputs
       `(("boost" ,boost)
         ("python-rdflib" ,python-rdflib)
         ("python" ,python)
         ("jack" ,jack-1)
         ("lv2" ,lv2)
         ("lilv" ,lilv)
         ("raul" ,raul-devel)
         ("ganv" ,ganv-devel)
         ("suil" ,suil)
         ("serd" ,serd)
         ("sord" ,sord)
         ("sratom" ,sratom)
         ("gtkmm" ,gtkmm-2)))
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ("python-pygments" ,python-pygments)))
      (home-page "https://drobilla.net/software/ingen")
      (synopsis "Modular audio processing system")
      (description "Ingen is a modular audio processing system for JACK and
LV2 based systems.  Ingen is built around LV2 technology and a strict
separation of engine from user interface.  The engine is controlled
exclusively through a protocol, and can execute as a headless process, with an
in-process GUI, or as an LV2 plugin.  The GUI can run as a program which
communicates over a Unix or TCP/IP socket, or as an embeddable LV2 GUI which
communicates via LV2 ports.  Any saved Ingen graph can be loaded as an LV2
plugin on any system where Ingen is installed.  This allows users to visually
develop custom plugins for use in other applications without programming.")
      (license license:agpl3+))))

(define-public qmidiarp
  (package
    (name "qmidiarp")
    (version "0.6.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/qmidiarp/qmidiarp/"
                                  version "/qmidiarp-" version ".tar.bz2"))
              (sha256
               (base32
                "043yh1p0rrbj1v840y27529m9260g55gvh1km8az4jxy7mns58r2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-qt5"
             "CXXFLAGS=-std=gnu++11")))
    (inputs
     `(("qtbase" ,qtbase)
       ("alsa-lib" ,alsa-lib)
       ("jack" ,jack-1)
       ("liblo" ,liblo)
       ("lv2" ,lv2)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (home-page "http://qmidiarp.sourceforge.net/")
    (synopsis "MIDI arpeggiator")
    (description "QMidiArp is an advanced MIDI arpeggiator, programmable step
sequencer and LFO.  It can hold any number of arpeggiator, sequencer, or LFO
modules running in parallel.")
    (license license:gpl2+)))

(define-public qmidiroute
  (package
    (name "qmidiroute")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/alsamodular/QMidiRoute/"
                                  version "/qmidiroute-" version ".tar.bz2"))
              (sha256
               (base32
                "19v1ppbglgl3z9v7xdqc0k33w71cqq8a7d6ihvfs7iz77dygrih9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-qt5"
             "CXXFLAGS=-std=gnu++11")))
    (inputs
     `(("qtbase" ,qtbase)
       ("alsa-lib" ,alsa-lib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (home-page "http://alsamodular.sourceforge.net/")
    (synopsis "MIDI event router and filter")
    (description "QMidiRoute is a MIDI event router and filter.  MIDI note,
control change, program change and pitch bend events are logged, and can be
filtered, redirected and transformed into other events according to MIDI maps
defined as tabs in the main control surface.")
    (license license:gpl2+)))

(define-public seq24
  (package
    (name "seq24")
    (version "0.9.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://launchpad.net/seq24/trunk/"
                                  version "/+download/seq24-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "12dphdhnvfk1k0vmagi1v2lhyxjyj1j3cz6ksjw0ydcvid1x8ap2"))
              (patches (search-patches "seq24-rename-mutex.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "CXXFLAGS=-std=gnu++11")))
    (inputs
     `(("gtkmm" ,gtkmm-2)
       ("alsa-lib" ,alsa-lib)
       ("jack" ,jack-1)
       ("lash" ,lash)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://edge.launchpad.net/seq24/")
    (synopsis "Real-time MIDI sequencer")
    (description "Seq24 is a real-time MIDI sequencer.  It was created to
provide a very simple interface for editing and playing MIDI loops.")
    (license license:gpl2+)))

(define-public python-discogs-client
  (package
    (name "python-discogs-client")
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "discogs-client" version))
              (sha256
               (base32
                "053ld2psh0yj3z0kg6z5bn4y3cr562m727494n0ayhgzbkjbacly"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-oauthlib" ,python-oauthlib)
       ("python-requests" ,python-requests)))
    (native-inputs
     `(("python-six" ,python-six)))
    (home-page "https://github.com/discogs/discogs_client")
    (synopsis "Official Python client for the Discogs API")
    (description "This is the official Discogs API client for Python. It enables
you to query the Discogs database for information on artists, releases, labels,
users, Marketplace listings, and more.  It also supports OAuth 1.0a
authorization, which allows you to change user data such as profile information,
collections and wantlists, inventory, and orders.")
    (license license:bsd-2)))

(define-public python2-discogs-client
  (package-with-python2 python-discogs-client))

(define-public libsmf
  (package
    (name "libsmf")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       ;; SF download page says development moved, but the link it points to
       ;; is gone (https://github.com/nilsgey/libsmf).  Someone else adopted
       ;; it but made no release so far (https://github.com/stump/libsmf).
       (uri (string-append "mirror://sourceforge/libsmf/libsmf/"
                           version "/libsmf-" version ".tar.gz"))
       (sha256
        (base32
         "16c0n40h0r56gzbh5ypxa4dwp296dan3jminml2qkb4lvqarym6k"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "static")) ; 88KiB of .a files
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'move-static-libraries
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Move static libraries to the "static" output.
             (let* ((out    (assoc-ref outputs "out"))
                    (lib    (string-append out "/lib"))
                    (static (assoc-ref outputs "static"))
                    (slib   (string-append static "/lib")))
               (mkdir-p slib)
               (for-each (lambda (file)
                           (install-file file slib)
                           (delete-file file))
                         (find-files lib "\\.a$"))
               #t))))))
    (inputs
     `(("readline" ,readline)
       ("glib" ,glib)))
    (native-inputs
     `(("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (home-page "http://libsmf.sourceforge.net/")
    (synopsis "Standard MIDI File format library")
    (description
     "LibSMF is a C library for handling SMF (\"*.mid\") files.  It transparently handles
conversions between time and pulses, tempo map handling and more.  The only dependencies
are a C compiler and glib.  Full API documentation and examples are included.")
    (license license:bsd-2)))

(define-public lmms
  (package
    (name "lmms")
    (version "1.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/LMMS/lmms.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "03hhymc6d73fa3wbcqb7rm1l03zkw605k5i9kvkvjmv488bqh3pd"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-ldflags
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "LDFLAGS"
                     (string-append
                      "-Wl,-rpath=\""
                      (assoc-ref outputs "out") "/lib/lmms"
                      ":"
                      (assoc-ref outputs "out") "/lib/lmms/ladspa"
                      "\""))
             #t))
         (add-before 'configure 'remove-Werror
           (lambda _
             (substitute* "CMakeLists.txt"
               (("SET\\(WERROR_FLAGS \"\\$\\{WERROR_FLAGS\\} -Werror")
                "SET(WERROR_FLAGS \"${WERROR_FLAGS}"))
             #t))
         (add-before 'reset-gzip-timestamps 'make-manpages-writable
           (lambda* (#:key outputs #:allow-other-keys)
             (map (lambda (file)
                    (make-file-writable file))
                  (find-files (string-append (assoc-ref outputs "out")
                                             "/share/man")
                              ".*\\.gz$"))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("sdl" ,sdl)
       ("qt" ,qt-4)
       ("fltk" ,fltk)
       ("libogg" ,libogg)
       ("libsamplerate" ,libsamplerate)
       ("fluidsynth" ,fluidsynth-1)         ;XXX: try using 2.x when updating
       ("libvorbis" ,libvorbis)
       ("alsa-lib" ,alsa-lib)
       ("portaudio" ,portaudio)
       ("ladspa" ,ladspa)
       ("libsndfile1" ,libsndfile)
       ("libxft" ,libxft)
       ("freetype2" ,freetype)
       ("fftw3f" ,fftwf)))
    (home-page "https://lmms.io/")
    (synopsis "Music composition tool")
    (description "LMMS is a digital audio workstation.  It includes tools for sequencing
melodies and beats and for mixing and arranging songs.  LMMS includes instruments based on
audio samples and various soft sythesizers.  It can receive input from a MIDI keyboard.")
    (license license:gpl2+)))

(define-public musescore
  (package
    (name "musescore")
    (version "3.0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/musescore/MuseScore.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1inf6zdyh6yspjv1i7g6rw9wn90vki1s2qgilkp0j4aphayj4mic"))
              (modules '((guix build utils)))
              (snippet
               ;; Un-bundle OpenSSL and remove unused libraries.
               '(begin
                  (substitute* "thirdparty/kQOAuth/CMakeLists.txt"
                    (("-I \\$\\{PROJECT_SOURCE_DIR\\}/thirdparty/openssl/include ")
                     ""))
                  (substitute* "thirdparty/kQOAuth/kqoauthutils.cpp"
                    (("#include <openssl/.*") ""))
                  (for-each delete-file-recursively
                            '("thirdparty/freetype"
                              "thirdparty/openssl"
                              "thirdparty/portmidi"))
                  #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       `("-DBUILD_CRASH_REPORTER=OFF"
         "-DBUILD_WEBENGINE=OFF"
         "-DDOWNLOAD_SOUNDFONT=OFF"
         "-DUSE_SYSTEM_FREETYPE=ON")
       ;; There are tests, but no simple target to run.  The command used to
       ;; run them is:
       ;;
       ;;   make debug && sudo make installdebug && cd \
       ;;   build.debug/mtest && make && ctest
       ;;
       ;; Basically, it requires to start a whole new build process.
       ;; So we simply skip them.
       #:tests? #f))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("freetype" ,freetype)
       ("gtk+-bin" ,gtk+ "bin")         ;for gtk-update-icon-cache
       ("jack" ,jack-1)
       ("lame" ,lame)
       ("libogg" ,libogg)
       ("libsndfile" ,libsndfile)
       ("libvorbis" ,libvorbis)
       ("portaudio" ,portaudio)
       ("portmidi" ,portmidi)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtscript" ,qtscript)
       ("qtsvg" ,qtsvg)
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (synopsis "Music composition and notation software")
    (description "MuseScore is a music score typesetter.  Its main purpose is
the creation of high-quality engraved musical scores in a WYSIWYG environment.

It supports unlimited staves, linked parts and part extraction, tablature,
MIDI input, percussion notation, cross-staff beaming, automatic transposition,
lyrics (multiple verses), fretboard diagrams, and in general everything
commonly used in sheet music.  Style options and style sheets to change the
appearance and layout are provided.

MuseScore can also play back scores through the built-in sequencer and SoundFont
sample library.")
    (home-page "https://musescore.org")
    (license license:gpl2)))

(define-public muse-sequencer
  (package
    (name "muse-sequencer")
    (version "3.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/muse-sequencer/muse.git")
                    (commit (string-append "muse_"
                                           (string-map (lambda (c)
                                                         (if (char=? c #\.)
                                                             #\_ c)) version)))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1nninz8qyqlxxjdnrm79y3gr3056pga9l2fsqh674jd3cjvafya3"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; there is no test target
       #:configure-flags
       (list "-DENABLE_LV2_SUPPLIED=OFF"
             "-DENABLE_RTAUDIO=OFF"    ; FIXME: not packaged
             "-DENABLE_INSTPATCH=OFF"  ; FIXME: not packaged
             "-DENABLE_VST_NATIVE=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "muse3"))))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("lash" ,lash)
       ("jack" ,jack-1)
       ("liblo" ,liblo)
       ("dssi" ,dssi)
       ("ladspa" ,ladspa)
       ("lv2" ,lv2)
       ("lilv" ,lilv)
       ("sord" ,sord)
       ("libsndfile" ,libsndfile)
       ("libsamplerate" ,libsamplerate)
       ("fluidsynth" ,fluidsynth)
       ("pcre" ,pcre)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (home-page "http://muse-sequencer.org")
    (synopsis "MIDI/Audio sequencer")
    (description "MusE is a MIDI/Audio sequencer with recording and editing
capabilities.  Its audio sequencer supports the LADSPA, DSSI, and LV2 audio
plugin formats; the MIDI sequencer provides a piano roll, a drum editor, a
list view, and a score editor.  MusE aims to be a complete multitrack virtual
studio.")
    (license license:gpl2+)))

(define-public dssi
  (package
    (name "dssi")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/dssi/dssi/" version
                    "/dssi-" version ".tar.gz"))
              (sha256
               (base32
                "0kl1hzhb7cykzkrqcqgq1dk4xcgrcxv0jja251aq4z4l783jpj7j"))))
    (build-system gnu-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("jack" ,jack-1)
       ("ladspa" ,ladspa)
       ("libsamplerate" ,libsamplerate)
       ("libsndfile" ,libsndfile)
       ("liblo" ,liblo)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Audio plugin API for soft synths and effects")
    (description "DSSI is a plugin API for software instruments with user
interfaces, permitting them to be hosted in-process by audio applications.
It is intended to be simple, GUI-toolkit-agnostic, and slightly biased
towards familiarity with MIDI.  The DSSI distribution package contains
a JACK/ALSA-sequencer reference host and some plugins as well as the
specification and header.")
    (home-page "http://dssi.sourceforge.net/")
    ;; The DSSI interface is LGPL2.1+, some tests and examples are GPL2+.
    ;; The vast majority of examples are in the public domain.
    (license (list license:lgpl2.1+ license:gpl2+))))

(define-public rosegarden
  (package
    (name "rosegarden")
    (version "18.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/rosegarden/rosegarden/"
                    version "/rosegarden-" version ".tar.bz2"))
              (sha256
               (base32
                "15i9fm0vkn3wsgahaxqi1j5zs0wc0j3wdwml0x49084gk2p328vb"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DCMAKE_BUILD_TYPE=Release")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda _
             (substitute* "CMakeLists.txt"
               (("BUILD_TESTING OFF") "BUILD_TESTING ON")
               ;; Make tests work.
               ((" -fvisibility=hidden") ""))
             #t))
         (add-after 'unpack 'fix-references
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/gui/general/ProjectPackager.cpp"
               (("\"flac\\>")
                (string-append "\"" (assoc-ref inputs "flac") "/bin/flac"))
               (("\"wavpack\\>")
                (string-append "\"" (assoc-ref inputs "wavpack") "/bin/wavpack"))
               (("\"wvunpack\\>")
                (string-append "\"" (assoc-ref inputs "wavpack") "/bin/wvunpack"))
               (("\"bash\\>")
                (string-append "\"" (assoc-ref inputs "bash") "/bin/bash"))
               (("\"tar\\>")
                (string-append "\"" (assoc-ref inputs "tar") "/bin/tar")))
             (substitute* "src/gui/general/LilyPondProcessor.cpp"
               (("\"convert-ly\\>")
                (string-append "\"" (assoc-ref inputs "lilypond") "/bin/convert-ly"))
               (("\"lilypond\\>")
                (string-append "\"" (assoc-ref inputs "lilypond") "/bin/lilypond")))
             #t))
         (add-after 'unpack 'make-reproducible
           (lambda _
             ;; Prevent Last-Modified from being written.
             ;; The "*.qm" files that are used in locale.qrc would have a new
             ;; mtime otherwise that is written into qrc_locale.cpp in the
             ;; end - except when we disable it.
             (substitute* "src/CMakeLists.txt"
               (("COMMAND [$][{]QT_RCC_EXECUTABLE[}]")
                "COMMAND ${QT_RCC_EXECUTABLE} --format-version 1")
               ;; Extraneous.
               ;(("qt5_add_resources[(]rg_SOURCES ../data/data.qrc[)]")
               ; "qt5_add_resources(rg_SOURCES ../data/data.qrc OPTIONS --format-version 1)")
                )
             ;; Make hashtable traversal order predicable.
             (setenv "QT_RCC_TEST" "1") ; important
             #t))
         (add-before 'check 'prepare-check
           (lambda _
             (setenv "QT_QPA_PLATFORM" "offscreen")
             ;; Tests create files in $HOME/.local/share/rosegarden .
             (mkdir-p "/tmp/foo")
             (setenv "HOME" "/tmp/foo")
             (setenv "XDG_RUNTIME_DIR" "/tmp/foo")
             #t)))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("bash" ,bash)
       ("dssi" ,dssi)
       ("flac" ,flac)
       ("fftwf" ,fftwf)
       ("jack" ,jack-1)
       ("ladspa" ,ladspa)
       ("liblo" ,liblo)
       ("libsamplerate" ,libsamplerate)
       ("lilypond" ,lilypond)
       ("lrdf" ,lrdf)
       ("qtbase" ,qtbase)
       ("tar" ,tar)
       ("lirc" ,lirc)
       ("wavpack" ,wavpack)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qtlinguist" ,qttools)))
    (synopsis "Music composition and editing environment based around a MIDI
sequencer")
    (description "Rosegarden is a music composition and editing environment
based around a MIDI sequencer that features a rich understanding of music
notation and includes basic support for digital audio.")
    (home-page "https://www.rosegardenmusic.com/")
    (license license:gpl2)))

(define-public patchmatrix
  ;; There have been no releases for more than a year.
  (let ((commit "a0b0b1e791f4574d5abd059cfe1819c71e8b18d5")
        (revision "1"))
    (package
      (name "patchmatrix")
      (version (git-version "0.12.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/OpenMusicKontrollers/patchmatrix.git")
                      (commit commit)))
                (file-name (string-append "patchmatrix-" version "-checkout"))
                (sha256
                 (base32
                  "0pph4ra7aci3rbpqvvr564pi16vxrk448bmvp8985cd9lbjlrp3m"))))
      (build-system meson-build-system)
      (arguments
       '(#:tests? #f          ; no test target
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map make-file-writable
                    (find-files (assoc-ref outputs "out") ".*\\.gz$"))
               #t)))))
      (inputs
       `(("jack" ,jack-1)
         ("lv2" ,lv2)
         ("mesa" ,mesa)))
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (home-page "https://github.com/OpenMusicKontrollers/patchmatrix")
      (synopsis "Simple JACK patch bay")
      (description "PatchMatrix is a patch bay for the JACK audio connection
kit.  It provides a patch bay in flow matrix style for audio, MIDI, CV, and
OSC connections.")
      (license license:artistic2.0))))

(define-public sorcer
  (package
    (name "sorcer")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/openAVproductions/"
                                  "openAV-Sorcer/archive/release-"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "07iyqj28wm0xc4arrq893bm12xjpz65db7ynrlmf6w8krg8wjmd0"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no tests included
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-architecture-specific-flags
           (lambda _
             (substitute* "CMakeLists.txt"
               (("-msse2 -mfpmath=sse") ""))
             #t))
         (add-after 'unpack 'build-faust-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "faust"
               (delete-file "main.cpp")
               (invoke "faust" "-i"
                       "-a" "lv2synth.cpp"
                       "-o" "main.cpp" "main.dsp")))))))
    (inputs
     `(("boost" ,boost)
       ("lv2" ,lv2)
       ("ntk" ,ntk)))
    (native-inputs
     `(("faust" ,faust)
       ("pkg-config" ,pkg-config)))
    (home-page "http://openavproductions.com/sorcer/")
    (synopsis "Wavetable LV2 plugin synth")
    (description "Sorcer is a wavetable LV2 plugin synthesizer, targeted at
the electronic or dubstep genre.")
    (license license:gpl3+)))

(define-public libmygpo-qt
  (package
    (name "libmygpo-qt")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://stefan.derkits.at/files/"
                                  "libmygpo-qt/libmygpo-qt." version ".tar.gz"))
              (sha256
               (base32
                "1kg18qrq2rsswgzhl65r3mlyx7kpqg4wwnbp4yiv6svvmadmlxl2"))
              (patches (search-patches "libmygpo-qt-fix-qt-5.11.patch"
                                       "libmygpo-qt-missing-qt5-modules.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("qt" ,qtbase)))
    (arguments
     `(#:configure-flags '("-DMYGPO_BUILD_TESTS=ON")
       ;; TODO: Enable tests when https://github.com/gpodder/gpodder/issues/446
       ;; is fixed.
       #:tests? #f))
    (home-page "http://wiki.gpodder.org/wiki/Libmygpo-qt")
    (synopsis "Qt/C++ library wrapping the gpodder web service")
    (description "@code{libmygpo-qt} is a Qt/C++ library wrapping the
@url{https://gpodder.net} APIs.  It allows applications to discover, manage
and track podcasts.")
    (license license:lgpl2.1+)))

(define-public sonivox-eas
  (package
    (name "sonivox-eas")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pedrolcl/Linux-SonivoxEas.git")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0l9gs00p5g4k4qy6i7nv1mfi2n2wnsycwjrgrh9hxzam4irf2mw2"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f)) ; there are no tests
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("drumstick" ,drumstick)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/pedrolcl/Linux-SonivoxEas")
    (synopsis "MIDI synthesizer library")
    (description "This project is a real time General MIDI synthesizer based
on the Sonivox EAS Synthesizer by Google.  It does not need external
soundfonts, using embedded samples instead.")
    ;; Sonivox is released under the ASL2.0; the rest of the code is under
    ;; GPLv2+.
    (license (list license:gpl2+ license:asl2.0))))

(define-public whysynth
  (package
    (name "whysynth")
    (version "20170701")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://smbolton.com/whysynth/whysynth-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "02qbn0hbvn1iym4zxv35b201blg31yjpgh71h8db0j5zls2xc0m6"))))
    (build-system gnu-build-system)
    (inputs
     `(("dssi" ,dssi)
       ("liblo" ,liblo)
       ("fftwf" ,fftwf)
       ("gtk+" ,gtk+-2)
       ("ladspa" ,ladspa)
       ("alsa-lib" ,alsa-lib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://smbolton.com/whysynth.html")
    (synopsis "DSSI software synthesizer")
    (description "WhySynth is a versatile softsynth which operates as a plugin
for the DSSI Soft Synth Interface.  A brief list of features:

@enumerate
@item 4 oscillators, 2 filters, 3 LFOs, and 5 envelope generators per voice.
@item 11 oscillator modes: minBLEP, wavecycle, chorused wavecycle,
  asynchronous granular, three FM modes, waveshaper, noise, PADsynth, and phase
  distortion.
@item 10 filter modes.
@item flexible modulation and mixdown options, plus effects.
@end enumerate
")
    (license license:gpl2+)))

(define-public libdiscid
  (package
    (name "libdiscid")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://ftp.musicbrainz.org/pub/musicbrainz/libdiscid/libdiscid-"
             version ".tar.gz"))
       (sha256
        (base32
         "1f9irlj3dpb5gyfdnb1m4skbjvx4d4hwiz2152f83m0d9jn47r7r"))))
    (arguments `(#:test-target "check"))
    (build-system cmake-build-system)
    (home-page "https://musicbrainz.org/doc/libdiscid")
    (synopsis "Disc id reader library")
    (description "libdiscid is a C library for creating MusicBrainz and freedb
disc IDs from audio CDs.  It reads a CD's table of contents (TOC) and generates
an identifier which can be used to lookup the CD at MusicBrainz.  Additionally,
it provides a submission URL for adding the disc ID to the database and gathers
ISRCs and the MCN (=UPC/EAN) from disc.")
    (license license:lgpl2.1+)))

(define-public python-discid
  (package
    (name "python-discid")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "discid" version))
       (sha256
        (base32
         "1fgp67nhqlbvhhwrcxq5avil7alpzw4s4579hlyvxzbphdnbz8vq"))))
    (build-system python-build-system)
    (inputs
     `(("libdiscid" ,libdiscid)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-libdiscid
           ;; Set path of libdiscid
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((discid (assoc-ref inputs "libdiscid")))
               (substitute* "discid/libdiscid.py"
                 (("lib_name = (.*)$" all name)
                  (string-append "lib_name = \"" discid
                                 "/lib/libdiscid.so.0\"\n")))
               #t))))))
    (home-page "https://python-discid.readthedocs.io/")
    (synopsis "Python bindings for Libdiscid")
    (description
     "This package provides Python bindings for the Libdiscid library.  The
main purpose is the calculation of @url{https://musicbrainz.org/doc/Disc%20ID,
Disc IDs} for use with the MusicBrainz database.  Additionally the disc
@dfn{Media Catalog Number} (MCN) and track @dfn{International Standard
Recording Code} (ISRC) can be extracted.}")
    (license license:lgpl3+)))

(define-public libmusicbrainz
  (package
    (name "libmusicbrainz")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/metabrainz/libmusicbrainz/releases/download/release-"
             version "/libmusicbrainz-" version ".tar.gz"))
       (sha256
        (base32
         "0ikb9igyyk28jm34raxfzkw2qyn4nzzwsymdyprp7cmvi6g2ajb7"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; requires network connections
             ;; (invoke "tests/mbtest")
             (invoke "tests/ctest")
             #t)))))
    (inputs `(("neon" ,neon)
              ("libxml2" ,libxml2)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://musicbrainz.org/doc/libmusicbrainz")
    (synopsis "MusicBrainz client library")
    (description "The MusicBrainz Client Library (libmusicbrainz), also known as
mb_client, is a development library geared towards developers who wish to add
MusicBrainz lookup capabilities to their applications.")
    (license license:lgpl2.1+)))

(define-public perl-musicbrainz-discid
  (package
    (name "perl-musicbrainz-discid")
    (version "0.04")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/N/NJ/NJH/MusicBrainz-DiscID-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1i4qk1qfcmxdibqkyfjrrjdq2zk42vjcz590qgiyc47fi9p6xx1j"))))
    (build-system perl-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("which" ,which)))
    (inputs `(("libdiscid" ,libdiscid)))
    (home-page "https://metacpan.org/release/MusicBrainz-DiscID")
    (synopsis "Perl interface to the MusicBrainz libdiscid library")
    (description
     "The @code{MusicBrainz::DiscID} module is a Perl interface to the
MusicBrainz libdiscid library, allowing you to manipulate digital audio
compact disc (CDDA) identifiers.")
    (license license:gpl2)))

(define-public perl-webservice-musicbrainz
  (package
    (name "perl-webservice-musicbrainz")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/B/BF/BFAIST/WebService-MusicBrainz-"
                    version ".tar.gz"))
              (sha256
               (base32
                "182z3xjajk6s7k5xm3kssjy3hqx2qbnq4f8864hma098ryy2ph3a"))))
    (build-system perl-build-system)
    (arguments
     ;; Tests try to connect to http://musicbrainz.org.
     '(#:tests? #f))
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-mojolicious" ,perl-mojolicious)))
    (home-page "https://metacpan.org/release/WebService-MusicBrainz")
    (synopsis "Web service API to the MusicBrainz database")
    (description
     "This module searches the MusicBrainz database through their web service
at @code{musicbrainz.org}.")
    (license license:perl-license)))

(define-public clyrics
  (package
    (name "clyrics")
    (version "0.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/trizen/clyrics.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0vfjy555achgi5zwa34x4q7r692nc6bq4jx518a5w4m7k5fvm71j"))))
    (build-system trivial-build-system)
    (inputs
     `(("bash" ,bash)                   ; for the wrapped program
       ("perl" ,perl)
       ("perl-www-mechanize" ,perl-www-mechanize)
       ("perl-lwp-protocol-https" ,perl-lwp-protocol-https)
       ;; Required or else LWP will fail with "GET https://www.google.com/ ==>
       ;; 500 Can't verify SSL peers without knowing which Certificate
       ;; Authorities to trust".
       ("perl-mozilla-ca" ,perl-mozilla-ca)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (ice-9 match)
                                (srfi srfi-26))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (output (assoc-ref %outputs "out")))
                     (setenv "PATH"
                             (string-append
                              (assoc-ref %build-inputs "bash") "/bin" ":"
                              (assoc-ref %build-inputs "perl") "/bin" ":"))
                     (copy-recursively source (getcwd))
                     (patch-shebang "clyrics")
                     (substitute* "clyrics"
                       (("/usr/share") output))
                     (install-file "clyrics" (string-append output "/bin"))
                     (wrap-program (string-append output "/bin/clyrics")
                       `("PERL5LIB" ":" =
                         ,(delete
                           ""
                           (map (match-lambda
                                 (((?  (cut string-prefix? "perl-" <>) name) . dir)
                                  (string-append dir "/lib/perl5/site_perl"))
                                 (_ ""))
                                %build-inputs))))
                     (copy-recursively "plugins" (string-append output "/clyrics"))
                     #t))))
    (home-page "https://github.com/trizen/clyrics")
    (synopsis "Extensible lyrics fetcher")
    (description
     "Clyrics is an extensible command-line tool to fetch the lyrics of songs.
It can be used in daemon mode along with the Music-on-Console (MOC) and cmus
console music players.")
    (license license:gpl3+)))

(define-public demlo
  (let ((commit "fe9ec4c8ac2fa995ec18e6ac86d50d46df06ec01")
        (revision "0"))
    (package
      (name "demlo")
      (version (git-version "3.8" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://gitlab.com/ambrevar/demlo")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1afkbqhzn6da7zaf5ab7dvyqj1izqhzprwfb4hw448fllda9bdvk"))))
      (build-system go-build-system)
      (native-inputs
       `(("lua" ,lua)
         ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
         ("go-github-com-mattn-go-colorable" ,go-github-com-mattn-go-colorable)
         ("go-github-com-aarzilli-golua" ,go-github-com-aarzilli-golua)
         ("go-gitlab-com-ambrevar-damerau" ,go-gitlab-com-ambrevar-damerau)
         ("go-gitlab-com-ambrevar-golua-unicode" ,go-gitlab-com-ambrevar-golua-unicode)
         ("go-github-com-mgutz-ansi" ,go-github-com-mgutz-ansi)
         ("go-github-com-michiwend-gomusicbrainz" ,go-github-com-michiwend-gomusicbrainz)
         ("go-github-com-stevedonovan-luar" ,go-github-com-stevedonovan-luar)
         ("go-github-com-wtolson-go-taglib" ,go-github-com-wtolson-go-taglib)
         ("go-github-com-yookoala-realpath" ,go-github-com-yookoala-realpath)))
      (inputs
       `(("chromaprint" ,chromaprint)
         ("ffmpeg" ,ffmpeg)))
      (arguments
       `(#:import-path "gitlab.com/ambrevar/demlo"
         #:phases
         (modify-phases %standard-phases
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (ffmpeg (assoc-ref inputs "ffmpeg"))
                     (chromaprint (assoc-ref inputs "chromaprint")))
                 (wrap-program (string-append out "/bin/demlo")
                   `("XDG_DATA_DIRS" ":" prefix (,out))
                   `("PATH" ":" prefix
                     ,(map (lambda (dir)
                             (string-append dir "/bin:"
                                            dir "/sbin"))
                           (list ffmpeg chromaprint))))
                 #t)))
           (add-after 'install 'install-scripts
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (root (string-append out "/src/gitlab.com/ambrevar/demlo"))
                      (xdg-data-dirs (string-append out "/demlo")))
                 (copy-recursively (string-append root "/actions")
                                   (string-append xdg-data-dirs "/actions"))
                 (copy-recursively (string-append root "/scripts")
                                   (string-append xdg-data-dirs "/scripts"))
                 (install-file (string-append root "/config.lua") xdg-data-dirs)
                 ;; TODO: Test fish completion.
                 (install-file (string-append root "/completion/demlo.fish")
                               (string-append out "/share/fish/vendor_completions.d"))
                 #t))))))
      (home-page "https://gitlab.com/ambrevar/demlo")
      (synopsis "Dynamic and extensible music library organizer")
      (description "Demlo is a music library organizer.  It can encode, fix
case, change folder hierarchy according to tags or file properties, tag from
an online database, copy covers while ignoring duplicates or those below a
quality threshold, and much more.  It makes it possible to manage your
libraries uniformly and dynamically.  You can write your own rules to fit your
needs best.

Demlo can address any of these recurring music library issues (and much more):

@itemize
@item Fix the lack of folder structure.
@item Normalize tags, fix their case, chose which tags to keep and which to
discard.
@item Handle lossy and lossless audio differently.
@item Handle mp3 id3tags hell...
@item Handle multiple covers, whether embedded and/or external, resize covers,
discard bad quality ones.
@end itemize\n")
      (license license:expat))))

(define-public lpd8editor
  (package
    (name "lpd8editor")
    (version "0.0.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/charlesfleche/lpd8editor.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1v3fz8h1zs7kkl80faah79pp6yqwz0j7bkv3dbmh2hp42zbfiz1q"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'setenv
           (lambda _
             (setenv "INSTALL_PREFIX"
                     (assoc-ref %outputs "out"))))
         (delete 'configure) ; no configure script
         (add-before 'build 'qmake
           (lambda _ (invoke "qmake"))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa" ,alsa-lib)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)))
    (synopsis "Graphical editor for the Akai LPD8 MIDI controller")
    (description "lpd8editor is a graphical patch editor for the Akai LPD8 MIDI
controller.")
    (home-page "https://github.com/charlesfleche/lpd8editor")
    (license license:expat)))
