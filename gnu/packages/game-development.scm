;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Tomáš Čech <sleep_walker@suse.cz>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2018 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015, 2016, 2017 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2020 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018 Julian Graham <joolean@gmail.com>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019, 2020, 2021 Leo Prikler <leo.prikler@student.tugraz.at>
;;; Copyright © 2019 Jethro Cao <jethrocao@gmail.com>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2021 Alexandru-Sergiu Marton <brown121407@posteo.ro>
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

(define-module (gnu packages game-development)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system scons)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages mono)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages stb)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public bullet
  (package
    (name "bullet")
    (version "2.89")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/bulletphysics/bullet3/")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10ncf2z474jnv7p5lv01ak2mk2hib3rj5rz1zr8v2v5pnciqbijl"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file (find-files "build3" "premake*"))
                  (with-directory-excursion "examples/ThirdPartyLibs"
                    (for-each delete-file-recursively
                              '("Gwen" "clsocket" "enet" "glad" "imgui"
                                "lua-5.2.3" "midi" "minizip" "openvr"
                                "optionalX11" "serial" "zlib")))
                  ;; These need files from ThirdPartyLibs.
                  (substitute* "Extras/CMakeLists.txt"
                    (("BulletRobotics") ""))
                  ;; Tests fail on linking, cannot find -lBussIK.
                  (substitute* "test/CMakeLists.txt"
                    ((" InverseDynamics")
                     "../examples/ThirdPartyLibs/BussIK InverseDynamics"))
                  ;  (("SharedMemory") ""))
                  #t))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags (list "-DBUILD_SHARED_LIBS=ON"
                               "-DBUILD_CPU_DEMOS=OFF"
                               "-DBUILD_OPENGL3_DEMOS=OFF"
                               "-DBUILD_BULLET2_DEMOS=OFF"
                               (string-append  "-DCMAKE_CXX_FLAGS=-fPIC "
                                               (or (getenv "CXXFLAGS") "")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-failing-tests
           ;; These tests fail specifically after removing 3rd party code.
           (lambda _
             (substitute* "test/SharedMemory/CMakeLists.txt"
               (("ADD_TEST") "# ADD_TEST"))
             (substitute* "test/InverseDynamics/CMakeLists.txt"
               (("ADD_TEST\\(Test_BulletInverseForward")
                "# ADD_TEST(Test_BulletInverseForward"))
             #t)))))
    (inputs
     `(("glu" ,glu)
       ("libx11" ,libx11)
       ("mesa" ,mesa)))
    (home-page "https://pybullet.org/wordpress/")
    (synopsis "3D physics engine library")
    (description
     "Bullet is a physics engine library usable for collision detection.  It
is used in some video games and movies.")
    (license license:zlib)))

(define-public deutex
  (package
   (name "deutex")
   (version "5.2.2")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "https://github.com/Doom-Utils/deutex"
                          "/releases/download/v" version "/"
                          "deutex-" version ".tar.zst"))
      (sha256
       (base32 "0psb2za6ldrlak7s8pjvli98ij5yiwjx8j1ms2v7rj9yadx0xv8h"))))
   (build-system gnu-build-system)
   (inputs
    `(("libpng" ,libpng)))
   (native-inputs
    `(("asciidoc" ,asciidoc)
      ("pkg-config" ,pkg-config)
      ("zstd" ,zstd)))
   (home-page "https://github.com/Doom-Utils/deutex")
   (synopsis "WAD file composer for Doom and related games")
   (description
    "DeuTex is a wad composer for Doom, Heretic, Hexen and Strife. It can be
used to extract the lumps of a wad and save them as individual files.
Conversely, it can also build a wad from separate files.  When extracting a
lump to a file, it does not just copy the raw data, it converts it to an
appropriate format (such as PPM for graphics, Sun audio for samples, etc.).
Conversely, when it reads files for inclusion in pwads, it does the necessary
conversions (for example, from PPM to Doom picture format).  In addition,
DeuTex has functions such as merging wads, etc.")
   (license license:gpl2+)))

(define-public grfcodec
  (package
    (name "grfcodec")
    (version "6.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://binaries.openttd.org/extra/"
                           name "/" version "/" name "-" version
                           "-source.tar.xz"))
       (sha256
        (base32 "08admgnpqcsifpicbm56apgv360fxapqpbbsp10qyk8i22w1ivsk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ;no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ;no configure script
         (replace 'install              ;no install target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc"))
                    (man (string-append out "/share/man/man1")))
               (for-each (lambda (file)
                           (install-file file bin))
                         '("grfcodec" "grfid" "grfstrip" "nforenum"))
               (install-file "COPYING" doc)
               (with-directory-excursion "docs"
                 (for-each (lambda (file)
                             (install-file (string-append file ".txt") doc))
                           '("auto_correct" "commands" "grf" "grfcodec" "grftut"
                             "readme" "readme.rpn"))
                 (for-each (lambda (file)
                             (install-file file man))
                           (find-files "." "\\.1"))))
             #t)))))
    (inputs
     `(("boost" ,boost)
       ("libpng" ,libpng)
       ("zlib" ,zlib)))
    (synopsis "GRF development tools")
    (description
     "The @dfn{Graphics Resource File} (GRF) development tools are a set of
tools for developing (New)GRFs. It includes a number of smaller programs, each
with a specific task:
@enumerate
@item @code{grfcodec} decodes and encodes GRF files for OpenTTD.
@item @code{grfid} extracts the so-called \"GRF ID\" from a GRF.
@item @code{grfstrip} strips all sprites from a GRF.
@item @code{nforenum} checks NFO code for errors, making corrections when
necessary.
@end enumerate")
    (home-page "https://dev.openttdcoop.org/projects/grfcodec")
    ;; GRFCodec, GRFID, and GRFStrip are exclusively under the GPL2.
    ;; NFORenum is under the GPL2+.
    ;; The MD5 implementation contained in GRFID is under the zlib license.
    (license (list license:gpl2 license:gpl2+ license:zlib))))

(define-public catcodec
  (package
    (name "catcodec")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://binaries.openttd.org/extra/catcodec/"
                           version "/catcodec-" version "-source.tar.xz"))
       (sha256
        (base32 "1qg0c2i4p29sxj0q6qp2jynlrzm5pphz2xhcjqlxa69ycrnlxzs7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:make-flags (list (string-append "prefix=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (home-page "https://dev.openttdcoop.org/projects/catcodec")
    (synopsis "Encode/decode OpenTTD sounds")
    (description "catcodec encodes and decodes sounds for OpenTTD.  These
sounds are not much more than some metadata (description and filename) and raw
PCM data.")
    (license license:gpl2)))

(define-public gzochi
  (package
    (name "gzochi")
    (version "0.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/gzochi/gzochi-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1vcvf04qqzs3q8kaild2x7qvkwc6bwzfsisb78147b8z747j7hj0"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'build 'no-Werror
                    (lambda _
                      ;; Don't abort builds due to things like GLib
                      ;; deprecation warnings.
                      (substitute* (find-files "." "^Makefile\\.in$")
                        (("-Werror") ""))
                      #t)))))
    (native-inputs `(("pkgconfig" ,pkg-config)))
    (inputs `(("bdb" ,bdb)
              ("glib" ,glib)
              ("guile" ,guile-3.0)
              ("libmicrohttpd" ,libmicrohttpd)
              ("ncurses" ,ncurses)
              ("sdl" ,sdl)
              ("zlib" ,zlib)))
    (home-page "https://www.nongnu.org/gzochi/")
    (synopsis "Scalable middleware for multiplayer games")
    (description
     "gzochi is a framework for developing massively multiplayer online games.
A server container provides services to deployed games, which are written in
Guile Scheme, that abstract and simplify some of the most challenging and
error-prone aspects of online game development: Concurrency, data persistence,
and network communications.  A very thin client library can be embedded to
provide connectivity for client applications written in any language.")
    (license license:gpl3+)))

(define-public nml
  (package
    (name "nml")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nml" version))
       (sha256
        (base32 "0l5pfs8q7jrl3xscqq7pnwh5h5d17fsyjy7xspkc73sa0ayjm9jx"))))
    (build-system python-build-system)
    ;; TODO: Fix test that fails with
    ;; "AttributeError: partially initialized module 'nml.nmlop' has no
    ;; attribute 'ADD' (most likely due to a circular import)"
    (arguments
     '(#:tests? #f))
    (propagated-inputs
     `(("python-pillow" ,python-pillow)
       ("python-ply" ,python-ply)))
    (home-page "https://github.com/OpenTTD/nml")
    (synopsis "NML compiler")
    (description
     "@dfn{NewGRF Meta Language} (NML) is a python-based compiler, capable of
compiling NML files (along with their associated language, sound and graphic
files) into @file{.grf} and/or @file{.nfo} files.")
    (license license:gpl2+)))

(define-public python-sge
  (package
    (name "python-sge")
    (version "1.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sge" version))
       (sha256
        (base32
         "02fn6v6bxk3sngwd4kd3mglrp0jlnhx7x6h8nnkik6wdv150a0wv"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pygame" ,python-pygame)
       ("python-uniseg" ,python-uniseg)))
    (home-page "https://python-sge.github.io/")
    (synopsis "2D game engine for Python")
    (description
     "The SGE Game Engine (\"SGE\", pronounced like \"Sage\") is a
general-purpose 2D game engine.  It takes care of several details for you so
you can focus on the game itself.  This makes more rapid game development
possible, and it also makes the SGE easy to learn.")
    (license license:lgpl3+)))

(define-public python-sge-pygame
  (deprecated-package "python-sge-pygame" python-sge))

(define-public python-tmx
  (package
    (name "python-tmx")
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/python-tmx/"
                           (version-major+minor version) "/tmx-"
                           version ".tar.gz"))
       (sha256
        (base32
         "073q0prg1nzlkga2b45vhscz374206qh4x68ccg00mxxwagn64z0"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page "http://python-tmx.nongnu.org")
    (synopsis "Python library for the @code{Tiled} TMX format")
    (description
     "Python TMX reads and writes the @code{Tiled} TMX format in a simple way.
This is useful for map editors or generic level editors, and it's also useful
for using a map editor or generic level editor like Tiled to edit your game's
levels.")
    (license (list license:asl2.0
                   ;; Documentation (only available in the source tarball) is
                   ;; under the CC0 license.
                   license:cc0))))

(define-public python2-tmx
  (let ((python2-tmx (package-with-python2 python-tmx)))
    (package
      (inherit python2-tmx)
      (propagated-inputs
       `(("python2-pathlib" ,python2-pathlib)
         ,@(package-propagated-inputs python2-tmx))))))

(define-public python-xsge
  (package
    (name "python-xsge")
    (version "2020.09.07")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/python-sge/xsge"
                                  "/releases/download/v" version
                                  "/xsge-" version ".zip"))
              (sha256
               (base32
                "136xgy3f9vw636wxpqbha022avk0wyxw63mm3a2dvwhh90s716f9"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; xSGE's setup.py script does not support one of the Python build
         ;; system's default flags, "--single-version-externally-managed".
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "python" "setup.py" "install"
                     (string-append "--prefix=" (assoc-ref outputs "out"))
                     "--root=/"))))
       #:tests? #f)) ; no check target
    (native-inputs
     `(("unzip" ,unzip)))
    (propagated-inputs
     `(("python-sge" ,python-sge)))
    (home-page "https://python-sge.github.io/")
    (synopsis "Extensions for the SGE Game Engine")
    (description
     "xSGE is a collection of modules that make doing certain tasks with the SGE
Game Engine easier.  In addition to SGE's conveniences, the user has access to a
GUI toolkit, lighting and physics frameworks and @code{Tiled} TMX format
support.")
    (license license:gpl3+)))

(define-public tiled
  (package
    (name "tiled")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bjorn/tiled")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1prajkx1xpp3csa0xpkrn3c2cnzvmwzxgrqb9d3gqszp3sllr2dg"))))
    (build-system gnu-build-system)
    (inputs
     `(("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtsvg" ,qtsvg)
       ("zlib" ,zlib)))
    (native-inputs
     `(("qttools" ,qttools)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "translations/translations.pro"
               (("LRELEASE =.*")
                (string-append "LRELEASE = "
                               (assoc-ref inputs "qttools")
                               "/bin/lrelease\n")))
             (let ((out (assoc-ref outputs "out")))
               (invoke "qmake"
                       (string-append "PREFIX=" out))))))))
    (home-page "https://www.mapeditor.org/")
    (synopsis "Tile map editor")
    (description
     "Tiled is a general purpose tile map editor.  It is meant to be used for
editing maps of any tile-based game, be it an RPG, a platformer or a Breakout
clone.")

    ;; As noted in 'COPYING', part of it is under GPLv2+, while the rest is
    ;; under BSD-2.
    (license license:gpl2+)))

(define-public tsukundere
  (package
    (name "tsukundere")
    (version "0.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/leoprikler/tsukundere")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05ckds2df810441wfavllx9lsw5jsc9h3nb7m31df01nsj56azdw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules (((guix build guile-build-system)
                   #:select (target-guile-effective-version))
                  ,@%gnu-build-system-modules)
       #:imported-modules ((guix build guile-build-system)
                           ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-command
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (version (target-guile-effective-version))
                    (scm (string-append out "/share/guile/site/"
                                        version))
                    (go (string-append out "/lib/guile/"
                                       version "/site-ccache")))

               (substitute* "bin/tsukundere"
                 (("exec guile .*" all)
                  (string-append
                   (format #f "export GUILE_LOAD_PATH=~@?~%"
                           "\"~a:~a\"" scm (getenv "GUILE_LOAD_PATH"))
                   (format #f "export GUILE_LOAD_COMPILED_PATH=~@?~%"
                           "\"~a:~a\"" go (getenv "GUILE_LOAD_COMPILED_PATH"))
                   all)))
               #t))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("guile" ,guile-3.0)
       ("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)))
    (propagated-inputs
     `(("guile-sdl2" ,guile3.0-sdl2)))
    (home-page "https://gitlab.com/leoprikler/tsukundere")
    (synopsis "Visual novel engine")
    (description "Tsukundere is a game engine geared heavily towards the
development of visual novels, written on top of Guile-SDL2.  It is still
experimental.")
    (license license:lgpl3+)))

(define-public sfml
  (package
    (name "sfml")
    (version "2.5.1")
    (source (origin
              (method git-fetch)
              ;; Do not fetch the archives from
              ;; http://mirror0.sfml-dev.org/files/ because files there seem
              ;; to be changed in place.
              (uri (git-reference
                    (url "https://github.com/SFML/SFML")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0abr8ri2ssfy9ylpgjrr43m6rhrjy03wbj9bn509zqymifvq5pay"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Ensure system libraries are used.
                  (delete-file-recursively "extlibs")
                  #t))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DSFML_INSTALL_PKGCONFIG_FILES=TRUE"
             "-DSFML_OS_PKGCONFIG_DIR=lib/pkgconfig")
       #:tests? #f)) ; no tests
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("mesa" ,mesa)
       ("glew" ,glew)
       ("libx11" ,libx11)
       ("xcb-util-image" ,xcb-util-image)
       ("libxrandr" ,libxrandr)
       ("eudev" ,eudev)
       ("libjpeg" ,libjpeg-turbo)
       ("libsndfile" ,libsndfile)
       ("stb-image" ,stb-image)
       ("stb-image-write" ,stb-image-write)))
    (propagated-inputs
     ;; In Requires.private of pkg-config files.
     `(("flac" ,flac)
       ("freetype" ,freetype)
       ("libvorbis" ,libvorbis)
       ("openal" ,openal)))
    (home-page "https://www.sfml-dev.org")
    (synopsis "Simple and Fast Multimedia Library")
    (description
     "SFML provides a simple interface to the various computer components,
to ease the development of games and multimedia applications.  It is composed
of five modules: system, window, graphics, audio and network.")
    (license license:zlib)))

(define-public sfxr
  (package
    (name "sfxr")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.drpetter.se/files/sfxr-sdl-1.2.1.tar.gz"))
              (sha256
               (base32
                "0dfqgid6wzzyyhc0ha94prxax59wx79hqr25r6if6by9cj4vx4ya"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure) ; no configure script
                  (add-before 'build 'patch-makefile
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* "Makefile"
                          (("\\$\\(DESTDIR\\)/usr") out))
                        (substitute* "main.cpp"
                          (("/usr/share")
                           (string-append out "/share")))
                        #t))))
       #:tests? #f)) ; no tests
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("desktop-file-utils" ,desktop-file-utils)))
    (inputs
     `(("sdl" ,sdl)
       ("gtk+" ,gtk+)))
    (synopsis "Simple sound effect generator")
    (description "Sfxr is a tool for quickly generating simple sound effects.
Originally created for use in video game prototypes, it can generate random
sounds from presets such as \"explosion\" or \"powerup\".")
    (home-page "http://www.drpetter.se/project_sfxr.html")
    (license license:expat)))

(define-public surgescript
  (package
    (name "surgescript")
    (version "0.5.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alemart/surgescript")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xwd4g7n0b0rxkpbyshkzyl472h1y606ghyvf8gv034n3jz2g4jk"))))
     (build-system cmake-build-system)
     (arguments
      '(#:configure-flags
        (let ((share (string-append (assoc-ref %outputs "out") "/share")))
          (list "-DWANT_STATIC=NO"
                (string-append "-DICON_PATH=" share "/pixmaps")
                (string-append "-DMETAINFO_PATH=" share "/metainfo")))
        #:tests? #f))
     (home-page "https://docs.opensurge2d.org")
     (synopsis "Scripting language for games")
     (description "@code{SurgeScript} is a dynamically typed object-oriented
scripting language designed for games.  Each object is a state machine that
can be customized by attaching other objects.  The language supports automatic
garbage collection and can be extended with plugins.")
    (license license:asl2.0)))

(define-public physfs
  (package
    (name "physfs")
    (version "3.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://icculus.org/physfs/downloads/physfs-"
                    version ".tar.bz2"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qzqz4r88gvd8m7sh2z5hvqcr0jfr4wb2f77c19xycyn0rigfk9h"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ; no check target
       #:configure-flags '("-DPHYSFS_BUILD_STATIC=OFF")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-CMakeLists.txt
                    (lambda _
                      (substitute* "CMakeLists.txt"
                        ;; XXX: For some reason CMakeLists.txt disables
                        ;; RUNPATH manipulation when the compiler is GCC.
                        (("CMAKE_COMPILER_IS_GNUCC") "FALSE"))
                      #t)))))
    (inputs
     `(("zlib" ,zlib)))
    (native-inputs
     `(("doxygen" ,doxygen)))
    (home-page "https://icculus.org/physfs")
    (synopsis "File system abstraction library")
    (description
     "PhysicsFS is a library to provide abstract access to various archives.
It is intended for use in video games.  For security, no file writing done
through the PhysicsFS API can leave a defined @emph{write directory}.  For
file reading, a @emph{search path} with archives and directories is defined,
and it becomes a single, transparent hierarchical file system.  So archive
files can be accessed in the same way as you access files directly on a disk,
and it makes it easy to ship a new archive that will override a previous
archive on a per-file basis.")
    (license license:zlib)))

(define-public love
  (package
    (name "love")
    (version "11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://bitbucket.org/rude/love/downloads/"
                                 "love-" version "-linux-src.tar.gz"))
             (sha256
              (base32
               "0m8lvlabmcchskx4qpzkdlsm44360f3j0q3vvvj2388cfnvhv7v4"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("devil" ,devil)
       ("freetype" ,freetype)
       ("libmodplug" ,libmodplug)
       ("libtheora" ,libtheora)
       ("libvorbis" ,libvorbis)
       ("luajit" ,luajit)
       ("mesa" ,mesa)
       ("mpg123" ,mpg123)
       ("openal" ,openal)
       ("sdl2" ,sdl2)
       ("zlib" ,zlib)))
    (synopsis "2D game framework for Lua")
    (description "LÖVE is a framework for making 2D games in the Lua
programming language.")
    (home-page "https://love2d.org/")
    (license license:zlib)))

(define-public love-nuklear
  (let ((version "v2.6")
        (commit "fef4e00a602efb16c57ae962850b6e7a01f7a29a"))
    (package
      (name "love-nuklear")
      (version (git-version version "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/keharriso/love-nuklear/")
                      (commit commit)
                      (recursive? #t)))
                ;; NOTE: the HEAD of the Nuklear git-submodule is at commit
                ;; "adc52d710fe3c87194b99f540c53e82eb75c2521" of Oct 1 2019
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "15qmy8mfwkxy2x9rmxs6f9cyvjvwwj6yf78bs863xmc56dmjzzbn"))))
      (build-system cmake-build-system)
      (arguments
       `(#:build-type "Release"
         #:tests? #f))
      (inputs
       `(("luajit" ,luajit)))
      (synopsis "Lightweight immediate mode GUI for LÖVE games")
      (description "LÖVE is a Lua framework for making 2D games.  Nuklear
is a minimal state immediate mode graphical user interface toolkit.  This
package is the Nuklear bindings for LÖVE created by Kevin Harrison.")
      (home-page "https://github.com/keharriso/love-nuklear/")
      (license license:expat))))

(define-public allegro-4
  (package
    (name "allegro")
    (version "4.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/liballeg/allegro5/"
                                  "releases/download/" version "/allegro-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1d5ws3ihvpa6f4qc6a6drq31pajw6bblxifr4kcxzqj9br1nw28y"))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-build-system
           (lambda _
             ;; Build addons as shared libraries.  Trying to set ADDON_LINKAGE
             ;; via a command line option doesn't work because it is
             ;; unconditionally clobbered in the build script.
             (substitute* '("CMakeLists.txt")
               (("ADDON_LINKAGE STATIC")
                "ADDON_LINKAGE SHARED"))
             #t)))))
    (inputs
     `(("glu" ,glu)
       ("libpng" ,libpng)
       ("libvorbis" ,libvorbis)
       ("mesa" ,mesa)
       ("zlib" ,zlib)))
    (synopsis "Game programming library")
    (description "Allegro is a library mainly aimed at video game and
multimedia programming.  It handles common, low-level tasks such as creating
windows, accepting user input, loading data, drawing images, playing sounds,
etc.")
    (home-page "https://liballeg.org")
    (license license:giftware)))

(define-public allegro
  (package
    (name "allegro")
    (version "5.2.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/liballeg/allegro5/releases"
                                  "/download/" version "/allegro-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "06dpkfnac8w3pq36834nn2iij3ajz6prladqd0w92lq39aiqv5jr"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f))          ; there are no tests
    (inputs
     ;; FIXME: Add the following optional inputs: xinput2, opensl, dumb
     `(("flac" ,flac)
       ("freetype" ,freetype)
       ("glu" ,glu)
       ("gtk" ,gtk+-2)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libtheora" ,libtheora)
       ("libvorbis" ,libvorbis)
       ("libxcursor" ,libxcursor)
       ("libxinerama" ,libxinerama)
       ("libxrandr" ,libxrandr)
       ("mesa" ,mesa)
       ("openal" ,openal)
       ("physfs" ,physfs)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Game programming library")
    (description "Allegro is a library mainly aimed at video game and
multimedia programming.  It handles common, low-level tasks such as creating
windows, accepting user input, loading data, drawing images, playing sounds,
etc.")
    (home-page "https://liballeg.org")
    (license license:bsd-3)))

(define-public allegro-5.0
  (package (inherit allegro)
    (name "allegro")
    (version "5.0.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/liballeg/allegro5/releases"
                                  "/download/" version "/allegro-"
                                  (if (equal? "0" (string-take-right version 1))
                                    (string-drop-right version 2)
                                    version)
                                  ".tar.gz"))
              (patches (search-patches
                        "allegro-mesa-18.2.5-and-later.patch"))
              (sha256
               (base32
                "0cd51qrh97jrr0xdmnivqgwljpmizg8pixsgvc4blqqlaz4i9zj9"))))))

(define-public aseprite
  (package
    (name "aseprite")
    (version "1.1.7") ; After 1.1.7 the source is no longer distributed under the GPL.
    ;; TODO: Unbundle third party software.
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://github.com/aseprite/aseprite"
                                  "/releases/download/v" version
                                  "/Aseprite-v" version "-Source.zip"))
              (sha256
               (base32
                "1plss4i1lfxcznv9p0pip1bkhj7ipw7jlhsh5avd6dzw079l4nvv"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       ;; Use shared libraries instead of building bundled source.
       (list "-DWITH_WEBP_SUPPORT=1"
             "-DUSE_SHARED_CURL=1"
             "-DUSE_SHARED_GIFLIB=1"
             "-DUSE_SHARED_JPEGLIB=1"
             "-DUSE_SHARED_ZLIB=1"
             "-DUSE_SHARED_LIBPNG=1"
             "-DUSE_SHARED_LIBLOADPNG=1"
             "-DUSE_SHARED_LIBWEBP=1"
             "-DUSE_SHARED_TINYXML=1"
             "-DUSE_SHARED_PIXMAN=1"
             "-DUSE_SHARED_FREETYPE=1"
             "-DUSE_SHARED_ALLEGRO4=1"
             "-DENABLE_UPDATER=0" ; no auto-updates
             (string-append "-DFREETYPE_INCLUDE_DIR="
                            (assoc-ref %build-inputs "freetype")
                            "/include/freetype2"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    ;; TODO: Use a patched Allegro 4 that supports window resizing.  This
    ;; patched version is bundled with Aseprite, but the patches should be
    ;; extracted and applied on top of a standalone Allegro 4 package.
    (inputs
     `(("allegro" ,allegro-4)
       ("curl" ,curl)
       ("freetype" ,freetype)
       ("giflib" ,giflib)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libwebp" ,libwebp)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxxf86vm" ,libxxf86vm)
       ("pixman" ,pixman)
       ("tinyxml" ,tinyxml)
       ("zlib" ,zlib)))
    (synopsis "Animated sprite editor and pixel art tool")
    (description "Aseprite is a tool for creating 2D pixel art for video
games.  In addition to basic pixel editing features, Aseprite can assist in
the creation of animations, tiled graphics, texture atlases, and more.")
    (home-page "https://www.aseprite.org/")
    (license license:gpl2+)))

(define-public qqwing
  (package
    (name "qqwing")
    (version "1.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://qqwing.com/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "0bw0papyqjg22z6irf36gs54y8236wa37b6gyn2h1spy65n76lqp"))))
    (build-system gnu-build-system)
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (home-page "https://qqwing.com/")
    (synopsis "Sudoku puzzle solver and generator")
    (description
     "QQWing is a Sudoku puzzle generator and solver.
It offers the following features:
@enumerate
@item Can solve 1000 puzzles in 1 second and generate 1000 puzzles in 25 seconds.
@item Uses logic.  Uses as many solve techniques as possible when solving
  puzzles rather than guessing.
@item Rates puzzles.  Most generators don't give an indication of the difficulty
  of a Sudoku puzzle.  QQwing does.
@item Can print solve instructions for any puzzle.
@item Customizable output style, including a CSV style that is easy to
  import into a database.
@end enumerate")
    (license license:gpl2+)))

(define-public quesoglc
  (package
    (name "quesoglc")
    (version "0.7.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/" name "/" version "/"
                                  name "-" version "-free.tar.bz2"))
              (sha256
               (base32
                "08ddhywdy2qg17m592ng3yr0p1ih96irg8wg729g75hsxxq9ipks"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("fontconfig" ,fontconfig)
              ("freeglute" ,freeglut)
              ("fribidi" ,fribidi)
              ("glew" ,glew)))
    (home-page "http://quesoglc.sourceforge.net")
    (synopsis "Implementation of the OpenGL Character Renderer (GLC)")
    (description
     "The OpenGL Character Renderer (GLC) is a state machine that provides
OpenGL programs with character rendering services via an application programming
interface (API).")
    (license (list license:expat license:lgpl2.1+))))

(define-public python-pygame
  (package
    (name "python-pygame")
    (version "1.9.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pygame" version))
              (sha256
               (base32
                "1dn0nb86jl7yr8709cncxdr0yrmviqakw7zx3g8jbbwrr60if3bh"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f                ; tests require pygame to be installed first
       #:phases
       (modify-phases %standard-phases
         ;; Set the paths to the dependencies manually because
         ;; the configure script does not allow passing them as
         ;; parameters.  This also means we can skip the configure
         ;; phase.
         (add-before 'build 'set-library-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((sdl-ref   (assoc-ref inputs "sdl"))
                   (font-ref  (assoc-ref inputs "sdl-ttf"))
                   (image-ref (assoc-ref inputs "sdl-image"))
                   (mixer-ref (assoc-ref inputs "sdl-mixer"))
                   (smpeg-ref (assoc-ref inputs "libsmpeg"))
                   (png-ref   (assoc-ref inputs "libpng"))
                   (jpeg-ref  (assoc-ref inputs "libjpeg"))
                   (freetype-ref (assoc-ref inputs "freetype"))
                   (v4l-ref   (assoc-ref inputs "v4l-utils"))
                   (out-ref   (assoc-ref outputs "out")))
               (substitute* "Setup.in"
                 (("SDL = -I/usr/include/SDL")
                  (string-append "SDL = -I" sdl-ref "/include/SDL -I.")))
               (substitute* "Setup.in"
                 (("FONT = -lSDL_ttf")
                  (string-append "FONT = -I" font-ref "/include/SDL -L"
                                 font-ref "/lib -lSDL_ttf")))
               (substitute* "Setup.in"
                 (("IMAGE = -lSDL_image")
                  (string-append "IMAGE = -I" image-ref "/include/SDL -L"
                                 image-ref "/lib -lSDL_image")))
               (substitute* "Setup.in"
                 (("MIXER = -lSDL_mixer")
                  (string-append "MIXER = -I" mixer-ref "/include/SDL -L"
                                 mixer-ref "/lib -lSDL_mixer")))
               (substitute* "Setup.in"
                 (("SMPEG = -lsmpeg")
                  (string-append "SMPEG = -I" smpeg-ref "/include/smpeg -L"
                                 smpeg-ref "/lib -lsmpeg")))
               (substitute* "Setup.in"
                 (("PNG = -lpng")
                  (string-append "PNG = -I" png-ref "/include -L"
                                 png-ref "/lib -lpng")))
               (substitute* "Setup.in"
                 (("JPEG = -ljpeg")
                  (string-append "JPEG = -I" jpeg-ref "/include -L"
                                 jpeg-ref "/lib -ljpeg")))

               (substitute* "Setup.in"
                 (("FREETYPE = -lfreetype")
                  (string-append "FREETYPE = -I" freetype-ref "/include/freetype2 -L"
                                 freetype-ref "/lib -lfreetype")))

               (substitute* "Setup.in"
                 (("^pypm") "#pypm"))
               ;; Create a path to a header file provided by v4l-utils.
               (system* "mkdir" "linux")
               (system* "ln" "--symbolic"
                        (string-append v4l-ref "/include/libv4l1-videodev.h")
                        "linux/videodev.h")
               (system* "ln" "--symbolic" "Setup.in" "Setup")))))))
    (inputs
     `(("freetype" ,freetype)
       ("sdl" ,sdl)
       ("sdl-image" ,sdl-image)
       ("sdl-mixer" ,sdl-mixer)
       ("sdl-ttf" ,sdl-ttf)
       ("sdl-gfx" ,sdl-gfx)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libX11" ,libx11)
       ("libsmpeg" ,libsmpeg)
       ("portmidi" ,portmidi)
       ("v4l-utils" ,v4l-utils)))
    (home-page "https://www.pygame.org")
    (synopsis "SDL wrapper for Python")
    (description "Pygame is a set of Python modules designed for writing games.
Pygame adds functionality on top of the excellent SDL library. This allows you
to create fully featured games and multimedia programs in the python language.")
    (license (list license:bsd-2
                   ;; python numeric license as listed by Debian looks like
                   ;; an Expat-style license with a warranty disclaimer for
                   ;; the U.S. government and the University of California.
                   license:expat
                   license:lgpl2.0+
                   license:lgpl2.1+
                   license:gpl3+
                   license:psfl
                   license:public-domain
                   license:lgpl2.1+))))

(define-public python2-pygame
  (package-with-python2 python-pygame))

(define-public python2-pygame-sdl2
  (let ((real-version "2.1.0")
        (renpy-version "7.4.2"))
    (package
      (inherit python2-pygame)
      (name "python2-pygame-sdl2")
      (version (string-append real-version "-for-renpy-" renpy-version))
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://www.renpy.org/dl/" renpy-version
                             "/pygame_sdl2-" version ".tar.gz"))
         (sha256 (base32 "1lpk69nh379x5pdlr838x5b49spzksn9hyqiq2g0q28k0xk4lm67"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; drop generated sources
             (delete-file-recursively "gen")
             (delete-file-recursively "gen-static")
             #t))))
      (build-system python-build-system)
      (arguments
       `(#:tests? #f                ; tests require pygame to be installed first
         #:python ,python-2
         #:phases
         (modify-phases %standard-phases
           (add-after 'set-paths 'set-sdl-vars
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "PYGAME_SDL2_CFLAGS"
                       (string-append "-I"
                                      (assoc-ref inputs "sdl-union")
                                      "/include/SDL2 -D_REENTRANT"))
               (setenv "PYGAME_SDL2_LDFLAGS"
                       (string-append "-L"
                                      (assoc-ref inputs "sdl-union")
                                      "/lib -Wl,-rpath,"
                                      (assoc-ref inputs "sdl-union")
                                      "/lib -Wl,--enable-new-dtags -lSDL2"))
               #t)))))
      (inputs
       `(("sdl-union"
          ,(sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf)))))
      (native-inputs
       `(("python2-cython" ,python2-cython)))
      (home-page "https://www.renpy.org/")
      (synopsis "Reimplementation of the Pygame API using SDL2")
      (description "Pygame_SDL2 reimplements the Pygame API using SDL2,
staying close to the original, but also adding some SDL2-specific features.
While it aims to be used as a drop-in replacement, it appears to be
developed mainly for Ren'py.")
      (license (list license:lgpl2.1 license:zlib)))))

(define-public python2-renpy
  (package
    (name "python2-renpy")
    (version "7.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.renpy.org/dl/" version
                           "/renpy-" version "-source.tar.bz2"))
       (sha256 (base32 "1mlrq9q3r36izyskq674qhp8s32iirvvfb4r8z6hi26189aaydsw"))
       (modules '((guix build utils)))
       (patches
        (search-patches
         "renpy-use-system-fribidi.patch"))
       (snippet
        '(with-directory-excursion "module"
           ;; drop generated sources
           (delete-file-recursively "gen")
           ;; drop fribidi sources
           (delete-file-recursively "fribidi-src")
           #t))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; Ren'py doesn't seem to package tests
       #:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-commands
           (lambda _
             (substitute* "renpy/editor.py"
               (("xdg-open")
                (which "xdg-open")))
             #t))
         (add-after 'unpack 'fix-include-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "module/setup.py"
               (("/usr/include/fribidi")
                (string-append (assoc-ref inputs "fribidi")
                               "/include/fribidi")))
             #t))
         (add-after 'set-paths 'set-build-vars
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "RENPY_CYTHON"
                     (string-append (assoc-ref inputs "python2-cython")
                                    "/bin/cython"))
             (setenv "RENPY_DEPS_INSTALL" (string-join (map cdr inputs) ":"))
             #t))
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
             ;; The "module" subdirectory contains a python (really cython)
             ;; project, which is built using a script, that is thankfully
             ;; named "setup.py".
             (with-directory-excursion "module"
               (apply (assoc-ref %standard-phases 'build) args))
             ;; the above causes renpy.__init__ to be compiled but does not
             ;; compile anything else, hence we do that here
             (delete-file "renpy/__init__.pyc")
             (invoke "python" "-m" "compileall" "renpy")
             #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
             ;; Again, we have to wrap the module installation.
             ;; Additionally, we want to install the python code
             ;; (both source and compiled) in the same directory.
             (let* ((out (assoc-ref outputs "out"))
                    (site (string-append "/lib/python"
                                         (python-version
                                          (assoc-ref inputs "python"))
                                         "/site-packages")))
               (with-directory-excursion "module"
                 (apply (assoc-ref %standard-phases 'install) args))
               (copy-recursively "renpy"
                                 (string-append out site "/renpy")))
             #t)))))
    (inputs
     `(("ffmpeg" ,ffmpeg)
       ("freetype" ,freetype)
       ("fribidi" ,fribidi)
       ("glew" ,glew)
       ("libpng" ,libpng)
       ("sdl-union"
        ,(sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf)))))
    (propagated-inputs
     `(("python2-future" ,python2-future)
       ("python2-pygame" ,python2-pygame-sdl2)))
    (native-inputs
     `(("gcc" ,gcc-8) ; for const variables as initializer elements
       ("python2-cython" ,python2-cython)
       ("xdg-utils" ,xdg-utils)))
    (home-page "https://www.renpy.org/")
    (synopsis "Ren'py python module")
    (description "This package contains the shared libraries and Python
modules of Ren'py.")
    (license license:expat)))

(define-public renpy
  (package
    (inherit python2-renpy)
    (name "renpy")
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; see python2-renpy
       #:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-commands
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "launcher/game/choose_directory.rpy"
               (("/usr/bin/python") (which "python2")))
             (substitute* "launcher/game/front_page.rpy"
               (("xdg-open")
                (which "xdg-open")))
             (substitute* "launcher/game/project.rpy"
               (("cmd = \\[ executable, \"-EO\", sys.argv\\[0\\] \\]")
                (string-append "cmd = [ \"" (assoc-ref outputs "out")
                               "/bin/renpy\" ]"))
               ;; Projects are still created in the usual style, so we need
               ;; to adjust the path.
               (("cmd.append\\(self.path\\)")
                "cmd.append(self.path + \"/game\")"))
             #t))
         (add-after 'unpack 'drop-game-from-paths
           (lambda _
             (substitute* (list "launcher/game/gui7.rpy"
                                "launcher/game/gui7/images.py")
               ((", \"game\",") ","))
             #t))
         (add-before 'build 'start-xserver
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xorg-server (assoc-ref inputs "xorg-server")))
               (setenv "HOME" (getcwd))
               (system (format #f "~a/bin/Xvfb :1 &" xorg-server))
               (setenv "DISPLAY" ":1")
               #t)))
         (replace 'build
           (lambda _
             (invoke "python" "renpy.py" "launcher" "quit")
             (invoke "python" "renpy.py" "the_question" "quit")
             (invoke "python" "renpy.py" "tutorial" "quit")
             #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Here we install our custom renpy program.
             ;; After finishing this step, "out" will have the following:
             ;; |-- bin/renpy
             ;; `-- share/renpy ; i.e. path_to_renpy_base()
             ;;     `-- common
             ;;
             ;; Note that common is also a de facto unused directory in
             ;; python2-renpy. On other systems, renpy_base would point to
             ;; site-packages or even somewhere in /opt.
             ;; The former approach is not as straightforward as it seems
             ;; -- it causes renpy to load files twice for some weird reason --
             ;; and the latter is impossible on Guix. Hence the detour through
             ;; share/renpy and the custom renpy program.
             ;;
             ;; As a convention, other games should be installed as
             ;; subdirectories of share/renpy in their respective outputs as
             ;; well. This differs from the traditional layout, which is
             ;; roughly the following:
             ;; `-- Super Awesome Game
             ;;     |-- game      ; <- the folder we actually want
             ;;     |-- lib       ; compiled renpy module and dependencies
             ;;     |-- renpy     ; Ren'py python code (source + compiled)
             ;;     |-- Super Awesome Game.py
             ;;     `-- Super Awesome Game.sh
             (let* ((out (assoc-ref outputs "out"))
                    (bin/renpy (string-append out "/bin/renpy")))
               (mkdir-p (string-append out "/bin"))
               (copy-recursively "renpy/common"
                                 (string-append out "/share/renpy/common"))
               (copy-recursively "gui"
                                 (string-append out "/share/renpy/gui"))

               (call-with-output-file bin/renpy
                 (lambda (port)
                   (format port "#!~a~%" (which "python2"))
                   (format port "
from __future__ import print_function

import os
import sys
import warnings

def path_to_common(renpy_base):
    return renpy_base + \"/common\"

def path_to_saves(gamedir, save_directory=None):
    import renpy  # @UnresolvedImport

    if save_directory is None:
        save_directory = renpy.config.save_directory
        save_directory = renpy.exports.fsencode(save_directory)

    if not save_directory:
        return gamedir + \"/saves\"

    return os.path.expanduser(\"~~/.renpy/\" + save_directory)

def path_to_renpy_base():
    return \"~a\"

def main():
    renpy_base = path_to_renpy_base()
    try:
        import renpy.bootstrap
        import renpy.arguments
    except ImportError:
        print(\"\"\"Could not import renpy.bootstrap.
Please ensure you decompressed Ren'Py correctly, preserving the directory
structure.\"\"\", file=sys.stderr)
        raise

    args = renpy.arguments.bootstrap()
    if not args.basedir:
        print(\"\"\"This Ren'py requires a basedir to launch.
The basedir is the directory, in which .rpy files live -- usually the 'game'
subdirectory of a game packaged by Ren'py.

If you want the Ren'py launcher, use renpy-launcher instead.\"\"\",
              file=sys.stderr)
        sys.exit()

    renpy.bootstrap.bootstrap(renpy_base)

if __name__ == \"__main__\":
    main()
"
                           (string-append out "/share/renpy"))))
               (chmod bin/renpy #o755)
               #t)))

         (add-after 'install 'install-games
           (lambda* (#:key outputs #:allow-other-keys)
             (define renpy (assoc-ref outputs "out"))
             ;; TODO: We should offer a renpy-build-system to make the
             ;; installation of Ren'py games easier.
             (define* (install-renpy-game #:key output game name (renpy renpy)
                                          #:allow-other-keys)
               (let* ((name (or name (basename game)))
                      (launcher (string-append output "/bin/renpy-" name))
                      (share (string-append output "/share/renpy/" name)))
                 (copy-recursively (string-append game "/game") share)
                 (mkdir-p (string-append output "/bin"))
                 (with-output-to-file launcher
                   (lambda ()
                     (format #t
                             "#!~a~%~a ~a \"$@\""
                             (which "bash")
                             (string-append renpy "/bin/renpy")
                             share)))
                 (chmod launcher #o755)))

             (install-renpy-game #:output (assoc-ref outputs "out")
                                 #:game "launcher")

             (install-renpy-game #:output (assoc-ref outputs "the-question")
                                 #:game "the_question"
                                 #:name "the-question")

             (install-renpy-game #:output (assoc-ref outputs "tutorial")
                                 #:game "tutorial")
             #t))
         (replace 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/renpy")
               `("PYTHONPATH" = (,(getenv "PYTHONPATH"))))
             #t)))))
    (inputs
     `(("python2-tkinter" ,python-2 "tk")
       ("python2-pygame" ,python2-pygame-sdl2)
       ("python2-renpy" ,python2-renpy)
       ("xorg-server" ,xorg-server)))
    (outputs
     (list "out" "tutorial" "the-question"))
    (home-page "https://www.renpy.org/")
    (synopsis "Visual Novel Engine")
    (description "Ren'Py is a visual novel engine that helps you use words,
images, and sounds to tell interactive stories that run on computers and
mobile devices.  These can be both visual novels and life simulation games.
The easy to learn script language allows anyone to efficiently write large
visual novels, while its Python scripting is enough for complex simulation
games.")
    (license license:expat)))

(define-public grafx2
  (package
    (name "grafx2")
    (version "2.4")
    (source (origin
              (method url-fetch)
              ;; XXX: There is no URL that contains the version. :(
              (uri "http://pulkomandy.tk/projects/GrafX2/downloads/21")
              (sha256
               (base32
                "0svsy6rqmdj11b400c242i2ixihyz0hds0dgicqz6g6dcgmcl62q"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure script
         (add-before 'build 'change-to-src-directory
           (lambda _
             (chdir "src")
             #t)))
       #:make-flags
       ;; SDL header files are referenced without the preceeding "SDL/".
       (list (string-append "CFLAGS=-I"
                            (assoc-ref %build-inputs "sdl-union")
                            "/include/SDL")
             (string-append "prefix="
                            (assoc-ref %outputs "out")))
       #:tests? #f)) ; no check target
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libpng" ,libpng)
       ("lua" ,lua-5.1)
       ("sdl-union" ,(sdl-union (list sdl sdl-image sdl-ttf)))))
    (synopsis "Bitmap paint program")
    (description "GrafX2 is a bitmap paint program inspired by the Amiga
programs Deluxe Paint and Brilliance.  Specializing in 256-color drawing, it
includes a very large number of tools and effects that make it particularly
suitable for pixel art, game graphics, and generally any detailed graphics
painted with a mouse.")
    (home-page "http://pulkomandy.tk/projects/GrafX2")
    (license license:gpl2))) ; GPLv2 only

(define-public ois
  (package
    (name "ois")
    (version "1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wgois/OIS")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g8krgq5bdx2rw7ig0xva4kqv4x815672i7z6lljp3n8847wmypa"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; no test suite
    (inputs
     `(("libx11" ,libx11)))
    (synopsis "Object Oriented Input System")
    (description
     "Cross Platform Object Oriented Input Lib System is a cross platform,
simple solution for using all kinds of Input Devices (Keyboards, Mice,
Joysticks, etc) and feedback devices (e.g. force feedback).  Meant to be very
robust and compatible with many systems and operating systems.")
    (home-page "https://github.com/wgois/OIS")
    (license license:zlib)))

(define-public mygui
  (package
    (name "mygui")
    (version "3.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/MyGUI/mygui")
             (commit (string-append "MyGUI" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1wk7jmwm55rhlqqcyvqsxdmwvl70bysl9azh4kd9n57qlmgk3zmw"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ; No test target
       #:configure-flags
       (list "-DMYGUI_INSTALL_DOCS=TRUE"
             (string-append "-DOGRE_INCLUDE_DIR="
                            (assoc-ref %build-inputs "ogre")
                            "/include/OGRE")
             ;; Demos and tools are Windows-specific:
             ;; https://github.com/MyGUI/mygui/issues/24.
             "-DMYGUI_BUILD_DEMOS=FALSE"
             "-DMYGUI_BUILD_TOOLS=FALSE")))
    (native-inputs
     `(("boost" ,boost)
       ("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("font-dejavu" ,font-dejavu)
       ("freetype" ,freetype)
       ("graphviz" ,graphviz)
       ("libx11" ,libx11)
       ("ogre" ,ogre)
       ("ois" ,ois)))
    (synopsis "Fast, flexible and simple GUI")
    (description
     "MyGUI is a library for creating Graphical User Interfaces (GUIs) for games
and 3D applications.  The main goals of mygui are: speed, flexibility and ease
of use.")
    (home-page "http://mygui.info/")
    (license license:expat)))

(define-public mygui-gl
  ;; Closure size is reduced by some 800 MiB.
  (package
    (inherit mygui)
    (name "mygui-gl")
    (version "3.2.2")
    (arguments
     (substitute-keyword-arguments (package-arguments mygui)
       ((#:configure-flags _)
        `(cons* "-DMYGUI_RENDERSYSTEM=4" ; 3 is Ogre, 4 is OpenGL.
                ;; We can't reuse the flags because of the mention to Ogre.
                (list "-DMYGUI_INSTALL_DOCS=TRUE"
                      ;; Demos and tools are Windows-specific:
                      ;; https://github.com/MyGUI/mygui/issues/24.
                      "-DMYGUI_BUILD_DEMOS=FALSE"
                      "-DMYGUI_BUILD_TOOLS=FALSE")))))
    (inputs
     `(("mesa" ,mesa)
       ("glu" ,glu)
       ,@(fold alist-delete (package-inputs mygui)
               '("ogre"))))
    (synopsis "Fast, flexible and simple GUI (OpenGL backend)")))

(define-public openmw
  (package
    (name "openmw")
    (version "0.46.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/OpenMW/openmw")
              (commit (string-append "openmw-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0rm32zsmxvr6b0jjihfj543skhicbw5kg6shjx312clhlm035w2x"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No test target
       #:configure-flags
       (list "-DDESIRED_QT_VERSION=5")))
    (native-inputs
     `(("boost" ,boost)
       ("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("bullet" ,bullet)
       ("ffmpeg" ,ffmpeg)
       ("libxt" ,libxt)
       ("mygui" ,mygui-gl)              ; OpenMW does not need Ogre.
       ("openal" ,openal)
       ("openscenegraph" ,openmw-openscenegraph)
       ("qtbase" ,qtbase)
       ("sdl" ,sdl2)
       ("unshield" ,unshield)))
    (synopsis "Re-implementation of the RPG Morrowind engine")
    (description
     "OpenMW is a game engine which reimplements and extends the one that runs
the 2002 open-world RPG Morrowind.  The engine comes with its own editor,
called OpenMW-CS which allows the user to edit or create their own original
games.")
    (home-page "https://openmw.org")
    (license license:gpl3)))

(define-public godot
  (package
    (name "godot")
    (version "3.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/godotengine/godot")
                    (commit (string-append version "-stable"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19vrp5lhyvxbm6wjxzn28sn3i0s8j08ca7nani8l1nrhvlc8wi0v"))
              (modules '((guix build utils)
                         (ice-9 ftw)
                         (srfi srfi-1)))
              (snippet
               '(begin
                  ;; Keep only those bundled files we have not (yet) replaced
                  ;; with Guix versions. Note that some of these may be
                  ;; modified; see "thirdparty/README.md".
                  (with-directory-excursion "thirdparty"
                    (let* ((preserved-files
                            '("README.md"
                              "assimp"
                              "certs"
                              "cvtt"
                              "enet"
                              "etc2comp"
                              "fonts"
                              "glad"
                              "jpeg-compressor"
                              "libsimplewebm"
                              "miniupnpc"
                              "minizip"
                              "misc"
                              "nanosvg"
                              "pvrtccompressor"
                              "recastnavigation"
                              "squish"
                              "tinyexr"
                              "vhacd"
                              "xatlas")))
                      (for-each delete-file-recursively
                                (lset-difference string=?
                                                 (scandir ".")
                                                 (cons* "." ".." preserved-files)))))
                  #t))))
    (build-system scons-build-system)
    (arguments
     `(#:scons ,scons-python2
       #:scons-flags (list "platform=x11" "target=release_debug"
                           ;; Avoid using many of the bundled libs.
                           ;; Note: These options can be found in the SConstruct file.
                           "builtin_bullet=no"
                           "builtin_freetype=no"
                           "builtin_glew=no"
                           "builtin_libmpdec=no"
                           "builtin_libogg=no"
                           "builtin_libpng=no"
                           "builtin_libtheora=no"
                           "builtin_libvorbis=no"
                           "builtin_libvpx=no"
                           "builtin_libwebp=no"
                           "builtin_mbedtls=no"
                           "builtin_opus=no"
                           "builtin_pcre2=no"
                           "builtin_wslay=no"
                           "builtin_zlib=no"
                           "builtin_zstd=no")
       #:tests? #f                      ; There are no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'scons-use-env
           (lambda _
             ;; Scons does not use the environment variables by default,
             ;; but this substitution makes it do so.
             (substitute* "SConstruct"
               (("env_base = Environment\\(tools=custom_tools\\)")
                (string-append
                 "env_base = Environment(tools=custom_tools)\n"
                 "env_base = Environment(ENV=os.environ)")))
             #t))
         ;; Build headless tools, used for packaging games without depending on X.
         (add-after 'build 'build-headless
           (lambda* (#:key scons-flags #:allow-other-keys)
             (apply invoke "scons"
                    `(,(string-append "-j" (number->string (parallel-job-count)))
                      "platform=server" ,@(delete "platform=x11" scons-flags)))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (headless (assoc-ref outputs "headless"))
                    (zenity (assoc-ref inputs "zenity")))
               ;; Strip build info from filenames.
               (with-directory-excursion "bin"
                 (for-each
                  (lambda (file)
                    (let ((dest (car (string-split (basename file) #\.))))
                      (rename-file file dest)))
                  (find-files "." "godot.*\\.x11\\.opt\\.tools.*"))
                 (install-file "godot" (string-append out "/bin"))
                 (install-file "godot_server" (string-append headless "/bin")))
               ;; Tell the editor where to find zenity for OS.alert().
               (wrap-program (string-append out "/bin/godot")
                 `("PATH" ":" prefix (,(string-append zenity "/bin")))))
             #t))
         (add-after 'install 'install-godot-desktop
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (applications (string-append out "/share/applications"))
                    (icons (string-append out "/share/icons/hicolor")))
               (mkdir-p applications)
               (copy-file "misc/dist/linux/org.godotengine.Godot.desktop"
                          (string-append applications "/godot.desktop"))
               (for-each (lambda (icon dest)
                           (mkdir-p (dirname dest))
                           (copy-file icon dest))
                         '("icon.png" "icon.svg")
                         `(,(string-append icons "/256x256/apps/godot.png")
                           ,(string-append icons "/scalable/apps/godot.svg"))))
             #t)))))
    (outputs '("out" "headless"))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("alsa-lib" ,alsa-lib)
              ("bullet" ,bullet)
              ("freetype" ,freetype)
              ("glew" ,glew)
              ("glu" ,glu)
              ("libtheora" ,libtheora)
              ("libvorbis" ,libvorbis)
              ("libvpx" ,libvpx)
              ("libwebp" ,libwebp)
              ("libx11" ,libx11)
              ("libxcursor" ,libxcursor)
              ("libxi" ,libxi)
              ("libxinerama" ,libxinerama)
              ("libxrandr" ,libxrandr)
              ("mbedtls" ,mbedtls-apache)
              ("mesa" ,mesa)
              ("opusfile" ,opusfile)
              ("pcre2" ,pcre2)
              ("pulseaudio" ,pulseaudio)
              ("wslay" ,wslay)
              ("zenity" ,zenity)
              ("zstd" ,zstd "lib")))
    (home-page "https://godotengine.org/")
    (synopsis "Advanced 2D and 3D game engine")
    (description
     "Godot is an advanced multi-platform game engine written in C++.  If
features design tools such as a visual editor, can import 3D models and
provide high-quality 3D rendering, it contains an animation editor, and can be
scripted in a Python-like language.")
    (license license:expat)))

(define-public entt
  (package
    (name "entt")
    (version "3.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/skypjack/entt")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nzvnhiw3r6nkmxp749zwxc8kzja09nijyxibdbri3g2i7pysf58"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DENTT_BUILD_TESTING=ON"
                               "-DENTT_FIND_GTEST_PACKAGE=ON"
                               "-DENTT_BUILD_DOCS=ON")
       ;; Only tests are compiled, and they need assertions to work correctly.
       #:build-type "Debug"))
    (native-inputs
     `(;; for testing
       ("googletest" ,googletest)
       ;; for documentation
       ("doxygen" ,doxygen)
       ("graphviz" ,graphviz)))
    (synopsis "Entity component system")
    (description "EnTT is a header-only library, containing (among other things)
@itemize
@item an entity component system based on sparse sets,
@item a configuration system using the monostate pattern,
@item a static reflection system,
@item and a cooperative scheduler.
@end itemize")
    (home-page "https://github.com/skypjack/entt")
    (license (list license:expat        ; code
                   license:cc-by4.0)))) ; documentation

(define-public eureka
  (package
    (name "eureka")
    (version "1.24")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/eureka-editor/Eureka/"
                                  version "/eureka-"
                                  ;; version without dots e.g 1.21 => 121
                                  (string-join (string-split version #\.) "")
                                  "-source.tar.gz"))
              (sha256
               (base32
                "1x4idjniz9sma3j9ss6ni7fafmz22zs2jnpsqw4my9rsnmra5d9v"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "PREFIX=" out)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'prepare-install-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/bin"))
               (mkdir-p (string-append out "/share"))

               (with-fluids ((%default-port-encoding #f))
                 (substitute* "./src/main.cc"
                   (("/usr/local") out)))

               (substitute* "Makefile"
                 (("-o root") ""))))))))
    (inputs `(("mesa" ,mesa)
              ("libxft" ,libxft)
              ("libxinerama" ,libxinerama)
              ("libfontconfig" ,fontconfig)
              ("libjpeg" ,libjpeg-turbo)
              ("libpng" ,libpng)
              ("fltk" ,fltk)
              ("zlib" ,zlib)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("xdg-utils" ,xdg-utils)))
    (synopsis "Doom map editor")
    (description "Eureka is a map editor for the classic DOOM games, and a few
related games such as Heretic and Hexen.  It comes with a 3d preview mode and
a 2D editor view.")
    (home-page "http://eureka-editor.sourceforge.net/")
    (license license:gpl2+)))

(define-public guile-chickadee
  (package
    (name "guile-chickadee")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.dthompson.us/chickadee/"
                                  "chickadee-" version ".tar.gz"))
              (sha256
               (base32
                "1jv4jkc35b7rizz8iflh74hhk9qy665isn1xa6gqz0qp9grwb019"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags '("GUILE_AUTO_COMPILE=0")))
    (propagated-inputs
     `(("guile-opengl" ,guile3.0-opengl)
       ("guile-sdl2" ,guile3.0-sdl2)))
    (inputs
     `(("freetype" ,freetype)
       ("guile" ,guile-3.0)
       ("libvorbis" ,libvorbis)
       ("mpg123" ,mpg123)
       ("openal" ,openal)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)))
    (home-page "https://dthompson.us/projects/chickadee.html")
    (synopsis "Game development toolkit for Guile Scheme with SDL2 and OpenGL")
    (description "Chickadee is a game development toolkit for Guile Scheme
built on top of SDL2 and OpenGL.  Chickadee aims to provide all the features
that parenthetically inclined game developers need to make 2D (and eventually
3D) games in Scheme, such as:

@enumerate
@item extensible, fixed-timestep game loop
@item OpenGL-based rendering engine
@item keyboard, mouse, controller input
@item REPL-driven development model
@end enumerate\n")
    (license license:gpl3+)))

(define-public guile3.0-chickadee
  (deprecated-package "guile3.0-chickadee" guile-chickadee))

(define-public bennu-game-development
  (package
    (name "bennu-game-development")
    (version "353")
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url "http://svn.code.sf.net/p/bennugd/code")
                    (revision (string->number version))))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "1iri58ryk9lbqn585cbccnvrfkj8qxlbcsk8rpih40jhvs1j101l"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "3rdparty") #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-configure-to-use-openssl
           (lambda* (#:key outputs #:allow-other-keys)
             (chdir "core")
             (delete-file "configure")
             (substitute* "configure.in"
               (("i\\*86\\)")
                "*)
                COMMON_CFLAGS=\"$COMMON_CFLAGS -DUSE_OPENSSL\"
                COMMON_LDFLAGS=\"$COMMON_LDFLAGS\"
                LIBSSL=\"crypto\"
                USE_OPENSSL=yes
                ;;

            i*86)"))
               #t)))))
    (inputs `(("openssl" ,openssl)
              ("zlib" ,zlib)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("libtool" ,libtool)))
    (synopsis "Programming language to create games")
    (description "Bennu Game Development, also known as bennudg, is a
programming language tailored at game development.  It is the successor of
Fenix.")
    (home-page "https://sourceforge.net/projects/bennugd/")
    (license license:zlib)))

(define-public bennu-game-development-modules
  (package
    (inherit bennu-game-development)
    (name "bennu-game-development-modules")
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-conflicting-definitions
           (lambda _
             (with-fluids ((%default-port-encoding #f))
               (substitute* "core/include/fmath.h"
                 (("extern fixed fmul\\( int x, int y \\);") "")
                 (("extern fixed fdiv\\( int x, int y \\);") "")))
             (chdir "modules"))))))
    (inputs `(("zlib" ,zlib)
              ("libpng" ,libpng)
              ("openssl" ,openssl)
              ("sdl-mixer" ,sdl-mixer)
              ("bennu-game-development" ,bennu-game-development)))
    (synopsis "Modules for the Bennu Game Development programming language")
    (description "This package contains a collection of modules for the Bennu
Game Development programming language, from CD handling through SDL to
joystick support.")))

(define-public plib
  (package
    (name "plib")
    (version "1.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://plib.sourceforge.net/dist/"
                                  "plib-" version ".tar.gz"))
              (sha256
               (base32
                "0cha71mflpa10vh2l7ipyqk67dq2y0k5xbafwdks03fwdyzj4ns8"))
              (patches (search-patches "plib-CVE-2011-4620.patch"
                                       "plib-CVE-2012-4552.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("mesa" ,mesa)
       ("libxi" ,libxi)
       ("libxmu" ,libxmu)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://plib.sourceforge.net/")
    (synopsis "Suite of portable game libraries")
    (description "PLIB is a set of libraries that will permit programmers to
write games and other realtime interactive applications that are 100% portable
across a wide range of hardware and operating systems.  PLIB includes sound
effects, music, a complete 3D engine, font rendering, a simple Windowing
library, a game scripting language, a GUI, networking, 3D math library and a
collection of handy utility functions.  All are 100% portable across nearly
all modern computing platforms.  Each library component is fairly independent
of the others")
    (license license:lgpl2.0+)))

(define-public ioquake3
  ;; We follow master since it seems that there won't be releases after 1.3.6.
  (let ((commit "95b9cab4d644fa3bf757cfff821cc4f7d76e38b0"))
    (package
      (name "ioquake3")
      (version (git-version "1.3.6" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ioquake/ioq3")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1vflk028z9gccg5yfi5451y1k5wxjdh3qbhjf4x6r7w2pzlxh16z"))))
      (build-system gnu-build-system)
      (inputs
       `(("sdl2" ,sdl2)
         ("libjpeg" ,libjpeg-turbo)
         ("openal" ,openal)
         ("curl" ,curl)
         ("opusfile" ,opusfile)
         ("opus" ,opus)
         ("libvorbis" ,libvorbis)
         ("freetype" ,freetype)
         ("libogg" ,libogg)))
      (native-inputs
       `(("which" ,which)               ; Else SDL_version.h won't be found.
         ("pkg-config" ,pkg-config)))
      (arguments
       '(#:tests? #f                    ; No tests.
         #:make-flags '("CC=gcc"
                        "USE_INTERNAL_LIBS=0"
                        "USE_FREETYPE=1"
                        "USE_RENDERER_DLOPEN=0"
                        "USE_OPENAL_DLOPEN=0"
                        "USE_CURL_DLOPEN=0")
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (invoke "make" "copyfiles" "CC=gcc"
                        "USE_INTERNAL_LIBS=0"
                       (string-append "COPYDIR="
                                      (assoc-ref outputs "out")
                                      "/bin")))))))
      (home-page "https://ioquake3.org/")
      (synopsis "FPS game engine based on Quake 3")
      (description "ioquake3 is a free software first person shooter engine
based on the Quake 3: Arena and Quake 3: Team Arena source code.  Compared to
the original, ioquake3 has been cleaned up, bugs have been fixed and features
added.  The permanent goal is to create the open source Quake 3 distribution
upon which people base their games, ports to new platforms, and other
projects.")
      (license license:gpl2))))

(define-public openvr
  (package
    (name "openvr")
    (version "1.8.19")
    (home-page "https://github.com/ValveSoftware/openvr/")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b8cppvw6ib0kvx0vjq7jsk3plg1vh171w8xr230vjn05381wp52"))))
    (build-system cmake-build-system)
    (arguments
     ;; No tests.
     '(#:tests? #f
       #:configure-flags (list "-DBUILD_SHARED=1")))
    (synopsis "Virtual reality software development kit")
    (description "OpenVR is an API and runtime that allows access to VR
hardware from multiple vendors without requiring that applications have
specific knowledge of the hardware they are targeting.")
    (license license:bsd-3)))

(define-public flatzebra
  (package
    (name "flatzebra")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://perso.b2b2c.ca/~sarrazip/dev/"
                           "flatzebra-" version ".tar.gz"))
       (sha256
        (base32 "1x2dy41c8vrq62bn03b82fpmk7x4rzd7qqiwvq0mgcl5rmasc2c8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-sdl-config
           (lambda* (#:key inputs #:allow-other-keys)
             ;; XXX: sdl-config in sdl-union is a link to sdl-config from
             ;; plain sdl package.  As a consequence, the prefix is wrong.
             ;; Force correct one with "--prefix" argument.
             (let ((sdl-union (assoc-ref inputs "sdl")))
               (setenv "SDL_CONFIG"
                       (string-append sdl-union
                                      "/bin/sdl-config --prefix="
                                      sdl-union)))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("sdl" ,(sdl-union (list sdl sdl-image sdl-mixer)))))
    (home-page "http://perso.b2b2c.ca/~sarrazip/dev/burgerspace.html")
    (synopsis "Generic game engine for 2D double-buffering animation")
    (description
     "Flatzebra is a simple, generic C++ game engine library supporting 2D
double-buffering.")
    (license license:gpl2+)))

(define-public fna
  (package
    (name "fna")
    (version "19.12.01")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FNA-XNA/FNA")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vdyi9hac24fqcs8kpj6yk36bf5rrl4dvlvdd9fc701fawcf6lrr"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; No tests.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'link-dep-src
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((sdl2 (assoc-ref inputs "sdl2-cs-src"))
                   (mojoshader (assoc-ref inputs "mojoshader-src"))
                   (faudio (assoc-ref inputs "faudio-src"))
                   (theorafile (assoc-ref inputs "theorafile-src")))
               (symlink (string-append sdl2 "/src") "lib/SDL2-CS/src")
               (symlink (string-append mojoshader "/csharp") "lib/MojoShader/csharp")
               (symlink (string-append faudio "/csharp") "lib/FAudio/csharp")
               (symlink (string-append theorafile "/csharp") "lib/Theorafile/csharp"))))
         (delete 'configure)
         (replace 'build
           (lambda _
             (invoke "make" "release")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "bin/Release/FNA.dll" (string-append out "/lib"))
               #t))))))
    (native-inputs
     `(("mono" ,mono)))
    (inputs `(("sdl2-cs-src" ,(package-source sdl2-cs))
              ("mojoshader-src" ,(package-source mojoshader-cs))
              ("faudio-src" ,(package-source faudio))
              ("theorafile-src" ,(package-source theorafile))))
    (home-page "https://fna-xna.github.io/")
    (synopsis "Accuracy-focused XNA4 reimplementation")
    (description "FNA is a Microsoft XNA Game Studio 4.0 reimplementation that
focuses solely on developing a fully accurate XNA4 runtime for the desktop.")
    (license (list license:ms-pl        ; FNA
                   license:lgpl2.1      ; LzxDecoder.cs
                   ;; Mono.Xna:
                   license:expat))))

(define-public libccd
  (package
    (name "libccd")
    (version "2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/danfis/libccd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sfmn5pd7k5kyhbxnd689xmsa5v843r7sska96dlysqpljd691jc"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_DOCUMENTATION=ON"
                           "-DBUILD_TESTING=ON"
                           "-DENABLE_DOUBLE_PRECISION=ON")))
    (native-inputs
     `(("python-sphinx" ,python-sphinx)))
    (home-page "https://github.com/danfis/libccd")
    (synopsis "Library for collision detection between two convex shapes")
    (description "@code{libccd} is library for a collision detection
between two convex shapes.  @code{libccd} implements variation on
Gilbert–Johnson–Keerthi algorithm plus Expand Polytope Algorithm (EPA)
and also implements algorithm Minkowski Portal Refinement (MPR,
a.k.a. XenoCollide) as described in Game Programming Gems 7.")
    (license license:expat)))

(define-public ode
  (package
    (name "ode")
    (version "0.16.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://bitbucket.org/odedevs/ode/downloads/"
                           "ode-" version ".tar.gz"))
       (sha256
        (base32 "08hgh4gqdk77jcw8b7gq2mwsfg4a5v5y0j7g42bxiqhmn3ffnsmj"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "libccd")
           #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DODE_WITH_LIBCCD_SYSTEM=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unbundle-libccd
           (lambda _
             (substitute* "CMakeLists.txt"
               (("configure_file\\(libccd/.*") ""))
             #t)))))
    (inputs
     `(("glu" ,glu)
       ("libccd" ,libccd)
       ("mesa" ,mesa)))
    (home-page "https://www.ode.org/")
    (synopsis "High performance library for simulating rigid body dynamics")
    (description "ODE is a high performance library for simulating
rigid body dynamics.  It is fully featured, stable, mature and
platform independent with an easy to use C/C++ API.  It has advanced
joint types and integrated collision detection with friction.  ODE is
useful for simulating vehicles, objects in virtual reality
environments and virtual creatures.  It is currently used in many
computer games, 3D authoring tools and simulation tools.")
    ;; Software is dual-licensed.
    (license (list license:lgpl2.1+ license:expat))))

(define-public chipmunk
  (package
    (name "chipmunk")
    (version "7.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/slembcke/Chipmunk2D")
             (commit (string-append "Chipmunk-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qmkn01g06p3rnhmbyffmjns6wj5vhgf9cscigk3wzxcpwv1hyxb"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:configure-flags '("-DBUILD_STATIC=OFF"
                           "-DBUILD_DEMOS=OFF")))
    (inputs
     `(("freeglut" ,freeglut)
       ("libxmu" ,libxmu)
       ("libxrandr" ,libxrandr)))
    (home-page "https://chipmunk-physics.net/")
    (synopsis "Fast and lightweight 2D game physics library")
    (description "Chipmunk is a simple, lightweight, fast and portable 2D
rigid body physics library written in C.")
    (license license:expat)))

(define-public libtcod
  (package
    (name "libtcod")
    (version "1.15.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libtcod/libtcod")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pzr8ajmbqvh43ldjajx962xirj3rf8ayh344p6mqlrmb8gxrfr5"))
              (modules '((guix build utils)))
              (snippet '(begin
                          (delete-file-recursively "src/vendor/utf8proc")
                          (delete-file-recursively "src/vendor/zlib")
                          (delete-file "src/vendor/stb_truetype.h")
                          (delete-file "src/vendor/stb_sprintf.h")
                          (delete-file "src/vendor/lodepng.cpp")
                          (delete-file "src/vendor/lodepng.h")

                          (substitute* "buildsys/autotools/sources.am"
                            (("\\.\\./\\.\\./src/vendor/lodepng\\.cpp \\\\\n") "")
                            (("\\.\\./\\.\\./src/vendor/stb\\.c \\\\")
                             "../../src/vendor/stb.c")
                            (("\\.\\./\\.\\./src/vendor/utf8proc/utf8proc\\.c") ""))

                          (substitute* "src/libtcod/sys_sdl_img_png.cpp"
                            (("\\.\\./vendor/") ""))

                          (substitute* '("src/libtcod/color/canvas.cpp"
                                         "src/libtcod/sys_sdl_img_png.cpp"
                                         "src/libtcod/tileset/truetype.cpp"
                                         "src/libtcod/tileset/tilesheet.cpp")
                            (("\\.\\./\\.\\./vendor/") ""))

                          (substitute* "src/libtcod/console/printing.cpp"
                            (("\\.\\./\\.\\./vendor/utf8proc/") ""))
                          #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--with-gnu-ld"
                           "LIBS=-lutf8proc -llodepng")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-to-build-dir
           (lambda _
             (chdir "buildsys/autotools")
             (patch-shebang "get_version.py")
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("python" ,python)
       ("pkg-config" ,pkg-config)
       ("stb-sprintf" ,stb-sprintf)
       ("stb-truetype" ,stb-truetype)))
    (inputs
     `(("lodepng" ,lodepng)
       ("sdl2" ,sdl2)
       ("utf8proc" ,utf8proc)
       ("zlib" ,zlib)))
    (home-page "https://github.com/libtcod/libtcod")
    (synopsis "Library specifically designed for writing roguelikes")
    (description
     "libtcod is a fast, portable and uncomplicated API for roguelike
developers providing an advanced true color console, input, and lots of other
utilities frequently used in roguelikes.")
    (license license:bsd-3)))

(define-public warsow-qfusion
  ;; As of 2020-04-09, the latest stable version 2.1.0 is deprecated.
  ;; The 2.5 beta as published on the homepage is commit
  ;; c4de15df559410aff0ca6643724e24cddb0ecbbd
  (let ((commit "c4de15df559410aff0ca6643724e24cddb0ecbbd"))
    (package
      (name "warsow-qfusion")
      (version (git-version "2.5" "1" commit)) ; 2.5-beta
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Warsow/qfusion/")
                      (commit commit)
                      (recursive? #t)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0xv2yycr43p3xmq7lm6j6zb3cpcr6w00x7qg918faq0mw9j7v48g"))
                ;; Issue reported here: https://github.com/Warsow/qfusion/issues/46
                (patches (search-patches "warsow-qfusion-fix-bool-return-type.patch"))
                (modules '((guix build utils)))
                (snippet '(begin
                            (delete-file-recursively "platforms")
                            (delete-file-recursively "debian")
                            (delete-file-recursively "libsrcs")
                            #t))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f                    ; No tests.
         #:configure-flags '("-DQFUSION_GAME=Warsow")
         #:modules
         ((guix build utils)
          (guix build cmake-build-system)
          (ice-9 match))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'change-to-build-dir
             (lambda _
               (chdir "source")
               #t))
           (add-after 'install 'really-install
             (lambda* (#:key outputs system #:allow-other-keys)
               (let ((arch (match system
                             ("x86_64-linux" "x86_64")
                             ("i686-linux" "i386")))
                     (out (assoc-ref outputs "out")))
                 (install-file (string-append "../source/build/basewsw/libgame_"
                                              arch ".so")
                               (string-append out "/lib/"))
                 (install-file (string-append "../source/build/libui_" arch ".so")
                               (string-append out "/lib/"))
                 (for-each
                  (lambda (file)
                    (install-file file (string-append out "/bin/")))
                  (append (find-files "../source/build" "warsow")
                          (find-files "../source/build" "wsw_server."))))
               #t)))))
      (inputs
       `(("alsa-lib" ,alsa-lib)
         ("curl" ,curl)
         ("freetype" ,freetype)
         ("ffmpeg" ,ffmpeg)
         ("libjpeg" ,libjpeg-turbo)
         ("libogg" ,libogg)
         ("libpng" ,libpng)
         ("libtheora" ,libtheora)
         ("libvorbis" ,libvorbis)
         ("mesa" ,mesa)
         ("openal" ,openal)
         ("pulseaudio" ,pulseaudio)
         ("qtbase" ,qtbase)
         ("qtdeclarative" ,qtdeclarative)
         ("sdl2" ,sdl2)
         ("uuid.h" ,util-linux "lib")
         ("zlib" ,zlib)))
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (home-page "https://github.com/Warsow/qfusion")
      (supported-systems '("i686-linux" "x86_64-linux"))
      (synopsis "Warsow's fork of qfusion, the id Tech 2 derived game engine")
      (description
       "This package contains the game engine of Warsow, a first-person
shooter video game.  The engine is based on qfusion, the id Tech 2 derived
game engine.  id Tech 2 is the engine originally behind Quake 2.")
      (license license:gpl2+))))

(define-public dhewm3
  (package
    (name "dhewm3")
    (version "1.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/dhewm/dhewm3/releases/download/"
                    version "/dhewm3-" version "-src.tar.xz"))
              (sha256
               (base32
                "0s2brx6wyljhjbpli97iy4lc4fqqsvdc09raz8njg0vgzcsiyrri"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No tests.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-to-build-dir
           (lambda _
             (chdir "neo")
             #t)))))
    (inputs
     `(("curl" ,curl)
       ("libjpeg" ,libjpeg-turbo)
       ("libogg" ,libogg)
       ("libvorbis" ,libvorbis)
       ("libx11" ,libx11)
       ("openal" ,openal)
       ("sdl2" ,sdl2)
       ("zlib" ,zlib)))
    (home-page "https://dhewm3.org/")
    (synopsis "Port of the original Doom 3 engine")
    (description
     "@command{dhewm3} is a source port of the original Doom 3 engine (not
Doom 3: BFG Edition), also known as id Tech 4.  Compared to the original
version of the Doom 3 engine, dhewm3 has many bugfixes, supports EAX-like
sound effects on all operating systems and hardware (via OpenAL Softs EFX
support), has much better support for widescreen resolutions and has 64bit
support.")
    (license license:gpl3)))

(define-public tesseract-engine
  (let ((svn-revision 2411))
    (package
      (name "tesseract-engine")
      (version (string-append "20200615-" (number->string svn-revision)))
      (source
       (origin
         (method svn-fetch)
         (uri (svn-reference
               (url "svn://svn.tuxfamily.org/svnroot/tesseract/main")
               (revision svn-revision)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1av9jhl2ivbl7wfszyhyna84llvh1z2d8khkmadm8d105addj10q"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("bin" "bin64"
                         ;; Remove "media" since some files such as
                         ;; media/sound/game/soundsnap/info.txt refer to a
                         ;; non-commercial license.
                         "media"
                         "server.bat"
                         "tesseract.bat"
                         "src/lib"
                         "src/lib64"))
             #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list "CC=gcc")
         #:tests? #f                    ; No tests.
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'cd-src
             (lambda _ (chdir "src") #t))
           (add-before 'build 'fix-env
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "CPATH"
                       (string-append (assoc-ref inputs "sdl2-union")
                                      "/include/SDL2:"
                                      (or (getenv "CPATH") "")))
               #t))
           (add-after 'install 'really-install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (share (string-append out "/share/tesseract"))
                      (bin (string-append out "/bin/tesseract"))
                      (client (string-append out "/bin/tesseract-client")))
                 (chdir "..")           ; Back to root.
                 (for-each
                  (lambda (dir)
                    (mkdir-p (string-append share "/" dir))
                    (copy-recursively dir (string-append share "/" dir)))
                  '("config"))
                 (mkdir-p (string-append out "/bin/"))
                 (copy-file "bin_unix/native_client" client)
                 (copy-file "bin_unix/native_server"
                            (string-append out "/bin/tesseract-server"))
                 (call-with-output-file bin
                   (lambda (p)
                     (format p "#!~a
TESS_DATA=~a
TESS_BIN=~a
TESS_OPTIONS=\"-u$HOME/.tesseract\"
cd \"$TESS_DATA\"
exec \"$TESS_BIN\" \"$TESS_OPTIONS\" \"$@\""
                             (which "bash")
                             share
                             client)))
                 (chmod bin #o755)
                 (install-file "src/readme_tesseract.txt"
                               (string-append out "/share/licenses/tesseract/LICENSE")))
               #t)))))
      (inputs
       `(("sdl2-union" ,(sdl-union (list sdl2 sdl2-mixer sdl2-image)))
         ("zlib" ,zlib)
         ("libpng" ,libpng)
         ("libgl" ,mesa)))
      (home-page "http://tesseract.gg/")
      (synopsis "First-person shooter engine with map editing, instagib, DM and CTF")
      (description "This package contains the game engine of Tesseract, a
first-person shooter focused on cooperative in-game map editing.

The engine is derived from @emph{Cube 2: Sauerbraten} technology but with
upgraded modern rendering techniques.  The new rendering features include
fully dynamic omnidirectional shadows, global illumination, HDR lighting,
deferred shading, morphological / temporal / multisample anti-aliasing, and
much more.")
      (license license:zlib))))
