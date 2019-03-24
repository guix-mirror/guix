;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Tomáš Čech <sleep_walker@suse.cz>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2018 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015, 2016, 2017 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018 Julian Graham <joolean@gmail.com>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
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
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages dbm)
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
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages stb)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public bullet
  (package
    (name "bullet")
    (version "2.88")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/bulletphysics/bullet3/")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00qkif245yj7n2f262bgjaxv1bz3wmmcsfnjgy3qpzvlpzpns5z8"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file (find-files "build3" "premake*"))
                  (with-directory-excursion "examples/ThirdPartyLibs"
                    (for-each delete-file-recursively
                              '("Gwen" "clsocket" "enet" "glad" "imgui"
                                "lua-5.2.3" "midi" "minizip" "openvr"
                                "optionalX11" "serial" "zlib")))
                  ;; These need files from ThirdPartyLibs
                  (substitute* "Extras/CMakeLists.txt"
                    (("BulletRobotics") "")
                    (("obj2sdf") ""))
                  ;; Tests fail on linking, cannot find -lBussIK
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
           ;; These tests fail specifically after removing 3rd party code
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
   (version "5.1.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/Doom-Utils/" name
                                "/releases/download/v" version "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "1rj3w4xa0n4jixy4j7p6gbclylbgxvhdnji7xnkydrqii9rxnbp4"))))
   (build-system gnu-build-system)
   (native-inputs `(("asciidoc" ,asciidoc)))
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
    (source (origin
              (method url-fetch)
              (uri (string-append "http://binaries.openttd.org/extra/"
                                  name "/" version "/" name "-" version
                                  "-source.tar.xz"))
              (sha256
               (base32
                "08admgnpqcsifpicbm56apgv360fxapqpbbsp10qyk8i22w1ivsk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no check target
       #:phases
      (modify-phases %standard-phases
        (delete 'configure) ; no configure script
        (replace 'install   ; no install target
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
    (home-page "http://dev.openttdcoop.org/projects/grfcodec")
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
        (base32
         "1qg0c2i4p29sxj0q6qp2jynlrzm5pphz2xhcjqlxa69ycrnlxzs7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:make-flags (list (string-append "prefix=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (home-page "http://dev.openttdcoop.org/projects/catcodec")
    (synopsis "Encode/decode OpenTTD sounds")
    (description "catcodec encodes and decodes sounds for OpenTTD.  These
sounds are not much more than some metadata (description and filename) and raw
PCM data.")
    (license license:gpl2)))

(define-public gzochi
  (package
    (name "gzochi")
    (version "0.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/gzochi/gzochi-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0h8yvk7154kd8zdfa9nqy73blrjq2x19kv305jcnwlmm09vvss59"))))
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
              ("guile" ,guile-2.2)
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
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://bundles.openttdcoop.org/nml/releases/"
                           version "/nml-" version ".tar.gz"))
       (sha256
        (base32
         "1pmvvm3sgnpngfa7884mqhq3fwdjh9sr0ca07ypnidcg0y341w53"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pillow" ,python-pillow)
       ("python-ply" ,python-ply)))
    (home-page "https://dev.openttdcoop.org/projects/nml")
    (synopsis "NML compiler")
    (description
     "@dfn{NewGRF Meta Language} (NML) is a python-based compiler, capable of
compiling NML files (along with their associated language, sound and graphic
files) into @file{.grf} and/or @file{.nfo} files.")
    (license license:gpl2+)))

(define-public python-sge-pygame
  (package
    (name "python-sge-pygame")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/stellarengine/"
                           (version-major+minor version) "/sge-pygame-"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1rl3xjzh78sl0sq3xl8rl7cgp9v9v3h7s2pfwn7nj1vrmffzkcpd"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pygame" ,python-pygame)
       ("python-six" ,python-six)
       ("python-uniseg" ,python-uniseg)))
    (home-page "http://stellarengine.nongnu.org")
    (synopsis "2D game engine for Python")
    (description
     "The SGE Game Engine (\"SGE\", pronounced like \"Sage\") is a
general-purpose 2D game engine.  It takes care of several details for you so
you can focus on the game itself.  This makes more rapid game development
possible, and it also makes the SGE easy to learn.")
    (license license:lgpl3+)))

(define-public python2-sge-pygame
  (package-with-python2 python-sge-pygame))

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
    (version "2018.02.26")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/xsge/xsge/xsge-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0bx93hgf7cgdw2gsygbh59y8vpw37pgsa279rajw3fkdpl8vrc40"))))
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
    (propagated-inputs
     `(("python-sge-pygame" ,python-sge-pygame)
       ("python-pygame" ,python-pygame)
       ("python-six" ,python-six)
       ("python-tmx" ,python-tmx)))
    (home-page "http://xsge.nongnu.org")
    (synopsis "Extensions for the SGE Game Engine")
    (description
     "xSGE is a collection of modules that make doing certain tasks with the SGE
Game Engine easier.  In addition to SGE's conveniences, the user has access to a
GUI toolkit, lighting and physics frameworks and @code{Tiled} TMX format
support.")
    (license license:gpl3+)))

(define-public python2-xsge
  (package-with-python2 python-xsge))

(define-public tiled
  (package
    (name "tiled")
    (version "1.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bjorn/tiled.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nfyigfkl10n9r82p1qxhpr09jn2kwalh9n5r209bcaj8dxspph8"))))
    (build-system gnu-build-system)
    (inputs
     `(("qtbase" ,qtbase)
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
    (home-page "http://www.mapeditor.org/")
    (synopsis "Tile map editor")
    (description
     "Tiled is a general purpose tile map editor.  It is meant to be used for
editing maps of any tile-based game, be it an RPG, a platformer or a Breakout
clone.")

    ;; As noted in 'COPYING', part of it is under GPLv2+, while the rest is
    ;; under BSD-2.
    (license license:gpl2+)))

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
                    (url "https://github.com/SFML/SFML.git")
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
       ("libjpeg" ,libjpeg)
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
    (version "11.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://bitbucket.org/rude/love/downloads/"
                                 "love-" version "-linux-src.tar.gz"))
             (sha256
              (base32
               "1pkwiszmjs0xrwk0wqbc5cp9108b1y8gwsid0gqk1s0x09q9lpmw"))))
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
       ("physfs" ,physfs)
       ("sdl2" ,sdl2)
       ("zlib" ,zlib)))
    (synopsis "2D game framework for Lua")
    (description "LÖVE is a framework for making 2D games in the Lua
programming language.")
    (home-page "https://love2d.org/")
    (license license:zlib)))

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
    (home-page "http://liballeg.org")
    (license license:giftware)))

(define-public allegro
  (package
    (name "allegro")
    (version "5.2.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/liballeg/allegro5/releases"
                                  "/download/" version "/allegro-"
                                  version ".tar.gz"))
              (patches (search-patches
                        "allegro-mesa-18.2.5-and-later.patch"))
              (sha256
               (base32
                "1w9a5yqi5q03b2qvmx5ff90paz0xbr9cy7i7f0xiqa65ava66q9l"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; there are no tests
    (inputs
     ;; FIXME: Add the following optional inputs: xinput2, opensl, dumb
     `(("flac" ,flac)
       ("freetype" ,freetype)
       ("glu" ,glu)
       ("gtk" ,gtk+-2)
       ("libjpeg" ,libjpeg)
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
    (home-page "http://liballeg.org")
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
       ("libjpeg" ,libjpeg)
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
       ("libjpeg" ,libjpeg)
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
             (url "https://github.com/wgois/OIS.git")
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
       (method url-fetch)
       (uri
        (string-append "https://github.com/MyGUI/" name
                       "/archive/MyGUI" version ".tar.gz"))
       (sha256
        (base32
         "13x7cydmj7gjmsg702sqjbfi53z265iv6j7binv3r6a7ibndfa0a"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ; No test target
       #:configure-flags
       (list "-DMYGUI_INSTALL_DOCS=TRUE"
             (string-append "-DOGRE_INCLUDE_DIR="
                            (assoc-ref %build-inputs "ogre")
                            "/include/OGRE"))))
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

(define-public openmw
  (package
    (name "openmw")
    (version "0.44.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/OpenMW/openmw/archive/"
                       "openmw-" version ".tar.gz"))
       (sha256
        (base32
         "03fgm2f2r7y0aqlgp038pdlnllgvm3jimrp968p4nhz1sffvjzcy"))))
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
       ("mygui" ,mygui)
       ("openal" ,openal)
       ("openscenegraph" ,openscenegraph)
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
    (version "3.0.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/godotengine/godot")
                    (commit (string-append version "-stable"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0g64h0x8dlv6aa9ggfcidk2mknkfl5li7z1phcav8aqp9srj8avf"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Drop libraries that we take from Guix.  Note that some
                  ;; of these may be modified; see "thirdparty/README.md".
                  (with-directory-excursion "thirdparty"
                    (for-each delete-file-recursively
                              '("freetype"
                                "libogg"
                                "libpng"
                                "libtheora"
                                "libvorbis"
                                "libvpx"
                                "libwebp"
                                "openssl"
                                "opus"
                                "zlib"))
                    #t)))))
    (build-system scons-build-system)
    (arguments
     `(#:scons ,scons-python2
       #:scons-flags (list "platform=x11"
                           ,@(if (string-prefix? "aarch64" (or (%current-target-system)
                                                               (%current-system)))
                               `("CCFLAGS=-DNO_THREADS")
                               '())
                           ;; Avoid using many of the bundled libs.
                           ;; Note: These options can be found in the SConstruct file.
                           "builtin_freetype=no"
                           "builtin_glew=no"
                           "builtin_libmpdec=no"
                           "builtin_libogg=no"
                           "builtin_libpng=no"
                           "builtin_libtheora=no"
                           "builtin_libvorbis=no"
                           "builtin_libvpx=no"
                           "builtin_libwebp=no"
                           "builtin_openssl=no"
                           "builtin_opus=no"
                           "builtin_zlib=no")
       #:tests? #f ; There are no tests
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
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (with-directory-excursion "bin"
                 (if (file-exists? "godot.x11.tools.64")
                     (rename-file "godot.x11.tools.64" "godot")
                     (rename-file "godot.x11.tools.32" "godot"))
                 (install-file "godot" bin))
               #t)))
         (add-after 'install 'install-godot-desktop
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (desktop (string-append out "/share/applications"))
                    (icon-dir (string-append out "/share/pixmaps")))
               (rename-file "icon.png" "godot.png")
               (install-file "godot.png" icon-dir)
               (mkdir-p desktop)
               (with-output-to-file
                   (string-append desktop "/godot.desktop")
                 (lambda _
                   (format #t
                           "[Desktop Entry]~@
                           Name=godot~@
                           Comment=The godot game engine~@
                           Exec=~a/bin/godot~@
                           TryExec=~@*~a/bin/godot~@
                           Icon=godot~@
                           Type=Application~%"
                           out)))
               #t))))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("alsa-lib" ,alsa-lib)
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
              ("mesa" ,mesa)
              ("openssl" ,openssl)
              ("opusfile" ,opusfile)
              ("pulseaudio" ,pulseaudio)))
    (home-page "https://godotengine.org/")
    (synopsis "Advanced 2D and 3D game engine")
    (description
     "Godot is an advanced multi-platform game engine written in C++.  If
features design tools such as a visual editor, can import 3D models and
provide high-quality 3D rendering, it contains an animation editor, and can be
scripted in a Python-like language.")
    (license license:expat)))

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
              ("libjpeg" ,libjpeg)
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
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.dthompson.us/chickadee/"
                                  "chickadee-" version ".tar.gz"))
              (sha256
               (base32
                "0jl223dybsj5gvs7z4q60gnafj1b7kgi5mx0kj58m5knrp8qwg5h"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags '("GUILE_AUTO_COMPILE=0")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-godir
           (lambda _
             ;; Install compiled '.go' files into the site directory.
             (substitute* "Makefile.in"
               (("/ccache") "/site-ccache")))))))
    (propagated-inputs
     `(("guile-opengl" ,guile-opengl)
       ("guile-sdl2" ,guile-sdl2)))
    (inputs
     `(("guile" ,guile-2.2)))
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

(define-public bennu-game-development
  (package
    (name "bennu-game-development")
    (version "348")
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url "http://svn.code.sf.net/p/bennugd/code")
                    (revision (string->number version))))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "0wpzsbh4zi3931493dnyl5ffmh1b7fj2sx3mzrq304z9zs4d6lqq"))
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
    (synopsis "Modules for the Bennu Game Developement programming language")
    (description "This package contains a collection of modules for the Bennu
Game Developement programming language, from CD handling through SDL to
joystick support.")))
