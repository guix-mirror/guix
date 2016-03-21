;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Tomáš Čech <sleep_walker@suse.cz>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Julian Graham <joolean@gmail.com>
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages doxygen)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages zip)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages image)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages xml))

(define-public bullet
  (package
    (name "bullet")
    (version "2.82-r2704")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bullet.googlecode.com/files/bullet-"
                                  version ".tgz"))
              (sha256
               (base32
                "1lnfksxa9b1slyfcxys313ymsllvbsnxh9np06azkbgpfvmwkr37"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f ; no 'test' target
                 #:configure-flags (list
                                    (string-append
                                     "-DCMAKE_CXX_FLAGS=-fPIC "
                                     (or (getenv "CXXFLAGS") "")))))
    (home-page "http://bulletphysics.org/")
    (synopsis "3D physics engine library")
    (description
     "Bullet is a physics engine library usable for collision detection.  It
is used in some video games and movies.")
    (license license:zlib)))

(define-public gzochi
  (package
    (name "gzochi")
    (version "0.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/gzochi/gzochi-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1nf8naqbc4hmhy99b8n70yswg9j71nh5mfpwwh6d8pdw5mp9b46a"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'remove-Werror
                              (lambda _
                                ;; We can't build with '-Werror', notably
                                ;; because deprecated functions of
                                ;; libmicrohttpd are being used.
                                (substitute* (find-files "." "^Makefile\\.in$")
                                  (("-Werror")
                                   ""))
                                #t)))))
    (native-inputs `(("pkgconfig" ,pkg-config)))
    (inputs `(("bdb" ,bdb)
              ("glib" ,glib)
              ("gmp" ,gmp)
              ("guile" ,guile-2.0)
              ("libmicrohttpd" ,libmicrohttpd)
              ("ncurses" ,ncurses)
              ("sdl" ,sdl)
              ("zlib" ,zlib)))
    (home-page "http://www.nongnu.org/gzochi/")
    (synopsis "Scalable middleware for multiplayer games")
    (description
     "gzochi is a framework for developing massively multiplayer online games.
A server container provides services to deployed games, which are written in
Guile Scheme, that abstract and simplify some of the most challenging and
error-prone aspects of online game development: Concurrency, data persistence,
and network communications.  A very thin client library can be embedded to
provide connectivity for client applications written in any language.")
    (license license:gpl3+)))

(define-public tiled
  (package
    (name "tiled")
    (version "0.15.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/bjorn/tiled/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "10bbjdv5r36229r1dqg32bxcj9yvpkx9jgs9v4s1qb71v856b15h"))))
    (build-system gnu-build-system)
    (inputs `(("qt" ,qt)
              ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (system* "qmake"
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
    (version "2.3.2")
    (source (origin
              (method url-fetch)
              ;; Do not fetch the archives from
              ;; http://mirror0.sfml-dev.org/files/ because files there seem
              ;; to be changed in place.
              (uri (string-append "https://github.com/SFML/SFML/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0k2fl5xk3ni2q8bsxl0551inx26ww3w6cp6hssvww0wfjdjcirsm"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (inputs
     `(("mesa" ,mesa)
       ("glew" ,glew)
       ("flac" ,flac)
       ("libvorbis" ,libvorbis)
       ("libx11" ,libx11)
       ("xcb-util-image" ,xcb-util-image)
       ("libxrandr" ,libxrandr)
       ("eudev" ,eudev)
       ("freetype" ,freetype)
       ("libjpeg" ,libjpeg)
       ("libsndfile" ,libsndfile)
       ("openal" ,openal)))
    (home-page "http://www.sfml-dev.org")
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
    (version "2.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://icculus.org/physfs/downloads/physfs-"
                    version ".tar.bz2"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0sbbyqzqhyf0g68fcvvv20n3928j0x6ik1njmhn1yigvq2bj11na"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; no check target
    (inputs
     `(("zlib" ,zlib)))
    (native-inputs
     `(("doxygen" ,doxygen)))
    (home-page "http://icculus.org/physfs")
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
    (version "0.10.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://bitbucket.org/rude/love/downloads/"
                                 "love-" version "-linux-src.tar.gz"))
             (sha256
              (base32
               "1r2n1nrw3hcdvy14fjbwz3l9swcy65v3lqwpj2frnkkcwncdz94p"))))
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
    (version "4.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.gna.org/allegro/allegro/"
                                  version "/allegro-" version ".tar.gz"))
              (sha256
               (base32
                "1p0ghkmpc4kwij1z9rzxfv7adnpy4ayi0ifahlns1bdzgmbyf88v"))))
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

(define-public aseprite
  (package
    (name "aseprite")
    (version "1.1.1")
    ;; The release tarball isn't good enough because it's missing some
    ;; necessary code that is only in git submodules.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aseprite/aseprite.git")
                    (commit "v1.1.1")
                    (recursive? #t)))
              (sha256
               (base32
                "1yr0l3bc68lyrffrzxgw98zznv8yz5ldl98lyvp6s5grny4s4jyk"))))
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
                            "/include/freetype2"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-freetype-utils
           (lambda _
             ;; Fix C preprocessor include directive.
             (substitute* '("src/app/util/freetype_utils.cpp")
               (("freetype/") ""))
             #t)))))
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
    (home-page "http://www.aseprite.org/")
    (license license:gpl2+)))
