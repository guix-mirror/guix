;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Tomáš Čech <sleep_walker@suse.cz>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages zip)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages image)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xiph))

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

(define-public tiled
  (package
    (name "tiled")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/bjorn/tiled/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cfdhjwwwnc5fsq53axxyf6vmyy1grkk9ghi58dir68pwf6rlr3n"))))
    (build-system gnu-build-system)
    (inputs `(("qt" ,qt)
              ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (alist-replace
        'configure
        (lambda* (#:key outputs #:allow-other-keys)
          (let ((out (assoc-ref outputs "out")))
            (system* "qmake"
                     (string-append "PREFIX=" out))))
        %standard-phases)))
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
    (version "2.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://mirror0.sfml-dev.org/files/SFML-"
                                  version "-sources.zip"))
              (sha256
               (base32
                "0mjpkgfnz6ka4p0ir219pcqsbdy7gwcjydk7xxmjjfm2k5sw2qys"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (native-inputs
     `(("unzip" ,unzip)))
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
