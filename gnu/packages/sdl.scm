;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 David Thompson <dthompson2@worcester.edu>
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

(define-module (gnu packages sdl)
  #:use-module (gnu packages)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((gnu packages fontutils) #:prefix font:)
  #:use-module (gnu packages libjpeg)
  #:use-module (gnu packages libpng)
  #:use-module (gnu packages libtiff)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages oggvorbis)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xorg)
  #:export (sdl
            sdl2
            libmikmod
            sdl-gfx
            sdl-image
            sdl-mixer
            sdl-net
            sdl-ttf))

(define sdl
  (package
    (name "sdl")
    (version "1.2.15")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "http://libsdl.org/release/SDL-"
                             version ".tar.gz"))
             (sha256
              (base32
               "005d993xcac8236fpvd1iawkz4wqjybkpn8dbwaliqz5jfkidlyn"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f)) ; no check target
    (propagated-inputs
     ;; SDL headers include X11 headers.
     `(("libx11" ,libx11)))
    (inputs `(("libxrandr" ,libxrandr)
              ("mesa" ,mesa)
              ("alsa-lib" ,alsa-lib)
              ("pkg-config" ,pkg-config)
              ("pulseaudio" ,pulseaudio)))
    (synopsis "Cross platform game development library")
    (description "Simple DirectMedia Layer is a cross-platform development
library designed to provide low level access to audio, keyboard, mouse,
joystick, and graphics hardware.")
    (home-page "http://libsdl.org/")
    (license lgpl2.1)))

(define sdl2
  (package (inherit sdl)
    (name "sdl2")
    (version "2.0.0")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "http://libsdl.org/release/SDL2-"
                             version ".tar.gz"))
             (sha256
              (base32
               "0y3in99brki7vc2mb4c0w39v70mf4h341mblhh8nmq4h7lawhskg"))))
    (license bsd-3)))

(define libmikmod
  (package
    (name "libmikmod")
    (version "3.3.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/mikmod/libmikmod/"
                                 version "/libmikmod-" version ".tar.gz"))
             (sha256
              (base32
               "0dr4kgvhq9wf2riibh178c2al996spwwak6zffpv5n5bqmw29w3r"))))
    (build-system gnu-build-system)
    (inputs `(("alsa-lib" ,alsa-lib)
              ("libx11" ,libx11)))
    (synopsis "Library for module sound formats.")
    (description
     "MikMod is able to play a wide range of module formats, as well as
digital sound files. It can take advantage of particular features of your
system, such as sound redirection over the network.")
    (license lgpl2.1)
    (home-page "http://mikmod.sourceforge.net/")))

(define sdl-gfx
  (package
    (name "sdl-gfx")
    (version "2.0.24")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "http://www.ferzkopp.net/Software/SDL_gfx-2.0/SDL_gfx-"
                              version ".tar.gz"))
              (sha256
               (base32
                "064islldm4r42lgj9fr4kbk66r7jmmakk9745hhyb1kmw71kib9h"))))
    (build-system gnu-build-system)
    (propagated-inputs `(("sdl" ,sdl)))
    (synopsis "SDL graphics primitives library")
    (description "SDL_gfx provides graphics drawing primitives, rotozoom and
other supporting functions for SDL.")
    (home-page "http://www.ferzkopp.net/joomla/software-mainmenu-14/4-ferzkopps-linux-software/19-sdlgfx")
    (license zlib)))

(define sdl-image
  (package
    (name "sdl-image")
    (version "1.2.12")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "http://www.libsdl.org/projects/SDL_image/release/SDL_image-"
                             version ".tar.gz"))
             (sha256
              (base32
               "16an9slbb8ci7d89wakkmyfvp7c0cval8xw4hkg0842nhhlp540b"))))
    (build-system gnu-build-system)
    ;; FIXME: Add webp
    (inputs `(("libpng" ,libpng)
              ("libjpeg" ,libjpeg)
              ("libtiff" ,libtiff)
              ("pkg-config" ,pkg-config)))
    (propagated-inputs `(("sdl" ,sdl)))
    (synopsis "SDL image loading library")
    (description "SDL_image is an image file loading library for SDL that
supports the following formats: BMP, GIF, JPEG, LBM, PCX, PNG, PNM, TGA, TIFF,
WEBP, XCF, XPM, and XV.")
    (home-page "www.libsdl.org/projects/SDL_image/")
    (license zlib)))

(define sdl-mixer
  (package
    (name "sdl-mixer")
    (version "1.2.12")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "http://www.libsdl.org/projects/SDL_mixer/release/SDL_mixer-"
                              version ".tar.gz"))
              (sha256
               (base32
                "0alrhqgm40p4c92s26mimg9cm1y7rzr6m0p49687jxd9g6130i0n"))))
    (build-system gnu-build-system)
    ;; no check target
    ;; use libmad instead of smpeg
    (arguments `(#:tests? #f
                 #:configure-flags '("--enable-music-mp3-mad-gpl")))
    (inputs `(("libvorbis" ,libvorbis)
              ("libflac" ,flac)
              ("libmad" ,libmad)
              ("libmikmod" ,libmikmod)))
    ;; FIXME: Add libfluidsynth
    (propagated-inputs `(("sdl" ,sdl)))
    (synopsis "SDL multi-channel audio mixer library")
    (description "SDL_mixer is a multi-channel audio mixer library for SDL.
It supports any number of simultaneously playing channels of 16 bit stereo
audio, plus a single channel of music.  Supported format include FLAC, MOD,
MIDI, Ogg Vorbis, and MP3.")
    (home-page "www.libsdl.org/projects/SDL_mixer/")
    (license zlib)))

(define sdl-net
  (package
    (name "sdl-net")
    (version "1.2.8")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "http://www.libsdl.org/projects/SDL_net/release/SDL_net-"
                              version ".tar.gz"))
              (sha256
               (base32
                "1d5c9xqlf4s1c01gzv6cxmg0r621pq9kfgxcg3197xw4p25pljjz"))))
    (build-system gnu-build-system)
    (propagated-inputs `(("sdl" ,sdl)))
    (inputs `(("pkg-config" ,pkg-config)))
    (synopsis "SDL networking library")
    (description "SDL_net is a small, cross-platform networking library for
SDL.")
    (home-page "www.libsdl.org/projects/SDL_net/")
    (license zlib)))

(define sdl-ttf
  (package
    (name "sdl-ttf")
    (version "2.0.11")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "http://www.libsdl.org/projects/SDL_ttf/release/SDL_ttf-"
                             version ".tar.gz"))
             (sha256
              (base32
               "1dydxd4f5kb1288i5n5568kdk2q7f8mqjr7i7sd33nplxjaxhk3j"))))
    (build-system gnu-build-system)
    (propagated-inputs `(("sdl" ,sdl)))
    (inputs `(("freetype" ,font:freetype)
              ("mesa" ,mesa)
              ("pkg-config" ,pkg-config)))
    (synopsis "SDL TrueType font library")
    (description "SDL_ttf is a TrueType font rendering library for SDL.")
    (home-page "www.libsdl.org/projects/SDL_ttf/")
    (license zlib)))
