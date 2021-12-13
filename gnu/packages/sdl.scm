;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2017 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2017 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2015 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016, 2017, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2019, 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
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
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fcitx)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mono)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:export (sdl-union))

(define-public sdl
  (package
    (name "sdl")
    (version "1.2.15")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "https://libsdl.org/release/SDL-"
                             version ".tar.gz"))
             (sha256
              (base32
               "005d993xcac8236fpvd1iawkz4wqjybkpn8dbwaliqz5jfkidlyn"))
             (patches (search-patches "sdl-libx11-1.6.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(;; Explicitly link against shared libraries instead of dlopening them.
       ;; For X11, ALSA, and PulseAudio.
       ;; OpenGL library is still dlopened at runtime.
       #:configure-flags '("--disable-alsa-shared"
                           "--disable-pulseaudio-shared"
                           "--disable-x11-shared"
                           ;; Explicitly link with mesa.
                           ;; This add mesa to libsdl's RUNPATH, to make dlopen
                           ;; finding the libGL from mesa at runtime.
                           "LDFLAGS=-lGL")

       #:make-flags '("V=1")            ;build verbosely

       #:tests? #f)) ; no check target
    (propagated-inputs
     ;; SDL headers include X11 headers.
     (list libx11
           libcap ; 'libSDL.la' contain `-lcap'.
           ;; TODO: Since building Mesa with Meson it is now necessary that Mesa is
           ;; a propogated input. We still need to figure out why, possibly due to a
           ;; change in pkg-config.
           mesa))
    (native-inputs (list pkg-config))
    (inputs (list libxrandr glu alsa-lib pulseaudio))
    (outputs '("out" "debug"))
    (synopsis "Cross platform game development library")
    (description "Simple DirectMedia Layer is a cross-platform development
library designed to provide low level access to audio, keyboard, mouse,
joystick, and graphics hardware.")
    (home-page "https://libsdl.org/")
    (license license:lgpl2.1)))

(define-public sdl2
  (package (inherit sdl)
    (name "sdl2")
    (version "2.0.14")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "https://libsdl.org/release/SDL2-"
                             version ".tar.gz"))
             (sha256
              (base32
               "1g1jahknv5r4yhh1xq5sf0md20ybdw1zh1i15lry26sq39bmn8fq"))))
    (arguments
     (substitute-keyword-arguments (package-arguments sdl)
       ((#:configure-flags flags)
        `(append '("--disable-wayland-shared" "--enable-video-kmsdrm"
                   "--disable-kmsdrm-shared")
                 ,flags))
       ((#:make-flags flags ''())
        `(cons*
          ;; SDL dlopens libudev, so make sure it is in rpath. This overrides
          ;; the LDFLAG set in sdl’s configure-flags, which isn’t necessary
          ;; as sdl2 includes Mesa by default.
          (string-append "LDFLAGS=-Wl,-rpath,"
                         (assoc-ref %build-inputs "eudev") "/lib")
          ,flags))))
    (inputs
     ;; SDL2 needs to be built with ibus support otherwise some systems
     ;; experience a bug where input events are doubled.
     ;;
     ;; For more information, see: https://dev.solus-project.com/T1721
     (append `(("dbus" ,dbus)
               ("eudev" ,eudev) ; for discovering input devices
               ("fcitx" ,fcitx) ; helps with CJK input
               ("glib" ,glib)
               ("ibus" ,ibus)
               ("libxkbcommon" ,libxkbcommon)
               ("libxcursor" ,libxcursor) ; enables X11 cursor support
               ("wayland" ,wayland)
               ("wayland-protocols" ,wayland-protocols))
             (package-inputs sdl)))
    (license license:bsd-3)))

(define-public libmikmod
  (package
    (name "libmikmod")
    (version "3.3.11.1")
    (source (origin
             (method url-fetch)
             (uri (list
                   (string-append "mirror://sourceforge/mikmod/libmikmod/"
                                  version "/libmikmod-" version ".tar.gz")
                   ;; Older versions are sometimes moved to:
                   (string-append "mirror://sourceforge/mikmod/"
                                  "outdated_versions/libmikmod/"
                                  version "/libmikmod-" version ".tar.gz")))
             (sha256
              (base32
               "06bdnhb0l81srdzg6gn2v2ydhhaazza7rshrcj3q8dpqr3gn97dd"))))
    (build-system gnu-build-system)
    (arguments
     ;; By default, libmikmod tries to dlopen libasound etc., which won't work
     ;; unless the right libalsa happens to be in $LD_LIBRARY_PATH.  Pass
     ;; '--disable-dl' to avoid that.
     '(#:configure-flags '("--disable-dl")))
    (synopsis "Library for module sound formats")
    (description
     "MikMod is able to play a wide range of module formats, as well as
digital sound files.  It can take advantage of particular features of your
system, such as sound redirection over the network.")
    (license license:lgpl2.1)
    (home-page "http://mikmod.sourceforge.net/")))

(define-public sdl-gfx
  (package
    (name "sdl-gfx")
    (version "2.0.26")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "http://www.ferzkopp.net/Software/SDL_gfx-2.0/SDL_gfx-"
                              version ".tar.gz"))
              (sha256
               (base32
                "0ijljhs0v99dj6y27hc10z6qchyp8gdp4199y6jzngy6dzxlzsvw"))))
    (build-system gnu-build-system)
    (outputs '("out" "debug"))
    (arguments
     `(,@(if (any (cute string-prefix? <> (or (%current-system)
                                              (%current-target-system)))
                  '("x86_64" "i686"))
        ;; mmx is supported only on Intel processors.
        '()
        '(#:configure-flags '("--disable-mmx")))))
    (propagated-inputs (list sdl))
    (synopsis "SDL graphics primitives library")
    (description "SDL_gfx provides graphics drawing primitives, rotozoom and
other supporting functions for SDL.")
    (home-page "http://www.ferzkopp.net/joomla/software-mainmenu-14/4-ferzkopps-linux-software/19-sdlgfx")
    (license license:zlib)))

(define-public sdl-image
  (package
    (name "sdl-image")
    (version "1.2.12")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "https://www.libsdl.org/projects/SDL_image/release/SDL_image-"
                             version ".tar.gz"))
             (sha256
              (base32
               "16an9slbb8ci7d89wakkmyfvp7c0cval8xw4hkg0842nhhlp540b"))))
    (build-system gnu-build-system)
    (outputs '("out" "debug"))
    (arguments
     ;; Explicitly link against shared libraries instead of dlopening them.
     '(#:configure-flags '("--disable-jpg-shared"
                           "--disable-png-shared"
                           "--disable-tif-shared"
                           "--disable-webp-shared")))
    (native-inputs (list pkg-config))
    ;; libjpeg, libpng, and libtiff are propagated inputs because the
    ;; SDL_image headers include the headers of these libraries.  SDL is a
    ;; propagated input because the pkg-config file refers to SDL's pkg-config
    ;; file.
    (propagated-inputs `(("sdl" ,sdl)
                         ("libjpeg" ,libjpeg-turbo)
                         ("libpng" ,libpng)
                         ("libtiff" ,libtiff)
                         ("libwebp" ,libwebp)))
    (synopsis "SDL image loading library")
    (description "SDL_image is an image file loading library for SDL that
supports the following formats: BMP, GIF, JPEG, LBM, PCX, PNG, PNM, TGA, TIFF,
WEBP, XCF, XPM, and XV.")
    (home-page "https://www.libsdl.org/projects/SDL_image/")
    (license license:zlib)))

(define-public sdl-mixer
  (package
    (name "sdl-mixer")
    (version "1.2.12")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://www.libsdl.org/projects/SDL_mixer/release/SDL_mixer-"
                              version ".tar.gz"))
              (sha256
               (base32
                "0alrhqgm40p4c92s26mimg9cm1y7rzr6m0p49687jxd9g6130i0n"))))
    (build-system gnu-build-system)
    (outputs '("out" "debug"))
    (arguments
     `(#:tests? #f ; No check target.
       #:configure-flags
       '("--enable-music-mp3-mad-gpl" ; Use libmad instead of smpeg.
         ;; Explicitly link against shared libraries instead of dlopening them.
         "--disable-music-flac-shared"
         "--disable-music-fluidsynth-shared"
         "--disable-music-mod-shared"
         "--disable-music-ogg-shared")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-fluidsynth
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "configure"
              (("EXTRA_LDFLAGS -lfluidsynth")
               (string-append "EXTRA_LDFLAGS "
                              "-L"
                              (assoc-ref inputs "fluidsynth")
                              "/lib -lfluidsynth")))
             #t)))))
    (inputs
     `(("fluidsynth" ,fluidsynth)
       ("libflac" ,flac)
       ("libmad" ,libmad)
       ("libmikmod" ,libmikmod)
       ("libvorbis" ,libvorbis)))
    (propagated-inputs (list sdl))
    (synopsis "SDL multi-channel audio mixer library")
    (description "SDL_mixer is a multi-channel audio mixer library for SDL.
It supports any number of simultaneously playing channels of 16 bit stereo
audio, plus a single channel of music.  Supported formats include FLAC, MOD,
MIDI, Ogg Vorbis, and MP3.

This package supports two MIDI backends, selectable at runtime.  To use the
newer @code{fluidsynth} library, install a soundfont such as @code{fluid-3}
and specify it using the @code{SDL_SOUNDFONTS} environment variable.  For the
legacy @code{timidity} backend, install a patch set such as @code{freepats}
and set the path to the configuration file with @code{TIMIDITY_CFG}.")
    (home-page "https://www.libsdl.org/projects/SDL_mixer/")
    (license license:zlib)))

(define-public sdl-net
  (package
    (name "sdl-net")
    (version "1.2.8")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://www.libsdl.org/projects/SDL_net/release/SDL_net-"
                              version ".tar.gz"))
              (sha256
               (base32
                "1d5c9xqlf4s1c01gzv6cxmg0r621pq9kfgxcg3197xw4p25pljjz"))))
    (build-system gnu-build-system)
    (propagated-inputs (list sdl))
    (native-inputs (list pkg-config))
    (outputs '("out" "debug"))
    (synopsis "SDL networking library")
    (description "SDL_net is a small, cross-platform networking library for
SDL.")
    (home-page "https://www.libsdl.org/projects/SDL_net/")
    (license license:zlib)))

(define-public sdl-pango
  (package
    (name "sdl-pango")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/sdlpango/SDL_Pango/" version "/"
             "SDL_Pango-" version  ".tar.gz"))
       (sha256
        (base32 "197baw1dsg0p4pljs5k0fshbyki00r4l49m1drlpqw6ggawx6xbz"))
       (patches (search-patches "sdl-pango-api_additions.patch"
                                "sdl-pango-blit_overflow.patch"
                                "sdl-pango-fillrect_crash.patch"
                                "sdl-pango-header-guard.patch"
                                "sdl-pango-matrix_declarations.patch"
                                "sdl-pango-sans-serif.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autogen
           ;; Force reconfiguration because the included libtool
           ;; generates linking errors.
           (lambda _ (invoke "autoreconf" "-vif"))))))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glib" ,glib)
       ("harfbuzz" ,harfbuzz)
       ("pango" ,pango)
       ("sdl" ,sdl)))
    (home-page "http://sdlpango.sourceforge.net")
    (synopsis "Pango SDL binding")
    (description "This library is a wrapper around the Pango library.
It allows you to use TrueType fonts to render internationalized and
tagged text in SDL applications.")
    (license license:lgpl2.1)))

(define-public sdl-ttf
  (package
    (name "sdl-ttf")
    (version "2.0.11.1")
    ;; No release tarball for 2.0.11.1, changes:
    ;; <https://github.com/libsdl-org/SDL_ttf/commit/e31d11a692>
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libsdl-org/SDL_ttf")
                    (commit "e31d11a692e5b55e8e624ad766e4e44d655422c8")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1id1cdign615wd5rq0g4ppzwclvhkwd61yb5rwvvvakkpplp3lvd"))
              ;; Remove bundled libraries.
              (modules '((guix build utils)))
              (snippet '(delete-file-recursively "external"))))
    (build-system gnu-build-system)
    (propagated-inputs (list sdl))
    (inputs (list freetype mesa))
    (native-inputs (list pkg-config))
    (outputs '("out" "debug"))
    (synopsis "SDL TrueType font library")
    (description "SDL_ttf is a TrueType font rendering library for SDL.")
    (home-page "https://www.libsdl.org/projects/SDL_ttf/")
    (license license:zlib)))

(define* (sdl-union #:optional (packages (list sdl sdl-gfx sdl-net sdl-ttf
                                               sdl-image sdl-mixer)))
  "Return 'sdl-union' package which is a union of PACKAGES.
If PACKAGES are not specified, all SDL packages are used."
  (package
    (name "sdl-union")
    (version (package-version sdl))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
                   (use-modules (ice-9 match)
                                (guix build union))
                   (match %build-inputs
                     (((names . directories) ...)
                      (union-build (assoc-ref %outputs "out")
                                   directories)
                      #t)))))
    (inputs (map (lambda (package)
                   (list (package-name package) package))
                 packages))
    (synopsis "Union of SDL libraries")
    (description
     "A union of SDL and its extension libraries.  A union is required because
sdl-config assumes that all of the headers and libraries are in the same
directory.")
    (home-page (package-home-page sdl))
    (license (package-license sdl))))

(define (propagated-inputs-with-sdl2 package)
  "Replace the \"sdl\" propagated input of PACKAGE with SDL2."
  (map (match-lambda
         (("sdl" _)
          `("sdl2" ,sdl2))
         (other other))
       (package-propagated-inputs package)))

(define-public sdl2-gfx
  (package (inherit sdl-gfx)
    (name "sdl2-gfx")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://www.ferzkopp.net/Software/SDL2_gfx/SDL2_gfx-"
                              version ".tar.gz"))
              (sha256
               (base32
                "0qk2ax7f7grlxb13ba0ll3zlm8780s7j8fmrhlpxzjgdvldf1q33"))))
    (propagated-inputs
     (propagated-inputs-with-sdl2 sdl-gfx))))

(define-public sdl2-image
  (package (inherit sdl-image)
    (name "sdl2-image")
    (version "2.0.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.libsdl.org/projects/SDL_image/release/"
                       "SDL2_image-" version ".tar.gz"))
       (sha256
        (base32 "1l0864kas9cwpp2d32yxl81g98lx40dhbdp03dz7sbv84vhgdmdx"))))
    (propagated-inputs
     (propagated-inputs-with-sdl2 sdl-image))))

(define-public sdl2-mixer
  (package (inherit sdl-mixer)
    (name "sdl2-mixer")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "http://www.libsdl.org/projects/SDL_mixer/release/"
                       "SDL2_mixer-" version ".tar.gz"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Remove bundled libraries.
                   (delete-file-recursively "external")
                   #t))
       (sha256
        (base32 "0694vsz5bjkcdgfdra6x9fq8vpzrl8m6q96gh58df7065hw5mkxl"))))
    (arguments
      (substitute-keyword-arguments (package-arguments sdl-mixer)
         ((#:configure-flags flags)
          `(cons*
            "--disable-music-opus-shared"
            ;; These options were renamed in SDL2 mixer. Keeping the inherited
            ;; variants produces a harmless warning.
            "--disable-music-mod-modplug-shared"
            "--disable-music-midi-fluidsynth-shared"
            ,flags))))
    (inputs
     (modify-inputs (package-inputs sdl-mixer)
       (delete "libmikmod")
       (prepend opusfile
                ;; The default MOD library changed in SDL2 mixer.
                libmodplug)))
    (native-inputs
     `(("pkgconfig" ,pkg-config))) ; Needed to find the opus library.
    (propagated-inputs
     (propagated-inputs-with-sdl2 sdl-mixer))))

(define-public sdl2-net
  (package (inherit sdl-net)
    (name "sdl2-net")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "http://www.libsdl.org/projects/SDL_net/release/"
                       "SDL2_net-" version ".tar.gz"))
       (sha256
        (base32
         "08cxc1bicmyk89kiks7izw1rlx5ng5n6xpy8fy0zxni3b9z8mkhm"))))
    (propagated-inputs
     (propagated-inputs-with-sdl2 sdl-net))))

(define-public sdl2-ttf
  (package (inherit sdl-ttf)
    (name "sdl2-ttf")
    (version "2.0.15")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "https://www.libsdl.org/projects/SDL_ttf/release/SDL2_ttf-"
                             version ".tar.gz"))
             (modules '((guix build utils)))
             (snippet (begin
                        ;; Remove bundled libraries.
                        '(delete-file-recursively "external")
                        #t))
             (sha256
              (base32
               "0cyd48dipc0m399qy8s03lci8b0bpiy8xlkvrm2ia7wcv0dfpv59"))))
    (propagated-inputs
     (propagated-inputs-with-sdl2 sdl-ttf))))

(define-public guile-sdl
  (package
    (name "guile-sdl")
    (version "0.5.3")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://gnu/guile-sdl/guile-sdl-"
                              version ".tar.lz"))
              (sha256
               (base32
                "040gyk3n3yp8i30ngdg97n3083g8b6laky2nlh10jqcyjdd550d6"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("lzip" ,lzip)
       ("pkg-config" ,pkg-config)
       ;; Required by test suite.
       ("libjpeg" ,libjpeg-turbo)
       ("xorg-server" ,xorg-server)))
    (inputs
     (list guile-2.2
           (sdl-union)))
    (arguments
     '(#:configure-flags
       (list (string-append "--with-sdl-prefix="
                            (assoc-ref %build-inputs "sdl-union")))
       #:modules ((ice-9 popen)
                  (guix build utils)
                  (guix build gnu-build-system))

       #:parallel-build? #f ; parallel build fails

       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-env-and-patch
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "GUILE_AUTO_COMPILE" "0")
             ;; SDL_image needs to dlopen libjpeg in the test suite.
             (setenv "LD_LIBRARY_PATH"
                     (string-append (assoc-ref inputs "libjpeg") "/lib"))

             ;; Change the site directory /site/X.Y like Guile expects.
             (substitute* "build-aux/guile-baux/re-prefixed-site-dirs"
               (("\"/site\"")
                (let ((effective
                       (read
                        (open-pipe* OPEN_READ
                                    "guile" "-c"
                                    "(write (effective-version))"))))
                  (string-append "\"/site/" effective "\""))))

             ;; Skip tests that rely on sound support, which is unavailable in
             ;; the build environment.
             (substitute* "test/Makefile.in"
               (("HAVE_MIXER = .*$")
                "HAVE_MIXER = 0\n"))
             #t))
         (add-before 'check 'start-xorg-server
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The test suite requires a running X server.
             (system (format #f "~a/bin/Xvfb :1 &"
                             (assoc-ref inputs "xorg-server")))
             (setenv "DISPLAY" ":1")
             #t))
         (add-before 'check 'skip-cursor-test
           (lambda _
             ;; XXX: This test sometimes enters an endless loop, and sometimes
             ;; crashes with:
             ;;   guile: xcb_io.c:147: append_pending_request: Assertion `!xcb_xlib_unknown_seq_number' failed.
             ;; Skip it.
             (substitute* "test/cursor.scm"
               (("\\(SDL:init .*" all)
                (string-append "(exit 77)  ;" all "\n")))
             #t)))))
    (synopsis "Guile interface for SDL (Simple DirectMedia Layer)")
    (description "Guile-SDL is a set of bindings to the Simple DirectMedia
Layer (SDL).  With them, Guile programmers can have easy access to graphics,
sound and device input (keyboards, joysticks, mice, etc.).")
    (home-page "https://www.gnu.org/software/guile-sdl/")
    (license license:gpl3+)))

(define-public guile-sdl2
  (package
    (name "guile-sdl2")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.dthompson.us/guile-sdl2/"
                                  "guile-sdl2-" version ".tar.gz"))
              (sha256
               (base32
                "197dzkxw8nv92da56iv2r8ih5r3pr4pd5c5j2q83aqb78h4jqjl7"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags '("GUILE_AUTO_COMPILE=0")))
    (native-inputs
     (list guile-3.0 pkg-config))
    (inputs
     (list sdl2 sdl2-image sdl2-mixer sdl2-ttf))
    (synopsis "Guile bindings for SDL2")
    (home-page "https://dthompson.us/projects/guile-sdl2.html")
    (description
     "Guile-SDL2 provides Guile Scheme bindings for the SDL2 C shared library.
The bindings are written in pure Scheme using Guile's foreign function
interface.")
    (license license:lgpl3+)))

(define-public guile2.2-sdl2
  (package/inherit guile-sdl2
    (name "guile2.2-sdl2")
    (native-inputs
     `(("guile" ,guile-2.2)
       ("pkg-config" ,pkg-config)))))

(define-public guile3.0-sdl2
  (deprecated-package "guile3.0-sdl2" guile-sdl2))

(define-public sdl2-cs
  (let ((commit "1a3556441e1394eb0b5d46aeb514b8d1090b93f8"))
    (package
      (name "sdl2-cs")
      (version (git-version "B1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/flibitijibibo/SDL2-CS")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "007mzkqr9nmvfrvvhs2r6cm36lzgsww24kwshsz9c4fd97f9qk58"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f  ; No tests.
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda _
               (invoke "make" "release")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (install-file "bin/Release/SDL2-CS.dll" (string-append out "/lib"))
                 #t))))))
      (native-inputs
       (list mono))
      (inputs
       (list sdl2 sdl2-image sdl2-mixer sdl2-ttf))
      (home-page "https://dthompson.us/projects/guile-sdl2.html")
      (synopsis "C# wrapper for SDL2")
      (description
       "SDL2-CS provides C# bindings for the SDL2 C shared library.
The C# wrapper was written to be used for FNA's platform support.  However, this
is written in a way that can be used for any general C# application.")
      (license license:zlib))))
