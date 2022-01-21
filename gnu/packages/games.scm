;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 John Darrington <jmd@gnu.org>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2014 Cyrill Schenkel <cyrill.schenkel@gmail.com>
;;; Copyright © 2014 Sylvain Beucler <beuc@beuc.net>
;;; Copyright © 2014, 2015, 2018, 2019, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2014, 2015, 2019 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2015, 2017, 2018, 2021 Chris Lemmer Webber <cwebber@dustycloud.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2016, 2017 Rodger Fox <thylakoid@openmailbox.org>
;;; Copyright © 2016, 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2016 Albin Söderqvist <albin@fripost.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016 Steve Webber <webber.sl@gmail.com>
;;; Copyright © 2017 Adonay "adfeno" Felipe Nogueira <https://libreplanet.org/wiki/User:Adfeno> <adfeno@hyperbola.info>
;;; Copyright © 2017, 2018, 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2019 nee <nee-git@hidamari.blue>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2017-2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 okapi <okapi@firemail.cc>
;;; Copyright © 2018 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2018 Madalin Ionel-Patrascu <madalinionel.patrascu@mdc-berlin.de>
;;; Copyright © 2018 Benjamin Slade <slade@jnanam.net>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2019, 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019, 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019, 2020 Jesse Gibbons <jgibbons2357+guix@gmail.com>
;;; Copyright © 2019 Dan Frumin <dfrumin@cs.ru.nl>
;;; Copyright © 2019, 2020, 2021, 2022 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019, 2020 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2019 Josh Holland <josh@inv.alid.pw>
;;; Copyright © 2019 Pkill -9 <pkill9@runbox.com>
;;; Copyright © 2017, 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Alberto Eleuterio Flores Guerrero <barbanegra+guix@posteo.mx>
;;; Copyright © 2020 Naga Malleswari <nagamalli@riseup.net>
;;; Copyright © 2020 Vitaliy Shatrov <D0dyBo0D0dyBo0@protonmail.com>
;;; Copyright © 2020 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020, 2021 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Trevor Hass <thass@okstate.edu>
;;; Copyright © 2020, 2021 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2020 Lu hux <luhux@outlook.com>
;;; Copyright © 2020 Tomás Ortín Fernández <tomasortin@mailbox.org>
;;; Copyright © 2021 Olivier Rojon <o.rojon@posteo.net>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 David Pflug <david@pflug.io>
;;; Copyright © 2021, 2022 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021 Solene Rapenne <solene@perso.pw>
;;; Copyright © 2021 Noisytoot <noisytoot@disroot.org>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2021 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
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

(define-module (gnu packages games)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages code)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages emulators)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnu-doc)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gsasl)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages less)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages music)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages squirrel)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages unicode)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system scons)
  #:use-module (guix build-system trivial)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (srfi srfi-26))

(define-public abe
  (package
    (name "abe")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/abe/abe/abe-" version
                           "/abe-" version ".tar.gz"))
       (sha256
        (base32 "1xvpnq1y6y48fn3pvn2lk0h1ilmalv7nb7awpid1g4jcq1sfmi6z"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-data-dir="
                            (assoc-ref %outputs "out")
                            "/share/abe"))
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _ (invoke "sh" "autogen.sh")))
         (add-before 'build 'set-SDL
           ;; Set correct environment for SDL.
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append
                      (assoc-ref inputs "sdl") "/include/SDL:"
                      (or (getenv "CPATH") "")))
             #t))
         (add-after 'install 'finalize-install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((share (string-append (assoc-ref outputs "out") "/share")))
               ;; Installation script does not copy game data files.
               (let ((data (string-append share "/abe")))
                 (for-each (lambda (dir)
                             (let ((target (string-append data "/" dir)))
                               (mkdir-p target)
                               (copy-recursively dir target)))
                           '("images" "maps" "sounds")))
               ;; Create desktop file.
               (let ((apps (string-append share "/applications")))
                 (mkdir-p apps)
                 (make-desktop-entry-file
                  (string-append apps "/abe.desktop")
                  #:name "Abe's Amazing Adventure"
                  #:exec ,name
                  #:categories '("AdventureGame" "Game")
                  #:keywords
                  '("side-scrolling" "adventure" "pyramid" "singleplayer")
                  #:comment
                  '(("de" "Ein sich seitwärts bewegendes Abenteuerspiel")
                    (#f "Side-scrolling game")))))
             #t)))))
    (native-inputs
     (list autoconf automake))
    (inputs
     `(("libxi" ,libxi)
       ("libxmu" ,libxmu)
       ("libxt" ,libxt)
       ("sdl" ,(sdl-union (list sdl sdl-mixer)))))
    (home-page "http://abe.sourceforge.net")
    (synopsis "Scrolling, platform-jumping, ancient pyramid exploring game")
    (description
     "Abe's Amazing Adventure is a scrolling,
platform-jumping, key-collecting, ancient pyramid exploring game, vaguely in
the style of similar games for the Commodore+4.")
    (license license:gpl2+)))

;; Data package for adanaxisgpl.
(define adanaxis-mush
  (let ((version "1.1.0"))
    (origin
      (method url-fetch)
      (uri (string-append "http://www.mushware.com/files/adanaxis-mush-"
                          version ".tar.gz"))
      (sha256
       (base32 "0mk9ibis5nkdcalcg1lkgnsdxxbw4g5w2i3icjzy667hqirsng03")))))

(define-public adanaxisgpl
  (package
    (name "adanaxisgpl")
    (version "1.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.mushware.com/files/adanaxisgpl-"
                           version ".tar.gz"))
       (sha256
        (base32 "0jkn637jaabvlhd6hpvzb57vvjph94l6fbf7qxbjlw9zpr19dw1f"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Necessary for building with gcc >=4.7.
           (substitute* "src/Mushcore/MushcoreSingleton.h"
             (("SingletonPtrSet\\(new SingletonType\\);")
              "MushcoreSingleton::SingletonPtrSet(new SingletonType);"))
           ;; Avoid an "invalid conversion from const char* to char*" error.
           (substitute* "src/Platform/X11/PlatformMiscUtils.cpp"
             (("char \\*end, \\*result;")
              (string-append "const char *end;"
                             "\n"
                             "char *result;")))
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-data
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((data (assoc-ref inputs "adanaxis-mush"))
                   (share (string-append (assoc-ref outputs "out")
                                         "/share/" ,name "-" ,version)))
               (mkdir-p share)
               (invoke "tar" "xvf" data "-C" share)))))))
    (native-inputs
     `(("adanaxis-mush" ,adanaxis-mush))) ; game data
    (inputs
     `(("expat" ,expat)
       ("freeglut" ,freeglut)
       ("glu" ,glu)
       ("libjpeg" ,libjpeg-turbo)
       ("libogg" ,libogg)
       ("libtiff" ,libtiff)
       ("libvorbis" ,libvorbis)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("pcre" ,pcre)
       ("sdl" ,sdl)
       ("sdl-mixer" ,sdl-mixer)))
    (home-page "https://www.mushware.com")
    (synopsis "Action game in four spatial dimensions")
    (description
     "Adanaxis is a fast-moving first person shooter set in deep space, where
the fundamentals of space itself are changed.  By adding another dimension to
space this game provides an environment with movement in four directions and
six planes of rotation.  Initially the game explains the 4D control system via
a graphical sequence, before moving on to 30 levels of gameplay with numerous
enemy, ally, weapon and mission types.  Features include simulated 4D texturing,
mouse and joystick control, and original music.")
    (license license:gpl2)))

(define-public alex4
  (package
    (name "alex4")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/carstene1ns/alex4")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "098wy72mh4lsvq3gm0rhamjssf9l1hp6hhkpzrv7klpb97cwwc3h"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:make-flags
       (list "CC=gcc"
             "CFLAGS=-D_FILE_OFFSET_BITS=64"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'install 'install-data
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((share (string-append (assoc-ref outputs "out")
                                         "/share/" ,name)))
               (install-file "alex4.ini" share)
               #t))))))
    (inputs
     `(("allegro" ,allegro-4)
       ("dumb" ,dumb-allegro4)))
    (home-page "http://allegator.sourceforge.net/")
    (synopsis "Retro platform game")
    (description
     "Guide Alex the Allegator through the jungle in order to save his
girlfriend Lola from evil humans who want to make a pair of shoes out of her.
Plenty of classic platforming in four nice colors guaranteed!

The game includes a built-in editor so you can design and share your own maps.")
    (license license:gpl2+)))

(define-public armagetronad
  (package
    (name "armagetronad")
    (version "0.2.9.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/armagetronad/stable/"
                                  version "/armagetronad-" version ".tbz"))
              (sha256
               (base32
                "18gn4sg4j5sw38ngb90sl50raliplrsgjcvy8fjwry733k0cgdjr"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libxml2
           sdl
           sdl-image
           freeglut
           libpng
           libjpeg-turbo))
    (home-page "http://www.armagetronad.org")
    (synopsis "Tron clone in 3D")
    (description "Armagetron Advanced is a multiplayer game in 3d that
attempts to emulate and expand on the lightcycle sequence from the movie Tron.
It's an old school arcade game slung into the 21st century.  Highlights
include a customizable playing arena, HUD, unique graphics, and AI bots.  For
the more advanced player there are new game modes and a wide variety of
physics settings to tweak as well.")
    (license license:gpl2+)))

(define-public astromenace
  (package
    (name "astromenace")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/viewizard/astromenace")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ad6l887jxqv8xspwc2rvy8ym9sdlmkqdqhsh0pi076kjarxsyws"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:configure-flags (list (string-append "-DDATADIR="
                                              (assoc-ref %outputs "out")
                                              "/share/astromenace"))
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           ;; Upstream provides no install phase.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share (string-append out "/share"))
                    (apps (string-append share "/applications"))
                    (data (string-append share "/astromenace"))
                    (icons (string-append share "/icons/hicolor/64x64/apps")))
               (install-file "astromenace" bin)
               (install-file "gamedata.vfs" data)
               (let ((source (assoc-ref inputs "source")))
                 (with-directory-excursion (string-append source "/share")
                   (install-file "astromenace.desktop" apps)
                   (mkdir-p icons)
                   (copy-file "astromenace_64.png"
                              (string-append icons "/astromenace.png")))))
             #t)))))
    (inputs
     (list freealut
           freetype
           glu
           libogg
           libvorbis
           openal
           sdl2))
    (home-page "https://www.viewizard.com/")
    (synopsis "3D space shooter with spaceship upgrade possibilities")
    (description
     "Space is a vast area, an unbounded territory where it seems there is
a room for everybody, but reversal of fortune put things differently.  The
hordes of hostile creatures crawled out from the dark corners of the universe,
craving to conquer your homeland.  Their force is compelling, their legions
are interminable.  However, humans didn't give up without a final showdown and
put their best pilot to fight back.  These malicious invaders chose the wrong
galaxy to conquer and you are to prove it!  Go ahead and make alien aggressors
regret their insolence.")
    ;; Game is released under GPL3+ terms.  Artwork is subject to CC
    ;; BY-SA 4.0, and fonts to OFL1.1.
    (license (list license:gpl3+ license:cc-by-sa4.0 license:silofl1.1))))

(define-public bastet
  (package
    (name "bastet")
    (version "0.43.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fph/bastet")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09kamxapm9jw9przpsgjfg33n9k94bccv65w95dakj0br33a75wn"))
       (patches
        (search-patches "bastet-change-source-of-unordered_set.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CXXFLAGS=-I"
                            (assoc-ref %build-inputs "boost") "/include"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'check
           ;; The 'Test' target builds the tests, but doesn't actually run them.
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "Test" make-flags)
             (setenv "HOME" ".")
             (invoke "./Test")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (share   (string-append out "/share"))
                    (hicolor (string-append share "/icons/hicolor")))
               (install-file "bastet"
                             (string-append out "/bin"))

               (install-file "bastet.desktop"
                             (string-append share "/applications"))
               (install-file "bastet.svg"
                             (string-append hicolor "/scalable/apps"))

               (install-file "bastet.appdata.xml"
                             (string-append share "/appdata"))

               (install-file "bastet.6"
                             (string-append out "/share/man/man6"))
               #t))))))
    (native-inputs
     (list hicolor-icon-theme))
    (inputs
     (list boost ncurses))
    (home-page "https://fph.altervista.org/prog/bastet.html")
    (synopsis "Antagonistic Tetris-style falling brick game for text terminals")
    (description
     "Bastet (short for Bastard Tetris) is a simple ncurses-based falling brick
game.  Unlike normal Tetris, Bastet does not choose the next brick at random.
Instead, it uses a special algorithm to choose the worst brick possible.

Playing bastet can be a painful experience, especially if you usually make
canyons and wait for the long I-shaped block to clear four rows at a time.")
    (license license:gpl3+)))

(define-public tetrinet
  (package
    (name "tetrinet")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://tetrinet.or.cz/download/tetrinet-" version
             ".tar.bz2"))
       (sha256
        (base32
         "0b4pddqz6is1771qmvcj8qqlr4in2djdbkk13agvp9yhfah2v8x7"))))
    (build-system gnu-build-system)
    (inputs
     (list ncurses))
    (arguments
     `(#:tests? #f                      ;no tests
       #:make-flags '("CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ;no configure script
         (add-after 'unpack 'fix-install-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/bin"))
               (substitute* "Makefile"
                 (("/usr/games") (string-append out "/bin"))))))
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (for-each (lambda (file)
                           (install-file file doc))
                         (list "README" "tetrinet.txt"))))))))
    (home-page "http://tetrinet.or.cz")
    (synopsis "Terminal-based multiplayer Tetris clone")
    (description "Tetrinet is a multiplayer Tetris-like game with powerups and
attacks you can use on opponents.")
    (license license:public-domain)))

(define-public vitetris
  (package
    (name "vitetris")
    (version "0.59.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vicgeralds/vitetris")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1ah1c5g7abksif0n8v5rb7r4pn2az20c3mkp4ak13vgs23ddmds5"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:make-flags
       (list ,(string-append "CC=" (cc-for-target))
             (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             ;; the non standard configure script does not accept
             ;; standard parameters -> invoke configure by hand
             (invoke "./configure" "prefix=")
             ;; src/src-conf.mk must be writable for the build step
             (make-file-writable "src/src-conf.mk"))))))
    (home-page "http://victornils.net/tetris/")
    (synopsis "Terminal-based Tetris clone")
    (description "Vitetris is a classic multiplayer Tetris clone for the
terminal.")
    (license license:bsd-2)))

(define-public blobwars
  (package
    (name "blobwars")
    (version "2.00")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/blobwars/"
                           "blobwars-" version ".tar.gz"))
       (sha256
        (base32 "16aagvkx6azf75gm5kaa94bh5npydvhqp3fvdqyfsanzdjgjf1n4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "PREFIX=" out)
               (string-append "BINDIR=" out "/bin/")
               "USEPAK=1"
               "RELEASE=1"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'werror-begone
           (lambda _
             (substitute* "Makefile" (("-Werror") ""))
             #t))
         (delete 'configure))))         ;no configure script
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("hicolor-icon-theme" ,hicolor-icon-theme)
       ("sdl" ,(sdl-union (list sdl2
                                sdl2-image
                                sdl2-mixer
                                sdl2-ttf
                                sdl2-net)))))
    (home-page "https://sourceforge.net/projects/blobwars/")
    (synopsis "Platform action game featuring a blob with a lot of weapons")
    (description "Blobwars: Metal Blob Solid is a 2D platform game, the first
in the Blobwars series.  You take on the role of a fearless Blob agent.  Your
mission is to infiltrate various enemy bases and rescue as many MIAs as
possible, while battling many vicious aliens.")
    (license (list license:gpl2      ; For code and graphics
                   license:cc0       ; Music and sounds have specific licenses
                   license:cc-by3.0  ; see /doc/readme
                   license:cc-by-sa3.0
                   license:lgpl2.1+
                   license:bsd-2))))

(define-public bsd-games
  (package
    (name "bsd-games")
    (version "2.17.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://ibiblio.org/pub/linux/games/bsd-games-2.17.tar.gz")
       (sha256
        (base32 "0q7zdyyfvn15y0w4g54kq3gza89h61py727m8slmw73cxx594vq6"))
       (patches
        (search-patches
         ;; thanks Arch, and Debian
         "bsd-games-2.17-64bit.patch"
         "bsd-games-bad-ntohl-cast.patch"
         "bsd-games-gamescreen.h.patch"
         "bsd-games-getline.patch"
         "bsd-games-null-check.patch"
         "bsd-games-number.c-and-test.patch"
         "bsd-games-stdio.h.patch"
         "bsd-games-prevent-name-collisions.patch"
         ;; Guix customizations
         "bsd-games-add-configure-config.patch"
         "bsd-games-dont-install-empty-files.patch"
         "bsd-games-add-wrapper.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list flex bison))
    (inputs
     `(("curses" ,ncurses)
       ("pager" ,less)
       ("miscfiles" ,miscfiles)
       ("openssl" ,openssl)))           ;used only by 'factor'
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/bsd-games-" ,version))
                    (man (string-append out "/share/man"))
                    (word-list (search-input-file inputs "/share/web2"))
                    (static-data (string-append out "/share/games/bsd-games"))
                    ;; Not a "./" because of substitute* in 'patch-install
                    ;; below.  The .// allow us not to mess with the games'
                    ;; code any further: we just use a wrapper script that
                    ;; cd's to a BSD_GAMES_DIR.  :]
                    (save-files ".//"))
               (substitute* "configure"
                 (("/usr/share/man") man)
                 (("/usr/share/doc/bsd-games") doc)
                 (("/usr/share/[^\n/]*") static-data)
                 (("/var/games") save-files)
                 (("/usr/bin/less") (which "less"))
                 (("(/usr/bin|/usr/games)") bin))
               (substitute* "config.params" (("WORD_LIST") word-list))
               (substitute* "wrapper" (("STATIC_DATA") static-data))
               (invoke "./configure"))
             #t))
         (add-before 'install 'patch-install
           ;; Some games need a writable directory containing pre-maded files.
           ;; The files get installed to the Store.  Then the wrapper kicks in.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (static-data (string-append out "/share/games/bsd-games"))
                    (save-files ".//"))
               (substitute* "Makeconfig" ((save-files) static-data)))
             #t))
         (add-after 'install 'install-documents
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/bsd-games-" ,version)))
               (rename-file "phantasia/COPYRIGHT" "phantasia-COPYRIGHT")
               (for-each
                (lambda(file) (install-file file doc))
                '("AUTHORS" "BUGS" "README" "SECURITY" "THANKS"
                  "phantasia-COPYRIGHT")))
             #t)))))
    (home-page "https://github.com/vattam/BSDGames")
    (synopsis "Collection of the old text-based games and amusements")
    (description
     "These are the BSD games.  See the fortune-mod package for fortunes.

Action: atc (keep the airplanes safe), hack (explore the dangerous Dungeon),
hunt (kill the others for the Pair of Boots, multi-player only), robots (avoid
the evil robots), sail (game of naval warfare with wooden ships), snake (steal
the $$ from the cave, anger the snake, and get out alive), tetris (game of
lining up the falling bricks of different shapes), and worm (eat, grow big,
and neither bite your tail, nor ram the wall).

Amusements: banner (prints a large banner), bcd & morse & ppt (print a punch
card, or paper tape, or Morse codes), caesar & rot13 (ciphers and deciphers
the input), factor (factorizes a number), number (translates numbers into
text), pig (translates from English to Pig Latin), pom (should print the
Moon's phase), primes (generates primes), rain & worms (plays an screen-saver
in terminal), random (prints randomly chosen lines from files, or returns a
random exit-code), and wtf (explains what do some acronyms mean).

Board: backgammon (lead the men out of board faster than the friend do),
boggle (find the words in the square of letters), dab (game of dots and
boxes), gomoku (game of five in a row), hangman (guess a word before man is
hanged), and monop (game of monopoly, hot-seat only).  Also the card-games:
canfield, cribbage, fish (juniors game), and mille.

Quests: adventure (search for treasures with the help of wizard),
battlestar (explore the world around, starting from dying spaceship),
phantasia (role-play as an rogue), trek (hunt the Klingons, and save the
Federation), and wump (hunt the big smelly Wumpus in a dark cave).

Quizzes: arithmetic and quiz.")
    ;; "Auxiliary and data files, distributed with the games in NetBSD, but
    ;; not bearing copyright notices, probably fall under the terms of the UCB
    ;; or NetBSD copyrights and licences.  The file "fortune/Notes" contains a
    ;; warning in regard to the fortune databases."
    (license (list
              ;; Most games.  Files: countmail/countmail.6, dab/dab.6,
              ;; lib/strlcpy.c, wargames/wargames.6
              license:bsd-3
              ;; dab and hunt.  Files: adventure/extern.h,
              ;; backgammon/backgammon/backlocal.h, caesar/rot13.in,
              ;; countmail/countmail, dm/utmpentry.c, dm/utmpentry.h,
              ;; hack/extern.h, robots/auto.c, sail/display.h,
              ;; sail/restart.h, wargames/wargames
              license:bsd-4
              ;; wtf (the game)
              license:public-domain
              ;; phantasia (all but phantasia/pathnames.h.in, which is bsd-3)
              (license:fsf-free "file:///phantasia/COPYRIGHT")))))


(define-public bzflag
  (package
    (name "bzflag")
    (version "2.4.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.bzflag.org/bzflag/source/"
                           version "/bzflag-" version ".tar.bz2"))
       (sha256
        (base32 "0kba0011nswc2csqlzkd7bas307zm5813zlnby5vsmxn08rnar4y"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-desktop-file-and-icons
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((share (string-append (assoc-ref outputs "out") "/share"))
                    (data (string-append share "/bzflag"))
                    (hicolor (string-append share "/icons/hicolor"))
                    (applications (string-append share "/applications")))
               ;; Move desktop file.
               (install-file (string-append data "/bzflag.desktop")
                             applications)
               ;; Install icons.
               (for-each (lambda (size)
                           (let* ((dim (string-append size "x" size))
                                  (dir (string-append hicolor "/" dim "/apps")))
                             (mkdir-p dir)
                             (copy-file
                              (string-append data "/bzflag-" dim ".png")
                              (string-append dir "/bzflag.png"))))
                         '("48" "256")))
             #t)))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list c-ares
           curl
           glew
           glu
           sdl2
           zlib))
    (home-page "https://www.bzflag.org/")
    (synopsis "3D first person tank battle game")
    (description
     "BZFlag is a 3D multi-player multiplatform tank battle game that
allows users to play against each other in a network environment.
There are five teams: red, green, blue, purple and rogue (rogue tanks
are black).  Destroying a player on another team scores a win, while
being destroyed or destroying a teammate scores a loss.  Rogues have
no teammates (not even other rogues), so they cannot shoot teammates
and they do not have a team score.

There are two main styles of play: capture-the-flag and free-for-all.
In capture-the-flag, each team (except rogues) has a team base and
each team with at least one player has a team flag.  The object is to
capture an enemy team's flag by bringing it to your team's base.  This
destroys every player on the captured team, subtracts one from that
team's score, and adds one to your team's score.  In free-for-all,
there are no team flags or team bases.  The object is simply to get as
high a score as possible.")
    ;; The game is dual-licensed.
    (license (list license:lgpl2.1 license:mpl2.0))))

(define-public cataclysm-dda
  (package
    (name "cataclysm-dda")
    (version "0.F-3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/CleverRaven/Cataclysm-DDA")
             (commit version)))
       (sha256
        (base32 "1qnsz6az9qp4sbr3y4rcqhlmadrrdzafvd2xwf3db5wn0swvbjys"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "USE_HOME_DIR=1" "DYNAMIC_LINKING=1" "RELEASE=1"
             "LOCALIZE=1" "LANGUAGES=all")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; Apparently we can't do make on both tiles and a console version at
         ;; the same time anymore, so we have to either "make clean" between
         ;; builds or do some other hackery.  See:
         ;;   https://github.com/CleverRaven/Cataclysm-DDA/issues/42598#issuecomment-667702746
         (add-after 'install 'make-clean-pre-tiles
           (lambda* (#:key make-flags outputs #:allow-other-keys)
             ;; Change prefix directory and enable tile graphics and sound.
             (invoke "make" "clean")))
         (add-after 'make-clean-pre-tiles 'build-tiles
           (lambda* (#:key make-flags outputs #:allow-other-keys)
             ;; Change prefix directory and enable tile graphics and sound.
             (apply invoke "make" "TILES=1" "SOUND=1"
                    (string-append "PREFIX="
                                   (assoc-ref outputs "tiles"))
                    (cdr make-flags))))
         (add-after 'build-tiles 'install-tiles
           (lambda* (#:key make-flags outputs #:allow-other-keys)
             (apply invoke "make" "install" "TILES=1" "SOUND=1"
                    (string-append "PREFIX="
                                   (assoc-ref outputs "tiles"))
                    (cdr make-flags)))))
       ;; TODO: Add libtap++ from https://github.com/cbab/libtappp as a native
       ;;       input in order to support tests.
       #:tests? #f))
    (outputs '("out"
               "tiles"))                ;for tile graphics and sound support
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("astyle" ,astyle)))
    (inputs
     (list freetype
           libogg
           libvorbis
           ncurses
           sdl2
           sdl2-image
           sdl2-ttf
           sdl2-mixer))
    (home-page "https://cataclysmdda.org/")
    (synopsis "Survival horror roguelike video game")
    (description
     "Cataclysm: Dark Days Ahead (or \"DDA\" for short) is a roguelike set
in a post-apocalyptic world.  Struggle to survive in a harsh, persistent,
procedurally generated world.  Scavenge the remnants of a dead civilization
for food, equipment, or, if you are lucky, a vehicle with a full tank of gas
to get you out of Dodge.  Fight to defeat or escape from a wide variety of
powerful monstrosities, from zombies to giant insects to killer robots and
things far stranger and deadlier, and against the others like yourself, that
want what you have.")
    (license license:cc-by-sa3.0)))

(define-public cockatrice
  (let ((release-date "2021-01-26"))
    (package
      (name "cockatrice")
      (version "2.8.0")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Cockatrice/Cockatrice")
               (commit (string-append release-date "-Release-" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0q8ffcklb2b7hcqhy3d2f9kz9aw22pp04pc9y4sslyqmf17pwnz9"))
         (modules '((guix build utils)))
         (snippet
          ;; Strip image URLs as they point towards non-free web services
          '(substitute* "cockatrice/src/settings/downloadsettings.cpp"
             (("downloadURLs.append\\(\".*\"\\);") "")))))
      (build-system qt-build-system)
      (arguments
       `(#:configure-flags '("-DWITH_SERVER=1"
                             "-DWITH_CLIENT=1"
                             "-DWITH_ORACLE=1"
                             "-DTEST=1")))
      (native-inputs
       (list googletest pkg-config))
      (inputs
       (list protobuf
             qtbase-5
             qtmultimedia
             qtsvg
             qttools
             qtwebsockets
             xz
             zlib))
      (home-page "https://cockatrice.github.io")
      (synopsis "Tabletop card game simulator")
      (description "Cockatrice is a program for playing tabletop card games
over a network.  Its server design prevents users from manipulating the game
for unfair advantage.  The client also provides a single-player mode, which
allows users to brew while offline.")
      (license license:gpl2))))

(define-public corsix-th
  (package
    (name "corsix-th")
    (version "0.65.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/CorsixTH/CorsixTH")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hw92ln9jm9v55drmbfqjng58yshgwfpv7fqynryrg3gvg8zhbvh"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-binary
           (lambda _
             ;; Set Lua module paths and default MIDI soundfont on startup.
             (let* ((out (assoc-ref %outputs "out"))
                    (fluid (assoc-ref %build-inputs "fluid-3"))
                    (lua-version ,(version-major+minor (package-version lua)))
                    (lua-cpath
                     (map (lambda (lib)
                            (string-append
                             (assoc-ref %build-inputs (string-append "lua-" lib))
                             "/lib/lua/" lua-version "/?.so"))
                          '("filesystem" "lpeg"))))
               (wrap-program (string-append out "/bin/corsix-th")
                 `("LUA_CPATH" ";" = ,lua-cpath)
                 `("SDL_SOUNDFONTS" ":" suffix
                   (,(string-append fluid "/share/soundfonts/FluidR3Mono_GM.sf3")))))
             #t)))
       #:tests? #f)) ; TODO need busted package to run tests
    ;; Omit Lua-Socket dependency to disable automatic updates.
    (inputs
     (list ffmpeg
           fluid-3
           freetype
           lua
           lua-filesystem
           lua-lpeg
           sdl2
           sdl2-mixer))
    (home-page "https://corsixth.com")
    (synopsis "Implementation of the @i{Theme Hospital} game engine")
    (description
     "This package provides a reimplementation of the 1997 Bullfrog business
simulation game @i{Theme Hospital}.  As well as faithfully recreating the
original engine, CorsixTH adds support for high resolutions, custom levels and
more.  This package does @emph{not} provide the game assets.")
    (license (list
              license:expat ; main license
              license:bsd-3)))) ; CorsixTH/Src/random.c

(define-public cowsay
  (package
    (name "cowsay")
    (version "3.04")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/tnalpgge/rank-amateur-cowsay")
                     (commit (string-append name "-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06455kq37hvq1xb7adyiwrx0djs50arsxvjgixyxks16lm1rlc7n"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (delete 'build)                ; nothing to be built
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "sh" "install.sh"
                     (assoc-ref outputs "out"))))
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke (string-append (assoc-ref outputs "out")
                                    "/bin/cowsay")
                     "We're done!"))))))
    (inputs
     (list perl))
    (home-page (string-append "https://web.archive.org/web/20071026043648/"
                              "http://www.nog.net:80/~tony/warez/cowsay.shtml"))
    (synopsis "Speaking cow text filter")
    (description "Cowsay is basically a text filter.  Send some text into it,
and you get a cow saying your text.  If you think a talking cow isn't enough,
cows can think too: all you have to do is run @command{cowthink}.  If you're
tired of cows, a variety of other ASCII-art messengers are available.")
    (license license:gpl3+)))

(define-public lolcat
  (let ((commit "35dca3d0a381496d7195cd78f5b24aa7b62f2154")
        (revision "0"))
    (package
      (name "lolcat")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jaseg/lolcat")
               (commit commit)))
         (sha256
          (base32
           "0jjbkqcc2ikjxd1xgdyv4rb0vsw218181h89f2ywg29ffs3ypd8g"))
         (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; no check target
         #:make-flags
         (list ,(string-append "CC=" (cc-for-target)))
         #:phases
         (modify-phases %standard-phases
           (delete 'bootstrap)
           (delete 'configure)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out  (assoc-ref outputs "out"))
                      (dest (string-append out "/bin")))
                 (mkdir-p dest)
                 (install-file "lolcat" dest)
                 (install-file "censor" dest)
                 #t))))))
      (home-page "https://github.com/jaseg/lolcat")
      (synopsis "Rainbow coloring effect for text console display")
      (description "@command{lolcat} concatenates files and streams like
regular @command{cat}, but it also adds terminal escape codes between
characters and lines resulting in a rainbow effect.")
      (license license:wtfpl2))))

(define-public falltergeist
  (package
    (name "falltergeist")
    (version "0.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/falltergeist/falltergeist")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05cg58i2g32wbmrvmdsicic8xs83gld3qr1p7r4lnlckcl1l7dy4"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ; no tests provided
    (native-inputs (list pkg-config))
    (inputs `(("sdl" ,(sdl-union (list sdl2
                                       sdl2-image
                                       sdl2-mixer)))
              ("glew" ,glew)
              ("glm" ,glm)))
    (home-page "https://falltergeist.org/")
    (synopsis "Fallout 2 game engine")
    (description "This package provides the Fallout 2 game engine.  Game data
should be placed in @file{~/.local/share/falltergeist}.")
    (license license:gpl3+)))

(define-public foobillard++
  ;; Even though this latest revision is old already, stable release is
  ;; lagging way behind it, and has issues with textures rendering.
  (let ((svn-revision 170))
    (package
      (name "foobillard++")
      (version (string-append "3.43-r" (number->string svn-revision)))
      (source
       (origin
         (method svn-fetch)
         (uri (svn-reference
               (url "svn://svn.code.sf.net/p/foobillardplus/code/")
               (revision svn-revision)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "00b693ys5zvzjbjzzj3dqfzm5xw64gwjf9m8qv6bkmf0klbhmayk"))
         (patches
          (search-patches "foobillard++-pkg-config.patch"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; Unfortunately, the game includes background music with
             ;; a non-commercial clause.  Delete it.
             (for-each delete-file (find-files "data/music" "\\.ogg$"))
             #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags
         (list
          ;; Install data in a less exotic location.
          (string-append "--prefix=" (assoc-ref %outputs "out") "/share")
          ;; Prevent a build error about undefined trigonometric functions.
          "--enable-fastmath=no")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-makefile
             ;; Remove hard-coded directories.  Also fix installation
             ;; rule: it tries to move around non-existent files or
             ;; files already moved.
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "Makefile.am"
                 (("/usr") (assoc-ref outputs "out"))
                 (("cp .*?/foobillardplus\\.desktop.*") "")
                 (("cp .*?/foobillardplus\\.(png|xbm) \\$\\(datarootdir\\).*")
                  ""))
               #t))
           (add-after 'unpack 'unbundle-font
             ;; XXX: The package ships with LinBiolinum_aSB.ttf and
             ;; LinBiolinum_aS.ttf, which are not provided by
             ;; `font-linuxlibertine' package.  Therefore, we cannot replace
             ;; them yet.
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((dejavu (string-append (assoc-ref inputs "font-dejavu")
                                            "/share/fonts/truetype/")))
                 (with-directory-excursion "data"
                   (for-each (lambda (f)
                               (delete-file f)
                               (symlink (string-append dejavu f) f))
                             '("DejaVuSans-Bold.ttf" "DejaVuSans.ttf"))))
               #t))
           (replace 'bootstrap
             (lambda _
               (invoke "aclocal" "--force")
               (invoke "autoconf" "-f")
               (invoke "autoheader" "-f")
               (invoke "automake" "-a" "-c" "-f")))
           (add-before 'build 'prepare-build
             ;; Set correct environment for SDL.
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "CPATH"
                       (string-append
                        (search-input-directory inputs "include/SDL")
                        ":" (or (getenv "CPATH") "")))))
           (add-before 'build 'fix-settings-directory
             ;; Hide foobillardplus settings directory in $HOME.
             (lambda _
               (substitute* "src/history.c"
                 (("/foobillardplus-data") "/.foobillardplus"))
               #t))
           (add-before 'install 'create-directories
             ;; Install process does not create directories before
             ;; trying to move file in it.
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (mkdir-p (string-append out "/share/icons"))
                 (mkdir-p (string-append out "/share/applications")))
               #t))
           (add-after 'install 'symlink-executable
             ;; Symlink executable to $out/bin.
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (mkdir-p bin)
                 (with-directory-excursion bin
                   (symlink "../share/foobillardplus/bin/foobillardplus"
                            "foobillardplus"))
                 #t))))))
      (native-inputs
       (list autoconf automake pkg-config))
      (inputs
       `(("font-dejavu" ,font-dejavu)
         ("freetype" ,freetype)
         ("glu" ,glu)
         ("libpng" ,libpng)
         ("sdl" ,(sdl-union (list sdl sdl-mixer sdl-net)))))
      (home-page "http://foobillardplus.sourceforge.net/")
      (synopsis "3D billiard game")
      (description "FooBillard++ is an advanced 3D OpenGL billiard game
based on the original foobillard 3.0a sources from Florian Berger.
You can play it with one or two players or against the computer.

The game features:

@itemize
@item Wood paneled table with gold covers and gold diamonds.
@item Reflections on balls.
@item Zoom in and out, rotation, different angles and bird's eye view.
@item Different game modes: 8 or 9-ball, Snooker or Carambole.
@item Tournaments.  Compete against other players.
@item Animated cue with strength and eccentric hit adjustment.
@item Jump shots and snipping.
@item Realistic gameplay and billiard sounds.
@item Red-Green stereo.
@item And much more.
@end itemize")
      (license (list license:gpl2 license:silofl1.1)))))

(define-public freedoom
  (package
    (name "freedoom")
    (version "0.12.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/freedoom/freedoom")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mq60lfwaaxmch7hsz8403pwafnlsmsd5z2df2j77ppwndwcrypb"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((freedoom (assoc-ref outputs "out"))
                    (wad-dir (string-append freedoom "/share/games/doom")))
               ;; Make sure that the install scripts know where to find
               ;; the appropriate WAD files.
               (substitute* "dist/freedoom"
                 (("IWAD=freedm.wad")
                  (string-append "IWAD=" wad-dir "/freedm.wad"))
                 (("IWAD=freedoom1.wad")
                  (string-append "IWAD=" wad-dir "/freedoom1.wad"))
                 (("IWAD=freedoom2.wad")
                  (string-append "IWAD=" wad-dir "/freedoom2.wad")))
               #t))))))
    (native-inputs
     (list asciidoc deutex python python-pillow))
    (home-page "https://freedoom.github.io/")
    (synopsis "Free content game based on the Doom engine")
    (native-search-paths
     (list (search-path-specification
            (variable "DOOMWADDIR")
            (files '("share/games/doom")))
           (search-path-specification
            (variable "DOOMWADPATH")
            (files '("share/games/doom")))))
    (description
     "The Freedoom project aims to create a complete free content first person
shooter game.  Freedoom by itself is just the raw material for a game: it must
be paired with a compatible game engine (such as @code{prboom-plus}) to be
played.  Freedoom complements the Doom engine with free levels, artwork, sound
effects and music to make a completely free game.")
    (license license:bsd-3)))

(define-public freedroidrpg
  (package
    (name "freedroidrpg")
    (version "0.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://ftp.osuosl.org/pub/freedroid/"
                           "freedroidRPG-" (version-major+minor version) "/"
                           "freedroidRPG-" version ".tar.gz"))
       (sha256
        (base32 "0n4kn38ncmcy3lrxmq8fjry6c1z50z4q1zcqfig0j4jb0dsz2va2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        (string-append "CFLAGS="
                       "-fcommon "      ; XXX needed to build with GCC 10
                       "-I" (assoc-ref %build-inputs "sdl-gfx") "/include/SDL "
                       "-I" (assoc-ref %build-inputs "sdl-image") "/include/SDL "
                       "-I" (assoc-ref %build-inputs "sdl-mixer") "/include/SDL")
        "--enable-opengl")
       ;; FIXME: the test suite fails with the following error output:
       ;;   4586 Segmentation fault      env SDL_VIDEODRIVER=dummy \
       ;;   SDL_AUDIODRIVER=dummy ./src/freedroidRPG -nb text
       #:tests? #f))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("glu" ,glu)
       ("libjpeg" ,libjpeg-turbo)
       ("libogg" ,libogg)
       ("libpng" ,libpng)
       ("libvorbis" ,libvorbis)
       ("mesa" ,mesa)
       ("python" ,python-wrapper)
       ("sdl" ,sdl)
       ("sdl-gfx" ,sdl-gfx)
       ("sdl-image" ,sdl-image)
       ("sdl-mixer" ,sdl-mixer)
       ("zlib" ,zlib)))
    (home-page "https://www.freedroid.org/")
    (synopsis "Isometric role-playing game against killer robots")
    (description
     "Freedroid RPG is an @dfn{RPG} (Role-Playing Game) with isometric graphics.
The game tells the story of a world destroyed by a conflict between robots and
their human masters.  To restore peace to humankind, the player must complete
numerous quests while fighting off rebelling robots---either by taking control
of them, or by simply blasting them to pieces with melee and ranged weapons in
real-time combat.")
    (license (list license:expat        ; lua/
                   license:gpl3         ; src/gen_savestruct.py
                   license:gpl2+))))    ; the rest

(define-public golly
  (package
    (name "golly")
    (version "3.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/golly/golly/golly-"
                                  version "/golly-" version
                                  "-src.tar.gz"))
              (sha256
               (base32
                "1j3ksnar4rdam4xiyspgyrs1pifbvxfxkrn65brkwxpx39mpgzc8"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list "CC=gcc"
                          (string-append "GOLLYDIR="
                                         (assoc-ref %outputs "out")
                                         "/share/golly"))
       #:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             ;; For some reason, setting the PYTHON_SHLIB make flag doesn't
             ;; properly set the path to the Python shared library. This
             ;; substitution acheives the same end by different means.
             (substitute* "gui-wx/wxprefs.cpp"
               (("pythonlib = wxT\\(STRINGIFY\\(PYTHON_SHLIB\\)\\)")
                (string-append "pythonlib = \""
                               (assoc-ref inputs "python")
                               "/lib/libpython-2.7.so\"")))
             #t))
         (replace 'build
           (lambda* (#:key make-flags outputs #:allow-other-keys)
             (with-directory-excursion "gui-wx"
               (apply invoke `("make" ,@make-flags "-f" "makefile-gtk")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/golly"))
                    (pixmaps (string-append out "/share/pixmaps"))
                    (share (string-append out "/share/golly")))
               (for-each (lambda (binary)
                           (install-file binary bin))
                         '("bgolly" "golly"))
               (for-each (lambda (document)
                           (install-file
                            (string-append "docs/" document ".html")
                            doc))
                         '("License" "ReadMe" "ToDo"))
               (install-file "gui-wx/icons/appicon.xpm" pixmaps)
               (for-each (lambda (folder)
                           (copy-recursively
                            folder
                            (string-append share "/" folder)))
                         '("Help" "Patterns" "Rules" "Scripts")))
             #t)))))
    (native-inputs
     (list lua))
    (inputs
     `(("glu" ,glu)
       ("mesa" ,mesa)
       ("python" ,python-2)
       ("wxwidgets" ,wxwidgets-gtk2)
       ("zlib" ,zlib)))
    (home-page "http://golly.sourceforge.net/")
    (synopsis "Software for exploring cellular automata")
    (description
     "Golly simulates Conway's Game of Life and many other types of cellular
automata.  The following features are available:
@enumerate
@item Support for bounded and unbounded universes, with cells of up to 256
  states.
@item Support for multiple algorithms, including Bill Gosper's Hashlife
  algorithm.
@item Loading patterns from BMP, PNG, GIF and TIFF image files.
@item Reading RLE, macrocell, Life 1.05/1.06, dblife and MCell files.
@item Scriptable via Lua or Python.
@item Extracting patterns, rules and scripts from zip files.
@item Downloading patterns, rules and scripts from online archives.
@item Pasting patterns from the clipboard.
@item Unlimited undo/redo.
@item Configurable keyboard shortcuts.
@item Auto fit option to keep patterns within the view.
@end enumerate")
    (license license:gpl2+)))

(define-public joycond
  (let ((commit "f9a66914622514c13997c2bf7ec20fa98e9dfc1d")
        (revision "1"))
    (package
      (name "joycond")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/DanielOgorchock/joycond")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "07z86yp27vxc0b44jgvf1vpa69rh3wdvd1xbzcsrj3f32743pv5a"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f                    ;no test suite
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-bin-location
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (substitute* "CMakeLists.txt"
                   (("/lib/udev/rules.d")
                    (string-append out "/lib/udev/rules.d"))
                   (("/etc/systemd/system")
                    (string-append out "/etc/systemd/system"))
                   (("/etc/modules-load.d")
                    (string-append out "/etc/modules-load.d"))
                   (("/usr/bin")
                    (string-append out "/bin")))))))))
      (native-inputs (list pkg-config))
      (inputs
       (list eudev libevdev))
      (home-page "https://github.com/DanielOgorchock/joycond")
      (synopsis "Joy-Con controller daemon")
      (description "This package provides a userspace daemon for the Nintendo
Joy-Con controllers.")
      (license license:gpl3))))

(define-public julius
  (package
    (name "julius")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bvschaik/julius")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nfdn8n6ywhm69ckz9a1chl5xxiqyaj3l337wadsbppnpscjihrc"))
       ;; Remove unused bundled libraries.
       (modules '((guix build utils)))
       (snippet
        '(begin
           (with-directory-excursion "ext"
             (for-each delete-file-recursively '("dirent" "png" "SDL2" "zlib")))
           #t))))
    (build-system cmake-build-system)
    (inputs
     (list libpng sdl2 sdl2-mixer))
    (home-page "https://github.com/bvschaik/julius")
    (synopsis "Re-implementation of Caesar III game engine")
    (description
     "Engine for Caesar III, a city-building real-time strategy game.
Julius includes some UI enhancements while preserving the logic (including
bugs) of the original game, so that saved games are compatible.  This package
does not include game data.")
    (license (list license:agpl3
                   license:zlib))))     ; ext/tinyfiledialogs

(define-public augustus
  (package
    (inherit julius)
    (name "augustus")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Keriew/augustus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0czazw8mc3fbvdazs2nzvgxd1dpzjc8z5fwiv89vv4nd7laz3jkj"))
       ;; Remove unused bundled libraries.
       (modules '((guix build utils)))
       (snippet
        '(begin
           (with-directory-excursion "ext"
             (for-each delete-file-recursively '("dirent" "png" "SDL2" "zlib")))
           #t))))
    (arguments
     ;; No tests.  See https://github.com/Keriew/augustus/issues/82.
     `(#:tests? #f))
    (home-page "https://github.com/Keriew/augustus")
    (synopsis "Re-implementation of Caesar III game engine with gameplay changes")
    (description
     "Fork of Julius, an engine for the a city-building real-time strategy
game Caesar III.  Gameplay enhancements include:

@itemize
@item roadblocks;
@item market special orders;
@item global labour pool;
@item partial warehouse storage;
@item increased game limits;
@item zoom controls.
@end itemize\n")))

(define-public meandmyshadow
  (package
    (name "meandmyshadow")
    (version "0.5a")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/meandmyshadow/"
                                  version "/meandmyshadow-" version
                                  "-src.tar.gz"))
              (sha256
               (base32
                "0i98v6cgmpsxy7mbb0s2y6f6qq6mkwzk2nrv1nz39ncf948aky2h"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; there are no tests
    (native-inputs
     (list pkg-config))
    (inputs
     `(("curl" ,curl)
       ("libarchive" ,libarchive)
       ("lua" ,lua)
       ("sdl" ,(sdl-union (list sdl2
                                sdl2-image
                                sdl2-mixer
                                sdl2-ttf)))))
    (home-page "https://acmepjz.github.io/meandmyshadow/")
    (synopsis "Puzzle/platform game")
    (description "Me and My Shadow is a puzzle/platform game in which you try
to reach the exit by solving puzzles.  Spikes, moving blocks, fragile blocks
and much more stand between you and the exit.  Record your moves and let your
shadow mimic them to reach blocks you couldn't reach alone.")
    (license license:gpl3+)))

(define-public opensurge
  (package
    (name "opensurge")
    (version "0.5.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/alemart/opensurge")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13g5izss7dmgigc8iif8hid3z6i066b0z29rbql2b9qjmdj1dp41"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;there are no tests
       #:configure-flags
       (let* ((out (assoc-ref %outputs "out"))
              (share (string-append out "/share")))
         (list (string-append "-DCMAKE_INSTALL_PREFIX=" out)
               (string-append "-DGAME_BINDIR=" out "/bin") ; not /bin/games
               (string-append "-DGAME_DATADIR=" share "/" ,name)
               (string-append "-DDESKTOP_ENTRY_PATH=" share "/applications")
               (string-append "-DDESKTOP_ICON_PATH=" share "/pixmaps")
               (string-append "-DDESKTOP_METAINFO_PATH=" share "/metainfo")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-xdg-open-path
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Look for xdg-open in the store.
             (substitute* "src/core/web.c"
               (("/usr(/bin/xdg-open)" _ bin)
                (search-input-file inputs bin)))))
         (add-after 'unpack 'unbundle-fonts
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Replace bundled Roboto fonts with links to the store.
             (with-directory-excursion "fonts"
               (let ((roboto-dir (string-append
                                  (assoc-ref inputs "font-google-roboto")
                                  "/share/fonts/truetype/")))
                 (for-each
                  (lambda (font)
                    (delete-file font)
                    (symlink (string-append roboto-dir font) font))
                  '("Roboto-Black.ttf" "Roboto-Bold.ttf" "Roboto-Medium.ttf")))
               #t))))))
    (inputs
     (list allegro font-google-roboto surgescript xdg-utils))
    (home-page "https://opensurge2d.org")
    (synopsis "2D retro side-scrolling game")
    (description "@code{Open Surge} is a 2D retro side-scrolling platformer
inspired by the Sonic games.  The player runs at high speeds through each
level while collecting items and avoiding obstacles.  The game includes a
built-in level editor.")
    (license
     ;; Code is under GPL 3+, assets are under various licenses.
     ;; See src/misc/credits.c for details.
     (list license:gpl3+
           license:cc0
           license:cc-by3.0
           license:cc-by-sa3.0
           license:expat
           license:public-domain
           license:silofl1.1))))

(define-public knights
  (package
    (name "knights")
    (version "025")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.knightsgame.org.uk/files/knights_"
                                  version "_src.tar.gz"))
              (sha256
               (base32
                "18vp2ygvn0s0jz8rm585jqf6hjqkam1ximq81k0r9hpmfj7wb88f"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "CXXFLAGS=-lpthread")
       #:phases
       (modify-phases %standard-phases
         ;; No configure script.
         (delete 'configure))
       #:tests? #f)) ;; No check target.
    (inputs
     `(("boost" ,boost)
       ("sdl-union" ,(sdl-union (list sdl sdl-image sdl-mixer)))
       ("freetype" ,freetype)
       ("fontconfig" ,fontconfig)
       ("curl" ,curl)))
    (native-inputs
     (list pkg-config))
    (home-page "http://www.knightsgame.org.uk/")
    (synopsis "Multiplayer dungeon game involving knights and quests")
    (description "Knights is a multiplayer game involving several knights who
must run around a dungeon and complete various quests.  Each game revolves
around a quest – for example, you might have to find some items and carry them
back to your starting point.  This may sound easy, but as there are only
enough items in the dungeon for one player to win, you may end up having to
kill your opponents to get their stuff!  Other quests involve escaping from
the dungeon, fighting a duel to the death against the enemy knights, or
destroying an ancient book using a special wand.")
    ;; This package includes modified sources of lua (X11), enet (Expat), and
    ;; guichan (BSD-3).  The "Coercri" library is released under the Boost
    ;; license.  The whole package is released under GPLv3+.
    (license license:gpl3+)))

(define-public gnome-2048
  (package
    (name "gnome-2048")
    (version "3.38.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-2048/"
                                  (version-major+minor version)  "/"
                                  "gnome-2048-" version ".tar.xz"))
              (sha256
               (base32
                "0s5fg4z5in1h39fcr69j1qc5ynmg7a8mfprk3mc3c0csq3snfwz2"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") "true"))
             #t)))))
    (inputs
     (list gtk+ clutter clutter-gtk libgee libgnome-games-support))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin") ; for desktop-file-validate and appstream-util
       ("itstool" ,itstool)
       ("libxml2" ,libxml2)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://wiki.gnome.org/Apps/2048")
    (synopsis "Move the tiles until you obtain the 2048 tile")
    (description "GNOME 2048 provides a 2D grid for playing 2048, a
single-player sliding tile puzzle game.  The objective of the game is to merge
together adjacent tiles of the same number until the sum of 2048 is achieved
in one tile.")
    (license license:gpl3+)))

(define-public gnome-chess
  (package
    (name "gnome-chess")
    (version "3.37.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-chess/"
                                  (version-major+minor version)  "/"
                                  "gnome-chess-" version ".tar.xz"))
              (sha256
               (base32
                "09axf0q1mp13sv8cs0syfg8ahcd9r2qb26278r09j6s4njxmkfv4"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") "true"))
             #t)))))
    (inputs
     (list gtk+ librsvg))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin") ; for desktop-file-validate and appstream-util
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://wiki.gnome.org/Apps/Chess")
    (synopsis "Chess board for GNOME")
    (description "GNOME Chess provides a 2D board for playing chess games
against human or computer players.  It supports loading and saving games in
Portable Game Notation.  To play against a computer, install a chess engine
such as chess or stockfish.")
    (license license:gpl3+)))

(define-public gnubg
  (package
    (name "gnubg")
    (version "1.06.002")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/gnubg/gnubg-release-"
                           version "-sources.tar.gz"))
       (sha256
        (base32
         "11xwhcli1h12k6rnhhyq4jphzrhfik7i8ah3k32pqw803460n6yf"))))
    (build-system gnu-build-system)
    (inputs (list ;; XXX: Build with an older Pango for 'pango_font_get_hb_font' and
                  ;; 'pango_coverage_get_type'.  Try removing this for versions > 1.06.002.
                  pango-1.42
                  glib
                  readline
                  gtk+-2
                  mesa
                  glu
                  gtkglext
                  sqlite
                  libcanberra))
    (native-inputs `(("python-2" ,python-2)
                     ("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       ;; SSE instructions are available on Intel systems only.
       (list ,@(if (any (cute string-prefix? <> (or (%current-target-system)
                                                    (%current-system)))
                        '("x86_64" "i686"))
                   '("--enable-simd=sse2") ; prevent avx instructions
                   '()))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (apps (string-append out "/share/applications")))
               (mkdir-p apps)
               (with-output-to-file (string-append apps "/gnubg.desktop")
                 (lambda _
                   (format #t
                           "[Desktop Entry]~@
                            Name=GNU Backgammon~@
                            Exec=~a/bin/gnubg -w~@
                            Icon=gnubg~@
                            Categories=Game;~@
                            Terminal=false~@
                            Type=Application~%"
                           out))))
             #t)))))
    (home-page "https://www.gnu.org/software/gnubg/")
    (synopsis "Backgammon game")
    (description "The GNU backgammon application (also known as \"gnubg\") can
be used for playing, analyzing and teaching the game.  It has an advanced
evaluation engine based on artificial neural networks suitable for both
beginners and advanced players.  In addition to a command-line interface, it
also features an attractive, 3D representation of the playing board.")
    (license license:gpl3+)))

(define-public gnubik
  (package
    (name "gnubik")
    (version "2.4.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnubik/gnubik-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1vlf924mq8hg93bsjj0rzvs0crc6psmlxyc6zn0fr7msnmpx6gib"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'skip-gtk-update-icon-cache
                    (lambda _
                      ;; Do not attempt to run 'gtk-update-icon-cache', which is
                      ;; unnecessary and causes a needless dependency on glib.
                      (substitute* "Makefile.in"
                        (("gtk-update-icon-cache")
                         "true"))
                      #t)))))
    (inputs (list gtk+-2
                  mesa
                  glu
                  libx11
                  guile-2.0
                  gtkglext))
    (native-inputs `(("gettext" ,gettext-minimal)
                     ("pkg-config" ,pkg-config)))
    (home-page "https://www.gnu.org/software/gnubik/")
    (synopsis "3d Rubik's cube game")
    (description
     "GNUbik is a puzzle game in which you must manipulate a cube to make
each of its faces have a uniform color.  The game is customizable, allowing
you to set the size of the cube (the default is 3x3) or to change the colors.
You may even apply photos to the faces instead of colors.  The game is
scriptable with Guile.")
    (license license:gpl3+)))

(define-public gnushogi
  (package
    (name "gnushogi")
    (version "1.4.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnushogi/gnushogi-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0a9bsl2nbnb138lq0h14jfc5xvz7hpb2bcsj4mjn6g1hcsl4ik0y"))))
    (arguments `(#:tests? #f)) ;; No check target.
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/gnushogi/")
    (synopsis "The game of Shogi (Japanese chess)")
    (description  "GNU Shogi is a program that plays the game Shogi (Japanese
Chess).  It is similar to standard chess but this variant is far more complicated.")
    (license license:gpl3+)))

(define-public ltris
  (package
    (name "ltris")
    (version "1.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/lgames/ltris/"
                           "ltris-" version ".tar.gz"))
       (sha256
        (base32 "1a2m17jwkyar8gj07bn5jk01j2ps4vvc48z955jjjsh67q2svi0f"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append
                      (search-input-directory inputs "include/SDL")
                      ":" (or (getenv "CPATH") ""))))))))
    (inputs
     (list (sdl-union (list sdl sdl-mixer))))
    (home-page "http://lgames.sourceforge.net/LTris/")
    (synopsis "Tetris clone based on the SDL library")
    (description
     "LTris is a tetris clone: differently shaped blocks are falling down the
rectangular playing field and can be moved sideways or rotated by 90 degree
units with the aim of building lines without gaps which then disappear (causing
any block above the deleted line to fall down).  LTris has three game modes: In
Classic you play until the stack of blocks reaches the top of the playing field
and no new blocks can enter.  In Figures the playing field is reset to a new
figure each level and later on tiles and lines suddenly appear.  In Multiplayer
up to three players (either human or CPU) compete with each other sending
removed lines to all opponents.  There is also a Demo mode in which you can
watch your CPU playing while enjoying a cup of tea!")
    (license license:gpl2+)))

(define-public nethack
  (package
    (name "nethack")
    (version "3.6.6")
    (source
      (origin
        (method url-fetch)
        (uri
         (string-append "https://www.nethack.org/download/" version "/nethack-"
                        (string-join (string-split version #\.) "") "-src.tgz"))
        (sha256
          (base32 "1liyckjp34j354qnxc1zn9730lh1p2dabrg1hap24z6xnqx0rpng"))))
    (native-inputs
      (list bison flex))
    (inputs
      (list ncurses less))
    (build-system gnu-build-system)
    (arguments
      '(#:make-flags
        `(,(string-append "PREFIX=" (assoc-ref %outputs "out")))
        #:phases
        (modify-phases %standard-phases
          (add-before 'configure 'patch-paths
            (lambda _
              (substitute* "sys/unix/nethack.sh"
                (("^ *cd .*$") ""))
              (substitute* "sys/unix/Makefile.utl"
                (("^YACC *=.*$") "YACC = bison -y\n")
                (("^LEX *=.*$") "LEX = flex\n")
                (("^# CC = gcc") "CC = gcc"))
              (substitute* "sys/unix/hints/linux"
                (("/bin/gzip") (string-append
                                 (assoc-ref %build-inputs "gzip")
                                 "/bin/gzip"))
                (("^WINTTYLIB=.*") "WINTTYLIB=-lncurses"))
              (substitute* "include/config.h"
                (("^.*define CHDIR.*$") "")
                (("^/\\* *#*define *REPRODUCIBLE_BUILD *\\*/")
                 ;; Honor SOURCE_DATE_EPOCH.
                 "#define REPRODUCIBLE_BUILD"))

              ;; Note: 'makedefs' rejects and ignores dates that are too old
              ;; or too new, so we must choose something reasonable here.
              (setenv "SOURCE_DATE_EPOCH" "1531865062")

              (substitute* "sys/unix/Makefile.src"
                 (("^# CC = gcc") "CC = gcc"))
              #t))
          (replace 'configure
            (lambda _
              (let ((bash (string-append
                            (assoc-ref %build-inputs "bash")
                            "/bin/bash")))
                (with-directory-excursion "sys/unix"
                  (substitute* "setup.sh" (("/bin/sh") bash))
                  (invoke bash "setup.sh" "hints/linux"))
                #t)))
          (add-after 'install 'fixup-paths
            (lambda _
              (let* ((output (assoc-ref %outputs "out"))
                     (nethack-script (string-append output "/bin/nethack")))
                (mkdir-p (string-append output "/games/lib/nethackuserdir"))
                (for-each
                  (lambda (file)
                    (rename-file
                      (string-append output "/games/lib/nethackdir/" file)
                      (string-append output "/games/lib/nethackuserdir/"
                                     file)))
                  '("xlogfile" "logfile" "perm" "record" "save"))
                (mkdir-p (string-append output "/bin"))
                (call-with-output-file nethack-script
                  (lambda (port)
                    (format port "#!~a/bin/sh
PATH=~a:$PATH
if [ ! -d ~~/.config/nethack ]; then
  mkdir -p ~~/.config/nethack
  cp -r ~a/games/lib/nethackuserdir/* ~~/.config/nethack
  chmod -R +w ~~/.config/nethack
fi

RUNDIR=$(mktemp -d)

cleanup() {
  rm -rf $RUNDIR
}
trap cleanup EXIT

cd $RUNDIR
for i in ~~/.config/nethack/*; do
  ln -s $i $(basename $i)
done
for i in ~a/games/lib/nethackdir/*; do
  ln -s $i $(basename $i)
done
~a/games/nethack"
                      (assoc-ref %build-inputs "bash")
                      (list->search-path-as-string
                        (list
                          (string-append
                            (assoc-ref %build-inputs "coreutils") "/bin")
                          (string-append
                            (assoc-ref %build-inputs "less") "/bin"))
                        ":")
                      output
                      output
                      output)))
                (chmod nethack-script #o555)
                #t)))
          (delete 'check))))
    (home-page "https://nethack.org")
    (synopsis "Classic dungeon crawl game")
    (description "NetHack is a single player dungeon exploration game that runs
on a wide variety of computer systems, with a variety of graphical and text
interfaces all using the same game engine.  Unlike many other Dungeons &
Dragons-inspired games, the emphasis in NetHack is on discovering the detail of
the dungeon and not simply killing everything in sight - in fact, killing
everything in sight is a good way to die quickly.  Each game presents a
different landscape - the random number generator provides an essentially
unlimited number of variations of the dungeon and its denizens to be discovered
by the player in one of a number of characters: you can pick your race, your
role, and your gender.")
    (license
      (license:fsdg-compatible
        "https://nethack.org/common/license.html"))))

(define-public pipewalker
  (package
    (name "pipewalker")
    (version "0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/pipewalker/pipewalker/"
                           version "/pipewalker-" version ".tar.gz"))
       (sha256
        (base32 "1x46wgk0s55562pd96cxagxkn6wpgglq779f9b64ff1k3xzp3myn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--docdir=" (assoc-ref %outputs "out")
                            "/share/doc/" ,name "-" ,version))
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-docdir
           ;; Makefile.in ignores configure's ‘--docdir=...’ option.  Fix that.
           (lambda _
             (substitute* "Makefile"
               (("(pkgdocdatadir = ).*" _ assignment)
                (string-append assignment "$(docdir)\n")))
             #t)))))
    (inputs
     (list libpng mesa sdl))
    (home-page "http://pipewalker.sourceforge.net/")
    (synopsis "Logical tile puzzle")
    (description
     "PipeWalker is a simple puzzle game with many diffent themes: connect all
computers to one network server, bring water from a source to the taps, etc.
The underlying mechanism is always the same: you must turn each tile in the
grid in the right direction to combine all components into a single circuit.
Every puzzle has a complete solution, although there may be more than one.")
    (license license:gpl3+)))

(define-public prboom-plus
  (package
   (name "prboom-plus")
   (version "2.5.1.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/prboom-plus/prboom-plus/"
                                version "/prboom-plus-" version ".tar.gz"))
            (sha256
             (base32 "151v6nign86m1a2vqz27krsccpc9m4d1jax4y43v2fa82wfj9qp0"))
            (modules '((guix build utils)))
            (snippet
             '(begin
                (substitute* "src/version.c"
                  (("__DATE__") "")
                  (("__TIME__") ""))
                #t))))
   (build-system gnu-build-system)
   (arguments
    '(#:configure-flags '("--disable-cpu-opt")
      #:make-flags `(,(string-append "gamesdir="
                                     (assoc-ref %outputs "out") "/bin"))
      #:phases
      (modify-phases %standard-phases
        (add-after 'set-paths 'set-sdl'paths
          (lambda* (#:key inputs #:allow-other-keys)
            (setenv "CPATH"
                    (string-append
                     (search-input-directory inputs "/include/SDL")
                     ":" (or (getenv "CPATH") ""))))))))
   (inputs
    (list fluidsynth
          glu
          libmad
          libpng
          libvorbis
          pcre
          portmidi
          (sdl-union (list sdl sdl-image sdl-mixer sdl-net))))
   (home-page "http://prboom-plus.sourceforge.net/")
   (synopsis "Version of the classic 3D shoot'em'up game Doom")
   (description
    "PrBoom+ is a Doom source port developed from the original PrBoom project.")
   (license license:gpl2+)))

(define-public retux
  (let ((release "1.5")
        (revision 0))
    (package
      (name "retux")
      (version (if (zero? revision)
                   release
                   (string-append release "-"
                                  (number->string revision))))
      (source (origin
                (method url-fetch)
                (uri (string-append "https://github.com/retux-game/retux/"
                                    "releases/download/v"
                                    version "/retux-"
                                    release "-src.zip"))
                (sha256
                 (base32
                  "1yima7s36hn2kh5h08lczc5iid8jbdxk7x1g5ms6knaznzj7rll3"))))
      (build-system python-build-system)
      (arguments
       `(#:tests? #f                    ; no check target
         #:phases
         (modify-phases %standard-phases
           ;; no setup.py script
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out    (assoc-ref outputs "out"))
                      (bin    (string-append out "/bin"))
                      (data   (string-append out "/share/retux")))
                 (mkdir-p bin)

                 (substitute* "retux.py"
                   ;; Use the correct data directory.
                   (("os\\.path\\.join\\(os\\.path\\.dirname\\(__file__\\), \"data\"\\),")
                    (string-append "\"" data "\",")))

                 (copy-file "retux.py" (string-append bin "/retux"))
                 (copy-recursively "data" data)
                 #t))))))
      (native-inputs
       (list unzip))
      (inputs
       (list python-sge python-six python-xsge))
      (home-page "https://retux-game.github.io/")
      (synopsis "Action platformer game")
      (description
       "ReTux is an action platformer loosely inspired by the Mario games,
utilizing the art assets from the @code{SuperTux} project.")
      ;; GPL version 3 or later is the license for the code and some art.
      ;; The rest of the licenses are for the art exclusively, as listed in
      ;; data/LICENSES.
      (license (list license:cc0
                     license:cc-by3.0
                     license:cc-by-sa3.0
                     license:cc-by-sa4.0
                     license:gpl2+
                     license:gpl3+)))))

(define-public roguebox-adventures
  (package
    (name "roguebox-adventures")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://download.tuxfamily.org/rba/RogueBoxAdventures_v"
             (string-join (string-split version #\.) "_") "_Source.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32
         "05zd03s5w9kcpklfgcggbaa6rwf59nm0q9vcj6gh9v2lh402k067"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (and (invoke "unzip" source)
                  ;; The actual source is buried a few directories deep.
                  (chdir (string-append "RogueBoxAdventures_v"
                                        (string-join
                                         (string-split ,version #\.) "_")
                                        "_Source")))))
         ;; no setup.py script
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (data (string-append
                           out "/share/games/roguebox-adventures")))
               ;; Use the correct data directory.
               (substitute* '("main.py" "LIB/getch.py" "LIB/getch_gcwz.py")
                 (("basic_path + os\\.sep + 'DATA'")
                  (string-append "'" data "'"))
                 (("^basic_path.*$")
                  (string-append "basic_path ='" data "'\n")))
               (substitute* "LIB/dialog.py"
                 (("d_path = os\\.path\\.dirname\\(.*\\)\\)")
                  (string-append "d_path = '" data "'")))
               (substitute* "LIB/gra_files.py"
                 (("basic_path = b_path\\.replace\\('/LIB',''\\)")
                  (string-append "basic_path ='" data "'\n")))

               ;; The game must save in the user's home directory because
               ;; the store is read-only.
               (substitute* "main.py"
                 (("home_save = False") "home_save = True")
                 (("'icon_small.png'")
                  (string-append "'" data "/icon_small.png'"))))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (roguebox-adventures
                     (string-append bin "/roguebox-adventures"))
                    (data (string-append
                           out "/share/games/roguebox-adventures"))
                    (lib (string-append data "/LIB"))
                    (doc (string-append
                          out "/share/doc/roguebox-adventures")))
               (mkdir-p bin)
               (mkdir-p doc)

               (for-each (lambda (file)
                           (copy-recursively file
                                             (string-append data "/" file)))
                         '("AUDIO" "FONT" "GRAPHIC" "LIB" "LICENSE"
                           "icon_big.png" "icon_small.png"))
               (for-each (lambda (file)
                           (chmod file #o555)
                           (install-file file lib))
                         '("main.py" "run.py"))

               (copy-recursively "DOC" doc)

               (call-with-output-file
                   roguebox-adventures
                 (lambda (p)
                   (format p "\
#!~a
export GUIX_PYTHONPATH=~a/LIB:~a
exec -a \"~a\" ~a \"$@\"\n"
                           (which "bash") data (getenv "GUIX_PYTHONPATH")
                           (which "python3")
                           (string-append lib "/main.py"))))
               (chmod roguebox-adventures #o555))
             #t)))))
    (native-inputs
     (list unzip))
    (inputs
     (list python-pygame python-tmx))
    (home-page "https://rogueboxadventures.tuxfamily.org")
    (synopsis "Classical roguelike/sandbox game")
    (description
     "RogueBox Adventures is a graphical roguelike with strong influences
from sandbox games like Minecraft or Terraria.  The main idea of RogueBox
Adventures is to offer the player a kind of roguelike toy-world.  This world
can be explored and changed freely.")
    ;; The GPL3+ is for code, the rest are for art.
    (license (list license:cc0
                   license:cc-by3.0
                   license:gpl3+
                   license:silofl1.1))))

(define-public seahorse-adventures
  (package
    (name "seahorse-adventures")
    (version "1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dulsi/seahorse-adventures")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m53jh2gchzr7rs35hml6lbvc5kb5hp229wlfqa09098b7hhl15a"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-1)))
       ;; Remove non-free (non-commercial) font.
       (snippet
        `(begin
           (for-each delete-file (find-files "data/fonts" "."))
           #t))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:phases
       (modify-phases %standard-phases
         (delete 'build)                ;pure Python
         (replace 'install              ;no install script
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share (string-append out "/share"))
                    (applications (string-append share "/applications"))
                    (data (string-append share "/seahorse-adventures")))
               ;; Install data.
               (for-each (lambda (f)
                           (chmod f #o555)
                           (install-file f data))
                         '("leveledit.py" "run_game.py" "tileedit.py"))
               (for-each (lambda (dir)
                           (let ((target (string-append data "/" dir)))
                             (mkdir-p target)
                             (copy-recursively dir target)))
                         '("data" "lib"))
               ;; Create executable.
               (mkdir-p bin)
               (let ((executable (string-append bin "/seahorse-adventures")))
                 (call-with-output-file executable
                   (lambda (p)
                     (format p
                             "#!~a~@
                              export GUIX_PYTHONPATH=~a:~a~@
                              exec -a \"~a\" ~a \"$@\"~%"
                             (which "bash") data (getenv "GUIX_PYTHONPATH")
                             (which "python3")
                             (string-append data "/run_game.py"))))
                 (chmod executable #o555))
               ;; Add desktop file.
               (mkdir-p applications)
               (make-desktop-entry-file
                (string-append applications "/seahorse-adventures.desktop")
                #:name "Seahorse Adventures"
                #:comment
                '((#f "Help Barbie the seahorse float on bubbles to the moon"))
                #:exec ,name
                #:icon ,name
                #:categories '("Game" "ActionGame")
                #:keywords '("game" "retro" "platform"))
               ;; Add icons.
               (for-each
                (lambda (size)
                  (let ((dir (string-append share "/icons/hicolor/"
                                            size "x" size "/apps")))
                    (mkdir-p dir)
                    (copy-file
                     (string-append "icon" size ".png")
                     (string-append dir "/searhorse-adventures.png"))))
                '("32" "64" "128")))
             #t))
         (add-after 'install 'unbundle-fonts
           ;; Unbundle Bitstream Vera font and replace deleted one.
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (data (string-append out "/share/seahorse-adventures"))
                    (vera (search-input-file
                           inputs "/share/fonts/truetype/Vera.ttf")))
               (let ((themes-dir (string-append data "/data/themes/")))
                 (for-each
                  (lambda (theme)
                    (let ((target (string-append themes-dir theme "/Vera.ttf")))
                      (delete-file target)
                      (symlink vera target)))
                  '("default" "gray")))
               (symlink vera (string-append data "/data/fonts/04B_20__.TTF"))
               (substitute* (string-append data "/lib/main.py")
                 (("f_scale = 0.35") "f_scale = 0.47")))
             #t)))))
    (inputs
     (list font-bitstream-vera python-pygame))
    (home-page "http://www.imitationpickles.org/barbie/")
    (synopsis "Help Barbie the seahorse float on bubbles to the moon")
    (description
     "Barbie Seahorse Adventures is a retro style platform arcade game.
You are Barbie the seahorse who travels through the jungle, up to the
volcano until you float on bubbles to the moon.  On the way to your
final destination you will encounter various enemies, servants of the
evil overlord who has stolen the galaxy crystal.  Avoid getting hit
and defeat them with your bubbles!")
    ;; GPL2+ is for code, CC0 is for art.
    (license (list license:gpl2+ license:cc0))))

(define-public solarus
  (package
    (name "solarus")
    ;; XXX: When updating this package, please also update hash in
    ;; `solarus-quest-editor' below.
    (version "1.6.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/solarus-games/solarus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ny9dgqphjv2l39rff2621hnrzpf8qin8vmnv7jdz20azjk4m8id"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           ;; The following tests fail reporting a missing "/dev/dri"
           ;; file.
           (lambda _
             (substitute* "tests/cmake/AddTestMaps.cmake"
               ((".*1200_create_shader_from_source.*" all)
                (string-append "#" all))
               ((".*1210_shader_scaling_factor.*" all)
                (string-append "#" all)))
             #t))
         (add-before 'check 'set-home
           ;; Tests fail without setting the following environment
           ;; variables.
           (lambda _
             (setenv "HOME" (getcwd))
             (setenv "XDG_RUNTIME_DIR" (getcwd))
             #t)))))
    (native-inputs
     (list pkg-config qttools))
    (inputs
     `(("glm" ,glm)
       ("libmodplug" ,libmodplug)
       ("libogg" ,libogg)
       ("libvorbis" ,libvorbis)
       ("luajit" ,luajit)
       ("openal" ,openal)
       ("physfs" ,physfs)
       ("qtbase" ,qtbase-5)
       ("sdl2" ,(sdl-union (list sdl2 sdl2-image sdl2-ttf)))))
    (home-page "https://www.solarus-games.org/")
    (synopsis "Lightweight game engine for Action-RPGs")
    (description
     "Solarus is a 2D game engine written in C++, that can run games
scripted in Lua.  It has been designed with 16-bit classic Action-RPGs
in mind.")
    ;; The source code is licensed under the terms of GPL-3.0.
    ;; Resources are licensed under the terms of CC-BY-SA-3.0 and
    ;; CC-BY-SA 4.0.
    (license (list license:gpl3 license:cc-by-sa3.0 license:cc-by-sa4.0))))

(define-public solarus-quest-editor
  (package
    (inherit solarus)
    (name "solarus-quest-editor")
    (version (package-version solarus))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/solarus-games/solarus-quest-editor")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pvjgd4faxii5sskw1h55lw90hlbazhwni8nxyywzrmkjbq7irm0"))))
    (arguments
     `(#:tests? #false))                ; no test suite
    (inputs
     (modify-inputs (package-inputs solarus)
       (prepend solarus)))
    (synopsis "Create and modify quests for the Solarus engine")
    (description
     "Solarus Quest Editor is a graphical user interface to create and
modify quests for the Solarus engine.")))

(define-public superstarfighter
  (package
    (name "superstarfighter")
    (version "0.6.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/notapixelstudio/superstarfighter")
             ;; The commit is not tagged upstream:
             ;; https://github.com/notapixelstudio/superstarfighter/commit/350605bf5454c26ebe2c57d8217edd03689c0573
             (commit "32521f467616bb390e3929d07e1936ff43fe64da")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ckghzrfgvk9z1n5f4ivnamm6s8h9sbv0a3aq9pp4a3yrhkgld0k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;there are no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (chdir "godot")
             (setenv "HOME" (getcwd))
             (with-output-to-file "export_presets.cfg"
               (lambda ()
                 (display
                  "[preset.0]
name=\"Guix\"
platform=\"Linux/X11\"
runnable=true
[preset.0.options]")))
             #t))
         (replace 'build
           (lambda _
             (let ((godot (assoc-ref %build-inputs "godot-headless")))
               (invoke (string-append godot "/bin/godot_server")
                       "--export-pack" "Guix"
                       "superstarfighter.pck" "project.godot"))
             #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share (string-append out "/share"))
                    (data (string-append share "/superstarfighter"))
                    (icons (string-append share "/icons/hicolor/256x256/apps")))
               (install-file "superstarfighter.pck" data)
               (mkdir-p bin)
               (call-with-output-file (string-append bin "/superstarfighter")
                 (lambda (port)
                   (format port
                           "#!/bin/sh~@
                            exec ~a/bin/godot --main-pack ~a/superstarfighter.pck~%"
                           (assoc-ref inputs "godot")
                           data)
                   (chmod port #o755)))
               (mkdir-p icons)
               (copy-file "icon.png" (string-append icons "/" ,name ".png"))
               (make-desktop-entry-file
                (string-append share "/applications/" ,name ".desktop")
                #:name "SuperStarfighter"
                #:comment "Fast-paced arcade combat game"
                #:exec ,name
                #:icon ,name
                #:categories '("Game" "ArcadeGame")))
             #t)))))
    (native-inputs
     `(("godot-headless" ,godot "headless")))
    (inputs
     (list godot))
    (home-page "https://notapixel.itch.io/superstarfighter")
    (synopsis "Fast-paced local multiplayer arcade game")
    (description "In SuperStarfighter, up to four local players compete in a
2D arena with fast-moving ships and missiles.  Different game types are
available, as well as a single-player mode with AI-controlled ships.")
    (license (list license:expat         ; game
                   license:silofl1.1)))) ; fonts

(define %ufoai-commit "a542a87a891f96b1ab2c44d35b2f6f16859a5019")
(define %ufoai-revision "0")
(define %ufoai-version (git-version "2.6.0_dev" %ufoai-revision %ufoai-commit))
(define ufoai-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "git://git.code.sf.net/p/ufoai/code") ;HTTPS fails mid-clone
          (commit %ufoai-commit)))
    (file-name (string-append "ufoai-" %ufoai-version "-checkout"))
    (sha256
     (base32
      "024s7b9rcg7iw8i2p72gwnvabk23ljlq0nldws0y4b6hpwzyn1wz"))
    (modules '((guix build utils)
               (srfi srfi-1)
               (ice-9 ftw)))
    (snippet
     '(begin
        ;; Delete ~32MiB of bundled dependencies.
        (with-directory-excursion "src/libs"
          (for-each delete-file-recursively
                    (lset-difference equal? (scandir ".")
                                     '("." ".." "gtest" "mumble"))))

        ;; Use relative path to Lua headers.
        (substitute* "src/common/scripts_lua.h"
          (("\\.\\./libs/lua/") ""))

        ;; Adjust Makefile targets to not depend on 'ufo2map', since we build
        ;; it as a separate package.  This way we don't need to make the same
        ;; adjustments for 'ufoai-data' and 'ufoai' below.
        (substitute* "build/maps.mk"
          (("\\./ufo2map") "ufo2map")
          (("maps: ufo2map") "maps:"))
        (substitute* "build/modules/testall.mk"
          (("testall: ufo2map") "testall:"))

        ;; If no cURL headers are found, the build system will try to include
        ;; the bundled version, even when not required.  Prevent that.
        (substitute* "build/default.mk"
          (("^include src/libs/curl/lib/Makefile\\.inc")
           ""))

        ;; While here, improve reproducibility by adding the '-X' flag to the
        ;; zip command used to create the map files, in order to prevent time
        ;; stamps from making it into the generated archives.
        (substitute* "build/data.mk"
          (("\\$\\(call ZIP\\)")
           "$(call ZIP) -X"))
        #t))))

(define-public trigger-rally
  (package
    (name "trigger-rally")
    (version "0.6.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/trigger-rally/"
                           "trigger-" version "/"
                           "trigger-rally-" version ".tar.gz"))
       (sha256
        (base32
         "016bc2hczqscfmngacim870hjcsmwl8r3aq8x03vpf22s49nw23z"))))
    (build-system gnu-build-system)
    (inputs
     `(("freealut" ,freealut)
       ("glew" ,glew)
       ("glu" ,glu)
       ("mesa" ,mesa)
       ("openal" ,openal)
       ("physfs" ,physfs)
       ("sdl" ,(sdl-union (list sdl2 sdl2-image)))
       ("tinyxml2" ,tinyxml2)))
    (arguments
     `(#:make-flags (list (string-append "prefix=" %output)
                          "bindir=$(prefix)/bin"
                          "datadir=$(datarootdir)"
                          "OPTIMS=-Ofast")
       #:tests? #f                      ; No tests present
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'cd-src
           (lambda _ (chdir "src")))
         (add-before 'build 'remove-timestamps
           (lambda _
             (substitute* (list "Trigger/menu.cpp"
                                "PEngine/app.cpp")
               ((".*__DATE__.*") ""))))
         (add-before 'build 'make-verbose
           (lambda _
             (substitute* "GNUmakefile"
               (("@\\$\\(CXX\\)") "$(CXX)"))))
         (add-after 'build 'set-data-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "../bin/trigger-rally.config.defs"
                 (("<data path=\"C:[^\"]*\"")
                  (string-append "<data path=\"" out "/share/trigger-rally\""))))))
         (add-after 'install 'create-desktop-entry
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (apps (string-append out "/share/applications")))
               (mkdir-p apps)
               (with-output-to-file
                   (string-append apps "/trigger-rally.desktop")
                 (lambda ()
                   (format #t           ; Borrowed from Debian package
                           "[Desktop Entry]~@
                            Name=Trigger Rally~@
                            Icon=trigger-rally~@
                            Comment=3D rally racing car game~@
                            Comment[de]=3D Rally-Autorennen~@
                            Comment[fr_FR]=un jeu de rally en 3D~@
                            Comment[ro_RO]=Un joc în 3D cu curse de raliu~@
                            Exec=~a/bin/trigger-rally~@
                            Terminal=false~@
                            StartupNotify=false~@
                            Type=Application~@
                            TryExec=~:*~a/bin/trigger-rally~@
                            Categories=Game;ArcadeGame;~@
                            Keywords=racing;tracks;~@
                            Keywords[de]=Rennstrecke;~%"
                           out)))))))))
    (home-page "http://trigger-rally.sourceforge.net")
    (synopsis "Fast-paced single-player racing game")
    (description "Trigger-rally is a 3D rally simulation with great physics
for drifting on over 200 maps.  Different terrain materials like dirt,
asphalt, sand, ice, etc. and various weather, light, and fog conditions give
this rally simulation the edge over many other games.  You need to make it
through the maps in often tight time limits and can further improve by beating
the recorded high scores.  All attached single races must be finished in time
in order to win an event, unlocking additional events and cars.  Most maps are
equipped with spoken co-driver notes and co-driver icons.")
    (license (list license:cc0               ;textures and audio in data.zip
                   license:gpl2+))))

(define-public ufo2map
  (package
    (name "ufo2map")
    (version %ufoai-version)
    (home-page "https://ufoai.org/")
    (source ufoai-source)
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("CC=gcc" "CXX=g++"
                           "--enable-release"
                           "--enable-ufo2map"
                           "--disable-uforadiant"
                           "--disable-cgame-campaign"
                           "--disable-cgame-multiplayer"
                           "--disable-cgame-skirmish"
                           "--disable-game"
                           "--disable-memory"
                           "--disable-testall"
                           "--disable-ufoded"
                           "--disable-ufo"
                           "--disable-ufomodel"
                           "--disable-ufoslicer")
       #:tests? #f ;no tests
       #:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key (configure-flags '()) #:allow-other-keys)
                      ;; The home-made configure script does not understand
                      ;; some of the default flags of gnu-build-system.
                      (apply invoke "./configure" configure-flags)))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (install-file "ufo2map" (string-append out "/bin"))
                        (install-file "debian/ufo2map.6"
                                      (string-append out "/share/man/man6"))
                        #t))))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("lua" ,lua-5.1)
       ("sdl-union" ,(sdl-union (list sdl2 sdl2-mixer sdl2-ttf)))))
    (synopsis "UFO: AI map generator")
    (description
     "This package provides @command{ufo2map}, a program used to generate
maps for the UFO: Alien Invasion strategy game.")
    (license license:gpl2+)))

(define ufoai-data
  (package
    (name "ufoai-data")
    (version %ufoai-version)
    (home-page "https://ufoai.org/")
    (source ufoai-source)
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:configure-flags '("CC=gcc" "CXX=g++")
       #:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key outputs (configure-flags '()) #:allow-other-keys)
                      (apply invoke "./configure" configure-flags)))
                  (replace 'build
                    (lambda* (#:key (parallel-build? #t) #:allow-other-keys)
                      (invoke "make"
                              "-j" (if parallel-build?
                                       (number->string (parallel-job-count))
                                       "1")
                              "maps")))
                  (add-after 'build 'pack
                    (lambda _
                      (invoke "make" "pk3")))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (for-each (lambda (file)
                                    (install-file file out))
                                  (find-files "base" "\\.pk3$"))
                        #t))))))
    (native-inputs
     `(("python" ,python-2)
       ("ufo2map" ,ufo2map)
       ("which" ,which)
       ("zip" ,zip)))
    (synopsis "UFO: AI data files")
    (description
     "This package contains maps and other assets for UFO: Alien Invasion.")
    ;; Most assets are available under either GPL2 or GPL2+.  Some use other
    ;; licenses, see LICENSES for details.
    (license (list license:gpl2+ license:gpl2 license:cc-by3.0
                   license:cc-by-sa3.0 license:public-domain))))

(define-public ufoai
  (package
    (name "ufoai")
    (version %ufoai-version)
    (home-page "https://ufoai.org/")
    (source ufoai-source)
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--prefix=" (assoc-ref %outputs "out"))
             (string-append "--datadir=" (assoc-ref %outputs "out")
                            "/share/games/ufo")
             "CC=gcc" "CXX=g++"
             "--enable-release"
             "--enable-game"
             "--disable-ufo2map"
             "--disable-dependency-tracking"

             ;; Disable hard links to prevent huge NARs.
             "--disable-hardlinkedgame"
             "--disable-hardlinkedcgame")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'symlink-data-files
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((data (assoc-ref inputs "ufoai-data")))
                        ;; Symlink the data files to where the build system
                        ;; expects to find them.  Ultimately these files are
                        ;; copied to $out/share/games/ufoai/base, losing the
                        ;; symlinks; we could fix that after install, but it
                        ;; does not make a big difference in practice due to
                        ;; deduplication.
                        (with-directory-excursion "base"
                          (for-each (lambda (file)
                                      (symlink file (basename file)))
                                    (find-files data "\\.pk3$")))
                        #t)))
                  (add-before 'configure 'create-language-files
                    (lambda _
                      (invoke "make" "lang")))
                  (replace 'configure
                    (lambda* (#:key outputs (configure-flags '()) #:allow-other-keys)
                      (apply invoke "./configure" configure-flags)))
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (invoke "./testall")
                          (format #t "test suite not run~%"))
                      #t))
                  (add-after 'install 'install-man-pages
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (man6 (string-append out "/share/man/man6")))
                        (install-file "debian/ufo.6" man6)
                        (install-file "debian/ufoded.6" man6)
                        #t))))

       ;; TODO: Some map tests occasionally fail because of randomness issues,
       ;; e.g. not enough generated aliens.  The test runner also fails early
       ;; in the build container with 'failed to shutdown server'?
       #:tests? #f))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("ufo2map" ,ufo2map)
       ("ufoai-data" ,ufoai-data)))
    (inputs
     `(("curl" ,curl)
       ("libjpeg" ,libjpeg-turbo)
       ("libogg" ,libogg)
       ("libpng" ,libpng)
       ("libtheora" ,libtheora)
       ("libvorbis" ,libvorbis)
       ("libxml2" ,libxml2)
       ("lua" ,lua-5.1)
       ("mesa" ,mesa)
       ("minixml" ,minixml)
       ("sdl-union" ,(sdl-union (list sdl2 sdl2-mixer sdl2-ttf)))
       ("zlib" ,zlib)))
    (synopsis "Turn-based tactical strategy game")
    (description
     "UFO: Alien Invasion is a tactical strategy game set in the year 2084.
You control a secret organisation charged with defending Earth from a brutal
alien enemy.  Build up your bases, prepare your team, and dive head-first into
the fast and flowing turn-based combat.

Over the long term you will need to conduct research into the alien threat to
figure out their mysterious goals and use their powerful weapons for your own
ends.  You will produce unique items and use them in combat against your
enemies.

You can also use them against your friends with the multiplayer functionality.

Warning: This is a pre-release version of UFO: AI!  Some things may not work
properly.")

    ;; The game code and most assets are GPL2+, but we use GPL2 only here
    ;; because some assets do not use the "or later" clause.  Many individual
    ;; assets use Creative Commons or Public Domain; see the LICENSE file.
    (license (delete license:gpl2+ (package-license ufoai-data)))))

(define-public xshogi
  (package
    (name "xshogi")
    (version "1.4.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnushogi/xshogi-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1dns0nhymak44by18sv48m4xb2skiwbi2i3nb9hl6w9iwd2i2brf"))))
    (build-system gnu-build-system)
    (inputs
     (list libxaw libxt))
    (home-page "https://www.gnu.org/software/gnushogi/")
    (synopsis "User interface for gnushogi")
    (description  "A graphical user interface for the package @code{gnushogi}.")
    ;; Contains a copy of GPLv3 but the licence notices simply
    ;; state "GNU General Public Licence" without specifying a version.
    (license license:gpl1+)))

(define-public abbaye
  (package
    (name "abbaye")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nevat/abbayedesmorts-gpl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pwqf7r9bqb2p3xrw9i7y8pgr1401fy3mnnqpb1qkhmdl3gqi9hb"))
       (modules '((guix build utils)))
       (snippet
        ;; Unbundle fonts.
        '(begin
           (delete-file-recursively "fonts")
           #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags '("CC=gcc")
       #:phases (modify-phases %standard-phases
                  (add-after 'set-paths 'set-sdl-paths
                    (lambda* (#:key inputs #:allow-other-keys)
                      (setenv "CPATH"
                              (string-append
                               (search-input-directory inputs "include/SDL2")
                               ":" (or (getenv "CPATH") "")))))
                  (add-after 'patch-source-shebangs 'patch-makefile
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Replace /usr with package output directory.
                      (substitute* "Makefile"
                        (("/usr") (assoc-ref outputs "out")))))
                  (add-before 'install 'make-install-dirs
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((prefix (assoc-ref outputs "out")))
                        ;; Create directories that the makefile assumes exist.
                        (mkdir-p (string-append prefix "/bin"))
                        (mkdir-p (string-append prefix "/share/applications"))
                        (mkdir-p (string-append prefix "/share/pixmaps")))))
                  ;; No configure script.
                  (delete 'configure))
       #:tests? #f)) ;; No check target.
    (native-inputs (list pkg-config))
    (inputs (list (sdl-union (list sdl2 sdl2-image sdl2-mixer))))
    (home-page "https://github.com/nevat/abbayedesmorts-gpl")
    (synopsis "GNU/Linux port of the indie game \"l'Abbaye des Morts\"")
    (description "L'Abbaye des Morts is a 2D platform game set in 13th century
France.  The Cathars, who preach about good Christian beliefs, were being
expelled by the Catholic Church out of the Languedoc region in France.  One of
them, called Jean Raymond, found an old church in which to hide, not knowing
that beneath its ruins lay buried an ancient evil.")
    (license license:gpl3)))

(define-public angband
  (package
    (name "angband")
    (version "4.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/angband/angband")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1psrdbf90mb6dhq0b9z18pz1csnshz1kvwg82dvwa99apqdw0la8"))
       (modules '((guix build utils)))
       (snippet
        ;; So, some of the sounds/graphics/tilesets are under different
        ;; licenses... some of them even nonfree!  This is a console-only
        ;; version of this package so we just remove them.
        ;; In the future, if someone tries to make a graphical variant of
        ;; this package, they can deal with that mess themselves. :)
        '(begin
           (for-each (lambda (subdir)
                       (let ((lib-subdir (string-append "lib/" subdir)))
                         (delete-file-recursively lib-subdir)))
                     '("fonts" "icons" "sounds" "tiles"))
           (substitute* "lib/Makefile"
             ;; And don't try to invoke makefiles in the directories we removed.
             (("gamedata customize help screens fonts tiles sounds icons user")
              "gamedata customize help screens user"))
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:configure-flags (list (string-append "--bindir=" %output "/bin"))))
    (native-inputs
     (list autoconf automake))
    (inputs (list ncurses))
    (home-page "https://rephial.org/")
    (synopsis "Dungeon exploration roguelike")
    (description "Angband is a Classic dungeon exploration roguelike.  Explore
the depths below Angband, seeking riches, fighting monsters, and preparing to
fight Morgoth, the Lord of Darkness.")
    (license license:gpl2)))

(define-public pingus
  (package
    (name "pingus")
    (version "0.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://gitlab.com/pingus/pingus.git")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0wp06kcmknsnxz7bjnsndb8x062z7r23fb3yrnbfnj68qhz18y74"))
       (patches (search-patches "pingus-boost-headers.patch"
                                "pingus-sdl-libs-config.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "src/pingus/screens/demo_session.cpp"
             (("#include <iostream>")
              ;; std::function moved to <functional> with C++ 11.
              ;; Remove this for versions newer than 0.7.6.
              "#include <iostream>\n#include <functional>"))
           #t))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config scons-python2))
    (inputs (list sdl
                  sdl-image
                  sdl-mixer
                  mesa
                  glu
                  libpng
                  boost))
    (arguments
     '(#:make-flags (list (string-append "PREFIX=" %output))
       #:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; no configure script
    (home-page "https://pingus.seul.org/")
    (synopsis "Lemmings clone")
    (description
     "Pingus is a free Lemmings-like puzzle game in which the player takes
command of a bunch of small animals and has to guide them through levels.
Since the animals walk on their own, the player can only influence them by
giving them commands, like build a bridge, dig a hole, or redirect all animals
in the other direction.  Multiple such commands are necessary to reach the
level's exit.  The game is presented in a 2D side view.")
    ;; Some source files are under bsd-3 and gpl2+ licenses.
    (license license:gpl3+)))

(define-public talkfilters
  (package
    (name "talkfilters")
    (version "2.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.hyperrealm.com/talkfilters/"
                           "talkfilters-" version  ".tar.gz"))
       (sha256
        (base32 "19nc5vq4bnkjvhk8srqddzhcs93jyvpm9r6lzjzwc1mgf08yg0a6"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/talkfilters/")
    (synopsis "Convert English text to humorous dialects")
    (description "The GNU Talk Filters are programs that convert English text
into stereotyped or otherwise humorous dialects.  The filters are provided as
a C library, so they can easily be integrated into other programs.")
    (license license:gpl2+)))

(define-public taisei
  (package
    (name "taisei")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/taisei-project/"
                           "taisei/releases/download/v" version
                           "/taisei-v" version ".tar.xz"))
       (sha256
        (base32 "1g53fcyrlzmvlsb40pw90gaglysv6n1w42hk263iv61ibhdmzh6v"))))
    (build-system meson-build-system)
    (arguments
     `(#:build-type "release"      ;comment out for bug-reporting (and cheats)
       #:configure-flags
       (list "-Dr_default=gles30"
             "-Dr_gles20=true"
             "-Dr_gles30=true"
             "-Dshader_transpiler=true")))
    (native-inputs
     (list pkg-config python python-docutils python-pygments))
    (inputs
     (list cglm
           freetype
           libpng
           libwebp
           libzip
           mesa
           openssl
           opusfile
           sdl2
           sdl2-mixer
           shaderc
           spirv-cross
           zlib))
    (home-page "https://taisei-project.org/")
    (synopsis "Shoot'em up fangame and libre clone of Touhou Project")
    (description
     "The player controls a character (one of three: Good, Bad, and Dead),
dodges the missiles (lots of it cover the screen, but the character's hitbox
is very small), and shoot at the adversaries that keep appear on the screen.")
    (license (list ;;game
                   license:expat
                   ;;resources/00-taisei.pkgdir/bgm/
                   ;;atlas/portraits/
                   license:cc-by4.0
                   ;;miscellaneous
                   license:cc0
                   license:public-domain))))

(define-public cmatrix
  (package
    (name "cmatrix")
    (version "2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/abishekvashok/cmatrix")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1h9jz4m4s5l8c3figaq46ja0km1gimrkfxm4dg7mf4s84icmasbm"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; This old ‘configure’ script doesn't support
             ;; variables passed as arguments.
             (let ((out (assoc-ref outputs "out")))
               (setenv "CONFIG_SHELL" (which "bash"))
               (invoke "./configure"
                       (string-append "--prefix=" out))))))))
    (inputs (list ncurses))
    (home-page "http://www.asty.org/cmatrix")
    (synopsis "Simulate the display from \"The Matrix\"")
    (description "CMatrix simulates the display from \"The Matrix\" and is
based on the screensaver from the movie's website.  It works with terminal
settings up to 132x300 and can scroll lines all at the same rate or
asynchronously and at a user-defined speed.")
    (license license:gpl2+)))

(define-public chess
  (package
    (name "chess")
    (version "6.2.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/chess/gnuchess-" version
                           ".tar.gz"))
       (sha256
        (base32
         "140qqkmvldnf41s39khrgyzr6a0az7dcfhkcmflh0sbmvl5w5z6x"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'fix-shell-scripts
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (chdir bin)
               (substitute* '("gnuchessx" "gnuchessu")
                 (("^gnuchess") (string-append bin "/gnuchess")))))))))
    (home-page "https://www.gnu.org/software/chess/")
    (synopsis "Full chess implementation")
    (description "GNU Chess is a chess engine.  It allows you to compete
against the computer in a game of chess, either through the default terminal
interface or via an external visual interface such as GNU XBoard.")
    (properties '((upstream-name . "gnuchess")
                  (ftp-directory . "/chess")))
    (license license:gpl3+)))

(define-public freedink-engine
  (package
    (name "freedink-engine")
    (version "109.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/freedink/freedink-" version
                                  ".tar.gz"))
              (patches (search-patches "freedink-engine-fix-sdl-hints.patch"))
              (sha256
               (base32
                "00hhk1bjdrc1np2qz44sa5n1mb62qzwxbvsnws3vpms6iyn3a2sy"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-embedded-resources")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-graphical-tests
           (lambda _
             ;; These tests require a graphical interface.
             (substitute* "src/Makefile.am"
               (("test_gfx_fonts TestIOGfxDisplay") ""))
             #t))
         (add-before 'bootstrap 'autoreconf
           (lambda _
	     ;; automake is out of date in the source
	     ;; autoreconf updates the automake scripts
	     (invoke "autoreconf")
	     ;; Build fails when autom4te.cache exists.
	     (delete-file-recursively "autom4te.cache")
             #t))
         (add-after 'install 'delete-freedinkedit-desktop
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
             ;; freedinkedit does not know where to find freedink data
             ;; freedink data is read-only, so it cannot be edited anyway.
             ;; TODO: fix freedink.desktop
             (delete-file-recursively (string-append
                            out "/share/applications"))
             #t))))))
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("cxxtest" ,cxxtest)
                     ("gettext" ,gettext-minimal)
                     ("help2man" ,help2man)
                     ("pkg-config" ,pkg-config)))
    (inputs `(("sdl-union" ,(sdl-union (list sdl2 sdl2-image sdl2-mixer
                                             sdl2-ttf sdl2-gfx)))
              ("fontconfig" ,fontconfig)
              ("glm" ,glm)))
    (properties '((ftp-directory . "/freedink")
                  (upstream-name . "freedink")))
    (home-page "https://www.gnu.org/software/freedink/")
    (synopsis "Twisted adventures of young pig farmer Dink Smallwood")
    (description
     "GNU FreeDink is a free and portable re-implementation of the engine
for the role-playing game Dink Smallwood.  It supports not only the original
game data files but it also supports user-produced game mods or \"D-Mods\".
To that extent, it also includes a front-end for managing all of your D-Mods.")
    (license license:gpl3+)))

(define-public freedink-data
  (package
    (name "freedink-data")
    (version "1.08.20190120")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/freedink/freedink-data-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "17gvryadlxk172mblbsil7hina1z5wahwaxnr6g3mdq57dvl8pvi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (delete 'check))               ; no tests
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (properties '((ftp-directory . "/freedink")))
    (home-page "https://www.gnu.org/software/freedink/")
    (synopsis "Game data for GNU Freedink")
    (description
     "This package contains the game data of GNU Freedink.")
    (license license:gpl3+)))

(define-public freedink-dfarc
  (package
    (name "freedink-dfarc")
    (version "3.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/freedink/dfarc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1yp8n3w426xnlp10xk06vfi2y3k9xrcfyck7s7qs1v0ys7n284d5"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool))
    (inputs
     (list bzip2 wxwidgets))
    (properties '((ftp-directory . "/freedink")
                  (upstream-name . "dfarc")))
    (home-page "https://www.gnu.org/software/freedink/")
    (synopsis "Front-end for managing and playing Dink Modules")
    (description "DFArc makes it easy to play and manage the GNU FreeDink game
and its numerous D-Mods.")
    (license license:gpl3+)))

(define-public freedink
  ;; This is a wrapper that tells the engine where to find the data.
  (package (inherit freedink-engine)
    (name "freedink")
    (build-system trivial-build-system)
    (arguments
     '(#:builder (begin
                   (use-modules (guix build utils))

                   (let* ((output     (assoc-ref %outputs "out"))
                          (bin        (string-append output "/bin"))
                          (executable (string-append bin "/freedink")))
                     (mkdir-p bin)
                     (call-with-output-file executable
                       (lambda (port)
                         (format port "#!~a/bin/sh
exec ~a/bin/freedink -refdir ~a/share/dink\n"
                                 (assoc-ref %build-inputs "bash")
                                 (assoc-ref %build-inputs "engine")
                                 (assoc-ref %build-inputs "data"))
                         (chmod port #o777)))
                     #t))
       #:modules ((guix build utils))))
    (inputs `(("engine" ,freedink-engine)
              ("data" ,freedink-data)
              ("bash" ,bash)))
    (native-inputs '())))

(define-public xboard
  (package
    (name "xboard")
    (version "4.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/xboard/xboard-" version
                           ".tar.gz"))
       (sha256
        (base32
         "1mkh36xnnacnz9r00b5f9ld9309k32jv6mcavklbdnca8bl56bib"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("CFLAGS=-fcommon")
       #:phases
       (modify-phases %standard-phases
         ;; Fixes https://issues.guix.gnu.org/47195.
         (add-after 'unpack 'patch-aplay-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "xboard.conf"
               (("aplay -q")
                (string-append (search-input-file inputs "/bin/aplay")
                               " -q")))))
         ;; Fixes https://issues.guix.gnu.org/45236.
         (add-after 'unpack 'patch-default-engine
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "xboard.conf"
               (("-firstChessProgram fairymax")
                (string-append "-firstChessProgram "
                               (assoc-ref inputs "chess")
                               "/bin/gnuchessx"))))))))
    (inputs
     (list alsa-utils chess gtk+-2 librsvg))
    (native-inputs
     (list texinfo pkg-config))
    (home-page "https://www.gnu.org/software/xboard/")
    (synopsis "Graphical user interface for chess programs")
    (description "GNU XBoard is a graphical board for all varieties of chess,
including international chess, xiangqi (Chinese chess), shogi (Japanese chess)
and Makruk.  Several lesser-known variants are also supported.  It presents a
fully interactive graphical interface and it can load and save games in the
Portable Game Notation.")
    (license license:gpl3+)))

(define-public gtypist
  (package
    (name "gtypist")
    (version "2.9.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gtypist/gtypist-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0xzrkkmj0b1dw3yr0m9hml2y634cc4h61im6zwcq57s7285z8fn1"))
              (modules '((guix build utils)))
              (snippet
               ;; We do not provide `ncurses.h' within an `ncursesw'
               ;; sub-directory, so patch the source accordingly.  See
               ;; <http://bugs.gnu.org/19018>.
               '(begin
                  (for-each (lambda (file)
                              (substitute* file
                                (("ncursesw/ncurses.h")
                                 "ncurses.h")))
                            (find-files "." "configure$|\\.c$"))
                  #t))))
    (build-system gnu-build-system)
    (inputs (list ncurses perl))
    (home-page "https://www.gnu.org/software/gtypist/")
    (synopsis "Typing tutor")
    (description
     "GNU Typist is a universal typing tutor.  It can be used to learn and
practice touch-typing.  Several tutorials are included; in addition to
tutorials for the standard QWERTY layout, there are also tutorials for the
alternative layouts Dvorak and Colemak, as well as for the numpad.  Tutorials
are primarily in English, however some in other languages are provided.")
    (license license:gpl3+)))

(define-public irrlicht
  (package
    (name "irrlicht")
    (version "1.8.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/irrlicht/Irrlicht%20SDK/"
                    (version-major+minor version)
                    "/" version "/irrlicht-" version ".zip"))
              (sha256
               (base32
                "0cz4z4dwrv5ypl19ll67wl6jjpy5k6ly4vr042w4br88qq5jhazl"))
              (patches (search-patches "irrlicht-use-system-libs.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file-recursively
                     '("bin" ; bundled compiled Windows binaries"
                       "source/Irrlicht/MacOSX"
                       "source/Irrlicht/bzip2"
                       "source/Irrlicht/jpeglib"
                       "source/Irrlicht/libpng"
                       "source/Irrlicht/lzma"
                       "source/Irrlicht/zlib"))
                  (delete-file "source/Irrlicht/glext.h")
                  (delete-file "source/Irrlicht/glxext.h")
                  (delete-file "source/Irrlicht/wglext.h")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir-to-source
           (lambda _
             ;; The actual source is buried a few directories deep.
             (chdir "source/Irrlicht/")
             #t))
         (add-after 'chdir-to-source 'remove-<sys/sysctl.h>
           (lambda _
             (substitute* "COSOperator.cpp"
               (("#include <sys/sysctl.h>") ""))))
         (add-after 'chdir-to-source 'fix-build-env
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("INSTALL_DIR = /usr/local/lib")
                  (string-append "INSTALL_DIR = " out "/lib"))
                 ;; Add '-fpermissive' to the CXXFLAGS
                 (("-Wall") "-Wall -fpermissive")) ; CImageLoaderJPG.cpp
               ;; The Makefile assumes these directories exist.
               (mkdir-p (string-append out "/lib"))
               (mkdir-p (string-append out "/include")))))
         (delete 'configure))           ; no configure script
       #:tests? #f                      ; no check target
       #:make-flags '("CC=gcc" "sharedlib")))
    (inputs
     `(("bzip2" ,bzip2)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libx11" ,libx11)
       ("libxxf86vm" ,libxxf86vm)
       ("mesa" ,mesa)))
    (synopsis "3D game engine written in C++")
    (description
     "The Irrlicht Engine is a high performance realtime 3D engine written in
C++.  Features include an OpenGL renderer, extensible materials, scene graph
management, character animation, particle and other special effects, support
for common mesh file formats, and collision detection.")
    (home-page "https://irrlicht.sourceforge.io/")
    (license license:zlib)))

(define-public mars
  ;; The latest release on SourceForge relies on an unreleased version of SFML
  ;; with a different API, so we take the latest version from the official
  ;; repository on Github.
  (let ((commit   "c855d044094a1d92317e38935d81ba938946132e")
        (revision "1"))
    (package
      (name "mars")
      (version (string-append "0.7.5." revision "." (string-take commit 7) ))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/thelaui/M.A.R.S.")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1r4c5gap1z2zsv4yjd34qriqkxaq4lb4rykapyzkkdf4g36lc3nh"))
                (patches (search-patches "mars-sfml-2.3.patch"
                                         "mars-install.patch"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f        ; There are no tests
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-install-path
            (lambda _
              (substitute* "src/CMakeLists.txt"
                (("\\$\\{CMAKE_INSTALL_PREFIX\\}/games")
                 "${CMAKE_INSTALL_PREFIX}/bin"))
              #t))
           (add-after 'unpack 'fix-data-path
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "src/System/settings.cpp"
                (("C_dataPath = \"./data/\";")
                 (string-append "C_dataPath = \""
                                (assoc-ref outputs "out")
                                "/share/games/marsshooter/\";")))
              #t)))))
      (inputs
       (list mesa fribidi taglib sfml))
      (home-page "http://mars-game.sourceforge.net/")
      (synopsis "2D space shooter")
      (description
       "M.A.R.S. is a 2D space shooter with pretty visual effects and
attractive physics.  Players can battle each other or computer controlled
enemies in different game modes such as space ball, death match, team death
match, cannon keep, and grave-itation pit.")
      (license license:gpl3+))))

(define glkterm
  (package
   (name "glkterm")
   (version "1.0.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "http://www.ifarchive.org/if-archive/programming/"
                         "glk/implementations/glkterm-104.tar.gz"))
     (sha256
      (base32
       "0zlj9nlnkdlvgbiliczinirqygiq8ikg5hzh5vgcmnpg9pvnwga7"))))
   (build-system gnu-build-system)
   (propagated-inputs `(("ncurses" ,ncurses))) ; required by Make.glkterm
   (arguments
    '(#:tests? #f ; no check target
      #:phases
      (modify-phases %standard-phases
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (inc (string-append out "/include"))
                   (lib (string-append out "/lib")))
              (for-each
               (lambda (file)
                 (install-file file inc))
               '("glk.h" "glkstart.h" "gi_blorb.h" "gi_dispa.h" "Make.glkterm"))
              (install-file "libglkterm.a" lib))
            #t))
        (delete 'configure))))          ; no configure script
   (home-page "http://www.eblong.com/zarf/glk/")
   (synopsis "Curses Implementation of the Glk API")
   (description
    "Glk defines a portable API for applications with text UIs.  It was
primarily designed for interactive fiction, but it should be suitable for many
interactive text utilities, particularly those based on a command line.
This is an implementation of the Glk library which runs in a terminal window,
using the @code{curses.h} library for screen control.")
   (license (license:fsf-free "file://README"))))

(define-public glulxe
  (package
   (name "glulxe")
   (version "0.5.4")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "http://www.ifarchive.org/if-archive/programming/"
                         "glulx/interpreters/glulxe/glulxe-054.tar.gz"))
     (sha256
      (base32
       "0vipydg6ra90yf9b3ipgppwxyb2xdhcxwvirgjy0v20wlf56zhhz"))))
   (build-system gnu-build-system)
   (inputs `(("glk" ,glkterm)))
   (arguments
    '(#:tests? #f                       ; no check target
      #:make-flags
      (let* ((glk (assoc-ref %build-inputs "glk")))
        (list (string-append "GLKINCLUDEDIR=" glk "/include")
              (string-append "GLKLIBDIR=" glk "/lib")
              (string-append "GLKMAKEFILE=" "Make.glkterm")))
      #:phases
      (modify-phases %standard-phases
        (replace 'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (bin (string-append out "/bin")))
              (install-file "glulxe" bin))
            #t))
        (delete 'configure))))          ; no configure script
   (home-page "https://www.eblong.com/zarf/glulx/")
   (synopsis "Interpreter for Glulx VM")
   (description
    "Glulx is a 32-bit portable virtual machine intended for writing and
playing interactive fiction.  It was designed by Andrew Plotkin to relieve
some of the restrictions in the venerable Z-machine format.  This is the
reference interpreter, using the Glk API.")
   (license license:expat)))

(define-public fifechan
  (package
    (name "fifechan")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://codeload.github.com/fifengine/"
                                  "fifechan/tar.gz/" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wxs9vz5x9y8chghd8vp7vfk089lfb0qnzggi17zrqkrngs5zgi9"))))
    (build-system cmake-build-system)
    (inputs
     (list sdl2 sdl2-image mesa))
    (arguments
     '(#:tests? #f))                    ; No included tests
    (home-page "https://fifengine.github.io/fifechan/")
    (synopsis "Cross platform GUI library specifically for games")
    (description
     "Fifechan is a lightweight cross platform GUI library written in C++
specifically designed for games.  It has a built in set of extendable GUI
Widgets, and allows users to create more.")
    (license license:lgpl2.1+)))

(define-public fifengine
  (package
    (name "fifengine")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://codeload.github.com/fifengine/"
                                  "fifengine/tar.gz/" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (patches (search-patches "fifengine-swig-compat.patch"
                                       "fifengine-boost-compat.patch"))
              (sha256
               (base32
                "1y4grw25cq5iqlg05rnbyxw1njl11ypidnlsm3qy4sm3xxdvb0p8"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f         ;TODO The test running fails to run some tests.
           #:configure-flags
           #~(list
              (string-append "-DOPENALSOFT_INCLUDE_DIR="
                             (search-input-directory %build-inputs "include/AL"))
              (string-append "-DPYTHON_SITE_PACKAGES="
                             #$output "/lib/python"
                             #$(version-major+minor
                                (package-version (this-package-input "python")))
                             "/site-packages"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-run_tests.py
                 (lambda _
                   ;; Patch the test runner to exit with a status of 1 if any test
                   ;; fails, to allow detecting failures.
                   (substitute* "run_tests.py"
                     (("ERROR\\. One or more tests failed!'\\)")
                      "ERROR. One or more tests failed!')
\t\texit(1)"))))
               ;; Run tests after installation so that we can make use of the built
               ;; python modules.
               (delete 'check)
               (add-after 'install 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     ;; The tests require an X server.
                     (system "Xvfb :1 &")
                     (setenv "DISPLAY" ":1")
                     (setenv "XDG_RUNTIME_DIR" "/tmp")
                     ;; Run tests
                     (chdir #$(string-append "../" (package-name this-package)
                                             "-" (package-version this-package)))
                     (invoke "python3" "run_tests.py" "-a")))))))
    (native-inputs
     (list python swig xorg-server-for-tests))
    (inputs
     (list sdl2
           sdl2-image
           sdl2-ttf
           tinyxml
           openal
           libogg
           glew
           libvorbis
           boost
           fifechan
           swig
           python))
    (propagated-inputs
     (list python-future))
    (home-page "https://www.fifengine.net/")
    (synopsis "FIFE is a multi-platform isometric game engine written in C++")
    (description
     "@acronym{FIFE, Flexible Isometric Free Engine} is a multi-platform
isometric game engine.  Python bindings are included allowing users to create
games using Python as well as C++.")
    (license license:lgpl2.1+)))

(define-public fizmo
  (package
    (name "fizmo")
    (version "0.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://fizmo.spellbreaker.org/source/"
                                  "fizmo-" version ".tar.gz"))
              (sha256
               (base32
                "1amyc4n41jf08kxmdgkk30bzzx54miaxa97w28f417qwn8lrl98w"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (let ((libjpeg (assoc-ref %build-inputs "libjpeg"))
             (ncurses (assoc-ref %build-inputs "ncurses")))
         (list (string-append "--with-jpeg-includedir=" libjpeg "/include")))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("freetype" ,freetype)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libsndfile" ,libsndfile)
       ("libxml2" ,libxml2)
       ("ncurses" ,ncurses)
       ("sdl2" ,sdl2)))
    (home-page "https://fizmo.spellbreaker.org/")
    (synopsis "Z-machine interpreter")
    (description
     "Fizmo is a console-based Z-machine interpreter.  It is used to play
interactive fiction, also known as text adventures, which were implemented
either by Infocom or created using the Inform compiler.")
    (license license:bsd-3)))

(define-public gnugo
  (package
    (name "gnugo")
    (version "3.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/gnugo/gnugo-" version
                                 ".tar.gz"))
             (sha256
              (base32
               "0wkahvqpzq6lzl5r49a4sd4p52frdmphnqsfdv7gdp24bykdfs6s"))))
    (build-system gnu-build-system)
    (inputs
     (list readline))
    (arguments
     `(#:configure-flags '("CFLAGS=-fcommon")))
    (synopsis "Play the game of Go")
    (description
     "GNU Go is a program that plays the game of Go, in which players
place stones on a grid to form territory or capture other stones.  While
it can be played directly from the terminal, rendered in ASCII characters,
it is also possible to play GNU Go with 3rd party graphical interfaces or
even in Emacs.  It supports the standard game storage format (SGF, Smart
Game Format) and inter-process communication format (GMP, Go Modem
Protocol).")
    (home-page "https://www.gnu.org/software/gnugo/")
    (license license:gpl3+)))

(define-public extremetuxracer
  (package
    (name "extremetuxracer")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/extremetuxracer/releases/"
                    version "/etr-" version ".tar.xz"))
              (sha256
               (base32
                "0hc3qd9hv3h9qm53yxgc7iy1v1wyajwxyvil4vqvzf9ascz9dnlj"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list glu sfml))
    (synopsis "High-speed arctic racing game based on Tux Racer")
    ;; Snarfed straight from Debian.
    (description "Extreme Tux Racer, or etracer as it is called for short, is
a simple OpenGL racing game featuring Tux, the Linux mascot.  The goal of the
game is to slide down a snow- and ice-covered mountain as quickly as possible,
avoiding the trees and rocks that will slow you down.

Collect herrings and other goodies while sliding down the hill, but avoid fish
bones.

This game is based on the GPL version of the famous game TuxRacer.")
    (home-page "https://sourceforge.net/projects/extremetuxracer/")
    (license license:gpl2+)))

(define-public supertuxkart
  (package
    (name "supertuxkart")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/supertuxkart/stk-code/"
                           "releases/download/"
                           version "/SuperTuxKart-" version "-src.tar.xz"))
       (sha256
        (base32
         "1z9z13zarv28h4jrmjna5hr6m9266pm7c2kgiwhqls01k06ypazf"))
       (modules '((guix build utils)))
       (snippet
        ;; Delete bundled library sources
        '(begin
           ;; Supertuxkart uses modified versions of the Irrlicht engine
           ;; and the bullet library.  The developers gave an explanation
           ;; here: http://forum.freegamedev.net/viewtopic.php?f=17&t=3906
           ;; FIXME: try to unbundle angelscript, libmcpp and libraqm
           (for-each delete-file-recursively
                     '("lib/dnsc"
                       "lib/enet"
                       "lib/mojoal"
                       "lib/wiiuse"))
           #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:configure-flags
       (list "-DUSE_WIIUSE=0"
             "-DUSE_SYSTEM_ENET=TRUE"
             "-DUSE_CRYPTO_OPENSSL=TRUE"
             ;; In order to use the system ENet library, IPv6 support (added in
             ;; SuperTuxKart version 1.1) must be disabled.
             "-DUSE_IPV6=FALSE"
             ;; FIXME: needs libopenglrecorder
             "-DBUILD_RECORDER=0")))
    (inputs
     `(("curl" ,curl)
       ("freetype" ,freetype)
       ("fribidi" ,fribidi)
       ("glew" ,glew)
       ("harfbuzz" ,harfbuzz)
       ("libvorbis" ,libvorbis)
       ("libx11" ,libx11)
       ("libxrandr" ,libxrandr)
       ("mesa" ,mesa)
       ("openal" ,openal)
       ("sdl2" ,sdl2)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)
       ;; The following input is needed to build the bundled and modified
       ;; version of irrlicht.
       ("enet" ,enet)
       ("libjpeg" ,libjpeg-turbo)
       ("openssl" ,openssl)))
    (native-inputs
     (list pkg-config))
    (home-page "https://supertuxkart.net/Main_Page")
    (synopsis "3D kart racing game")
    (description "SuperTuxKart is a 3D kart racing game, with a focus on
having fun over realism.  You can play with up to 4 friends on one PC, racing
against each other or just trying to beat the computer; single-player mode is
also available.")
    (license license:gpl3+)))

(define-public unknown-horizons
  (package
    (name "unknown-horizons")
    (version "2019.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://codeload.github.com/unknown-horizons/"
                                  "unknown-horizons/tar.gz/" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1n747p7h0qp48szgp262swg0xh8kxy1bw8ag1qczs4i26hyzs5x4"))
              (patches (search-patches "unknown-horizons-python-3.8-distro.patch"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-HOME
           (lambda _
             (setenv "HOME" "/tmp")
             #t))
         (add-after 'build 'build-extra
           (lambda _
             (invoke "python3" "./setup.py" "build_i18n")
             (invoke "python3" "horizons/engine/generate_atlases.py" "2048")
             #t))
         (add-after 'install 'patch
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* (string-append out "/bin/unknown-horizons")
                 (("os\\.chdir\\(get\\_content\\_dir\\_parent_path\\(\\)\\)")
                  (string-append "os.chdir(\""
                                 (assoc-ref outputs "out")
                                 "/share/unknown-horizons\")"))))
             #t))
         (add-before 'check 'fix-tests-with-pytest>=4
           (lambda _
             (substitute* "tests/conftest.py"
               (("pytest_namespace")
                "pytest_configure")
               (("get_marker")
                "get_closest_marker"))
             #t))
         ;; TODO: Run GUI tests as well
         (replace 'check
           (lambda _
             (substitute* "horizons/constants.py"
               (("IS_DEV_VERSION = False")
                "IS_DEV_VERSION = True"))
             (invoke "pytest" "tests")
             (substitute* "horizons/constants.py"
               (("IS_DEV_VERSION = True")
                "IS_DEV_VERSION = False"))
             #t)))))
    (inputs
     `(("fifengine" ,fifengine)
       ("python:tk" ,python "tk")
       ("python-pillow" ,python-pillow)
       ("python-pyyaml" ,python-pyyaml)))
    (native-inputs
     (list intltool
           python-distro
           ;; Required for tests
           python-greenlet
           python-polib
           python-pytest
           python-pytest-mock))
    (home-page "https://unknown-horizons.org/")
    (synopsis "Isometric realtime strategy, economy and city building simulation")
    (description
     "Unknown Horizons is a 2D realtime strategy simulation with an emphasis
on economy and city building.  Expand your small settlement to a strong and
wealthy colony, collect taxes and supply your inhabitants with valuable
goods.  Increase your power with a well balanced economy and with strategic
trade and diplomacy.")
    (license (list
              license:gpl2+        ; Covers code
              license:expat        ; tests/dummy.py, ext/polib.py
              license:cc-by-sa3.0  ; Covers some media content
              license:cc-by3.0     ; Covers some media content
              license:bsd-3))))    ; horizons/ext/speaklater.py

(define-public gnujump
  (package
    (name "gnujump")
    (version "1.0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gnujump/gnujump-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "05syy9mzbyqcfnm0hrswlmhwlwx54f0l6zhcaq8c1c0f8dgzxhqk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'link-libm
          (lambda _ (setenv "LIBS" "-lm")))
         (add-after 'install 'create-desktop-entry
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (apps (string-append out "/share/applications")))
               (mkdir-p apps)
               (with-output-to-file
                 (string-append apps "/gnujump.desktop")
                 (lambda _
                   (format #t
                           "[Desktop Entry]~@
                           Name=GNUjump~@
                           Comment=Jump up the tower to survive~@
                           Exec=~a/bin/gnujump~@
                           Terminal=false~@
                           Type=Application~@
                           Categories=Game;ArcadeGame~%"
                           out)))))))))
    (inputs
     (list glu mesa sdl sdl-image sdl-mixer))
    (home-page "http://gnujump.es.gnu.org/")
    (synopsis
     "Game of jumping to the next floor, trying not to fall")
    (description
     "GNUjump is a simple, yet addictive game in which you must jump from
platform to platform to avoid falling, while the platforms drop at faster rates
the higher you go.  The game features multiplayer, unlimited FPS, smooth floor
falling, themeable graphics and sounds, and replays.")
    (license license:gpl3+)))

(define-public wesnoth
  (package
    (name "wesnoth")
    (version "1.16.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/wesnoth/wesnoth-"
                                  (version-major+minor version)
                                  "/wesnoth-" version "/"
                                  "wesnoth-" version ".tar.bz2"))
              (sha256
               (base32
                "0cyrwmdg93pqpdm7030540jznaky9rda355i9ym8am4k9civlcwf"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ;no check target
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list boost
           dbus
           fribidi
           libvorbis
           openssl
           pango
           (sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf))))
    (home-page "https://www.wesnoth.org/")
    (synopsis "Turn-based strategy game")
    (description
     "The Battle for Wesnoth is a fantasy, turn based tactical strategy game,
with several single player campaigns, and multiplayer games (both networked and
local).

Battle for control on a range of maps, using variety of units which have
advantages and disadvantages against different types of attacks.  Units gain
experience and advance levels, and are carried over from one scenario to the
next campaign.")
    (license license:gpl2+)))

(define-public wesnoth-server
  (package
    (inherit wesnoth)
    (name "wesnoth-server")
    (inputs
     (list boost icu4c openssl sdl2))
    (arguments
     `(#:configure-flags '("-DENABLE_GAME=OFF")
       ,@(package-arguments wesnoth)))
    (synopsis "Dedicated @emph{Battle for Wesnoth} server")
    (description "This package contains a dedicated server for @emph{The
Battle for Wesnoth}.")))

(define-public gamine
  (package
    (name "gamine")
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gamine-game/"
                                  "gamine-" version ".tar.gz"))
              (sha256
               (base32
                "1sc6f4445ciigd6yw0ri92746k4hk6ps0bvj9fm1gbp3c3fslk5n"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config intltool))
    (inputs
     (list gstreamer
           gst-plugins-base ; playbin plugin
           gst-plugins-good ; for wav playback
           gtk+))
    (arguments
     `(#:tests? #f
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "PREFIX=" out)
               (string-append "SYSCONFDIR=" out "/etc")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after
          'install 'wrap-gamine
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out             (assoc-ref outputs "out"))
                  (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH")))
              (wrap-program (string-append out "/bin/gamine")
                `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))))
            #t)))))
    (home-page "http://gamine-game.sourceforge.net/")
    (synopsis "Mouse and keyboard discovery for children")
    (description
     "Gamine is a game designed for young children who are learning to use the
mouse and keyboard.  The child uses the mouse to draw colored dots and lines
on the screen and keyboard to display letters.")
    ;; Most files under gpl2+ or gpl3+, but eat.wav under gpl3
    (license license:gpl3)))

(define-public manaplus
  (package
    (name "manaplus")
    (version "1.9.3.23")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://repo.manaplus.org/manaplus/download/"
                    version "/manaplus-" version ".tar.xz"))
              (sha256
               (base32
                "1ky182p4svwdqm6cf7jbns85hidkhkhq4s17cs2p381f0klapfjz"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "CPPFLAGS=-I"
                            (assoc-ref %build-inputs "sdl-union")
                            "/include/SDL"))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list glu curl libxml2 mesa
           (sdl-union)))
    (home-page "https://manaplus.org")
    (synopsis "Client for 'The Mana World' and similar games")
    (description
     "ManaPlus is a 2D MMORPG client for game servers.  It is the only
fully supported client for @uref{http://www.themanaworld.org, The mana
world}, @uref{http://evolonline.org, Evol Online} and
@uref{http://landoffire.org, Land of fire}.")
    ;; "src/debug/*" and "src/sdl2gfx/*" are under Zlib.
    ;; "data/themes/{golden-delicious,jewelry}/*" are under CC-BY-SA.
    ;; The rest is under GPL2+.
    (license (list license:gpl2+ license:zlib license:cc-by-sa4.0))))

(define openttd-engine
  (package
    (name "openttd-engine")
    (version "12.0")
    (source
     (origin (method url-fetch)
             (uri (string-append "https://cdn.openttd.org/openttd-releases/"
                                 version "/openttd-" version "-source.tar.xz"))
             (sha256
              (base32
               "1p1j5cf4ry57dcgm7qx2g2s00z1c6qgjabb4kqjp00yz00wgv85v"))))
    (build-system cmake-build-system)
    (inputs
     `(("allegro" ,allegro)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("icu4c" ,icu4c)
       ("libpng" ,libpng)
       ("lzo" ,lzo)
       ("sdl" ,sdl)
       ("xz" ,xz)
       ("zlib" ,zlib)))
    (synopsis "Transportation economics simulator game")
    (description "OpenTTD is a game in which you transport goods and
passengers by land, water and air.  It is a re-implementation of Transport
Tycoon Deluxe with many enhancements including multiplayer mode,
internationalization support, conditional orders and the ability to clone,
autoreplace and autoupdate vehicles.  This package only includes the game
engine.  When you start it you will be prompted to download a graphics set.")
    (home-page "https://www.openttd.org/")
    ;; This package is GPLv2, except for a few files located in
    ;; "src/3rdparty/" which are under the 3-clause BSD, LGPLv2.1+ and Zlib
    ;; licenses.  In addition, this software contains an in-game downloader
    ;; from which the user may find non-functional data licensed under
    ;; different terms.
    (license (list license:bsd-3 license:gpl2 license:lgpl2.1+ license:zlib))))

(define openttd-opengfx
  (package
    (name "openttd-opengfx")
    (version "7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://cdn.openttd.org/opengfx-releases/"
                           version "/opengfx-" version "-source.tar.xz"))
       (sha256
        (base32
         "0nhzlk6s73qvznm5fdwcs1b42g2plf26s5ag39fvck45zm7m48jk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "INSTALL_DIR="
                                         (assoc-ref %outputs "out")
                                         "/share/games/openttd/baseset/opengfx"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             ;; Make sure HOME is writable for GIMP.
             (setenv "HOME" (getcwd))

             ;; Redirect stdout, not stderr, to /dev/null. This prevents
             ;; dos2unix from receiving its version information as a flag.
             (substitute* "Makefile"
               (("\\$\\(UNIX2DOS\\) -q --version 2>/dev/null")
                "$(UNIX2DOS) -q --version 1>/dev/null")))))
       ;; The check phase for this package only checks the md5sums of the built
       ;; GRF files against the md5sums of the release versions. Because we use
       ;; different software versions than upstream does, some of the md5sums
       ;; are different. However, the package is still reproducible, it's safe
       ;; to disable this test.
       #:tests? #f
       #:parallel-build? #f))
    (native-inputs `(("dos2unix" ,dos2unix)
                     ("gimp" ,gimp)
                     ("grfcodec" ,grfcodec)
                     ("nml" ,nml)
                     ("which" ,which)
                     ("python" ,python)))
    (home-page "http://dev.openttdcoop.org/projects/opengfx")
    (synopsis "Base graphics set for OpenTTD")
    (description
     "The OpenGFX project is an implementation of the OpenTTD base graphics
set that aims to ensure the best possible out-of-the-box experience.

OpenGFX provides you with...
@enumerate
@item All graphics you need to enjoy OpenTTD.
@item Uniquely drawn rail vehicles for every climate.
@item Completely snow-aware rivers.
@item Different river and sea water.
@item Snow-aware buoys.
@end enumerate")
    (license license:gpl2)))

(define openttd-opensfx
  (package
    (name "openttd-opensfx")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://cdn.openttd.org/opensfx-releases/"
             version "/opensfx-" version "-source.tar.xz"))
       (sha256
        (base32
         "0aym026lg0r7dp3jxxs9c0rj8lwy1fz3v9hmk3mml6sycsg3fv42"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("catcodec" ,catcodec)
       ("python" ,python)
       ("tar" ,tar)))
    (arguments
     `(#:make-flags
       (list (string-append "DIR_NAME=opensfx")
             (string-append "TAR="
                            (search-input-file %build-inputs "/bin/tar")))
       ;; The check phase only verifies md5sums, see openttd-opengfx.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             ;; Remove the time dependency of the installed tarball by setting
             ;; the modification times if its members to 0.
             (substitute* "scripts/Makefile.def"
               (("-cf") " --mtime=@0 -cf"))
             #t))
         (delete 'configure)
         (add-before 'build 'prebuild
           (lambda _ (invoke "make" "opensfx.cat")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (copy-recursively "opensfx"
                               (string-append (assoc-ref outputs "out")
                                              "/share/games/openttd/baseset"
                                              "/opensfx")))))))
    (home-page "http://dev.openttdcoop.org/projects/opensfx")
    (synopsis "Base sounds for OpenTTD")
    (description "OpenSFX is a set of free base sounds for OpenTTD which make
it possible to play OpenTTD without requiring the proprietary sound files from
the original Transport Tycoon Deluxe.")
    (license license:cc-by-sa3.0)))

(define openttd-openmsx
  (package
    (name "openttd-openmsx")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://cdn.openttd.org/openmsx-releases/"
             version "/openmsx-" version "-source.tar.xz"))
       (sha256
        (base32
         "0h583d8fxy78kc3jvpp78r76a48qhxrhm4q7jbnj74aw0kwrcl8g"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("grfcodec" ,grfcodec)
       ; Scripts are Python3 compatible, but call the interpreter as
       ; python instead of python3.
       ("python" ,python-wrapper)
       ("tar" ,tar)))
    (arguments
     `(#:make-flags
       (list (string-append "DIR_NAME=openmsx")
             (string-append "TAR="
                            (search-input-file %build-inputs "/bin/tar")))
       ;; The check phase only verifies md5sums, see openttd-opengfx.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (copy-recursively "openmsx"
                               (string-append (assoc-ref outputs "out")
                                              "/share/games/openttd/baseset"
                                              "/openmsx")))))))
    (home-page "http://dev.openttdcoop.org/projects/openmsx")
    (synopsis "Music set for OpenTTD")
    (description "OpenMSX is a music set for OpenTTD which makes it possible
to play OpenTTD without requiring the proprietary music from the original
Transport Tycoon Deluxe.")
    (license license:gpl2)))

(define-public openttd
  (package
    (inherit openttd-engine)
    (name "openttd")
    (arguments
     `(#:configure-flags
       (let* ((out (assoc-ref %outputs "out")))
         (list (string-append "-DCMAKE_INSTALL_BINDIR=" out "/bin")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'install-data
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (for-each
              (lambda (input)
                (copy-recursively (assoc-ref inputs input)
                                  (assoc-ref outputs "out")))
              (list "opengfx" "openmsx" "opensfx")))))))
    (inputs
     (modify-inputs (package-inputs openttd-engine)
       (prepend timidity++)))
    (native-inputs
     `(("opengfx" ,openttd-opengfx)
       ("openmsx" ,openttd-openmsx)
       ("opensfx" ,openttd-opensfx)
       ,@(package-native-inputs openttd-engine)))))

(define openrct2-title-sequences
  (package
   (name "openrct2-title-sequences")
   (version "0.1.2")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/OpenRCT2/title-sequences/releases/download/v"
                         version "/title-sequence-v" version ".zip"))
     (file-name (string-append name "-" version ".zip"))
     (sha256
      (base32
       "0qbyxrsw8hlgaq0r5d7lx7an3idy4qbfv7yiw9byhldk763n9cfw"))))
   (build-system trivial-build-system)
   (native-inputs
    `(("bash" ,bash)
      ("coreutils" ,coreutils)
      ("unzip" ,unzip)))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let* ((out (assoc-ref %outputs "out"))
               (openrct2-title-sequences (string-append out
                                         "/share/openrct2/title-sequences"))
               (source (assoc-ref %build-inputs "source"))
               (unzip (search-input-file %build-inputs "/bin/unzip")))
          (copy-file source (string-append ,name "-" ,version ".zip"))
          (invoke unzip (string-append ,name "-" ,version ".zip"))
          (delete-file (string-append ,name "-" ,version ".zip"))
          (mkdir-p openrct2-title-sequences)
          (copy-recursively "."
                            openrct2-title-sequences)
          #t))))
   (home-page "https://github.com/OpenRCT2/OpenRCT2")
   (synopsis "Title sequences for OpenRCT2")
   (description
    "openrct2-title-sequences is a set of title sequences for OpenRCT2.")
   (license license:gpl3+)))

(define openrct2-objects
  (package
   (name "openrct2-objects")
   (version "1.0.20")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/OpenRCT2/objects/releases/download/v"
                         version "/objects.zip"))
     (file-name (string-append name "-" version ".zip"))
     (sha256
      (base32 "1q7a38kcwrfijav6app1gf253yfv8b0rljbkah8040y6i7snw9mw"))))
   (build-system trivial-build-system)
   (native-inputs
    `(("bash" ,bash)
      ("coreutils" ,coreutils)
      ("unzip" ,unzip)))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
      (begin
        (use-modules (guix build utils))
        (let* ((out (assoc-ref %outputs "out"))
               (openrct2-objects (string-append out
                                         "/share/openrct2/objects"))
               (source (assoc-ref %build-inputs "source"))
               (unzip (search-input-file %build-inputs "/bin/unzip")))
          (copy-file source (string-append ,name "-" ,version ".zip"))
          (invoke unzip (string-append ,name "-" ,version ".zip"))
          (delete-file (string-append ,name "-" ,version ".zip"))
          (mkdir-p openrct2-objects)
          (copy-recursively "."
                            openrct2-objects)
          #t))))
   (home-page "https://github.com/OpenRCT2/OpenRCT2")
   (synopsis "Objects for OpenRCT2")
   (description
    "openrct2-objects is a set of objects for OpenRCT2.")
   (license license:gpl3+)))

(define-public openrct2
  (package
    (name "openrct2")
    (version "0.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenRCT2/OpenRCT2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01nanpbz5ycdhkyd46fjfvj18sw729l4vk7xg12600f9rjngjk76"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DDOWNLOAD_OBJECTS=OFF"
                               "-DDOWNLOAD_TITLE_SEQUENCES=OFF")
       #:tests? #f                      ; tests require network access
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-usr-share-paths&add-data
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((titles (assoc-ref inputs "openrct2-title-sequences"))
                   (objects (assoc-ref inputs "openrct2-objects")))
               ;; Fix some references to /usr/share.
               (substitute* "src/openrct2/platform/Platform.Linux.cpp"
                 (("/usr/share")
                  (string-append (assoc-ref %outputs "out") "/share")))
               (copy-recursively
                (string-append titles "/share/openrct2/title-sequences")
                "data/title")
               (copy-recursively
                (string-append objects "/share/openrct2/objects")
                "data/object"))))
         (add-before 'configure 'get-rid-of-errors
           (lambda _
             ;; Don't treat warnings as errors.
             (substitute* "CMakeLists.txt"
               (("-Werror") ""))
             #t)))))
    (inputs `(("curl" ,curl)
              ("duktape" ,duktape)
              ("fontconfig" ,fontconfig)
              ("freetype" ,freetype)
              ("icu4c" ,icu4c)
              ("jansson" ,jansson)
              ("json-modern-cxx" ,json-modern-cxx)
              ("libpng" ,libpng)
              ("libzip" ,libzip)
              ("mesa" ,mesa)
              ("openrct2-objects" ,openrct2-objects)
              ("openrct2-title-sequences" ,openrct2-title-sequences)
              ("openssl" ,openssl)
              ("sdl2" ,sdl2)
              ("speexdsp" ,speexdsp)
              ("zlib" ,zlib)))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/OpenRCT2/OpenRCT2")
    (synopsis "Free software re-implementation of RollerCoaster Tycoon 2")
    (description "OpenRCT2 is a free software re-implementation of
RollerCoaster Tycoon 2 (RCT2).  The gameplay revolves around building and
maintaining an amusement park containing attractions, shops and facilities.

Note that this package does @emph{not} provide the game assets (sounds,
images, etc.)")
    ;; See <https://github.com/OpenRCT2/OpenRCT2/wiki/Required-RCT2-files>
    ;; regarding assets.
    (license license:gpl3+)))

(define-public pinball
  (package
    (name "pinball")
    (version "0.3.20201218")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/adoptware/pinball")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "056jk98v6zlkrj9vjm06p0pmpnav1x658n6qw10v5klg5gr6ldf7"))
             (patches (search-patches "pinball-system-ltdl.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config libtool))
    (inputs
     (list glu
           libltdl
           mesa
           sdl
           sdl-image
           sdl-mixer))
    (arguments
     '(#:configure-flags
       ;; Configure tries to use pkg-config, but falls short, so:
       (list (string-append "CPPFLAGS=-I"
                            (assoc-ref %build-inputs "sdl-image")
                            "/include/SDL -I"
                            (assoc-ref %build-inputs "sdl-mixer")
                            "/include/SDL"))
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           ;; The `bootstrap` script tries to call a script with
           ;; `/usr/bin/make` in the shebang, but ultimately does the same as
           ;; autoreconf would do, so just use that.
           (lambda _
             (symlink "README.md" "README")
             (display (which "autoreconf")) (newline)
             (invoke "autoreconf" "-vif"))))))
    (home-page "http://pinball.sourceforge.net")
    (synopsis "Pinball simulator")
    (description "The Emilia Pinball Project is a pinball simulator.  There
are only two levels to play with, but they are very addictive.")
    (license license:gpl2)))

(define-public pioneers
  (package
    (name "pioneers")
    (version "15.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://downloads.sourceforge.net/pio/"
                                  "pioneers-" version ".tar.gz"))
              (sha256
               (base32
                "07b3xdd81n8ybsb4fzc5lx0813y9crzp1hj69khncf4faj48sdcs"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Fixes https://issues.guix.gnu.org/47131.
         (add-after 'unpack 'patch-beep-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "client/gtk/audio.c"
               (("\"beep\"")
                (string-append "\"" (assoc-ref inputs "beep") "/bin/beep\"")))
             #t)))))
    (inputs (list avahi beep gtk+ librsvg))
    (native-inputs (list intltool itstool libxml2 pkg-config))
    (synopsis "Board game inspired by The Settlers of Catan")
    (description "Pioneers is an emulation of the board game The Settlers of
Catan.  It can be played on a local network, on the internet, and with AI
players.")
    (home-page "http://pio.sourceforge.net/")
    (license license:gpl2+)))

(define-public einstein
  (package
    (name "einstein")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://debian/pool/main/e/"
                                  "einstein/einstein_2.0.dfsg.2.orig.tar.gz"))
              (sha256
               (base32
                "1hxrlv6n8py48j487i6wbb4n4vd55w0na69r7ccmmr9vmrsw5mlk"))
              (patches (search-patches "einstein-build.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("freetype" ,freetype)
       ("sdl" ,(sdl-union (list sdl sdl-mixer sdl-ttf)))
       ("zlib" ,zlib)))
    (native-inputs
     (list font-dejavu))
    (arguments
     `(#:tests? #f ; no check target
       #:phases
        (modify-phases %standard-phases
          (replace 'configure
          (lambda* (#:key outputs inputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (dejavu (search-input-file
                           inputs "/share/fonts/truetype/DejaVuSans.ttf")))
              (substitute* "Makefile"
                (("PREFIX=/usr/local") (string-append "PREFIX=" out)))
              ;; The patch above registers a free font for use by the binary,
              ;; but the font is copied during the compile phase into a
              ;; resources file, so we need to make the ttf file available.
              (symlink dejavu "res/DejaVuSans.ttf")
              #t))))))
    (synopsis "Logic puzzle game")
    (description "The goal of this logic game is to open all cards in a 6x6
grid, using a number of hints as to their relative position.  The game idea
is attributed to Albert Einstein.")
    ;; The original home page has disappeared.
    (home-page (string-append "http://web.archive.org/web/20120521062745/"
                              "http://games.flowix.com/en/index.html"))
    ;; License according to
    ;; http://web.archive.org/web/20150222180355/http://www.babichev.info/en/projects/index.html
    ;; The source code is a DFSG-sanitized tarball and does not contain any
    ;; license information.
    (license license:gpl3+)))

(define-public powwow
  (package
    (name "powwow")
    (version "1.2.23")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.hoopajoo.net/static/projects/powwow-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1wkl6j91pp40aps2hhnsv0bndgq49smfffws4hqcn7847bpnwwm6"))))
    (inputs
     (list ncurses))
    (build-system gnu-build-system)
    (home-page "https://www.hoopajoo.net/projects/powwow.html")
    (synopsis "MUD and telnet client")
    (description
     "POWWOW is a client software which can be used for telnet as well as for
@dfn{Multi-User Dungeon} (MUD).  Additionally it can serve as a nice client for
the chat server psyced with the specific config located at
http://lavachat.symlynx.com/unix/")
    (license license:gpl2+)))

(define-public red-eclipse
  (let ((release "2.0.0")
        (revision 0))
    (package
      (name "red-eclipse")
      (version (if (zero? revision)
                   release
                   (string-append release "-"
                                  (number->string revision))))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/redeclipse/base")
               (commit (string-append "v" release))
               (recursive? #t))) ; for game data
         (file-name (git-file-name name version))
         (sha256
          (base32 "0sz0mqhwx8r9n4mk3qrxw420nlsm3y0n48gd0lazgd64lfqjh3ab"))
         (modules '((guix build utils)))
         (snippet
          ;; Remove proprietary libraries and other pre-compiled binaries.
          '(begin
             (delete-file-recursively "bin")
             #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f            ; no check target
         #:make-flags (list "CC=gcc" "-Csrc"
                            (string-append "INSTDIR="
                                           (assoc-ref %outputs "out") "/bin")
                            (string-append "prefix="
                                           (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'add-store-data-package-path-as-default
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "src/engine/server.cpp"
                 (("data = \"data\"")
                  (string-append "data = \""
                                 (assoc-ref outputs "out")
                                 "/share/redeclipse/data\"")))
               #t))
           (delete 'configure)  ; no configure script
           (add-after 'set-paths 'set-sdl-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "CPATH"
                       (string-append
                        (search-input-directory inputs "/include/SDL2")
                        ":" (or (getenv "CPATH") "")))))
           (add-after 'install 'copy-data
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (copy-recursively "config"
                                   (string-append out "/config"))
                 (copy-file "doc/examples/servinit.cfg"
                            (string-append out "/config/servinit.cfg"))
                 (copy-recursively "data"
                                   (string-append out "/share/redeclipse/data"))
                 (mkdir-p (string-append out "/lib/redeclipse"))
                 (symlink (string-append out "/share/redeclipse/data")
                          (string-append out "/lib/redeclipse/data")))
               #t))
           (add-after 'copy-data 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (with-directory-excursion bin
                   (rename-file "redeclipse_linux"
                                ".redeclipse_linux-real")
                   (rename-file "redeclipse_server_linux"
                                ".redeclipse_server_linux-real")
                   (call-with-output-file "redeclipse_linux"
                     (lambda (port)
                       (format port "#!~a/bin/sh
# Run the thing from its home, otherwise it just bails out.
cd \"~a\"
exec -a \"$0\" ~a/.redeclipse_linux-real~%"
                               (assoc-ref inputs "bash") ;implicit input
                               (string-append out)
                               (string-append bin))))
                   (call-with-output-file "redeclipse_server_linux"
                     (lambda (port)
                       (format port "#!~a/bin/sh
# Run the thing from its home, otherwise it just bails out.
cd \"~a\"
exec -a \"$0\" ~a/.redeclipse_server_linux-real~%"
                               (assoc-ref inputs "bash") ;implicit input
                               (string-append out)
                               (string-append bin))))
                   (chmod "redeclipse_linux" #o555)
                   (chmod "redeclipse_server_linux" #o555)))
               #t)))))
      (native-inputs
       (list pkg-config))
      (inputs
       (list curl freetype glu
             (sdl-union (list sdl2 sdl2-image sdl2-mixer))))
      (home-page "https://redeclipse.net/")
      (synopsis "Arena shooter derived from the Cube 2 engine")
      (description
       "Red Eclipse is an arena shooter, created from the Cube2 engine.
Offering an innovative parkour system and distinct but all potent weapons,
Red Eclipse provides fast paced and accessible gameplay.")
      ;; The engine is under Zlib; data files are covered by the other
      ;; licenses.  More details at file:///doc/all-licenses.txt.
      (license (list license:expat
                     license:zlib
                     license:cc-by-sa4.0
                     license:cc-by-sa3.0
                     license:cc-by3.0
                     license:cc0
                     license:public-domain
                     license:silofl1.1)))))

(define-public grue-hunter
  (package
    (name "grue-hunter")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://jxself.org/grue-hunter.tar.gz"))
              (sha256
               (base32
                "1hjcpy5439qs3v2zykis7hsi0i17zjs62gks3zd8mnfw9ni4i2h3"))))
    (build-system trivial-build-system) ; no Makefile.PL
    (arguments `(#:modules ((guix build utils))
                 #:builder
                 (begin
                   (use-modules (guix build utils))
                   (use-modules (srfi srfi-1))

                   (let* ((tarball (assoc-ref %build-inputs "tarball"))
                          (perl    (string-append (assoc-ref %build-inputs
                                                             "perl")
                                                  "/bin"))
                          (gzip    (string-append (assoc-ref %build-inputs
                                                             "gzip")
                                                  "/bin/gzip"))
                          (tar     (string-append (assoc-ref %build-inputs
                                                             "tar")
                                                  "/bin/tar"))
                          (out     (assoc-ref %outputs "out"))
                          (bin     (string-append out "/bin"))
                          (doc     (string-append out
                                                  "/share/doc/grue-hunter")))
                     (copy-file tarball "grue-hunter.tar.gz")
                     (invoke gzip "-d" "grue-hunter.tar.gz")
                     (invoke tar "xvf" "grue-hunter.tar")

                     (mkdir-p bin)
                     (copy-file "grue-hunter/gh.pl"
                                (string-append bin "/grue-hunter"))
                     (patch-shebang (string-append bin "/grue-hunter")
                                    (list perl))

                     (install-file "grue-hunter/AGPLv3.txt" doc)

                     #t))))
    (inputs `(("perl" ,perl)
              ("tar" ,tar)
              ("gzip" ,gzip)
              ("tarball" ,source)))
    (home-page "https://jxself.org/grue-hunter.shtml")
    (synopsis "Text adventure game")
    (description
     "Grue Hunter is a text adventure game written in Perl.  You must make
your way through an underground cave system in search of the Grue.  Can you
capture it and get out alive?")
    (license license:agpl3+)))

(define-public lierolibre
  (package
    (name "lierolibre")
    (version "0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://launchpad.net/lierolibre/trunk/"
                                  version "/+download/lierolibre-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1cf1gvsn4qq190lrf9k5bpjnqwlcfw7pajvdnh7z5r4jqw0rsbl9"))
              (patches
               (search-patches "lierolibre-check-unaligned-access.patch"
                               "lierolibre-try-building-other-arch.patch"
                               "lierolibre-remove-arch-warning.patch"
                               "lierolibre-newer-libconfig.patch"
                               "lierolibre-is-free-software.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete pre-compiled files.
                  (delete-file "data/LIERO.CHR")
                  (delete-file "data/LIERO.SND")
                  #t))))
    (build-system gnu-build-system)
    (native-inputs
     (list imagemagick pkg-config util-linux sox))
    (inputs
     (list boost libconfig
           (sdl-union (list sdl sdl-image sdl-mixer)) zlib))
    (home-page "https://gitlab.com/lierolibre/lierolibre")
    (synopsis "Old-school earthworm action game")
    (description
     "lierolibre is an earthworm action game where you fight another player
(or the computer) underground using a wide array of weapons.

Features:
@itemize
@item 2 worms, 40 weapons, great playability, two game modes: Kill'em All
and Game of Tag, plus AI-players without true intelligence!
@item Dat nostalgia.
@item Extensions via a hidden F1 menu:
@itemize
@item Replays
@item Game controller support
@item Powerlevel palettes
@end itemize
@item Ability to write game variables to plain text files.
@item Ability to load game variables from both EXE and plain text files.
@item Scripts to extract and repack graphics, sounds and levels.
@end itemize

To switch between different window sizes, use F6, F7 and F8, to switch to
fullscreen, use F5 or Alt+Enter.")
    ;; Code mainly BSD-2, some parts under Boost 1.0. All assets are WTFPL2.
    (license (list license:bsd-2 license:boost1.0 license:wtfpl2))))

(define-public tennix
  (package
    (name "tennix")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://repo.or.cz/tennix.git")
             (commit (string-append "tennix-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02cj4lrdrisal5s9pnbf2smx7qz9czczjzndfkhfx0qy67b957sk"))
       ;; Remove non-free images.
       (modules '((guix build utils)))
       (snippet
        '(begin
           (for-each delete-file
                     '("data/loc_training_camp.png"
                       "data/loc_austrian_open.png"
                       "data/loc_olympic_green_tennis.png"))
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-include
           (lambda _
             (substitute* '("src/graphics.h" "src/sound.h")
               (("#include \"(SDL_(image|ttf|mixer)\\.h)\"" _ header)
                (string-append "#include \"SDL/" header "\"")))
             (substitute* '("src/tennix.h" "src/network.h" "src/SDL_rotozoom.h")
               (("#include <SDL.h>") "#include <SDL/SDL.h>")
               (("#include <SDL_net.h>") "#include <SDL/SDL_net.h>"))
             #t))
         (add-after 'unpack 'locate-install
           ;; Build process cannot expand "$(INSTALL)" in Makefile.
           (lambda _
             (substitute* "makefile"
               (("^CONFIGURE_OUTPUT :=.*" all)
                (string-append "INSTALL := install -c\n" all)))
             #t))
         (replace 'configure
           ;; The "configure" script is picky about the arguments it
           ;; gets.  Call it ourselves.
           (lambda _
             (invoke "./configure" "--prefix" (assoc-ref %outputs "out")))))))
    (native-inputs
     (list which))
    (inputs
     `(("python" ,python-wrapper)
       ("sdl" ,(sdl-union (list sdl sdl-image sdl-mixer sdl-ttf sdl-net)))))
    (home-page "http://icculus.org/tennix/")
    (synopsis "Play tennis against the computer or a friend")
    (description "Tennix is a 2D tennis game.  You can play against the
computer or against another player using the keyboard.  The game runs
in-window at 640x480 resolution or fullscreen.")
    ;; Project is licensed under GPL2+ terms.  It includes images
    ;; released under Public Domain terms, and SDL_rotozoom, released
    ;; under LGPL2.1 terms.
    (license (list license:gpl2+ license:public-domain license:lgpl2.1))))

(define-public warzone2100
  (package
    (name "warzone2100")
    (version "4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/warzone2100/releases/"
                           version
                           "/warzone2100_src.tar.xz"))
       (sha256
        (base32 "1f8a4kflslsjl8jrryhwg034h1yc9y3y1zmllgww3fqkz3aj4xik"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (with-directory-excursion "3rdparty"
             (for-each
              delete-file-recursively
              '("discord-rpc"
                "miniupnp"
                "utfcpp")))
             #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DWZ_DISTRIBUTOR=Guix"
                           "-DWZ_ENABLE_BACKEND_VULKAN=off"
                           "-DENABLE_DISCORD=off")
       #:tests? #f ; TODO: Tests seem to be broken, configure.ac is missing.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-utfcpp-include
           (lambda _
             (substitute* "lib/framework/wzstring.cpp"
               (("<utfcpp/source/utf8.h>") "<utf8.h>"))
             #t))
         (add-after 'unpack 'link-tests-with-qt
           (lambda _
             (substitute* "tests/Makefile.am"
               (("(framework_linktest_LDADD|maptest_LDADD) = " prefix)
                (string-append prefix "$(QT5_LIBS) ")))
             #t))
         (add-after 'unpack 'fix-ivis-linktest
           (lambda _
             (substitute* "tests/ivis_linktest.cpp"
               (("iV_DrawTextRotated.*;")
                (string-append "iV_DrawTextRotated(\"Press ESC to exit.\", "
                               "100, 100, 0.0f, font_regular);")))
             #t)))))
    (native-inputs `(("asciidoc" ,asciidoc)
                     ("asciidoctor" ,ruby-asciidoctor)
                     ("gettext" ,gettext-minimal)
                     ("pkg-config" ,pkg-config)
                     ("unzip" ,unzip)
                     ;; 7z is used to create .zip archive, not `zip' as in version 3.2.*.
                     ("p7zip" ,p7zip)))
    (inputs `(("curl" ,curl)
              ("fontconfig" ,fontconfig)
              ("freetype" ,freetype)
              ("glew" ,glew)
              ("harfbuzz" ,harfbuzz)
              ("libtheora" ,libtheora)
              ("libvorbis" ,libvorbis)
              ("libxrandr" ,libxrandr)
              ("libsodium" ,libsodium)
              ("miniupnpc" ,miniupnpc)
              ("openal" ,openal)
              ("physfs" ,physfs)
              ("qtbase" ,qtbase-5)
              ("qtscript" ,qtscript)
              ("openssl" ,openssl)
              ("sdl2" ,sdl2)
              ("sqlite" ,sqlite)
              ("utfcpp" ,utfcpp)))
    (home-page "https://wz2100.net")
    (synopsis "3D Real-time strategy and real-time tactics game")
    (description
     "Warzone 2100 offers campaign, multi-player, and single-player skirmish
modes.  An extensive tech tree with over 400 different technologies, combined
with the unit design system, allows for a wide variety of possible units and
tactics.")
    ;; Everything is GPLv2+ unless otherwise specified in COPYING.NONGPL
    (license (list license:bsd-3
                   license:cc0
                   license:cc-by-sa3.0
                   license:expat
                   license:gpl2+
                   license:lgpl2.1+))))

(define-public widelands
  (package
    (name "widelands")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/widelands/widelands")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hw51binnbia15mj1gzx1cbk3cw9r91sisqci2qzcy4ahxiadnw0"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "src/third_party/minizip")
           #t))
       (patches
        ;; Use system Minizip.  Patch is provided by Debian, and discussed
        ;; upstream at <https://github.com/widelands/widelands/issues/399>.
        (search-patches "widelands-system-wide_minizip.patch"
                        "widelands-add-missing-map-include.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (let* ((out (assoc-ref %outputs "out"))
              (share (string-append out "/share")))
         (list (string-append "-DCMAKE_INSTALL_PREFIX=" out "/bin")
               (string-append "-DWL_INSTALL_BASEDIR=" share "/widelands")
               (string-append "-DWL_INSTALL_DATADIR=" share "/widelands")
               "-DOPTION_BUILD_WEBSITE_TOOLS=OFF"
               ;; CMakeLists.txt does not handle properly RelWithDebInfo build
               ;; type.  When used, no game data is installed!
               "-DCMAKE_BUILD_TYPE=Release"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unbundle-fonts
           ;; Unbundle fonts already packaged in Guix.  XXX: missing fonts are
           ;; amiri, Culmus, mmrCensus, Nakula, and Sinhala.
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "data/i18n/fonts"
               (for-each (lambda (font)
                           (delete-file-recursively font)
                           (symlink (string-append (assoc-ref inputs font)
                                                   "/share/fonts/truetype")
                                    font))
                         '("DejaVu" "MicroHei")))
             #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("python" ,python-wrapper)))
    (inputs
     `(("curl" ,curl)
       ("boost" ,boost)
       ("glew" ,glew)
       ("icu4c" ,icu4c)
       ("libpng" ,libpng)
       ("minizip" ,minizip)
       ("sdl" ,(sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf)))
       ("zlib" ,zlib)
       ;; Fonts for the ‘unbundle-fonts’ phase.  Case matters in name!
       ("DejaVu" ,font-dejavu)
       ("MicroHei" ,font-wqy-microhei)))
    (home-page "https://www.widelands.org/")
    (synopsis "Fantasy real-time strategy game")
    (description
     "In Widelands, you are the regent of a small clan.  You start out with
nothing but your headquarters, where all your resources are stored.

In the course of the game, you will build an ever growing settlement.  Every
member of your clan will do his or her part to produce more resources---wood,
food, iron, gold and more---to further this growth.  The economic network is
complex and different in the five tribes (Barbarians, Empire, Atlanteans,
Frisians and Amazons).

As you are not alone in the world, you will meet other clans sooner or later.
Some of them may be friendly and you may eventually trade with them.  However,
if you want to rule the world, you will have to train soldiers and fight.

Widelands offers single-player mode with different campaigns; the campaigns
all tell stories of tribes and their struggle in the Widelands universe!
However, settling really starts when you unite with friends over the Internet
or LAN to build up new empires together---or to crush each other in the dusts
of war.  Widelands also offers an Artificial Intelligence to challenge you.")
    ;; Game is released as GPL2+.  Some parts, e.g., art, are released under
    ;; different licenses.
    (license (list license:gpl2+
                   license:expat           ;src/third_party/eris
                   license:silofl1.1       ;Widelands.ttf
                   license:cc-by-sa3.0)))) ;some music files

(define-public widelands-21
  (package
    (inherit widelands)
    (version "21")
    (properties `((superseded . ,widelands)))))

(define-public starfighter
  (package
    (name "starfighter")
    (version "2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/pr-starfighter/starfighter/releases"
                    "/download/v" version "/starfighter-"
                    version "-src.tar.gz"))
              (sha256
               (base32
                "0ips79j3sdy8wa64jqka0skbbqkzqiln9bbiiilh4z717q7vz9r5"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list pango sdl2 sdl2-image sdl2-mixer sdl2-ttf))
    (home-page "https://pr-starfighter.github.io/")
    (synopsis "2D scrolling shooter game")
    (description
     "In the year 2579, the intergalactic weapons corporation, WEAPCO, has
dominated the galaxy.  Guide Chris Bainfield and his friend Sid Wilson on
their quest to liberate the galaxy from the clutches of WEAPCO.  Along the
way, you will encounter new foes, make new allies, and assist local rebels
in strikes against the evil corporation.")
    ;; gfx and music are under CC-BY 3.0, CC-BY-SA 3.0, CC0 or Public Domain.
    (license (list license:gpl3+
                   license:cc-by3.0
                   license:cc-by-sa3.0
                   license:cc0
                   license:public-domain))))

(define-public chromium-bsu
  (package
    (name "chromium-bsu")
    (version "0.9.16.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/chromium-bsu"
                                  "/Chromium B.S.U. source code/"
                                  "chromium-bsu-" version ".tar.gz"))
              (sha256
               (base32
                "0jk2w5b6s6nkzri585bbz16cif2fhqcnl5l1mq3rd98r9nil3hd1"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list gettext-minimal glu quesoglc
                  (sdl-union (list sdl sdl-image sdl-mixer))))
    (home-page "http://chromium-bsu.sourceforge.net/")
    (synopsis "Fast-paced, arcade-style, top-scrolling space shooter")
    (description
     "In this game you are the captain of the cargo ship Chromium B.S.U. and
are responsible for delivering supplies to the troops on the front line.  Your
ship has a small fleet of robotic fighters which you control from the relative
safety of the Chromium vessel.")
    ;; Clarified Artistic License for everything but sound, which is covered
    ;; by the Expat License.
    (license (list license:clarified-artistic license:expat))))

(define-public tuxpaint
  (package
    (name "tuxpaint")
    (version "0.9.23")                  ;keep VER_DATE below in sync
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/tuxpaint/tuxpaint/"
                           version "/tuxpaint-" version ".tar.gz"))
       (sha256
        (base32
         "09k9pxi88r3dx6dyjwf9h85d4qpva4i29qz63dc558hg9v21k69l"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove win32 directory which contains binary dll's and the
           ;; deprecated visualc directory.
           (for-each delete-file-recursively '("win32" "visualc"))
           (substitute* "Makefile"
             ;; Do not rely on $(GPERF) being an absolute file name
             (("\\[ -x \\$\\(GPERF\\) \\]")
              "$(GPERF) --version >/dev/null 2>&1"))
           #t))
       (patches (search-patches "tuxpaint-stamps-path.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gperf pkg-config))
    (inputs
     `(("cairo" ,cairo)
       ("fribidi" ,fribidi)
       ("gettext" ,gettext-minimal)
       ("libpng" ,libpng)
       ("librsvg" ,librsvg)
       ("libpaper" ,libpaper)
       ("netpbm" ,netpbm)
       ("sdl" ,(sdl-union (list sdl sdl-mixer sdl-ttf sdl-image)))))
    ;; TODO: Use system fonts rather than those in data/fonts
    (arguments
     `(#:make-flags `("VER_DATE=2018-09-02"
                      "GPERF=gperf" "CC=gcc"
                      "SDL_PCNAME=sdl SDL_image SDL_mixer SDL_ttf"
                      ,(string-append "PREFIX=" %output)
                      "KDE_PREFIX=$(PREFIX)/share/applications"
                      "KDE_ICON_PREFIX=$(PREFIX)/share/icons/"
                      "COMPLETIONDIR=$(PREFIX)/etc/bash_completion.d")
       #:parallel-build? #f             ;fails on some systems
       #:tests? #f                      ;No tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure)   ;no configure phase
                  (add-before 'install 'no-sys-cache
                    (lambda _           ;do not rebuild system conf cache
                      (substitute* "Makefile"
                        (("kbuildsycoca4") ""))
                      #t))
                  (add-after 'install 'fix-import
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (net (assoc-ref inputs "netpbm"))
                             (tpi (string-append out "/bin/tuxpaint-import")))
                        (substitute* tpi
                          ;; Point to installation prefix so that the default
                          ;; configure file is found.
                          (("/usr/local") out))
                        ;; tuxpaint-import uses a bunch of programs from
                        ;; netpbm, so make sure it knows where those are
                        (wrap-program tpi
                          `("PATH" ":" prefix
                            (,(string-append net "/bin"))))))))))
    (native-search-paths
     (list (search-path-specification
            (variable "TUXPAINT_STAMPS_PATH")
            (files '("share/tuxpaint/stamps")))))
    (home-page "http://www.tuxpaint.org")
    (synopsis "Drawing software for children")
    (description
     "Tux Paint is a free drawing program designed for young children (kids
ages 3 and up).  It has a simple, easy-to-use interface; fun sound effects;
and an encouraging cartoon mascot who helps guide children as they use the
program.  It provides a blank canvas and a variety of drawing tools to help
your child be creative.")
    (license license:gpl2+)))

(define-public tuxpaint-stamps
  (package
    (name "tuxpaint-stamps")
    (version "2018.09.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/tuxpaint/tuxpaint-stamps/"
                           (string-map (λ (x) (if (eq? x #\.) #\- x)) version)
                           "/tuxpaint-stamps-" version ".tar.gz"))
       (sha256
        (base32
         "1skr23k27yj3vgwfazpzxp90lb2a278gxrkr3bxw7az6zpkmb3yp"))))
    (build-system trivial-build-system)
    (native-inputs
     (list tar gzip))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (setenv "PATH"
                           (string-append
                            (assoc-ref %build-inputs "tar") "/bin" ":"
                            (assoc-ref %build-inputs "gzip") "/bin"))
                   (invoke "tar" "xvf" (assoc-ref %build-inputs "source"))
                   (chdir (string-append ,name "-" ,version))
                   (let ((dir (string-append %output "/share/tuxpaint/stamps")))
                     (mkdir-p dir)
                     (copy-recursively "stamps" dir))
                   #t)))
    (home-page (package-home-page tuxpaint))
    (synopsis "Stamp images for Tux Paint")
    (description
     "This package contains a set of \"Rubber Stamp\" images which can be used
with the \"Stamp\" tool within Tux Paint.")
    (license license:gpl2+)))

(define-public tuxpaint-config
  (package
    (name "tuxpaint-config")
    (version "0.0.14")                  ;keep VER_DATE below in sync
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/tuxpaint/tuxpaint-config/"
                           version "/tuxpaint-config-" version ".tar.gz"))
       (sha256
        (base32
         "0zkgxk436nqcp43zghkfmh397c7dvh5bwn2as7gwvv208bzyij6g"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (inputs
     (list fltk
           libpaper
           ;; TODO: Should the following be propagated by fltk?
           libx11
           libxft
           mesa))
    (arguments
     `(#:make-flags `("VER_DATE=2018-09-01"
                      "CONFDIR=/etc/tuxpaint" ;don't write to store
                      ,(string-append "PREFIX=" %output)
                      "GNOME_PREFIX=$(PREFIX)")
       #:parallel-build? #f             ;race conditions
       #:tests? #f                      ;no tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure)   ;no configure phase
                  (add-before 'install 'gzip-no-name
                    (lambda* _
                      (substitute* "Makefile"
                        ;; tuxpaint-config compresses its own documentation;
                        ;; make sure it uses flags for reproducibility.
                        (("gzip") "gzip --no-name"))))
                  (add-before 'install 'make-install-dirs
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (mkdir-p (string-append out "/bin"))
                        #t))))))
    (home-page (package-home-page tuxpaint))
    (synopsis "Configure Tux Paint")
    (description
     "Tux Paint Config is a graphical configuration editor for Tux Paint.")
    (license license:gpl2)))            ;no "or later" present

(define-public supertux
  (package
   (name "supertux")
   (version "0.6.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/SuperTux/supertux/"
                                "releases/download/v" version "/SuperTux-v"
                                version "-Source.tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32
              "1xkr3ka2sxp5s0spp84iv294i29s1vxqzazb6kmjc0n415h0x57p"))
            (patches
             (search-patches "supertux-unbundle-squirrel.patch"))))
   (arguments
    '(#:tests? #f
      #:configure-flags '("-DINSTALL_SUBDIR_BIN=bin"
                          "-DUSE_SYSTEM_PHYSFS=ON")
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'patch-squirrel-path
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((squirrel (assoc-ref inputs "squirrel")))
              (substitute* "CMakeLists.txt"
                (("set\\(SQUIRREL_PREFIX.*")
                 (string-append "set(SQUIRREL_PREFIX " squirrel ")"))
                (("add_dependencies\\(supertux2_lib squirrel\\)") "")
                (("\\$\\{SQUIRREL_PREFIX\\}/include")
                 (string-append "${SQUIRREL_PREFIX}/include/squirrel"))))
            #t)))))
   (build-system cmake-build-system)
   (inputs (list boost
                 curl
                 freetype
                 glew
                 glm
                 libogg
                 libvorbis
                 mesa
                 openal
                 physfs
                 sdl2
                 sdl2-image
                 sdl2-mixer
                 squirrel))
   (native-inputs
    (list pkg-config))
   (synopsis "2D platformer game")
   (description "SuperTux is a classic 2D jump'n run sidescroller game in
a style similar to the original Super Mario games.")
   (home-page "https://supertux.org/")
   (license license:gpl3+)))

(define-public tintin++
  (package
    (name "tintin++")
    (version "2.02.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/tintin/TinTin++ Source Code/"
                           (string-drop-right version 1)
                           "/tintin-" version ".tar.gz"))
       (sha256
        (base32 "000sg16w7790ha8ys31qjh1ip5hl02ldfwj1zy6dqz0y5i7zvydn"))))
    (inputs
     (list gnutls pcre readline zlib))
    (arguments
     '(#:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         ;; The source is in tt/src.
         (add-before 'configure 'chdir
           (lambda _
             (chdir "src")
             #t)))))
    (build-system gnu-build-system)
    (home-page "https://tintin.mudhalla.net/")
    (synopsis "MUD client")
    (description
     "TinTin++ is a MUD client which supports MCCP (Mud Client Compression
Protocol), MMCP (Mud Master Chat Protocol), xterm 256 colors, most TELNET
options used by MUDs, as well as those required to login via telnet on
Linux / Mac OS X servers, and an auto mapper with a VT100 map display.")
    (license license:gpl3+)))

(define-public laby
  (package
    (name "laby")
    (version "0.7.0")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/sgimenez/laby")
                    (commit (string-append name "-" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1y6nfxcjhqg9bb81hs0wijg7kcwk5kff81rgd8bsv5ps7ia9nj6b"))
             (patches (search-patches "laby-make-install.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("lablgtk3" ,lablgtk3)
       ("ocaml" ,ocaml)
       ("ocaml-findlib" ,ocaml-findlib)
       ("ocamlbuild" ,ocamlbuild)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'set-library-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((lablgtk (assoc-ref inputs "lablgtk3")))
               (setenv "LD_LIBRARY_PATH"
                       (string-append lablgtk "/lib/ocaml/stublibs"))))))
       #:tests? #f ; no 'check' target
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")) "all")))
    (home-page "https://sgimenez.github.io/laby/")
    (synopsis "Programming game")
    (description "Learn programming, playing with ants and spider webs ;-)
Your robot ant can be programmed in many languages: OCaml, Python, C, C++,
Java, Ruby, Lua, JavaScript, Pascal, Perl, Scheme, Vala, Prolog.  Experienced
programmers may also add their own favorite language.")
    (license license:gpl3+)))

(define-public bambam
  (package
    (name "bambam")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/porridge/bambam")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "18cwd1wpyyx8y61cags9bkdhx9x858xicc4y1c9c2s0xjmgzhl3i"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'build)                ; nothing to build
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (share (string-append out "/share")))
               (mkdir-p bin)
               (copy-file "bambam.py" (string-append bin "/bambam"))
               (install-file "bambam.6" (string-append share "/man/man6"))
               (copy-recursively "data" (string-append share "/bambam/data")))
             #t)))))
    (inputs
     (list python-pygame))
    (home-page "https://github.com/porridge/bambam")
    (synopsis "Keyboard mashing and doodling game for babies")
    (description "Bambam is a simple baby keyboard (and gamepad) masher
application that locks the keyboard and mouse and instead displays bright
colors, pictures, and sounds.")
    (license license:gpl3+)))

(define-public mrrescue
  (package
    (name "mrrescue")
    (version "1.02e")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/SimonLarsen/mrrescue/releases/"
                    "download/" version "/mrrescue" version ".love"))
              (file-name (string-append name "-" version ".love"))
              (sha256
               (base32
                "0jwzbwkgp1l5ia6c7s760gmdirbsncp6nfqp7vqdqsfb63la9gl2"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (script (string-append out "/bin/" ,name))
                (data   (string-append out "/share/" ,name))
                (source (assoc-ref %build-inputs "source"))
                (unzip  (search-input-file %build-inputs "/bin/unzip"))
                (patch  (search-input-file %build-inputs "/bin/patch"))
                (bash   (search-input-file %build-inputs "/bin/bash"))
                (love   (search-input-file %build-inputs "/bin/love")))

           (mkdir-p (dirname script))
           (with-output-to-file script
             (lambda ()
               (format #t "#!~a~%" bash)
               (format #t "exec -a ~a \"~a\" \"~a\"~%" ,name love data)))
           (chmod script #o755)

           ;; The better way to package this game would be to install *only* the
           ;; script above, pointing to the unextracted .love file in the store.
           ;; However, mrrescue 1.02e needs to be patched to work with Love 11.
           ;; Instead of extracting the .love file, patching it, and re-zipping
           ;; it to the store, simply point the script to the extracted patched
           ;; data directory directly.
           (mkdir-p data)
           (with-directory-excursion data
             (invoke unzip source)
             (invoke patch "-p1" "-i"
                     (assoc-ref %build-inputs "love-11.patch")))
           #t))))
    (native-inputs
     `(("unzip" ,unzip)
       ("patch" ,patch)
       ("love-11.patch" ,(search-patch "mrrescue-support-love-11.patch"))))
    (inputs
     (list bash love))
    (home-page "https://tangramgames.dk/games/mrrescue")
    (synopsis "Arcade-style fire fighting game")
    (description
     "Mr. Rescue is an arcade styled 2d action game centered around evacuating
civilians from burning buildings.  The game features fast-paced fire
extinguishing action, intense boss battles, a catchy soundtrack, and lots of
throwing people around in pseudo-randomly generated buildings.")
    (license (list license:zlib             ; for source code
                   license:cc-by-sa3.0))))  ; for graphics and music assets

(define-public hyperrogue
  (package
    (name "hyperrogue")
    (version "11.3a")
    ;; When updating this package, be sure to update the "hyperrogue-data"
    ;; origin in native-inputs.
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.roguetemple.com/z/hyper/hyperrogue"
                    (string-join (string-split version #\.) "")
                    "-src.tgz"))
              (sha256
               (base32
                "1yxabbswq02fc5frigvs43f83m5vlxybc7n5mynkwzj2c70lfp2k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:make-flags '("HYPERROGUE_USE_GLEW=1"
                      "HYPERROGUE_USE_PNG=1")
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append (or (getenv "CPATH") "") ":"
                                    (assoc-ref inputs "sdl-union")
                                    "/include/SDL"))))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share-dir (string-append out "/share/hyperrogue"))
                    (dejavu-dir (string-append
                                 (assoc-ref inputs "font-dejavu")
                                 "/share/fonts/truetype"))
                    (dejavu-font "DejaVuSans-Bold.ttf")
                    (music-file "hyperrogue-music.txt"))
               ;; Fix font and music paths.
               (substitute* "basegraph.cpp"
                 ((dejavu-font)
                  (string-append dejavu-dir "/" dejavu-font)))
               (substitute* music-file
                 (("\\*/")
                  (string-append share-dir "/sounds/")))
               (substitute* "sound.cpp"
                 (("musicfile = \"\"")
                  (string-append "musicfile = \""
                                 share-dir "/" music-file "\"")))
               ;; Disable build machine CPU optimizations and warnings treated
               ;; as errors.
               (substitute* "Makefile"
                 (("-march=native") "")
                 (("-Werror") "")))
             #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share-dir (string-append out "/share/hyperrogue")))
               (mkdir-p bin)
               (install-file "hyperrogue" bin)
               (install-file "hyperrogue-music.txt" share-dir))
             #t))
         (add-after 'install 'install-data
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((data (assoc-ref inputs "hyperrogue-data"))
                    (out (assoc-ref outputs "out"))
                    (sounds (string-append out "/share/hyperrogue/sounds"))
                    (unzip (search-input-file inputs "/bin/unzip")))
               ;; Extract media license information into sounds directory.
               (invoke unzip "-j" data
                       (string-append
                        "hyperrogue"
                        (string-join (string-split ,version #\.) "")
                        "/sounds/credits.txt") "-d" sounds)
               ;; Extract sounds and music into sounds directory.
               (invoke "unzip" "-j" data
                       (string-append
                        "hyperrogue"
                        (string-join (string-split ,version #\.) "")
                        "/*.ogg") "-d" sounds)))))))
    (native-inputs
     `(("hyperrogue-data"
        ,(origin
           (method url-fetch)
           (uri
            (string-append
             "https://www.roguetemple.com/z/hyper/hyperrogue"
             (string-join (string-split version #\.) "")
             "-win.zip"))
           (sha256
            (base32
             "11yhbia45f1w9z0j67h9nynwjqmvakr9l6rnrmdrdkzin6lvzzj4"))))
       ("unzip" ,unzip)))
    (inputs
     (list font-dejavu glew libpng
           (sdl-union (list sdl sdl-gfx sdl-mixer sdl-ttf))))
    (home-page "https://www.roguetemple.com/z/hyper/")
    (synopsis "Non-euclidean graphical rogue-like game")
    (description
     "HyperRogue is a game in which the player collects treasures and fights
monsters -- rogue-like but for the fact that it is played on the hyperbolic
plane and not in euclidean space.

In HyperRogue, the player can move through different parts of the world, which
are home to particular creatures and may be subject to their own rules of
\"physics\".

While the game can use ASCII characters to display the the classical rogue
symbols, it still needs graphics to render the non-euclidean world.")
    (license (list license:bsd-3         ; glew.c, mtrand.*
                   license:cc-by-sa3.0   ; music
                   license:cc-by-sa4.0   ; sounds
                   license:cc0
                   license:public-domain ; direntx.*, some sounds
                   license:zlib          ; savepng.*
                   license:gpl2+))))     ; remaining files

(define-public kobodeluxe
  (package
    (name "kobodeluxe")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://olofson.net/kobodl/download/KoboDeluxe-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0b2wvdpnmaibsy419c16dfwj5kvd3pccby2aaqvm964x74592yqg"))
              (patches (search-patches
                        "kobodeluxe-const-charp-conversion.patch"
                        "kobodeluxe-enemies-pipe-decl.patch"
                        "kobodeluxe-graphics-window-signed-char.patch"
                        "kobodeluxe-manpage-minus-not-hyphen.patch"
                        "kobodeluxe-midicon-segmentation-fault.patch"
                        "kobodeluxe-paths.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "CPPFLAGS=-I"
                            (assoc-ref %build-inputs "sdl-union")
                            "/include/SDL"))))
    (inputs
     (list glu
           (sdl-union (list sdl sdl-image))))
    (synopsis "Shooter with space station destruction")
    (description
     "Kobo Deluxe is an enhanced version of Akira Higuchi's XKobo graphical game
for Un*x systems with X11.")
    (home-page "http://olofson.net/kobodl/")
    (license license:gpl2+)))

(define-public freeciv
  (package
   (name "freeciv")
   (version "2.6.6")
   (source
    (origin
     (method url-fetch)
     (uri (list (string-append
                  "http://files.freeciv.org/stable/freeciv-"
                  version ".tar.bz2")
                (string-append
                  "mirror://sourceforge/freeciv/Freeciv%20"
                  (version-major+minor version) "/" version
                  "/freeciv-" version ".tar.bz2")))
     (sha256
      (base32 "04aq2v1ima87sap6yjb7jrm1ss63ax7v5kg7rpkj44887kfybkvv"))))
   (build-system gnu-build-system)
   (inputs
    (list curl cyrus-sasl gtk+ sdl-mixer zlib))
   (native-inputs
    (list pkg-config))
   (home-page "http://www.freeciv.org/")
   (synopsis "Turn-based empire building strategy game")
   (description "Freeciv is a turn-based empire building strategy game
inspired by the history of human civilization.  The game commences in
prehistory and your mission is to lead your tribe from the Stone Age
into the Space Age.")
   (license license:gpl2+)))

(define-public no-more-secrets
  (package
    (name "no-more-secrets")
    (version "0.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bartobri/no-more-secrets")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zfv4qabikf8w9winsr4brxrdvs3f0d7xvydksyx8bydadsm2v2h"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list "CC=gcc" "all-ncurses"
                          (string-append "prefix="
                                         (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     (list ncurses))
    (home-page "https://github.com/bartobri/no-more-secrets")
    (synopsis "Recreation of data decryption effect in \"Sneakers\"")
    (description
     "@code{No More Secrets} provides a command line tool called \"nms\"
that recreates the famous data decryption effect seen on screen in the 1992
movie \"Sneakers\".

This command works on piped data.  Pipe any ASCII or UTF-8 text to nms, and
it will apply the hollywood effect, initially showing encrypted data, then
starting a decryption sequence to reveal the original plaintext characters.")
    (license license:expat)))

(define-public megaglest-data
  (package
    (name "megaglest-data")
    (version "3.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/MegaGlest/megaglest-data"
             "/releases/download/" version "/megaglest-data-"
             version ".tar.xz"))
       (sha256
        (base32
         "0ipgza33z89fw3si32iafm981f3fvm0zldvbxj29whghd2k3rpj3"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://megaglest.org/")
    (synopsis "Data files for MegaGlest")
    (description "This package contains the data files required for MegaGlest.")
    (license license:cc-by-sa3.0)))

(define-public megaglest
  (package
    (name "megaglest")
    (version "3.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/MegaGlest/megaglest-source"
             "/releases/download/" version "/megaglest-source-"
             version ".tar.xz"))
       (sha256
        (base32
         "1ffck3ii1wp5k3nn5p0ga06jgp7pzk4zw0xln3xim2w7qrxzdzh9"))))
    (build-system cmake-build-system)
    (inputs
     `(("curl" ,curl)
       ("fontconfig" ,fontconfig)
       ("ftgl" ,ftgl)
       ("glew" ,glew)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("megaglest-data" ,megaglest-data)
       ("mesa" ,mesa)
       ("miniupnpc" ,miniupnpc)
       ("openal" ,openal)
       ("libircclient" ,libircclient)
       ("libpng" ,libpng)
       ("libvorbis" ,libvorbis)
       ("lua" ,lua)
       ("sdl2" ,sdl2)
       ("wxwidgets" ,wxwidgets)))
    (native-inputs
     (list cppunit pkg-config))
    (arguments
     `(#:configure-flags
       (list "-DCMAKE_CXX_FLAGS=-fcommon"
             "-DCMAKE_C_FLAGS=-fcommon"
             (string-append "-DCUSTOM_DATA_INSTALL_PATH="
                            (assoc-ref %build-inputs "megaglest-data")
                            "/share/megaglest")
             "-DBUILD_MEGAGLEST_TESTS=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-ini-search-path
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* "source/glest_game/global/config.cpp"
                        (("/usr/share/megaglest/")
                         (string-append (assoc-ref outputs "out")
                                        "/share/megaglest/"))))))
       #:test-target "megaglest_tests"))
    (home-page "https://megaglest.org/")
    (synopsis "3D real-time strategy (RTS) game")
    (description "MegaGlest is a cross-platform 3D real-time strategy (RTS)
game, where you control the armies of one of seven different factions: Tech,
Magic, Egypt, Indians, Norsemen, Persian or Romans.")
    (license license:gpl2+)))

(define-public freegish
  (let ((commit "21977ee5fc2008231b35160df00efe954c508b16")
        (revision "2"))
    (package
      (name "freegish")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/freegish/freegish")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1qh0gcnbyxyzmb13jifwba4xrzj94m4w9whdvl0gnds6ricmwply"))
                (modules '((guix build utils)))
                ;; The audio files in the "music" directory are licensed under
                ;; CC-BY-NC, so we delete them.
                (snippet
                 '(begin
                    (delete-file-recursively "music")
                    #t))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f ; no tests included
         #:configure-flags
         (list "-DCMAKE_INSTALL_FHS=ON")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'set-DATAPATH
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "CMakeLists.txt"
                 (("^option\\(INSTALL_FHS" line)
                  (string-append "add_definitions(-DDATAPATH=\""
                                 (assoc-ref outputs "out") "/share/freegish\")\n"
                                 line)))
               #t)))))
      (inputs
       (list (sdl-union (list sdl sdl-mixer))
             openal
             libvorbis
             libogg
             mesa
             libpng
             zlib))
      (home-page "https://github.com/freegish/freegish")
      (synopsis "Side-scrolling physics platformer with a ball of tar")
      (description "In FreeGish you control Gish, a ball of tar who lives
happily with his girlfriend Brea, until one day a mysterious dark creature
emerges from a sewer hole and pulls her below ground.")
      ;; The textures are available under the Expat license.  All other assets
      ;; (including levels) are covered under CC-BY-SA or public domain.  The
      ;; source code is under GPLv2+.
      (license (list license:gpl2+
                     license:expat
                     license:public-domain
                     license:cc-by-sa3.0)))))

(define-public cdogs-sdl
  (package
    (name "cdogs-sdl")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cxong/cdogs-sdl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vx37zb2iw7sfw5a2bs97ydlmb301nvy485ybdm8g46c5hn9s13c"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DCDOGS_DATA_DIR="
                            (assoc-ref %outputs "out")
                            "/share/cdogs-sdl/"))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list gtk+ mesa sdl2 sdl2-image sdl2-mixer))
    (home-page "https://cxong.github.io/cdogs-sdl/")
    (synopsis "Classic overhead run-and-gun game")
    (description "C-Dogs SDL is a classic overhead run-and-gun game,
supporting up to 4 players in co-op and deathmatch modes.  Customize your
player, choose from many weapons, and blast, slide and slash your way through
over 100 user-created campaigns.")
    ;; GPLv2+ for code (includes files under BSD-2 and BSD-3),
    ;; CC0/CC-BY/CC-BY-SA for assets.
    (license (list license:gpl2+
                   license:bsd-2
                   license:bsd-3
                   license:cc0
                   license:cc-by3.0
                   license:cc-by-sa3.0))))

(define-public kiki
  (package
    (name "kiki")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/kiki/kiki-src/"
                                  version "/kiki-" version "-src.tgz"))
              (sha256
               (base32
                "0ihjdsxbn8z3cz0gpcprafiipcqaiskgdnh1rhmw4qff8dszalbn"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file (find-files "." "\\.dll$"))
                  #t))
              (patches
               (search-patches "kiki-level-selection-crash.patch"
                               "kiki-makefile.patch"
                               "kiki-missing-includes.patch"
                               "kiki-portability-64bit.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:make-flags '("CXX=g++")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (setenv "CPLUS_INCLUDE_PATH"
                     (string-append
                      (search-input-directory inputs "include/SDL") ":"
                      (search-input-directory inputs "include/python2.7")
                      ":" (or (getenv "CPLUS_INCLUDE_PATH") "")))
             (substitute* "src/main/main.cpp"
               (("#include <SDL.h>" line)
                (string-append line "
#define K_INCLUDE_GLUT
#include \"KIncludeTools.h\""))
               (("// initialize SDL" line)
                (string-append "glutInit(&argc,argv);\n" line)))
             (substitute* "src/main/KikiController.cpp"
               (("getenv\\(\"KIKI_HOME\"\\)")
                (string-append "\"" (assoc-ref outputs "out") "/share/kiki/\"")))
             (substitute* "linux/Makefile"
               (("CXXOPTS =" line)
                (string-append line " -fpermissive"))
               (("PYTHON_VERSION=.*") "PYTHON_VERSION=2.7")
               (("PYTHONHOME =.*")
                (string-append "PYTHONHOME = "
                               (assoc-ref inputs "python")
                               "/lib/python2.7/"))
               (("\\$\\(GLLIBS\\)" line)
                (string-append line " -lm -lpython2.7")))
             (substitute* "src/main/KikiPythonWidget.h"
               (("#define __KikiPythonWidget" line)
                (string-append line "\n#include \"KikiPython.h\"")))
             #t))
         (add-before 'build 'build-kodilib
           (lambda* (#:key make-flags #:allow-other-keys)
             (with-directory-excursion "kodilib/linux"
               (apply invoke "make" make-flags))))
         (add-after 'build-kodilib 'chdir
           (lambda _ (chdir "linux") #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (share (string-append out "/share/kiki")))
               (mkdir-p bin)
               (mkdir-p share)
               (install-file "kiki" bin)
               (copy-recursively "../py" (string-append share "/py"))
               (copy-recursively "../sound" (string-append share "/sound"))
               #t))))))
    (inputs
     `(("glu" ,glu)
       ;; Kiki builds fine with freeglut 3.0.0 but segfaults on start.
       ("freeglut" ,freeglut-2.8)
       ("sdl-union" ,(sdl-union (list sdl
                                      sdl-mixer
                                      sdl-image)))
       ("python" ,python-2)))
    (native-inputs
     (list swig))
    (home-page "http://kiki.sourceforge.net/")
    (synopsis "3D puzzle game")
    (description "Kiki the nano bot is a 3D puzzle game.  It is basically a
mixture of the games Sokoban and Kula-World.  Your task is to help Kiki, a
small robot living in the nano world, repair its maker.")
    ;; See <http://metadata.ftp-master.debian.org/changelogs/main/k/
    ;; kiki-the-nano-bot/kiki-the-nano-bot_1.0.2+dfsg1-4_copyright>
    ;; for a statement from the author.
    (license license:public-domain)))

(define-public teeworlds
  (package
    (name "teeworlds")
    (version "0.7.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/teeworlds/teeworlds")
                    (commit version)
                    ;; There are two submodules in datasrc/{languages,maps}
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1l19ksmimg6b8zzjy0skyhh7z11ql7n5gvilkv7ay5x2b9ndbqwz"))
              (modules '((guix build utils)
                         (ice-9 ftw)
                         (ice-9 regex)
                         (srfi srfi-1)
                         (srfi srfi-26)))
              (snippet ; remove bundled libraries except md5
               '(let ((base-dir "src/engine/external/"))
                  (for-each (compose (cut delete-file-recursively <>)
                                     (cut string-append base-dir <>))
                            (remove (cut string-match "(^.)|(^md5$)" <>)
                                    (scandir base-dir)))
                  #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:test-target "run_tests"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Embed path to assets.
             (substitute* "src/engine/shared/storage.cpp"
               (("#define DATA_DIR.*")
                (string-append "#define DATA_DIR \""
                               (assoc-ref outputs "out")
                               "/share/teeworlds/data"
                               "\"")))
             #t))
         (add-after 'unpack 'replace-font
           (lambda* (#:key inputs #:allow-other-keys)
             (delete-file "datasrc/fonts/DejaVuSans.ttf")
             (symlink (string-append (assoc-ref inputs "font-dejavu")
                                     "/share/fonts/truetype/DejaVuSans.ttf")
                      "datasrc/fonts/DejaVuSans.ttf")
             #t)))))
    (inputs
     (list freetype
           font-dejavu
           glu
           json-parser
           mesa
           pnglite
           sdl2
           sdl2-image
           sdl2-mixer
           wavpack
           openssl
           zlib))
    (native-inputs
     `(("googletest" ,googletest)
       ("python" ,python-wrapper)
       ("pkg-config" ,pkg-config)))
    (home-page "https://www.teeworlds.com")
    (synopsis "2D retro multiplayer shooter game")
    (description "Teeworlds is an online multiplayer game.  Battle with up to
16 players in a variety of game modes, including Team Deathmatch and Capture
The Flag.  You can even design your own maps!")
    (license (list license:bsd-3 license:cc-by-sa3.0)))) ; game+maps&languages

(define-public enigma
  (package
    (name "enigma")
    (version "1.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/enigma-game/"
                                  "Release%20" version "/enigma-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "00ffh9pypj1948pg3q9sjp1nmiabh52p5c8wpg9n1dcfgl3cywnq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--with-system-enet")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build-with-new-gcc
           (lambda _
             ;; Fix build with GCC6 and later by avoiding comparing ifstream
             ;; to NULL.  Can be removed for versions > 1.21.
             (substitute* "src/lev/Proxy.cc"
               (("ifs != NULL")
                "ifs"))
             #t))
         (add-after 'unpack 'find-sdl
           (lambda _
             (substitute* "configure"
               (("SDL_ttf.h") "SDL/SDL_ttf.h"))
             (substitute* '("tools/ttf2bmf.cc"
                            "lib-src/enigma-core/ecl_font.cc"
                            "lib-src/enigma-core/ecl_video.cc"
                            "lib-src/enigma-core/ecl_buffer.hh"
                            "src/SoundEngine.cc"
                            "src/SoundEngine.hh"
                            "src/MusicManager.cc"
                            "src/MusicManager.hh"
                            "src/d_models.cc"
                            "src/main.cc"
                            "src/network.cc")
               (("#include \"SDL_(image|ttf|mixer|types|syswm|mutex).h\"" line header)
                (string-append "#include \"SDL/SDL_" header ".h\"")))
             (substitute* "src/main.cc"
               (("#include <SDL_(image|ttf|mixer).h>" line header)
                (string-append "#include \"SDL/SDL_" header ".h\"")))
             #t)))))
    (inputs
     (list xerces-c
           (sdl-union (list sdl sdl-image sdl-mixer sdl-ttf)) curl
           enet))
    (native-inputs
     (list pkg-config imagemagick))
    (home-page "https://www.nongnu.org/enigma")
    (synopsis "Puzzle game with a dexterity component")
    (description "Enigma is a puzzle game with 550 unique levels.  The object
of the game is to find and uncover pairs of identically colored ‘Oxyd’ stones.
Simple?  Yes.  Easy?  Certainly not!  Hidden traps, vast mazes, laser beams,
and most of all, countless hairy puzzles usually block your direct way to the
Oxyd stones.  Enigma’s game objects (and there are hundreds of them, lest you
get bored) interact in many unexpected ways, and since many of them follow the
laws of physics (Enigma’s special laws of physics, that is), controlling them
with the mouse isn’t always trivial.")
    (license license:gpl2+)))

(define-public chroma
  (package
    (name "chroma")
    (version "1.20")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://level7.org.uk/chroma/download/chroma-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "02hn448ckfxbx2fqr9wgf66rwl0vr4gl87yvsr5fc99zz9zw2f5v"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))                    ; no tests included
    (inputs
     `(("sdl-union" ,(sdl-union (list sdl sdl-image sdl-mixer sdl-ttf)))
       ("freetype" ,freetype)
       ("ncurses" ,ncurses)
       ("fontconfig" ,fontconfig)
       ("libxft" ,libxft)))
    (native-inputs
     (list pkg-config))
    (home-page "http://level7.org.uk/chroma/")
    (synopsis "Abstract puzzle game")
    (description "Chroma is an abstract puzzle game. A variety of colourful
shapes are arranged in a series of increasingly complex patterns, forming
fiendish traps that must be disarmed and mysterious puzzles that must be
manipulated in order to give up their subtle secrets.  Initially so
straightforward that anyone can pick it up and begin to play, yet gradually
becoming difficult enough to tax even the brightest of minds.")
    (license license:gpl2+)))

(define-public fillets-ng
  (package
    (name "fillets-ng")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/fillets/"
                                  "Fish%20Fillets%20-%20Next%20Generation/"
                                  version "/fillets-ng-" version ".tar.gz"))
              (sha256
               (base32
                "1nljp75aqqb35qq3x7abhs2kp69vjcj0h1vxcpdyn2yn2nalv6ij"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-lua="
                            (assoc-ref %build-inputs "lua")))
       #:make-flags
       (list (string-append "CFLAGS=-I"
                            (assoc-ref %build-inputs "sdl-union")
                            "/include/SDL")
             (string-append "CXXFLAGS=-I"
                            (assoc-ref %build-inputs "sdl-union")
                            "/include/SDL"))
       #:phases
       (modify-phases %standard-phases
         ;; Lua 5.1 does not provide it.
         (add-after 'unpack 'do-not-link-with-lualib
           (lambda _
             (substitute* "configure"
               (("-llualib") ""))
             #t))
         (add-after 'install 'install-data
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((data (string-append (assoc-ref outputs "out")
                                        "/share/games/fillets-ng")))
               (mkdir-p data)
               (invoke "tar" "-xvf"
                       (assoc-ref inputs "fillets-ng-data")
                       "--strip-components=1"
                       "-C" data)))))))
    (inputs
     (list (sdl-union (list sdl sdl-mixer sdl-image sdl-ttf)) fribidi
           libx11 lua-5.1))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("fillets-ng-data"
        ,(origin
           (method url-fetch)
           (uri (string-append "mirror://sourceforge/fillets/"
                               "Fish%20Fillets%20-%20Next%20Generation/"
                               version "/fillets-ng-data-" version ".tar.gz"))
           (sha256
            (base32
             "169p0yqh2gxvhdilvjc2ld8aap7lv2nhkhkg4i1hlmgc6pxpkjgh"))))))
    (home-page "http://fillets.sourceforge.net/")
    (synopsis "Puzzle game")
    (description "Fish Fillets NG is strictly a puzzle game.  The goal in
every of the seventy levels is always the same: find a safe way out.  The fish
utter witty remarks about their surroundings, the various inhabitants of their
underwater realm quarrel among themselves or comment on the efforts of your
fish.  The whole game is accompanied by quiet, comforting music.")
    (license license:gpl2+)))

(define-public crawl
  (package
    (name "crawl")
    (version "0.27.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/crawl/crawl/releases/download/"
                           version "/stone_soup-" version "-nodeps.tar.xz"))
       (sha256
        (base32 "0nkhyhrrma8gmwxp15j84cn1k2yvyq7ar9rd0j2qjjlv2kdis5z2"))
       (patches (search-patches "crawl-upgrade-saves.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("lua51" ,lua-5.1)
       ("ncurses" ,ncurses)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("perl" ,perl)
       ("python" ,python-wrapper)
       ("python-pyyaml" ,python-pyyaml)
       ("pkg-config" ,pkg-config)))
    (arguments
     `(#:make-flags
       (let* ((sqlite (assoc-ref %build-inputs "sqlite"))
              (out (assoc-ref %outputs "out")))
         (list (string-append "SQLITE_INCLUDE_DIR=" sqlite "/include")
               (string-append "prefix=" out)
               "SAVEDIR=~/.crawl"
               ;; Don't compile with SSE on systems which don't have it.
               ,@(match (%current-system)
                   ((or "i686-linux" "x86_64-linux")
                    '())
                   (_ '("NOSSE=TRUE")))
               ;; don't build any bundled dependencies
               "BUILD_LUA="
               "BUILD_SQLITE="
               "BUILD_ZLIB="
               "-Csource"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-SDL-image
           (lambda _
             (substitute* "source/windowmanager-sdl.cc"
               (("SDL_image.h") "SDL2/SDL_image.h"))
             #t))
         (delete 'configure)
         (replace 'check
           (lambda* (#:key inputs outputs make-flags #:allow-other-keys)
             (setenv "HOME" (getcwd))
             ;; Fake a terminal for the test cases.
             (setenv "TERM" "xterm-256color")
             ;; Run the tests that don't require a debug build.
             (apply invoke "make" "nondebugtest"
                    (format #f "-j~d" (parallel-job-count))
                    ;; Force command line build for test cases.
                    (append make-flags '("GAME=crawl" "TILES="))))))))
    (synopsis "Roguelike dungeon crawler game")
    (description "Dungeon Crawl Stone Soup (also known as \"Crawl\" or DCSS
for short) is a roguelike adventure through dungeons filled with dangerous
monsters in a quest to find the mystifyingly fabulous Orb of Zot.")
    (home-page "https://crawl.develz.org")
    (license (list license:gpl2+
                   license:bsd-2
                   license:bsd-3
                   license:cc0
                   license:expat
                   license:zlib
                   license:asl2.0))))

;; The linter here claims that patch file names should start with the package
;; name. But, in this case, the patches are inherited from crawl with the
;; "crawl-" prefix instead of "crawl-tiles-".
(define-public crawl-tiles
  (package
    (inherit crawl)
    (name "crawl-tiles")
    (arguments
     (substitute-keyword-arguments
         (package-arguments crawl)
       ((#:make-flags flags)
        `(let ((dejavu (assoc-ref %build-inputs "font-dejavu")))
           (cons*
            (string-append "PROPORTIONAL_FONT=" dejavu
                           "/share/fonts/truetype/DejaVuSans.ttf")
            (string-append "MONOSPACED_FONT=" dejavu
                           "/share/fonts/truetype/DejaVuSansMono.ttf")
            "TILES=y"
            ;; Rename the executable to allow parallel installation with crawl.
            "GAME=crawl-tiles"
            ,flags)))))
    (inputs
     `(,@(package-inputs crawl)
       ("font-dejavu" ,font-dejavu)
       ("freetype6" ,freetype)
       ("glu" ,glu)
       ("libpng" ,libpng)
       ("sdl2" ,sdl2)
       ("sdl2-image" ,sdl2-image)
       ("sdl2-mixer" ,sdl2-mixer)))
    (native-inputs
     `(,@(package-native-inputs crawl)
       ("pngcrush" ,pngcrush)
       ("which" ,which)))
    (synopsis "Graphical roguelike dungeon crawler game")))

(define-public lugaru
  (package
    (name "lugaru")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bitbucket.org/osslugaru/lugaru/downloads/"
                                  "lugaru-" version ".tar.xz"))
              (sha256
               (base32
                "15zgcshy22q51rm72zi6y9z7qlgnz5iw3gczjdlir4bqmxy4gspk"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DSYSTEM_INSTALL=ON")
       ;; no test target
       #:tests? #f))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("sdl2" ,sdl2)
       ("glu" ,glu)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("openal" ,openal)
       ("vorbis" ,libvorbis)
       ("zlib" ,zlib)))
    (home-page "https://osslugaru.gitlab.io")
    (synopsis "Cross-platform third-person action game")
    (description "Lugaru is a third-person action game.  The main character,
Turner, is an anthropomorphic rebel bunny rabbit with impressive combat skills.
In his quest to find those responsible for slaughtering his village, he uncovers
a far-reaching conspiracy involving the corrupt leaders of the rabbit republic
and the starving wolves from a nearby den.  Turner takes it upon himself to
fight against their plot and save his fellow rabbits from slavery.")
    (license (list license:gpl2+ ; code
                   ;; assets:
                   license:cc-by-sa3.0
                   license:cc-by-sa4.0))))

(define-public 0ad-data
  (package
    (name "0ad-data")
    (version "0.0.25b-alpha")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://releases.wildfiregames.com/0ad-"
                           version "-unix-data.tar.xz"))
       (file-name (string-append name "-" version ".tar.xz"))
       (sha256
        (base32 "1c9zrddmjxvvacismld6fbwbw9vrdbq6g6d3424p8w5p6xg5wlwy"))))
    (build-system trivial-build-system)
    (native-inputs (list tar unzip xz))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((out (assoc-ref %outputs "out"))
               (source (assoc-ref %build-inputs "source"))
               (tar (search-input-file %build-inputs "/bin/tar"))
               (unzip (search-input-file %build-inputs "/bin/unzip"))
               (xz-path (string-append (assoc-ref %build-inputs "xz") "/bin")))
           (setenv "PATH" xz-path)
           (mkdir out)
           (invoke tar "xvf" source "-C" out "--strip=3")
           (for-each (lambda (name)
                       (let* ((dir (string-append out "/mods/" name))
                              (file (string-append dir "/" name ".zip")))
                         (invoke unzip "-o" "-d" dir file)
                         (delete-file file)))
                     '("mod" "public"))))))
    (synopsis "Data files for 0ad")
    (description "0ad-data provides the data files required by the game 0ad.")
    (home-page "https://play0ad.com")
    (license (list (license:fsdg-compatible
                    "http://tavmjong.free.fr/FONTS/ArevCopyright.txt"
                    "Similar to the license of the Bitstream Vera fonts.")
                   (license:fsdg-compatible
                    "https://www.gnome.org/fonts/#Final_Bitstream_Vera_Fonts")
                   license:cc-by-sa3.0
                   license:expat
                   license:gfl1.0
                   license:gpl2+
                   license:gpl3+))))

(define-public 0ad
  (package
    (name "0ad")
    (version "0.0.25b-alpha")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://releases.wildfiregames.com/0ad-"
                           version "-unix-build.tar.xz"))
       (file-name (string-append name "-" version ".tar.xz"))
       (sha256
        (base32 "1p9fa8f7sjb9c5wl3mawzyfqvgr614kdkhrj2k4db9vkyisws3fp"))))
    ;; A snippet here would cause a build failure because of timestamps
    ;; reset.  See https://bugs.gnu.org/26734.
    (inputs
     (list #{0ad-data}#
           curl
           enet
           fmt
           gloox
           icu4c-68
           libidn
           libpng
           libsodium
           libvorbis
           libxcursor
           libxml2
           miniupnpc
           mozjs-78
           openal
           sdl2
           wxwidgets
           zlib))
    (native-inputs
     (list boost
           cmake-minimal
           cxxtest
           mesa
           pkg-config
           python-2))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("config=release" "verbose=1" "-C" "build/workspaces/gcc")
       #:tests? #f                      ;tests fail currently
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-bundles
           (lambda* (#:key inputs #:allow-other-keys)
             (delete-file-recursively "libraries/source/spidermonkey")
             (delete-file-recursively "libraries/source/cxxtest-4.4")
             (substitute* "build/premake/premake5.lua"
               (("rootdir\\.\\.\"\\/libraries\\/source\\/cxxtest-4.4\\/bin\\/cxxtestgen\"")
                (string-append "\"" (assoc-ref inputs "cxxtest")
                               "/bin/cxxtestgen"
                               "\"")))))
         (add-after 'unpack 'fix-mozjs-compatibility
           ;; 0ad only builds fine with a specific version of mozjs
           ;; (version 78.6 for 0ad-0.0.25).
           ;; Here we change the error in case of version mismatch to a warning,
           ;; and add some minor compatibility fixes.
           (lambda _
             (substitute* "source/scriptinterface/ScriptTypes.h"
               (("#error Your compiler is trying to use")
                "#warning Your compiler is trying to use"))
             (substitute* "source/scriptinterface/ScriptContext.cpp"
               (("JS::PrepareZoneForGC\\(")
                "JS::PrepareZoneForGC(m_cx, "))))
         (replace 'configure
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (let* ((jobs (number->string (parallel-job-count)))
                    (out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (data (string-append out "/share/0ad")))
               (setenv "JOBS" (string-append "-j" jobs))
               (setenv "CC" "gcc")
               (with-directory-excursion "build/workspaces"
                 (apply invoke
                        `("./update-workspaces.sh"
                          ,(string-append "--libdir=" lib)
                          ,(string-append "--datadir=" data)
                          ;; TODO: "--with-system-nvtt"
                          "--with-system-mozjs"
                          ,@(if tests? '() '("--without-tests"))))))))
         (delete 'check)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (chdir "binaries")
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (data (string-append out "/share/0ad"))
                    (applications (string-append out "/share/applications"))
                    (hicolor (string-append out "/share/icons/hicolor/128x128/apps"))
                    (metainfo (string-append out "/share/metainfo"))
                    (mime (string-append out "/share/mime/application"))
                    (0ad-data (assoc-ref inputs "0ad-data")))
               ;; data
               (copy-recursively "data" data)
               (for-each (lambda (file)
                           (symlink (string-append 0ad-data "/" file)
                                    (string-append data "/" file)))
                         '("config" "mods/mod" "mods/public" "tools"))
               ;; libraries
               (for-each (lambda (file)
                           (install-file file lib))
                         (find-files "system" "\\.so$"))
               ;; binaries
               (install-file "system/pyrogenesis" bin)
               (with-directory-excursion bin
                 (symlink "pyrogenesis" "0ad"))
               ;; resources
               (with-directory-excursion "../build/resources"
                 (install-file "0ad.desktop" applications)
                 (install-file "0ad.png" hicolor)
                 (install-file "0ad.appdata.xml" metainfo)
                 (install-file "pyrogenesis.xml" mime)))))
         (add-after 'install 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "system"
                 (invoke "./test"))))))))
    (home-page "https://play0ad.com")
    (synopsis "3D real-time strategy game of ancient warfare")
    (description "0 A.D. is a real-time strategy (RTS) game of ancient
warfare.  It's a historically-based war/economy game that allows players to
relive or rewrite the history of twelve ancient civilizations, each depicted
at their peak of economic growth and military prowess.

0ad needs a window manager that supports 'Extended Window Manager Hints'.")
    (license (list license:bsd-2
                   license:bsd-3
                   license:expat
                   license:gpl2+
                   license:ibmpl1.0
                   license:isc
                   license:lgpl2.1
                   license:lgpl3
                   license:mpl2.0
                   license:zlib))))

(define-public open-adventure
  (package
    (name "open-adventure")
    (version "1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/esr/open-adventure")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "123svzy7xczdklx6plbafp22yv9bcvwfibjk0jv2c9i22dfsr07f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ;no configure script
         (add-before 'build 'use-echo
           (lambda _
             (substitute* "tests/Makefile"
               (("/bin/echo") (which "echo")))
             #t))
         (add-after 'build 'build-manpage
           (lambda _
             ;; This target is missing a dependency
             (substitute* "Makefile"
               ((".adoc.6:" line)
                (string-append line " advent.adoc")))
             (invoke "make" ".adoc.6")))
         ;; There is no install target.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man6")))
               (install-file "advent" bin)
               (install-file "advent.6" man))
             #t)))))
    (native-inputs
     `(("asciidoc" ,asciidoc)
       ("libedit" ,libedit)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("python-pyyaml" ,python-pyyaml)))
    (home-page "https://gitlab.com/esr/open-adventure")
    (synopsis "Colossal Cave Adventure")
    (description
     "The original Colossal Cave Adventure from 1976 was the origin of all
text adventures, dungeon-crawl (computer) games, and computer-hosted
roleplaying games.  This is a forward port of the last version released by
Crowther & Woods, its original authors, in 1995.  It has been known as
``adventure 2.5'' and ``430-point adventure''.")
    (license license:bsd-2)))

(define-public open-adventure-2.5
  (package
    (inherit open-adventure)
    (version "2.5")
    (properties `((superseded . ,open-adventure)))))

(define-public tome4
  (package
    (name "tome4")
    (version "1.7.4")
    (synopsis "Single-player, RPG roguelike game set in the world of Eyal")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://te4.org/dl/t-engine/t-engine4-src-"
                           version ".tar.bz2"))
       (sha256
        (base32 "197jmd99l3w3sig32pvdlq9fcgdjjx7g9csy08kz174cyhrlyly3"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* '("src/music.h" "src/tSDL.h")
             (("#elif defined(__FreeBSD__)" line)
              (string-append
               line " || defined(__GNUC__)")))
           (substitute* '("src/tgl.h")
             (("#include <GL/glext.h>") ""))))))
    (build-system gnu-build-system)
    (native-inputs
     (list unzip))
    (inputs
     `(("sdl-union" ,(sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf)))
       ("glu" ,glu)
       ("premake4" ,premake4)
       ("openal" ,openal)
       ("vorbis" ,libvorbis)
       ("luajit" ,luajit)))
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             "config=release")
       ;; XXX: Building in parallel occasionally causes this build failure:
       ;;   ../src/luajit2/src/host/buildvm.c:73:10: fatal error: buildvm_arch.h:
       ;;   No such file or directory
       #:parallel-build? #f
       #:phases (modify-phases %standard-phases
                  (delete 'bootstrap)
                  (replace 'configure
                    (lambda _
                      (invoke "premake4" "gmake")))
                  (add-after 'set-paths 'set-sdl-paths
                    (lambda* (#:key inputs #:allow-other-keys)
                      (setenv "CPATH"
                              (string-append
                               (search-input-directory inputs "/include/SDL2")
                               ":" (or (getenv "CPATH") "")))))
                  (delete 'check)
                  ;; premake doesn't provide install target
                  (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (usr (string-append out "/usr"))
                             (bin (string-append out "/bin"))
                             (licenses (string-append out "/share/licenses"))
                             (documents (string-append out "/share/doc"))
                             (pixmaps (string-append out "/share/pixmaps"))
                             (icon "te4-icon.png")
                             (data (string-append out "/share/" ,name))
                             (applications (string-append
                                            out "/share/applications"))
                             (unzip (string-append
                                     (assoc-ref inputs "unzip") "/bin/unzip"))
                             (wrapper (string-append bin "/" ,name)))
                        ;; icon
                        (mkdir-p pixmaps)
                        (invoke unzip "-j"
                                (string-append
                                 "game/engines/te4-" ,version ".teae")
                                (string-append
                                 "data/gfx/" icon) "-d" pixmaps)
                        ;; game executable
                        (install-file "t-engine" data)
                        (mkdir-p bin)
                        (with-output-to-file wrapper
                          (lambda ()
                            (display
                             (string-append
                              "#!/bin/sh\n"
                              ;; No bootstrap code found,
                              ;; defaulting to working directory
                              ;; for engine code!
                              "cd " data "\n"
                              "exec -a tome4 ./t-engine \"$@\"\n"))))
                        (chmod wrapper #o555)
                        ;; licenses
                        (for-each (lambda (file)
                                    (install-file file licenses))
                                  '("COPYING" "COPYING-MEDIA"))
                        ;; documents
                        (for-each (lambda (file)
                                    (install-file file documents))
                                  '("CONTRIBUTING" "CREDITS"))
                        ;; data
                        (copy-recursively "bootstrap" (string-append
                                                       data "/bootstrap"))
                        (copy-recursively "game" (string-append data "/game"))
                        ;; launcher
                        (mkdir-p applications)
                        (make-desktop-entry-file
                         (string-append applications "/" ,name ".desktop")
                         #:name "ToME4"
                         #:comment ,synopsis
                         #:exec ,name
                         #:icon icon
                         #:categories '("Game" "RolePlaying"))))))))
    (home-page "https://te4.org")
    (description "Tales of Maj’Eyal (ToME) RPG, featuring tactical turn-based
combat and advanced character building.  Play as one of many unique races and
classes in the lore-filled world of Eyal, exploring random dungeons, facing
challenging battles, and developing characters with your own tailored mix of
abilities and powers.  With a modern graphical and customisable interface,
intuitive mouse control, streamlined mechanics and deep, challenging combat,
Tales of Maj’Eyal offers engaging roguelike gameplay for the 21st century.")
    (license license:gpl3+)))

(define-public quakespasm
  (package
    (name "quakespasm")
    (version "0.93.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/quakespasm/Source/quakespasm-"
                           version ".tgz"))
       (sha256
        (base32 "0qm0j5drybvvq8xadfyppkpk3rxqsxbywzm6iwsjwdf0iia3gss5"))))
    (arguments
     `(#:tests? #f
       #:make-flags '("CC=gcc"
                      "MP3LIB=mpg123"
                      "USE_CODEC_FLAC=1"
                      "USE_CODEC_MIKMOD=1"
                      "USE_SDL2=1"
                      "-CQuake")
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'unpack 'fix-makefile-paths
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (mkdir-p (string-append out "/bin"))
                        (substitute* "Quake/Makefile"
                          (("/usr/local/games")
                           (string-append out "/bin")))
                        #t))))))
    (build-system gnu-build-system)
    (inputs (list libmikmod
                  libvorbis
                  flac
                  mesa
                  mpg123
                  sdl2))
    (synopsis "First person shooter engine for Quake 1")
    (description "Quakespasm is a modern engine for id software's Quake 1.
It includes support for 64 bit CPUs, custom music playback, a new sound driver,
some graphical niceities, and numerous bug-fixes and other improvements.")
    (home-page "http://quakespasm.sourceforge.net/")
    (license license:gpl2+)))

(define-public vkquake
  (package
    (inherit quakespasm)
    (name "vkquake")
    (version "1.01.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Novum/vkQuake")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iwin8j5kbyrknbkhjgpy8nmm7pxqzr0daa9gn7p38qhg2mh0a39"))))
    (arguments
     `(#:make-flags
       (let ((vulkanlib (string-append (assoc-ref %build-inputs
                                                  "vulkan-loader") "/lib")))
         (list "CC=gcc"
               "MP3LIB=mpg123"
               "USE_CODEC_FLAC=1"
               "USE_CODEC_MIKMOD=1"
               "USE_SDL2=1"
               (string-append "LDFLAGS=-Wl,-rpath=" vulkanlib)
               "-CQuake"))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-for-new-vulkan
                    (lambda _
                      ;; Mimic upstream commit a869a22d9b51c68e for
                      ;; compatibility with newer vulkan-headers.
                      (substitute* "Quake/gl_rmisc.c"
                        (("VK_DYNAMIC_STATE_RANGE_SIZE")
                         "3"))
                      #t))
                  (delete 'configure)
                  (add-after 'unpack 'fix-makefile-paths
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((vulkan (assoc-ref %build-inputs
                                               "vulkan-loader"))
                            (out (assoc-ref outputs "out")))
                        (mkdir-p (string-append out "/bin"))
                        (substitute* "Quake/Makefile" ((" /usr")
                                                       (string-append " " out)))
                        (substitute* "Quake/Makefile" (("/games")
                                                       (string-append "/bin")))
                        (substitute* "Quake/Makefile" (("..VULKAN_SDK.") vulkan))
                        #t))))
       ,@(strip-keyword-arguments '(#:make-flags #:phases)
                                  (package-arguments quakespasm))))
    (inputs (modify-inputs (package-inputs quakespasm)
              (prepend vulkan-headers vulkan-loader)))
    (description "vkquake is a modern engine for id software's Quake 1.
It includes support for 64 bit CPUs, custom music playback, a new sound driver,
some graphical niceities, and numerous bug-fixes and other improvements.")
    (home-page "https://github.com/Novum/vkQuake")))

(define-public yamagi-quake2
  (package
    (name "yamagi-quake2")
    (version "7.45")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://deponie.yamagi.org/quake2/quake2-"
                           version ".tar.xz"))
       (sha256
        (base32 "0rgz8x7lzd0zb0xqd0gvnf2641nr9xpfm6v14mgh99hspxklaln7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags
       (list "CC=gcc"
             ;; An optional directory where it will look for quake2 data files
             ;; in addition to the current working directory.
             "WITH_SYSTEMWIDE=yes"
             "WITH_SYSTEMDIR=\"/opt/quake2\"")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'patch-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The game writes paths to openal.so and curl.so to ~/.yq2/...
             ;; Workaround: hard-code the compiled paths where it loads them;
             ;; this prevents loading old or garbage collected libraries.
             (substitute* "src/client/sound/qal.c"
               (("al_driver->string")
                (string-append "\"" (assoc-ref inputs "openal")
                               "/lib/libopenal.so\"")))
             (substitute* "src/client/curl/qcurl.c"
               (("cl_libcurl->string")
                (string-append "\"" (assoc-ref inputs "curl")
                               "/lib/libcurl.so\"")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/lib"))
               (mkdir-p (string-append out "/bin"))
               ;; The yamagi-quake2 binary must be in the same directory
               ;; as its engine libraries, but symlinking it to /bin is okay.
               ;; https://github.com/yquake2/yquake2/blob/master/stuff/packaging.md
               (copy-recursively "release"
                                 (string-append out "/lib/yamagi-quake2"))
               (symlink (string-append out "/lib/yamagi-quake2/quake2")
                        (string-append out "/bin/yamagi-quake2"))
               (symlink (string-append out "/lib/yamagi-quake2/q2ded")
                        (string-append out "/bin/yamagi-q2ded"))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list curl
           libvorbis
           mesa
           openal
           sdl2
           zlib))
    (synopsis "First person shooter engine based on quake2")
    (description "Yamagi Quake II is an enhanced client for id Software's Quake II.
The main focus is an unchanged single player experience like back in 1997,
thus the gameplay and the graphics are unaltered.  However the user may use one
of the unofficial retexturing packs.  In comparison with the official client,
over 1000 bugs were fixed and an extensive code audit done,
making Yamagi Quake II one of the most solid Quake II implementations available.")
    (home-page "https://www.yamagi.org/quake2/")
    (license (list license:gpl2+         ; game and server
                   (license:non-copyleft ; info-zip
                    "file://LICENSE"
                    "See Info-Zip section.")
                   license:public-domain)))) ; stb

(define-public nudoku
  (package
    (name "nudoku")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jubalh/nudoku")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12v00z3p0ymi8f3w4b4bgl4c76irawn3kmd147r0ap6s9ssx2q6m"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake gettext-minimal pkg-config))
    (inputs
     (list ncurses))
    (home-page "https://jubalh.github.io/nudoku/")
    (synopsis "Sudoku for your terminal")
    (description "Nudoku is a ncurses-based Sudoku game for your terminal.")
    (license license:gpl3+)))

(define-public the-butterfly-effect
  (package
    (name "the-butterfly-effect")
    (version "0.9.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/the-butterfly-effect/tbe")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ag2cp346f9bz9qy6za6q54id44d2ypvkyhvnjha14qzzapwaysj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; There is no "install" phase.  By default, tbe is installed
         ;; in the build directory.  Provide our own installation.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (share (string-append out "/share")))
               (install-file "usr/games/tbe" bin)
               (mkdir-p share)
               (copy-recursively "usr/share" share)
               #t))))
       ;; Test suite requires a running Xorg server. Even when
       ;; provided, it fails with "D-Bus library appears to be
       ;; incorrectly set up; failed to read machine uuid: Failed to
       ;; open "/etc/machine-id": No such file or directory" along
       ;; with multiple "QPainter:: ... Painter not active" warnings.
       #:tests? #f))
    (inputs
     (list qtbase-5 qtsvg))
    (native-inputs
     `(("cmake" ,cmake-minimal)
       ("gettext-minimal" ,gettext-minimal)
       ("qttools" ,qttools)))
    (synopsis "Realistic physics puzzle game")
    (description "The Butterfly Effect (tbe) is a game that uses
realistic physics simulations to combine lots of simple mechanical
elements to achieve a simple goal in the most complex way possible.")
    (home-page "http://the-butterfly-effect.org/")
    ;; Main license is GPL2-only.  However, artwork is distributed
    ;; under various licenses, listed here.
    (license (list license:gpl2 license:public-domain license:expat
                   license:cc-by-sa3.0 license:gpl3+ license:wtfpl2))))

(define-public pioneer
  (package
    (name "pioneer")
    (version "20210723")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pioneerspacesim/pioneer")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hj99jxb9n3r0bkq87p1c24862xa1xyzjyfdyyx88ckszxb05qf3"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list assimp
           curl
           freetype
           glew
           glu
           libpng
           libsigc++-2
           libvorbis
           lua-5.2                      ;not compatible with 5.3
           mesa
           (sdl-union (list sdl2 sdl2-image))))
    (arguments
     `(#:tests? #f                      ;tests are broken
       #:configure-flags (list "-DUSE_SYSTEM_LIBLUA:BOOL=YES"
                               (string-append "-DPIONEER_DATA_DIR="
                                              %output "/share/games/pioneer"))
       #:make-flags (list "all" "build-data")))
    (home-page "https://pioneerspacesim.net")
    (synopsis "Game of lonely space adventure")
    (description
     "Pioneer is a space adventure game set in our galaxy at the turn of the
31st century.  The game is open-ended, and you are free to eke out whatever
kind of space-faring existence you can think of.  Look for fame or fortune by
exploring the millions of star systems.  Turn to a life of crime as a pirate,
smuggler or bounty hunter.  Forge and break alliances with the various
factions fighting for power, freedom or self-determination.  The universe is
whatever you make of it.")
    (license license:gpl3)))

(define-public badass
  (let ((commit "3c3cd669b4fc8f73a102e3702788f7b28dc47dbb")
        (revision "0"))
  (package
    (name "badass")
    (version (git-version "0.0" revision commit))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/umayr/badass")
                     (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05c9vdcb5ym3z0n5ll3v39mw4yl9jcjnlydmn0yl89ai9pv71zb6"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/umayr/badass"))
    (synopsis "Hacking contribution graphs in git")
    (description
     "Badass generates false commits for a range of dates, essentially
hacking the gamification of contribution graphs on platforms such as
Github or Gitlab.")
    (home-page "https://github.com/umayr/badass")
    (license license:expat))))

(define-public colobot
  (package
    (name "colobot")
    (version "0.2.0-alpha")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/colobot/colobot")
             (commit (string-append "colobot-gold-" version))
             (recursive? #t)))          ;for "data/" subdir
       (file-name (git-file-name name version))
       (sha256
        (base32 "02z21pw47j2afjsikn5b162gacwgiahdrlhmfxhq4xqlzsvz58z6"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (add-after 'unpack 'fix-directories
           (lambda _
             (substitute* "CMakeLists.txt"
               (("(\\$\\{CMAKE_INSTALL_PREFIX\\})/games" _ prefix)
                (string-append prefix "/bin"))
               (("(\\$\\{CMAKE_INSTALL_PREFIX\\}/share)/games/colobot" _ prefix)
                (string-append prefix "/colobot")))
             #t))
         (add-after 'fix-directories 'install-music
           ;; Retrieve and install music files.
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Installation process tries to download music files using
             ;; "wget" if not already present.  Since we are going another
             ;; route, skip "wget" command check.
             (substitute* "data/music/CMakeLists.txt"
               (("find_program\\(WGET wget\\)") ""))
             ;; Populate "music/" directory.
             (let ((data (assoc-ref inputs "colobot-music")))
               (invoke "tar" "-xvf" data "-Cdata/music"))
             #t)))))
    (native-inputs
     `(("colobot-music"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://colobot.info/files/music/"
                               "colobot-music_ogg_" version ".tar.gz"))
           (sha256
            (base32
             "1s86cd36rwkff329mb1ay1wi5qqyi35564ppgr3f4qqz9wj9vs2m"))))
       ("gettext" ,gettext-minimal)
       ("librsvg" ,librsvg)
       ("po4a" ,po4a)
       ("python" ,python-wrapper)))
    (inputs
     `(("boost" ,boost)
       ("glew" ,glew)
       ("libogg" ,libogg)
       ("libpng" ,libpng)
       ("libsndfile" ,libsndfile)
       ("libvorbis" ,libvorbis)
       ("openal" ,openal)
       ("physfs" ,physfs)
       ("sdl" ,(sdl-union (list sdl2 sdl2-image sdl2-ttf)))))
    (synopsis "Educational programming strategy game")
    (description "Colobot: Gold Edition is a real-time strategy game, where
you can program your units (bots) in a language called CBOT, which is similar
to C++ and Java.  Your mission is to find a new planet to live and survive.
You can save humanity and get programming skills!")
    (home-page "https://colobot.info")
    (license license:gpl3+)))

(define-public gzdoom
  (package
    (name "gzdoom")
    (version "4.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/coelckers/gzdoom")
             (commit (string-append "g" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i4hyg72z84fc6ca2ic9q82q5cbgrbd7bynl3kpkypxvyasq08wz"))
       (patches (search-patches "gzdoom-search-in-installed-share.patch"
                                "gzdoom-find-system-libgme.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove some bundled libraries.  XXX There are more, but removing
           ;; them would require, at least, patching the build system.
           (with-directory-excursion "libraries"
             (delete-file-recursively "bzip2")
             (delete-file-recursively "game-music-emu")
             (delete-file-recursively "jpeg")
             (delete-file-recursively "zlib"))
           #t))))
    (arguments
     '(#:tests? #f
       #:configure-flags
       (let ((out (assoc-ref %outputs "out")))
         (list
          (string-append
           "-DCMAKE_CXX_FLAGS:="
           "-DSHARE_DIR=\\\"" out "/share/\\\" "
           "-DGUIX_OUT_PK3=\\\"" out "/share/games/doom\\\"")

          ;; The build requires some extra convincing not to use the bundled
          ;; libgme previously deleted in the soure snippet.
          "-DFORCE_INTERNAL_GME=OFF"

          ;; Link libraries at build time instead of loading them at run time.
          "-DDYN_OPENAL=OFF"
          "-DDYN_FLUIDSYNTH=OFF"
          "-DDYN_GTK=OFF"
          "-DDYN_MPG123=OFF"
          "-DDYN_SNDFILE=OFF"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-referenced-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((fluid-3 (assoc-ref inputs "fluid-3"))
                   (timidity++ (assoc-ref inputs "timidity++"))
                   (out (assoc-ref outputs "out")))

               (substitute*
                   "src/CMakeLists.txt"
                 (("COMMAND /bin/sh")
                  (string-append "COMMAND " (which "sh"))))

               (substitute*
                   "libraries/zmusic/mididevices/music_fluidsynth_mididevice.cpp"
                 (("/usr/share/sounds/sf2/FluidR3_GM.sf2")
                  (string-append fluid-3 "/share/soundfonts/FluidR3Mono_GM.sf3")))

               (substitute*
                   "libraries/zmusic/mididevices/music_timiditypp_mididevice.cpp"
                 (("exename = \"timidity\"")
                  (string-append "exename = \"" timidity++ "/bin/timidity\"")))
               #t))))))
    (build-system cmake-build-system)
    (inputs `(("bzip2" ,bzip2)
              ("fluid-3" ,fluid-3)
              ("fluidsynth" ,fluidsynth)
              ("gtk+3" ,gtk+)
              ("libgme" ,libgme)
              ("libjpeg" ,libjpeg-turbo)
              ("libsndfile" ,libsndfile)
              ("mesa" ,mesa)
              ("mpg123" ,mpg123)
              ("openal" ,openal)
              ("sdl2" ,sdl2)
              ("timidity++" ,timidity++)
              ("zlib" ,zlib)))
    (native-inputs (list pkg-config unzip))
    (synopsis "Modern Doom 2 source port")
    (description "GZdoom is a port of the Doom 2 game engine, with a modern
renderer.  It improves modding support with ZDoom's advanced mapping features
and the new ZScript language.  In addition to Doom, it supports Heretic, Hexen,
Strife, Chex Quest, and fan-created games like Harmony, Hacx and Freedoom.")
    (home-page "https://zdoom.org/index")
    ;; The source uses x86 assembly
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license (list license:gpl3+         ; gzdoom game
                   license:lgpl3+        ; gzdoom renderer
                   license:expat         ; gdtoa
                   (license:non-copyleft ; modified dumb
                    "file://dumb/licence.txt"
                    "Dumb license, explicitly GPL compatible.")))))

(define-public odamex
  (package
    (name "odamex")
    (version "0.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/odamex/Odamex/" version "/"
             "odamex-src-" version ".tar.bz2"))
       (sha256
        (base32 "1x0c9vnwn336inkfamh4na8xjyfjmzfxfn49j4snqymkypjqw6jq"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f))          ; no tests
    (native-inputs
     (list deutex))
    (inputs
     `(("sdl" ,sdl2)
       ("sdl-mixer" ,sdl2-mixer)
       ("zlib" ,zlib)
       ("libpng" ,libpng)
       ("curl" ,curl-minimal)
       ("alsa-lib" ,alsa-lib)))
    (home-page "https://odamex.net/")
    (synopsis "Multiplayer Doom port")
    (description "Odamex is a modification of the Doom engine that
allows players to easily join servers dedicated to playing Doom
online.")
    (license license:gpl2+)))

(define-public chocolate-doom
  (package
    (name "chocolate-doom")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.chocolate-doom.org/downloads/"
                                  version
                                  "/chocolate-doom-"
                                  version
                                  ".tar.gz"))
              (sha256
               (base32
                "1iy8rx7kjvi1zjiw4zh77szzmd1sgpqajvbhprh1sj93fhbxcdfl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "CFLAGS=-fcommon")))
    (inputs (list sdl2-net sdl2-mixer sdl2))
    (native-inputs
     (list pkg-config))
    (synopsis "Doom source port preserving the look, feel, and bugs of vanilla
Doom")
    (description
     "Chocolate Doom takes a different approach to other source ports.  Its
aim is to accurately reproduce the experience of playing Vanilla Doom.  It is
a conservative, historically accurate Doom source port, which is compatible
with the thousands of mods and levels that were made before the Doom source
code was released.  Rather than flashy new graphics, Chocolate Doom's main
features are its accurate reproduction of the game as it was played in the
1990s.  The project is developed around a carefully-considered philosophy that
intentionally restricts which features may be added (and rejects any that
affect gameplay).")
    (home-page "https://www.chocolate-doom.org/wiki/index.php/Chocolate_Doom")
    (license license:gpl2)))

(define-public crispy-doom
  (package
    (inherit chocolate-doom)
    (name "crispy-doom")
    (version "5.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fabiangreffrath/crispy-doom")
                    (commit (string-append "crispy-doom-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1b6gn0dysv631jynh769whww9xcss1gms78sz3nrn855q1dsvcb4"))))
    (native-inputs
     (append
      (package-native-inputs chocolate-doom)
      `(("automake" ,automake)
        ("autoreconf" ,autoconf))))
    (arguments
     `(#:configure-flags '("CFLAGS=-fcommon")
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           ;; The bundled autogen.sh script unconditionally runs ./configure.
           (lambda _ (invoke "autoreconf" "-vif"))))))
    (synopsis "Limit-removing enhanced-resolution Doom source port based on
Chocolate Doom")
    (description
     "Crispy Doom is a friendly fork of Chocolate Doom that provides a higher
display resolution, removes the static limits of the Doom engine and offers
further optional visual, tactical and physical enhancements while remaining
entirely config file, savegame, netplay and demo compatible with the
original.")
    (home-page "https://www.chocolate-doom.org/wiki/index.php/Crispy_Doom")))

(define shlomif-cmake-modules
  (origin
    (method url-fetch)
    (uri (string-append
          "https://raw.githubusercontent.com/shlomif/shlomif-cmake-modules/"
          "89f05caf86078f783873975525230cf4fecede8a"
          "/shlomif-cmake-modules/Shlomif_Common.cmake"))
    (sha256
     (base32 "05xdikw5ln0yh8p5chsmd8qnndmxg5b5vjlfpdqrjcb1ncqzywkc"))))

(define-public rinutils
  (package
    (name "rinutils")
    (version "0.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/shlomif/rinutils")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05h9sq3w900mx8xij7qgqgqcbdk1x5gvbpz7prw2pfbzrrbiq2ns"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'copy-cmake-modules
                 (lambda _
                   (copy-file #$shlomif-cmake-modules
                              (string-append "cmake/"
                                             (strip-store-file-name
                                              #$shlomif-cmake-modules)))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (with-directory-excursion "../source"
                       (setenv "FCS_TEST_BUILD" "1")
                       (setenv "RINUTILS_TEST_BUILD" "1")
                       ;; TODO: Run tests after setting RINUTILS_TEST_TIDY to `1',
                       ;; which requires tidy-all.
                       ;; (setenv "RINUTILS_TEST_TIDY" "1")
                       (invoke "perl"
                               "CI-testing/continuous-integration-testing.pl"))))))))
    (native-inputs
     (list perl
           ;; The following are needed only for tests.
           perl-class-xsaccessor
           perl-file-find-object
           perl-io-all
           perl-test-differences
           perl-test-runvalgrind
           pkg-config))
    (inputs
     (list cmocka
           perl-env-path
           perl-inline
           perl-inline-c
           perl-string-shellquote
           perl-test-trailingspace
           perl-file-find-object-rule
           perl-text-glob
           perl-number-compare
           perl-moo))
    (home-page "https://www.shlomifish.org/open-source/projects/")
    (synopsis "C11 / gnu11 utilities C library")
    (description "This package provides C11 / gnu11 utilities C library")
    (license license:expat)))

(define-public fortune-mod
  (package
    (name "fortune-mod")
    (version "3.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shlomif/fortune-mod")
             (commit (string-append "fortune-mod-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iq3bxrw8758jqvfqaasd7w1zm0g28g9n25qccnzvr98997h6r2n"))))
    (build-system cmake-build-system)
    (arguments
     (list #:test-target "check"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'enter-build-directory
                 (lambda _
                   (chdir "fortune-mod")))
               (add-after 'enter-build-directory 'symlink-rinutils
                 (lambda _
                   (mkdir-p "rinutils")
                   (symlink #$(this-package-native-input "rinutils")
                            "rinutils/rinutils")))
               (add-after 'enter-build-directory 'copy-cmake-modules
                 (lambda _
                   (copy-file #$shlomif-cmake-modules
                              (string-append "cmake/"
                                             (strip-store-file-name
                                              #$shlomif-cmake-modules)))))
               (add-after 'enter-build-directory 'delete-failing-test
                 (lambda _
                   ;; TODO: Valgrind tests fail for some reason.  Similar issue?
                   ;; https://github.com/shlomif/fortune-mod/issues/21
                   (delete-file "tests/data/valgrind.t")
                   (with-output-to-file "tests/scripts/split-valgrind.pl"
                     (const #t))))
               (add-after 'install 'fix-install-directory
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Move binary from "games/" to "bin/" and remove the
                   ;; latter.  This is easier than patching CMakeLists.txt
                   ;; since the tests hard-code the location as well.
                   (let* ((out   (assoc-ref outputs "out"))
                          (bin   (string-append out "/bin"))
                          (games (string-append out "/games")))
                     (rename-file (string-append games "/fortune")
                                  (string-append bin "/fortune"))
                     (rmdir games)))))))
    (inputs (list recode))
    (native-inputs
     (list perl
           ;; For generating the documentation.
           docbook-xml-5
           docbook-xsl
           perl-app-xml-docbook-builder
           ;; The following are only needed for tests.
           perl-file-find-object
           perl-test-differences
           perl-class-xsaccessor
           perl-io-all
           perl-test-runvalgrind
           rinutils))
    (home-page "https://www.shlomifish.org/open-source/projects/fortune-mod/")
    (synopsis "The Fortune Cookie program from BSD games")
    (description "Fortune is a command-line utility which displays a random
quotation from a collection of quotes.")
    (license license:bsd-4)))

(define xonotic-data
  (package
    (name "xonotic-data")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://dl.xonotic.org/xonotic-"
                           version ".zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32
         "1mcs6l4clvn7ibfq3q69k2p0z6ww75rxvnngamdq5ic6yhq74bx2"))))
    (build-system trivial-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (xonotic (string-append out "/share/xonotic"))
                (source (assoc-ref %build-inputs "source"))
                (unzip (search-input-file %build-inputs "/bin/unzip")))
           (copy-file source (string-append ,name "-" ,version ".zip"))
           (invoke unzip (string-append ,name "-" ,version ".zip"))
           (mkdir-p out)
           (mkdir-p xonotic)
           (chdir "Xonotic")
           (copy-recursively "data"
                             (string-append xonotic "/data"))
           (copy-recursively "server"
                             (string-append xonotic "/server"))
           (install-file "key_0.d0pk" xonotic)))))
    (home-page "http://xonotic.org")
    (synopsis "Data files for Xonotic")
    (description
     "Xonotic-data provides the data files required by the game Xonotic.")
    (license (list license:gpl2+
                   (license:x11-style "file://server/rcon.pl")))))

(define-public xonotic
  (package
    (name "xonotic")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://dl.xonotic.org/xonotic-"
                           version "-source.zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32
         "0axxw04fyz6jlfqd0kp7hdrqa0li31sx1pbipf2j5qp9wvqicsay"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list (string-append "--prefix="
                                              (assoc-ref %outputs "out"))
                               "--disable-rijndael")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'make-darkplaces
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (sharedir (string-append out "/share/xonotic/")))
               (invoke "make" "-C" "source/darkplaces"
                       (string-append "DP_FS_BASEDIR="
                                      sharedir)
                       "DP_LINK_TO_LIBJPEG=1"
                       "DP_SOUND_API=ALSA"
                       "CC=gcc"
                       "-f" "makefile"
                       "cl-release")
               (invoke "make" "-C" "source/darkplaces"
                       (string-append "DP_FS_BASEDIR="
                                      sharedir)
                       "DP_LINK_TO_LIBJPEG=1"
                       "DP_SOUND_API=ALSA"
                       "CC=gcc"
                       "-f" "makefile"
                       "sdl-release")
               (invoke "make" "-C" "source/darkplaces"
                       (string-append "DP_FS_BASEDIR="
                                      sharedir)
                       "DP_LINK_TO_LIBJPEG=1"
                       "DP_SOUND_API=ALSA"
                       "CC=gcc"
                       "-f" "makefile"
                       "sv-release"))))
         (add-before 'configure 'bootstrap
           (lambda _
             (chdir "source/d0_blind_id")
             (invoke "sh" "autogen.sh")))
         (add-after 'build 'install-desktop-entry
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Add .desktop files for the 2 variants and the symlink
             (let* ((output (assoc-ref outputs "out"))
                    (apps (string-append output "/share/applications")))
               (mkdir-p apps)
               (with-output-to-file
                   (string-append apps "/xonotic-glx.desktop")
                 (lambda _
                   (format #t
                           "[Desktop Entry]~@
                     Name=xonotic-glx~@
                     Comment=Xonotic glx~@
                     Exec=~a/bin/xonotic-glx~@
                     TryExec=~@*~a/bin/xonotic-glx~@
                     Icon=xonotic~@
                     Categories=Game~@
                     Type=Application~%"
                           output)))
               (with-output-to-file
                   (string-append apps "/xonotic-sdl.desktop")
                 (lambda _
                   (format #t
                           "[Desktop Entry]~@
                     Name=xonotic-sdl~@
                     Comment=Xonotic sdl~@
                     Exec=~a/bin/xonotic-sdl~@
                     TryExec=~@*~a/bin/xonotic-sdl~@
                     Icon=xonotic~@
                     Categories=Game~@
                     Type=Application~%"
                           output)))
               (with-output-to-file
                   (string-append apps "/xonotic.desktop")
                 (lambda _
                   (format #t
                           "[Desktop Entry]~@
                     Name=xonotic~@
                     Comment=Xonotic~@
                     Exec=~a/bin/xonotic-glx~@
                     TryExec=~@*~a/bin/xonotic~@
                     Icon=xonotic~@
                     Categories=Game~@
                     Type=Application~%"
                           output)))
               #t)))
         (add-after 'install-desktop-entry 'install-icons
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion "../../misc/logos/icons_png/"
                 (for-each
                  (lambda (file)
                    (let* ((size (string-filter char-numeric? file))
                           (icons (string-append out "/share/icons/hicolor/"
                                                 size "x" size "/apps")))
                      (mkdir-p icons)
                      (copy-file file (string-append icons "/xonotic.png"))))
                  '("xonotic_16.png" "xonotic_22.png" "xonotic_24.png"
                    "xonotic_32.png" "xonotic_48.png" "xonotic_64.png"
                    "xonotic_128.png" "xonotic_256.png" "xonotic_512.png"))))))
         (add-after 'install-icons 'install-binaries
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (define (install src dst)
                 (let ((dst (string-append out dst)))
                   (mkdir-p (dirname dst))
                   (copy-file src dst)))
               (mkdir-p (string-append out "/bin"))
               (install "../darkplaces/darkplaces-dedicated"
                        "/bin/xonotic-dedicated")
               (install "../darkplaces/darkplaces-glx"
                        "/bin/xonotic-glx")
               (install "../darkplaces/darkplaces-sdl"
                        "/bin/xonotic-sdl")
               ;; Provide a default xonotic executable, defaulting to SDL.
               (symlink (string-append out "/bin/xonotic-sdl")
                        (string-append out "/bin/xonotic"))
               #t)))
         (add-after 'install-binaries 'install-data
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (data (assoc-ref inputs "xonotic-data")))
               (symlink (string-append data "/share/xonotic")
                        (string-append out "/share/xonotic"))
               #t)))
         (add-after 'install-binaries 'wrap-binaries
           (lambda* (#:key outputs inputs #:allow-other-keys)
             ;; Curl and libvorbis need to be wrapped so that we get
             ;; sound and networking.
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/xonotic"))
                    (bin-sdl (string-append out "/bin/xonotic-sdl"))
                    (bin-glx (string-append out "/bin/xonotic-glx"))
                    (bin-dedicated (string-append out "/bin/xonotic-dedicated"))
                    (curl (assoc-ref inputs "curl"))
                    (vorbis (assoc-ref inputs "libvorbis")))
               (wrap-program bin
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,(string-append curl "/lib:" vorbis "/lib"))))
               (wrap-program bin-sdl
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,(string-append curl "/lib:" vorbis "/lib"))))
               (wrap-program bin-glx
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,(string-append curl "/lib:" vorbis "/lib"))))
               (wrap-program bin-dedicated
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,(string-append curl "/lib:" vorbis "/lib"))))
               #t))))))
    (inputs
     `(("xonotic-data" ,xonotic-data)
       ("alsa-lib" ,alsa-lib)
       ("curl" ,curl)
       ("libjpeg" ,libjpeg-turbo)
       ("libmodplug" ,libmodplug)
       ("libvorbis" ,libvorbis)
       ("libogg" ,libogg)
       ("libxpm" ,libxpm)
       ("libxxf86dga" ,libxxf86dga)
       ("libxxf86vm" ,libxxf86vm)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxau" ,libxau)
       ("libxdmcp" ,libxdmcp)
       ("mesa" ,mesa)
       ("glu" ,glu)
       ("freetype" ,freetype)
       ("sdl2" ,sdl2)
       ("libpng" ,libpng)
       ("hicolor-icon-theme" ,hicolor-icon-theme)))
    (native-inputs
     (list unzip
           autoconf
           automake
           pkg-config
           libtool
           gmp))
    (home-page "https://xonotic.org")
    (synopsis "Fast-paced first-person shooter game")
    (description
     "Xonotic is a free, fast-paced first-person shooter.
The project is geared towards providing addictive arena shooter
gameplay which is all spawned and driven by the community itself.
Xonotic is a direct successor of the Nexuiz project with years of
development between them, and it aims to become the best possible
open-source FPS of its kind.")
    (license (list license:gpl2+
                   license:bsd-3 ; /source/d0_blind_id folder and others
                   (license:x11-style "" "See file rcon.pl.")))))

(define-public frotz
  (package
    (name "frotz")
    (version "2.44")
    (source (origin
              (method url-fetch)
              (uri (list (string-append
                          "http://www.ifarchive.org/if-archive/infocom/interpreters/"
                          "frotz/frotz-" version ".tar.gz")
                         (string-append
                          "ftp://ftp.ifarchive.org/if-archive/infocom/interpreters/"
                          "frotz/frotz-" version ".tar.gz")))
              (sha256
               (base32
                "1v735xr3blznac8fnwa27s1vhllx4jpz7kw7qdw1bsfj6kq21v3k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'curses
           (lambda _
             (substitute* "Makefile"
               (("lcurses") "lncurses"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man6")))
               (install-file "frotz" bin)
               (mkdir-p man)
               (install-file "doc/frotz.6" man)
               #t))))))
    (inputs (list libmodplug libsamplerate libsndfile libvorbis ncurses))
    (synopsis "Portable Z-machine interpreter (ncurses version) for text adventure games")
    (description "Frotz is an interpreter for Infocom games and other Z-machine
games in the text adventure/interactive fiction genre.  This version of Frotz
complies with standard 1.0 of Graham Nelson's specification.  It plays all
Z-code games V1-V8, including V6, with sound support through libao, and uses
ncurses for text display.")
    (home-page "http://frotz.sourceforge.net")
    (license license:gpl2+)))

(define-public naev
  (package
    (name "naev")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/naev/naev/releases/download/v"
                           version "/naev-" version "-source.tar.xz"))
       (sha256
        (base32 "1p6424n2rgrlb0h71gvww40vxs1h583d9im8bzgmv6dhgclbg0nl"))))
    (build-system meson-build-system)
    (arguments
     ;; XXX: Do not add debugging symbols, which cause the build to fail.
     `(#:configure-flags (list "--buildtype=release")
       #:tests? #f))          ;sole test fails with a missing "/dev/dri" error
    (native-inputs
     (list gettext-minimal pkg-config))
    (inputs
     (list freetype
           glpk
           libpng
           libunibreak
           libvorbis
           libwebp
           libxml2
           luajit
           openal
           openblas
           physfs
           python
           python-pyyaml
           (sdl-union (list sdl2 sdl2-image sdl2-mixer))
           suitesparse))
    (home-page "https://naev.org/")
    (synopsis "Game about space exploration, trade and combat")
    (description
     "Naev is a 2d action/rpg space game that combines elements from
the action, RPG and simulation genres.  You pilot a spaceship from
a top-down perspective, and are more or less free to do what you want.
As the genre name implies, you’re able to trade and engage in combat
at will.  Beyond that, there’s an ever-growing number of story-line
missions, equipment, and ships; even the galaxy itself grows larger
with each release.  For the literacy-inclined, there are large amounts
of lore accompanying everything from planets to equipment.")
    (license (list license:gpl3
                   license:public-domain
                   license:expat        ;edtaa3func.c
                   license:bsd-2        ;distance_field.c
                   license:bsd-3        ;perlin.c
                   ))))

(define-public frotz-dumb-terminal
  (package
    (name "frotz-dumb-terminal")
    (version "2.44")
    (source (origin
              (method url-fetch)
              (uri (list (string-append
                          "http://www.ifarchive.org/if-archive/infocom/interpreters/"
                          "frotz/frotz-" version ".tar.gz")
                         (string-append
                          "ftp://ftp.ifarchive.org/if-archive/infocom/interpreters/"
                          "frotz/frotz-" version ".tar.gz")))
              (sha256
               (base32
                "1v735xr3blznac8fnwa27s1vhllx4jpz7kw7qdw1bsfj6kq21v3k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (invoke "make" "dumb")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man6")))
               (install-file "dfrotz" bin)
               (mkdir-p man)
               (install-file "doc/dfrotz.6" man)
               #t))))))
    (synopsis "Portable Z-machine dumb interpreter for text adventure games")
    (description "Frotz is an interpreter for Infocom games and
other Z-machine games in the text adventure/interactive fiction genre.
dfrotz is the dumb interface version.  You get no screen control; everything
is just printed to the terminal line by line.  The terminal handles all the
scrolling.  Maybe you'd like to experience what it's like to play Adventure on
a teletype.  A much cooler use for compiling Frotz with the dumb interface is
that it can be wrapped in CGI scripting, PHP, and the like to allow people
to play games on webpages.  It can also be made into a chat bot.")
    (home-page "http://frotz.sourceforge.net")
    (license license:gpl2+)))

(define-public frotz-sdl
  (let* ((commit "4de8c34f2116fff554af6216c30ec9d41bf50b24"))
    (package
      (name "frotz-sdl")
      (version "2.45pre")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/DavidGriffith/frotz")
                      (commit commit)))
                (sha256
                 (base32
                  "18ms21pcrl7ipcnyqnf8janamkryzx78frsgd9kfk67jvbj0z2k8"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; there are no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-before 'build 'patch-makefile
             (lambda _
               (substitute* "Makefile"
                 (("lcurses") "lncurses")
                 (("^BUILD_DATE_TIME =.*$")
                  "BUILD_DATE_TIME = \"2.45pre-20180907.00000\"\n"))
               #t))
           (replace 'build
             (lambda _
               (invoke "make" "sdl")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (man (string-append out "/share/man/man6")))
                 (install-file "sfrotz" bin)
                 (mkdir-p man)
                 (install-file "doc/sfrotz.6" man)
                 #t))))))
      (native-inputs
       (list pkg-config which perl))
      (inputs (list sdl2
                    sdl2-mixer
                    libmodplug
                    libsamplerate
                    libsndfile
                    libvorbis
                    ncurses
                    freetype
                    libjpeg-turbo))
      (synopsis "Portable Z-machine interpreter (SDL port) for text adventure games")
      (description "Frotz is an interpreter for Infocom games and other Z-machine
games in the text adventure/interactive fiction genre.  This version of Frotz
using SDL fully supports all these versions of the Z-Machine including the
graphical version 6.  Graphics and sound are created through the use of the SDL
libraries.  AIFF sound effects and music in MOD and OGG formats are supported
when packaged in Blorb container files or optionally from individual files.")
      (home-page "http://frotz.sourceforge.net")
      (license license:gpl2+))))

(define-public frozen-bubble
  ;; Last official release is very outdated (2010).  Use latest commit (2017).
  (let ((commit "d6a029110ad6ab9e4960052e175addc98807fb7e")
        (revision "1"))
    (package
      (name "frozen-bubble")
      (version (git-version "2.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kthakore/frozen-bubble")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1rfrcym5lf4qac2qdklikb1ywijyxypq298azzxahy461dadl6cx"))))
      (build-system perl-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           ;; Build process needs to create files in the "server"
           ;; directory.
           (add-after 'unpack 'fix-permissions
             (lambda _
               (for-each make-file-writable
                         (find-files "server" "." #:directories? #t))))
           ;; By default, build stops at warnings.
           (add-after 'unpack 'prevent-build-error
             (lambda _
               (substitute* "inc/My/Builder.pm"
                 (("-Werror") ""))
               #t))
           (add-after 'install 'install-desktop-file-and-icons
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((share (string-append (assoc-ref outputs "out") "/share"))
                      (hicolor (string-append share "/icons/hicolor")))
                 ;; Create desktop entry.
                 (make-desktop-entry-file
                  (string-append share "/applications/" ,name ".desktop")
                  #:name "Frozen Bubble"
                  #:comment "Frozen Bubble arcade game"
                  #:exec ,name
                  #:icon ,name
                  #:categories '("Game" "ArcadeGame"))
                 ;; Add icons.
                 (with-directory-excursion "share/icons"
                   (for-each
                    (lambda (size)
                      (let* ((dim (string-append size "x" size))
                             (dir (string-append hicolor "/" dim "/apps")))
                        (mkdir-p dir)
                        (copy-file
                         (string-append "frozen-bubble-icon-" dim ".png")
                         (string-append dir "/frozen-bubble.png"))))
                    '("16" "32" "48" "64"))))
               #t))
           (add-after 'install 'wrap-perl-libs
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (perl5lib (getenv "PERL5LIB")))
                 (for-each (lambda (prog)
                             (wrap-program (string-append out "/" prog)
                               `("PERL5LIB" ":" prefix
                                 (,(string-append perl5lib ":" out
                                                  "/lib/perl5/site_perl")))))
                           (find-files "bin" ".")))
               #t)))))
      (native-inputs
       (list perl-alien-sdl perl-capture-tiny perl-locale-maketext-lexicon
             perl-module-build pkg-config))
      (inputs
       `(("glib" ,glib)
         ("perl-compress-bzip2" ,perl-compress-bzip2)
         ("perl-file-sharedir" ,perl-file-sharedir)
         ("perl-file-slurp" ,perl-file-slurp)
         ("perl-file-which" ,perl-file-which)
         ("perl-ipc-system-simple" ,perl-ipc-system-simple)
         ("perl-sdl" ,perl-sdl)
         ("sdl" ,(sdl-union (list sdl sdl-image sdl-mixer sdl-pango sdl-ttf)))))
      (home-page "http://frozen-bubble.org/")
      (synopsis "Puzzle with bubbles")
      (description
       "Frozen-Bubble is a clone of the popular Puzzle Bobble game, in which
you attempt to shoot bubbles into groups of the same color to cause them to
pop.

Players compete as penguins and must use the arrow keys to aim a colored
bubble at groups of bubbles.  The objective is to clear all the bubbles off
the screen before a bubble passes below a line at the bottom.

It features 100 single-player levels, a two-player mode, music and striking
graphics.  A level editor is also included to allow players to create and play
their own levels.")
      (license license:gpl2))))

(define-public libmanette
  (package
    (name "libmanette")
    (version "0.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libmanette/"
                                  (version-major+minor version) "/"
                                  "libmanette-" version ".tar.xz"))
              (sha256
               (base32
                "1b3bcdkk5xd5asq797cch9id8692grsjxrc1ss87vv11m1ck4rb3"))))
    (build-system meson-build-system)
    (native-inputs
     (list `(,glib "bin") ; for glib-compile-resources
           gobject-introspection pkg-config vala))
    (inputs
     (list libevdev libgudev))
    (home-page "https://wiki.gnome.org/Apps/Games")
    (synopsis "Game controller library")
    (description "Libmanette is a small GObject library giving you simple
access to game controllers.  It supports the de-facto standard gamepads as
defined by the W3C standard Gamepad specification or as implemented by the SDL
GameController.")
    (license license:lgpl2.1+)))

(define-public quadrapassel
  (package
    (name "quadrapassel")
    (version "40.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/quadrapassel/"
                                  (version-major version) "/"
                                  "quadrapassel-" version ".tar.xz"))
              (sha256
               (base32
                "02n0khwy38pykw4xqpnkym6xvj2sv8izfjbaxlik3iq7890j5n0b"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson_post_install.py"
               (("gtk-update-icon-cache") (which "true"))))))))
    (native-inputs
     (list desktop-file-utils           ; for desktop-file-validate
           gettext-minimal
           (list glib "bin")            ; for glib-compile-resources
           itstool
           libxml2                      ; for xmllint
           pkg-config
           vala))
    (inputs
     (list clutter
           clutter-gtk
           gsound
           gtk+
           libcanberra
           libmanette
           librsvg))
    (home-page "https://wiki.gnome.org/Apps/Quadrapassel")
    (synopsis "GNOME version of Tetris")
    (description "Quadrapassel comes from the classic falling-block game,
Tetris.  The goal of the game is to create complete horizontal lines of
blocks, which will disappear.  The blocks come in seven different shapes made
from four blocks each: one straight, two L-shaped, one square, and two
S-shaped.  The blocks fall from the top center of the screen in a random
order.  You rotate the blocks and move them across the screen to drop them in
complete lines.  You score by dropping blocks fast and completing lines.  As
your score gets higher, you level up and the blocks fall faster.")
    (license license:gpl2+)))

(define-public endless-sky
  (package
    (name "endless-sky")
    (version "0.9.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/endless-sky/endless-sky")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12iganf8dxiyrjznnabsarxjsr0h717j3k4mz15p0k67wxyahhmf"))))
    (build-system scons-build-system)
    (arguments
     `(#:scons ,scons-python2
       #:scons-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Look for resources in the store directory.
             (substitute* "source/Files.cpp"
               (("/usr/local") (assoc-ref outputs "out")))
             ;; Install game binary into %out/bin.
             (substitute* "SConstruct"
               (("games\"") "bin\""))))
         (add-before 'build 'use-gcc-ar
           ;; Use gcc-ar to support LTO.
           (lambda _ (setenv "AR" "gcc-ar"))))))
    (inputs
     `(("glew" ,glew)
       ("libjpeg" ,libjpeg-turbo)
       ("libmad" ,libmad)
       ("libpng" ,libpng)
       ("openal" ,openal)
       ("sdl2" ,sdl2)))
    (home-page "https://endless-sky.github.io/")
    (synopsis "2D space trading and combat game")
    (description "Endless Sky is a 2D space trading and combat game.  Explore
other star systems.  Earn money by trading, carrying passengers, or completing
missions.  Use your earnings to buy a better ship or to upgrade the weapons and
engines on your current one.  Blow up pirates.  Take sides in a civil war.  Or
leave human space behind and hope to find friendly aliens whose culture is more
civilized than your own.")
    (license (list license:gpl3+
                   license:cc-by-sa3.0
                   license:cc-by-sa4.0
                   license:public-domain))))

(define-public stepmania
  (package
    (name "stepmania")
    (version "5.1.0-b2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stepmania/stepmania")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0a7y9l7xm510vgnpmj1is7p9m6d6yd0fcaxrjcickz295k5w3rdn"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove song files, which are licensed under a non-commercial
           ;; clause, and a course pointing to them.
           (for-each delete-file-recursively
                     '("Songs/StepMania 5/Goin' Under"
                       "Songs/StepMania 5/MechaTribe Assault"
                       "Songs/StepMania 5/Springtime"))
           (for-each delete-file '("Courses/Default/Jupiter.crs"
                                   "Courses/Default/Jupiter.png"))
           ;; Unbundle libpng.
           (substitute* "extern/CMakeLists.txt"
             (("include\\(CMakeProject-png.cmake\\)") ""))
           (delete-file-recursively "extern/libpng")
           #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;FIXME: couldn't find how to run tests
       #:build-type "Release"
       #:out-of-source? #f              ;for the 'install-desktop' phase
       #:configure-flags
       (list "-DWITH_SYSTEM_FFMPEG=1"
             ;; SSE instructions are available on Intel systems only.
             ,@(if (any (cute string-prefix? <> (or (%current-target-system)
                                                    (%current-system)))
                        '("x86_64" "i686"))
                   '()
                   '("-DWITH_SSE2=NO"))
             ;; Configuration cannot find GTK2 without the two following
             ;; flags.
             (string-append "-DGTK2_GDKCONFIG_INCLUDE_DIR="
                            (assoc-ref %build-inputs "gtk+")
                            "/lib/gtk-2.0/include")
             (string-append "-DGTK2_GLIBCONFIG_INCLUDE_DIR="
                            (assoc-ref %build-inputs "glib")
                            "/lib/glib-2.0/include"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'ensure-application-files-can-be-found
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "src/arch/LoadingWindow/LoadingWindow_Gtk.cpp"
                 (("RageFileManagerUtil::sDirOfExecutable \\+ \"/\" \\+ \"GtkModule.so\"")
                  (string-append "\"" out
                                 "/share/stepmania/GtkModule.so\"")))
               (substitute* "src/arch/ArchHooks/ArchHooks_Unix.cpp"
                 (("Root = sDirOfExecutable")
                  (string-append "Root = \"" out "/share/stepmania/\""))
                 (("sDirOfExecutable \\+ \"/(Packages|Songs)\"" _ dir)
                  (string-append "\"" out "/share/stepmania/" dir "\"")))
               (substitute* "src/RageFileManager.cpp"
                 (("RageFileManagerUtil::sDirOfExecutable \\+ \"/\"")
                  (string-append "\"" out "/share/stepmania/\""))))
             #t))
         (add-after 'unpack 'fix-install-subdir
           ;; Installation would be done in "%out/stepmania-X.Y", but we
           ;; prefer the more common layout "%out/share/stepmania".
           (lambda _
             (substitute* "src/CMakeLists.txt"
               (("\"stepmania-.*?\"") "\"share/stepmania\""))
             #t))
         (add-after 'unpack 'unbundle-libpng
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/CMakeLists.txt"
               (("\\$\\{SM_EXTERN_DIR\\}/libpng/include")
                (string-append (assoc-ref inputs "libpng") "/include")))
             #t))
         (add-after 'install 'install-executable
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (exe (string-append out "/share/stepmania/stepmania")))
               (mkdir-p bin)
               (symlink exe (string-append bin "/stepmania"))
               #t)))
         (add-after 'install-executable 'install-desktop
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (applications (string-append share "/applications"))
                    (icons (string-append share "/icons")))
               (install-file "stepmania.desktop" applications)
               (mkdir-p icons)
               (copy-recursively "icons" icons)
               #t)))
         ;; Move documentation in a more usual place, i.e.,
         ;; "%out/share/doc/stepmania/".
         (add-after 'install-desktop 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share")))
               (with-directory-excursion share
                 (mkdir-p "doc")
                 (symlink "../stepmania/Docs" "doc/stepmania"))
               #t))))))
    (native-inputs
     (list pkg-config yasm))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ;; Per upstream, StepMania is only guaranteed to work with a very
       ;; specific FFmpeg version, which is included in the repository as
       ;; a Git submodule.  This particular version requirement usually
       ;; changes every few years.
       ("ffmpeg" ,ffmpeg-for-stepmania)
       ("glib" ,glib)
       ("glew" ,glew)
       ("gtk+" ,gtk+-2)
       ("jsoncpp" ,jsoncpp)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg-turbo)
       ("libmad" ,libmad)
       ("libogg" ,libogg)
       ("libva" ,libva)
       ("libvorbis" ,libvorbis)
       ("libxinerama" ,libxinerama)
       ("libxrandr" ,libxrandr)
       ("mesa" ,mesa)
       ("pcre" ,pcre)
       ("pulseaudio" ,pulseaudio)
       ("sdl" ,sdl2)
       ("udev" ,eudev)
       ("zlib" ,zlib)))
    (synopsis "Advanced rhythm game designed for both home and arcade use")
    (description "StepMania is a dance and rhythm game.  It features 3D
graphics, keyboard and dance pad support, and an editor for creating your own
steps.

This package provides the core application, but no song is shipped.  You need
to download and install them in @file{$HOME/.stepmania-X.Y/Songs} directory.")
    (home-page "https://www.stepmania.com")
    (license license:expat)))

(define-public oshu
  (package
    (name "oshu")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fmang/oshu")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1g598incc6zlls876slgwqblwiwiszkmqa4xpzw0z7mbjmmzsizz"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           ;; `make test' doesn't actually build the test executable
           (lambda _ (invoke "make" "zerotokei"))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list cairo ffmpeg pango sdl2 sdl2-image))
    (home-page "https://github.com/fmang/oshu/")
    (synopsis "Rhythm game in which you click on circles")
    (description "@i{oshu!} is a minimalist variant of the @i{osu!} rhythm game,
which is played by pressing buttons and following along sliders as they appear
on screen.  Its aim is to be able to play any beatmap even on low-end hardware.

This package provides the core application, but no beatmaps.  You need to
download and unpack them separately.")
    (license license:gpl3+)))

(define-public btanks
  (package
    (name "btanks")
    (version "0.9.8083")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/btanks/btanks-source/"
                           "btanks-" version ".tar.bz2"))
       (sha256
        (base32
         "0ha35kxc8xlbg74wsrbapfgxvcrwy6psjkqi7c6adxs55dmcxliz"))))
    (build-system scons-build-system)
    (arguments
     `(#:tests? #f                      ; there are none
       #:scons ,scons-python2
       #:scons-flags (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'replace-removed-scons-syntax
           (lambda _
             (substitute* "SConstruct"
               (("Options") "Variables")
               (("opts.Add\\(BoolOption.*") "opts.Add('gcc_visibility', 'gcc visibility', 'true')")
               (("opts.Add\\(EnumOption.*") "opts.Add('mode', 'build mode', 'release')"))
             #t))
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append
                      (search-input-directory inputs "/include/SDL")
                      ":" (or (getenv "CPATH") "")))))
         (add-after 'unpack 'fix-compilation-errors
           (lambda _
             (substitute* "mrt/base_file.h"
               (("#include <string>" m)
                (string-append m "\n#include <sys/types.h>")))
             (substitute* '("engine/sl08/sl08.h"
                            "engine/sl08/sl08.py")
               (("signal = NULL") "signal = 0")
               (("object\\(NULL\\)") "object(0)")
               (("func\\(NULL\\)") "func(0)")
               ((" connect\\(signal_ref\\)")
                " this->connect(signal_ref)"))
             (substitute* "math/range_list.h"
               ((" lower_bound\\(value\\)")
                " this->lower_bound(value)")
               (("	erase\\(i\\)")
                "	this->erase(i)"))
             (substitute* "clunk/source.cpp"
               (("using namespace clunk" m)
                (string-append "# define pow10f(x) exp10f(x)\n" m)))
             #t))
         (add-after 'unpack 'find-lua
           (lambda _
             (substitute* "engine/SConscript"
               (("lua5.1") "lua-5.1")
               (("bt_libs.append\\(lua\\)")
                "bt_libs.append(\"lua\")"))
             #t)))))
    (inputs
     `(("expat" ,expat)
       ("glu" ,glu)
       ("libsmpeg" ,libsmpeg-with-sdl1)
       ("libvorbis" ,libvorbis)
       ("lua51" ,lua-5.1)
       ("sdl" ,(sdl-union (list sdl
                                sdl-mixer
                                sdl-image
                                sdl-ttf)))
       ("zlib" ,zlib)))
    (native-inputs
     (list pkg-config zip))
    (home-page "http://btanks.sourceforge.net")
    (synopsis "Multiplayer tank battle game")
    (description "Battle Tanks (also known as \"btanks\") is a funny battle
game, where you can choose one of three vehicles and eliminate your enemy
using the whole arsenal of weapons.  It has original cartoon-like graphics and
cool music, it’s fun and dynamic, it has several network modes for deathmatch
and cooperative.")
    ;; Some parts (e.g. mrt/b64.cpp) are LGPLv2.1+, but the whole package is
    ;; released under GPLv2 or later.  It comes with extra exceptions for the
    ;; developers.
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public slimevolley
  (package
    (name "slimevolley")
    (version "2.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.tuxfamily.org/slime/"
                           "slimevolley_" version ".tar.gz"))
       (sha256
        (base32 "1pi60zjpx95mfdkrbwf4cbzy5lv4v5qrljvgck46qca78i9g9g46"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       ;; Work around build failure: "error adding symbols: DSO
       ;; missing from command line".
       #:configure-flags (list "-DCMAKE_EXE_LINKER_FLAGS=-lm")))
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (inputs
     `(("sdl" ,(sdl-union (list sdl sdl-image sdl-net sdl-ttf)))))
    (home-page "https://slime.tuxfamily.org/")
    (synopsis "Unrealistic 2D volleyball simulation")
    (description
     "Slime Volley is a 2D arcade-oriented volleyball simulation, in
the spirit of some Java games of the same name.

Two teams, 1-3 players each, try to be the first to get 10 points.
This happens when the one ball touches the floor on the other side of
the net.  There can be 1 to 8 balls in game.  Once one ball touches
the ground, the set ends and all balls are served again.")
    (license license:gpl3+)))

(define-public slingshot
  (package
    (name "slingshot")
    (version "0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ryanakca/slingshot")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "19m8b6nsi786bc6gmkp185mwri3r5y249gjmqd5qsc23nnfhgrs1"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (inputs
     (list python2-pygame))
    (home-page "https://github.com/ryanakca/slingshot")
    (synopsis "Simple 2D shooting strategy game set in space")
    (description "Slingshot is a two-dimensional strategy game where two
players attempt to shoot one another through a section of space populated by
planets.  The main feature of the game is that the shots, once fired, are
affected by the gravity of the planets.")
    (license license:gpl2+)))

(define-public 4dtris
  (package
    (name "4dtris")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://launchpad.net/4dtris/"
                           (version-major+minor version)
                           "/" version "/+download/4dtris_"
                           version ".orig.tar.gz"))
       (sha256
        (base32
         "1nfkhcm0l89jyw8yr65na97g4l385zhjf7whkyg47c3v5sdqq2g7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("CFLAGS=-fcommon")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-install-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile.in"
                 (("bindir = /usr/games")
                  (string-append "bindir = " out "/bin"))
                 (("/usr/share/applications")
                  (string-append out "/share/applications"))
                 (("/usr/share/games/4dtris")
                  (string-append out "/share/4dtris"))))
             #t))
         (add-after 'set-paths 'set-sdl-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append
                      (search-input-directory inputs "/include/SDL")
                      ":" (or (getenv "CPATH") ""))))))))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freeglut" ,freeglut)
       ("sdl" ,(sdl-union (list sdl sdl-ttf)))))
    (home-page "https://launchpad.net/4dtris/")
    (synopsis "4D Tetris")
    (description "4D-TRIS is an alteration of the well-known Tetris game.  The
game field is extended to 4D space, which has to filled up by the gamer with
4D hyper cubes.")
    (license license:gpl3)))

(define-public arx-libertatis
  (package
    (name "arx-libertatis")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://arx-libertatis.org/files/arx-libertatis-"
                           version ".tar.xz"))
       (sha256
        (base32
         "035dflxffa98bxmxkrqfizmhvnr09wyhhmzaqxk92772qil7gkxs"))))
    (build-system cmake-build-system)
    (outputs '("out" "installer"))
    (arguments
     '(#:tests? #f                      ; No tests.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-install-helper-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((p7zip (assoc-ref inputs "p7zip"))
                   (innoextract (assoc-ref inputs "innoextract"))
                   (wget (assoc-ref inputs "wget"))
                   (zenity (assoc-ref inputs "zenity")))
               (substitute* "scripts/arx-install-data"
                 (("have innoextract")
                  (string-append "have " innoextract "/bin/innoextract"))
                 (("then innoextract")
                  (string-append "then " innoextract "/bin/innoextract"))
                 (("else innoextract")
                  (string-append "else " innoextract "/bin/innoextract"))
                 (("for _extract_zip_sz in 7za 7z")
                  (string-append "for _extract_zip_sz in " p7zip "/bin/7za"))
                 (("else if have 7z")
                  (string-append "else if have " p7zip "/bin/7za"))
                 (("7z x -tiso")
                  (string-append p7zip "/bin/7z x -tiso"))
                 (("if have wget")
                  (string-append "if have " wget "/bin/wget"))
                 (("wget -O")
                  (string-append wget "/bin/wget -O"))
                 (("for backend in \\$preferred zenity")
                  (string-append "for backend in $preferred " zenity "/bin/zenity"))
                 (("zenity +--title")
                  (string-append zenity "/bin/zenity --title"))
                 (("^zenity\\)")
                  (string-append zenity "/bin/zenity)"))))
             #t))
         (add-after 'install 'move-installer
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (installer (assoc-ref outputs "installer")))
               (mkdir-p (string-append installer "/bin"))
               (rename-file (string-append out "/bin/arx-install-data")
                            (string-append installer "/bin/arx-install-data"))))))))
    (inputs
     (list sdl2
           libepoxy
           glew
           openal
           zlib
           boost
           glm
           freetype
           ;; The following are only needed by the arx-install-data script.
           p7zip ; Install-helper uses it to extract ISO and .cab archives.
           zenity ; GUI for install-helper.
           wget ; Used by the install-helper to download the patch.
           ;; The install-helper needs it to extract the patch.
           innoextract))
    (home-page "https://arx-libertatis.org/")
    (synopsis "Port of Arx Fatalis, a first-person role-playing game")
    (description "Arx Libertatis is a cross-platform port of Arx Fatalis, a 2002
first-person role-playing game / dungeon crawler developed by Arkane Studios.
This port however does not include the game data, so you need to obtain a copy
of the original Arx Fatalis or its demo to play Arx Libertatis.  Arx Fatalis
features crafting, melee and ranged combat, as well as a unique casting system
where the player draws runes in real time to effect the desired spell.")
    (license license:gpl3+)))

(define-public edgar
  (package
    (name "edgar")
    (version "1.35")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/riksweeney/edgar/releases/download/"
                       version "/edgar-" version "-1.tar.gz"))
       (sha256
        (base32 "0hwp73ili10kzx0aibhvgxfddqm94pimdaqhpnba6jzn119834q7"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f            ; there are no tests
                 #:make-flags
                 (list "CC=gcc"
                       (string-append "PREFIX=" (assoc-ref %outputs "out"))
                       (string-append "BIN_DIR=" (assoc-ref %outputs "out") "/bin/"))
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure)
                   (add-before 'build 'fix-env
                     (lambda* (#:key inputs #:allow-other-keys)
                       (setenv "CPATH"
                               (string-append
                                (search-input-directory inputs "/include/SDL2")
                                ":" (or (getenv "CPATH") ""))))))))
    (inputs
     (list (sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf))
           zlib))
    (native-inputs
     (list pkg-config
           autoconf
           automake
           gettext-minimal
           libtool
           which))
    (synopsis "2d action platformer game")
    (description "The Legend of Edgar is a 2D platform game with a persistent world.
When Edgar's father fails to return home after venturing out one dark and stormy night,
Edgar fears the worst: he has been captured by the evil sorcerer who lives in
a fortress beyond the forbidden swamp.")
    (home-page "https://www.parallelrealities.co.uk/games/edgar/")
    (license license:gpl2+)))

(define-public openclonk
  (package
    (name "openclonk")
    (version "8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.openclonk.org/builds/release/" version "/"
                    "openclonk-" version "-src.tar.bz2"))
              (sha256
               (base32
                "0imkqjp8lww5p0cnqf4k4mb2v682mnsas63qmiz17rspakr7fxik"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DAudio_TK=OpenAL")
       #:test-target "tests"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prepare-gmock
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir "gmock")
             (copy-recursively (assoc-ref inputs "googlemock") "gmock")
             (substitute* "tests/CMakeLists.txt"
               (("/usr/src/gmock")
                (string-append (getcwd) "/gmock/googlemock"))
               (("/usr/src/gtest")
                (string-append (getcwd) "/gmock/googletest"))
               (("PATH_SUFFIXES \"src\" \"gtest\"")
                "PATH_SUFFIXES \"src\""))
             #t))
         (add-after 'unpack 'adjust-backward-cpp-includes
           (lambda _
             ;; XXX: The bundled backward-cpp exports a CMake "interface"
             ;; that includes external libraries such as libdl from glibc.
             ;; By default, CMake interface includes are treated as "system
             ;; headers", and GCC behaves poorly when glibc is passed as a
             ;; system header (causing #include_next failures).

             ;; Here we prevent targets that consume the Backward::Backward
             ;; interface from treating it as "system includes".
             (substitute* "CMakeLists.txt"
               (("target_link_libraries\\((.+) Backward::Backward\\)" all target)
                (string-append "set_property(TARGET " target " PROPERTY "
                               "NO_SYSTEM_FROM_IMPORTED true)\n"
                               all)))
             #t))
         (add-after 'unpack 'add-libiberty
           ;; Build fails upon linking executables without this.
           (lambda _
             (substitute* "thirdparty/backward-cpp/BackwardConfig.cmake"
               (("set\\(LIBBFD_LIBRARIES (.*?)\\)" _ libraries)
                (string-append "set(LIBBFD_LIBRARIES " libraries " iberty)")))
             #t))
         (add-after 'add-libiberty 'lax-freealut-requirement
           ;; TODO: We provide freealut 1.1.0, but pkg-config somehow detects
           ;; it as 1.0.1.  Force minimal version.
           (lambda _
             (substitute* "cmake/FindAudio.cmake"
               (("freealut>=1.1.0") "freealut>=1.0.1"))
             #t))
         (add-after 'lax-freealut-requirement 'fix-directories
           ;; Prefer "$out/share/openclonk" over
           ;; "$out/share/games/openclonk". Also install "openclonk"
           ;; binary in "bin/", not "games/".
           (lambda _
             (substitute* "CMakeLists.txt"
               (("share/games/openclonk") "share/openclonk")
               (("TARGETS openclonk DESTINATION games")
                "TARGETS openclonk DESTINATION bin"))
             #t)))))
    (native-inputs
     `(("googlemock" ,(package-source googletest))
       ("googletest" ,googletest)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("freealut" ,freealut)
       ("freetype" ,freetype)
       ("glew" ,glew)
       ("libiberty" ,libiberty)
       ("libjpeg" ,libjpeg-turbo)
       ("libogg" ,libogg)
       ("libpng" ,libpng)
       ("libvorbis" ,libvorbis)
       ("libxrandr" ,libxrandr)
       ("mesa" ,mesa)
       ("miniupnpc" ,miniupnpc)
       ("openal" ,openal)
       ("qtbase" ,qtbase-5)
       ("readline" ,readline)
       ("sdl" ,sdl2)
       ("tinyxml" ,tinyxml)
       ("zlib" ,zlib)))
    (home-page "https://www.openclonk.org/")
    (synopsis
     "Multiplayer action game where you control small and nimble humanoids")
    (description "OpenClonk is a multiplayer action/tactics/skill game.  It is
often referred to as a mixture of The Settlers and Worms.  In a simple 2D
antfarm-style landscape, the player controls a crew of Clonks, small but
robust humanoid beings.  The game encourages free play but the normal goal is
to either exploit valuable resources from the earth by building a mine or
fight each other on an arena-like map.")
    ;; Software as a whole is licensed under ISC, artwork under CC-BY.
    (license (list license:isc license:cc-by3.0))))

(define-public flare-engine
  (package
    (name "flare-engine")
    (version "1.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/flareteam/flare-engine")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0h4xxj6r194pw68m3ngrnzkh6xgiblyrsc54z8abwba8m0mqbvmk"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:configure-flags '("-DBINDIR=bin" "-DDATADIR=share/flare")))
    (inputs
     `(("hicolor-icon-theme" ,hicolor-icon-theme)
       ("python" ,python-wrapper)
       ("sdl" ,(sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf)))))
    (home-page "http://www.flarerpg.org/")
    (synopsis "Action Roleplaying Engine")
    (description "Flare (Free Libre Action Roleplaying Engine) is a simple
game engine built to handle a very specific kind of game: single-player 2D
action RPGs.")
    (license license:gpl3+)))

(define-public flare-game
  (package
    (name "flare-game")
    (version "1.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/flareteam/flare-game")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0h9i128kq6disppbrplkf13zdmsg4cq23nim53mgwpawc4mqz7ga"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:configure-flags '("-DDATADIR=share/flare")
       #:phases
       (modify-phases %standard-phases
         ;; Flare expects the mods to be located in the same folder.
         ;; Yet, "default" mod is in the engine, whereas the others
         ;; are in the current package.  Merge everything here with
         ;; a symlink.
         (add-after 'install 'add-default-mod
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (mods (string-append out "/share/flare/mods")))
               (with-directory-excursion mods
                 (symlink (string-append (assoc-ref inputs "flare-engine")
                                         "/share/flare/mods/default")
                          "default")))
             #t))
         (add-after 'install 'install-executable
           ;; The package only provides assets for the game, the
           ;; executable coming from "flare-engine".  Since more than
           ;; one game may use the engine, we create a new executable,
           ;; "flare-game", which launches the engine with appropriate
           ;; parameters.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash (search-input-file inputs "/bin/bash"))
                    (flare (search-input-file inputs "/bin/flare"))
                    (script (string-append out "/bin/flare-game")))
               (mkdir-p (dirname script))
               (call-with-output-file script
                 (lambda (port)
                   (format port
                           "#!~a
exec ~a --data-path=~a/share/flare --mods=empyrean_campaign~%"
                           bash
                           flare
                           out)))
               (chmod script #o755))
             #t)))))
    (inputs
     (list flare-engine))
    (home-page "http://www.flarerpg.org/")
    (synopsis "Fantasy action RPG using the FLARE engine")
    (description "Flare is a single-player 2D action RPG with
fast-paced action and a dark fantasy style.")
    (license license:cc-by-sa3.0)))

(define-public meritous
  (package
    (name "meritous")
    (version "1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/meritous/meritous.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0n5jm4g0arjllgqmd2crv8h02i6hs3hlh1zyc7ng7yfpg1mbd8p8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:make-flags
       (list "CC=gcc"
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'fix-sdl-path
           ;; XXX: For some reason, `sdl-config' reports stand-alone SDL
           ;; directory, not SDL-union provided as an input to the package.
           ;; We force the latter with "--prefix=" option.
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile"
               (("sdl-config" command)
                (string-append command " --prefix=" (assoc-ref inputs "sdl"))))
             #t))
         (add-after 'unpack 'fix-crash
           ;; XXX: Songs are not present in the repository, due to licensing
           ;; issues.  Yet, the game tries to load them, and, since it cannot
           ;; find them, crashes.  Users cannot add them back, the store being
           ;; read-only, so we turn off background music altogether.
           (lambda _
             (substitute* "src/audio.c"
               (("PlayBackgroundMusic\\(new_track\\);" all)
                (string-append "// " all)))
             #t)))))
    (native-inputs
     (list intltool))
    (inputs
     `(("sdl" ,(sdl-union (list sdl sdl-image sdl-mixer)))
       ("zlib" ,zlib)))
    (home-page "https://gitlab.com/meritous/meritous")
    (synopsis "Action-adventure dungeon crawl game")
    (description "Far below the surface of the planet is a place of limitless
power.  Those that seek to control such a utopia will soon bring an end to
themselves.  Seeking an end to the troubles that plague him, PSI user Merit
journeys into the hallowed Orcus Dome in search of answers.

Meritous is a action-adventure game with simple controls but a challenge to
find a balance of power versus recovery time during real-time battles.  Set in
a procedurally generated world, the player can explore thousands of rooms in
search of powerful artifacts, tools to help them, and to eventually free the
Orcus Dome from evil.")
    (license license:gpl3+)))

(define-public marble-marcher
  (let ((commit "e580460a0c3826f9b28ab404607942a8ecb625d7")
        (revision "1"))
    (package
      (name "marble-marcher")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/HackerPoet/MarbleMarcher")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0jjv832hl1v170n6gryp2sr3lgqndi9ab841qvgqk68bks8701mx"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f  ; there are none
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'embed-asset-directory
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((assets (string-append (assoc-ref outputs "out")
                                            "/share/marble-marcher/assets/")))
                 ;; Some of the files we're patching are
                 ;; ISO-8859-1-encoded, so choose it as the default
                 ;; encoding so the byte encoding is preserved.
                 (with-fluids ((%default-port-encoding #f))
                   (substitute* "src/Resource.rc"
                     (("../assets/icon.ico")
                      (string-append assets "icon.ico")))
                   (substitute* "src/Res.h"
                     (("assets/") assets))))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (assets (string-append out "/share/marble-marcher/assets"))
                      (bin (string-append out "/bin/")))
                 (mkdir-p bin)
                 (mkdir-p assets)
                 (copy-recursively "../source/assets" assets)
                 (install-file "MarbleMarcher" bin))
               #t)))))
      (inputs
       (list eigen mesa sfml))
      (native-inputs
       (list pkg-config))
      (home-page "https://codeparade.itch.io/marblemarcher")
      (synopsis "Guide a marble across fractal landscapes")
      (description "Marble Marcher is a video game that uses a fractal physics
engine and fully procedural rendering to produce beautiful and unique
gameplay.  The game is played on the surface of evolving fractals.  The goal
of the game is to get your marble to the flag as quickly as possible.  But be
careful not to fall off the level or get crushed by the fractal!  There are 24
levels to unlock.")
      ;; Code is under GPLv2+, assets are under CC-BY-SA 3.0 and OFL 1.1.
      (license (list license:gpl2+
                     license:silofl1.1
                     license:cc-by-sa3.0)))))

;; This must be updated together with flightgear.
(define simgear
  (package
    (name "simgear")
    (version "2020.3.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/flightgear/release-"
                           (version-major+minor version) "/"
                           "simgear-" version ".tar.bz2"))
       (sha256
        (base32 "0g2g3n3sb6kdimvcrn9kvlhyyrp5c6lx20fgzz8l609v5aygr3dv"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; There are some bundled libraries.
           (for-each delete-file-recursively
                     '("3rdparty/expat/"))
           #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DSYSTEM_EXPAT=ON")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Skip tests that require internet access.
               (invoke "ctest" "-E" "(http|dns)")))))))
    (inputs
     `(("boost" ,boost)
       ("curl" ,curl)
       ("expat" ,expat)
       ("mesa" ,mesa)
       ("openal" ,openal)
       ("openscenegraph" ,openscenegraph)
       ("zlib" ,zlib)))
    (home-page "https://home.flightgear.org/")
    (synopsis "Libraries for 3D simulations and games")
    (description "SimGear is a set of libraries designed to be used as
building blocks for quickly assembling 3D simulations, games, and
visualization applications.  SimGear is developed by the FlightGear project
and also provides the base for the FlightGear Flight Simulator.")
    (license license:lgpl2.0+)))

(define-public flightgear
  (package
    (name "flightgear")
    (version (package-version simgear))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/flightgear/release-"
                           (version-major+minor version) "/"
                           "flightgear-" version ".tar.bz2"))
       (sha256
        (base32 "15sar94x13j2y1m6adgmz2q1m1i9bzj3sxqla6y3m9vyf33hc9zy"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; There are some bundled libraries.
           (for-each delete-file-recursively
                     '("3rdparty/sqlite3/"
                       "3rdparty/cppunit/"))
           #t))))
    (build-system qt-build-system)
    (arguments
     `(#:configure-flags
       (list "-DSYSTEM_SQLITE=ON"
             "-DSYSTEM_CPPUNIT=ON"
             (string-append "-DFG_DATA_DIR="
                            (assoc-ref %outputs "out")
                            "/share/flightgear"))
       ;; TODO: test suite segfaults.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-some-tests
           (lambda _
             (substitute* "test_suite/unit_tests/Instrumentation/test_gps.hxx"
               (("CPPUNIT_TEST\\(testLongLegWestbound\\);" all)
                (string-append "// " all))
               (("CPPUNIT_TEST\\(testFinalLegCourse\\);" all)
                (string-append "// " all)))))
         (add-after 'build 'build-test-suite
           (lambda* args
             ((assoc-ref %standard-phases 'build)
              #:make-flags (list "fgfs_test_suite"))))
         ;; Test suite needs access to FGData so run it after 'install.
         (delete 'check)
         (add-after 'install-data 'check
           (assoc-ref %standard-phases 'check))
         (add-after 'install 'install-data
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((share (string-append (assoc-ref outputs "out") "/share/flightgear")))
               (mkdir-p share)
               (with-directory-excursion share
                 (invoke "tar" "xf" (assoc-ref inputs "flightgear-data")
                         "--strip-components=1")))
             #t)))))
    (inputs
     (list boost
           dbus
           eudev
           freeglut
           freetype
           glew
           libpng
           openal
           openscenegraph
           plib
           qtbase-5
           qtdeclarative
           qtsvg
           simgear
           speexdsp
           sqlite
           zlib))
    (native-inputs
     `(("cppunit" ,cppunit)
       ("pkg-config" ,pkg-config)
       ("qttools" ,qttools)
       ("flightgear-data"
        ,(origin
           (method url-fetch)
           (uri (string-append "mirror://sourceforge/flightgear/release-"
                               (version-major+minor version) "/"
                               "FlightGear-" version "-data.txz"))
           (sha256
            (base32
             "0n5mw9vw1snab16c1y3i9ylkiv54az57bs2mvpq20hhg5hdiagqj"))))))
    (home-page "https://www.flightgear.org/")
    (synopsis "Flight simulator")
    (description "The goal of the FlightGear project is to create a
sophisticated flight simulator framework for use in research or academic
environments, pilot training, as an industry engineering tool, for DIY-ers to
pursue their favorite interesting flight simulation idea, and last but
certainly not least as a fun, realistic, and challenging desktop flight
simulator.")
    (license license:gpl2+)))

(define-public jumpnbump
  (package
    (name "jumpnbump")
    (version "1.61")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/LibreGames/jumpnbump.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12lwl5sl5n009nb83r8l4lakb9286csqdf1ynpmwwydy17giqsdp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f                      ;no test
       #:phases
       (modify-phases %standard-phases
         ;; There is no configure script
         (replace 'configure
           (lambda _
             (substitute* "Makefile"
               (("-funroll-loops")
                "-funroll-loops -fcommon")
               (("SDL_CFLAGS =")
                "SDL_CFLAGS = -fcommon"))))
         (add-after 'unpack 'fix-sdl-path
           ;; XXX: For some reason, `sdl2-config' reports stand-alone SDL
           ;; directory, not SDL-union provided as an input to the package.
           ;; We force the latter with "--prefix=" option.
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile"
               (("sdl2-config" command)
                (string-append command " --prefix=" (assoc-ref inputs "sdl")))))))))
    (inputs
     `(("bzip2" ,bzip2)
       ("sdl" ,(sdl-union (list sdl2 sdl2-mixer sdl2-net)))
       ("zlib" ,zlib)))
    (native-inputs
     `(("gettext" ,gettext-minimal)))   ;for msgfmt
    (home-page "https://gitlab.com/LibreGames/jumpnbump")
    (synopsis "Multiplayer platform game with bunnies")
    (description "You, as a bunny, have to jump on your opponents to make them
explode.  It is a true multiplayer game; you cannot play this alone.  You can
play with up to four players simultaneously.  It has network support.")
    (license license:gpl2+)))

(define-public hedgewars
  (package
    (name "hedgewars")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.hedgewars.org/download/releases/"
                                  "hedgewars-src-" version ".tar.bz2"))
              (patches (search-patches "hedgewars-network-bsd.patch"))
              (sha256
               (base32
                "0nqm9w02m0xkndlsj6ys3wr0ik8zc14zgilq7k6fwjrf3zk385i1"))))
    (build-system cmake-build-system)
    (arguments
     ;; XXX: Engine is built as Pascal source code, requiring Free Pascal
     ;; Compiler, which we haven't packaged yet.  With the flag below, we use
     ;; a Pascal to C translator and Clang instead.
     `(#:configure-flags (list "-DBUILD_ENGINE_C=ON"
                               "-Dhaskell_flags=-dynamic;-fPIC")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-sources
           (lambda _
             ;; Fix a missing 'include'.
             (substitute* "QTfrontend/ui/page/pagegamestats.cpp"
               (("#include <QSizePolicy>")
                "#include <QSizePolicy>\n#include <QPainterPath>"))))
         (replace 'check
           (lambda _ (invoke "ctest")))
         (add-after 'install 'install-icon
           (lambda _
             ;; Install icon for the desktop file.
             (let* ((out (assoc-ref %outputs "out"))
                    (icons (string-append out "/share/icons/hicolor/512x512/apps")))
               (with-directory-excursion (string-append "../hedgewars-src-" ,version)
                 (install-file "misc/hedgewars.png" icons)))
             #t)))))
    (inputs
     `(("ffmpeg" ,ffmpeg)
       ("freeglut" ,freeglut)
       ("ghc-entropy" ,ghc-entropy)
       ("ghc-hslogger" ,ghc-hslogger)
       ("ghc-network" ,ghc-network)
       ("ghc-random" ,ghc-random)
       ("ghc-regex-tdfa" ,ghc-regex-tdfa)
       ("ghc-sandi" ,ghc-sandi)
       ("ghc-sha" ,ghc-sha)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-vector" ,ghc-vector)
       ("ghc-zlib" ,ghc-zlib)
       ("glew" ,glew)
       ("libpng" ,libpng)
       ("lua" ,lua-5.1)
       ("physfs" ,physfs)
       ("qtbase" ,qtbase-5)
       ("sdl" ,(sdl-union
                (list sdl2 sdl2-mixer sdl2-net sdl2-ttf sdl2-image)))))
    (native-inputs
     (list clang ghc pkg-config qttools))
    (home-page "https://hedgewars.org/")
    (synopsis "Turn-based artillery game featuring fighting hedgehogs")
    (description
     "Hedgewars is a turn based strategy, artillery, action and comedy game,
featuring the antics of pink hedgehogs with attitude as they battle from the
depths of hell to the depths of space.

As commander, it's your job to assemble your crack team of hedgehog soldiers
and bring the war to your enemy.")
    ;; Software as a whole is licensed under GPL-2 terms.  Artwork and
    ;; scripts are distributed under various terms.
    (license (list license:gpl2
                   license:bsd-2 license:bsd-3 license:cc-by3.0 license:cc0
                   license:expat license:fdl1.3+ license:public-domain
                   license:zlib))))

(define-public go-github-com-anaseto-gruid
  (package
    (name "go-github-com-anaseto-gruid")
    (version "0.21.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anaseto/gruid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0rvsavkvg2hziwdh8sjk3n5v92m5mfjb8v9m7ch22maxfwq5kv6y"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/anaseto/gruid"))
    (propagated-inputs
     (list go-golang-org-x-image))
    (home-page "https://github.com/anaseto/gruid")
    (synopsis "Cross-platform grid-based UI and game framework")
    (description "The gruid module provides packages for easily building
grid-based applications in Go.  The library abstracts rendering and input for
different platforms.  There are drivers available for terminal apps, native
graphical apps and browser apps.  The original application for the library was
creating grid-based games, but it's also well suited for any grid-based
application.")
    (license license:isc)))

(define-public go-github-com-anaseto-gruid-tcell
  (package
    (name "go-github-com-anaseto-gruid-tcell")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/anaseto/gruid-tcell")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "09ajr9mbldjfc44qprplbf8dr8yhlbn2nfnas2z62m9wmklc0qiv"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/anaseto/gruid-tcell"))
    (propagated-inputs
     `(("go-github-com-gdamore-tcell-v2" ,go-github-com-gdamore-tcell-v2)
       ("go-github-com-anaseto-gruid" ,go-github-com-anaseto-gruid)))
    (home-page "https://github.com/anaseto/gruid-tcell")
    (synopsis "Gruid driver using the tcell library")
    (description "The gruid-tcell module provides a Gruid driver for building
terminal full-window applications.")
    (license license:isc)))

(define-public harmonist
  (package
    (name "harmonist")
    (version "0.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://git.tuxfamily.org/harmonist/harmonist.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17ai39pw9xq4asfvhs0whx07hljlivygazbwrxjrnxwrn06483hr"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "git.tuxfamily.org/harmonist/harmonist"))
    (inputs
     `(("go-github-com-gdamore-tcell-v2" ,go-github-com-gdamore-tcell-v2)
       ("go-github.com-nsf-termbox-go" ,go-github.com-nsf-termbox-go)
       ("go-github-com-anaseto-gruid" ,go-github-com-anaseto-gruid)
       ("go-github-com-anaseto-gruid-tcell" ,go-github-com-anaseto-gruid-tcell)))
    (home-page "https://harmonist.tuxfamily.org/")
    (synopsis "Stealth coffee-break roguelike game")
    (description "Harmonist: Dayoriah Clan Infiltration is a stealth
coffee-break roguelike game.  The game has a heavy focus on tactical
positioning, light and noise mechanisms, making use of various terrain types
and cones of view for monsters.  Aiming for a replayable streamlined experience,
the game avoids complex inventory management and character building, relying
on items and player adaptability for character progression.")
    (license license:isc)))

(define-public drascula
  (package
    (name "drascula")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/scummvm/extras/"
                                  "Drascula_%20The%20Vampire%20Strikes%20Back/"
                                  "drascula-" version ".zip"))
              (sha256
               (base32
                "1pj29rpb754sn6a56f8brfv6f2m1p5qgaqik7d68pfi2bb5zccdp"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match))
         (let* ((out (assoc-ref %outputs "out"))
                (share (string-append out "/share/drascula"))
                (scummvm (assoc-ref %build-inputs "scummvm")))
           ;; Install data.
           (let ((unzip (search-input-file %build-inputs "/bin/unzip"))
                 (doc (string-append out "/share/doc/" ,name "-" ,version)))
             (for-each
              (lambda (input)
                (invoke unzip
                        "-j"
                        (assoc-ref %build-inputs input)
                        "-x" "__MACOSX")
                ;; Every input provides "readme.txt", and we want to
                ;; preserve them all.  Therefore we rename them first.
                (match input
                  ("drascula-int"
                   (rename-file "readme.txt" "readme-international.txt"))
                  ("drascula-audio"
                   (rename-file "readme.txt" "readme-audio.txt"))
                  (_ #f))
                ;; Install documentation.
                (for-each (lambda (f) (install-file f doc))
                          (find-files "." "\\.(txt|doc)$"))
                ;; Install data.
                (for-each (lambda (f) (install-file f share))
                          (find-files "." "\\.(ogg|00[0-9])$")))
              '("drascula-audio" "drascula-int" "source")))
           ;; Create standalone executable.
           (let* ((bin (string-append out "/bin"))
                  (executable (string-append bin "/drascula"))
                  (bash (search-input-file %build-inputs "/bin/bash")))
             (mkdir-p bin)
             (with-output-to-file executable
               (lambda ()
                 (format #t "#!~a~%" bash)
                 (format #t
                         "exec ~a/bin/scummvm --path=~a drascula~%"
                         scummvm share)))
             (chmod executable #o755))
           ;; Create desktop file.  There is no dedicated icon for the
           ;; game, so we borrow SCUMMVM's.
           (let ((apps (string-append out "/share/applications")))
             (mkdir-p apps)
             (make-desktop-entry-file
              (string-append apps "/drascula.desktop")
              #:name "Drascula: The Vampire Strikes Back"
              #:generic-name "Drascula"
              #:exec (string-append out "/bin/drascula")
              #:icon (string-append scummvm "/share/icons/hicolor/scalable/apps/scummvm.svg")
              #:categories '("AdventureGame" "Game" "RolePlaying")
              #:keywords '("game" "adventure" "roleplaying" "2D" "fantasy")
              #:comment '((#f "Classic 2D point and click adventure game")
                          ("de" "Klassisches 2D-Abenteuerspiel in Zeigen-und-Klicken-Manier")
                          ("fr" "Jeu classique d'aventure pointer-et-cliquer en 2D")
                          ("it" "Gioco classico di avventura punta e clicca 2D"))))
           #t))))
    (native-inputs
     (list bash unzip))
    (inputs
     `(("scummvm" ,scummvm)
       ("drascula-int"
        ,(let ((version "1.1"))
           (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/scummvm/extras/"
                                 "Drascula_%20The%20Vampire%20Strikes%20Back/"
                                 "drascula-int-" version ".zip"))
             (sha256
              (base32
               "12236i7blamal92p1i8dgp3nhp2yicics4whsl63v682bj999n14")))))
       ("drascula-audio"
        ,(let ((version "2.0"))
           (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/scummvm/extras/"
                                 "Drascula_%20The%20Vampire%20Strikes%20Back/"
                                 "drascula-audio-" version ".zip"))
             (sha256
              (base32
               "00g4izmsqzxb8ry1vhfx6jrygl58lvlij09nw01ds4zddsiznsky")))))))
    (home-page "https://www.scummvm.org")
    (synopsis "Classic 2D point and click adventure game")
    (description "Drascula: The Vampire Strikes Back is a classic humorous 2D
point and click adventure game.

In Drascula you play the role of John Hacker, a British estate agent, that
gets to meet a gorgeous blond girl who is kidnapped by the notorious vampire
Count Drascula and embark on a fun yet dangerous quest to rescue her.
Unfortunately, Hacker is not aware of Drascula's real ambitions: DOMINATING
the World and demonstrating that he is even more evil than his brother Vlad.")
    ;; Drascula uses a BSD-like license.
    (license (license:non-copyleft "file:///readme.txt"))))

(define (make-lure-package name language hash)
  (package
    (name name)
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/scummvm/extras/"
             "Lure%20of%20the%20Temptress/"
             name "-" version ".zip"))
       (sha256
        (base32 hash))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (share (string-append out "/share"))
                (data (string-append share "/" ,name "/" ,language))
                (apps (string-append share "/applications"))
                (bin (string-append out "/bin"))
                (executable (string-append bin "/" ,name))
                (scummvm (assoc-ref %build-inputs "scummvm")))
           (let ((unzip (search-input-file %build-inputs "/bin/unzip")))
             (invoke unzip "-j" (assoc-ref %build-inputs "source")))
           (let ((doc (string-append share "/doc/" ,name "-" ,version)))
             (for-each (lambda (f) (install-file f doc))
                       (find-files "." "\\.(txt|PDF|pdf)$")))
           (for-each (lambda (f) (install-file f data))
                     (find-files "." "\\.(vga|VGA)$"))
           ;; Build the executable.
           (mkdir-p bin)
           (let ((bash (assoc-ref %build-inputs "bash")))
             (with-output-to-file executable
               (lambda ()
                 (format #t "#!~a/bin/bash~%" bash)
                 (format #t "exec ~a/bin/scummvm -q ~a -p ~a lure~%"
                         scummvm ,language data))))
           (chmod executable #o755)
           ;; Create desktop file.  There is no dedicated
           ;; icon for the game, so we borrow SCUMMVM's.
           (mkdir-p apps)
           (with-output-to-file (string-append apps "/" ,name ".desktop")
             (lambda _
               (format #t
                       "[Desktop Entry]~@
                     Name=Lure of the Temptress~@
                     GenericName=Lure~@
                     Exec=~a~@
                     Icon=~a/share/icons/hicolor/scalable/apps/scummvm.svg~@
                     Categories=AdventureGame;Game;RolePlaying;~@
                     Keywords=game;adventure;roleplaying;2D,fantasy;~@
                     Comment=Classic 2D point and click adventure game~@
                     Comment[de]=klassisches 2D-Abenteuerspiel in Zeigen-und-Klicken-Manier~@
                     Comment[fr]=Jeu classique d'aventure pointer-et-cliquer en 2D~@
                     Comment[it]=Gioco classico di avventura punta e clicca 2D~@
                     Type=Application~%"
                       executable scummvm)))
           #t))))
    (native-inputs
     (list unzip))
    (inputs
     (list bash scummvm))
    (home-page "https://www.scummvm.org")
    (synopsis "2D point and click fantasy adventure game")
    (description
     "Lure of the Temptress is a classic 2D point and click adventure game.

You are Diermot, an unwilling hero who'd prefer a quiet life, and are, to all
intents and purposes, a good man.  After decades of unrest the King has united
the warring factions in his kingdom and all his lands are at peace, except
a remote region around a town called Turnvale.  A revolt has recently taken
place in Turnvale, a revolt orchestrated by an apprentice sorceress called
Selena, the titular temptress.  The king calls together his finest horsemen
and heads off (with you in tow) to Turnvale just to witness how hellish
mercenary monsters called Skorl are invading the town.

The king's men are defeated, the king is killed and you fall of your horse and
bang your head heavily on the ground.  You have been *unconscious for a while
when you realize that you are in a dingy cell guarded by a not so friendly
Skorl.  Maybe it would be an idea to try and escape...")
    (license (license:non-copyleft "file:///README"))))

(define-public lure
  (make-lure-package
   "lure" "en" "0201i70qcs1m797kvxjx3ygkhg6kcl5yf49sihba2ga8l52q45zk"))

(define-public lure-de
  (make-lure-package
   "lure-de" "de" "0sqq7h5llml6rv85x0bfv4bgzwhs4c82p4w4zmfcaab6cjlad0sy"))

(define-public lure-es
  (make-lure-package
   "lure-es" "es" "1dvv5znvlsakw6w5r16calv9jkgw27aymgybsf4q22lcmpxbj1lk"))

(define-public lure-fr
  (make-lure-package
   "lure-fr" "fr" "1y51jjb7f8023832g44vd1jsb6ni85586pi2n5hjg9qjk6gi90r9"))

(define-public lure-it
  (make-lure-package
   "lure-it" "it" "1ks6n39r1cllisrrh6pcr39swsdv7ng3gx5c47vaw71zzfr70hjj"))

(define (make-queen-package name file-prefix release language hash)
  (package
    (name name)
    (version release)
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/scummvm/extras/"
                           "Flight%20of%20the%20Amazon%20Queen/"
                           file-prefix release ".zip"))
       (sha256
        (base32 hash))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (share (string-append out "/share"))
                (data (string-append share "/" ,name))
                (apps (string-append share "/applications"))
                (bin (string-append out "/bin"))
                (executable (string-append bin "/" ,name))
                (scummvm (assoc-ref %build-inputs "scummvm")))
           (let ((unzip (search-input-file %build-inputs "/bin/unzip")))
             (invoke unzip "-j" (assoc-ref %build-inputs "source")))
           (let ((doc (string-append share "/doc/" ,name "-" ,version)))
             (install-file "readme.txt" doc))
           (install-file "queen.1c" data)
           (mkdir-p bin)
           (let ((bash (assoc-ref %build-inputs "bash")))
             (with-output-to-file executable
               (lambda ()
                 (format #t "#!~a/bin/bash~%" bash)
                 (format #t "exec ~a/bin/scummvm -q fr -p ~a queen~%"
                         scummvm data))))
           (chmod executable #o755)
           ;; Create desktop file.  There is no dedicated
           ;; icon for the game, so we borrow SCUMMVM's.
           (mkdir-p apps)
           (with-output-to-file (string-append apps "/" ,name ".desktop")
             (lambda _
               (format #t
                       "[Desktop Entry]~@
                       Name=Flight of the Amazon Queen~@
                       GenericName=Queen~@
                       Comment=Embark on a quest to rescue a kidnapped princess and in the process, discover the true sinister intentions of a suspiciously located Lederhosen company~@
                       Comment[de]=Begib dich auf ein Abenteuer, um eine entführte Prinzessin zu retten und entdecke die wahren, finsteren Absichten eines verdächtig erscheinenden Lederhosen-Unternehmens~@
                       Type=Application~@
                       Exec=~a~@
                       Icon=~a/share/icons/hicolor/scalable/apps/scummvm.svg~@
                       Categories=AdventureGame;Game;RolePlaying;~@
                       Keywords=adventure;game;roleplaying;fantasy;~%"
                       executable scummvm))))
         #t)))
    (native-inputs
     (list unzip))
    (inputs
     (list bash scummvm))
    (home-page "https://www.scummvm.org/")
    (synopsis "Classic 2D point and click adventure game")
    (description "Flight of the Amazon Queen is a 2D point-and-click
adventure game set in the 1940s.

You assume the role of Joe King, a pilot for hire who is given the job
of flying Faye Russell (a famous movie star) into the Amazon jungle
for a photo shoot.  Of course, things never go according to plans.
After an unfortunate turn of events they find themselves stranded in
the heart of the Amazon jungle, where Joe will embark on a quest to
rescue a kidnapped princess and in the process, discover the true
sinister intentions of a suspiciously located Lederhosen company.  In
a rich 2D environment, Joe will cross paths with a variety of unlikely
jungle inhabitants including, but not limited to, a tribe of Amazon
women and 6-foot-tall pygmies.")
    (license (license:non-copyleft "file:///readme.txt"))))

(define-public queen
  (make-queen-package
   "queen" "FOTAQ_Talkie-" "1.1" "en"
   "1a6q71q1dl9vvw2qqsxk5h1sv0gaqy6236zr5905w2is01gdsp52"))

(define-public queen-de
  (make-queen-package
   "queen-de" "FOTAQ_Ger_talkie-" "1.0" "de"
   "13vn43x7214vyprlpqabvv71k890nff3d6fjscflr1ll7acjca3f"))

(define-public queen-fr
  (make-queen-package
   "queen-fr" "FOTAQ_Fr_Talkie_" "1.0" "fr"
   "0hq5g4qrkcwm2kn5i4kv4hijs9hi7bw9xl1vrwd1l69qqn30crwy"))

(define-public queen-it
  (make-queen-package
   "queen-it" "FOTAQ_It_Talkie_" "1.0" "it"
   "1h76y70lrpzfjkm53n4nr364nhyka54vbz9r7sadzyzl7c7ilv4d"))

(define-public sky
  (package
    (name "sky")
    (version "1.2")                     ;1.3 is floppy version
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/scummvm/extras/"
                           "Beneath%20a%20Steel%20Sky/"
                           "bass-cd-" version ".zip"))
       (sha256
        (base32 "14s5jz67kavm8l15gfm5xb7pbpn8azrv460mlxzzvdpa02a9n82k"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (share (string-append out "/share"))
                (data (string-append share "/" ,name))
                (apps (string-append share "/applications"))
                (bin (string-append out "/bin"))
                (executable (string-append bin "/" ,name))
                (scummvm (assoc-ref %build-inputs "scummvm")))
           (let ((unzip (search-input-file %build-inputs "/bin/unzip")))
             (invoke unzip "-j" (assoc-ref %build-inputs "source")))
           (let ((doc (string-append share "/doc/bass-" ,version)))
             (install-file "readme.txt" doc))
           (for-each (lambda (f) (install-file f data))
                     (find-files "." "^sky\\."))
           ;; Build the executable.
           (mkdir-p bin)
           (let ((bash (assoc-ref %build-inputs "bash")))
             (with-output-to-file executable
               (lambda ()
                 (format #t "#!~a/bin/bash~%" bash)
                 (format #t "exec ~a/bin/scummvm -p ~a sky~%" scummvm data))))
           (chmod executable #o755)
           ;; Create desktop file.  There is no dedicated
           ;; icon for the game, so we borrow SCUMMVM's.
           (mkdir-p apps)
           (with-output-to-file (string-append apps "/" ,name ".desktop")
             (lambda _
               (format #t
                       "[Desktop Entry]~@
                       Name=Beneath a Steel Sky~@
                       GenericName=Bass~@
                       Exec=~a~@
                       Icon=~a/share/icons/hicolor/scalable/apps/scummvm.svg~@
                       Categories=AdventureGame;Game;RolePlaying;~@
                       Keywords=adventure;game;roleplaying;cyberpunk;~@
                       Comment=A science-fiction adventure game set in a bleak post-apocalyptic vision of the future~@
                       Comment[de]=Ein Science-Fiction-Abenteuerspiel \
angesiedelt in einer düsteren, postapokalyptischen Vision der Zukunft~@
                       Type=Application~%"
                       executable scummvm)))
           #t))))
    (native-inputs
     (list unzip))
    (inputs
     (list bash scummvm))
    (home-page "https://www.scummvm.org/")
    (synopsis "Classic 2D point and click science-fiction adventure game")
    (description
     "Beneath a Steel Sky is a science-fiction thriller set in a bleak
post-apocalyptic vision of the future.  It revolves around Union City,
where selfishness, rivalry, and corruption by its citizens seems to be
all too common, those who can afford it live underground, away from
the pollution and social problems which are plaguing the city.

You take on the role of Robert Foster, an outcast of sorts from the
city since a boy who was raised in a remote environment outside of
Union City simply termed ``the gap''.  Robert's mother took him away
from Union City as a child on their way to ``Hobart'' but the
helicopter crashed on its way.  Unfortunately, Robert's mother died,
but he survived and was left to be raised by a local tribe from the
gap.

Years later, Union City security drops by and abducts Robert, killing
his tribe in the process; upon reaching the city the helicopter taking
him there crashes with him escaping, high upon a tower block in the
middle of the city.  He sets out to discover the truth about his past,
and to seek vengeance for the killing of his tribe.")
    (license (license:non-copyleft "file:///readme.txt"))))

(define-public gnurobots
  (package
    (name "gnurobots")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/gnurobots/gnurobots-"
                           version ".tar.gz"))
       (sha256
        (base32
         "07gi3lsmbzzsjambgixj6xy79lh22km84z7bnzgwzxdy806lyvwb"))))
    (build-system gnu-build-system)
    (inputs
     (list glib gtk+-2 vte/gtk+-2 readline guile-1.8))
    (native-inputs
     (list pkg-config))
    (arguments
     `(#:make-flags
       (list
        ;; Do not abort build on "deprecated-declarations" warnings.
        "CFLAGS=-Wno-error=deprecated-declarations"
        ;; Find readline headers in sub-directory.
        (string-append "READLINE_CFLAGS=-I"
                       (assoc-ref %build-inputs "readline")
                       "/include/readline/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "doc/Robots-HOWTO"
                           (string-append (assoc-ref outputs "out")
                                          "/share/doc/gnurobots-"
                                          ,version))
             #t)))))
    (home-page "https://www.gnu.org/software/gnurobots/")
    (synopsis "Program a little robot and watch it explore a world")
    (description
     "GNU Robots is a game in which you program a robot to explore a world
full of enemies that can hurt it, obstacles and food to be eaten.  The goal of
the game is to stay alive and collect prizes.  The robot program conveniently
may be written in a plain text file in the Scheme programming language.")
    (license license:gpl3+)))

(define-public ri-li
  (package
    (name "ri-li")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/ri-li/"
                                  "Ri-li%20Linux_Unix/Ri-li%20V" version "/"
                                  "Ri-li-" version ".tar.bz2"))
              (sha256
               (base32
                "1gcdsgnnbbn1mb1hkpwniv3fhkaj1nn8gq33v5c16q3wqchcq77p"))
              ;; Taken from
              ;; <https://github.com/NixOS/nixpkgs/blob/master/pkgs/games/rili/moderinze_cpp.patch>.
              ;; It doesn't build otherwise.
              (patches (search-patches "ri-li-modernize_cpp.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Remove "gentoo" subdirectory from Makefile, as it is
         ;; missing a make file and generates a build failure.
         (add-after 'configure 'fix-build
           (lambda _
             (substitute* "Makefile"
               ((" gentoo") ""))
             #t))
         (add-after 'install 'install-desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (apps (string-append out "/share/applications"))
                    (pixmaps (string-append out "/share/pixmaps")))
               (for-each (lambda (f) (install-file f pixmaps))
                         (find-files "data" "\\.(png|ico)$"))
               (mkdir-p apps)
               (with-output-to-file (string-append apps "/ri-li.desktop")
                 (lambda _
                   (format #t
                           "[Desktop Entry]~@
                     Name=Ri-li~@
                     Exec=~a/bin/Ri_li~@
                     Icon=~a/Ri-li-icon-32x32.png~@
                     Categories=Game;ArcadeGame;~@
                     Keywords=toy;train;wooden;snake-like;engine;~@
                     Comment=a toy simulator game~@
                     Comment[de]=Ein Spiel mit einem kleinen Zug~@
                     Comment[fr]=un jeu de petit train~@
                     Comment[ro_RO]=un joc cu un tren de jucărie~@
                     Terminal=false~@
                     Type=Application~%"
                           out pixmaps))))
             #t))
         (add-after 'install-desktop-file 'remove-spurious-files
           ;; Delete redundant files already installed somewhere else.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each delete-file
                         (find-files (string-append out "/share/Ri-li")
                                     "\\.(png|ico)|COPYING"))
               #t))))))
    (inputs
     `(("sdl" ,(sdl-union (list sdl sdl-mixer)))))
    (home-page "http://www.ri-li.org")
    (synopsis "Toy train simulation game")
    (description "Ri-li is a game in which you drive a wooden toy
steam locomotive across many levels and collect all the coaches to
win.")
    ;; The project is dual-licensed GPL2+ and GPL3+.
    (license (list license:gpl2+ license:gpl3+))))

(define-public freeorion
  (package
    (name "freeorion")
    (version "0.4.10.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/freeorion/freeorion")
             (commit (string-append "v" version))))
       (sha256
        (base32 "12fhwa3cs6lvdbdhina310qk2g7zcphldsh7ibsbxn8d1m731xlk"))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; There are some bundled fonts.
           (for-each delete-file-recursively '("default/data/fonts"))))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ;no test
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unbundle-fonts
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((roboto-dir (string-append (assoc-ref inputs "font-roboto")
                                              "/share/fonts/truetype/")))
               (substitute* "UI/ClientUI.cpp"
                 (("\\(GetRootDataDir.*?Roboto-(Bold|Regular)\\.ttf\"\\)\\.string\\(\\)\\);"
                   all type)
                  (string-append "\"" roboto-dir "Roboto-" type ".ttf\");")))
               #t))))))
    (inputs
     `(("boost" ,boost)
       ("boost_signals" ,boost-signals2)
       ("font-dejavu" ,font-dejavu)
       ("font-roboto" ,font-google-roboto)
       ("freetype2" ,freetype)
       ("glew" ,glew)
       ("glu" ,glu)
       ("libogg" ,libogg)
       ("libpng" ,libpng)
       ("libvorbis" ,libvorbis)
       ("openal" ,openal)
       ("python" ,python)
       ("sdl2" ,sdl2)
       ("zlib" ,zlib)))
    (home-page "https://www.freeorion.org/index.php/Main_Page")
    (synopsis "Turn-based space empire and galactic conquest computer game")
    (description
     "FreeOrion is a turn-based space empire and galactic conquest (4X)
computer game being designed and built by the FreeOrion project.  Control an
empire with the goal of exploring the galaxy, expanding your territory,
exploiting the resources, and exterminating rival alien empires.  FreeOrion is
inspired by the tradition of the Master of Orion games, but is not a clone or
remake of that series or any other game.")
    ;; Source code is released under gpl2.  Artwork, music and sounds, and
    ;; in-game text are released under cc-by-sa3.0.  Game content scripts are
    ;; released under both gpl2 and cc-by-sa3.0.  Bundled Gigi library is
    ;; released under lgpl2.1+.
    (license (list license:gpl2 license:cc-by-sa3.0 license:lgpl2.1+))))

(define-public leela-zero
  (package
   (name "leela-zero")
   (version "0.17")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/leela-zero/leela-zero")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32
       "17px5iny8mql5c01bymcli7zfssswkzvb2i8gnsmjcck6i2n8srl"))
     (patches (search-patches "leela-zero-gtest.patch"))))
   (build-system cmake-build-system)
   (native-inputs
    (list googletest))
   (inputs
    (list boost
          opencl-icd-loader
          openblas
          opencl-headers
          qtbase-5
          zlib))
   (arguments
    '(#:configure-flags '("-DUSE_BLAS=YES")
      #:phases (modify-phases %standard-phases
                 (add-before 'configure 'fix-tests
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let ((home (getcwd)))
                       (setenv "HOME" home)
                       (substitute* "src/tests/gtests.cpp"
                         (("\\.\\./src/tests/0k\\.txt")
                          (string-append home "/src/tests/0k.txt"))
                         (("cfg_gtp_mode = true;")
                          "cfg_gtp_mode = true; cfg_cpu_only = true;")))
                     #t))
                 (replace 'check
                   (lambda _
                     (invoke "./tests"))))))
   (home-page "https://github.com/leela-zero/leela-zero")
   (synopsis "Program playing the game of Go")
   (description
    "Leela-zero is a Go engine with no human-provided knowledge, modeled after
the AlphaGo Zero paper.  The current best network weights file for the engine
can be downloaded from @url{https://zero.sjeng.org/best-network}.")
   (license license:gpl3+)))

(define-public q5go
  (package
   (name "q5go")
   (version "1.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/bernds/q5Go")
                  (commit (string-append "q5go-" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1gdlfqcqkqv7vph3qwq78d0qz6dhmdsranxq9bmixiisbzkqby31"))))
   (build-system gnu-build-system)
   (native-inputs
    (list pkg-config))
   (inputs
    (list qtbase-5 qtmultimedia qtsvg))
   (arguments
    '(#:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'fix-configure-script
          (lambda _
            ;; Bypass the unavailable qtchooser program.
            (substitute* "configure"
              (("test -z \"QTCHOOSER\"")
               "false")
              (("qtchooser -run-tool=(.*) -qt=qt5" _ command)
               command))
            #t))
        (add-after 'unpack 'fix-header
          (lambda _
            (substitute* "src/bitarray.h"
              (("#include <cstring>" all)
               (string-append all "\n#include <stdexcept>")))))
        (add-after 'unpack 'fix-paths
          (lambda _
            (substitute* '("src/pics/Makefile.in"
                           "src/translations/Makefile.in")
              (("\\$\\(datadir\\)/qGo/")
               "$(datadir)/q5go/"))
            #t))
        (add-after 'install 'install-desktop-file
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (apps (string-append out "/share/applications"))
                   (pics (string-append out "/share/q5go/pics")))
              (delete-file-recursively (string-append out "/share/applnk"))
              (delete-file-recursively (string-append out "/share/mimelnk"))
              (install-file "../source/src/pics/Bowl.ico" pics)
              (mkdir-p apps)
              (with-output-to-file (string-append apps "/q5go.desktop")
                (lambda _
                  (format #t
                          "[Desktop Entry]~@
                           Name=q5go~@
                           Exec=~a/bin/q5go~@
                           Icon=~a/Bowl.ico~@
                           Categories=Game;~@
                           Comment=Game of Go~@
                           Comment[de]=Spiel des Go~@
                           Comment[eo]=Goo~@
                           Comment[es]=Juego de Go~@
                           Comment[fr]=Jeu de Go~@
                           Comment[ja]=囲碁~@
                           Comment[ko]=바둑~@
                           Comment[zh]=围棋~@
                           Terminal=false~@
                           Type=Application~%"
                          out pics))))
             #t)))))
   (synopsis "Qt GUI to play the game of Go")
   (description
    "This a tool for Go players which performs the following functions:
@itemize
@item SGF editor,
@item Analysis frontend for Leela Zero (or compatible engines),
@item GTP interface (to play against an engine),
@item IGS client (to play on the internet),
@item Export games to a variety of formats.
@end itemize")
   (home-page "https://github.com/bernds/q5Go")
   (license license:gpl2+)))

(define-public ktuberling
  (package
    (name "ktuberling")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/ktuberling-" version ".tar.xz"))
      (sha256
       (base32 "0mlv9qllg70p26dbrcsr820c70d3ib88hapc1z6wgjhdpmc12ni1"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools perl))
    (inputs
     (list kcompletion
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kio
           kwidgetsaddons
           kxmlgui
           libkdegames
           python-wrapper
           qtbase-5
           qtmultimedia
           qtdeclarative
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Stamp drawing toy")
    (description "KTuberling is a drawing toy intended for small children and
adults who remain young at heart.  The game has no winner; the only purpose is
to make the funniest faces you can.  Several activities are possible, e.g.:

@itemize
@item Give the potato a funny face, clothes, and other goodies
@item Build a small town, complete with school, zoo, and fire department
@item Create a fantastic moonscape with spaceships and aliens
@item Top a pizza
@end itemize

KTuberling can speak the name of each the object in several languages,
to assist in learning basic vocabulary.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public picmi
  (package
    (name "picmi")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/picmi-" version ".tar.xz"))
      (sha256
       (base32 "1dfq9m4njh0czz8zws46rkz6xq2n6xra5w223m3s2f5civiw5msz"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcoreaddons
           kcrash
           kdbusaddons
           kdeclarative
           ki18n
           kio
           knewstuff
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Number logic game")
    (description "Picmi is a number logic game in which cells in a grid have
to be colored or left blank according to numbers given at the side of the
grid.  The aim is to reveal a hidden picture.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kolf
  (package
    (name "kolf")
    (version "20.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kolf-" version ".tar.xz"))
       (sha256
        (base32 "1lpp6pzr5dgd4si4a8c7hcvgxgqy0bgyhkx9m6jqb0zhll6dxj10"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kio
           kwidgetsaddons
           kxmlgui
           ktextwidgets
           libkdegames
           qtbase-5
           qtdeclarative))
    (home-page "https://games.kde.org/")
    (synopsis "Miniature golf game")
    (description "Kolf is a miniature golf game for one to ten players.  The
game is played from an overhead view, with a short bar representing the golf
club.  Kolf features many different types of objects, such as water hazards,
slopes, sand traps, and black holes (warps), among others.

Features are:
@itemize
@item Single and Multi-player (up to ten players) modes
@item High scores table
@item Dynamic courses
@item Third-party courses
@item Course editor
@end itemize

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public libkmahjongg
  (package
    (name "libkmahjongg")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/"
                          version "/src/libkmahjongg-" version ".tar.xz"))
      (sha256
       (base32 "10ljzbf7qki5flydankrbksaihhkqpfyljb8c71fbwqwmkr7rgfq"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kauth
           kcompletion
           ;("kconfig" ,kconfig)
           kcodecs
           kconfigwidgets
           kcoreaddons
           ki18n
           kwidgetsaddons
           qtbase-5
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Shared library for kmahjongg and kshisen")
    (description "Shared library and common files for kmahjongg, kshisen and
other Mah Jongg like games.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kmahjongg
  (package
    (name "kmahjongg")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/"
                          version "/src/kmahjongg-" version ".tar.xz"))
      (sha256
       (base32 "1fcj4jb2zzbaxp7cp04w36y0c7lh77yzin66fmvrcxkl11xi2wwd"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kcrash
           kdbusaddons
           kdeclarative
           ki18n
           knewstuff
           kxmlgui
           libkdegames
           libkmahjongg
           qtbase-5
           qtdeclarative
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Tile laying patience")
    (description "In KMahjongg the tiles are scrambled and staked on top of
each other to resemble a certain shape.  The player is then expected to remove
all the tiles off the game board by locating each tile's matching pair.

A variety of tile layouts are included, as well as an editor to create new
layouts.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kshisen
  (package
    (name "kshisen")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/"
                          version "/src/kshisen-" version ".tar.xz"))
      (sha256
       (base32 "1hrwr0f1kidivsp8lnwdbqz3xxagjvjwh72r3gma8smfilybygfb"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules
           ;("perl" ,perl)
           ;("pkg-config" ,pkg-config)
           kdoctools))
    (inputs
     (list kauth
           kcompletion
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kxmlgui
           libkdegames
           libkmahjongg
           qtbase-5
           qtdeclarative))
    (home-page "https://games.kde.org/")
    (synopsis "Shisen-Sho solitaire game")
    (description "KShisen is a solitaire-like game played using the standard
set of Mahjong tiles.  Unlike Mahjong however, KShisen has only one layer of
scrambled tiles

This package is part of the KDE games module.")
    (license license:gpl2+)))

(define-public kajongg
  (package
    (name "kajongg")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/"
                          version "/src/kajongg-" version ".tar.xz"))
      (sha256
       (base32 "03fdbnx7zx7vgcxvwd1h1098ks9gq162bwz35fhpyzpynr667m5r"))))
    (build-system qt-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/mjresource.py"
               (("'share', 'kmahjongglib'" all)
                (string-append "'" (assoc-ref inputs "libkmahjongg")
                               "/share', 'kmahjongglib'")))))
         (add-after 'qt-wrap 'wrap
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (program)
                           (wrap-program program
                             `("GUIX_PYTHONPATH" ":" prefix
                               (,(getenv "GUIX_PYTHONPATH")))))
                         (list (string-append out "/bin/kajongg")
                               (string-append out "/bin/kajonggserver")))))))))
    (native-inputs
     (list extra-cmake-modules
           ;("perl" ,perl)
           kdoctools))
    (inputs
     (list kconfig
           kconfigwidgets
           kcoreaddons
           ki18n
           libkmahjongg
           python
           python-twisted
           python-pyqt
           python-zope-interface
           qtbase-5
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Classical Mah Jongg game for 4 players")
    (description "Kajongg is the ancient Chinese board game for 4 players.

If you are looking for the Mah Jongg solitaire please use the application
kmahjongg.

Kajongg can be used in two different ways: Scoring a manual game where you
play as always and use Kajongg for the computation of scores and for
bookkeeping.  Or you can use Kajongg to play against any combination of other
human players or computer players.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kbreakout
  (package
    (name "kbreakout")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kbreakout-" version ".tar.xz"))
      (sha256
       (base32 "0kqj2cx0ny3qq65c6w5fpnzmrwl9irg8slzvpd3anck5cnvma3j4"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative))
    (home-page "https://games.kde.org/")
    (synopsis "Breakout like game")
    (description "KBreakout is similar to the classics breakout and xboing,
featuring a number of added graphical enhancements and effects.  You control a
paddle at the bottom of the playing-field, and must destroy bricks at the top
by bouncing balls against them.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kmines
  (package
    (name "kmines")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kmines-" version ".tar.xz"))
      (sha256
       (base32 "0hqjwh3jq2npqwkvh67fyn2xnq8swiasdw5jz8f0ikl0k28id775"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           ktextwidgets
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative))
    (home-page "https://games.kde.org/")
    (synopsis "Classical mine sweeper game")
    (description "KMines is a classic Minesweeper game.  The idea is to
uncover all the squares without blowing up any mines.  When a mine is blown
up, the game is over.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public konquest
  (package
    (name "konquest")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/"
                          version "/src/konquest-" version ".tar.xz"))
      (sha256
       (base32 "0lnwj06vv4qx05hr8pzysnvrxh8y04asajrph0rsj37v8hs9g5lh"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kcoreaddons
           kcrash
           kdbusaddons
           kguiaddons
           ki18n
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Simple turn-based strategy game")
    (description "Konquest is the KDE version of Gnu-Lactic Konquest.  Players
conquer other planets by sending ships to them.  The goal is to build an
interstellar empire and ultimately conquer all other player's planets.  The
game can be played with up to nine empires, commanded either by the computer
or by puny earthlings.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kbounce
  (package
    (name "kbounce")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/"
                          version "/src/kbounce-" version ".tar.xz"))
      (sha256
       (base32 "0ymy0z1qlw3n653xs3dsa1xm78q4xaj09dnnci4km77rzis26vb6"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kio
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Jezzball arcade game")
    (description "KBounce is a single player arcade game with the elements of
puzzle.  It is played on a field, surrounded by wall, with two or more balls
bouncing around within the walls.  The object of the game is to build new
walls to decrease the size of the active field.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kblocks
  (package
    (name "kblocks")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/"
                          version "/src/kblocks-" version ".tar.xz"))
      (sha256
       (base32 "09yfm9mzbamp294cvc5finq6ilxvxr68i0dnb0m72pa4sfzmij32"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Single player falling blocks puzzle game")
    (description "KBlocks is the classic Tetris-like falling blocks game.

The idea is to stack the falling blocks to create horizontal lines without any
gaps.  When a line is completed it is removed, and more space is available in
the play area.  When there is not enough space for blocks to fall, the game is
over.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public ksudoku
  (package
    (name "ksudoku")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/"
                          version "/src/ksudoku-" version ".tar.xz"))
      (sha256
       (base32 "0pj6ry7ak1rnpb93mqypaxrcbmrhwg9ir6zhb3ybzfkfcrh67g12"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list karchive
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kguiaddons
           ki18n
           kiconthemes
           kio
           kwidgetsaddons
           kxmlgui
           libkdegames
           glu
           qtbase-5
           qtdeclarative
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Sudoku puzzle game and solver")
    (description "KSudoku is a Sudoku game and solver, supporting a range of
2D and 3D Sudoku variants.  In addition to playing Sudoku, it can print Sudoku
puzzle sheets and find the solution to any Sudoku puzzle.

The word Sudoku means \"single number in an allotted place\" in Japanese.
These are the basic rules: Every Sudoku is a square divided into 3x3
subsquares with 3x3 cells each.

Some cells are filled with a number at the beginning.  The remaining ones are
to be filled by the player using numbers from 1 to 9, without repeating a
number twice on each column, row or subsquare (each of them must contain only
one 1, one 2, one 3, and so on).  The game requires logic and patience.
Solving takes usually 10 to 30 minutes, depending on puzzle level, your skill
and experience.

The numerals in Sudoku puzzles are used for convenience (for example in 16x16
board we use letters): arithmetic relationships between numbers are
irrelevant.

This program supports also 16x16 games with numbers from 1 to 16 and 256
cells with 16 cols, rows and subsquares!

More information at http://en.wikipedia.org/wiki/Sudoku

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public klines
  (package
    (name "klines")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/klines-" version ".tar.xz"))
      (sha256
       (base32 "0y8lnwawrkl4ixn7v4dg48k2zpr083krv7dv4d94b2dpkh7xfvih"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative))
    (home-page "https://games.kde.org/")
    (synopsis "Place 5 equal pieces together, but wait, there are 3 new ones")
    (description "KLines is a simple but highly addictive one player game.

The player has to move the colored balls around the game board, gathering them
into the lines of the same color by five.  Once the line is complete it is
removed from the board, therefore freeing precious space.  In the same time
the new balls keep arriving by three after each move, filling up the game
board.

KLines is a single-player game where the player removes colored balls from the
board by arranging them into lines of five or more.  However, every time the
player moves a ball, three more balls are added to the board.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kgoldrunner
  (package
    (name "kgoldrunner")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kgoldrunner-" version ".tar.xz"))
      (sha256
       (base32 "17ra5d3r9ajy2inj17gwd5xphzhvbzx5kpvdwyj6msx4dd9wxgfi"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kio
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative))
    (home-page "https://games.kde.org/")
    (synopsis "Action and puzzle solving game")
    (description "KGoldrunner is an action game where the hero runs through a
maze, climbs stairs, dig holes and dodges enemies in order to collect all the
gold nuggets and escape to the next level.  Your enemies are also after the
gold.  Worse still, they are after you!.

KGoldrunner is a fast-paced platform game where the player must navigate a
maze while collecting gold nuggets and avoiding enemies.  A variety of level
packs are included, as well as an editor to create new levels.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kdiamond
  (package
    (name "kdiamond")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kdiamond-" version ".tar.xz"))
      (sha256
       (base32 "1iyxrx3422asa58kh0siwvi1svds5kccrym6gdfpdhlmhmciqlzi"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           knotifications
           knotifyconfig
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative))
    (home-page "https://games.kde.org/")
    (synopsis "Three-in-a-row game")
    (description "KDiamond is a three-in-a-row game like Bejeweled.  It
features unlimited fun with randomly generated games and five difficulty
levels with varying number of diamond colors and board sizes.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kfourinline
  (package
    (name "kfourinline")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kfourinline-" version ".tar.xz"))
      (sha256
       (base32 "0plx3lv35fc8q9svbyl71mms3ji6zn58j306bvm1f8kkgg0x395b"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdnssd
           ki18n
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Place 4 pieces in a row")
    (description "KFourInLine is a board game for two players based on the
Connect-Four game.

KFourInLine is a game where two players take turns dropping pieces into a
grid, the winner being the first to place four pieces in a line.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kblackbox
  (package
    (name "kblackbox")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kblackbox-" version ".tar.xz"))
      (sha256
       (base32 "0la5w44b0gl72g3wfp0pw8gwnm287lh7nd9k5ikpszw5nn49db0h"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list karchive
           kcompletion
           kconfig
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           ktextwidgets
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Find atoms in a grid by shooting electrons")
    (description "KBlackbox is a game of hide and seek played on a grid of
boxes where the computer has hidden several balls.  The position of the hidden
balls can be deduced by shooting beams into the box

KBlackBox is a game of hide and seek played on an grid of boxes, where the
player shoots rays into the grid to deduce the positions of hidden objects.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public knetwalk
  (package
    (name "knetwalk")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/knetwalk-" version ".tar.xz"))
      (sha256
       (base32 "060kj06vpigdy570izsjfgnmqqrpmb8bkr9arqc109hg3avl5wjz"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           ktextwidgets
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative))
    (home-page "https://games.kde.org/")
    (synopsis "Turn the board pieces to get all computers connected")
    (description "KNetWalk is a small game where you have to build up a
computer network by rotating the wires to connect the terminals to the server.
When the network is build, a highscore-list comes up where competitions can be
fought out.

KNetwalk is a puzzle game where the player arranges sections of wire to
connect all the computers on the board.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public bomber
  (package
    (name "bomber")
    (version "20.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/bomber-" version ".tar.xz"))
       (sha256
        (base32 "1fjcwm591jgx3bgqpi0j5fnb2l2r2h3r6lav3vhaxz4rkf56pg2a"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative))
    (home-page "https://games.kde.org/")
    (synopsis "Arcade bombing game")
    (description "Bomber is a single player arcade game.

The player is invading various cities in a plane that is decreasing in height.
The goal of the game is to destroy all the buildings and advance to the next
level.  Each level gets a bit harder by increasing the speed of the plane and
the height of the buildings.

Bomber is a game where you fly a spaceship and attempt to bomb the buildings
below you.  Each pass the spaceship makes, it gets lower and lower.  If you've
not destroyed a building in your path, you will crash into it.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public granatier
  (package
    (name "granatier")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/granatier-" version ".tar.xz"))
      (sha256
       (base32 "1fyh7zyacb3pnlfd29jw2jmyl8a7sjw354pi234nd5x5999xw5z6"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           knewstuff
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Bomberman clone")
    (description "Granatier is a clone of the classic Bomberman game,
inspired by the work of the Clanbomber clone.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public ksirk
  (package
    (name "ksirk")
    (version "20.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/ksirk-" version ".tar.xz"))
       (sha256
        (base32 "10y7nm0x6zcc0gh3am69bbxyyb8azbbfyrdqsa023ggr7n04cn21"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcrash
           ki18n
           kiconthemes
           kio
           knewstuff
           kwallet
           kxmlgui
           libkdegames
           phonon
           qca
           qtbase-5
           qtdeclarative
           qtsvg
           zlib))
    (home-page "https://games.kde.org/")
    (synopsis "Computerized version of the well known strategy board game
'Risk'")
    (description "KsirK is a multi-player network-enabled game.  The goal of
the game is simply to conquer the world by attacking your neighbors with your
armies.

At the beginning of the game, countries are distributed to all the players.
Each country contains one army represented by an infantryman.  Each player has
some armies to distribute to his countries.  On each turn, each player can
attack his neighbours, eventually conquering one or more countries.  At the
end of each turn, some bonus armies are distributed to the players in function
of the number of countries they own.  The winner is the player that conquered
all the world.

Features:
@itemize
@item Support for 1-6 human or computer players
@item Multi-player gaming over a network
@item You can easily create new skins with SVG graphics and the skin editor
@item Hot New Stuff support.  You can easily download and install new skins
@end itemize

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public palapeli
  (package
    (name "palapeli")
    (version "20.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/palapeli-" version ".tar.xz"))
       (sha256
        (base32 "0xxz9g4zxljlg20g88a5lkbwzzm9yg4vxnrfigk8m018cz0nqd5b"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list karchive
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kcrash
           ki18n
           ki18n
           kio
           kitemviews
           knotifications
           kservice
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative
           qtsvg
           shared-mime-info))
    (home-page "https://games.kde.org/")
    (synopsis "Jigsaw puzzle game")
    (description "Palapeli is a jigsaw puzzle game.  Unlike other games in
that genre, you are not limited to aligning pieces on imaginary grids.  The
pieces are freely moveable.  Also, Palapeli features real persistency, i.e.
everything you do is saved on your disk immediately.

Palapeli is the Finnish word for jigsaw puzzle.

This package is part of the KDE games module.")
    (license license:gpl2+)))

(define-public kiriki
  (package
    (name "kiriki")
    (version "20.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kiriki-" version ".tar.xz"))
       (sha256
        (base32 "0milc8fl1rj4yrwdvm60ampd47dyiys1xvqi5f0g7y6mgymgyk4x"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kiconthemes
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative))
    (home-page "https://games.kde.org/")
    (synopsis "Yahtzee dice game")
    (description "Kiriki is an addictive and fun dice game, designed to be
played by as many as six players.

Participants have to collect points by rolling five dice for up to three times
per single turn to make combinations with the highest score.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kigo
  (package
    (name "kigo")
    (version "20.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kigo-" version ".tar.xz"))
       (sha256
        (base32 "088752yzmfsnppd27p8hld4x5s7sw5fagm08024l5ra1mlicdfz9"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kio
           knewstuff
           ktextwidgets
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Go board game")
    (description "Kigo is an open-source implementation of the popular Go
game.

Go is a strategic board game for two players.  It is also known as
igo (Japanese), weiqi or wei ch'i (Chinese) or baduk (Korean).  Go is noted
for being rich in strategic complexity despite its simple rules.  The game is
played by two players who alternately place black and white stones (playing
pieces, now usually made of glass or plastic) on the vacant intersections of a
grid of 19x19 lines (9x9 or 13x13 for easier games).

You also need to install a go engine, e.g. @code{gnugo}.

This package is part of the KDE games module.")
    (license license:gpl3+)))

(define-public kubrick
  (package
    (name "kubrick")
    (version "20.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kubrick-" version ".tar.xz"))
       (sha256
        (base32 "0h3mypwd67sss08j5vvrih5f5ss85m9kax6412y40xmsm51lz2pq"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list glu
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           ki18n
           kio
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Game based on Rubik's Cube")
    (description "Kubrick is a game based on the Rubik's Cube puzzle.

The cube sizes range from 2x2x2 up to 6x6x6, or you can play with irregular
\"bricks\" such as 5x3x2 or \"mats\" such as 6x4x1 or 2x2x1.  The game has a
selection of puzzles at several levels of difficulty, as well as demos of
pretty patterns and solution moves, or you can make up your own puzzles.  The
game has unlimited undo, redo, save and reload capabilities.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public lskat
  (package
    (name "lskat")
    (version "20.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/lskat-" version ".tar.xz"))
       (sha256
        (base32 "1wg9zxp64kwjxqs4qw0h7j8yhgffbmvh8j9d4dgmz45dscngnjli"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kcoreaddons
           kcrash
           kguiaddons
           ki18n
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Lieutnant Skat card game")
    (description "Lieutnant Skat (from German \"Offiziersskat\") is a fun and
engaging card game for two players, where the second player is either live
opponent, or a built in artificial intelligence.

Lieutnant Skat is a simplified variant of the Skat card game for two players.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kapman
  (package
    (name "kapman")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kapman-" version ".tar.xz"))
      (sha256
       (base32 "14x3v6li4r3gzzwfd6ar9saq2rhc7yxs0sp9ygalzq8vq4d7i1kh"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Pac-Man clone")
    (description "Kapman is a clone of the well known game Pac-Man.

You must run through the maze to eat all pills without being captured by a
ghost.  By eating an energizer, Kapman gets the ability to eat ghosts for a
few seconds.  When a stage is cleared of pills and energizer the player is
taken to the next stage with slightly increased game speed

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kspaceduel
  (package
    (name "kspaceduel")
    (version "20.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kspaceduel-" version ".tar.xz"))
       (sha256
        (base32 "1aixh6ygif2cm1a5g32sl5y6b5x68139pzihaxq4334c6avamdai"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Two player game with shooting spaceships flying around a sun")
    (description "KSpaceduel is a space battle game for one or two players,
where two ships fly around a star in a struggle to be the only survivor.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public bovo
  (package
    (name "bovo")
    (version "20.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/bovo-" version ".tar.xz"))
       (sha256
        (base32 "18qbac366m0xma3ary11q9zxz0wgnysppcl7kpypl6ic3nf61wqz"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Classic pen and paper game: five in a line")
    (description "Bovo is a Gomoku (from Japanese 五目並べ - lit.  \"five
points\") like game for two players, where the opponents alternate in placing
their respective pictogram on the game board.  The winner is the first to
complete a line of five markers.  (Also known as: Connect Five, Five in a row,
X and O, Naughts and Crosses)

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public killbots
  (package
    (name "killbots")
    (version "20.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/killbots-" version ".tar.xz"))
       (sha256
        (base32 "1296gww42nwnai7y6m2qpjqpyc30p7z9chfv5rv0n48jvdhva88y"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative))
    (home-page "https://games.kde.org/")
    (synopsis "Port of the classic BSD console game robots")
    (description "Killbots is a simple game of evading killer robots.

Who created the robots and why they have been programmed to destroy, no one
knows.  All that is known is that the robots are numerous and their sole
objective is to destroy you.  Fortunately for you, their creator has focused
on quantity rather than quality and as a result the robots are severely
lacking in intelligence.  Your superior wit and a fancy teleportation device
are your only weapons against the never-ending stream of mindless automatons.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public ksnakeduel
  (package
    (name "ksnakeduel")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/ksnakeduel-" version ".tar.xz"))
      (sha256
       (base32 "0mp6g258n3xzvgf23jnhkw10xgwqwqdzqfdc6r9jq6a6m8v77swz"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kguiaddons
           ki18n
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Snake race played against the computer")
    (description "KSnakeDuel is a fast action game where you steer a snake
which has to eat food.  While eating the snake grows.  But once a player
collides with the other snake or the wall the game is lost.  This becomes of
course more and more difficult the longer the snakes grow.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kollision
  (package
    (name "kollision")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kollision-" version ".tar.xz"))
      (sha256
       (base32 "180ybafizpwjsg80npy0l9142cjsnlyxwv9dz3bq6r8v4smn2g6b"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative))
    (home-page "https://games.kde.org/")
    (synopsis "Simple ball dodging game")
    (description "In Kollision you use mouse to control a small blue ball in a
closed space environment filled with small red balls, which move about
chaotically.  Your goal is to avoid touching any of those red balls with your
blue one, because the moment you do the game will be over.  The longer you can
stay in game the higher will your score be.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public knavalbattle
  (package
    (name "knavalbattle")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/knavalbattle-" version ".tar.xz"))
      (sha256
       (base32 "03rqf4avn61b0v340ymmzgp7s0axygjgxq1nlp5aaqbx70zcb4lq"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kauth
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kdnssd
           ki18n
           ktextwidgets
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative))
    (home-page "https://games.kde.org/")
    (synopsis "Battleship board game with built-in game server")
    (description "KBattleship is a Battle Ship game for KDE.

Ships are placed on a board which represents the sea.  Players try to hit each
others ships in turns without knowing where they are placed.  The first player
to destroy all ships wins the game.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public kreversi
  (package
    (name "kreversi")
    (version "20.08.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kreversi-" version ".tar.xz"))
      (sha256
       (base32 "0d3y072q61xcik9lf0pz0c9njvarwlvf6hqv5fp5jyqaf2902pmi"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kdeclarative
           ki18n
           kiconthemes
           kio
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Old reversi board game, also known as othello")
    (description "KReversi is a simple one player strategy game played
against the computer.

If a player's piece is captured by an opposing player, that piece is turned
over to reveal the color of that player.  A winner is declared when one player
has more pieces of his own color on the board and there are no more possible
moves.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public ksquares
  (package
    (name "ksquares")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/ksquares-" version ".tar.xz"))
      (sha256
       (base32 "0chd30byl2kww1k699vkygrxq2wdyvi84m2bimk23q96fl8h831y"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative))
    (home-page "https://games.kde.org/")
    (synopsis "Dots and Boxes game")
    (description "KSquares is an implementation of the popular paper based
game Squares.  Two players take turns connecting dots on a grid to complete
squares, the player with the most squares wins.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kjumpingcube
  (package
    (name "kjumpingcube")
    (version "20.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/release-service/" version
                          "/src/kjumpingcube-" version ".tar.xz"))
      (sha256
       (base32 "1mk73il4jh15z5pm3fp65hsyvmrga11c3h7w96yamy2n2bbniapq"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kio
           kwidgetsaddons
           kxmlgui
           libkdegames
           qtbase-5
           qtdeclarative
           qtsvg))
    (home-page "https://games.kde.org/")
    (synopsis "Simple tactical game for number-crunchers")
    (description "KJumpingcube is a simple tactical game for one or two
players, played on a grid of numbered squares.  Each turn, players compete for
control of the board by capturing or adding to one square.

This package is part of the KDE games module.")
    (license (list license:gpl2+ license:fdl1.2+))))

(define-public xmoto
  (package
    (name "xmoto")
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xmoto/xmoto")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00f5ha79lfa2iiaz66wl0hl5dapa1l15qdr7m7knzi0ll7j6z66n"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-1)))
       ;; XXX: Remove some bundled libraries.  Guix provides Chipmunk, but
       ;; it appears to be incompatible with the (older) one bundled.
       (snippet
        `(begin
           (let ((keep '("chipmunk" "glad" "md5sum")))
             (with-directory-excursion "vendor"
               (for-each delete-file-recursively
                         (lset-difference string=?
                                          (scandir ".")
                                          (cons* "." ".." keep))))
             (substitute* "src/CMakeLists.txt"
               (("add_subdirectory\\(.*?/vendor/(.+?)\".*" line library)
                (if (member library keep) line ""))))
           #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-hard-coded-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/common/VFileIO.cpp"
               (("/usr/share")
                (string-append (assoc-ref outputs "out") "/share")))
             #t))
         (add-before 'build 'set-SDL
           ;; Set correct environment for SDL.
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append
                      (assoc-ref inputs "sdl") "/include/SDL:"
                      (or (getenv "CPATH") "")))
             #t))
         (add-after 'install 'unbundle-fonts
           ;; Unbundle DejaVuSans TTF files.
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((font-dir (string-append (assoc-ref inputs "font-dejavu")
                                            "/share/fonts/truetype/"))
                   (target-dir (string-append (assoc-ref outputs "out")
                                              "/share/xmoto/Textures/Fonts/")))
               (for-each (lambda (f)
                           (let ((font (string-append font-dir f))
                                 (target (string-append target-dir f)))
                             (delete-file target)
                             (symlink font target)))
                         '("DejaVuSans.ttf" "DejaVuSansMono.ttf"))
               #t))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("bzip2" ,bzip2)
       ("curl" ,curl)
       ("font-dejavu" ,font-dejavu)
       ("glu" ,glu)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libxdg-basedir" ,libxdg-basedir)
       ("libxml2" ,libxml2)
       ("lua" ,lua-5.1)
       ("ode" ,ode)
       ("sdl" ,(sdl-union (list sdl sdl-mixer sdl-net sdl-ttf)))
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (home-page "https://xmoto.tuxfamily.org/")
    (synopsis "2D motocross platform game")
    (description
     "X-Moto is a challenging 2D motocross platform game, where
physics play an all important role in the gameplay.  You need to
control your bike to its limit, if you want to have a chance finishing
the more difficult challenges.")
    (license (list license:gpl2+        ;whole project
                   license:bsd-3        ;vendor/md5sum
                   license:lgpl2.1+
                   license:expat))))

(define-public eboard
  (package
    (name "eboard")
    (version "1.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fbergo/eboard")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1z4pwpqyvxhlda99h6arh2rjk90fbms9q29fqizjblrdn01dlxn1"))))
    (build-system gnu-build-system)
    (native-inputs
     (list perl pkg-config))
    (inputs
     (list gtk+-2 libpng gstreamer))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (make-file-writable "eboard-config")
             (setenv "CC" "gcc")
             (invoke "./configure"
                     (string-append "--prefix=" (assoc-ref outputs "out")))
             #t))
         (add-before 'install 'make-required-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out")
                                     "/share/eboard"))
             #t)))))
    (synopsis "Graphical user interface to play chess")
    (description
     "Eboard is a chess board interface for ICS (Internet Chess Servers)
and chess engines.")
    (home-page "https://www.bergo.eng.br/eboard/")
    (license license:gpl2+)))

(define-public chessx
  (package
    (name "chessx")
    (version "1.5.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/chessx/chessx/"
                           version "/chessx-" version ".tgz"))
       (sha256
        (base32 "01fjchil2h6ry2ywr0dwjw2g7zd29580cr4c74d5z74h999lp6nh"))))
    (build-system qt-build-system)
    (native-inputs
     (list qttools))
    (inputs
     (list qtbase-5 qtmultimedia qtspeech qtsvg zlib))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "chessx.pro"
               (("\\$\\$\\[QT_INSTALL_BINS\\]/lrelease")
                (search-input-file inputs "/bin/lrelease")))))
         (add-after 'fix-paths 'make-qt-deterministic
           (lambda _
             (setenv "QT_RCC_SOURCE_DATE_OVERRIDE" "1")
             #t))
         (add-after 'make-qt-deterministic 'disable-versioncheck
           (lambda _
             (substitute* "src/database/settings.cpp"
               (("\"/General/onlineVersionCheck\", true")
                "\"/General/onlineVersionCheck\", false"))
             #t))
         (replace 'configure
           (lambda _
             (invoke "qmake")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "release/chessx" (string-append out "/bin"))
               (install-file "unix/chessx.desktop"
                             (string-append out "/share/applications")))
             #t)))))
    (synopsis "Chess game database")
    (description
     "ChessX is a chess database.  With ChessX you can operate on your
collection of chess games in many ways: browse, edit, add, organize, analyze,
etc.  You can also play games on FICS or against an engine.")
    (home-page "http://chessx.sourceforge.net/")
    (license license:gpl2+)))

(define-public stockfish
  (let ((neural-network-revision "13406b1dcbe0")) ; also update hash below
    (package
      (name "stockfish")
      (version "14.1")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/official-stockfish/Stockfish")
               (commit (string-append "sf_" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0apqqcgpcflm3c6mcl13ln2y04f6zksnljmk4ys7naf7xk4vdgkd"))))
      (build-system gnu-build-system)
      (inputs
       `(("neural-network"
          ,(origin
             (method url-fetch)
             (uri (string-append "https://tests.stockfishchess.org/api/nn/nn-"
                                 neural-network-revision ".nnue"))
             (sha256
              (base32
               "0vr3hcmlqqm74pn7hc54gmfs9drqvgc53nh7bvy6v8z0rcfnnh0k"))))))
      (arguments
       `(#:tests? #f
         #:make-flags (list "-C" "src"
                            "build"
                            (string-append "PREFIX="
                                           (assoc-ref %outputs "out"))
                            (string-append "ARCH="
                                           ,(match (%current-system)
                                              ("x86_64-linux" "x86-64")
                                              ("i686-linux" "x86-32")
                                              ("aarch64-linux" "armv8")
                                              ("armhf-linux" "armv7")
                                              ("mips64el-linux" "general-64")
                                              (_ "general-32"))))
         #:phases (modify-phases %standard-phases
                    (delete 'configure)
                    ;; The official neural network file is needed for building
                    ;; and is embedded in the resulting binary.
                    (add-after 'unpack 'copy-net
                      (lambda* (#:key inputs #:allow-other-keys)
                        (copy-file (assoc-ref inputs "neural-network")
                                   (format #f "src/nn-~a.nnue"
                                           ,neural-network-revision)))))))
      (synopsis "Strong chess engine")
      (description
       "Stockfish is a very strong chess engine.  It is much stronger than the
best human chess grandmasters.  It can be used with UCI-compatible GUIs like
ChessX.")
      (home-page "https://stockfishchess.org/")
      (license license:gpl3+))))

(define-public barrage
  (package
    (name "barrage")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/lgames/barrage/"
                           "barrage-" version ".tar.gz"))
       (sha256
        (base32 "0139wxyrir10cbkvkjn548xgmp84wax8mfwk80yxbxlcdamrg257"))))
    (build-system gnu-build-system)
    (inputs
     (list hicolor-icon-theme sdl sdl-mixer))
    (arguments
     `(#:configure-flags
       (list
        (string-append "CFLAGS="
                       "-I" (assoc-ref %build-inputs "sdl-mixer")
                       "/include/SDL"))))
    (home-page "http://lgames.sourceforge.net/Barrage/")
    (synopsis "Violent point-and-click shooting game with nice effects")
    (description
     "Barrage is a rather destructive action game that puts you on a shooting
range with the objective to hit as many dummy targets as possible within
3 minutes.  You control a gun that may either fire small or large grenades at
soldiers, jeeps and tanks.  The gameplay is simple but it is not that easy to
get high scores.")
    (license license:gpl2+)))

(define-public burgerspace
  (package
    (name "burgerspace")
    (version "1.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://perso.b2b2c.ca/~sarrazip/dev/"
                           "burgerspace-" version ".tar.gz"))
       (sha256
        (base32 "1xb4immzmd419aa08lgkzf7ibxa6ax238zb2l5iw9nkgvzlh1v6l"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list flatzebra))
    (home-page "http://perso.b2b2c.ca/~sarrazip/dev/burgerspace.html")
    (synopsis "Avoid evil foodstuffs and make burgers")
    (description
     "This is a clone of the classic game BurgerTime.  In it, you play
the part of a chef who must create burgers by stepping repeatedly on
the ingredients until they fall into place.  And to make things more
complicated, you also must avoid evil animate food items while
performing this task, with nothing but your trusty pepper shaker to
protect you.")
    (license license:gpl2+)))

(define-public 7kaa
  (package
    (name "7kaa")
    (version "2.15.4p1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/the3dfxdude/7kaa/"
                           "releases/download/v" version "/"
                           "7kaa-" version ".tar.xz"))
       (sha256
        (base32 "1y7v0jhp3apb619p7asikqr1dnwb2yxbh40wbx1ppmr5f03mq9ph"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gettext-minimal pkg-config))
    (inputs
     (list curl enet openal sdl2))
    (home-page "https://7kfans.com/")
    (synopsis "Seven Kingdoms Ancient Adversaries: real-time strategy game")
    (description
     "Seven Kingdoms, designed by Trevor Chan, brings a blend of Real-Time
Strategy with the addition of trade, diplomacy, and espionage.  The game
enables players to compete against up to six other kingdoms allowing players
to conquer opponents by defeating them in war (with troops or machines),
capturing their buildings with spies, or offering opponents money for their
kingdom.")
    (license license:gpl2+)))

(define-public neverball
  ;; Git version is 6-years younger than latest release.
  (let ((commit "760a25d32a5fb0661b99426d4ddcb9ac9f3d1644")
        (revision "1"))
    (package
      (name "neverball")
      (version (git-version "1.6.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Neverball/neverball")
               (commit commit)))
         (sha256
          (base32
           "0bwh67df3lyf33bv710y25l3frjdd34j9b7gsjadwxviz6r1vpj5"))
         (file-name (git-file-name name version))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; Octocat seems to be non-free.  Oddly, Debian doesn't strip it.
             (delete-file-recursively "data/ball/octocat")
             #t))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         '(("neverball" "bin/")
           ("neverputt" "bin/")
           ("mapc" "bin/")
           ("data" "share/games/neverball/")
           ("locale" "share/")
           ("dist/" "share/games/neverball" #:include ("neverball_replay.png"
                                                       "neverlogos.svg"
                                                       "svg readme.txt"))
           ("dist/" "share/applications" #:include ("neverball.desktop"
                                                    "neverputt.desktop"))
           ("dist/neverball_16.png"
            "/share/icons/hicolor/16x16/apps/neverball.png")
           ("dist/neverball_24.png"
            "/share/icons/hicolor/24x24/apps/neverball.png")
           ("dist/neverball_32.png"
            "/share/icons/hicolor/32x32/apps/neverball.png")
           ("dist/neverball_48.png"
            "/share/icons/hicolor/48x48/apps/neverball.png")
           ("dist/neverball_64.png"
            "/share/icons/hicolor/64x64/apps/neverball.png")
           ("dist/neverball_128.png"
            "/share/icons/hicolor/128x128/apps/neverball.png")
           ("dist/neverball_256.png"
            "/share/icons/hicolor/256x256/apps/neverball.png")
           ("dist/neverball_512.png"
            "/share/icons/hicolor/512x512/apps/neverball.png")
           ("dist/neverputt_16.png"
            "/share/icons/hicolor/16x16/apps/neverputt.png")
           ("dist/neverputt_24.png"
            "/share/icons/hicolor/24x24/apps/neverputt.png")
           ("dist/neverputt_32.png"
            "/share/icons/hicolor/32x32/apps/neverputt.png")
           ("dist/neverputt_48.png"
            "/share/icons/hicolor/48x48/apps/neverputt.png")
           ("dist/neverputt_64.png"
            "/share/icons/hicolor/64x64/apps/neverputt.png")
           ("dist/neverputt_128.png"
            "/share/icons/hicolor/128x128/apps/neverputt.png")
           ("dist/neverputt_256.png"
            "/share/icons/hicolor/256x256/apps/neverputt.png")
           ("dist/neverputt_512.png"
            "/share/icons/hicolor/512x512/apps/neverputt.png")
           ("dist/" "share/man/man1" #:include ("mapc.1"))
           ("dist/" "share/man/man6" #:include ("neverball.6" "neverputt.6"))
           ("doc/" "share/doc/neverball")
           ("README.md" "share/doc/neverball/"))
         #:phases
         (modify-phases %standard-phases
           (add-before 'install 'build
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (sdl (assoc-ref inputs "sdl")))
                 (invoke "make" "-j" (number->string (parallel-job-count))
                         "--environment-overrides"
                         "CC=gcc" "BUILD=release"
                         (string-append "DATADIR="
                                        out
                                        "/share/games/neverball/data")
                         (string-append "LOCALEDIR=" out "/share/locale")
                         (string-append "SDL_CPPFLAGS=-I"
                                        sdl
                                        "/include/SDL2/")))
               #t))
           (add-after 'install 'fix-some-broken-fonts
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out")))
                 (wrap-program (string-append out "/bin/neverball")
                   `("LANG" = ("en_US.utf8")))
                 (wrap-program (string-append out "/bin/neverputt")
                   `("LANG" = ("en_US.utf8"))))
               #t)))))
      (native-inputs
       `(("gettext" ,gettext-minimal))) ;for msgfmt
      (inputs
       `(("libjpeg" ,libjpeg-turbo)
         ("libpng" ,libpng)
         ("libvorbis" ,libvorbis)
         ("physfs" ,physfs)
         ("sdl" ,(sdl-union (list sdl2 sdl2-ttf)))))
      (home-page "https://neverball.org/")
      (synopsis "3D floor-tilting game")
      (description
       "In the grand tradition of Marble Madness and Super Monkey Ball,
Neverball has you guide a rolling ball through dangerous territory.  Balance
on narrow bridges, navigate mazes, ride moving platforms, and dodge pushers
and shovers to get to the goal.  Race against the clock to collect coins to
earn extra balls.  Also included is Neverputt, which is a 3D miniature golf
game.")  ;thanks to Debian for description
      (license license:gpl2+))))

(define-public pokerth
  (package
    (name "pokerth")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/pokerth/pokerth/"
                           version "/pokerth-" version ".tar.gz"))
       (sha256
        (base32 "0yi9bj3k8yc1gkwmaf14zbbvvn13n54n1dli8k6j1pkph3p3vjq2"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove bundled websocketpp.
           (delete-file-recursively "src/third_party/websocketpp")
           (substitute* "pokerth_lib.pro"
             (("src/third_party/websocketpp")
              ""))
           #t))))
    (build-system qt-build-system)
    (inputs
     `(("boost" ,boost)
       ("curl" ,curl)
       ("gsasl" ,gsasl)
       ("libgcrypt" ,libgcrypt)
       ("libircclient" ,libircclient)
       ("protobuf" ,protobuf-2)         ; remove package when no longer needed
       ("qtbase" ,qtbase-5)
       ("sdl" ,(sdl-union (list sdl sdl-mixer)))
       ("sqlite" ,sqlite)
       ("tinyxml" ,tinyxml)
       ("websocketpp" ,websocketpp)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f ; No test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (find-files "." "\\.pro$")
               (("/opt/gsasl")
                (assoc-ref inputs "gsasl"))
               (("\\$\\$\\{PREFIX\\}/include/libircclient")
                (search-input-directory inputs "/include/libircclient"))
               (("LIB_DIRS =")
                (string-append "LIB_DIRS = "
                               (assoc-ref inputs "boost") "/lib")))
             #t))
         (add-after 'unpack 'fix-build
           (lambda _
             ;; Fixes for Boost versions >= 1.66.
             (substitute* '("src/net/common/clientthread.cpp"
                            "src/net/serveraccepthelper.h")
               (("boost::asio::socket_base::non_blocking_io command\\(true\\);")
                "")
               (("newSock->io_control\\(command\\);")
                "newSock->non_blocking(true);")
               (("acceptedSocket->io_control\\(command\\);")
                "acceptedSocket->non_blocking(true);"))
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "qmake" "pokerth.pro" "CONFIG+=client"
                     (string-append "PREFIX=" (assoc-ref outputs "out"))))))))
    (home-page "https://www.pokerth.net")
    (synopsis "Texas holdem poker game")
    (description
     "With PokerTH you can play the Texas holdem poker game, either against
computer opponents or against real players online.")
    (license license:agpl3+)))

(define-public xblackjack
  (package
    (name "xblackjack")
    (version "2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.ibiblio.org/pub/X11/contrib/games/"
                           "xblackjack-" version ".tar.gz"))
       (sha256
        (base32 "05h93rya7zwnx2l58f0a7wkjadymkj4y77clcr2hryhrhhy1vwjx"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((imake (assoc-ref inputs "imake"))
                   (out (assoc-ref outputs "out")))
               (substitute* "Imakefile"
                 (("EXTRA_LIBRARIES = -lXm \\$\\(DEPLIBS\\) -lbsd")
                  "EXTRA_LIBRARIES = -lXm -lXt -lXmu -lXext -lX11")
                 (("^#define NonStandardInstallTargets NO")
                  "#define NonStandardInstallTargets YES")
                 (("BINDIR = /usr/local/bin")
                  (string-append "BINDIR = " out "/bin"))
                 (("MANDIR = /usr/local/man/cat1")
                  (string-append "MANDIR = " out "/share/man/man1"))
                 (("XAPPLOADDIR = /usr/local/lib/app-defaults")
                  (string-append "XAPPLOADDIR = " out "/lib/X11/app-defaults")))

               (invoke "xmkmf")  ; Generate Makefile.
               (substitute* "Makefile"
                 ((imake) out)
                 (("ETCX11DIR = /etc/X11")
                  (string-append "ETCX11DIR = " out "/etc/X11"))
                 ;; Fix incorrect argument given to gcc. Error message:
                 ;; "gcc: error: DefaultGcc2AMD64Opt: No such file or directory"
                 (("CDEBUGFLAGS = [^\n]*") ""))

               ;; Fix header paths.
               (substitute* '("Draw.c"
                              "Strategy.c")
                 (("^#include <X11/Xm/Xm.h>")
                  "#include <Xm/Xm.h>"))
               (substitute* "Strategy.c"
                 (("^#include <X11/Xm/Label.h>")
                  "#include <Xm/Label.h>"))

               ;; Fix compilation errors.
               (substitute* "Table.c"
                 (("/\\* focus_moved_proc \\*/\tXtInheritFocusMovedProc,") "")
                 (("_XmMoveObject\\(\\(RectObj\\) w, rx, ry\\);")
                  "_XmMoveObject(w, rx, ry);")
                 (("_XmResizeObject\\(\\(RectObj\\) managed->locs[i].w, nw, nh,")
                  "_XmResizeObject(managed->locs[i].w, nw, nh,")))))
         (add-after 'install 'install-man-pages
           (lambda _
             (invoke "make" "install.man"))))
       #:tests? #f))  ; No check target.
    (inputs
     (list lesstif libx11 libxext libxmu libxt))
    (native-inputs
     (list imake))
    (home-page "https://www.ibiblio.org/pub/X11/contrib/games/")
    (synopsis "X11/Motif blackjack game")
    (description
     "Xblackjack is a MOTIF/OLIT based tool constructed to get you ready for
the casino.  It was inspired by a book called \"Beat the Dealer\" by Edward
O. Thorp, Ph.D. of UCLA.  A number of important statistics are maintained
for display, and used by the program to implement Thorp's \"Complete Point
System\" (high-low system).")
    (license (license:x11-style "" "See file headers."))))

(define-public azimuth
  (package
    (name "azimuth")
    ;; Not marked as latest release, but it fixes a compiling issue
    ;; and adds the install target.
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mdsteele/azimuth")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1znfvpmqiixd977jv748glk5zc4cmhw5813zp81waj07r9b0828r"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         ;; Build release version instead of debug version.
         (add-after 'unpack 'set-release
           (lambda _
             (setenv "BUILDTYPE" "release") #t))
         (add-after 'unpack 'fix-install ; set install directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile" (("/usr") (assoc-ref outputs "out"))) #t)))))
    (inputs (list sdl))
    (home-page "https://mdsteele.games/azimuth/")
    (synopsis "Metroidvania game with vector graphics")
    (description
     "Pilot your ship inside a planet to find and rescue the colonists trapped
inside the Zenith Colony.")
    (license license:gpl3+)))

(define-public cgoban
  (package
    (name "cgoban")
    (version "1.9.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/cgoban1/cgoban1/"
                           version "/cgoban-" version ".tar.gz"))
       (sha256
        (base32 "0qlvkiaglqq0izfph3l04mp4rqqqm9ks6rcsrmzrggw9x706z2iv"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11 libxt xorgproto))
    (home-page "http://cgoban1.sourceforge.net/")
    (synopsis "Go client for X11")
    (description "Provides a large set of Go-related services for X11:
@itemize
@item Local games with precise implementation of the Chinese and Japanese rulesets
@item Edition and visualization of SGF files-Connection to the NNGS or IGS Go servers
@item Bridge to Go modem protocol, allowing to play against Go modem-capable AIs
such as GnuGo.
@end itemize")
    (license license:gpl2+)))

(define-public passage
  (package
    (name "passage")
    (version "4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/hcsoftware/Passage/v"
                           version "/Passage_v" version "_UnixSource.tar.gz"))
       (sha256
        (base32 "02ky4a4xdjvr71r58339jjrjyz76b5skcnbq4f8707mrln9vhby3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #false                  ; there are none
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "gamma256/gameSource")
             (system "cat Makefile.GnuLinux Makefile.all > Makefile")))
         (replace 'configure
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append
                      (assoc-ref inputs "sdl") "/include/SDL:"
                      (or (getenv "CPATH") "")))
             (let* ((out (assoc-ref outputs "out"))
                    (assets (string-append out "/share/passage")))
               (substitute* "common.cpp"
                 (("readTGA\\( \"graphics\"")
                  (format #false "readTGA(\"~a/graphics\"" assets)))
               (substitute* "musicPlayer.cpp"
                 (("readTGA\\( \"music\"")
                  (format #false "readTGA(\"~a/music\"" assets))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (assets (string-append out "/share/passage/")))
               (install-file "Passage" bin)
               (install-file "../documentation/Readme.txt" assets)
               (copy-recursively "graphics" (string-append assets "graphics"))
               (copy-recursively "music" (string-append assets "music"))
               (copy-recursively "settings" (string-append assets "settings"))))))))
    (inputs
     `(("sdl" ,(sdl-union (list sdl sdl-mixer)))))
    (native-inputs
     (list imagemagick))
    (home-page "http://hcsoftware.sourceforge.net/passage/")
    (synopsis "Memento mori game")
    (description
     "Passage is meant to be a memento mori game.  It presents an entire life,
from young adulthood through old age and death, in the span of five minutes.
Of course, it's a game, not a painting or a film, so the choices that you make
as the player are crucial.  There's no ``right'' way to play Passage, just as
there's no right way to interpret it.")
    (license license:public-domain)))

(define-public paperview
  (let ((commit "9f8538eb6734c76877b878b8f1e52587f2ae19e6")
        (revision "1"))
    (package
      (name "paperview")
      (version (git-version "0.0.1" revision commit)) ;no upstream release
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/glouw/paperview")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "09sb9sg44fvkgfdyycrm1ndpx7cnkrglxhci41y8f3gpagnvi7jk"))))
      (build-system gnu-build-system)
      (inputs
       (list sdl2))
      (arguments
       '(#:tests? #f ;no tests
         #:make-flags
         (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure))))
      (home-page "https://github.com/glouw/paperview/")
      (synopsis "High performance X11 animated wallpaper setter")
      (description "High performance animated desktop background setter for
X11 that won't set your CPU on fire, drain your laptop battery, or lower video
game FPS.")
      (license license:unlicense))))

(define-public curseofwar
  (package
    (name "curseofwar")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/a-nikolaev/curseofwar")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wd71wdnj9izg5d95m81yx3684g4zdi7fsy0j5wwnbd9j34ilz1i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs (list ncurses))
    (home-page "https://a-nikolaev.github.io/curseofwar/")
    (synopsis "Fast-paced action strategy game")
    (description "Curse of War is a fast-paced action strategy game originally
implemented using ncurses user interface.  An SDL graphical version is also
available.")
    (license license:gpl3+)))

(define-public schiffbruch
  ;; There haven't been any releases for several years, so I've taken the most
  ;; recent commit from the master branch that didn't fail to build (the last
  ;; commit gave me a compile error).
  (let ((commit "e41916d15d87749c82c5005cbb42d1bb079b43d9"))
    (package
      (name "schiffbruch")
      (version (git-version "1.2.1" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sandsmark/Schiffbruch")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0lg3rqacrapf6c4sxi12cm9bmg43mlbclway1zxcm848pi1xkzwv"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f                              ; no tests
         #:build-type "Release"))
      (inputs
       (list sfml))
      (home-page "https://github.com/sandsmark/Schiffbruch/")
      (synopsis "Pixelart survival game")
      (description
       "Schiffbruch is a mix of building, strategy and adventure and gets played
with a two-dimensional view.  The game deals with the consequences of a ship
wreckage.  You're stranded on a desert island and have to survive.  In order to
do so you need to explore the island, find food, build a shelter and try to
get attention, so you get found.")
      (license license:cc-by4.0))))

(define-public sdlpop
  (package
    (name "sdlpop")
    (version "1.22")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/NagyD/SDLPoP")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1yy5r1r0hv0xggk8qd8bwk2zy7abpv89nikq4flqgi53fc5q9xl7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests provided
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'prepare-build
           ;; Set correct environment for SDL.
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append (assoc-ref inputs "sdl")
                                    "/include/SDL2:"
                                    (or (getenv "CPATH") "")))))
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "src")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (opt (string-append out "/opt/sdlpop"))
                    (app (string-append out "/usr/share/applications"))
                    (template "src/SDLPoP.desktop.template"))
               (chdir "..")
               (install-file "prince" bin)
               (substitute* template (("\\$ROOT") out))
               (substitute* "src/seg009.c"
                 (("g_argv[0]") (string-append "\"" out "\"")))
               (install-file template app)
               (rename-file (string-append app "/SDLPoP.desktop.template")
                            (string-append app "/SDLPoP.desktop"))
               (install-file "SDLPoP.ini" opt)
               (copy-recursively "data" (string-append bin "/data"))
               (copy-recursively "doc" opt)
               (copy-recursively "mods" opt)))))))
    (native-inputs (list pkg-config))
    (inputs `(("sdl" ,(sdl-union (list sdl2
                                       sdl2-image
                                       sdl2-mixer)))))
    (synopsis "Port of Prince of Persia game")
    (description "This package provides port of Prince of Persia, based on the
disassembly of the DOS version, extended with new features.")
    (home-page "https://github.com/NagyD/SDLPoP")
    (license license:gpl3+)))

(define-public fheroes2
  (package
    (name "fheroes2")
    (version "0.9.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ihhub/fheroes2")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m8649srzg3j2b1hs4x2y8fib6hn7v0afv4c7bjnfk4bhpi4cqd7"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags '("FHEROES2_STRICT_COMPILATION=1"
                      "RELEASE=1")))
    (native-inputs
     (list gettext-minimal))
    (inputs
     (list libpng
           (sdl-union (list sdl2 sdl2-image sdl2-mixer sdl2-ttf))
           zlib))
    (home-page "https://ihhub.github.io/fheroes2/")
    (synopsis "Turn-based strategy game engine")
    (description "@code{fheroes2} is an implementation of Heroes of Might and
Magic II (aka HOMM2) game engine.  It requires assets and game resources to
play; it will look for them at @file{~/.local/share/fheroes2} folder.")
    (license license:gpl2)))
