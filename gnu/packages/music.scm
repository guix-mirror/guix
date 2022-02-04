;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2016 Al McElrath <hello@yrns.org>
;;; Copyright © 2016, 2017, 2019, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2018, 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2019 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016 John J. Foerch <jjfoerch@earthlink.net>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 nikita <nikita@n0.is>
;;; Copyright © 2017 Rodger Fox <thylakoid@openmailbox.org>
;;; Copyright © 2017–2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2017, 2018, 2019, 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 nee <nee.git@hidamari.blue>
;;; Copyright © 2018, 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018, 2019, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2019 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2019 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2019 Jakob L. Kreuze <zerodaysfordays@sdf.org>
;;; Copyright © 2019 raingloom <raingloom@protonmail.com>
;;; Copyright © 2019 David Wilson <david@daviwil.com>
;;; Copyright © 2019, 2020, 2021 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2019 Riku Viitanen <riku.viitanen0@gmail.com>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2021 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2021, 2022 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2021 Bonface Munyoki Kilyungi <me@bonfacemunyoki.com>
;;; Copyright © 2021 Frank Pursel <frank.pursel@gmail.com>
;;; Copyright © 2021 Rovanion Luckey <rovanion.luckey@gmail.com>
;;; Copyright © 2021 Justin Veilleux <terramorpha@cock.li>
;;; Copyright © 2021, 2022 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021 Simon Streit <simon@netpanic.org>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Thomas Albers Raviola <thomas@thomaslabs.org>
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
  #:use-module (guix gexp)
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
  #:use-module (guix build-system qt)
  #:use-module (guix build-system scons)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system waf)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system go)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages apr)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base) ;libbdf
  #:use-module (gnu packages bash)
  #:use-module (gnu packages benchmark)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages code)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
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
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages gpodder)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libevent)
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
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-web)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio) ;libsndfile
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages stb)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vim)       ;for 'xxd'
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages lua)
  #:use-module ((srfi srfi-1) #:select (last)))

(define-public audacious
  (package
    (name "audacious")
    (version "4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://distfiles.audacious-media-player.org/"
                           "audacious-" version ".tar.bz2"))
       (sha256
        (base32 "0p734psjjvjcmla2hg5h6a9v1prvy63jj9xm2g2ngs49jy7qan0z"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "LDFLAGS=-Wl,-rpath=" %output "/lib")
             "--disable-gtk")
       #:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'unpack-plugins
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((plugins (assoc-ref inputs "audacious-plugins")))
               (invoke "tar" "xvf" plugins)
               #t)))
         (add-after 'unpack-plugins 'configure-plugins
           (lambda* (#:key configure-flags outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion
                   (string-append "audacious-plugins-" ,version)
                 (substitute* "configure"
                   (("/bin/sh") (which "sh")))
                 (apply invoke "./configure"
                        (append configure-flags
                                ;; audacious-plugins requires audacious to build.
                                (list (string-append "PKG_CONFIG_PATH="
                                                     out "/lib/pkgconfig:"
                                                     (getenv "PKG_CONFIG_PATH"))
                                      (string-append "--prefix=" out))))))))
         (add-after 'configure-plugins 'build-plugins
           (lambda _
             (with-directory-excursion
                 (string-append "audacious-plugins-" ,version)
               (invoke "make" "-j" (number->string (parallel-job-count))))))
         (add-after 'build-plugins 'install-plugins
           (lambda _
             (with-directory-excursion
                 (string-append "audacious-plugins-" ,version)
               (invoke "make" "install")))))))
    (native-inputs
     `(("audacious-plugins"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://distfiles.audacious-media-player.org/"
                               "audacious-plugins-" version ".tar.bz2"))
           (sha256
            (base32 "0k0xnqmxi5lna034i2cnzvfzrykxmv4fbs1nkrc9sd2ma1igrmns"))))
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")         ; for gdbus-codegen
       ("pkg-config" ,pkg-config)))
    (inputs
     (list dbus
           qtbase-5
           qtmultimedia
           ;; Plugin dependencies
           alsa-lib
           curl
           faad2
           ffmpeg
           flac
           fluidsynth
           lame
           libbs2b
           libcddb
           libcdio-paranoia
           libcue
           libnotify
           libogg
           libopenmpt
           libsamplerate
           libsndfile
           libvorbis
           libxcomposite
           libxml2
           libxrender
           lirc
           jack-1
           mesa
           mpg123
           neon
           pulseaudio
           sdl2
           soxr
           wavpack))
    (home-page "https://audacious-media-player.org")
    (synopsis "Modular and skinnable audio player")
    (description
     "Audacious is an audio player descended from XMMS.  Drag and drop
folders and individual song files, search for artists and albums in
your entire music library, or create and edit your own custom
playlists.  Listen to CD’s or stream music from the Internet.  Tweak
the sound with the graphical equalizer or experiment with LADSPA
effects.  Enjoy the modern GTK-themed interface or change things up
with Winamp Classic skins.  Use the plugins included with Audacious to
fetch lyrics for your music, to set an alarm in the morning, and
more.")
    ;; According to COPYING, Audacious and its plugins are licensed
    ;; under the BSD 2-clause license and libguess is licensed under
    ;; the BSD 3-clause license.
    (license (list license:bsd-2
                   license:bsd-3
                   ;; Plugin licenses that aren't BSD 2- or 3-clause.
                   license:lgpl2.1
                   license:gpl2
                   license:gpl3
                   license:expat
                   license:isc
                   license:lgpl2.0))))

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
     (list wxwidgets glib alsa-lib))
    (native-inputs
     (list pkg-config))
    (home-page "http://ariamaestosa.sourceforge.net/")
    (synopsis "MIDI sequencer and editor")
    (description
     "Aria Maestosa is a MIDI sequencer and editor.  It lets you compose, edit
and play MIDI files with a few clicks in a user-friendly interface offering
score, keyboard, guitar, drum and controller views.")
    (license license:gpl3+)))

(define-public clementine
  (package
    (name "clementine")
    (version "1.4.0rc1-450-g2725ef99d")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/clementine-player/Clementine")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pcwwi9b2qcfjn748577gqx6d1hgg7cisw2dn43npwafdvvkdb90"))
              (modules '((guix build utils)
                         (ice-9 regex)))
              (snippet
               '(begin
                  (use-modules ((ice-9 regex)))
                  (for-each
                   (lambda (dir)
                     ;; TODO: The following dependencies are still bundled:
                     ;; - "qxt": Appears to be unmaintained upstream.
                     ;; - "qsqlite"
                     ;; - "qtsingleapplication"
                     ;; - "qocoa"
                     ;; - "qtiocompressor"
                     (let ((bundled '("qsqlite"
                                      "qtsingleapplication"
                                      "qxt"
                                      "qocoa"
                                      "qtiocompressor")))
                       (if (not
                            (string-match
                              (string-append ".?*(" (string-join bundled "|") ")")
                              dir))
                           (delete-file-recursively dir))))
                   (find-files "3rdparty"
                               (lambda (file stat)
                                 (string-match "^3rdparty/[^/]*$" file))
                               #:directories? #t))
                  #t))))
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
             ;; Clementine checks that the taglib version is higher than 1.11,
             ;; because of https://github.com/taglib/taglib/issues/864. Remove
             ;; this flag when 1.12 is released.
             "-DUSE_SYSTEM_TAGLIB=TRUE")
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
       ("googletest" ,googletest)
       ("pkg-config" ,pkg-config)
       ("qtlinguist" ,qttools)))
    (inputs
     (list boost
           chromaprint
           fftw
           glib
           glu
           gstreamer
           gst-plugins-base
           gst-plugins-good
           gst-libav
           libcdio
           libmygpo-qt
           ;; TODO: Package libgpod.
           libmtp
           libxml2
           protobuf
           pulseaudio
           qtbase-5
           qtx11extras
           sqlite
           sparsehash
           taglib))
    (home-page "https://clementine-player.org")
    (synopsis "Music player and library organizer")
    (description "Clementine is a multiplatform music player.  It is inspired
by Amarok 1.4, focusing on a fast and easy-to-use interface for searching and
playing your music.")
    (license (list
               ;; clementine and qtiocompressor are under GPLv3.
               license:gpl3+
               ;; qxt is under CPL1.0.
               license:cpl1.0
               ;; qsqlite and qtsingleapplication are under LGPL2.1+.
               license:lgpl2.1+
               ;; qocoa is under MIT and CC by-sa for the icons.
               license:cc-by-sa3.0))))

(define-public strawberry
  (package
    (name "strawberry")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/strawberrymusicplayer/strawberry")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "077dlj0kjcrj4g1h4w8finzykizaar67ik6yng6c8zjp2i5bam1j"))
              (modules '((guix build utils)
                         (ice-9 regex)))
              (snippet
               '(begin
                  (use-modules ((ice-9 regex)))
                  (for-each
                   (lambda (dir)
                     ;; TODO: The following dependencies are still bundled:
                     ;; - "singleapplication"
                     (let ((bundled '("singleapplication")))
                       (if (not
                            (string-match
                              (string-append ".?*(" (string-join bundled "|") ")")
                              dir))
                           (delete-file-recursively dir))))
                   (find-files "3rdparty"
                               (lambda (file stat)
                                 (string-match "^3rdparty/[^/]*$" file))
                               #:directories? #t))
                  #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:test-target "run_strawberry_tests"
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out             (assoc-ref outputs "out"))
                   (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH")))
               (wrap-program (string-append out "/bin/strawberry")
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path)))
               #t)))
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xorg-server (assoc-ref inputs "xorg-server")))
               (system (format #f "~a/bin/Xvfb :1 &" xorg-server))
               (setenv "DISPLAY" ":1")
               (setenv "HOME" (getcwd))
               #t))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("googletest" ,googletest)
       ("pkg-config" ,pkg-config)
       ("qtlinguist" ,qttools)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     (list alsa-lib
           boost
           chromaprint
           dbus
           fftw
           glib
           gnutls
           gstreamer
           gst-plugins-base
           gst-plugins-good
           libcdio
           libmtp
           protobuf
           pulseaudio
           qtbase-5
           qtx11extras
           sqlite
           taglib))
    (home-page "https://www.strawberrymusicplayer.org/")
    (synopsis "Music player and library organizer")
    (description "Strawberry is a music player and music collection organizer.
It is a fork of Clementine aimed at music collectors and audiophiles.")
    (license (list
              ;; strawberry.
              license:gpl3+
              ;; singleapplication
              license:expat
              ;; icons.
              license:cc-by-sa3.0))))

(define-public cmus
  (package
    (name "cmus")
    (version "2.9.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cmus/cmus")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zjkimni2fhv4yskrjrgj6b74f33rfj58zgd7khwrz4z8nf88j0w"))))
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
     (list pkg-config))
    (inputs
     (list alsa-lib
           ao
           faad2
           ffmpeg
           flac
           jack-1
           libcddb
           libcdio-paranoia
           libcue
           libmad
           libmodplug
           libmpcdec
           libsamplerate
           libvorbis
           ncurses
           opusfile
           pulseaudio
           wavpack))
     (home-page "https://cmus.github.io/")
     (synopsis "Small console music player")
     (description "Cmus is a small and fast console music player.  It supports
many input formats and provides a customisable Vi-style user interface.")
     (license license:gpl2+)))

(define-public denemo
  (package
    (name "denemo")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/denemo/denemo-" version ".tar.gz"))
       (sha256
        (base32 "05kwy8894hsxr6123hc854j2qq2sxyjw721zk4g3vzz8pw29p887"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Tests require to write $HOME.
             (setenv "HOME" (getcwd))
             ;; Replace hard-coded diff file name.
             (substitute* "tests/integration.c"
               (("/usr/bin/diff")
                (search-input-file inputs "/bin/diff")))
             ;; Denemo's documentation says to use this command to run its
             ;; test suite.
             (invoke "make" "-C" "tests" "check")))
         (add-before 'build 'set-lilypond
           ;; This phase sets the default path for lilypond to its current
           ;; location in the store.
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((lilypond (search-input-file inputs "/bin/lilypond")))
               (substitute* "src/core/prefops.c"
                 (("g_string_new \\(\"lilypond\"\\);")
                  (string-append "g_string_new (\""
                                 lilypond
                                 "\");"))))
             #t)))))
    (native-inputs
     `(("diffutils" ,diffutils)
       ("glib:bin" ,glib "bin")         ; for gtester
       ("gtk-doc" ,gtk-doc)
       ("intltool" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list alsa-lib
           aubio
           evince
           fftw
           fluidsynth
           glib
           gtk+
           gtksourceview-3
           guile-2.0
           librsvg
           libsndfile
           libxml2
           lilypond
           portaudio
           portmidi
           rubberband))
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

(define-public dumb
  (package
    (name "dumb")
    (version "2.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kode54/dumb")
             (commit version)))
       (sha256
        (base32 "1cnq6rb14d4yllr0yi32p9jmcig8avs3f43bvdjrx4r1mpawspi6"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f ; no check target
       #:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             "-DBUILD_EXAMPLES=OFF")))
    (home-page "https://github.com/kode54/dumb")
    (synopsis "Module audio renderer library")
    (description
     "DUMB is a tracker library with support for IT, XM, S3M and MOD files.  It
targets maximum accuracy to the original formats, with low-pass resonant filters
for the IT files, accurate timing and pitching, and three resampling quality
settings (aliasing, linear interpolation and cubic interpolation).")
    ;; The DUMB license is a bit peculiar.
    ;; Clause 8 states that clauses 4, 5 and 6 are null and void, leaving only
    ;; the first three clauses for genuine consideration.
    ;; Clauses 1, 2 and 3 are analogous to clauses 1, 2 and 3 of the zlib
    ;; license, a known free software license.
    ;; Therefore, the DUMB license may be considered a free software license.
    (license (license:fsf-free "file://LICENSE"))))

(define-public dumb-allegro4
  (package
    (inherit dumb)
    (name "dumb-allegro4")
    (arguments
     (substitute-keyword-arguments (package-arguments dumb)
       ((#:configure-flags flags)
        `(cons "-DBUILD_ALLEGRO4=ON" ,flags))))
    (inputs
     (list allegro-4))))

(define-public hydrogen
  (package
    (name "hydrogen")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hydrogen-music/hydrogen")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12mwkglyz88hwb16fb3fahn95janim2mrvnxkzp8pcsw3wybi3dn"))))
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
       ("pkg-config" ,pkg-config)
       ("qtlinguist" ,qttools)))
    (inputs
     (list alsa-lib
           jack-1
           ;; ("ladspa" ,ladspa) ; require LADSPA_PATH to be set
           lash
           libarchive
           liblo
           libsndfile
           lrdf
           pulseaudio
           qtbase-5
           qtxmlpatterns
           zlib))
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
     (list flac
           gtk+
           id3lib
           libid3tag
           libvorbis
           opusfile
           speex
           taglib
           wavpack
           yelp))
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

                      Name: libid3tag~@
                      Description:~@
                      Version:~@
                      Libs: -L${libdir} -lid3tag -lz~@
                      Cflags: -I${includedir}~%"
                     libid3tag)))
               (setenv "PKG_CONFIG_PATH"
                 (string-append (getenv "PKG_CONFIG_PATH")
                   ":" (getcwd) "/pkgconfig"))
               #t)))
         (add-after 'unpack 'patch-makefile
           (lambda _
             (substitute* "Makefile.in"
               ;; The Makefile generates a test-desktop-file-validate.sh
               ;; script with /bin/sh hard-coded.
               (("/bin/sh") (which "sh"))
               ;; Don't create 'icon-theme.cache'.
               (("gtk-update-icon-cache") "true"))
             #t)))))
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
    (version "0.8.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/digego/extempore")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "16i12zl3g1zpx6lhg5pg821xirdf9rxx5m11b68inf83wn6hknhb"))
              (file-name (git-file-name name version))
              (patches (search-patches
                        "extempore-unbundle-external-dependencies.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove bundled sources.
                  (map delete-file-recursively
                       '("src/pcre"))
                  #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DJACK=ON"
                               "-DPACKAGE=ON"
                               (string-append "-DEXT_SHARE_DIR="
                                              (assoc-ref %outputs "out")
                                              "/share"))
       #:modules ((ice-9 match)
                  (guix build cmake-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-aot-libs
           (lambda _
             (for-each (lambda (target)
                         (invoke "make" target))
                       '("aot_base"
                         "aot_math"
                         "aot_instruments"))
             #t))
         (add-after 'unpack 'patch-install-locations
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("EXT_SHARE_DIR=\"\\.\"\\)")
                "EXT_SHARE_DIR=\"${EXT_SHARE_DIR}/extempore\")")
               (("DESTINATION \"\\.\"\\)") "DESTINATION bin)")
               (("DESTINATION \"\\.\"\n") "DESTINATION share/extempore\n"))
             #t))
         (add-after 'unpack 'patch-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "extras/extempore.el"
               (("\\(runtime-directory \\(concat default-directory \"runtime\"\\)\\)")
                (string-append "(runtime-directory \""
                               (assoc-ref outputs "out")
                               "/share/extempore/runtime"
                               "\")")))
             #t))
         (add-after 'unpack 'link-with-additional-libs
           (lambda _
             ;; The executable must be linked with libffi and zlib.
             (substitute* "CMakeLists.txt"
               (("target_link_libraries\\(extempore PRIVATE dl" line)
                (string-append line " ffi z")))
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
               ;; Extempore needs to be told where the runtime is to be found.
               ;; While we're at it we disable automatic tuning for a specific
               ;; CPU to make binary substitution possible.
               (("COMMAND extempore" prefix)
                (string-append prefix " --sharedir " (getcwd)
                               " --mcpu=generic --attr=none")))
             #t))
         (add-after 'unpack 'symlink-assets
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((assets (assoc-ref inputs "extempore-assets")))
               (symlink assets "assets")
               #t))))))
    (inputs
     `(("llvm"
        ,(package
           (inherit llvm-3.8)
           (name "llvm-for-extempore")
           (source
            (origin
              (method url-fetch)
              (uri (string-append "http://extempore.moso.com.au/extras/"
                                  "llvm-3.8.0.src-patched-for-extempore.tar.xz"))
              (sha256
               (base32
                "1svdl6fxn8l01ni8mpm0bd5h856ahv3h9sdzgmymr6fayckjvqzs"))))))
       ("extempore-assets"
        ,(let ((commit "0c9f32c18169b3fbc24bc1ad66283125b54a0c85")
               (revision "0")
               (version "0.0.0"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/extemporelang/extempore-assets")
                   (commit commit)))
             (file-name (git-file-name "extempore-assets"
                                       (git-version version revision commit)))
             (sha256
              (base32 "1pxmcbngd9qx8m71d5rfsmf4h31jnsnd3wjh8vb0rwskif22xz8l")))))
       ("libffi" ,libffi)
       ("jack" ,jack-1)
       ("libsndfile" ,libsndfile)
       ("glfw" ,glfw)
       ("apr" ,apr)
       ("stb-image"
        ,(let ((revision "1")
               (commit "152a250a702bf28951bb0220d63bc0c99830c498"))
           (package
             (inherit stb-image)
             (name "stb-image-for-extempore")
             (version (git-version "0" revision commit))
             (source
              (origin (method git-fetch)
                      (uri (git-reference
                            (url "https://github.com/extemporelang/stb")
                            (commit commit)))
                      (sha256
                       (base32
                        "0y0aa20pj9311x2ii06zg8xs34idg14hfgldqc5ymizc6cf1qiqv"))
                      (file-name (git-file-name name version))))
             (build-system cmake-build-system)
             (arguments `(#:tests? #f)) ;no tests included
             (inputs '()))))
       ("kiss-fft" ,kiss-fft-for-extempore)
       ("nanovg" ,nanovg-for-extempore)
       ("portmidi"
        ,(let ((version "217")
               (revision "0")
               (commit "8602f548f71daf5ef638b2f7d224753400cb2158"))
           (package
             (inherit portmidi)
             (name "portmidi-for-extempore")
             (version (git-version version revision commit))
             (source (origin
                       (method git-fetch)
                       (uri (git-reference
                             (url "https://github.com/extemporelang/portmidi")
                             (commit commit)))
                       (file-name (git-file-name name version))
                       (sha256
                        (base32
                         "1qidzl1s3kzhczzm96rcd2ppn27a97k2axgfh1zhvyf0s52d7m4w"))))
             (build-system cmake-build-system)
             (arguments `(#:tests? #f)) ;no tests
             (native-inputs '()))))
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
    (home-page "https://github.com/digego/extempore")
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

(define-public fluida-lv2
  (package
   (name "fluida-lv2")
   (version "0.6")
   (source
    (origin
      (method git-fetch)
      (uri
       (git-reference
        (url "https://github.com/brummer10/Fluida.lv2")
        (commit (string-append "v" version))
        (recursive? #t))) ; references specific commit of libxputty
      (file-name (git-file-name name version))
      (sha256
       (base32
        "1v0bh4wcx79y832qigc3my8ixq0r4ica6z5fg2rg946pkh20x1a2"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f  ; no "check" target
      #:make-flags
      (list (string-append "INSTALL_DIR="
                           (assoc-ref %outputs "out") "/lib/lv2")
            "CC=gcc")
      #:phases
      (modify-phases %standard-phases
        (delete 'configure))))
   (inputs
    (list cairo libx11 lv2 fluidsynth))
   (native-inputs
    (list pkg-config))
   (home-page "https://github.com/brummer10/Fluida.lv2")
   (synopsis "Fluidsynth as an LV2 audio plugin")
   (description "Fluida is an audio plugin in the LV2 format that acts as
a frontend for fluidsynth.")
   (license license:gpl2+)))

(define-public surge-synth
  (package
   (name "surge-synth")
   (version "1.7.1")
   (source
     (origin
       (method git-fetch)
        (uri (git-reference
               (url "https://github.com/surge-synthesizer/surge")
               (commit (string-append "release_" version))
               (recursive? #t))) ; build system expects modules to be there
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1jhk8iaqh89dnci4446b47315v2lc8gclraygk8m9jl20zpjxl0l"))))
   (build-system cmake-build-system)
   (arguments
    `(#:tests? #f ; no tests included
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'replace-python
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "CMakeLists.txt"
              ((" python ")
               (string-append " " (assoc-ref inputs "python")
                              "/bin/python3 ")))
            #t))
        (add-after 'unpack 'fix-data-directory-name
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "src/common/SurgeStorage.cpp"
              (("/usr") (assoc-ref outputs "out")))
            #t))
        (replace 'install ; no install target
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((src (assoc-ref inputs "source"))
                   (out (assoc-ref outputs "out"))
                   (share (string-append out "/share"))
                   (lib (string-append out "/lib"))
                   (lv2 (string-append lib "/lv2"))
                   (vst3 (string-append lib "/vst3")))
              (mkdir-p lv2)
              (mkdir-p vst3)
              ;; Install LV2 plugin.
              (copy-recursively "surge_products/Surge.lv2"
                                (string-append lv2 "/Surge.lv2"))
              ;; Install VST3 plugin.
              (copy-recursively "surge_products/Surge.vst3"
                                (string-append vst3 "/Surge.vst3"))
              ;; Install data.
              (copy-recursively (string-append src "/resources/data")
                                (string-append share "/Surge"))
              #t))))))
   (inputs
    (list cairo
          libxkbcommon
          python
          xcb-util
          xcb-util-cursor
          xcb-util-keysyms))
   (native-inputs
    (list pkg-config))
   (home-page "https://surge-synthesizer.github.io/")
   (synopsis "Synthesizer plugin")
   (description
    "Surge is a subtractive hybrid digital synthesizer.  Each patch contains
two @dfn{scenes} which are separate instances of the entire synthesis
engine (except effects) that can be used for layering or split patches.")
   (license license:gpl3+)))

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
       #:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'be-permissive
           (lambda _
             (substitute* "SConstruct"
               (("'-Wall'") "'-Wall', '-fpermissive'"))
             #t))
         (add-after 'unpack 'replace-removed-scons-syntax
           (lambda _
             (substitute* "SConstruct"
               (("BoolOption") "BoolVariable")
               (("PathOption") "PathVariable")
               (("Options") "Variables"))
             #t)))))
    (inputs
     (list boost
           jack-1
           libsndfile
           libsamplerate
           liblo
           rubberband))
    (native-inputs
     (list pkg-config))
    (home-page "http://das.nasophon.de/klick/")
    (synopsis "Metronome for JACK")
    (description
     "klick is an advanced command-line based metronome for JACK.  It allows
you to define complex tempo maps for entire songs or performances.")
    (license license:gpl2+)))

(define-public glyr
  (package
    (name "glyr")
    (version "1.0.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sahib/glyr")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1miwbqzkhg0v3zysrwh60pj9sv6ci4lzq2vq2hhc6pc6hdyh8xyr"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DTEST=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda _
             (substitute* "spec/capi/check_api.c"
               (("fail_unless \\(c != NULL,\"Could not load www.google.de\"\\);")
                ""))
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; capi tests
               (invoke "bin/check_api")
               ;; (invoke "bin/check_opt") TODO Very dependent on the network
               (invoke "bin/check_dbc"))

             ;; TODO Work out how to run the spec/providers Python tests
             #t)))))
    (inputs
     (list glib curl sqlite))
    (native-inputs
     (list pkg-config check))
    (home-page "https://github.com/sahib/glyr")
    (synopsis "Search engine for music related metadata")
    (description
     "Glyr comes both in a command-line interface tool (@command{glyrc}) and
as a C library (libglyr).

The sort of metadata glyr is searching (and downloading) is usually the data
you see in your musicplayer.  And indeed, originally it was written to serve
as internally library for a musicplayer, but has been extended to work as a
standalone program which is able to download cover art, lyrics, photos,
biographies, reviews and more.")
    (license license:lgpl3+)))

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
     (list klick python2-pyliblo python2-pygtk))
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (home-page "http://das.nasophon.de/gtklick/")
    (synopsis "Simple metronome with an easy-to-use graphical interface")
    (description
     "Gtklick is a simple metronome with an easy-to-use graphical user
interface.  It is implemented as a frontend to @code{klick}.")
    (license license:gpl2+)))

(define-public lingot
  (package
    (name "lingot")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ibancg/lingot")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04lcjzfhddbyskxr2068z609y6x0s2gjx1wl78w0dkxdi459zrn9"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           cunit
           `(,glib "bin") ; for glib-compile-resources
           intltool
           libtool
           pkg-config))
    (inputs
     (list alsa-lib
           fftw
           gtk+
           jack-2
           json-c
           pulseaudio))
    (home-page "http://lingot.nongnu.org/")
    (synopsis "Accurate & configurable musical instrument tuner")
    (description
     "LINGOT is a musical instrument tuner.  It's accurate, easy to use, and
highly configurable.  Originally conceived to tune electric guitars, it can now
be used to tune other instruments.

It looks like an analogue tuner, with a gauge indicating the relative shift to a
certain note, determined automatically as the closest note to the estimated
frequency.")
    (license license:gpl2+)))

(define-public ninjas2
  (package
    (name "ninjas2")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/clearly-broken-software/ninjas2")
         (commit (string-append "v" version))
         ;; Bundles a specific commit of the DISTRHO plugin framework.
         (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kwp6pmnfar2ip9693gprfbcfscklgri1k1ycimxzlqr61nkd2k9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no tests
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "CC=" ,(cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ;no configure target
         (replace 'install              ;no install target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lv2 (string-append out "/lib/lv2")))
               ;; Install LV2.
               (for-each
                (lambda (file)
                  (copy-recursively file
                                    (string-append lv2 "/" (basename file))))
                (find-files "bin" "\\.lv2$" #:directories? #t))
               ;; Install executables.
               (for-each
                 (lambda (file)
                   (install-file file bin))
                 (find-files "bin"
                             (lambda (name stat)
                               (and
                                 (equal? (dirname name) "bin")
                                 (not (string-suffix? ".so" name))
                                 (not (string-suffix? ".lv2" name))))))
               #t))))))
    (inputs
     (list fftwf
           jack-1 ; for the standalone JACK application
           libsamplerate
           mesa
           libsndfile))
    (native-inputs
     (list ladspa lv2 pkg-config))
    (synopsis "Sample slicer audio plugin")
    (description
     "Ninjas 2 is a rewrite of the Ninjas sample slicer audio plugin.
Its goal is to be an easy to use sample slicer with quick slicing of samples
and auto-mapping slices to MIDI note numbers.")
    (home-page "https://github.com/clearly-broken-software/ninjas2")
    (license license:gpl3+)))

(define-public lilypond
  (package
    (name "lilypond")
    (version "2.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://lilypond.org/download/sources/"
                           "v" (version-major+minor version) "/"
                           "lilypond-" version ".tar.gz"))
       (sha256
        (base32 "0qd6pd4siss016ffmcyw5qc6pr2wihnvrgd4kh1x725w7wr02nar"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;out-test/collated-files.html fails
       #:out-of-source? #t
       #:make-flags '("conf=www")       ;to generate images for info manuals
       #:configure-flags
       (list "CONFIGURATION=www"
             (string-append "--with-texgyre-dir="
                            (assoc-ref %build-inputs "font-tex-gyre")
                            "/share/fonts/opentype/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-path-references
           (lambda _
             (substitute* "scm/backend-library.scm"
               (("\\(search-executable '\\(\"gs\"\\)\\)")
                (string-append "\"" (which "gs") "\""))
               (("\"/bin/sh\"")
                (string-append "\"" (which "sh") "\"")))))
         (add-after 'fix-path-references 'adjust-to-API-change
           (lambda _
             (substitute* '("Documentation/pictures/GNUmakefile"
                            "stepmake/stepmake/tex-rules.make")
               ((".*-c .setpdfwrite.*") ""))
             (substitute* "scm/backend-library.scm"
               (("\"-c.setpdfwrite\"") ""))
             (substitute* "scm/framework-ps.scm"
               (("\".setpdfwrite ") "\""))))
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
             (setenv "conf" "www")))
         (add-after 'install 'install-info
           (lambda _
             (invoke "make"
                     "-j" (number->string (parallel-job-count))
                     "conf=www" "install-info"))))))
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
       ("netpbm" ,netpbm)               ;for pngtopnm
       ("texlive" ,(texlive-updmap.cfg (list texlive-metapost
                                        texlive-epsf
                                        texlive-lh
                                        texlive-latex-cyrillic)))
       ("texinfo" ,texinfo)
       ("texi2html" ,texi2html-1.82)
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

(define-public music21
  (package
    (name "music21")
    (version "7.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "music21" version))
        (sha256
          (base32 "17v2id8qm99xqymqsdczq173fmbdha4w109ahh8j1d9l5a7mqc86"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               ;; See: https://github.com/cuthbertLab/music21/issues/1164
               (invoke "python" "-m" "music21.stream.tests")))))))
    (propagated-inputs
      (list python-chardet python-joblib python-more-itertools
            python-webcolors))
    (home-page "https://web.mit.edu/music21/")
    (synopsis "Toolkit for Computational Musicology")
    (description
     "Music21 is a set of tools for helping scholars and other active
listeners answer questions about music quickly and simply.")
    ;; Software is dual-licensed.
    (license (list license:bsd-3 license:lgpl3+))))

(define-public abjad
  (package
    (name "abjad")
    (version "3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
         (url "https://github.com/Abjad/abjad")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0s63vk9fifp0im9c31kb9ck39mbaxhrls993d8fvg0nkg41z1jnz"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'loosen-requirements
           (lambda _
             (substitute* "setup.py"
               ;; Permit newer versions of uqbar.  Remove for >3.4.
               ((", <0\\.5\\.0")
                ""))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; See: https://stackoverflow.com/a/34140498
               (invoke "python" "-m" "pytest" "tests")))))))
    (inputs
     (list lilypond))
    (propagated-inputs
     (list python-ply
           python-quicktions
           python-roman
           python-six
           python-uqbar
           ;; XXX: These test dependencies(?) are listed as install_requires
           ;; in setup.py.  Propagate accordingly.
           python-black
           python-flake8
           python-isort
           python-mypy
           python-pytest
           python-pytest-cov
           python-pytest-helpers-namespace
           python-sphinx-autodoc-typehints))
    (home-page "https://abjad.github.io")
    (synopsis "Python API for building LilyPond files")
    (description
     "Abjad helps composers build up complex pieces of music notation in iterative
and incremental ways.  Use Abjad to create a symbolic representation of all the notes,
rests, chords, tuplets, beams and slurs in any score.  Because Abjad extends the Python
programming language, you can use Abjad to make systematic changes to music as you work.
Because Abjad wraps the LilyPond music notation package, you can use Abjad to control the
typographic detail of symbols on the page.")
     (license license:expat)))

(define-public python-abjad
  (deprecated-package "python-abjad" abjad))

(define-public abjad-ext-rmakers
  (package
    (name "abjad-ext-rmakers")
    (version "3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
         (url "https://github.com/Abjad/abjad-ext-rmakers")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0wma9vzn42h1rhbzh2dwjsrzjhsi1yqdgn6wx1dfk78vaki6prd8"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "pytest" ".")
               #t))))))
    (native-inputs
     (list lilypond
           python-black
           python-flake8
           python-iniconfig
           python-isort
           python-mypy
           python-pytest
           python-pytest-cov
           python-pytest-helpers-namespace))
    (propagated-inputs
     (list abjad))
    (home-page "https://abjad.github.io")
    (synopsis "Abjad rhythm-maker extension package")
    (description
     "@code{abjad-ext-rmakers} includes a collection of classes for creating and
and manipulating rhythms such as accelerandi, taleas, and more.")
    (license license:expat)))

(define-public abjad-ext-nauert
  (package
    (name "abjad-ext-nauert")
    (version "3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
         (url "https://github.com/Abjad/abjad-ext-nauert")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "05hr2lr6myzi493k8vc19cqzraxxnbdwlckwbnras19l5g5ns38x"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "pytest" "tests")
               #t))))))
    (native-inputs
     (list lilypond
           python-black
           python-flake8
           python-iniconfig
           python-isort
           python-mypy
           python-pytest
           python-pytest-cov
           python-pytest-helpers-namespace))
    (propagated-inputs
     (list abjad))
    (home-page "https://abjad.github.io")
    (synopsis "Abjad quantization extension, based on Paul Nauert's Q-Grids")
    (description
     "@code{abjad-ext-nauert} provides classes for dealing with composer and
music theorist Paul Nauert's quantization grids or Q-Grids, for short.")
    (license license:expat)))

(define-public abjad-ext-ipython
  (package
    (name "abjad-ext-ipython")
    (version "3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
         (url "https://github.com/Abjad/abjad-ext-ipython")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1vv0alpiz0gf5lgjfvlh4km72dvrxfqkwzxl3k4amzci3i0jzbs2"))))
    (build-system python-build-system)
    (arguments
     ;; UnboundLocalError: local variable 'output_path' referenced before assignment
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'loosen-requirements
           (lambda _
             (substitute* "setup.py"
               ;; Don't require a specific version of abjad.
               (("abjad==")
                "abjad>="))))
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (setenv "HOME" (getcwd))
               (add-installed-pythonpath inputs outputs)
               ;; From 'make jupyter-test'
               (invoke "jupyter" "nbconvert" "--to=html"
               "--ExecutePreprocessor.enabled=True" "tests/test.ipynb")))))))
    (native-inputs
     (list lilypond
           python-black
           python-flake8
           python-iniconfig
           python-isort
           python-mypy
           python-pytest
           python-pytest-cov
           python-pytest-helpers-namespace))
    (propagated-inputs
     (list abjad jupyter))
    (home-page "https://abjad.github.io")
    (synopsis "Abjad IPython Extension")
    (description
     "@code{abjad-ext-ipython} makes it possible to embed music notation in
@code{jupyter} notebooks.")
    (license license:expat)))

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
         #:python ,python-2))
      (inputs
       (list jack-1 libsigc++ liblo ntk))
      (native-inputs
       (list pkg-config))
      (home-page "https://non.tuxfamily.org/wiki/Non%20Sequencer")
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
     (list jack-1 liblo ntk))
    (native-inputs
     (list pkg-config))
    (home-page "https://non.tuxfamily.org/nsm/")
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
     (list jack-1 liblo ladspa lrdf ntk))
    (native-inputs
     (list pkg-config))
    (home-page "https://non.tuxfamily.org/wiki/Non%20Mixer")
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
     (list jack-1 liblo libsndfile ntk))
    (native-inputs
     (list pkg-config))
    (home-page "https://non.tuxfamily.org/wiki/Non%20Timeline")
    (synopsis "Modular digital audio timeline arranger")
    (description
     "The Non Timeline is a powerful, reliable and fast modular digital audio
timeline arranger.  It utilizes JACK for inter-application audio I/O and the
NTK GUI toolkit for a fast and lightweight user interface.  Non Timeline can
be used alone or in concert with Non Mixer and Non Sequencer to form a
complete studio.")
    (license license:gpl2+)))

(define-public tascam-gtk
  ;; This commit represents the latest version at the time of this writing.
  (let ((commit "17b8575ff88dfd2ede0f7ef9c5c5597ab8a00702")
        (revision "0"))
    (package
      (name "tascam-gtk")
      (version (git-version "0.4" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/onkelDead/tascam-gtk")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "07k7rccqqg7lnygkh97a200l1i6s1rl92n01v0q6n4257sinir6f"))))
      (build-system gnu-build-system)
      (inputs
       (list liblo gtkmm-3 alsa-lib libxml++-2))
      (native-inputs
       (list `(,glib "bin") pkg-config))
      (home-page "https://github.com/onkelDead/tascam-gtk")
      (synopsis "GTK+ based application to control Tascam US-16x08 DSP mixer")
      (description "This is a mixer application to control the Tascam US-16x08
audio interface.  This device contains about 280 control elements and this
mixer application aims to provide comfortable access to the DSP effects the
device supports.")
      (license license:expat))))

(define-public bsequencer
  (package
    (name "bsequencer")
    (version "1.8.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sjaehn/BSEQuencer")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0w3m7x0619iq8rafcy0bal4gwh9m9h7iq93q7gkpxhv6dq58ix6l"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f                      ; there are none
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     (list cairo lv2 libx11))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/sjaehn/BSEQuencer")
    (synopsis "Multi-channel MIDI step sequencer LV2 plugin")
    (description
     "This package provides a multi-channel MIDI step sequencer LV2 plugin
with a selectable pattern matrix size.")
    (license license:gpl3+)))

(define-public bchoppr
  (package
    (inherit bsequencer)
    (name "bchoppr")
    (version "1.10.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sjaehn/BChoppr")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gxz0cpxdfj7ajcd9yg05d26i7p24mx5865vy3ph76ni8kycdlrc"))))
    (synopsis "Audio stream-chopping LV2 plugin")
    (description "B.Choppr cuts the audio input stream into a repeated
sequence of up to 16 chops.  Each chop can be leveled up or down (gating).
B.Choppr is the successor of B.Slizr.")
    (home-page "https://github.com/sjaehn/BChoppr")
    (license license:gpl3+)))

(define-public bshapr
  (package
    (inherit bsequencer)
    (name "bshapr")
    (version "0.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sjaehn/BShapr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qr3fxqcplgb6iqi2vxc27jghhv6qsidww2by15zb2vs34yh73pl"))))
    (synopsis "Beat/envelope shaper LV2 plugin")
    (description "B.Shapr is a beat/envelope shaper LV2 plugin.")
    (home-page "https://github.com/sjaehn/BShapr")
    (license license:gpl3+)))

(define-public bjumblr
  (package
    (inherit bsequencer)
    (name "bjumblr")
    (version "1.6.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sjaehn/BJumblr")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00fgax8aqqggs54pjpagw5pc30kgxaghh8mrzpqwhs06cnchcam9"))))
    (inputs
     (list cairo libsndfile lv2))
    (synopsis "Pattern-controlled audio stream/sample re-sequencer LV2 plugin")
    (description "B.Jumblr is a pattern-controlled audio stream / sample
re-sequencer LV2 plugin.")
    (home-page "https://github.com/sjaehn/BJumblr")
    (license license:gpl3+)))

(define-public bschaffl
  (package
    (inherit bsequencer)
    (name "bschaffl")
    (version "1.4.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sjaehn/BSchaffl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kfc75xhj365fwl8cbvhg5chwz1snzcvf4929flds02ljylc7k6d"))))
    (inputs
     `(("cairo" ,cairo)
       ("fontconfig" ,fontconfig)
       ("libsndfile" ,libsndfile)
       ("libx11" ,libx11)
       ("lv2" ,lv2)))
    (home-page "https://github.com/sjaehn/BSchaffl")
    (synopsis "Pattern-controlled MIDI amp & time stretch LV2 plugin")
    (description "This package provides an LV2 plugin that allows for
pattern-controlled MIDI amp & time stretching to produce shuffle / swing
effects.

Key features include:

@enumerate
@item MIDI velocity amplification and timing manipulation plugin
@item Swing and shuffle rhythms
@item Pre-generator dynamics
@item Tempo rubato
@item Pattern (sliders) or shape-controlled
@item MIDI filters
@item Smart quantization
@end itemize
")
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
                    (path (getenv "GUIX_PYTHONPATH")))
               (wrap-program (string-append out "/bin/solfege")
                 `("GUIX_PYTHONPATH" ":" prefix (,path))))
             #t)))))
    (inputs
     `(("python" ,python-2)
       ("pygtk" ,python2-pygtk)
       ("gettext" ,gettext-minimal)
       ("gtk" ,gtk+)
       ("lilypond" ,lilypond)))
    (native-inputs
     (list pkg-config
           txt2man
           libxml2 ; for tests
           ghostscript
           texinfo-5))
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
    (version "2.0.0-alpha14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/powertab/powertabeditor")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wsvni2aa9h2bpndlic7ckch4n600ahwm56n521y5vxivwjx3jmj"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check (lambda _ (invoke "bin/pte_tests")))
         (add-after 'unpack 'fix-pugixml-detection
           (lambda _
             (substitute* "cmake/third_party/pugixml.cmake"
               (("add_library") "#add_library"))
             #t)))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("boost" ,boost)
       ("minizip" ,minizip)
       ("pugixml" ,pugixml)
       ("qtbase" ,qtbase-5)
       ("rapidjson" ,rapidjson)
       ("rtmidi" ,rtmidi)
       ("timidity" ,timidity++)
       ("zlib" ,zlib)))
    (native-inputs
     (list doctest pkg-config))
    (home-page "https://github.com/powertab/powertabedito")
    (synopsis "Guitar tablature editor")
    (description
     "Power Tab Editor 2.0 is the successor to the famous original Power Tab
Editor.  It is compatible with Power Tab Editor 1.7 and Guitar Pro.")
    (license license:gpl3+)))

(define-public jalv-select
  (package
    (name "jalv-select")
    (version "1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/brummer10/jalv_select")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15yanq1wra0hyh6x72ji7pk562iddg476g3vksj495x91zhnl6vm"))))
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
                (string-append "ls -1 " (assoc-ref inputs "jalv") "/bin"))))))))
    (inputs
     (list lilv lv2 jalv gtkmm-2))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/brummer10/jalv_select")
    (synopsis "GUI to select LV2 plugins and run them with jalv")
    (description
     "The jalv.select package provides a graphical user interface allowing
users to select LV2 plugins and run them with jalv.")
    (license license:public-domain)))

(define-public mixxx
  (package
    (name "mixxx")
    (version "2.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mixxxdj/mixxx")
             (commit version)))
       (file-name (git-file-name name version))
       (patches
        (search-patches "mixxx-link-qtscriptbytearray-qtscript.patch"
                        "mixxx-system-googletest-benchmark.patch"))
       (sha256
        (base32 "1h26vpf60rk56fsw1nvmxihg7ammlj8q4zgim49q4s4ww7j8wwqj"))
       (modules '((guix build utils)))
       (snippet
        ;; Delete libraries that we already have or don't need.
        ;; TODO: try to unbundle more (see lib/).
        `(begin
           (let ((third-parties '("apple" "benchmark" "googletest" "hidapi"
                                  "libebur128")))
             (with-directory-excursion "lib"
               (map (lambda (third-party)
                      (delete-file-recursively third-party))
                    third-parties)))
           #t))))
    (build-system qt-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Tests need a running X server.
         (add-before 'check 'prepare-x-for-test
           (lambda _
             (system "Xvfb &")
             (setenv "DISPLAY" ":0")))
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (faad2 (assoc-ref inputs "faad2")))
               (wrap-program (string-append out "/bin/mixxx")
                 `("LD_LIBRARY_PATH" ":" prefix
                   ,(list (string-append faad2 "/lib"))))))))))
    (native-inputs
     (list benchmark
           googletest
           python-wrapper
           qttools
           xorg-server-for-tests))
    (inputs
     (list bash-minimal
           chromaprint
           faad2
           ffmpeg
           fftw
           flac
           glu
           hidapi
           jack-1
           lame
           libdjinterop
           libebur128
           libid3tag
           libkeyfinder
           libmad
           libmp4v2
           libmodplug
           libsndfile
           libshout
           ;; XXX: Mixxx complains the libshout-idjc package suffers from bug
           ;; lp1833225 and refuses to use it.  Use the bundle for now.
           ;; libshout-idjc
           libusb
           libvorbis
           lilv
           mp3guessenc
           openssl
           opusfile
           portaudio
           portmidi
           protobuf
           qtbase-5
           qtdeclarative
           qtkeychain
           qtscript
           qtsvg
           qtx11extras
           rubberband
           soundtouch
           sqlite
           taglib
           upower
           vamp
           wavpack))
    (home-page "https://mixxx.org/")
    (synopsis "DJ software to perform live mixes")
    (description "Mixxx is a DJ software.  It integrates the tools DJs need to
perform creative live mixes with digital music files.")
    (license license:gpl2+)))

(define-public synthv1
  (package
    (name "synthv1")
    (version "0.9.24")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://sourceforge/synthv1/synthv1/" version
                              "/synthv1-" version ".tar.gz"))
              (sha256
               (base32
                "0m9xpl7kq0zlain8598q5cqrh1c7ima2w8jrpq7ds8vh97r7p1bl"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; there are no tests
    (inputs
     (list jack-1
           lv2
           alsa-lib
           non-session-manager
           liblo
           qtbase-5))
    (native-inputs
     (list pkg-config qttools))
    (home-page "https://synthv1.sourceforge.io")
    (synopsis "Polyphonic subtractive synthesizer")
    (description
     "Synthv1 is an old-school subtractive polyphonic synthesizer with four
oscillators and stereo effects.")
    (license license:gpl2+)))

(define-public drumkv1
  (package
    (name "drumkv1")
    (version "0.9.24")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://sourceforge/drumkv1/drumkv1/" version
                              "/drumkv1-" version ".tar.gz"))
              (sha256
               (base32
                "04k0mkvlz9ka0mlck4g7c86s1rhb9pkhml6j6n9b5hwlbq8a9mxk"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; there are no tests
    (inputs
     (list jack-1
           lv2
           libsndfile
           alsa-lib
           non-session-manager
           liblo
           qtbase-5))
    (native-inputs
     (list pkg-config qttools))
    (home-page "https://drumkv1.sourceforge.io")
    (synopsis "Drum-kit sampler synthesizer with stereo effects")
    (description
     "Drumkv1 is an old-school drum-kit sampler synthesizer with stereo
effects.")
    (license license:gpl2+)))

(define-public samplv1
  (package
    (name "samplv1")
    (version "0.9.24")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://sourceforge/samplv1/samplv1/" version
                              "/samplv1-" version ".tar.gz"))
              (sha256
               (base32
                "1c3ksd02dfqvzc3zk4x282b6gxr1l4ya9c4l04dcn55mrymgn2zz"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; there are no tests
    (inputs
     (list jack-1
           lv2
           libsndfile
           alsa-lib
           non-session-manager
           liblo
           qtbase-5))
    (native-inputs
     (list pkg-config qttools))
    (home-page "https://samplv1.sourceforge.io")
    (synopsis "Polyphonic sampler synthesizer with stereo effects")
    (description
     "Samplv1 is an old-school polyphonic sampler synthesizer with stereo
effects.")
    (license license:gpl2+)))

(define-public padthv1
  (package
    (name "padthv1")
    (version "0.9.24")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://sourceforge/padthv1/padthv1/" version
                              "/padthv1-" version ".tar.gz"))
              (sha256
               (base32
                "0p3qp2lzhmj8i17bcqzsi0782nz7s90hx3s8vax6bxl4mqxsvnxb"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; there are no tests
    (inputs
     (list jack-1
           lv2
           alsa-lib
           non-session-manager
           liblo
           fftwf
           qtbase-5))
    (native-inputs
     (list pkg-config qttools))
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
    (version "1.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/amsynth/amsynth/releases/"
                           "download/release-" version
                           "/amsynth-" version ".tar.gz"))
       (sha256
        (base32 "1fabxx4nv5rsi4kjqachzh2r6zb40lszbv09gcz41ql8qslrw842"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-file-names
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "src/GUI/editor_pane.c"
                     (("/usr/bin/unzip")
                      (search-input-file inputs "bin/unzip")))
                   (substitute* "src/GUI/MainMenu.cpp"
                     (("/usr/bin/which")
                      (search-input-file inputs "bin/which"))))))))
    (inputs
     (list alsa-lib
           gtk+-2
           jack-1
           libsndfile
           lv2
           ;; External commands invoked at run time.
           unzip
           which))
    (propagated-inputs
     ;; avoid runtime error:
     ;; GLib-GIO-ERROR **: 22:14:48.344: Settings schema
     ;;   'org.gnome.desktop.interface' is not installed
     (list gsettings-desktop-schemas))
    (native-inputs
     (list intltool pkg-config))
    (home-page "https://amsynth.github.io")
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
    (version "0.8.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pantherb/setBfree")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1s3ps3cjwlm63ml4izb7mizy8ci5pl9a19lvz62xf0br089h3afc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no "check" target
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
     (list jack-1
           lv2
           zita-convolver
           glu
           ftgl
           font-bitstream-vera))
    (native-inputs
     (list help2man pkg-config))
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
     (list alsa-lib jack-1 liblo libx11))
    (native-inputs
     (list pkg-config))
    (home-page "http://bristol.sourceforge.net/")
    (synopsis "Synthesizer emulator")
    (description
     "Bristol is an emulation package for a number of different @code{classic}
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
    (version "1.5.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/tuxguitar/TuxGuitar/TuxGuitar-"
                    version "/tuxguitar-" version "-src.tar.gz"))
              (sha256
               (base32
                "0fjhf56lhlhm84v08917xp4yw8y6d0qajm4qiy1gfp8dm74whwwg"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "build"
       #:jdk ,icedtea-8
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((initial-classpath (getenv "CLASSPATH"))
                    (build-dir (lambda (dir)
                                 (setenv
                                  "CLASSPATH"
                                  (string-join (cons initial-classpath
                                                     (find-files (getcwd) "\\.jar$"))
                                               ":"))
                                 (with-directory-excursion dir
                                   (if (file-exists? "build.xml")
                                       ((assoc-ref %standard-phases 'build)
                                        #:build-target "build")
                                       (begin
                                         ;; Generate default build.xml.
                                         ((@@ (guix build ant-build-system)
                                              default-build.xml)
                                          (string-append (string-downcase dir) ".jar")
                                          (string-append (assoc-ref outputs "out")
                                                         "/share/java"))
                                         ((assoc-ref %standard-phases 'build))))))))
               (map build-dir '("TuxGuitar-lib"
                                "TuxGuitar-editor-utils"
                                "TuxGuitar-ui-toolkit"
                                "TuxGuitar-ui-toolkit-swt"
                                "TuxGuitar-viewer"
                                "TuxGuitar"
                                "TuxGuitar-gm-utils"
                                "TuxGuitar-alsa"
                                "TuxGuitar-midi"
                                "TuxGuitar-midi-ui"))
               #t)))
         (add-after 'build 'build-jni
           (lambda _
             (setenv "CC" "gcc")
             (setenv "CFLAGS" (string-append
                               "-fpic -I"
                               (getcwd)
                               "/build-scripts/native-modules/common-include"))
             (invoke "make" "-C" "./TuxGuitar-alsa/jni" "-f" "GNUmakefile")))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (share (string-append out "/share"))
                    (jni-lib (string-append out "/lib"))
                    (lib   (string-append share "/java"))
                    (swt   (assoc-ref inputs "java-swt"))
                    (mime  (string-append share "/mime/packages"))
                    (app   (string-append share "/applications"))
                    (man   (string-append share "/man/man1")))

               (mkdir-p bin)
               ;; Install all jars.
               (for-each (lambda (file)
                           (install-file file lib))
                         (find-files "." "\\.jar$"))

               ;; Install jni libraries
               (for-each (lambda (file)
                           (install-file file jni-lib))
                         (find-files "." "\\-jni.so$"))

               ;; Install all resources.
               (copy-recursively "./TuxGuitar/share" share)

               ;; Install desktop and mime files
               (install-file "./misc/tuxguitar.xml" mime)
               (install-file "./misc/tuxguitar.desktop" app)

               ;; Install manaual
               (install-file "./misc/tuxguitar.1" man)

               ;; Create wrapper.
               (call-with-output-file (string-append bin "/tuxguitar")
                 (lambda (port)
                   (let ((classpath (string-join (append  (find-files lib "\\.jar$")
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
                                     " -Djava.library.path=" out "/lib"
                                     " org.herac.tuxguitar.app.TGMainSingleton"
                                     " \"$1\" \"$2\"")))))
               (chmod (string-append bin "/tuxguitar") #o555)
               #t))))))
    (inputs
     (list alsa-lib java-swt))
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
    (version "0.51-4")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "http://msp.ucsd.edu/Software/pd-"
                              version ".src.tar.gz"))
              (sha256
               (base32
                "1hgw1ciwr59f4f9s0h7c2l36wcsn3jsddhr1r9qj97vf64c1ynaj"))))
    (build-system gnu-build-system)
    (arguments
     (let ((wish (string-append "wish" (version-major+minor
                                        (package-version tk)))))
       `(#:tests? #f                    ; no "check" target
         #:configure-flags
         (list
          "--enable-jack"
          (string-append "--with-wish=" (string-append
                                         (assoc-ref %build-inputs "tk")
                                         "/bin/" ,wish)))
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'fix-with-path
             (lambda _
               (substitute* "tcl/pd-gui.tcl"
                 (("exec wish ") (string-append "exec " (which ,wish) " ")))
               #t))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list tk alsa-lib jack-1))
    (home-page "https://puredata.info")
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
                    (url "https://github.com/libpd/libpd")
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
                 (symlink "libportmidi.so" "libporttime.so")))))
         (add-after 'install 'install-pkg-config
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (pkg-config-dir (string-append out "/lib/pkgconfig")))
               (mkdir-p pkg-config-dir)
               (with-output-to-file (string-append pkg-config-dir "/portmidi.pc")
                 (lambda _
                   (format #t
                           "prefix=~@*~a~@
                           libdir=${prefix}/lib~@
                           includedir=${prefix}/include~@

                           Name: portmidi~@
                           Description:~@
                           Version: ~a~@
                           Libs: -L${libdir} -lportmidi~@
                           Cflags: -I${includedir}~%"
                           out ,version)))))))))
    (inputs
     (list alsa-lib))
    (native-inputs
     (list unzip))
    (home-page "http://portmedia.sourceforge.net/portmidi/")
    (synopsis "Library for MIDI I/O")
    (description
     "PortMidi is a library supporting real-time input and output of MIDI data
using a system-independent interface.")
    (license license:expat)))

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
     (list portmidi alsa-lib))
    (native-inputs
     (list python-cython unzip))
    (home-page "http://portmedia.sourceforge.net/portmidi/")
    (synopsis "Python bindings to PortMidi")
    (description
     "This package provides Python bindings to the PortMidi library.")
    (license license:expat)))

(define-public frescobaldi
  (package
    (name "frescobaldi")
    (version "3.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/wbsoft/frescobaldi/releases/download/v"
             version "/frescobaldi-" version ".tar.gz"))
       (sha256
        (base32 "1hg9yc8kj445fjsby92g3qf50crcl1pb079zfma18sb7ycv50zww"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;no tests included
    (inputs
     (list lilypond
           poppler
           portmidi
           python-ly
           python-poppler-qt5
           python-pyportmidi
           python-pyqt
           python-sip))
    (home-page "https://www.frescobaldi.org/")
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
    (version "2.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/drumstick/"
                                  version "/drumstick-" version ".tar.bz2"))
              (sha256
               (base32
                "1rs248pkgn6d29nkvw9ab6dvi1vsz220jdmz1ddzr29cpyc0adfh"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no test target
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
     (list qtbase-5 qtsvg qttools alsa-lib))
    (native-inputs
     (list pkg-config
           libxslt ; for xsltproc
           docbook-xsl
           doxygen
           graphviz)) ; for dot
    (home-page "https://drumstick.sourceforge.io/")
    (synopsis "C++ MIDI library")
    (description
     "Drumstick is a set of MIDI libraries using C++/Qt5 idioms and style.  It
includes a C++ wrapper around the ALSA library sequencer interface.  A
complementary library provides classes for processing SMF (Standard MIDI
files: .MID/.KAR), Cakewalk (.WRK), and Overture (.OVE) file formats.  A
multiplatform realtime MIDI I/O library is also provided with various output
backends, including ALSA, OSS, Network and FluidSynth.")
    (license license:gpl2+)))

(define-public vmpk
  (package
    (name "vmpk")
    (version "0.8.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/vmpk/vmpk/"
                                  version "/vmpk-" version ".tar.bz2"))
              (sha256
               (base32
                "0kh8pns9pla9c47y2nwckjpiihczg6rpg96aignsdsd7vkql69s9"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f  ; no test target
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
     (list drumstick qtbase-5 qtsvg qtx11extras))
    (native-inputs
     (list libxslt ;for xsltproc
           docbook-xsl qttools pkg-config))
    (home-page "https://vmpk.sourceforge.io/")
    (synopsis "Virtual MIDI piano keyboard")
    (description
     "Virtual MIDI Piano Keyboard is a MIDI events generator and receiver.  It
doesn't produce any sound by itself, but can be used to drive a MIDI
synthesizer (either hardware or software, internal or external).  You can use
the computer's keyboard to play MIDI notes, and also the mouse.  You can use
the Virtual MIDI Piano Keyboard to display the played MIDI notes from another
instrument or MIDI file player.")
    (license license:gpl3+)))

(define-public zynaddsubfx
  (package
    (name "zynaddsubfx")
    (version "3.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/zynaddsubfx/zynaddsubfx/"
                    version "/zynaddsubfx-" version ".tar.bz2"))
              (sha256
               (base32
                "0qwzg14h043rmyf9jqdylxhyfy4sl0vsr0gjql51wjhid0i34ivl"))))
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
     (list liblo
           ntk
           mesa
           alsa-lib
           jack-1
           fftw
           minixml
           libxpm
           zlib))
    (native-inputs
     (list pkg-config))
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
    (version "1.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/yoshimi/"
                           (version-major+minor version)
                           "/yoshimi-" version ".tar.bz2"))
       (sha256
        (base32 "0lxfqj4p4njww3n0wa6yfj38zfls16y3wszd47gvc5asmqyg5vjd"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
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
     (list pkg-config))
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
    (version "4.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.linuxsampler.org/packages/"
                                  "libgig-" version ".tar.bz2"))
              (sha256
               (base32
                "1zs5yy124bymfyapsnljr6rv2lnn5inwchm0xnwiw44b2d39l8hn"))))
    (build-system gnu-build-system)
    (inputs
     (list `(,util-linux "lib") libsndfile))
    (native-inputs
     (list pkg-config))
    (home-page "https://linuxsampler.org/libgig/")
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
     (list jack-1 lash gtk+-2))
    (native-inputs
     (list pkg-config))
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
                    (url "https://github.com/kmatheussen/jack_capture")
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
     (list pkg-config which))
    (inputs
     (list gtk+-2
           jack-1
           libogg
           liblo
           lame
           libsndfile))
    (home-page "https://github.com/kmatheussen/jack_capture")
    (synopsis "Program for recording sound files with JACK")
    (description "This is a program for recording sound files with JACK.  It
can connect to any JACK port and record the output into a stereo WAV file.")
    (license license:gpl2+)))

(define-public jack-select
  (package
    (name "jack-select")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jack-select" version))
              (sha256
               (base32
                "1zijk9ly2fczxsnnrqr8s0ajmlyx1j1vd8gk0rm5dj5zyhhmia7f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build python-build-system))
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f                      ; there are none
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             ;; python-dbus cannot be found but it's really there.  See
             ;; https://github.com/SpotlightKid/jack-select/issues/2
             (substitute* "setup.py"
               (("'dbus-python',") ""))
             ;; Fix reference to dlopened libraries.
             (substitute* "jackselect/alsainfo.py"
               (("libasound.so.2")
                (search-input-file inputs "/lib/libasound.so.2")))))
         (replace 'build
           (assoc-ref python:%standard-phases 'build))
         (add-after 'install 'wrap
           (assoc-ref python:%standard-phases 'wrap)))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("python" ,python-wrapper)
       ("python-pyudev" ,python-pyudev)
       ("python-pyxdg" ,python-pyxdg)
       ("python-dbus" ,python-dbus)
       ("python-pygobject" ,python-pygobject)))
    (home-page "https://github.com/SpotlightKid/jack-select")
    (synopsis "Systray application to quickly change the JACK-DBus configuration")
    (description "This application displays an icon in the system tray (also
known as notification area) of your desktop, which shows the status of the
JACK audio server and when you click on it, a menu pops up, which lets you
quickly select from the JACK configuration presets you created with QjackCtl.
When you select a preset, its JACK engine and driver configuration settings
are loaded via DBus into JACK and then the server is restarted.  This allows
you to switch between different audio setups with just two mouse clicks.")
    (license license:expat)))

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
    (native-inputs (list pkg-config))
    ;; TODO: See https://github.com/iyoko/cursynth/issues/4 which currently
    ;; prevents us from using pulseaudio
    (inputs (list ncurses alsa-lib))
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
    (version "0.9.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/aj-snapshot/"
                                  "aj-snapshot-" version ".tar.bz2"))
              (sha256
               (base32
                "0z8wd5yvxdmw1h1rj6km9h01xd4xmp4d86gczlix7hsc7zrf0wil"))))
    (build-system gnu-build-system)
    (inputs
     (list minixml jack-1 alsa-lib))
    (native-inputs
     (list pkg-config))
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
    (version "0.9.25")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://downloads.sourceforge.net/qtractor/"
                                  "qtractor-" version ".tar.gz"))
              (sha256
               (base32
                "0x99yjabznr2pl867j3h9zdsjzkxzxm2c0xk1xwk50gfp8gwg9bh"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; no "check" target
    (inputs
     `(("qt" ,qtbase-5)
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
     (list pkg-config qttools))
    (home-page "https://qtractor.org/")
    (synopsis "Audio/MIDI multi-track sequencer")
    (description
     "Qtractor is an Audio/MIDI multi-track sequencer application.  It uses
JACK for audio and ALSA sequencer for MIDI as multimedia infrastructures and
follows a traditional multi-track tape recorder control paradigm.")
    (license license:gpl2+)))

(define-public ams-lv2
  (package
    (name "ams-lv2")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blablack/ams-lv2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lz2mvk4gqsyf92yxd3aaldx0d0qi28h4rnnvsaz4ls0ccqm80nk"))))
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
       #:tests? #f))                    ; no tests
    (inputs
     `(("cairo" ,cairo)
       ("fftw" ,fftw)
       ("gtk" ,gtk+-2)
       ("gtkmm" ,gtkmm-2)
       ("lv2" ,lv2)
       ("lvtk" ,lvtk)))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/blablack/ams-lv2")
    (synopsis "Port of Alsa Modular Synth internal modules into LV2")
    (description "This set of LV2 plugins is a port of the internal modules
found in Alsa Modular Synth.  These plugins are used to create modular
synthesizers and contain: VCO, VCF, VCA, LFO, slew limiter, envelopes, sample
and hold, etc.")
    (license license:gpl2)))

(define-public gxtuner
  (package
    (name "gxtuner")
    (version "2.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/brummer10/gxtuner")
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
         (delete 'configure)
         (add-before 'build 'target-specific-glib-API-version
           (lambda _
             ;; See https://github.com/brummer10/gxtuner/pull/21
             (setenv "CFLAGS"
                     "-DGLIB_VERSION_MIN_REQUIRED=GLIB_VERSION_2_54\
 -DGLIB_VERSION_MAX_ALLOWED=GLIB_VERSION_2_54"))))))
    (inputs
     (list gtk+ jack-1 fftwf cairo zita-resampler))
    (native-inputs
     (list pkg-config))
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
       (list lilv
             fftw
             fftwf
             lv2
             jack-1
             readline))
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
                      (url "https://github.com/chronitis/curseradio")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "11bf0jnj8h2fxhpdp498189r4s6b47vy4wripv0z4nx7lxajl88i"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'link-to-mpv
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "curseradio/curseradio.py"
               (("/usr/bin/mpv")
                (search-input-file inputs "/bin/mpv"))))))))
    (propagated-inputs
     (list python-lxml python-requests python-pyxdg))
    (inputs
     (list mpv))
    (home-page "https://github.com/chronitis/curseradio")
    (synopsis "Command-line Internet radio player")
    (description "Curseradio is a Curses-based radio player that uses a
tune-in sender list from @url{http://opml.radiotime.com}.")
    (license license:expat))))

(define-public pianobar
  (package
    (name "pianobar")
    (version "2020.11.28")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PromyLOPh/pianobar")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13qx52a1yj2wghf7yd9jf4ar92scbc8zgqdq0kkqf4p9isf1phf3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags (list "CC=gcc" (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (inputs
     (list ao curl libgcrypt json-c ffmpeg))
    (native-inputs
     (list pkg-config))
    (home-page "https://6xq.net/pianobar/")
    (synopsis "Console-based pandora.com player")
    (description "pianobar is a console-based music player for the
personalized online radio pandora.com.  It has configurable keys for playing
and managing stations, can be controlled remotely via fifo, and can run
event-based scripts for scrobbling, notifications, etc.")
    (license license:expat)))

(define-public picard
  (package
    (name "picard")
    (version "2.4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://musicbrainz.osuosl.org/pub/musicbrainz/"
                    "picard/picard-" version ".tar.gz"))
              (sha256
               (base32
                "1c5l7i43jaj3s4wklc0cba6nn2x9cmpcggk4q4h9m1bci2xilsiy"))
              (patches (search-patches "picard-fix-id3-rename-test.patch"))))
    (build-system python-build-system)
    (arguments
     '(#:use-setuptools? #f
       #:configure-flags
       (list "--root=/"
             ;; Don't phone home or show ‘Check for Update…’ in the Help menu.
             "--disable-autoupdate")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "picard/const/__init__.py"
               (("pyfpcalc")
                (string-append
                 "pyfpcalc', '"
                 (assoc-ref inputs "chromaprint") "/bin/fpcalc")))
             #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("python-dateutil" ,python-dateutil)))
    (inputs
     (list chromaprint python-discid python-pyqt python-mutagen))
    (home-page "https://picard.musicbrainz.org/")
    (synopsis "Graphical music tagging application")
    (description
     "MusicBrainz Picard is a music tagging application, supporting multiple
formats, looking up tracks through metadata and audio fingerprints.")
    (license license:gpl2+)))

(define-public python-mutagen
  (package
    (name "python-mutagen")
    (version "1.45.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "mutagen" version))
              (sha256
               (base32
                "1qdk6i8gyhbi1c4j5jmbfpac3q8sff2ysri1pnp7nb9wzcp615v3"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest python-hypothesis python-flake8))
    (home-page "https://mutagen.readthedocs.io/")
    (synopsis "Read and write audio tags")
    (description "Mutagen is a Python module to handle audio metadata.  It
supports ASF, FLAC, M4A, Monkey’s Audio, MP3, Musepack, Ogg FLAC, Ogg Speex, Ogg
Theora, Ogg Vorbis, True Audio, WavPack and OptimFROG audio files.  All versions
of ID3v2 are supported, and all standard ID3v2.4 frames are parsed.  It can read
Xing headers to accurately calculate the bitrate and length of MP3s.  ID3 and
APEv2 tags can be edited regardless of audio format.  It can also manipulate Ogg
streams on an individual packet/page level.")
    (license license:gpl2))) ; "later version" never mentioned

(define-public python-mediafile
  (package
    (name "python-mediafile")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mediafile" version))
       (sha256
        (base32
         "0ipb001j19s9wvssmrj8wz0nrkbl0k3zr3dgzyp1bd9cjc6vklnp"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-mutagen python-six))
    (home-page "https://github.com/beetbox/mediafile")
    (synopsis "Read and write audio file tags")
    (description
     "MediaFile is a simple interface to the metadata tags for many audio file
formats.  It wraps Mutagen, a high-quality library for low-level tag
manipulation, with a high-level, format-independent interface for a common set
of tags.")
    (license license:expat)))

(define-public python-musicbrainzngs
  (package
    (name "python-musicbrainzngs")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "musicbrainzngs" version))
              (sha256
               (base32
                "09z6k07pxncfgfc8clfmmxl2xqbd7h8x8bjzwr95hc0bzl00275b"))))
    (build-system python-build-system)
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
     (list python-discid python-musicbrainzngs))
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
    (version "4.2.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pylast" version))
              (sha256
               (base32
                "0pzzhr4mlwpvfhy9gzq86ppz29fmf5z0w3xkl5if1fm59r1afms7"))))
    (build-system python-build-system)
    ;; Tests require network access.  See
    ;; https://github.com/pylast/pylast/issues/105
    (arguments '(#:tests? #f))
    (native-inputs
     (list python-coverage python-pytest python-flaky python-pyyaml
           python-setuptools-scm))
    (home-page "https://github.com/pylast/pylast")
    (synopsis "Python interface to Last.fm and Libre.fm")
    (description "A Python interface to Last.fm and other API-compatible
websites such as Libre.fm.")
    (license license:asl2.0)))

(define-public instantmusic
  (let ((commit "300891d09c703525215fa5a116b9294af1c923c8")
        (revision "1"))
    (package
      (name "instantmusic")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/yask123/Instant-Music-Downloader")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0j7qivaa04bpdz3anmgci5833dgiyfqqwq9fdrpl9m68b34gl773"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-requests eyed3 python-beautifulsoup4 youtube-dl))
    (arguments
     '(#:modules ((guix build python-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'change-directory
                    (lambda _
                      (chdir "instantmusic-0.1") #t))
                  (add-before 'install 'fix-file-permissions
                    (lambda _
                      ;; Fix some read-only files that would cause a build failure
                      (for-each (cut chmod <> #o644)
                                (find-files "instantmusic.egg-info"
                                            "PKG-INFO|.*\\.txt"))
                      #t)))))
    (home-page "https://github.com/yask123/Instant-Music-Downloader")
    (synopsis "Command-line program to download a song from YouTube")
    (description "InstantMusic downloads a song from YouTube in MP3 format.
    Songs can be searched by artist, name or even by a part of the song text.")
    (license license:expat))))

(define-public beets
  (package
    (name "beets")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "beets" version))
              (sha256
               (base32
                "0arl4nc3y8iwa331hf6ggai19y8ns9pl03g5d6ac857wq2x7nzw8"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-HOME
           (lambda _
             (setenv "HOME" (string-append (getcwd) "/tmp"))
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-v" "test"))))
         ;; Wrap the executable, so it can find python-gi (aka
         ;; pygobject) and gstreamer plugins.
         (add-after 'wrap 'wrap-typelib
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((prog (string-append (assoc-ref outputs "out")
                                        "/bin/beet"))
                   (plugins (getenv "GST_PLUGIN_SYSTEM_PATH"))
                   (types (getenv "GI_TYPELIB_PATH")))
               (wrap-program prog
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,plugins))
                 `("GI_TYPELIB_PATH" ":" prefix (,types)))
               #t))))))
    (native-inputs
     (list gobject-introspection
           python-flask
           python-mock
           python-py7zr
           python-pytest-6
           python-responses))
    (inputs
     (list bash-minimal
           gst-plugins-base
           gst-plugins-good
           gstreamer
           python-confuse
           python-jellyfish
           python-mediafile
           python-munkres
           python-musicbrainzngs
           python-pyyaml
           python-six
           python-unidecode
           ;; Optional dependencies for plugins. Some of these are also required by tests.
           python-beautifulsoup4 ; For lyrics.
           python-discogs-client ; For discogs.
           python-mpd2 ; For mpdstats.
           python-mutagen ; For scrub.
           python-langdetect ; For lyrics.
           python-pillow ; For fetchart, embedart, thumbnails.
           python-pyacoustid ; For chroma.
           python-pygobject ; For bpd, replaygain.
           python-pylast ; For lastgenre, lastimport.
           python-pyxdg ; For thumbnails.
           python-rarfile ; For import.
           python-reflink ; For reflink.
           python-requests
           python-requests-oauthlib)) ; For beatport.
    (home-page "https://beets.io")
    (synopsis "Music organizer")
    (description "The purpose of beets is to get your music collection
right once and for all.  It catalogs your collection, automatically
improving its metadata as it goes using the MusicBrainz database.
Then it provides a variety of tools for manipulating and accessing
your music.")
    (license license:expat)))

(define-public beets-next
  (deprecated-package "beets-next" beets))

(define-public beets-bandcamp
  (package
    (name "beets-bandcamp")
    (version "0.1.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "beets-bandcamp" version))
              (sha256
               (base32
                "0dwbdkrb9c0ppzm5s78h47ndpr88cw1k0z8fgfhkl706wazx2ddg"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))          ; there are no tests
    (propagated-inputs
     (list beets
           python-beautifulsoup4
           python-confuse
           python-isodate
           python-jellyfish
           python-mediafile
           python-munkres
           python-musicbrainzngs
           python-requests
           python-six
           python-unidecode))
    (home-page "https://github.com/unrblt/beets-bandcamp")
    (synopsis "Bandcamp plugin for beets")
    (description
     "This plugin for beets automatically obtains tag data from @uref{Bandcamp,
https://bandcamp.com/}.  It's also capable of getting song lyrics and album art
using the beets FetchArt plugin.")
    (license license:gpl2)))

(define-public milkytracker
  (package
    (name "milkytracker")
    (version "1.03.00")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/milkytracker/MilkyTracker")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "025fj34gq2kmkpwcswcyx7wdxb89vm944dh685zi4bxx0hz16vvk"))
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
     (list pkg-config))
    (synopsis "Music tracker for working with .MOD/.XM module files")
    (description "MilkyTracker is a music application for creating .MOD and .XM
    module files.  It attempts to recreate the module replay and user experience of
    the popular DOS program Fasttracker II, with special playback modes available
    for improved Amiga ProTracker 2/3 compatibility.")
    (home-page "https://milkytracker.titandemo.org/")
    ;; 'src/milkyplay' is under Modified BSD, the rest is under GPL3 or later.
    (license (list license:bsd-3 license:gpl3+))))

(define-public schismtracker
  (package
    (name "schismtracker")
    (version "20190805")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/schismtracker/schismtracker")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qqps20vvn3rgpg8174bjrrm38gqcci2z5z4c1r1vhbccclahgsd"))
              (modules '((guix build utils)))
              (snippet
               ;; Remove use of __DATE__ and __TIME__ for reproducibility.
               `(begin
                  (substitute* "schism/version.c"
                    (("Schism Tracker built %s %s.*$")
                     (string-append "Schism Tracker version " ,version "\") ;")))
              #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'link-libm
           (lambda _ (setenv "LIBS" "-lm") #t)))))
    (native-inputs
     (list autoconf automake python))
    (inputs
     (list alsa-lib ; for asound dependency
           libx11 libxext sdl))
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
    (version "1.7.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://essej.net/sooperlooper/sooperlooper-"
                           version ".tar.gz"))
       (sha256
        (base32 "0kbb1pj62rl32c88j6p7dg823kvs0gb5s42qy3bl6yg0wn10dksj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
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
                                      cwd "/libs/midi++:"
                                      (or (getenv "CPATH") ""))))
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
       ("libsigc++" ,libsigc++-2)
       ("ncurses" ,ncurses)))
    (native-inputs
     (list pkg-config))
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
     (list alsa-lib
           curl
           faad2
           ffmpeg-3.4
           file
           jack-1
           libid3tag
           libltdl
           libmodplug
           libmpcdec
           libmad
           libogg
           libvorbis
           ncurses
           openssl
           cyrus-sasl
           speex
           taglib
           wavpack
           zlib))
    (native-inputs
     (list pkg-config))
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
                    (url "https://github.com/brummer10/GxGuvnor.lv2")
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
     (list lv2))
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
       (list lv2 gtk+-2))
      (native-inputs
       (list pkg-config))
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
                    (url "https://github.com/brummer10/GxSuppaToneBender.lv2")
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
                    (url "https://github.com/brummer10/GxHyperion.lv2")
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
                    (url "https://github.com/brummer10/GxVoodoFuzz.lv2")
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
                    (url "https://github.com/brummer10/GxSuperFuzz.lv2")
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
                    (url "https://github.com/brummer10/GxVintageFuzzMaster.lv2")
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
  ;; This commit corresponds to the beta_3 tag
  (let ((commit "7edcb4e29a358623bfd57fa2c27e5da60adfcec3")
        (revision "2"))
    (package
      (name "rkrlv2")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ssj71/rkrlv2")
                      (commit commit)))
                (sha256
                 (base32
                  "16i4ajrib7kb0abdcn4901g8a4lkwkp2fyqyms38dhqq84slyfjs"))
                (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments '(#:tests? #f)) ; there are no tests
      (inputs
       (list fftw libsamplerate lv2))
      (native-inputs
       (list pkg-config))
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
                      (url "https://github.com/moddevices/mod-utilities")
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
       (list lv2))
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
       (list boost
             python-rdflib
             python
             jack-1
             lv2
             lilv
             raul-devel
             ganv
             suil
             serd
             sord
             sratom
             gtkmm-2))
      (native-inputs
       (list pkg-config python-pygments))
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
       (list "--enable-qt5")))
    (inputs
     (list qtbase-5 alsa-lib jack-1 liblo lv2))
    (native-inputs
     (list pkg-config qttools))
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
       (list "--enable-qt5")))
    (inputs
     (list qtbase-5 alsa-lib))
    (native-inputs
     (list pkg-config qttools))
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
    (inputs
     (list gtkmm-2 alsa-lib jack-1 lash))
    (native-inputs
     (list pkg-config))
    (home-page "https://edge.launchpad.net/seq24/")
    (synopsis "Real-time MIDI sequencer")
    (description "Seq24 is a real-time MIDI sequencer.  It was created to
provide a very simple interface for editing and playing MIDI loops.")
    (license license:gpl2+)))

(define-public python-discogs-client
  (package
    (name "python-discogs-client")
    (version "2.3.12")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "python3-discogs-client" version))
              (sha256
               (base32
                "1zmib0i9jicv9fyphgkcrk418qmpv3l4p38ibl31sh237ki5xqw9"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-dateutil python-oauthlib python-requests))
    (home-page "https://github.com/joalla/discogs_client")
    (synopsis "Python client for the Discogs API")
    (description "This is the continuation of the official Discogs API
client for Python. It enables you to query the Discogs database for
information on artists, releases, labels, users, Marketplace listings,
and more.  It also supports OAuth 1.0a authorization, which allows you to
change user data such as profile information, collections and wantlists,
inventory, and orders.")
    (license license:bsd-2)))

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
     (list readline glib))
    (native-inputs
     (list doxygen pkg-config))
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
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/LMMS/lmms")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "11xgf461cnmq0jkgdgx5bddi87ammpik4whg1m4fcvd3i0d5i601"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; no tests
       ;; Qt 5 support must be explicitly enabled in the 1.2 stable versions of
       ;; LMMS, so try removing "-DWANT_QT5=ON" in later versions.
       ;; Also, explicitly disabling VST support gets rid of the in-tree
       ;; dependency on qt5-x11embed.
       #:configure-flags '("-DWANT_QT5=ON"
                           "-DWANT_VST=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-rpmalloc
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "rpmalloc")
                               "src/3rdparty/rpmalloc/rpmalloc")
             #t))
         (add-before 'configure 'set-ldflags
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "LDFLAGS"
                     (string-append
                      "-Wl,-rpath=\""
                      (assoc-ref outputs "out") "/lib/lmms"
                      ":"
                      (assoc-ref outputs "out") "/lib/lmms/ladspa"
                      "\"")))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)
       ;; rpmalloc is a public domain memory allocator. This version specified
       ;; below is the version required by LMMS.
       ;; To get the new commit of rpmalloc to use here, run
       ;;   `git submodule--helper list | grep rpmalloc | cut -f2 -d' '`
       ;; in the cloned LMMS repository.
       ("rpmalloc"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/mjansson/rpmalloc")
                 (commit "b5bdc18051bb74a22f0bde4bcc90b01cf590b496")))
           (sha256
            (base32
             "0g9pls46iggg7rdm65vzfj8nyr3v2n5xkp54c4qbh9hhalpsw4ay"))))))
    (inputs
     `(("sdl" ,sdl)
       ("qtbase" ,qtbase-5)
       ("qtx11extras" ,qtx11extras)
       ("fltk" ,fltk)
       ("libogg" ,libogg)
       ("libsamplerate" ,libsamplerate)
       ("fluidsynth" ,fluidsynth)
       ("libvorbis" ,libvorbis)
       ("alsa-lib" ,alsa-lib)
       ("portaudio" ,portaudio)
       ("ladspa" ,ladspa)
       ("libsndfile1" ,libsndfile)
       ("libxft" ,libxft)
       ("freetype2" ,freetype)
       ("fftw3f" ,fftwf)
       ("jack" ,jack-1)
       ("carla" ,carla)))
    (home-page "https://lmms.io/")
    (synopsis "Music composition tool")
    (description "LMMS is a digital audio workstation.  It includes tools for sequencing
melodies and beats and for mixing and arranging songs.  LMMS includes instruments based on
audio samples and various soft sythesizers.  It can receive input from a MIDI keyboard.")
    (license license:gpl2+)))

(define-public liquidsfz
  (package
    (name "liquidsfz")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://space.twc.de/~stefan/liquidsfz/"
                                  "liquidsfz-" version ".tar.bz2"))
              (sha256
               (base32
                "1hb4hc3gkvjfbx0ls6wxzavhv2hf9ix11cz8yvndyb6q9lwkimwl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-shared")))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("jack" ,jack-2)
       ("lv2" ,lv2)
       ("readline" ,readline)
       ("libsndfile" ,libsndfile)))
    (home-page "https://github.com/swesterfeld/liquidsfz")
    (synopsis "Sampler library")
    (description "The main goal of liquidsfz is to provide an SFZ sampler
implementation library that is easy to integrate into other projects.  A
standalone JACK client and an LV2 plugin is also available.")
    (license license:lgpl2.1+)))

(define-public sfizz
  (package
    (name "sfizz")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/sfztools/sfizz"
                                  "/releases/download/" version
                                  "/sfizz-" version ".tar.gz"))
              (sha256
               (base32
                "1pk67xvyqkvhjz2q5hbj5v0mnfvdvvl8vl5bsh6ymwiq3glkd41l"))
              (modules '((guix build utils)))
              (snippet
               ;; TODO: pugixml is bundled, but can only be removed in
               ;; versions after 1.0.0.
               '(for-each delete-file-recursively
                          '("external/abseil-cpp"
                            "external/simde"
                            "plugins/editor/external/vstgui4"
                            "plugins/vst")))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DSFIZZ_LV2_UI=OFF"
             "-DSFIZZ_VST=OFF"
             "-DSFIZZ_VST2=OFF"
             "-DSFIZZ_TESTS=ON"
             "-DSFIZZ_USE_SYSTEM_ABSEIL=ON")))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("abseil-cpp" ,abseil-cpp)
       ("glib" ,glib)
       ("jack" ,jack-2)
       ("lv2" ,lv2)
       ("libsamplerate" ,libsamplerate)
       ("pugixml" ,pugixml)
       ("simde" ,simde)))
    (home-page "https://sfz.tools/sfizz/")
    (synopsis "SFZ parser and synth library")
    (description "Sfizz provides an SFZ parser and synth C++ library.  It
includes LV2 plugins and a JACK standalone client.")
    (license license:bsd-2)))

(define-public musescore
  (package
    (name "musescore")
    (version "3.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/musescore/MuseScore")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0szvb6mlzy9df9lrq546rrpixa480knzij1wgh6ilflxz87q048q"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove unused libraries.
        '(begin
           (for-each delete-file-recursively
                     '("thirdparty/freetype"
                       "thirdparty/openssl"
                       "thirdparty/portmidi"
                       "thirdparty/qt-google-analytics"))))))
    (build-system qt-build-system)
    (arguments
     `(#:configure-flags
       `("-DBUILD_TELEMETRY_MODULE=OFF" ;don't phone home
         "-DBUILD_WEBENGINE=OFF"
         "-DDOWNLOAD_SOUNDFONT=OFF"
         "-DMUSESCORE_BUILD_CONFIG=release"
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
     (list alsa-lib
           freetype
           `(,gtk+ "bin")               ;for gtk-update-icon-cache
           jack-1
           lame
           libogg
           libsndfile
           libvorbis
           portaudio
           portmidi
           pulseaudio
           qtbase-5
           qtdeclarative
           qtgraphicaleffects
           qtquickcontrols2
           qtscript
           qtsvg
           qtxmlpatterns))
    (native-inputs
     (list pkg-config qttools))
    (synopsis "Music composition and notation software")
    (description
     "MuseScore is a music score typesetter.  Its main purpose is the creation
of high-quality engraved musical scores in a WYSIWYG environment.

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
    (version "4.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/muse-sequencer/muse")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gamr9ln10l26wwyin1a4grrqy6h05qzcgp28wsp85yczkpsh02c"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f ; there is no test target
       #:configure-flags
       (list "-DENABLE_VST_NATIVE=OFF"
             (string-append "-DCMAKE_EXE_LINKER_FLAGS="
                            "-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib/muse-"
                            ,(version-major+minor version) "/modules")
             (string-append "-DCMAKE_SHARED_LINKER_FLAGS="
                            "-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib/muse-"
                            ,(version-major+minor version) "/modules"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "src"))))))
    (inputs
     (list alsa-lib
           dssi
           fluidsynth
           glib
           jack-1
           ladspa
           lash
           libinstpatch
           liblo
           libsamplerate
           libsndfile
           lilv
           lrdf
           lv2
           pcre
           pulseaudio ; required by rtaudio
           qtbase-5
           qtsvg
           rtaudio
           rubberband
           sord))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("qttools" ,qttools)))
    (home-page "https://muse-sequencer.github.io/")
    (synopsis "MIDI/Audio sequencer")
    (description "MusE is a MIDI/Audio sequencer with recording and editing
capabilities.  Its audio sequencer supports the LADSPA, DSSI, and LV2 audio
plugin formats; the MIDI sequencer provides a piano roll, a drum editor, a
list view, and a score editor.  MusE aims to be a complete multitrack virtual
studio.")
    (license license:gpl2+)))

(define-public gsequencer
  (package
    (name "gsequencer")
    (version "3.14.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.savannah.gnu.org/git/gsequencer")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18pfv4w30nng1p0vgmrnkfm38522iq1x1bj8iz4qfiffiv56dsnz"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'prepare-x-for-test
           (lambda _
             (system "Xvfb &")
             (setenv "DISPLAY" ":0"))))))
    (native-inputs
     (list autoconf
           automake
           cunit
           gettext-minimal
           gobject-introspection
           gtk-doc
           libtool
           libxslt
           pkg-config
           xorg-server-for-tests))
    (inputs
     (list alsa-lib
           dssi
           fftw
           gst-plugins-base
           gstreamer
           gtk+
           jack-1
           ladspa
           libinstpatch
           libsamplerate
           libsndfile
           libsoup-minimal-2
           libxml2
           lv2
           pulseaudio
           `(,util-linux "lib")
           webkitgtk-with-libsoup2))
    (home-page "https://nongnu.org/gsequencer/")
    (synopsis "Advanced Gtk+ Sequencer")
    (description
     "GSequencer allows you to play, capture and create music.  There is a piano
roll, automation and wave form editor.  It has machines for playing drum samples,
Soundfont2 sound containers and synthesizers.  They usually can be connected to a
MIDI input source (instrument).  It has support for various audio backends like
ALSA, Pulseaudio, JACK, OSSv4 and CoreAudio.")
    (license license:gpl3+)))

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
     (list alsa-lib
           jack-1
           ladspa
           libsamplerate
           libsndfile
           liblo))
    (native-inputs
     (list pkg-config))
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
    (version "21.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/rosegarden/rosegarden/"
                           (version-major+minor version) "/"
                           "rosegarden-" version ".tar.bz2"))
       (sha256
        (base32 "02984qff9rc2r83a5a5zgwawfgd583gnj2w3zvllsxaf0vdx6gnw"))))
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
               ((" -fvisibility=hidden") ""))))
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
                (string-append "\"" (assoc-ref inputs "lilypond") "/bin/lilypond")))))
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
               ;;(("qt5_add_resources[(]rg_SOURCES ../data/data.qrc[)]")
               ;; "qt5_add_resources(rg_SOURCES ../data/data.qrc OPTIONS --format-version 1)")
               )
             ;; Make hashtable traversal order predicable.
             (setenv "QT_RCC_TEST" "1"))) ; important
         (add-before 'check 'prepare-check
           (lambda _
             (setenv "QT_QPA_PLATFORM" "offscreen")
             ;; Tests create files in $HOME/.local/share/rosegarden .
             (mkdir-p "/tmp/foo")
             (setenv "HOME" "/tmp/foo")
             (setenv "XDG_RUNTIME_DIR" "/tmp/foo"))))))
    (inputs
     (list alsa-lib
           bash
           dssi
           flac
           fftwf
           jack-1
           ladspa
           liblo
           libsamplerate
           lilypond
           lrdf
           qtbase-5
           tar
           lirc
           wavpack
           zlib))
    (native-inputs
     (list pkg-config qttools))           ;for qtlinguist
    (synopsis "Music composition and editing environment based around a MIDI
sequencer")
    (description "Rosegarden is a music composition and editing environment
based around a MIDI sequencer that features a rich understanding of music
notation and includes basic support for digital audio.")
    (home-page "https://www.rosegardenmusic.com/")
    (license license:gpl2)))

(define-public patchmatrix
  (package
    (name "patchmatrix")
    (version "0.16.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/OpenMusicKontrollers/patchmatrix")
                    (commit version)))
              (file-name (git-file-name "patchmatrix" version))
              (sha256
               (base32
                "020vp7zzxxzzjfic57vkpg68dm8hi98ilr1bj88xjsv6i47xmjbn"))))
    (build-system meson-build-system)
    (arguments '(#:tests? #f))          ; no test target
    (inputs
     (list jack-1 lv2 mesa))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/OpenMusicKontrollers/patchmatrix")
    (synopsis "Simple JACK patch bay")
    (description "PatchMatrix is a patch bay for the JACK audio connection
kit.  It provides a patch bay in flow matrix style for audio, MIDI, CV, and
OSC connections.")
    (license license:artistic2.0)))

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
     (list boost lv2 ntk))
    (native-inputs
     (list faust pkg-config))
    (home-page "http://openavproductions.com/sorcer/")
    (synopsis "Wavetable LV2 plugin synth")
    (description "Sorcer is a wavetable LV2 plugin synthesizer, targeted at
the electronic or dubstep genre.")
    (license license:gpl3+)))

(define-public sonivox-eas
  (package
    (name "sonivox-eas")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pedrolcl/Linux-SonivoxEas")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1ygmlrsdzxii2dvj6id2ai3xv3klw2x67ip5rcp823jzczl0wpjd"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f)) ; there are no tests
    (inputs
     (list alsa-lib drumstick pulseaudio qtbase-5))
    (native-inputs
     (list pkg-config))
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
     (list dssi
           liblo
           fftwf
           gtk+-2
           ladspa
           alsa-lib))
    (native-inputs
     (list pkg-config))
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
     (list libdiscid))
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
         (add-after 'unpack 'fix-build
           (lambda _
             (substitute* "src/CMakeLists.txt"
               (("\\*.inc") ""))))
         (replace 'check
           (lambda _
             ;; requires network connections
             ;; (invoke "tests/mbtest")
             (invoke "tests/ctest")
             #t)))))
    (inputs (list neon libxml2))
    (native-inputs (list pkg-config))
    (home-page "https://musicbrainz.org/doc/libmusicbrainz")
    (synopsis "MusicBrainz client library")
    (description "The MusicBrainz Client Library (libmusicbrainz), also known as
mb_client, is a development library geared towards developers who wish to add
MusicBrainz lookup capabilities to their applications.")
    (license license:lgpl2.1+)))

(define-public perl-musicbrainz-discid
  (package
    (name "perl-musicbrainz-discid")
    (version "0.06")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/N/NJ/NJH/MusicBrainz-DiscID-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1azc91jnwa3gdmy9pc8mflakgvsvf69ywwlqllxmdzwpk386w2xs"))))
    (build-system perl-build-system)
    (native-inputs (list pkg-config which))
    (inputs (list libdiscid))
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
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/B/BF/BFAIST/WebService-MusicBrainz-"
                    version ".tar.gz"))
              (sha256
               (base32
                "16chs1l58cf000d5kalkyph3p31ci73p1rlyx98mfv10d2cq6fsj"))))
    (build-system perl-build-system)
    (arguments
     ;; Tests try to connect to http://musicbrainz.org.
     '(#:tests? #f))
    (native-inputs
     (list perl-module-build))
    (propagated-inputs
     (list perl-mojolicious))
    (home-page "https://metacpan.org/release/WebService-MusicBrainz")
    (synopsis "Web service API to the MusicBrainz database")
    (description
     "This module searches the MusicBrainz database through their web service
at @code{musicbrainz.org}.")
    (license license:perl-license)))

(define-public clyrics
  (package
    (name "clyrics")
    (version "0.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/trizen/clyrics")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l9iqz6vxrrxapv7s110g360bqxksir4dcqd8w0l4lhmnfmz3vnk"))))
    (build-system trivial-build-system)
    (inputs
     (list bash ; for the wrapped program
           perl
           perl-www-mechanize
           perl-lwp-protocol-https
           ;; Required or else LWP will fail with "GET https://www.google.com/ ==>
           ;; 500 Can't verify SSL peers without knowing which Certificate
           ;; Authorities to trust".
           perl-mozilla-ca))
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
       (list lua
             go-github-com-mattn-go-isatty
             go-github-com-mattn-go-colorable
             go-github-com-aarzilli-golua
             go-gitlab-com-ambrevar-damerau
             go-gitlab-com-ambrevar-golua-unicode
             go-github-com-mgutz-ansi
             go-github-com-michiwend-gomusicbrainz
             go-github-com-stevedonovan-luar
             go-github-com-wtolson-go-taglib
             go-github-com-yookoala-realpath))
      (inputs
       (list chromaprint ffmpeg))
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

(define-public fmit
  (package
    (name "fmit")
    (version "1.2.14")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/gillesdegottex/fmit/")
		    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
	      (sha256
               (base32
                "1q062pfwz2vr9hbfn29fv54ip3jqfd9r99nhpr8w7mn1csy38azx"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
	 (delete 'configure)
	 (add-before 'build 'qmake
	   (lambda _
	     (let ((out (assoc-ref %outputs "out")))
               (invoke "qmake"
                       "fmit.pro"
                       (string-append "PREFIX=" out)
                       (string-append "PREFIXSHORTCUT=" out)
                       "CONFIG+=acs_qt acs_alsa acs_jack acs_portaudio"))))
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/fmit")
                 `("QT_PLUGIN_PATH" ":" prefix
                   ,(map (lambda (label)
                           (string-append (assoc-ref inputs label)
                                          "/lib/qt5/plugins"))
                         '("qtbase" "qtmultimedia" "qtsvg")))
                 `("QML2_IMPORT_PATH" ":" prefix
                   ,(map (lambda (label)
                           (string-append (assoc-ref inputs label)
                                          "/lib/qt5/qml"))
                         '("qtmultimedia"))))
               #t))))))
    (inputs
     (list alsa-lib
           fftw
           jack-1
           portaudio
           qtbase-5
           qtmultimedia
           qtsvg))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("hicolor-icon-theme" ,hicolor-icon-theme)
       ("itstool" ,itstool)
       ("qttools" ,qttools)))
    (synopsis "Musical instrument tuner")
    (description "FMIT is a graphical utility for tuning musical instruments,
with error and volume history, and advanced features.")
    (home-page "https://gillesdegottex.github.io/fmit/")
    ;; Most of the code is under GPL2+, but some abstract or helper classes
    ;; are under LGPL2.1.
    (license (list license:gpl2+ license:lgpl2.1))))

(define-public mloop
  (let ((commit "adebff98b0b4dc5872a03acb82e89c77cb29c127")
        (revision "0"))
    (package
      (name "mloop")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "http://git.fuzzle.org/mloop")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "175gxvg5slq0bllcx1c381rjlq3xpxww8c3kpiw5i2kfr4m52myz"))))
      (build-system waf-build-system)
      (arguments
       `(#:python ,python-2
         #:tests? #f))                     ; no "check" target
      (inputs
       (list jack-1 ncurses))
      (native-inputs
       (list pkg-config))
      (home-page "https://fuzzle.org/~petern/mloop.html")
      (synopsis "Live MIDI looper")
      (description "mloop is a live MIDI looping system, using jack-midi.
Loops are recorded, optionally with beat quantization, and can then be played
back, either once or looping.  A 'note cache' system is implemented to
remember which notes are pressed and their velocities.  This allows for a loop
to start off with the currently pressed notes, making seamless loops much
easier to perform.  Features include:

@itemize
@item Quantisation; end a loop on a beat exactly.
@item Delayed recording; wait for a MIDI event before starting a loop record.
@item Adjust tempo; Playback speed of loops can be adjusted on the fly.
@end itemize\n")
      (license license:gpl2))))

(define-public pragha
  (package
    (name "pragha")
    (version "1.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/pragha-music-player/pragha/"
                                  "releases/download/v" version "/pragha-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "19kbhq99bkimx3aqrdzln0vlr4slkpx6kq66j731jvqyq76nlkp5"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list glib
           grilo
           gstreamer
           gst-plugins-base
           gst-plugins-good
           gtk+
           libcddb
           libcdio
           libcdio-paranoia
           libgudev
           libnotify
           libpeas
           libsoup
           sqlite
           taglib))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH")))
               (wrap-program (string-append out "/bin/pragha")
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path)))
               #t))))))
    (home-page "https://pragha-music-player.github.io")
    (synopsis "Music player")
    (description "Pragha is a lightweight music player based on Gtk and
sqlite.  It is constructed to be fast, light, and simultaneously tries to be
complete without obstructing your daily work.")
    (license license:gpl3+)))

(define-public playerctl
  (package
    (name "playerctl")
    (version "2.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/altdesktop/playerctl")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17hi33sw3663qz5v54bqqil31sgkrlxkb2l5bgqk87pac6x2wnbz"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Dintrospection=false" "-Dgtk-doc=false")))
    (inputs (list python-pygobject))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (synopsis "Control MPRIS-supporting media player applications")
    (description
     "Playerctl is a command-line utility and library for controlling media
players that implement the MPRIS D-Bus Interface Specification.  Playerctl
makes it easy to bind player actions, such as play and pause, to media keys.
You can also get metadata about the playing track such as the artist and title
for integration into status line generators or other command-line tools.")
    (home-page "https://github.com/altdesktop/playerctl")
    (license license:lgpl3+)))

(define-public artyfx
  (package
    (name "artyfx")
    (version "1.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://github.com/openAVproductions/openAV-ArtyFX")
                    (commit (string-append "release-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cxikdnxgjk5gp6kmml4dx2jy2cy4x0c837h7bwraj2pfz0nfgqq"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                                ; no tests included
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-architecture-specific-flags
           (lambda _
             (substitute* "CMakeLists.txt"
               (("-msse2 -mfpmath=sse") ""))
             #t)))))
    (inputs
     (list cairo libsndfile))
    (native-inputs
     (list pkg-config lv2))
    (home-page "http://openavproductions.com/artyfx/")
    (synopsis "Audio effect LV2 plugin bundle")
    (description "ArtyFX is an LV2 plugin bundle of artistic real-time audio
effects.  It contains a bitcrusher, delay, distortion, equalizer, compressor,
and reverb.")
    (license license:gpl2+)))

(define-public lsp-plugins
  (package
    (name "lsp-plugins")
    (version "1.1.26")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/sadko4u/lsp-plugins")
               (commit (string-append "lsp-plugins-" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1apw8zh3a3il4smkjji6bih4vbsymj0hjs10fgkrd4nazqkjvgyd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list
         (string-append "CC=" ,(cc-for-target))
         "BUILD_MODULES=\"lv2 ladspa jack\"" "VST_UI=0"
         (string-append "PREFIX=" (assoc-ref %outputs "out"))
         (string-append "ETC_PATH=" (assoc-ref %outputs "out") "/etc"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))           ; no configure script
       #:test-target "test"))
    (inputs
     (list cairo
           hicolor-icon-theme
           jack-1
           ladspa
           libsndfile
           lv2
           mesa))
    (native-inputs
     (list pkg-config))
    (synopsis "Audio plugin collection")
    (description "LSP (Linux Studio Plugins) is a collection of audio
plugins available as LADSPA/LV2 plugins and as standalone JACK
applications.")
    (home-page "https://lsp-plug.in/")
    (license license:lgpl3)))

(define-public sherlock-lv2
  (package
    (name "sherlock-lv2")
    (version "0.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://git.open-music-kontrollers.ch/lv2/"
             "sherlock.lv2/snapshot/sherlock.lv2-"
             version ".tar.xz"))
       (sha256
        (base32
         "08gjfx7vrsx9zvj04j8cr3vscxmq6jr2hbdi6dfgp1l1dnnpxsgq"))))
    (build-system meson-build-system)
    (inputs
     (list glu libx11 mesa sratom))
    (native-inputs
     (list flex pkg-config))
    (synopsis "Investigative LV2 plugin bundle")
    (description "The Sherlock plugin bundle contains LV2 plugins for
visualizing LV2 atom, MIDI and OSC events.  They can be used for monitoring
and debugging of event signal flows inside plugin graphs.")
    (home-page "https://open-music-kontrollers.ch/lv2/sherlock/")
    (license license:artistic2.0)))

(define-public foo-yc20
  (package
    (name "foo-yc20")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/sampov2/foo-yc20/releases/download/"
                           version "/foo-yc20-" version ".tar.bz2"))
       (sha256
        (base32
         "1drzfyr7mzb58pdv0gsqkg6ds6kbgp6g25rrv1yya1611cljgvjh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f  ; no automated test
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (substitute* "Makefile"
               (("-mtune=native") "")
               (("-march=native") ""))
             #t)))))
    (inputs
     `(("jack" ,jack-1)
       ("lv2" ,lv2)
       ("cairo" ,cairo)
       ("gtk" ,gtk+-2)))
    (native-inputs
     (list faust pkg-config))
    (home-page "https://foo-yc20.codeforcode.com/")
    (synopsis "Implementation of Yamaha YC-20 combo organ from 1969")
    (description "This is a Faust implementation of a 1969 designed Yamaha
combo organ, the YC-20.  This package provides an LV2 plugin and a standalone
version.  Processing for the organ is based on original schematics and
measurements from a working specimen.  This instrument simulates the circutry
as a whole to realisticly reproduce the features and flaws of the real deal.")
    ;; Note that after 1.3.0 the license was changed.
    (license license:gpl3+)))

(define-public spectacle-analyzer
  (package
    (name "spectacle-analyzer")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jpcima/spectacle")
             (commit (string-append "v" version))
             ;; Bundles a specific commit of the DISTRHO plugin framework.
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0xiqa6z8g68lcvnwhws4j7c4py35r9d20cirrili7ycyp3a6149a"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list pkg-config xxd))
    (inputs
     (list cairo
           fftw
           fftwf
           jack-1
           lv2
           mesa))
    (synopsis "Realtime graphical spectrum analyzer")
    (description "Spectacle is a real-time spectral analyzer using the
short-time Fourier transform, available as LV2 audio plugin and JACK client.")
    (home-page "https://github.com/jpcima/spectacle")
    ;; The project is licensed under the ISC license, and files in
    ;; sources/plugin carry the Expat license.
    (license (list license:isc license:expat))))

(define-public x42-plugins
  (package
    (name "x42-plugins")
    (version "20211016")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://gareus.org/misc/x42-plugins/x42-plugins-"
                       version ".tar.xz"))
       (sha256
        (base32 "11bf6q42ihyk4qxwf83y0847zzyl65kwpbk3hmyz0f0ykil5fsb7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no "check" target
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "LIBZITACONVOLVER=-lzita-convolver"
             (string-append "FONTFILE="
                            (assoc-ref %build-inputs "font-dejavu")
                            "/share/fonts/truetype/DejaVuSans-Bold.ttf")
             (string-append "CC=" ,(cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     (list cairo
           fftwf
           ftgl
           glib
           glu
           hicolor-icon-theme
           jack-1
           libltc
           libsamplerate
           libx11
           pango
           zita-convolver))
    (native-inputs
     (list help2man liblo lv2 font-dejavu pkg-config))
    (synopsis "Collection of LV2/JACK audio/MIDI processing plugins")
    (description "x42-plugins is a collection of over 80 cross-platform LV2
audio and MIDI plugins that can also run as standalone JACK applications.")
    (home-page "https://x42-plugins.com/x42/")
    (license license:gpl2+)))

(define-public zam-plugins
  (package
    (name "zam-plugins")
    (version "3.13")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/zamaudio/zam-plugins")
         (commit version)
         ;; Recursive to fetch the DISTRHO plugin framework. This
         ;; framework is intended to be included in the sources
         ;; and not to be used as a library.
         (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02blg0iqich4vx5z1ahj6avkh83yqszdiq83p9jd5qwm0i4llqjq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no "check" target
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "HAVE_ZITA_CONVOLVER=true")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-CC-variable
           (lambda _
             (setenv "CC" "gcc") #t))
         (delete 'configure))))
    (inputs
     (list fftwf
           jack-1 ;for the standalone JACK application
           liblo
           libsamplerate
           mesa
           zita-convolver))
    (native-inputs
     (list ladspa lv2 pkg-config))
    (synopsis "Collection of audio processing plugins")
    (description
     "Zam plugins is a collection of audio processing plugins in the LADSPA,
LV2 and VST2 formats, as well as standalone JACK versions.  The collection
includes ZaMaximX2, ZamAutoSat, ZamComp, ZamCompX2, ZamEQ2, ZamGEQ31,
ZamHeadX2, ZamPhono, ZamGate, ZamGateX2, ZamTube, ZamDelay, ZamDynamicEQ,
ZaMultiComp, ZaMultiCompX2 and ZamSynth.")
    (home-page "http://www.zamaudio.com/?p=976")
    (license license:gpl2+)))

(define-public geonkick
  (package
    (name "geonkick")
    (version "2.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/iurie-sw/geonkick")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w1mvqm46qdwldcl81svaykwii4wvx7mcr57kwvnj0iv2qrc891i"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no tests included
       #:configure-flags
       (list (string-append "-DGKICK_REDKITE_SDK_PATH="
                            (assoc-ref %build-inputs "redkite"))
             (string-append "-DCMAKE_INSTALL_PREFIX="
                            (assoc-ref %outputs "out")))))
    (inputs
     (list hicolor-icon-theme
           jack-1 ;for the standalone JACK application
           libsndfile
           libx11
           redkite
           rapidjson))
    (native-inputs
     (list lv2 pkg-config sord))
    (synopsis "Percussion synthesizer")
    (description "Geonkick is a synthesizer that can synthesize elements
of percussion such as kicks, snares, hit-hats, shakers, claps and sticks.
It can also play and mix samples.")
    (home-page "https://gitlab.com/iurie-sw/geonkick")
    (license license:gpl3+)))

(define-public mamba
  (package
   (name "mamba")
   (version "2.1")
   (source
    (origin
      (method git-fetch)
      (uri
       (git-reference
        (url "https://github.com/brummer10/Mamba")
        (commit (string-append "v" version))
        (recursive? #t))) ; references specific commit of libxputty
      (file-name (git-file-name name version))
      (sha256
       (base32
        "1bq6sqsij3cdwcsj3wpsnivi4c7jl4l5gwfywhqnib70v60smdja"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f  ; no "check" target
      #:make-flags
      (list (string-append "PREFIX="
                           (assoc-ref %outputs "out"))
            (string-append "CC=" ,(cc-for-target)))
      #:phases
      (modify-phases %standard-phases
        (delete 'configure))))
   (inputs
    (list alsa-lib
          cairo
          fluidsynth
          jack-1
          liblo
          libsigc++
          libsmf
          libx11))
   (native-inputs
    (list pkg-config))
   (home-page "https://github.com/brummer10/Mamba")
   (synopsis "Virtual MIDI keyboard and MIDI file player/recorder for JACK")
   (description "Mamba is a virtual MIDI keyboard and MIDI file
player/recorder for the JACK Audio Connection Kit.  It comes with predefined
keymaps for QWERTZ, QWERTY and AZERTY keyboards and also allows custom
ones.")
   (license license:bsd-0)))

(define-public dpf-plugins
  (package
    (name "dpf-plugins")
    (version "1.4")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/DISTRHO/DPF-Plugins")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y7qvpfm34g6f7d786c6c9043dlbg5c4h71l2s24dsc9m8i7x2ww"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no "check" target
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "CC=" ,(cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     (list cairo
           liblo ; for dssi plugins
           jack-1 ; for standalone applications
           mesa))
    (native-inputs
     (list pkg-config dssi lv2))
    (home-page "https://github.com/DISTRHO/DPF-Plugins")
    (synopsis "Audio plugin collection")
    (description "Collection of audio plugins built with the DISTRHO Plugin
Framework (DPF) available in LADSPA, DSSI, LV2 and VST2 formats.  This
package includes the following plugins: glBars, Kars, Max-Gen examples
(MaBitcrush, MaFreeverb, MaGigaverb, MaPitchshift), Mini-Series (3BandEQ,
3BandSplitter, PingPongPan), ndc-Plugs (Amplitude Imposer, Cycle Shifter,
Soul Force), MVerb, Nekobi, and ProM.")
    ;; This package consists of several plugins refactored to use the
    ;; DISTHRO Plugin Framework (DPF). Different copyrights and licenses
    ;; apply to different plugins. The root LICENSE file has a table with
    ;; license information for each plugin and paths to each license
    (license (list license:isc license:gpl3 license:lgpl3 license:expat license:gpl2))))

(define-public avldrums-lv2
  (package
    (name "avldrums-lv2")
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/x42/avldrums.lv2")
             (commit (string-append "v" version))
             ;; This plugin expects the robtk submodule's source files to be
             ;; there in order to build.
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14gka5g7va30gm1hn0cas4vvb8s764rfvzcxm67ww86hf54cpnig"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no "check" target
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'build 'set-CC-variable
           (lambda _
             (setenv "CC" "gcc"))))))
    (inputs
     (list cairo dssi glu mesa pango))
    (native-inputs
     (list pkg-config lv2))
    (home-page "https://x42-plugins.com/x42/x42-avldrums")
    (synopsis "Drum sample player LV2 plugin dedicated to the AVLinux Drumkits")
    (description "AVLdrums is a drum sample player LV2 plugin dedicated to Glen
MacArthur's AVLdrums.  This plugin provides a convenient way to sequence and mix
MIDI drums and comes as two separate drumkits: Black Pearl and Red Zeppelin.")
    (license license:gpl2+)))

(define-public helm
  (package
    (name "helm")
    (version "0.9.0")
    (source
      (origin
        (method git-fetch)
        (uri
          (git-reference
            (url "https://github.com/mtytel/helm")
            (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "17ys2vvhncx9i3ydg3xwgz1d3gqv4yr5mqi7vr0i0ca6nad6x3d4"))
        ;; Apply GCC 9 fixes from https://github.com/mtytel/helm/pull/233
        (patches (search-patches "helm-fix-gcc-9-build.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ; no "check" target
       #:make-flags
       (list (string-append "DESTDIR=" (assoc-ref %outputs "out"))
             "lv2" "standalone")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'include-pnglib-code-and-remove-usr-from-paths
           (lambda _
             (substitute* "standalone/builds/linux/Makefile"
               (("JUCE_INCLUDE_PNGLIB_CODE=0")
                "JUCE_INCLUDE_PNGLIB_CODE=1"))
             (substitute* "builds/linux/LV2/Makefile"
               (("JUCE_INCLUDE_PNGLIB_CODE=0")
                "JUCE_INCLUDE_PNGLIB_CODE=1"))
             (substitute* "Makefile"
               (("/usr") ""))
             #t))
         (delete 'configure))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("curl" ,curl)
       ("freetype2" ,freetype)
       ("hicolor-icon-theme" ,hicolor-icon-theme)
       ("libxcursor" ,libxcursor)
       ("libxinerama" ,libxinerama)
       ("jack" ,jack-1)
       ("mesa" ,mesa)))
    (native-inputs
     (list pkg-config lv2))
    (home-page "https://tytel.org/helm/")
    (synopsis "Polyphonic synth with lots of modulation")
    (description "Helm is a cross-platform polyphonic synthesizer available standalone
and as an LV2 plugin.")
    (license license:gpl3+)))

(define-public zrythm
  (package
    ;; Zrythm contains trademarks and comes with a trademark policy found in
    ;; TRADMARKS.md inside the release distribution.  The trademark policy
    ;; allows verbatim re-distribution, and it also allows FSF-approved
    ;; distros to make necessary changes to integrate the software into the
    ;; distribution.
    (name "zrythm")
    (version "1.0.0-alpha.12.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://www.zrythm.org/releases/zrythm-"
                            version ".tar.xz"))
        (sha256
          (base32
           "1si4n8rdg0a3frlbj6yqpyzr4f20v3cpl4m6kv0yf7r25psyl5pk"))))
   (build-system meson-build-system)
   (arguments
    `(#:glib-or-gtk? #t
      #:configure-flags
      `("-Dtests=true"
        "-Dmanpage=true"
        "-Ddseg_font=false"
        "-Dgraphviz=enabled" ; for exporting routing graphs
        "-Dguile=enabled" ; for Guile scripting
        "-Djack=enabled" ; for JACK audio/MIDI backend
        "-Drtmidi=enabled" ; for RtMidi backend (ALSA sequencer)
        "-Dsdl=enabled"))) ; for SDL audio backend (which uses ALSA)
   (inputs
    `(("alsa-lib" ,alsa-lib)
      ("jack" ,jack-1)
      ("font-dseg" ,font-dseg)
      ("fftw" ,fftw)
      ("fftwf" ,fftwf)
      ("gettext" ,gettext-minimal)
      ("glibc" ,glibc)
      ("graphviz" ,graphviz)
      ("gtk+" ,gtk+)
      ("gtksourceview" ,gtksourceview)
      ("guile" ,guile-2.2)
      ("libaudec" ,libaudec)
      ("libcyaml" ,libcyaml)
      ("libsamplerate" ,libsamplerate)
      ("libsndfile" ,libsndfile)
      ("libyaml" ,libyaml)
      ("lilv" ,lilv)
      ("lv2" ,lv2)
      ("pulseaudio" ,pulseaudio)
      ("reproc" ,reproc)
      ("rubberband" ,rubberband)
      ("rtmidi" ,rtmidi)
      ("sdl2" ,sdl2)
      ("xdg-utils" ,xdg-utils)
      ("zstd" ,zstd "lib")))
   (native-inputs
     (list pkg-config help2man
           `(,glib "bin"))) ; for 'glib-compile-resources'
   (synopsis "Digital audio workstation focusing on usability")
   (description "Zrythm is a digital audio workstation designed to be
featureful and easy to use.  It offers unlimited automation options, LV2
plugin support, JACK support and chord assistance.")
   (home-page "https://www.zrythm.org")
   (license license:agpl3+)))

(define-public dragonfly-reverb
  (package
    (name "dragonfly-reverb")
    (version "3.2.5")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/michaelwillis/dragonfly-reverb")
         (commit version)
         ;; Bundles a specific commit of the DISTRHO plugin framework.
         (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14kia9wjs0nqfx4psnr3vf4x6hihkf80gb0mjzmdnnnk4cnrdydm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ;no configure target
         (replace 'install              ;no install target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lv2 (string-append out "/lib/lv2")))
               ;; Install LV2.
               (for-each
                (lambda (file)
                  (copy-recursively file
                                    (string-append lv2 "/" (basename file))))
                (find-files "bin" "\\.lv2$" #:directories? #t))
               ;; Install executables.
               (for-each
                 (lambda (file)
                   (install-file file bin))
                 (find-files "bin"
                             (lambda (name stat)
                               (and
                                 (equal? (dirname name) "bin")
                                 (not (string-suffix? ".so" name))
                                 (not (string-suffix? ".lv2" name))))))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list jack-1 libx11 mesa))
    (home-page "https://michaelwillis.github.io/dragonfly-reverb/")
    (synopsis "Concert hall reverb and room reverb effects")
    (description
     "Dragonfly Reverb is a bundle of two free audio effects: a concert
hall reverb and a room reverb.  Both are available as LV2 plugins as well
as JACK standalone applications.")
    (license license:gpl3+)))

(define-public zplugins
  (package
    (name "zplugins")
    (version "0.1.7")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://git.zrythm.org/zrythm/zplugins")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1rkm2xajmyik6289b20rp5a5br9f3sh1xk8nb1bs6qpmcrfirgbs"))))
    (build-system meson-build-system)
    (inputs
      (list guile-2.2 libsndfile lv2 ztoolkit-rsvg))
    (native-inputs
      (list pkg-config))
    (synopsis "Audio plugin collection")
    (description "ZPlugins is a collection of audio DSP plugins intended
to be bundled with the Zrythm @dfn{digital audio workstation} (DAW).")
    (home-page "https://www.zrythm.org/en/plugins.html")
    (license license:agpl3+)))

(define-public zlfo
  ;; The "zlfo" package is now included in zplugins
  (deprecated-package "zlfo" zplugins))

(define-public remid-lv2
  (package
    (name "remid-lv2")
    (version "0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ssj71/reMID.lv2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "062kriniidsrhzwrf89kfxm9wb0cmgrl07asnlmgil8vcl7gl9y5"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; no tests included
    (inputs
     (list alsa-lib glib jack-1 lv2))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/ssj71/reMID.lv2")
    (synopsis
     "MIDI-controlled implementation of the SID 6581 chip used in the
Commodore 64")
    (description
     "The 6581 SID chip is the sound chip used in the Commodore 64 computer.
reMID is a MIDI implementation of the 6581 SID chip using the reSID library
to provide a virtual SID-based synthesizer, controllable in real-time via
MIDI.  It includes support for scripted instruments that allow complex sonic
control of the chip.")
    (license license:gpl2+)))

(define-public vl1-emulator
  (package
    (name "vl1-emulator")
    (version "1.1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/linuxmao-org/VL1-emulator")
             (commit (string-append "v" version))
             ;; bundles a specific commit of the DISTRHO plugin framework
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1npc86vqma8gk1hawa0lii0r2xmnv846plyl1ci3bdswyrdk5chm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no check target
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ;no configure target
    (inputs
     (list cairo jack-1 mesa))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/linuxmao-org/VL1-emulator")
    (synopsis "Emulator of Casio VL-Tone VL1")
    (description "The VL1-Emulator is an emulator of Casio VL-Tone VL1,
based on source code by PolyValens, offered as an LV2 plugin and a
standalone JACK application.")
    ;; Expat or CC0
    (license (list license:expat license:cc0))))

(define-public regrader
  (package
    (inherit vl1-emulator)
    (name "regrader")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/linuxmao-org/regrader")
             (commit (string-append "v" version))
             ;; bundles a specific commit of the DISTRHO plugin framework
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0gl4d5lf2afqknz22jz7hh7029sc9v1xrz6nbz9dlv42bwc0cvl0"))))
    (home-page "https://github.com/linuxmao-org/regrader")
    (synopsis "Delay effect plugin")
    (description
     "Regrader is a delay effect where the repeats degrade in resolution.
This is an unofficial port of the Regrader plugin created by Igorski.  It
is available as an LV2 plugin and a standalone JACK application.")
    (license license:expat)))

(define-public fogpad
  (package
    (inherit vl1-emulator)
    (name "fogpad")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/linuxmao-org/fogpad")
             (commit (string-append "v" version))
             ;; bundles a specific commit of the DISTRHO plugin framework
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1j1hbya2dsqpf22zkpi4kwz3dram9g1ndxzmgfwpmf3i4jd3csgb"))))
    (home-page "https://github.com/linuxmao-org/fogpad")
    (synopsis "Reverb effect plugin")
    (description
     "Fogpad is a reverb effect in which the reflections can be frozen,
filtered, pitch shifted and ultimately disintegrated.  This is an unofficial
port of the Regrader plugin created by Igorski.  It is available as an LV2
plugin and a standalone JACK application.")
    (license license:expat)))

(define-public tap-lv2
  (let ((commit "cab6e0dfb2ce20e4ad34b067d1281ec0b193598a")
        (revision "1"))
    (package
      (name "tap-lv2")
      (version (git-version "0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/moddevices/tap-lv2")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
            (base32
              "0q480djfqd9g8mzrggc4vl7yclrhdjqx563ghs8mvi2qq8liycw3"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                      ; no check target
         #:make-flags
         (list "CC=gcc")
         #:phases
         (modify-phases %standard-phases
           (delete 'configure) ; no configure
           (replace 'install
             (lambda _
               (invoke "make"
               (string-append "INSTALL_PATH="
                              (assoc-ref %outputs "out")
                              "/lib/lv2")
                       "install"))))))
      (inputs
        (list lv2))
      (native-inputs
        (list pkg-config))
      (synopsis "Audio plugin collection")
      (description "TAP (Tom's Audio Processing) plugins is a collection of
  audio effect plugins originally released as LADSPA plugins.  This package
  offers an LV2 version ported by moddevices.")
      (home-page "http://tap-plugins.sourceforge.net/")
      (license license:gpl2))))

(define-public wolf-shaper
  (package
    (name "wolf-shaper")
    (version "0.1.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/pdesaulniers/wolf-shaper")
               (commit (string-append "v" version))
               ;; Bundles a specific commit of the DISTRHO plugin framework.
               (recursive? #t)))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1j9xmh1nkf45ay1c5dz2g165qvrwlanzcq6mvb3nfxar265drd9q"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:make-flags (list "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ;no configure target
         (replace 'install              ;no install target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lv2 (string-append out "/lib/lv2")))
               ;; Install LV2.
               (for-each
                (lambda (file)
                  (copy-recursively file
                                    (string-append lv2 "/" (basename file))))
                (find-files "bin" "\\.lv2$" #:directories? #t))
               ;; Install executables.
               (for-each
                 (lambda (file)
                   (install-file file bin))
                 (find-files "bin"
                             (lambda (name stat)
                               (and
                                 (equal? (dirname name) "bin")
                                 (not (string-suffix? ".so" name))
                                 (not (string-suffix? ".lv2" name))))))
               #t))))))
    (native-inputs
     (list pkg-config))
    (inputs
      (list jack-1 lv2 mesa))
    (synopsis "Waveshaper plugin")
    (description "Wolf Shaper is a waveshaper plugin with a graph editor.
It is provided as an LV2 plugin and as a standalone Jack application.")
    (home-page "https://pdesaulniers.github.io/wolf-shaper/")
    (license license:gpl3)))

(define-public wolf-spectrum
  (package
    (inherit wolf-shaper)
    (name "wolf-spectrum")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/pdesaulniers/wolf-spectrum")
               (commit (string-append "v" version))
               ;; Bundles a specific commit of the DISTRHO plugin framework.
               (recursive? #t)))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "17db1jlj7vb1xyvkdhhrsvdbwb7jqw6i4168cdvlj3yvn2ra8gpm"))))
    (synopsis "2D spectrogram plugin")
    (description "Wolf Spectrum is a real-time 2D spectrogram plugin.
It is provided as an LV2 plugin and as a standalone Jack application.")
    (home-page "https://github.com/pdesaulniers/wolf-spectrum")
    (license license:gpl3)))

(define-public shiru-lv2
  (let ((commit "08853f99140012234649e67e5647906fda74f6cc")
        (revision "1"))
    (package
      (name "shiru-lv2")
      (version (git-version "0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/linuxmao-org/shiru-plugins")
                 (commit commit)
                 ;; Bundles a specific commit of the DISTRHO plugin framework.
                 (recursive? #t)))
          (file-name (git-file-name name version))
          (sha256
            (base32
              "00rf6im3rhg98h60sgl1r2s37za5vr5h14pybwi07h8zbc8mi6fm"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                      ; no check target
         #:make-flags (list "CC=gcc")
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)            ;no configure target
           (replace 'install              ;no install target
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (lv2 (string-append out "/lib/lv2")))
                 ;; Install LV2.
                 (for-each
                  (lambda (file)
                    (copy-recursively file
                                      (string-append lv2 "/" (basename file))))
                  (find-files "bin" "\\.lv2$" #:directories? #t))
                 ;; Install executables.
                 (for-each
                   (lambda (file)
                     (install-file file bin))
                   (find-files "bin"
                               (lambda (name stat)
                                 (and
                                   (equal? (dirname name) "bin")
                                   (not (string-suffix? ".so" name))
                                   (not (string-suffix? ".lv2" name))))))
                 #t))))))
      (native-inputs
       (list pkg-config))
      (inputs
        (list cairo
              glu
              jack-1
              lv2
              mesa
              pango))
      (synopsis "Audio plugin collection")
      (description "Shiru plugins is a collection of audio plugins created
  by Shiru, ported to LV2 by the Linux MAO project using the DISTRHO plugin
  framework.")
      (home-page "http://shiru.untergrund.net/software.shtml")
      (license license:wtfpl2))))

(define-public a2jmidid
  (package
    (name "a2jmidid")
    (version "9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jackaudio/a2jmidid")
                    (commit version)))
              (sha256
               (base32 "1x6rcl3f4nklnx4p5jln9a7fpj9y7agjxs9rw7cccmwnski7pnsq"))
              (file-name (git-file-name name version))))
    (arguments
     `(#:tests? #f      ; No tests.
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-programs
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/")))
               (substitute* (string-append bin "a2j")
                 (("a2j_control") (string-append bin "a2j_control")))
               (wrap-program (string-append bin "a2j_control")
                `("PYTHONPATH" prefix (,(getenv "GUIX_PYTHONPATH"))))
               #t))))))
    (build-system meson-build-system)
    (inputs
     (list alsa-lib
           bash-minimal ; for wrap-program
           dbus
           jack-1
           python
           python-dbus))
    (native-inputs
     (list pkg-config))
    (synopsis "ALSA sequencer to JACK MIDI bridging")
    (description
     "@code{a2jmidid} is a daemon that implements automatic bridging of ALSA
midi devices to JACK midi devices.")
    (home-page "https://github.com/jackaudio/a2jmidid")
    (license license:gpl2)))

(define-public opustags
  (package
    (name "opustags")
    (version "1.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fmang/opustags")
                    (commit version)))
              (sha256
               (base32 "1wsfw713rhi2gg5xc04cx5i31hlw0l3wdflj3r1y8w45bdk6ag1z"))
              (file-name (git-file-name name version))))
    (arguments
     (list
       #:test-target "check"
       #:phases
       #~(modify-phases %standard-phases
         ;; This package does not use the perl-build-system, so we have to
         ;; manually set up the Perl environment used by the test suite.
         (add-before 'check 'setup-perl-environment
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((perl-list-moreutils-lib
                      (string-append #$(this-package-native-input "perl-list-moreutils")
                                     "/lib/perl5/site_perl/"
                                     #$(package-version perl)))
                    (perl-exporter-tiny-lib
                      (string-append #$(this-package-native-input "perl-exporter-tiny")
                                     "/lib/perl5/site_perl/"
                                     #$(package-version perl))))
               (setenv "PERL5LIB" (string-append perl-list-moreutils-lib ":"
                                                 perl-exporter-tiny-lib))))))))
    (build-system cmake-build-system)
    (inputs
      (list libogg))
    (native-inputs
      (list pkg-config
            ffmpeg
            perl-exporter-tiny
            perl-list-moreutils
            perl-test-harness))
    (synopsis "Ogg Opus tags editor")
    (description "@code{opustags} is an Ogg Opus tag editor.  It reads and edits
the comment header of an Ogg Opus audio file, offering both read-only and
editing modes.  Tags can be edited interactively with an editor of your
choice.")
    (home-page "https://github.com/fmang/opustags")
    (license license:bsd-3)))

(define-public musikcube
  (package
    (name "musikcube")
    (version "0.96.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/clangen/musikcube/")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06myr83x8jvzlzchg3jsw1163n2lcsbmb176zgnx7xxa26jpdbh1"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f ; No test suite
       #:configure-flags
       '("-DCMAKE_BUILD_TYPE=Release"
         "-DENABLE_BUNDLED_TAGLIB=false"
         ;; Use the "wide" ncurses headers but don't look for them in an
         ;; ncursesw directory. For more info:
         ;; https://github.com/clangen/musikcube/wiki/building#compiler-cannot-find-ncurseswcursesh
         "-DNO_NCURSESW=true"
         ;; We will strip the binaries ourselves in the 'strip' phase.
         "-DDISABLE_STRIP=true")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list alsa-lib
           boost
           curl
           ffmpeg
           lame
           libev
           libmicrohttpd
           libopenmpt
           ncurses
           openssl
           pulseaudio
           taglib
           zlib))
    (synopsis "Terminal-based music player, library, and streaming audio server")
    (description "Musikcube is a terminal-based music player, library, and
streaming audio server.")
    (home-page "https://musikcube.com/")
    (license license:bsd-3)))
