;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Cyrill Schenkel <cyrill.schenkel@gmail.com>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2016, 2018, 2019, 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Evan Straw <evan.straw99@gmail.com>
;;; Copyright © 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2020, 2021 Simon Streit <simon@netpanic.org>
;;; Copyright © 2021 Noah Evans <noah@nevans.me>
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

(define-module (gnu packages mpd)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages build-tools) ;meson-next
  #:use-module (gnu packages boost)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages cmake) ;for MPD
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph))

(define-public libmpdclient
  (package
    (name "libmpdclient")
    (version "2.20")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://musicpd.org/download/libmpdclient/"
                              (car (string-split version #\.))
                              "/libmpdclient-" version ".tar.xz"))
              (sha256
               (base32
                "0z979qcjc0dqmpn3q9j174a29akx3zmavz6q6hg31hrrx5l3yy8q"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config
           ;; For building HTML documentation.
           doxygen
           ;; For tests.
           check))
    (arguments
     `(#:configure-flags
       (list "-Ddocumentation=true"
             "-Dtest=true")))
    (synopsis "Music Player Daemon client library")
    (description "A stable, documented, asynchronous API library for
interfacing MPD in the C, C++ & Objective C languages.")
    (home-page "https://www.musicpd.org/libs/libmpdclient/")
    (license license:bsd-3)))

(define-public mpd
  (package
    (name "mpd")
    (version "0.23.5")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://musicpd.org/download/mpd/"
                              (version-major+minor version)
                              "/mpd-" version ".tar.xz"))
              (sha256
               (base32
                "16d2ny05z47qpj4sjagdcyphb16dzdy21mwwdxbg819s14jjqb7j"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Ddocumentation=enabled")))
    (inputs (list ao
                  alsa-lib
                  avahi
                  boost
                  curl
                  ffmpeg
                  flac
                  fmt
                  glib
                  icu4c
                  ;; The LAME decoder comes from FFmpeg, but is added here so that
                  ;; configure picks up the LAME encoder.
                  lame
                  libid3tag
                  libmpdclient
                  libsamplerate
                  libsndfile
                  libvorbis
                  opus
                  pulseaudio
                  sqlite
                  zlib))
    (native-inputs (list cmake pkg-config python-sphinx))
    ;; Missing optional inputs:
    ;;   libyajl
    ;;   libcdio_paranoia
    ;;   libmms
    ;;   libadplug
    ;;   libaudiofile
    ;;   faad2
    ;;   fluidsynth
    ;;   libgme
    ;;   libshout
    ;;   libmpg123
    ;;   libmodplug
    ;;   libmpcdec
    ;;   libsidplay2
    ;;   libwavpack
    ;;   libwildmidi
    ;;   libtwolame
    ;;   libroar
    ;;   libjack
    ;;   OpenAL
    (synopsis "Music Player Daemon")
    (description "Music Player Daemon (MPD) is a flexible, powerful,
server-side application for playing music.  Through plugins and libraries it
can play a variety of sound files while being controlled by its network
protocol.")
    (home-page "https://www.musicpd.org/")
    (properties `((release-monitoring-url . "https://musicpd.org")))
    (license license:gpl2)))

(define-public mpd-mpc
  (package
    (name "mpd-mpc")
    (version "0.34")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://www.musicpd.org/download/mpc/"
                              (car (string-split version #\.))
                              "/mpc-" version ".tar.xz"))
              (sha256
               (base32
                "086sdx88zvgbv4j9kw4qlrsw1n621d6j6403pcid045wahv3y7k9"))))
    (build-system meson-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'remove-bogus-rsync-requirement
                 ;; Meson thinks a maintainer ‘upload to musicpd.org’ task
                 ;; merits a hard dependency on rsync.  Convince it otherwise.
                 ;; Don't use ‘true’ so that the build will fail if it is ever
                 ;; actually invoked.
                 (lambda _
                   (substitute* "doc/meson.build"
                     (("rsync") "ls")))))))
    (inputs (list libmpdclient))
    (native-inputs
     (list pkg-config python-sphinx))
    (synopsis "Music Player Daemon client")
    (description "MPC is a minimalist command line interface to MPD, the music
player daemon.")
    (home-page "https://www.musicpd.org/clients/mpc/")
    (license license:gpl2)))

(define-public ncmpc
  (package
    (name "ncmpc")
    (version "0.46")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "http://musicpd.org/download/ncmpc/"
                              (version-major version)
                              "/ncmpc-" version ".tar.xz"))
              (sha256
               (base32
                "0klkjaq6n05cmgcwiawjm6d3rn6mrncy72s3x0abjjnx177pfzqp"))))
    (build-system meson-build-system)
    (inputs (list boost pcre libmpdclient ncurses))
    (native-inputs
     (list gettext-minimal              ; for xgettext
           pkg-config
           python-sphinx))
    (synopsis "Curses Music Player Daemon client")
    (description "ncmpc is a fully featured MPD client, which runs in a
terminal using ncurses.")
    (home-page "https://www.musicpd.org/clients/ncmpc/")
    (license license:gpl2)))

(define-public ncmpcpp
  (package
    (name "ncmpcpp")
    (version "0.9.2")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://ncmpcpp.rybczak.net/stable/ncmpcpp-"
                              version ".tar.bz2"))
              (sha256
               (base32
                "06rs734n120jp51hr0fkkhxrm7zscbhpdwls0m5b5cccghazdazs"))))
    (build-system gnu-build-system)
    (inputs (list libmpdclient
                  boost
                  readline
                  ncurses
                  taglib
                  icu4c
                  curl))
    (native-inputs
     (list pkg-config))
    (arguments
     '(#:configure-flags
       '("BOOST_LIB_SUFFIX=" "--with-taglib" "--enable-clock")))
    (synopsis "Featureful ncurses based MPD client inspired by ncmpc")
    (description "Ncmpcpp is an mpd client with a UI very similar to ncmpc,
but it provides new useful features such as support for regular expressions
for library searches, extended song format, items filtering, the ability to
sort playlists, and a local file system browser.")
    (home-page "https://ncmpcpp.rybczak.net/")
    (license license:gpl2+)))

(define-public mpdscribble
  (package
    (name "mpdscribble")
    (version "0.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.musicpd.org/download/mpdscribble/"
                                  version "/mpdscribble-" version ".tar.xz"))
              (sha256
               (base32
                "0s66zqscb44p88cl3kcv5jkjcqsskcnrv7xgrjhzrchf2kcpwf53"))))
    (build-system meson-build-system)
    (inputs (list boost curl libgcrypt libmpdclient))
    (native-inputs (list pkg-config))
    (synopsis "MPD client for track scrobbling")
    (description "mpdscribble is a Music Player Daemon client which submits
information about tracks being played to a scrobbler, such as Libre.FM.")
    (home-page "https://www.musicpd.org/clients/mpdscribble/")
    (license license:gpl2+)))

(define-public python-mpd2
  (package
    (name "python-mpd2")
    (version "3.0.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "python-mpd2" version))
              (sha256
               (base32
                "1r8saq1460yfa0sxfrvxqs2r453wz2xchlc9gzbpqznr49786rvs"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (invoke "python" "-m" "pytest" "mpd/tests.py"))))))
    (native-inputs
     (list python-mock python-pytest))
    (home-page "https://github.com/Mic92/python-mpd2")
    (synopsis "Python MPD client library")
    (description "Python-mpd2 is a Python library which provides a client
interface for the Music Player Daemon.")
    (license license:lgpl3+)))

(define-public sonata
  (package
    (name "sonata")
    (version "1.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/multani/sonata")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rl8w7s2asff626clzfvyz987l2k4ml5dg417mqp9v8a962q0v2x"))))
    (build-system python-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build python-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build python-build-system)
                           (guix build glib-or-gtk-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap))
         (add-after 'install 'wrap-sonata
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH")))
               (wrap-program (string-append out "/bin/sonata")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
             #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (inputs
     (list python-mpd2
           gtk+
           gsettings-desktop-schemas
           gobject-introspection
           adwaita-icon-theme
           python-pygobject))
    (synopsis "Elegant client for the Music Player Daemon")
    (description "Sonata is an elegant graphical client for the Music Player
Daemon (MPD).  It supports playlists, multiple profiles (connecting to different
MPD servers, search and multimedia key support.")
    (home-page "https://www.nongnu.org/sonata/")
    (license license:gpl3+)))

(define-public ashuffle
  (package
    (name "ashuffle")
    (version "2.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/joshkunz/ashuffle")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11aa95cg0yca2m2d00sar6wr14g3lc7cfm9bin1h7lk7asdm8azp"))))
    (native-inputs (list pkg-config))
    (inputs (list libmpdclient))
    (build-system meson-build-system)
    (home-page "https://github.com/joshkunz/ashuffle")
    (synopsis "Automatic library-wide shuffle for mpd")
    (description "ashuffle is an application for automatically shuffling your
MPD library in a similar way to many other music players' 'shuffle library'
feature. ashuffle works like any other MPD client, and can be used alongside
other MPD frontends.")
    (license license:expat)))

(define-public mpdris2
  (package
    (name "mpdris2")
    (version "0.9.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/eonpatapon/mpDris2")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17ys8ccr3h6vix0agvhz859slz0phhk7lwqn5bg4az8s7zgqm3nm"))))
    (build-system gnu-build-system)
    ;; Manually wrap the binary, because we’re not using python-build-system.
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out         (assoc-ref outputs "out"))
                   (python-path (getenv "GUIX_PYTHONPATH")))
               (wrap-program (string-append out "/bin/mpDris2")
                 `("GUIX_PYTHONPATH" ":" prefix (,python-path)))
               #t))))))
    (inputs
     (list python-mpd2 python-dbus python-pygobject python))             ; Sets GUIX_PYTHONPATH.
    ;; For bootstrapping.
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("which" ,which)
       ("intltool" ,intltool)))
    (synopsis "MPRIS V2.1 support for MPD")
    (description "Client for the Music Player Daemon providing MPRIS 2
support")
    (home-page "https://github.com/eonpatapon/mpDris2")
    (license license:gpl3+)))

(define-public cantata
  (package
    (name "cantata")
    (version "2.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/CDrummond/"
                                  "cantata/releases/download/v" version "/"
                                  "cantata-" version ".tar.bz2"))
              (sha256
               (base32
                "10pcrpmb4n1mkgr21xd580nrbmh57q7s72cbs1zay847hc65vliy"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ; No test suite
    (native-inputs
     (list pkg-config))
    (inputs
     (list eudev
           ffmpeg
           libcdio-paranoia
           libebur128
           libmtp
           mpg123
           qtbase-5
           qtmultimedia
           qtsvg
           taglib
           zlib))
    (synopsis "Graphical MPD Client")
    (description "Cantata is a graphical client for the Music Player Daemon
(MPD), using the Qt5 toolkit.  Its user interface is highly customizable,
supporting multiple collections, ratings, and dynamic playlists.  A local cache
of the music library will be created to provide a hierarchy of albums and
artists along with albumart.")
    (home-page "https://github.com/cdrummond/cantata")
    (license license:gpl3+)))

(define-public mcg
  (package
    (name "mcg")
    (version "2.1.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://gitlab.com/coderkun/mcg")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "01iqxydssxyi4s644dwl64vm7xhn0szd99hdpywbipvb7kwp5196"))))
    (build-system python-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list avahi dconf gsettings-desktop-schemas gtk+ python-pygobject))
    (arguments
     `(#:imported-modules ((guix build glib-or-gtk-build-system)
                           ,@%python-build-system-modules)
       #:modules ((guix build python-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((prog (string-append (assoc-ref outputs "out")
                                        "/bin/mcg")))
               (wrap-program prog
                 `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")))
                 `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))))
         (add-after 'wrap-program 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (synopsis "Covergrid for the MPD")
    (description
     "mcg (CoverGrid) is a client for the Music Player Daemon (MPD), focusing
on albums instead of single tracks.  It is not intended to be a replacement
for your favorite MPD client but an addition to get a better
album-experience.")
    (home-page "https://gitlab.com/coderkun/mcg")
    (license license:gpl3+)))
