;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (gnu packages kde-multimedia)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages music)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg))

(define-public dragon
  (package
    (name "dragon")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/dragon-" version ".tar.xz"))
       (sha256
        (base32 "02l16k4sgrxbczxp8rlnxbylb5fmjh4zhl4xw55qxkvmvxnjy5zr"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcoreaddons" ,kcoreaddons)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kjobwidgets" ,kjobwidgets)
       ("knotifications" ,knotifications)
       ("kparts" ,kparts)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("oxygen-icons" ,oxygen-icons) ; default icon set
       ("phonon" ,phonon)
       ("phonon-backend-gstreamer" ,phonon-backend-gstreamer)
       ("qtbase" ,qtbase)
       ("solid" ,solid)))
    (home-page "https://kde.org/applications/multimedia/org.kde.dragonplayer")
    (synopsis "Simple video player")
    (description "Dragon Player is a multimedia player where the focus is on
simplicity, instead of features.  Dragon Player does one thing, and only one
thing, which is playing multimedia files.  It's simple interface is designed
not to get in your way and instead empower you to simply play multimedia
files.

This package is part of the KDE multimedia module.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public juk
  (package
    (name "juk")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/juk-" version ".tar.xz"))
       (sha256
        (base32 "0c1hrd1n4ah7qm8xr7bfswgbcmbvnnhai4bfawx6v6ab3frl7wvf"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)))
    (inputs
     `(("kcoreaddons" ,kcoreaddons)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kdoctools" ,kdoctools)
       ("kglobalaccel" ,kglobalaccel)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kjobwidgets" ,kjobwidgets)
       ("kio" ,kio)
       ("knotifications" ,knotifications)
       ("ktextwidgets" ,ktextwidgets)
       ("kwallet" ,kwallet)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("oxygen-icons" ,oxygen-icons) ; default icon set
       ("phonon" ,phonon)
       ("phonon-backend-gstreamer" ,phonon-backend-gstreamer)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("taglib" ,taglib)))
    (home-page "https://kde.org/applications/multimedia/org.kde.juk")
    (synopsis "Music jukebox / music player")
    (description "JuK is a powerful music player capable of managing a large
music collection.

Some of JuK's features include:
@itemize
@item Music collection, playlists, and smart playlists
@item Tag editing support, including the ability to edit multiple files at once
@item Tag-based music file organization and renaming
@item CD burning support using k3b
@item Album art using Google Image Search
@end itemize

This package is part of the KDE multimedia module.")
    (license license:gpl2+)))

(define-public k3b
  (package
    (name "k3b")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/k3b-" version ".tar.xz"))
       (sha256
        (base32 "08rbiy1hz650srdksr7ciq8dpcz20wczs22613pghrpgm5zsczhr"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("ffmpeg" ,ffmpeg)
       ("flac" ,flac)
       ("karchive" ,karchive)
       ("kcmutils" ,kcmutils)
       ("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kfilemetadata" ,kfilemetadata)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("kio" ,kio)
       ("kjobwidgets" ,kjobwidgets)
       ("knewstuff" ,knewstuff)
       ("knotifications" ,knotifications)
       ("knotifyconfig" ,knotifyconfig)
       ("kservice" ,kservice)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kxmlgui" ,kxmlgui)
       ("lame" ,lame)
       ("libdvdread" ,libdvdread)
       ;; TODO: LibFuzzer
       ("libiconv" ,libiconv)
       ("libkcddb" ,libkcddb)
       ("libmad" ,libmad)
       ("libmpcdec" ,libmpcdec) ;; FIXME: why does cmake not find this?
       ;;("libmusicbrainz" ,libmusicbrainz) ; wants old version 2
       ("libsamplerate" ,libsamplerate)
       ("libsndfile" ,libsndfile)
       ("libvorbis" ,libvorbis)
       ("oxygen-icons" ,oxygen-icons) ; default icon set
       ("qtbase" ,qtbase)
       ("qtwebkit" ,qtwebkit)
       ("shared-mime-info" ,shared-mime-info)
       ("solid" ,solid)
       ("taglib" ,taglib)))
    (home-page "https://kde.org/applications/multimedia/org.kde.k3b")
    (synopsis "Sophisticated CD/DVD burning application")
    (description "K3b is CD-writing software which intends to be feature-rich
and provide an easily usable interface.  Features include burning audio CDs
from .WAV and .MP3 audio files, configuring external programs and configuring
devices.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public kaffeine
  (package
    (name "kaffeine")
    (version "2.0.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/kaffeine"
                           "/kaffeine-" version ".tar.xz"))
       (sha256
        (base32 "10dnhr9v2jlki44i3gmjagky66ybixmv6f29z5imk9clgddrlyfr"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("eudev" ,eudev)
       ("kcoreaddons" ,kcoreaddons)
       ("kdbusaddons" ,kdbusaddons)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libxscrnsaver" ,libxscrnsaver)
       ("oxygen-icons" ,oxygen-icons) ; default icon set
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)
       ("solid" ,solid)
       ("v4l-utils" ,v4l-utils) ; libdvbv5
       ("vlc" ,vlc)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-code
           (lambda _
             (substitute* "src/dvb/dvbdevice_linux.cpp"
               (("\\s*qPrintable\\(transponder\\.getTransmissionType\\(\\)\\)\\);")
                 "transponder.getTransmissionType());"))
             #t)))))
    (home-page "https://kde.org/applications/multimedia/org.kde.kaffeine")
    (synopsis "Versatile media player for KDE")
    (description "Kaffeine is a media player for KDE.  While it supports
multiple Phonon backends, its default backend is Xine, giving Kaffeine a wide
variety of supported media types and letting Kaffeine access CDs, DVDs, and
network streams easily.

Kaffeine can keep track of multiple playlists simultaneously, and supports
autoloading of subtitle files for use while playing video.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public kmix
  (package
    (name "kmix")
    (version "19.08.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/applications/" version
                          "/src/kmix-" version ".tar.xz"))
      (sha256
       (base32 "1g42hlmpdf0rrgqapps6v47z9cnwpkfzpwgavaq26m5k3bpanwfg"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("glib" ,glib)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcompletion" ,kcompletion)
       ("kconfig" ,kconfig)
       ("kconfigwidgets" ,kconfigwidgets)
       ("kcrash" ,kcrash)
       ("kdbusaddons" ,kdbusaddons)
       ("kdoctools" ,kdoctools)
       ("kglobalaccel" ,kglobalaccel)
       ("ki18n" ,ki18n)
       ("kiconthemes" ,kiconthemes)
       ("knotifications" ,knotifications)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("kwindowsystem" ,kwindowsystem)
       ("kxmlgui" ,kxmlgui)
       ("libcanberra" ,libcanberra)
       ("oxygen-icons" ,oxygen-icons) ; default icon set
       ("plasma-framework" ,plasma-framework)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase)
       ("solid" ,solid)))
    (home-page "https://kde.org/applications/multimedia/org.kde.kmix")
    (synopsis "Volume control and mixer")
    (description "KMix is an audio device mixer, used to adjust volume, select
recording inputs, and set other hardware options.

This package is part of the KDE multimedia module.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kmplayer
  (package
    (name "kmplayer")
    (version "0.12.0b")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/kmplayer/0.12"
                           "/kmplayer-" version ".tar.bz2"))
       (sha256
        (base32 "0wzdxym4fc83wvqyhcwid65yv59a2wvp1lq303cn124mpnlwx62y"))
       (patches (search-patches
                 "kmplayer-aarch64.patch"
                 "kmplayer-upstream_Fix-build-with-Qt-5.9.patch"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("pkg-config" ,pkg-config)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kconfig" ,kconfig)
       ("kcoreaddons" ,kcoreaddons)
       ("kdelibs4support" ,kdelibs4support)
       ("ki18n" ,ki18n)
       ("kinit" ,kinit)
       ("kio" ,kio)
       ("kparts" ,kparts)
       ("kmediaplayer" ,kmediaplayer)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("libxcb" ,libxcb) ;; FIXME: why does cmake not find XEVIE and XPRINT?
       ("oxygen-icons" ,oxygen-icons) ; default icon set
       ("phonon" ,phonon)
       ("qtbase" ,qtbase)
       ("cairo" ,cairo)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)
       ("xcb-util" ,xcb-util)
       ("xcb-util-cursor" ,xcb-util-cursor)
       ("xcb-util-errors" ,xcb-util-errors)
       ("xcb-util-image" ,xcb-util-image)
       ("xcb-util-keysyms" ,xcb-util-keysyms)
       ("xcb-util-wm" ,xcb-util-wm)))
    (arguments
     `(#:configure-flags
       (list (string-append
              "-DCMAKE_CXX_FLAGS=-I"
              (assoc-ref %build-inputs "qtx11extras") "/include/qt5"))))
    (home-page "https://kde.org/applications/multimedia/org.kde.kmplayer")
    (synopsis "Media player using mplayer/phonon as backend")
    (description "Kmplayer can play all the audio/video supported by
mplayer/phonon from a local file or URL and be embedded in Konqueror and
KHTML.  It also plays DVDs.

Some features:
@itemize
@item play DVD/VCD movies (from file or url and from a video device)
@item embed inside konqueror (movie is played inside konqueror)
@item embed inside khtml (movie playback inside a html page)
@item Movie recording using mencoder (part of the mplayer package)
@item No video during recording, but you can always open a new window and play it
@item Broadcasting, http streaming, using ffserver/ffmpeg
@item For TV sources, you need v4lctl (part of the xawtv package)
@end itemize")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public libkcddb
  (package
    (name "libkcddb")
    (version "19.08.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/applications/" version
                           "/src/libkcddb-" version ".tar.xz"))
       (sha256
        (base32 "1rla9sfzpdfiki0p265ga6d1axqpq37825maaw85hm84mg7hkzjn"))))
    (build-system qt-build-system)
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("kdoctools" ,kdoctools)))
    (inputs
     `(("kcodecs" ,kcodecs)
       ("kconfig" ,kconfig)
       ("ki18n" ,ki18n)
       ("kio" ,kio)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("libmusicbrainz" ,libmusicbrainz)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:tests? #f)) ; Most tests require network
    (home-page "https://cgit.kde.org/libkcddb.git")
    (synopsis "CDDB library for KDE Platform (runtime)")
    (description "A library for retrieving and sending cddb information.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))
