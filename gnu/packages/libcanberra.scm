;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages libcanberra)
  #:use-module ((guix licenses) #:select (lgpl2.1+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xiph))

(define-public libcanberra
  (package
    (name "libcanberra")
    (version "0.30")
    (source
     (origin
      (method url-fetch)

      ;; This used to be at 0pointer.de but it vanished.
      (uri (string-append
            "http://pkgs.fedoraproject.org/repo/pkgs/libcanberra/libcanberra-"
            version ".tar.xz/34cb7e4430afaf6f447c4ebdb9b42072/libcanberra-"
            version ".tar.xz"))
      (sha256
       (base32
        "0wps39h8rx2b00vyvkia5j40fkak3dpipp1kzilqla0cgvk73dn2"))))
    (build-system gnu-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("gstreamer" ,gstreamer)
       ("gtk+" ,gtk+)
       ("libltdl" ,libltdl)
       ("libvorbis" ,libvorbis)
       ("pulseaudio" ,pulseaudio)
       ("udev" ,eudev)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://0pointer.de/lennart/projects/libcanberra/")
    (synopsis
     "Implementation of the XDG Sound Theme and Name Specifications")
    (description
     "Libcanberra is an implementation of the XDG Sound Theme and Name
Specifications, for generating event sounds on free desktops, such as
GNOME.  It comes with several backends (ALSA, PulseAudio, OSS, GStreamer,
null) and is designed to be portable.")
    (license lgpl2.1+)))

(define-public sound-theme-freedesktop
  (package
    (name "sound-theme-freedesktop")
    (version "0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://people.freedesktop.org/~mccann/dist/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "054abv4gmfk9maw93fis0bf605rc56dah7ys5plc4pphxqh8nlfb"))))
    (build-system gnu-build-system)
    (native-inputs `(("intltool" ,intltool)))
    (synopsis "Audio samples for use as a desktop sound theme")
    (description
     "This package provides audio samples that can be used by libcanberra as
sounds for various system events.")
    (license #f)
    (home-page "http://www.freedesktop.org/wiki/Specifications/sound-theme-spec/")))
