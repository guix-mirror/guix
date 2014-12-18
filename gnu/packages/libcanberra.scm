;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (gnu packages)
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
        "0wps39h8rx2b00vyvkia5j40fkak3dpipp1kzilqla0cgvk73dn2"))
      ;; "sound-theme-freedesktop" is the default and fall-back sound theme for
      ;; XDG desktops and should always be present.
      ;; http://www.freedesktop.org/wiki/Specifications/sound-theme-spec/
      ;; We make sure libcanberra will find it.
      ;;
      ;; We add the default sounds store directory to the code dealing with
      ;; XDG_DATA_DIRS and not XDG_DATA_HOME. This is because XDG_DATA_HOME
      ;; can only be a single directory and is inspected first.  XDG_DATA_DIRS
      ;; can list an arbitrary number of directories and is only inspected
      ;; later.  This is designed to allows the user to modify any theme at
      ;; his pleasure.
      (patch-flags '("-p0"))
      (patches
       (list (search-patch "libcanberra-sound-theme-freedesktop.patch")))))
    (build-system gnu-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("gstreamer" ,gstreamer)
       ("gtk+" ,gtk+)
       ("libtool" ,libtool)
       ("libvorbis" ,libvorbis)
       ("pulseaudio" ,pulseaudio)
       ("udev" ,eudev)
       ("sound-theme-freedesktop" ,sound-theme-freedesktop)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:phases
       (alist-cons-before
        'build 'patch-default-sounds-directory
        (lambda* (#:key inputs #:allow-other-keys)
          (substitute* "src/sound-theme-spec.c"
            (("@SOUND_THEME_DIRECTORY@")
             (string-append
              (assoc-ref inputs "sound-theme-freedesktop")
              "/share"))))
        %standard-phases)))
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
