;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages pulseaudio)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'l:))
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module ((gnu packages autotools) #:select (libtool))
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages check)
  #:use-module (gnu packages gdbm)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xiph)
  #:export (libsndfile
            libsamplerate
            json-c
            pulseaudio))

(define libsndfile
  (package
    (name "libsndfile")
    (version "1.0.25")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.mega-nerd.com/libsndfile/files/libsndfile-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "10j8mbb65xkyl0kfy0hpzpmrp0jkr12c7mfycqipxgka6ayns0ar"))))
    (build-system gnu-build-system)
    (inputs
     `(("libvorbis" ,libvorbis)
       ("libogg" ,libogg)
       ("flac" ,flac)
       ("pkg-config" ,pkg-config)))
    (home-page "http://www.mega-nerd.com/libsndfile/")
    (synopsis "Reading and writing files containing sampled sound")
    (description
     "Libsndfile is a C library for reading and writing files containing
sampled sound (such as MS Windows WAV and the Apple/SGI AIFF format) through
one standard library interface.

It was designed to handle both little-endian (such as WAV) and
big-endian (such as AIFF) data, and to compile and run correctly on
little-endian (such as Intel and DEC/Compaq Alpha) processor systems as well
as big-endian processor systems such as Motorola 68k, Power PC, MIPS and
SPARC.  Hopefully the design of the library will also make it easy to extend
for reading and writing new sound file formats.")
    (license l:gpl2+)))

(define libsamplerate
  (package
    (name "libsamplerate")                     ; aka. Secret Rabbit Code (SRC)
    (version "0.1.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.mega-nerd.com/SRC/libsamplerate-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "01hw5xjbjavh412y63brcslj5hi9wdgkjd3h9csx5rnm8vglpdck"))))
    (build-system gnu-build-system)
    (inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("libsndfile" ,libsndfile)
       ("fftw" ,fftw)))
    (home-page "http://www.mega-nerd.com/SRC/index.html")
    (synopsis "Audio sample rate conversion library")
    (description
     "Secret Rabbit Code (aka. libsamplerate) is a Sample Rate Converter for
audio.  One example of where such a thing would be useful is converting audio
from the CD sample rate of 44.1kHz to the 48kHz sample rate used by DAT
players.

SRC is capable of arbitrary and time varying conversions; from downsampling by
a factor of 256 to upsampling by the same factor.  Arbitrary in this case means
that the ratio of input and output sample rates can be an irrational
number.  The conversion ratio can also vary with time for speeding up and
slowing down effects.

SRC provides a small set of converters to allow quality to be traded off
against computation cost.  The current best converter provides a
signal-to-noise ratio of 145dB with -3dB passband extending from DC to 96% of
the theoretical best bandwidth for a given pair of input and output sample
rates. ")
    (license l:gpl2+)))

(define json-c
  (package
    (name "json-c")
    (version "0.11")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://s3.amazonaws.com/json-c_releases/releases/json-c-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1324jja19jgsvcz9ij3nf4sgkrf4fg0ilw77vzqls3fw8m8wdpr8"))))
    (build-system gnu-build-system)
    (arguments '(#:parallel-build? #f
                 #:parallel-tests? #f))
    (home-page "https://github.com/json-c/json-c/wiki")
    (synopsis "JSON implementation in C")
    (description
     "JSON-C implements a reference counting object model that allows you to
easily construct JSON objects in C, output them as JSON formatted strings and
parse JSON formatted strings back into the C representation of JSON objects.")
    (license l:x11)))

(define pulseaudio
  (package
    (name "pulseaudio")
    (version "4.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://freedesktop.org/software/pulseaudio/releases/pulseaudio-"
                   version ".tar.xz"))
             (sha256
              (base32
               "1bndz4l8jxyq3zq128gzp3gryxl6yjs66j2y1d7yabw2n5mv7kim"))
             (patches (map search-patch
                           '("pulseaudio-test-timeouts.patch"
                             "pulseaudio-volume-test.patch")))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '(;;"--sysconfdir=/etc"
                           "--disable-oss-output")
       #:phases (alist-cons-before
                 'check 'pre-check
                 (lambda _
                   ;; 'tests/lock-autospawn-test.c' wants to create a file
                   ;; under ~/.config/pulse.
                   (setenv "HOME" (getcwd)))
                 %standard-phases)))
    (inputs
     ;; TODO: Add optional inputs (GTK+?).
     `(;; ("sbc" ,sbc)
       ("alsa-lib" ,alsa-lib)
       ("json-c" ,json-c)
       ("speex" ,speex)
       ("libsndfile" ,libsndfile)
       ("libsamplerate" ,libsamplerate)
       ("dbus" ,dbus)
       ("glib" ,glib)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("m4" ,m4)
       ("libtool" ,libtool)
       ("fftw" ,fftw)
       ("avahi" ,avahi)
       ("check" ,check)))
    (propagated-inputs
     ;; 'libpulse*.la' contain `-lgdbm' and `-lcap', so propagate them.
     `(("libcap" ,libcap)
       ("gdbm" ,gdbm)))
    (home-page "http://www.pulseaudio.org/")
    (synopsis "Sound server")
    (description
     "PulseAudio is a sound server.  It is basically a proxy for your sound applications.
It allows you to do advanced operations on your sound data as it passes
between your application and your hardware.  Things like transferring the
audio to a different machine, changing the sample format or channel count and
mixing several sounds into one are easily achieved using a sound server. ")

    ;; PulseAudio is LGPLv2+, but some of the optional dependencies (GNU dbm,
    ;; FFTW, etc.) are GPL'd, so the result is effectively GPLv2+.  See
    ;; 'LICENSE' for details.
    (license l:gpl2+)))

(define-public pavucontrol
  (package
    (name "pavucontrol")
    (version "2.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://freedesktop.org/software/pulseaudio/pavucontrol/pavucontrol-"
                   version
                   ".tar.xz"))
             (sha256
              (base32
               "02s775m1531sshwlbvfddk3pz8zjmwkv1sgzggn386ja3gc9vwi2"))))
    (build-system gnu-build-system)
    (inputs
     `(("libcanberra" ,libcanberra)
       ("gtkmm" ,gtkmm)
       ("pulseaudio" ,pulseaudio)))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "http://freedesktop.org/software/pulseaudio/pavucontrol/")
    (synopsis "PulseAudio volume control")
    (description
     "PulseAudio Volume Control (pavucontrol) provides a GTK+
graphical user interface to connect to a PulseAudio server and
easily control the volume of all clients, sinks, etc.")
    (license l:gpl2+)))
