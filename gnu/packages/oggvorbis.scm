;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (gnu packages oggvorbis)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages libpng)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:export (libogg
            libvorbis
            libtheora
            speex
            ao
            flac
            libkate
            vorbis-tools))

(define libogg
  (package
   (name "libogg")
   (version "1.3.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://downloads.xiph.org/releases/ogg/libogg-"
                                version ".tar.xz"))
            (sha256
             (base32
              "0jy79ffkl34vycnwfsj4svqsdg1lwy2l1rr49y8r4d44kh12a5r3"))))
   (build-system gnu-build-system)
   (synopsis "libogg, a library for manipulating the ogg multimedia format")
   (description
    "The libogg library allows to manipulate the ogg multimedia container
format, which encapsulates raw compressed data and allows the interleaving of
audio and video data. In addition to encapsulation and interleaving of
multiple data streams, ogg provides packet framing, error detection, and
periodic timestamps for seeking.")
   (license (license:bsd-style "file://COPYING"
                               "See COPYING in the distribution."))
   (home-page "http://xiph.org/ogg/")))

(define libvorbis
  (package
   (name "libvorbis")
   (version "1.3.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://downloads.xiph.org/releases/vorbis/libvorbis-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1gby6hapz9njx4l9g0pndyk4q83z5fgrgc30mfwfgx7bllspsk43"))))
   (build-system gnu-build-system)
   (propagated-inputs `(("libogg" ,libogg)))
   (arguments `(#:configure-flags '("LDFLAGS=-lm")))
   (synopsis "libvorbis, a library implementing the vorbis audio format")
   (description
    "The libvorbis library implements the ogg vorbis audio format,
a fully open, non-proprietary, patent-and-royalty-free, general-purpose
compressed audio format for mid to high quality (8kHz-48.0kHz, 16+ bit,
polyphonic) audio and music at fixed and variable bitrates from 16 to
128 kbps/channel.")
   (license (license:bsd-style "file://COPYING"
                               "See COPYING in the distribution."))
   (home-page "http://xiph.org/vorbis/")))

(define libtheora
  (package
    (name "libtheora")
    (version "1.1.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://downloads.xiph.org/releases/theora/libtheora-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0q8wark9ribij57dciym5vdikg2464p8q2mgqvfb78ksjh4s8vgk"))))
    (build-system gnu-build-system)
    (inputs `(("libvorbis" ,libvorbis)))
    ;; The .pc files refer to libogg.
    (propagated-inputs `(("libogg" ,libogg)))
    (synopsis "Library implementing the Theora video format")
    (description
     "The libtheora library implements the ogg theora video format,
a fully open, non-proprietary, patent-and-royalty-free, general-purpose
compressed video format.")
    (license license:bsd-3)
    (home-page "http://xiph.org/theora/")))

(define speex
  (package
    (name "speex")
    (version "1.2rc1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://downloads.xiph.org/releases/speex/speex-"
                          version ".tar.gz"))
      (sha256
       (base32
        "19mpkhbz3s08snvndn0h1dk2j139max6b0rr86nnsjmxazf30brl"))))
    (build-system gnu-build-system)
    (inputs `(("libogg" ,libogg)))
    (home-page "https://gnu.org/software/speex")
    (synopsis "Library for patent-free audio compression format")
    (description
     "Speex is a patent-free audio compression codec specially designed for
speech.  It is well-adapted to internet applications, such as VoIP. It
features compression of different bands in the same bitstream, intensity
stereo encoding, and voice activity detection.")
    ;; 'src/getopt.c' is under LGPLv2+
    (license (license:bsd-style "file://COPYING"
                                "See COPYING in the distribution."))))

(define ao
  (package
    (name "ao")
    (version "1.1.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://downloads.xiph.org/releases/ao/libao-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1m0v2y6bhr4iwsgdkc7b3y0qgpvpv1ifbxsy8n8ahsvjn6wmppi9"))))
    (build-system gnu-build-system)
    ;; FIXME: Add further backends, see the summary printed after configure.
    ;; XXX: Should back-ends be pushed to different outputs?  For instance,
    ;; "out" would include only the ALSA back-end, while "pulse" would
    ;; contains 'lib/ao/plugins-4/libpulse.*'.
    (inputs `(("pkg-config" ,pkg-config)
              ("alsa-lib" ,alsa-lib)
              ("pulseaudio" ,pulseaudio)))
    (synopsis "Cross platform audio library")
    (description
     "Libao is a cross-platform audio library that allows programs to
output audio using a simple API on a wide variety of platforms.
It currently supports:
Null output (handy for testing without a sound device),
WAV files,
AU files,
RAW files,
OSS (Open Sound System, used on Linux and FreeBSD),
ALSA (Advanced Linux Sound Architecture),
aRts (Analog RealTime Synth, used by KDE),
PulseAudio (next generation GNOME sound server),
esd (EsounD or Enlightened Sound Daemon),
Mac OS X,
Windows (98 and later),
AIX,
Sun/NetBSD/OpenBSD,
IRIX,
NAS (Network Audio Server),
RoarAudio (Modern, multi-OS, networked Sound System),
OpenBSD's sndio.")
    (license license:gpl2+)
    (home-page "http://www.xiph.org/ao/")))

(define flac
  (package
   (name "flac")
   (version "1.2.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://downloads.xiph.org/releases/flac/flac-"
                                version ".tar.gz"))
            (sha256
             (base32
              "1pry5lgzfg57pga1zbazzdd55fkgk3v5qy4axvrbny5lrr5s8dcn"))
            (patches
             (list (search-patch "flac-fix-memcmp-not-declared.patch")))))
   (build-system gnu-build-system)
   (arguments
    `(#:parallel-tests? #f))
   ;; FIXME: configure also looks for xmms, input could be added once it exists
   (inputs `(("libogg" ,libogg)))
   (synopsis "flac free lossless audio codec")
   (description
"FLAC stands for Free Lossless Audio Codec, an audio format that is lossless,
meaning that audio is compressed in FLAC without any loss in quality.")
   (license (license:bsd-style "file://COPYING"
                               "See COPYING in the distribution.")) ; and LGPL and GPL
   (home-page "http://xiph.org/flac/")))

(define libkate
  (package
   (name "libkate")
   (version "0.4.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://libkate.googlecode.com/files/libkate-"
                                version ".tar.gz"))
            (sha256
             (base32
              "0s3vr2nxfxlf1k75iqpp4l78yf4gil3f0v778kvlngbchvaq23n4"))))
   (build-system gnu-build-system)
   ;; FIXME: Add optional inputs doxygen (for documentation) and liboggz
   (inputs `(("bison" ,bison)
             ("libogg" ,libogg)
             ("libpng" ,libpng)
             ("pkg-config" ,pkg-config)
             ("python" ,python-wrapper)
             ("zlib" ,zlib)))
   (synopsis "kate, a karaoke and text codec for embedding in ogg")
   (description
    "Kate is an overlay codec, originally designed for karaoke and text,
that can be multiplixed in Ogg. Text and images can be carried by a Kate
stream, and animated. Most of the time, this would be multiplexed with
audio/video to carry subtitles, song lyrics (with or without karaoke data),
etc., but doesn't have to be.

Series of curves (splines, segments, etc.) may be attached to various
properties (text position, font size, etc.) to create animated overlays.
This allows scrolling or fading text to be defined. This can even be used
to draw arbitrary shapes, so hand drawing can also be represented by a
Kate stream.")
   (license license:bsd-3)
   (home-page "http://code.google.com/p/libkate/")))

(define vorbis-tools
  (package
   (name "vorbis-tools")
   (version "1.4.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://downloads.xiph.org/releases/vorbis/vorbis-tools-"
                                version ".tar.gz"))
            (sha256
             (base32
              "1g12bnh5ah08v529y72kfdz5lhvy75iaz7f9jskyby23m9dkk2d3"))))
   (build-system gnu-build-system)
   (inputs `(("ao" ,ao)
             ("curl" ,curl)
             ("flac" ,flac)
             ("libkate" ,libkate)
             ("libogg" ,libogg)
             ("libvorbis" ,libvorbis)
             ("pkg-config" ,pkg-config)
             ("speex" ,speex)))
   (synopsis "ogg vorbis tools")
   (description
    "Ogg vorbis is a non-proprietary, patent-and-royalty-free,
general-purpose compressed audio format.

The package vorbis-tools contains
ogg123,  an ogg vorbis command line audio player;
oggenc,  the ogg vorbis encoder;
oggdec,  a simple, portable command line decoder (to wav and raw);
ogginfo, to obtain information (tags, bitrate, length, etc.) about
         an ogg vorbis file.")
   (license license:gpl2)
   (home-page "http://xiph.org/vorbis/")))
