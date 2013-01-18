;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public libogg
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
   (license (bsd-style "file://COPYING"
                       "See COPYING in the distribution."))
   (home-page "http://xiph.org/ogg/")))

(define-public libvorbis
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
   (inputs `(("libogg" ,libogg)))
   (arguments `(#:configure-flags '("LDFLAGS=-lm")))
   (synopsis "libvorbis, a library implementing the vorbis audio format")
   (description
    "The libvorbis library implements the ogg vorbis audio format,
a fully open, non-proprietary, patent-and-royalty-free, general-purpose
compressed audio format for mid to high quality (8kHz-48.0kHz, 16+ bit,
polyphonic) audio and music at fixed and variable bitrates from 16 to
128 kbps/channel.")
   (license (bsd-style "file://COPYING"
                       "See COPYING in the distribution."))
   (home-page "http://xiph.org/vorbis/")))

(define-public vorbis-tools
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
   (inputs `(("libogg" ,libogg)
             ("libvorbis" ,libvorbis)))
;; FIXME: Add more inputs, see the documentation:
;; All of the tools require libogg and libvorbis to be installed (along
;; with the header files).  Additionally, ogg123 requires libao, libcurl,
;; and a POSIX-compatible thread library.  Ogg123 can optionally compiled
;; to use libFLAC, and libspeex.  Oggenc can be optionally compiled with
;; libFLAC, and libkate.  The libraries libogg, libvorbis, and libao are
;; all available at
;;   http://www.vorbis.com/download.psp
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
   (license gpl2)
   (home-page "http://xiph.org/vorbis/")))
