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

(define-module (gnu packages mp3)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages oggvorbis)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public libmad
  (package
   (name "libmad")
   (version "0.15.1b")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/mad/libmad/"
                                version "/libmad-"
                                version ".tar.gz"))
            (sha256
             (base32
              "14460zhacxhswnzb36qfpd1f2wbk10qvksvm6wyq5hpvdgnw7ymv"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
       (alist-replace
        'configure
        (lambda* (#:key #:allow-other-keys #:rest args)
         (let ((configure (assoc-ref %standard-phases 'configure)))
           ;; remove option that is not supported by gcc any more
           (substitute* "configure" ((" -fforce-mem") ""))
           (apply configure args)))
       %standard-phases)))
   (synopsis "libmad, an MPEG audio decoder")
   (description
    "MAD (MPEG Audio Decoder) supports MPEG-1 and the MPEG-2 extension to
lower sampling frequencies, as well as the de facto MPEG 2.5 format.
All three audio layers — Layer I, Layer II, and Layer III (i.e. MP3) — are
fully implemented.

This package contains the library.")
   (license license:gpl2+)
   (home-page "http://www.underbit.com/products/mad/")))

(define-public libid3tag
  (package
   (name "libid3tag")
   (version "0.15.1b")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/mad/libid3tag/"
                                version "/libid3tag-"
                                version ".tar.gz"))
            (sha256
             (base32
              "0lb1w883dc46dajbdvnia5870brl5lvnlk7g7y58y9wpg5p4znk3"))))
   (build-system gnu-build-system)
   (inputs `(("zlib" ,zlib)))
   (synopsis "libid3tag, a library for reading ID3 tags")
   (description
    "libid3tag is a library for reading ID3 tags, both ID3v1 and the various
versions of ID3v2")
   (license license:gpl2+)
   (home-page "http://www.underbit.com/products/mad/")))

(define-public id3lib
  (package
   (name "id3lib")
   (version "3.8.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/id3lib/id3lib/"
                                version "/id3lib-"
                                version ".tar.gz"))
            (sha256
             (base32
              "0yfhqwk0w8q2hyv1jib1008jvzmwlpsxvc8qjllhna6p1hycqj97"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
       (alist-replace
        'configure
        (lambda* (#:key #:allow-other-keys #:rest args)
          (let ((configure (assoc-ref %standard-phases 'configure)))
            (substitute* "configure"
              (("iomanip.h") "")) ; drop check for unused header
            ;; see http://www.linuxfromscratch.org/patches/downloads/id3lib/
            (substitute* "include/id3/id3lib_strings.h"
              (("include <string>") "include <cstring>\n#include <string>"))
            (substitute* "include/id3/writers.h"
              (("//\\#include <string.h>") "#include <cstring>"))
            (substitute* "examples/test_io.cpp"
              (("dami;") "dami;\nusing namespace std;"))
            (apply configure args)))
         %standard-phases)))
   (synopsis "a library for reading, writing, and manipulating ID3v1 and ID3v2 tags")
   (description
    "id3lib is a cross-platform software development library for reading,
writing, and manipulating ID3v1 and ID3v2 tags. It is an on-going project
whose primary goals are full compliance with the ID3v2 standard, portability
across several platforms, and providing a powerful and feature-rich API with
a highly stable and efficient implementation.")
   (license license:lgpl2.0+)
   (home-page "http://id3lib.sourceforge.net/")))

(define-public libmp3splt
  (package
   (name "libmp3splt")
   (version "0.8.1a")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/mp3splt/libmp3splt/"
                                version "/libmp3splt-"
                                version ".tar.gz"))
            (sha256
             (base32
              "0d20r1bd4fgp9kp7w3qixjgll34czck9dgw59mx2kbhb0mhh00d8"))))
   (build-system gnu-build-system)
   (inputs `(("libid3tag" ,libid3tag)
             ("libmad" ,libmad)
             ("libogg" ,libogg)
             ("libtool" ,libtool)
             ("libvorbid" ,libvorbis)
             ("pkg-config" ,pkg-config)
             ("pcre" ,pcre)))
   (synopsis "libmp3splt, a library for splitting mp3 and ogg vorbis files")
   (description
    "Mp3splt is a utility to split mp3 and ogg vorbis files selecting a begin
and an end time position, without decoding. For splitting an album, one may
select split points and filenames manually or obtain them automatically from
CDDB (internet or a local file) or from .cue files. The tool also supports
automatic silence split, that can be used also to adjust cddb/cue splitpoints.

This package contains the library.")
   (license license:gpl2+)
   (home-page "http://mp3splt.sourceforge.net/mp3splt_page/home.php")))

(define-public mp3splt
  (package
   (name "mp3splt")
   (version "2.5.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/mp3splt/mp3splt/"
                                version "/mp3splt-"
                                version ".tar.gz"))
            (sha256
             (base32
              "106dnka56prlc9nsfh300f8841am2lmkwlzgl9lkr25mgnc35wwh"))))
   (build-system gnu-build-system)
   (inputs `(("libmp3splt" ,libmp3splt)))
   (synopsis "mp3splt, a utiliy for splitting mp3 and ogg vorbis files")
   (description
    "Mp3splt is a utility to split mp3 and ogg vorbis files selecting a begin
and an end time position, without decoding. For splitting an album, one may
select split points and filenames manually or obtain them automatically from
CDDB (internet or a local file) or from .cue files. The tool also supports
automatic silence split, that can be used also to adjust cddb/cue splitpoints.

This package contains the binary.")
   (license license:gpl2+)
   (home-page "http://mp3splt.sourceforge.net/mp3splt_page/home.php")))

