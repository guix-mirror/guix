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

(define-module (gnu packages mp3)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages pulseaudio)
  #:use-module ((gnu packages linux)
                #:select (alsa-lib))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake))

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
              "14460zhacxhswnzb36qfpd1f2wbk10qvksvm6wyq5hpvdgnw7ymv"))
            (patches (list (search-patch "libmad-mips-newgcc.patch")))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
       (alist-cons-before
        'configure 'remove-unsupported-gcc-flags
        (lambda _
          ;; remove option that is not supported by gcc any more
          (substitute* "configure" ((" -fforce-mem") "")))
       %standard-phases)))
   (synopsis "MPEG audio decoder")
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
   (synopsis "Library for reading ID3 tags")
   (description
    "Libid3tag is a library for reading ID3 tags, both ID3v1 and the various
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
       (alist-cons-before
        'configure 'apply-patches
        ;; TODO: create a patch for origin instead?
        (lambda _
          (substitute* "configure"
            (("iomanip.h") "")) ; drop check for unused header
          ;; see http://www.linuxfromscratch.org/patches/downloads/id3lib/
          (substitute* "include/id3/id3lib_strings.h"
            (("include <string>") "include <cstring>\n#include <string>"))
          (substitute* "include/id3/writers.h"
            (("//\\#include <string.h>") "#include <cstring>"))
          (substitute* "examples/test_io.cpp"
            (("dami;") "dami;\nusing namespace std;")))
         %standard-phases)))
   (synopsis "Library for reading, writing, and manipulating ID3v1 and ID3v2 tags")
   (description
    "Id3lib is a cross-platform software development library for reading,
writing, and manipulating ID3v1 and ID3v2 tags.  It is an on-going project
whose primary goals are full compliance with the ID3v2 standard, portability
across several platforms, and providing a powerful and feature-rich API with
a highly stable and efficient implementation.")
   (license license:lgpl2.0+)
   (home-page "http://id3lib.sourceforge.net/")))

(define-public taglib
  (package
    (name "taglib")
    (version "1.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://taglib.github.io/releases/taglib-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "06n7gnbcqa3r6c9gv00y0y1r48dyyazm6yj403i7ma0r2k6p3lvj"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))                    ;no 'test' target
    (inputs `(("zlib" ,zlib)))
    (home-page "http://developer.kde.org/~wheeler/taglib.html")
    (synopsis "Library to access audio file meta-data")
    (description
     "TagLib is a C++ library for reading and editing the meta-data of several
popular audio formats.  Currently it supports both ID3v1 and ID3v2 for MP3
files, Ogg Vorbis comments and ID3 tags and Vorbis comments in FLAC, MPC,
Speex, WavPack TrueAudio, WAV, AIFF, MP4 and ASF files.")

    ;; Dual-licensed: user may choose between LGPLv2.1 or MPLv1.1.
    (license (list license:lgpl2.1 license:mpl1.1))))

(define-public mp3info
  (package
    (name "mp3info")
    (version "0.8.5a")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.ibiblio.org/pub/linux/apps/sound/mp3-utils/mp3info/mp3info-"
                    version ".tgz"))
              (sha256
               (base32
                "042f1czcs9n2sbqvg4rsvfwlqib2gk976mfa2kxlfjghx5laqf04"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "Makefile"
                  (("/bin/rm") "rm")
                  (("/usr/bin/install") "install")
                  (("man/man1") "share/man/man1")))))
    (build-system gnu-build-system)
    (outputs '("out" "gui"))                      ;GTK+ interface in "gui"
    (arguments
     '(#:phases (alist-replace
                 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (substitute* "Makefile"
                       (("prefix=.*")
                        (string-append "prefix := " out "\n")))))
                 (alist-cons-before
                  'install 'pre-install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((out (assoc-ref outputs "out")))
                      (mkdir-p (string-append out "/bin"))
                      (mkdir-p (string-append out "/share/man/man1"))))
                  (alist-cons-after
                   'install 'post-install
                   (lambda* (#:key outputs #:allow-other-keys)
                     ;; Move the GTK+ interface to "gui".
                     (let ((out (assoc-ref outputs "out"))
                           (gui (assoc-ref outputs "gui")))
                       (mkdir-p (string-append gui "/bin"))
                       (rename-file (string-append out "/bin/gmp3info")
                                    (string-append gui "/bin/gmp3info"))))
                   %standard-phases)))
        #:tests? #f))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+-2)
       ("ncurses" ,ncurses)))
    (home-page "http://www.ibiblio.org/mp3info/")
    (synopsis "MP3 technical info viewer and ID3 1.x tag editor")
    (description
     "MP3Info is a little utility used to read and modify the ID3 tags of MP3
files.  MP3Info can also display various techincal aspects of an MP3 file
including playing time, bit-rate, sampling frequency and other attributes in a
pre-defined or user-specifiable output format.")
    (license license:gpl2+)))

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
             ("pcre" ,pcre)))
   (native-inputs
     `(("pkg-config" ,pkg-config)))
   (synopsis "Library for splitting mp3 and ogg vorbis files")
   (description
    "Mp3splt is a utility to split mp3 and ogg vorbis files selecting a begin
and an end time position, without decoding.  For splitting an album, one may
select split points and filenames manually or obtain them automatically from
CDDB (internet or a local file) or from .cue files.  The tool also supports
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
   (synopsis "Utiliy for splitting mp3 and ogg vorbis files")
   (description
    "Mp3splt is a utility to split mp3 and ogg vorbis files selecting a begin
and an end time position, without decoding.  For splitting an album, one may
select split points and filenames manually or obtain them automatically from
CDDB (internet or a local file) or from .cue files.  The tool also supports
automatic silence split, that can be used also to adjust cddb/cue splitpoints.

This package contains the binary.")
   (license license:gpl2+)
   (home-page "http://mp3splt.sourceforge.net/mp3splt_page/home.php")))

(define-public mpg123
  (package
    (name "mpg123")
    (version "1.19.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/mpg123/mpg123-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "06xhd68mj9yp0r6l771aq0d7xgnl402a3wm2mvhxmd3w3ph29446"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--with-default-audio=pulse")))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("pulseaudio" ,pulseaudio)
              ("alsa-lib" ,alsa-lib)))
    (home-page "http://www.mpg123.org/")
    (synopsis "Console MP3 player and decoder library")
    (description
     "Mpg123 is a real time MPEG 1.0/2.0/2.5 audio player/decoder for layers
1,2 and 3 (MPEG 1.0 layer 3 aka MP3 most commonly tested).  It comes with a
command-line tool as well as a C library, libmpg123.")
    (license license:lgpl2.1)))

(define-public mpg321
  (package
    (name "mpg321")
    (version "0.3.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/mpg321/"
                                 version "/mpg321-" version ".tar.gz"))
             (sha256
              (base32
               "0yamsqvj15nsxxnjs9mgyslzx7krgsfk3q1hk3g6l8w9bbbj770b"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--disable-alsa")))
    (inputs
     `(("zlib" ,zlib)
       ("libmad" ,libmad)
       ("libid3tag" ,libid3tag)
       ("libao" ,ao)))
    (home-page "http://mpg321.sourceforge.net/")
    (synopsis "Command-line MP3 player")
    (description "Mpg321 is a command-line mp3 player.  mpg321 is used for
frontends, as an mp3 player and as an mp3 to wave file decoder (primarily for
use with CD-recording software).")
    (license license:gpl2)))

(define-public lame
  (package
    (name "lame")
    (version "3.99.5")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/lame/lame/3.99/lame-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1zr3kadv35ii6liia0bpfgxpag27xcivp571ybckpbz4b10nnd14"))))
    (build-system gnu-build-system)
    (home-page "http://lame.sourceforge.net/")
    (synopsis "MPEG Audio Layer III (MP3) encoder")
    (description "LAME is a high quality MPEG Audio Layer III (MP3) encoder.")
    (license license:lgpl2.0)))

(define-public ripperx
  (package
   (name "ripperx")
   (version "2.7.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/ripperx/ripperx/"
                                version "/ripperX-"
                                version ".tar.gz"))
            (sha256
             (base32
              "130rsb2ly0l6hz728m9qr605ir4073xfl2acvf83id63kxfzjn3x"))
            (patches
             ;; see http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=713684
             (list (search-patch "ripperx-libm.patch")))))
   (build-system gnu-build-system)
   (propagated-inputs
    `(("gs-fonts" ,gs-fonts)
      ("cdparanoia" ,cdparanoia)
      ("flac" ,flac)
      ("lame" ,lame)
      ("vorbis-tools" ,vorbis-tools)))
   (inputs
    `(("glib" ,glib)
      ("gtk+" ,gtk+-2)
      ("id3lib" ,id3lib)))
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (synopsis "GTK program to rip and encode CD audio tracks")
   (description
    "RipperX is a GTK program to rip CD audio tracks and encode them to the
Ogg, MP3, or FLAC formats.  Its goal is to be easy to use, requiring only
a few mouse clicks to convert an entire album.  It supports CDDB lookups
for album and track information.")
   (license license:gpl2)
   (home-page "http://sourceforge.net/projects/ripperx/")))

(define-public libmpcdec
  (package
    (name "libmpcdec")
    (version "1.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://files.musepack.net/source/libmpcdec-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1a0jdyga1zfi4wgkg3905y6inghy3s4xfs5m4x7pal08m0llkmab"))))
    (build-system gnu-build-system)
    (synopsis "Decoding library for the Musepack audio format")
    (description
     "This library supports decoding of the Musepack (MPC) audio compression
format.")
    (license license:bsd-3)
    (home-page "http://musepack.net")))

(define-public mpc123
  (package
    (name "mpc123")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/mpc123/version%20"
                                  version "/mpc123-" version ".tar.gz"))
              (sha256
               (base32
                "0sf4pns0245009z6mbxpx7kqy4kwl69bc95wz9v23wgappsvxgy1"))
              (patches (list (search-patch "mpc123-initialize-ao.patch")))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-replace
                 'configure
                 (lambda _
                   (substitute* "Makefile"
                     (("CC[[:blank:]]*:=.*")
                      "CC := gcc\n")))
                 (alist-replace
                  'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (bin (string-append out "/bin")))
                      (mkdir-p bin)
                      (copy-file "mpc123" (string-append bin "/mpc123"))))
                  %standard-phases))
       #:tests? #f))

    (native-inputs
     `(("gettext" ,gnu-gettext)))
    (inputs
     `(("libao" ,ao)
       ("libmpcdec" ,libmpcdec)))
    (home-page "http://mpc123.sourceforge.net/")
    (synopsis "Audio player for Musepack-formatted files")
    (description
     "Mpc123 is a command-line player for files in the Musepack audio
compression format (.mpc files.)")
    (license license:gpl2+)))
