;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages linux)               ;alsa-lib
  #:use-module (gnu packages video)               ;ffmpeg
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
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
            (patches (search-patches "libmad-armv7-thumb-pt1.patch"
                                     "libmad-armv7-thumb-pt2.patch"
                                     "libmad-frame-length.patch"
                                     "libmad-mips-newgcc.patch"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-before 'configure 'remove-unsupported-gcc-flags
          (lambda _
            ;; remove option that is not supported by gcc any more
            (substitute* "configure" ((" -fforce-mem") ""))
            #t)))))
   (synopsis "MPEG audio decoder")
   (description
    "MAD (MPEG Audio Decoder) supports MPEG-1 and the MPEG-2 extension to
lower sampling frequencies, as well as the de facto MPEG 2.5 format.
All three audio layers — Layer I, Layer II, and Layer III (i.e. MP3) — are
fully implemented.

This package contains the library.")
   (license license:gpl2+)
   (home-page "https://www.underbit.com/products/mad/")))

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
versions of ID3v2.")
   (license license:gpl2+)
   (home-page "https://www.underbit.com/products/mad/")))

(define-public id3lib
  (package
   (name "id3lib")
   (version "3.8.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/id3lib/id3lib/"
                                version "/id3lib-" version ".tar.gz"))
            (sha256
             (base32
              "0yfhqwk0w8q2hyv1jib1008jvzmwlpsxvc8qjllhna6p1hycqj97"))
            (modules '((guix build utils)))
            ;; Don't use bundled zlib
            (snippet '(begin (delete-file-recursively "zlib") #t))
            (patches (search-patches "id3lib-CVE-2007-4460.patch"))))
   (build-system gnu-build-system)
   (inputs `(("zlib" ,zlib)))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-before 'configure 'apply-patches
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
              (("dami;") "dami;\nusing namespace std;"))
            #t)))))
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
    (version "1.11.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://taglib.github.io/releases/taglib-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0ssjcdjv4qf9liph5ry1kngam1y7zp8fzr9xv4wzzrma22kabldn"))))
    (build-system cmake-build-system)
    (arguments
      '(#:tests? #f ; Tests are not ran with BUILD_SHARED_LIBS on.
        #:configure-flags (list "-DBUILD_SHARED_LIBS=ON")))
    (inputs `(("zlib" ,zlib)))
    (home-page "http://taglib.org")
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
                    "https://ibiblio.org"
                    "/pub/linux/apps/sound/mp3-utils/mp3info/mp3info-"
                    version ".tgz"))
              (sha256
               (base32
                "042f1czcs9n2sbqvg4rsvfwlqib2gk976mfa2kxlfjghx5laqf04"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "Makefile"
                    (("/bin/rm") "rm")
                    (("/usr/bin/install") "install")
                    (("man/man1") "share/man/man1"))
                  #t))))
    (build-system gnu-build-system)
    (outputs '("out" "gui"))                      ;GTK+ interface in "gui"
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("prefix=.*")
                  (string-append "prefix := " out "\n"))))
             #t))
         (add-before 'install 'pre-install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/bin"))
               (mkdir-p (string-append out "/share/man/man1")))
             #t))
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Move the GTK+ interface to "gui".
             (let ((out (assoc-ref outputs "out"))
                   (gui (assoc-ref outputs "gui")))
               (mkdir-p (string-append gui "/bin"))
               (rename-file (string-append out "/bin/gmp3info")
                            (string-append gui "/bin/gmp3info")))
             #t)))
        #:tests? #f))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+-2)
       ("ncurses" ,ncurses)))
    (home-page "https://www.ibiblio.org/mp3info/")
    (synopsis "MP3 technical info viewer and ID3 1.x tag editor")
    (description
     "MP3Info is a little utility used to read and modify the ID3 tags of MP3
files.  MP3Info can also display various technical aspects of an MP3 file
including playing time, bit-rate, sampling frequency and other attributes in a
pre-defined or user-specifiable output format.")
    (license license:gpl2+)))

(define-public libmp3splt
  (package
   (name "libmp3splt")
   (version "0.9.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/mp3splt/libmp3splt/"
                                version "/libmp3splt-"
                                version ".tar.gz"))
            (sha256
             (base32
              "1p1mn2hsmj5cp40fnc8g1yfvk72p8pjxi866gjdkgjsqrr7xdvih"))))
   (build-system gnu-build-system)
   (inputs `(("libid3tag" ,libid3tag)
             ("libmad" ,libmad)
             ("libogg" ,libogg)
             ("libltdl" ,libltdl)
             ("libvorbis" ,libvorbis)
             ("pcre" ,pcre)))
   (native-inputs
     `(("pkg-config" ,pkg-config)))
   (synopsis "Library for splitting mp3 and ogg vorbis files")
   (description
    "Mp3splt is a utility to split mp3 and ogg vorbis files selecting a begin
and an end time position, without decoding.  For splitting an album, one may
select split points and file names manually or obtain them automatically from
CDDB (internet or a local file) or from .cue files.  The tool also supports
automatic silence split, that can be used also to adjust cddb/cue splitpoints.

This package contains the library.")
   (license license:gpl2+)
   (home-page "http://mp3splt.sourceforge.net/mp3splt_page/home.php")))

(define-public mp3splt
  (package
   (name "mp3splt")
   (version "2.6.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/mp3splt/mp3splt/"
                                version "/mp3splt-"
                                version ".tar.gz"))
            (sha256
             (base32
              "1aiv20gypb6r84qabz8gblk8vi42cg3x333vk2pi3fyqvl82phry"))))
   (build-system gnu-build-system)
   (native-inputs `(("pkg-config" ,pkg-config)))
   (inputs `(("libmp3splt" ,libmp3splt)))
   (synopsis "Utility for splitting mp3 and ogg vorbis files")
   (description
    "Mp3splt is a utility to split mp3 and ogg vorbis files selecting a begin
and an end time position, without decoding.  For splitting an album, one may
select split points and file names manually or obtain them automatically from
CDDB (internet or a local file) or from .cue files.  The tool also supports
automatic silence split, that can be used also to adjust cddb/cue splitpoints.

This package contains the binary.")
   (license license:gpl2+)
   (home-page "http://mp3splt.sourceforge.net/mp3splt_page/home.php")))

(define-public mpg123
  (package
    (name "mpg123")
    (version "1.25.10")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "mirror://sourceforge/mpg123/mpg123/"
                                        version "/mpg123-" version ".tar.bz2")
                         (string-append
                          "https://www.mpg123.org/download/mpg123-"
                          version ".tar.bz2")))
              (sha256
               (base32
                "08vhp8lz7d9ybhxcmkq3adwfryhivfvp0745k4r9kgz4wap3f4vc"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--with-default-audio=pulse")))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("pulseaudio" ,pulseaudio)
              ("alsa-lib" ,alsa-lib)))
    (home-page "https://www.mpg123.org/")
    (synopsis "Console MP3 player and decoder library")
    (description
     "Mpg123 is a real time MPEG 1.0/2.0/2.5 audio player/decoder for layers
1,2 and 3 (MPEG 1.0 layer 3 aka MP3 most commonly tested).  It comes with a
command-line tool as well as a C library, libmpg123.")
    (license license:lgpl2.1)))

(define-public mpg321
  (package
    (name "mpg321")
    (version "0.3.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/mpg321/mpg321/"
                                 version "/mpg321-" version ".tar.gz"))
             (sha256
              (base32
               "0ki8mh76bbmdh77qsiw682dvi8y468yhbdabqwg05igmwc1wqvq5"))))
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
    (version "3.100")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/lame/lame/"
                                 (version-major+minor version) "/lame-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "07nsn5sy3a8xbmw1bidxnsj5fj6kg9ai04icmqw40ybkp353dznx"))))
    (build-system gnu-build-system)
    (home-page "http://lame.sourceforge.net/")
    (synopsis "MPEG Audio Layer III (MP3) encoder")
    (description "LAME is a high quality MPEG Audio Layer III (MP3) encoder.")
    (license license:lgpl2.0)))

(define-public ripperx
  (package
   (name "ripperx")
   (version "2.8.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/ripperx/ripperx/"
                                version "/ripperx-"
                                version ".tar.bz2"))
            (sha256
             (base32
              "1ss3c1a5hx6c99q1cryxg0jhbnbdj6ga9xyz0dzlz9qhzg5qswfs"))
            (patches (search-patches "ripperx-missing-file.patch"))))
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
      ("id3lib" ,id3lib)
      ("taglib" ,taglib)))
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (synopsis "GTK program to rip and encode CD audio tracks")
   (description
    "RipperX is a GTK program to rip CD audio tracks and encode them to the
Ogg, MP3, or FLAC formats.  Its goal is to be easy to use, requiring only
a few mouse clicks to convert an entire album.  It supports CDDB lookups
for album and track information.")
   (license license:gpl2)
   (home-page "https://sourceforge.net/projects/ripperx/")))

(define-public libmpcdec
  (package
    (name "libmpcdec")
    (version "1.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://files.musepack.net/source/libmpcdec-"
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
    (home-page "https://musepack.net")))

(define-public mpc123
  (package
    (name "mpc123")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "mirror://sourceforge/mpc123/version%20"
                                        version "/mpc123-" version ".tar.gz")
                         (string-append "mirror://debian/pool/main/m/" name
                                        "/" name "_" version ".orig.tar.gz")))
              (sha256
               (base32
                "0sf4pns0245009z6mbxpx7kqy4kwl69bc95wz9v23wgappsvxgy1"))
              (patches (search-patches "mpc123-initialize-ao.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (substitute* "Makefile"
               (("CC[[:blank:]]*:=.*")
                "CC := gcc\n"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "mpc123" bin)))))
       #:tests? #f))
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (inputs
     `(("libao" ,ao)
       ("libmpcdec" ,libmpcdec)))
    (home-page "https://github.com/bucciarati/mpc123")
    (synopsis "Audio player for Musepack-formatted files")
    (description
     "Mpc123 is a command-line player for files in the Musepack audio
compression format (.mpc files).")
    (license license:gpl2+)))

(define-public eyed3
  (package
    (name "eyed3")
    (version "0.8.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "eyeD3" version))
       (sha256
        (base32 "1jb22n1jczxgbpcnfiw12r8dcs74556g1d09mzms44f52kgs7lgc"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))    ; the required test data contains copyrighted material
    (propagated-inputs
     `(("python-grako" ,python-grako)
       ("python-magic" ,python-magic)
       ("python-pathlib" ,python-pathlib)
       ("python-six" ,python-six)))
    (synopsis "MP3 tag ID3 metadata editor")
    (description "eyeD3 is a Python tool for working with audio files,
specifically mp3 files containing ID3 metadata (i.e. song info).  It provides a
command-line tool (eyeD3) and a Python library (import eyed3) that can be used
to write your own applications or plugins that are callable from the
command-line tool.")
    (home-page "https://eyed3.readthedocs.io/en/latest/")
    (license license:gpl2+)))

(define-public chromaprint
  (package
    (name "chromaprint")
    (version "1.4.3")
    (source (origin
      (method url-fetch)
      (uri (string-append
            "https://github.com/acoustid/chromaprint/releases/download/v"
            version "/chromaprint-" version ".tar.gz"))
      (sha256
       (base32
        "10kz8lncal4s2rp2rqpgc6xyjp0jzcrihgkx7chf127vfs5n067a"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; tests require googletest *sources*
       ;;#:configure-flags '("-DBUILD_TESTS=ON") ; for building the tests
       #:configure-flags '("-DBUILD_TOOLS=ON") ; for fpcalc
       #:test-target "check"))
    (inputs
     ;; requires one of FFmpeg (prefered), FFTW3 or vDSP
     ;; use the same ffmpeg version as for acoustid-fingerprinter
     `(("ffmpeg" ,ffmpeg)
       ("boost" ,boost)))
    (home-page "https://acoustid.org/chromaprint")
    (synopsis "Audio fingerprinting library")
    (description "Chromaprint is a library for calculating audio
fingerprints which are used by the Acoustid service.  Its main purpose
is to provide an accurate identifier for record tracks.")
    (license license:lgpl2.1+)))
