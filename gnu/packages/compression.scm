;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages compression)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages which))

(define-public zlib
  (package
    (name "zlib")
    (version "1.2.7")
    (source
     (origin
      (method url-fetch)
      (uri (list (string-append "http://zlib.net/zlib-"
                                 version ".tar.gz")
                 (string-append "mirror://sourceforge/libpng/zlib-"
                                 version ".tar.gz")))
      (sha256
       (base32
        "1i96gsdvxqb6skp9a58bacf1wxamwi9m9pg4yn7cpf7g7239r77s"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (alist-replace
                 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Zlib's home-made `configure' fails when passed
                   ;; extra flags like `--enable-fast-install', so we need to
                   ;; invoke it with just what it understand.
                   (let ((out (assoc-ref outputs "out")))
                     ;; 'configure' doesn't understand '--host'.
                     ,@(if (%current-target-system)
                           `((setenv "CHOST" ,(%current-target-system)))
                           '())
                     (zero?
                      (system* "./configure"
                               (string-append "--prefix=" out)))))
                 %standard-phases)))
    (home-page "http://zlib.net/")
    (synopsis "Compression library")
    (description
     "zlib is designed to be a free, general-purpose, legally unencumbered --
that is, not covered by any patents -- lossless data-compression library for
use on virtually any computer hardware and operating system.  The zlib data
format is itself portable across platforms.  Unlike the LZW compression method
used in Unix compress(1) and in the GIF image format, the compression method
currently used in zlib essentially never expands the data. (LZW can double or
triple the file size in extreme cases.)  zlib's memory footprint is also
independent of the input data and can be reduced, if necessary, at some cost
in compression.")
    (license license:zlib)))

(define-public gzip
  (package
   (name "gzip")
   (version "1.6")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/gzip/gzip-"
                                version ".tar.gz"))
            (sha256
             (base32
              "0zlgdm4v3dndrbiz7b67mbbj25dpwqbmbzjiycssvrfrcfvq7swp"))))
   (build-system gnu-build-system)
   (synopsis "General file (de)compression (using lzw)")
   (arguments
    ;; FIXME: The test suite wants `less', and optionally Perl.
    '(#:tests? #f))
   (description
    "GNU Gzip provides data compression and decompression utilities; the
typical extension is \".gz\".  Unlike the \"zip\" format, it compresses a single
file; as a result, it is often used in conjunction with \"tar\", resulting in
\".tar.gz\" or \".tgz\", etc.")
   (license license:gpl3+)
   (home-page "http://www.gnu.org/software/gzip/")))

(define-public bzip2
  (let ((fix-man-dir
         ;; Move man pages to $out/share/.
         '(lambda* (#:key outputs #:allow-other-keys)
            (with-directory-excursion (assoc-ref outputs "out")
              (mkdir "share")
              (rename-file "man" "share/man"))))
        (build-shared-lib
         ;; Build a shared library.
         '(lambda* (#:key inputs #:allow-other-keys)
            (patch-makefile-SHELL "Makefile-libbz2_so")
            (zero? (system* "make" "-f" "Makefile-libbz2_so"))))
        (install-shared-lib
         '(lambda* (#:key outputs #:allow-other-keys)
            (let* ((out    (assoc-ref outputs "out"))
                   (libdir (string-append out "/lib")))
              (for-each (lambda (file)
                          (let ((base (basename file)))
                            (format #t "installing `~a' to `~a'~%"
                                    base libdir)
                            (copy-file file
                                       (string-append libdir "/" base))))
                        (find-files "." "^libbz2\\.so")))))
        (set-cross-environment
         '(lambda* (#:key target #:allow-other-keys)
            (substitute* (find-files "." "Makefile")
              (("CC=.*$")
               (string-append "CC = " target "-gcc\n"))
              (("AR=.*$")
               (string-append "AR = " target "-ar\n"))
              (("RANLIB=.*$")
               (string-append "RANLIB = " target "-ranlib\n"))
              (("^all:(.*)test" _ prerequisites)
               ;; Remove 'all' -> 'test' dependency.
               (string-append "all:" prerequisites "\n"))))))
    (package
      (name "bzip2")
      (version "1.0.6")
      (source (origin
               (method url-fetch)
               (uri (string-append "http://www.bzip.org/" version "/bzip2-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "1kfrc7f0ja9fdn6j1y6yir6li818npy6217hvr3wzmnmzhs8z152"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (srfi srfi-1))
         #:phases
         ,(if (%current-target-system)

              ;; Cross-compilation: use the cross tools.
              `(alist-cons-before
                'build 'build-shared-lib ,build-shared-lib
                (alist-cons-after
                 'install 'fix-man-dir ,fix-man-dir
                 (alist-cons-after
                  'install 'install-shared-lib ,install-shared-lib
                  (alist-replace 'configure ,set-cross-environment
                                 %standard-phases))))

              ;; Native compilation: build the shared library.
              `(alist-cons-before
                'build 'build-shared-lib ,build-shared-lib
                (alist-cons-after
                 'install 'fix-man-dir ,fix-man-dir
                 (alist-cons-after
                  'install 'install-shared-lib ,install-shared-lib
                  (alist-delete 'configure %standard-phases)))))

         #:make-flags (list (string-append "PREFIX="
                                           (assoc-ref %outputs "out")))

         ;; Don't attempt to run the tests when cross-compiling.
         ,@(if (%current-target-system)
               '(#:tests? #f)
               '())))
      (synopsis "High-quality data compression program")
      (description
       "bzip2 is a freely available, patent free (see below), high-quality data
compressor.  It typically compresses files to within 10% to 15% of the best
available techniques (the PPM family of statistical compressors), whilst
being around twice as fast at compression and six times faster at
decompression.")
      (license (license:bsd-style "file://LICENSE"
                                  "See LICENSE in the distribution."))
      (home-page "http://www.bzip.org/"))))

(define-public xz
  (package
   (name "xz")
   (version "5.0.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://tukaani.org/xz/xz-" version
                                ".tar.gz"))
            (sha256
             (base32
              "1dl35ca8fdss9z2d6y234gxh24ixq904xksizrjmjr5dimwhax6n"))))
   (build-system gnu-build-system)
   (synopsis "General-purpose data compression")
   (description
    "XZ Utils is free general-purpose data compression software with high
compression ratio.  XZ Utils were written for POSIX-like systems, but also
work on some not-so-POSIX systems.  XZ Utils are the successor to LZMA Utils.

The core of the XZ Utils compression code is based on LZMA SDK, but it has
been modified quite a lot to be suitable for XZ Utils.  The primary
compression algorithm is currently LZMA2, which is used inside the .xz
container format.  With typical files, XZ Utils create 30 % smaller output
than gzip and 15 % smaller output than bzip2.")
   (license (list license:gpl2+ license:lgpl2.1+)) ; bits of both
   (home-page "http://tukaani.org/xz/")))

(define-public lzo
  (package
    (name "lzo")
    (version "2.06")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://www.oberhumer.com/opensource/lzo/download/lzo-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0wryshs446s7cclrbjykyj766znhcpnr7s3cxy33ybfn6vwfcygz"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--enable-shared")))
    (home-page "http://www.oberhumer.com/opensource/lzo")
    (synopsis
     "Data compresion library suitable for real-time data de-/compression")
    (description
     "LZO is a data compression library which is suitable for data
de-/compression in real-time.  This means it favours speed over
compression ratio.

LZO is written in ANSI C.  Both the source code and the compressed data
format are designed to be portable across platforms.")
    (license license:gpl2+)))

(define-public lzop
  (package
    (name "lzop")
    (version "1.03")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.lzop.org/download/lzop-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1jdjvc4yjndf7ihmlcsyln2rbnbaxa86q4jskmkmm7ylfy65nhn1"))))
    (build-system gnu-build-system)
    (inputs `(("lzo" ,lzo)))
    (home-page "http://www.lzop.org/")
    (synopsis "Compress or expand files")
    (description
     "Lzop is a file compressor which is very similar to gzip.  Lzop uses the
LZO data compression library for compression services, and its main advantages
over gzip are much higher compression and decompression speed (at the cost of
some compression ratio).")
    (license license:gpl2+)))

(define-public lzip
  (package
    (name "lzip")
    (version "1.16")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://savannah/lzip/lzip-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0l9724rw1l3hg2ldr3n7ihqich4m9nc6y7l302bvdj4jmxdw530j"))))
    (build-system gnu-build-system)
    (home-page "http://www.nongnu.org/lzip/lzip.html")
    (synopsis "Lossless data compressor based on the LZMA algorithm")
    (description
     "Lzip is a lossless data compressor with a user interface similar to the
one of gzip or bzip2.  Lzip decompresses almost as fast as gzip and compresses
more than bzip2, which makes it well suited for software distribution and data
archiving.  Lzip is a clean implementation of the LZMA algorithm.")
    (license license:gpl3+)))

(define-public sharutils
  (package
    (name "sharutils")
    (version "4.14")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/sharutils/sharutils-"
                          version ".tar.xz"))
      (sha256
       (base32
        "033sq1v0cp0bi1mp320xaqwd4fhakqc5747hh6qa1asjrzpqiqza"))))
    (build-system gnu-build-system)
    (inputs
     `(("which" ,which)))
    (arguments
     `(#:phases
        (alist-cons-after
         'patch-source-shebangs 'unpatch-source-shebang
         ;; revert the patch-shebang phase on a script which is
         ;; in fact test data
         (lambda _
           (substitute* "tests/shar-1.ok"
             (((which "sh")) "/bin/sh")))
         %standard-phases)))
    (home-page "http://www.gnu.org/software/sharutils/")
    (synopsis "Archives in shell scripts, uuencode/uudecode")
    (description
     "GNU sharutils is a package for creating and manipulating shell
archives that can be readily emailed.  A shell archive is a file that can be
processed by a Bourne-type shell to unpack the original collection of files. 
This package is mostly for compatibility and historical interest.")
    (license license:gpl3+)))

