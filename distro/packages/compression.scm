;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (distro packages compression)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public zlib
  (package
    (name "zlib")
    (version "1.2.7")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://zlib.net/zlib-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1i96gsdvxqb6skp9a58bacf1wxamwi9m9pg4yn7cpf7g7239r77s"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (alist-replace
                 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Zlib's home-made `configure' doesn't fails when passed
                   ;; extra flags like `--enable-fast-install', so we need to
                   ;; invoke it with just what it understand.
                   (let ((out (assoc-ref outputs "out")))
                     (zero? (system* "./configure"
                                     (string-append "--prefix=" out)))))
                 %standard-phases)))
    (home-page "http://zlib.net/")
    (synopsis "The zlib compression library")
    (description
     "zlib is designed to be a free, general-purpose, legally unencumbered --
that is, not covered by any patents -- lossless data-compression library for
use on virtually any computer hardware and operating system.  The zlib data
format is itself portable across platforms. Unlike the LZW compression method
used in Unix compress(1) and in the GIF image format, the compression method
currently used in zlib essentially never expands the data. (LZW can double or
triple the file size in extreme cases.)  zlib's memory footprint is also
independent of the input data and can be reduced, if necessary, at some cost
in compression.")

    ;; See <http://zlib.net/zlib_license.html>.
    (license "permissive")))

(define-public gzip
  (package
   (name "gzip")
   (version "1.5")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/gzip/gzip-"
                                version ".tar.gz"))
            (sha256
             (base32
              "18rm80kar7n016g8bsyy1a3zk50i2826xdgs874yh64rzj7nxmdm"))))
   (build-system gnu-build-system)
   (synopsis "Gzip, the GNU zip compression program")
   (arguments
    ;; FIXME: The test suite wants `less', and optionally Perl.
    '(#:tests? #f))
   (description
    "gzip (GNU zip) is a popular data compression program written by Jean-loup
Gailly for the GNU project.  Mark Adler wrote the decompression part.

We developed this program as a replacement for compress because of the Unisys
and IBM patents covering the LZW algorithm used by compress.  These patents
made it impossible for us to use compress, and we needed a replacement.  The
superior compression ratio of gzip is just a bonus.")
   (license "GPLv3+")
   (home-page "http://www.gnu.org/software/gzip/")))

(define-public bzip2
  (let ((fix-man-dir
         ;; Move man pages to $out/share/.
         '(lambda* (#:key outputs #:allow-other-keys)
            (with-directory-excursion (assoc-ref outputs "out")
              (mkdir "share")
              (rename-file "man" "share"))))
        (build-shared-lib
         ;; Build a shared library.
         '(lambda* (#:key inputs #:allow-other-keys)
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
                        (find-files "." "^libbz2\\.so"))))))
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
         (alist-cons-before
          'build 'build-shared-lib ,build-shared-lib
          (alist-cons-after
           'install 'fix-man-dir ,fix-man-dir
           (alist-cons-after
            'install 'install-shared-lib ,install-shared-lib
            (alist-delete 'configure %standard-phases))))
         #:make-flags (list (string-append "PREFIX="
                                           (assoc-ref %outputs "out")))))
      (synopsis "high-quality data compression program")
      (description
       "bzip2 is a freely available, patent free (see below), high-quality data
compressor.  It typically compresses files to within 10% to 15% of the best
available techniques (the PPM family of statistical compressors), whilst
being around twice as fast at compression and six times faster at
decompression.")
      (license "BSD-style")
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
   (synopsis
    "XZ, general-purpose data compression software, successor of LZMA")
   (description
    "XZ Utils is free general-purpose data compression software with high
compression ratio.  XZ Utils were written for POSIX-like systems, but also
work on some not-so-POSIX systems.  XZ Utils are the successor to LZMA Utils.

The core of the XZ Utils compression code is based on LZMA SDK, but it has
been modified quite a lot to be suitable for XZ Utils.  The primary
compression algorithm is currently LZMA2, which is used inside the .xz
container format.  With typical files, XZ Utils create 30 % smaller output
than gzip and 15 % smaller output than bzip2.")
   (license '("GPLv2+" "LGPLv2.1+"))              ; bits of both
   (home-page "http://tukaani.org/xz/")))
