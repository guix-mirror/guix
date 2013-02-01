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

