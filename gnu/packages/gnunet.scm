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

(define-module (gnu packages gnunet)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module ((gnu packages gettext)
                #:renamer (symbol-prefix-proc 'gnu:))
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages libjpeg)
  #:use-module (gnu packages libtiff)
  #:use-module (gnu packages oggvorbis)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages pkg-config)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public libextractor
  (package
   (name "libextractor")
   (version "1.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/libextractor/libextractor-"
                                version ".tar.gz"))
            (sha256
             (base32
              "1zvj64ig456c9ya3r8ib48ms42cnli9y7ig5p04xqm16z7vw5dyb"))))
   (build-system gnu-build-system)
   ;; WARNING: Checks require /dev/shm to be in the build chroot, especially
   ;; not to be a symbolic link to /run/shm.
   ;; FIXME:
   ;; The following dependencies are all optional, but should be
   ;; available for maximum coverage:
   ;; * libarchive
   ;; * libavutil / libavformat / libavcodec / libswscale (ffmpeg)
   ;; * libexiv2
   ;; * libgif (giflib)
   ;; * libgtk+ >= 3.0.0 (may probably drop glib then as a propagated input of
   ;;                     gtk)
   ;; * libgsf
   ;; * libgstreamer
   ;; * libmagic (file)
   ;; * libmpeg2
   ;; * libmp4v2
   ;; * librpm
   ;; * libsmf
   ;; * libtidy
   (inputs
    `(("gettext" ,gnu:gettext)
      ("flac" ,flac)
      ("glib" ,glib)
      ("libjpeg" ,libjpeg)
      ("libogg" ,libogg)
      ("libtiff" ,libtiff)
      ("libtool" ,libtool)
      ("libvorbis" ,libvorbis)
      ("pkg-config" ,pkg-config)
      ("zlib" ,zlib)))
   (synopsis "Library to extract meta-data from media files")
   (description
    "GNU Libextractor is a library used to extract meta data from files.
The goal is to provide developers of file-sharing networks, browsers or
WWW-indexing bots with a universal library to obtain simple keywords and
meta data to match against queries and to show to users instead of only
relying on filenames. libextractor contains the shell command extract that,
similar to the well-known file command, can extract meta data from a file
and print the results to stdout.

Currently, libextractor supports the following formats: HTML, MAN, PS, DVI,
OLE2 (DOC, XLS, PPT), OpenOffice (sxw), StarOffice (sdw), FLAC,
MP3 (ID3v1 and ID3v2), OGG, WAV, S3M (Scream Tracker 3), XM (eXtended Module),
IT (Impulse Tracker), NSF(E) (NES music), SID (C64 music), EXIV2, JPEG, GIF,
PNG, TIFF, DEB, RPM, TAR(.GZ), LZH, LHA, RAR, ZIP, CAB, 7-ZIP, AR, MTREE,
PAX, CPIO, ISO9660, SHAR, RAW, XAR FLV, REAL, RIFF (AVI), MPEG, QT and ASF.
Also, various additional MIME types are detected.")
   (license license:gpl3+)
   (home-page "http://www.gnu.org/software/libextractor/")))

(define-public libmicrohttpd
  (package
   (name "libmicrohttpd")
   (version "0.9.30")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/libmicrohttpd/libmicrohttpd-"
                                version ".tar.gz"))
            (sha256
             (base32
              "0v30w90qx8wpg5ksy97f5r4acpwd4q7q2v508mcss00vzj18rx40"))))
   (build-system gnu-build-system)
   (inputs
    `(("curl" ,curl)
      ("gnutls" ,gnutls)
      ("libgcrypt" ,libgcrypt)
      ("openssl" ,openssl)
      ("zlib" ,zlib)))
   (synopsis "C library implementing an HTTP 1.1 server")
   (description
    "GNU libmicrohttpd is a small C library that is supposed to make it
easy to run an HTTP server as part of another application.  Key features
that distinguish GNU Libmicrohttpd from other projects are:
C library is fast and small;
API is simple, expressive and fully reentrant;
implementation is HTTP 1.1 compliant;
HTTP server can listen on multiple ports;
four different threading models (select, poll, pthread, thread pool);
supported platforms include GNU/Linux, FreeBSD, OpenBSD, NetBSD, Android,
OS X, W32, Symbian and z/OS;
support for IPv6;
support for SHOUTcast;
support for incremental processing of POST data (optional);
support for basic and digest authentication (optional);
support for SSL3 and TLS (requires libgcrypt and libgnutls, optional);
binary is only about 32k (without TLS/SSL support and other optional features).")
   (license license:lgpl2.1+)
   (home-page "http://www.gnu.org/software/libmicrohttpd/")))
