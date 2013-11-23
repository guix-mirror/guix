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
  #:use-module (gnu packages geeqie)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages libjpeg)
  #:use-module (gnu packages libtiff)
  #:use-module (gnu packages oggvorbis)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages video)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public libextractor
  (package
   (name "libextractor")
   (version "1.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/libextractor/libextractor-"
                                version ".tar.gz"))
            (sha256
             (base32
              "1n7z6s5ils6xmf6b0z1xda41maxj94c1n6wlyyxmacs5lrkh2a96"))))
   (build-system gnu-build-system)
   ;; WARNING: Checks require /dev/shm to be in the build chroot, especially
   ;; not to be a symbolic link to /run/shm.
   ;; FIXME:
   ;; The following dependencies are all optional, but should be
   ;; available for maximum coverage:
   ;; * libarchive
   ;; * libgif (giflib)
   ;; * libgtk+ >= 3.0.0 (may probably drop glib then as a propagated input of
   ;;                     gtk)
   ;; * libgsf
   ;; * libmagic (file)
   ;; * libmpeg2
   ;; * libmp4v2
   ;; * librpm
   ;; * libsmf
   ;; * libtidy
   (inputs
    `(("exiv2" ,exiv2)
      ("flac" ,flac)
      ("ffmpeg" ,ffmpeg)
      ("gettext" ,gnu-gettext)
      ("glib" ,glib)
      ("gstreamer" ,gstreamer)
      ("gst-plugins-base" ,gst-plugins-base)
      ("libjpeg" ,libjpeg)
      ("libogg" ,libogg)
      ("libtiff" ,libtiff)
      ("libtool" ,libtool)
      ("libvorbis" ,libvorbis)
      ("pkg-config" ,pkg-config)
      ("zlib" ,zlib)))
   (synopsis "Library to extract meta-data from media files")
   (description
    "GNU Libextractor is a library for extracting metadata from files.  It
supports a very large number of file formats, including audio files, document
files, and archive files.  Each file format is implemented as a plugin, so
new formats can be added easily.  The package also containes a command-line
tool to extract metadata from a file and print the results.")
   (license license:gpl3+)
   (home-page "http://www.gnu.org/software/libextractor/")))

(define-public libmicrohttpd
  (package
   (name "libmicrohttpd")
   (version "0.9.31")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/libmicrohttpd/libmicrohttpd-"
                                version ".tar.gz"))
            (sha256
             (base32
              "06sxxial1794589k0ahi7nhhyfp14jf4jwirf6bkxqhs138pghfa"))))
   (build-system gnu-build-system)
   (inputs
    `(("curl" ,curl)
      ("gnutls" ,gnutls)
      ("libgcrypt" ,libgcrypt)
      ("openssl" ,openssl)
      ("zlib" ,zlib)))
   (arguments
    `(#:parallel-tests? #f))
   (synopsis "C library implementing an HTTP 1.1 server")
   (description
    "GNU libmicrohttpd is a small, embeddable HTTP server implemented as a
C library.  It makes it easy to run an HTTP server as part of another
application.  The library is fully HTTP 1.1 compliant.  It can listen on
multiple ports, supports four different threading models, and supports
IPv6.  It
also features security features such as basic and digest authentication
and support for SSL3 and TLS.")
   (license license:lgpl2.1+)
   (home-page "http://www.gnu.org/software/libmicrohttpd/")))
