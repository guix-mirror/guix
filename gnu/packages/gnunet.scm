;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Sree Harsha Totakura <sreeharsha@totakura.in>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages geeqie)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public libextractor
  (package
   (name "libextractor")
   (version "1.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/libextractor/libextractor-"
                                version ".tar.gz"))
            (sha256
             (base32
              "0zvv7wd011npcx7yphw9bpgivyxz6mlp87a57n96nv85k96dd2l6"))))
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
      ("zlib" ,zlib)))
   (native-inputs
      `(("pkg-config" ,pkg-config)))
   (synopsis "Library to extract meta-data from media files")
   (description
    "GNU libextractor is a library for extracting metadata from files.  It
supports a very large number of file formats, including audio files, document
files, and archive files.  Each file format is implemented as a plugin, so
new formats can be added easily.  The package also contains a command-line
tool to extract metadata from a file and print the results.")
   (license license:gpl3+)
   (home-page "http://www.gnu.org/software/libextractor/")))

(define-public libmicrohttpd
  (package
   (name "libmicrohttpd")
   (version "0.9.37")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/libmicrohttpd/libmicrohttpd-"
                                version ".tar.gz"))
            (sha256
             (base32
              "1p3wnhr43v6vqdgl86r76298wjfxz2ihj9zh9kpz8l7va30br357"))))
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

(define-public gnurl
  (package
   (name "gnurl")
   (version "7.37.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://gnunet.org/sites/default/files/gnurl-"
                                version ".tar.gz"))
            (sha256
             (base32 "1l2q9ih63vkm65zn886kmhqsx906pzx3qjvsxymlmf18kiv18pfd"))))
   (build-system gnu-build-system)
   (inputs `(("gnutls" ,gnutls)
             ("libidn" ,libidn)
             ("zlib" ,zlib)))
   (native-inputs
    `(("groff" ,groff)
      ("perl" ,perl)
      ("pkg-config" ,pkg-config)
      ("python" ,python-2)))
   (arguments
    `(#:configure-flags '("--enable-ipv6" "--with-gnutls" "--without-libssh2"
                          "--without-libmetalink" "--without-winidn"
                          "--without-librtmp" "--without-nghttp2"
                          "--without-nss" "--without-cyassl"
                          "--without-polarssl" "--without-ssl"
                          "--without-winssl" "--without-darwinssl"
                          "--disable-sspi" "--disable-ntlm-wb"
                          "--disable-ldap" "--disable-rtsp" "--disable-dict"
                          "--disable-telnet" "--disable-tftp" "--disable-pop3"
                          "--disable-imap" "--disable-smtp" "--disable-gopher"
                          "--disable-file" "--disable-ftp")
     #:test-target "test"
     #:parallel-tests? #f
     ;; We have to patch runtests.pl in tests/ directory
     #:phases
      (alist-cons-before
       'check 'patch-runtests
       (lambda _
         (substitute* "tests/runtests.pl"
                      (("/bin/sh") (which "sh"))))
       %standard-phases)))
   (synopsis "Microfork of cURL with support for the HTTP/HTTPS/GnuTLS subset of cURL")
   (description
    "Gnurl is a microfork of cURL, a command line tool for transferring data
with URL syntax. While cURL supports many crypto backends, libgnurl only
supports HTTPS, HTTPS and GnuTLS.")
   (license (license:bsd-style "file://COPYING"
                       "See COPYING in the distribution."))
   (home-page "https://gnunet.org/gnurl")))

(define-public gnunet
  (package
   (name "gnunet")
   (version "0.10.0")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnunet/gnunet-" version
                          ".tar.gz"))
      (sha256 (base32
               "0zqpc47kywhjrpphl0palz849khv00ra2gjrfkysp6p0gfsbvd0i"))
      (patches
       (list
        ;; Patch to fix serious bug in scheduler; upstream commit: #31747
        (search-patch "gnunet-fix-scheduler.patch")
        ;; Patch to fix bugs in testcases:
        ;; * Disable peerinfo-tool tests as they depend on reverse DNS lookups
        ;; * Allow revocation and integration-tests testcases to run on
        ;;   loopback; upstream: #32130, #32326
        ;; * Skip GNS testcases requiring DNS lookups; upstream: #32118
        (search-patch "gnunet-fix-tests.patch")))
      (patch-flags '("-p0"))))
   (build-system gnu-build-system)
   (inputs
    `(("glpk" ,glpk)
      ("gnurl" ,gnurl)
      ("gnutls" ,gnutls)
      ("libextractor" ,libextractor)
      ("libgcrypt" ,libgcrypt)
      ("libidn" ,libidn)
      ("libmicrohttpd" ,libmicrohttpd)
      ("libtool" ,libtool)
      ("libunistring" ,libunistring)
      ("openssl" ,openssl)
      ("opus" ,opus)
      ("pulseaudio", pulseaudio)
      ("sqlite" ,sqlite)
      ("zlib" ,zlib)))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("python" ,python-2)))
   (arguments
    '(#:phases
        ;; swap check and install phases and set paths to installed binaries
        (alist-cons-before
         'check 'set-path-for-check
         (lambda* (#:key outputs #:allow-other-keys)
          (let ((out (assoc-ref outputs "out")))
           (setenv "GNUNET_PREFIX" out)
           (setenv "PATH" (string-append (getenv "PATH") ":" out "/bin"))))
         (alist-cons-after
          'install 'check
          (assoc-ref %standard-phases 'check)
          (alist-delete
           'check
           %standard-phases)))))
   (synopsis "Secure, decentralized, peer-to-peer networking framework")
   (description
    "GNUnet is a framework for secure peer-to-peer networking that does not
use any centralized or otherwise trusted services.  Our high-level goal is to
provide a strong free software foundation for a global network that provides
security and privacy.  GNUnet started with an idea for anonymous
censorship-resistant file-sharing, but has grown to incorporate other
applications as well as many generic building blocks for secure networking
applications.  In particular, GNUnet now includes the GNU Name System, a
privacy-preserving, decentralized public key infrastructure.")
   (license license:gpl3+)
   (home-page "https://gnunet.org/")))
