;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Sree Harsha Totakura <sreeharsha@totakura.in>
;;; Copyright © 2015, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2017, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017, 2018 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2016, 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
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
  #:use-module (gnu packages file)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages backup)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu))

(define-public libextractor
  (package
   (name "libextractor")
   (version "1.8")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/libextractor/libextractor-"
                                version ".tar.gz"))
            (patches (search-patches "libextractor-CVE-2018-20430.patch"
                                     "libextractor-CVE-2018-20431.patch"))
            (sha256
             (base32
              "1z1cb35griqzvshqdv5ck98dy0sgpsswn7fgiy7lbzi34sma8dg2"))))
   (build-system gnu-build-system)
   ;; WARNING: Checks require /dev/shm to be in the build chroot, especially
   ;; not to be a symbolic link to /run/shm.
   ;; FIXME:
   ;; The following dependencies are all optional, but should be
   ;; available for maximum coverage:
   ;; * libmagic (file)
   ;; * librpm (rpm)    ; investigate failure
   ;; * libgif (giflib) ; investigate failure
   (inputs
    `(("exiv2" ,exiv2)
      ("bzip2" ,bzip2)
      ("flac" ,flac)
      ("ffmpeg" ,ffmpeg-3.4)
      ("file" ,file)                           ;libmagic, for the MIME plug-in
      ("glib" ,glib)
      ("gstreamer" ,gstreamer)
      ("gst-plugins-base" ,gst-plugins-base)
      ("gtk+" ,gtk+)
      ("libarchive" ,libarchive)
      ("libgsf" ,libgsf)
      ("libjpeg" ,libjpeg)
      ("libltdl" ,libltdl)
      ("libmpeg2" ,libmpeg2)
      ("libmp4v2" ,libmp4v2)
      ("libsmf" ,libsmf)
      ("tidy-html" ,tidy-html)
      ("libogg" ,libogg)
      ("libtiff" ,libtiff)
      ("libvorbis" ,libvorbis)
      ("zlib" ,zlib)))
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (outputs '("out"
              "static")) ; 396 KiB .a files
   (arguments
    `(#:configure-flags
      (list (string-append "--with-ltdl="
                           (assoc-ref %build-inputs "libltdl"))
            (string-append "--with-tidy="
                           (assoc-ref %build-inputs "tidy-html")))
      #:parallel-tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'install 'move-static-libraries
          (lambda* (#:key outputs #:allow-other-keys)
            ;; Move static libraries to the "static" output.
            (let* ((out    (assoc-ref outputs "out"))
                   (lib    (string-append out "/lib"))
                   (static (assoc-ref outputs "static"))
                   (slib   (string-append static "/lib")))
              (mkdir-p slib)
              (for-each (lambda (file)
                          (install-file file slib)
                          (delete-file file))
                        (find-files lib "\\.a$"))
              #t))))))
   (synopsis "Library to extract meta-data from media files")
   (description
    "GNU libextractor is a library for extracting metadata from files.  It
supports a very large number of file formats, including audio files, document
files, and archive files.  Each file format is implemented as a plugin, so
new formats can be added easily.  The package also contains a command-line
tool to extract metadata from a file and print the results.")
   (license license:gpl3+)
   (home-page "https://www.gnu.org/software/libextractor/")))

(define-public libmicrohttpd
  (package
   (name "libmicrohttpd")
   (version "0.9.63")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/libmicrohttpd/libmicrohttpd-"
                                version ".tar.gz"))
            (sha256
             (base32
              "173lj66crwvhzwp1r812sc5h1hda7g6lb9l1y5zf7w3pw4dnzhrp"))))
   (build-system gnu-build-system)
   (inputs
    `(("curl" ,curl)
      ("gnutls" ,gnutls/dane)
      ("libgcrypt" ,libgcrypt)
      ("openssl" ,openssl)
      ("zlib" ,zlib)))
   (synopsis "C library implementing an HTTP 1.1 server")
   (description
    "GNU libmicrohttpd is a small, embeddable HTTP server implemented as a
C library.  It makes it easy to run an HTTP server as part of another
application.  The library is fully HTTP 1.1 compliant.  It can listen on
multiple ports, supports four different threading models, and supports
IPv6.  It also features security features such as basic and digest
authentication and support for SSL3 and TLS.")
   (license license:lgpl2.1+)
   (home-page "https://www.gnu.org/software/libmicrohttpd/")))

(define-public gnurl
  (package
   (name "gnurl")
   (version "7.63.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/gnunet/" name "-" version ".tar.Z"))
            (sha256
             (base32
              "021b3pdfnqywk5q07y48kxyz7g4jjg35dk3cv0ps0x50qjr4ix33"))))
   (build-system gnu-build-system)
   (outputs '("out"
              "doc"))                             ; 1.7 MiB of man3 pages
   (inputs `(("gnutls" ,gnutls/dane)
             ("libidn" ,libidn)
             ("zlib" ,zlib)))
   (native-inputs
    `(("libtool" ,libtool)
      ("groff" ,groff)
      ("perl" ,perl)
      ("pkg-config" ,pkg-config)
      ("python" ,python-2)))
   (arguments
    `(#:configure-flags (list "--disable-ntlm-wb")
      #:test-target "test"
      #:parallel-tests? #f
      #:phases
      ;; We have to patch runtests.pl in tests/ directory
      (modify-phases %standard-phases
        (add-after 'install 'move-man3-pages
          (lambda* (#:key outputs #:allow-other-keys)
            ;; Move section 3 man pages to "doc".
            (let ((out (assoc-ref outputs "out"))
                  (doc (assoc-ref outputs "doc")))
              (mkdir-p (string-append doc "/share/man"))
              (rename-file (string-append out "/share/man/man3")
                           (string-append doc "/share/man/man3"))
              #t)))
        (replace 'check
          (lambda _
            (substitute* "tests/runtests.pl"
              (("/bin/sh") (which "sh")))

            ;; Make test output more verbose.
            (invoke "make" "-C" "tests" "test"))))))
   (synopsis "Microfork of cURL with support for the HTTP/HTTPS/GnuTLS subset of cURL")
   (description
    "Gnurl is a microfork of cURL, a command line tool for transferring data
with URL syntax.  While cURL supports many crypto backends, libgnurl only
supports HTTP, HTTPS and GnuTLS.")
   (license (license:non-copyleft "file://COPYING"
                                  "See COPYING in the distribution."))
   (properties '((ftp-server . "ftp.gnu.org")
                 (ftp-directory . "/gnunet")))
   (home-page "https://gnunet.org/gnurl")))

(define-public gnunet
  (package
   (name "gnunet")
   (version "0.10.1")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnunet/gnunet-" version
                          ".tar.gz"))
      (sha256
       (base32
        "04wxzm3wkgqbn42b8ksr4cx6m5cckyig5cls1adh0nwdczwvnp7n"))))
   (build-system gnu-build-system)
   (inputs
    `(("glpk" ,glpk)
      ("gnurl" ,gnurl)
      ("gstreamer" ,gstreamer)
      ("gst-plugins-base" ,gst-plugins-base)
      ("gnutls" ,gnutls/dane)
      ("libextractor" ,libextractor)
      ("libgcrypt" ,libgcrypt)
      ("libidn" ,libidn)
      ("libmicrohttpd" ,libmicrohttpd) ; hostlist, pt, contrib, and more
      ("libltdl" ,libltdl)
      ("libunistring" ,libunistring) ; fs and more
      ("openssl" ,openssl) ; transport, certificate creation, contribs
      ("opus" ,opus) ; gnunet-conversation
      ("pulseaudio" ,pulseaudio) ; conversation
      ("sqlite" ,sqlite) ; sqlite bindings, *store
      ("zlib" ,zlib)
      ("perl" ,perl) ; doxygen and more
      ("jansson" ,jansson) ; identity, taler (external), gnunet-json, gns
      ("nss" ,nss) ; gns
      ("gmp" ,gmp) ; util
      ("bluez" ,bluez) ; gnunet-transport
      ("glib" ,glib)
      ("libogg" ,libogg) ; gnunet-conversation
      ("python-2" ,python-2))) ; tests, gnunet-qr
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (arguments
    '(#:configure-flags
      (list (string-append "--with-nssdir=" %output "/lib"))
      #:parallel-tests? #f
      ;; test_gnunet_service_arm fails; reported upstream
      #:tests? #f
      #:phases
      (modify-phases %standard-phases
        ;; swap check and install phases and set paths to installed binaries
        (add-before 'check 'set-path-for-check
          (lambda* (#:key outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
             (setenv "GNUNET_PREFIX" (string-append out "/lib"))
             (setenv "PATH" (string-append (getenv "PATH") ":" out "/bin")))
           #t))
        (add-after 'install 'check
          (assoc-ref %standard-phases 'check))
        (delete 'check))))
   (synopsis "Secure, decentralized, peer-to-peer networking framework")
   (description
     "GNUnet is a framework for secure peer-to-peer networking.  The
high-level goal is to provide a strong foundation of free software for a
global, distributed network that provides security and privacy.  GNUnet in
that sense aims to replace the current internet protocol stack.  Along with
an application for secure publication of files, it has grown to include all
kinds of basic applications for the foundation of a GNU internet.")
   (license license:gpl3+)
   (home-page "https://gnunet.org/")))

(define-public guile-gnunet                       ;GSoC 2015!
  (let ((commit "d12167ab3c8d7d6caffd9c606e389ef043760602")
        (revision "1"))
    (package
      (name "guile-gnunet")
      (version (git-version "0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/guix/gnunet.git/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0nqc18jh9j30y4l6yh6j35byfg6qalq7yr3frv9rk10qa041c2sv"))))
      (build-system gnu-build-system)
      (native-inputs `(("pkg-config" ,pkg-config)
                       ("autoconf" ,autoconf-wrapper)
                       ("automake" ,automake)))
      (inputs `(("guile" ,guile-2.0)
                ("gnunet" ,gnunet)))
      (synopsis "Guile bindings for GNUnet services")
      (description
       "This package provides Guile bindings to the client libraries of various
GNUnet services, including the @dfn{identity} and @dfn{file sharing}
services.")
      (home-page "https://gnu.org/software/guix")
      (license license:gpl3+))))

;; FIXME: "gnunet-setup" segfaults under certain conditions and "gnunet-gtk"
;; does not seem to be fully functional.  This has been reported upstream:
;; http://lists.gnu.org/archive/html/gnunet-developers/2016-02/msg00004.html
(define-public gnunet-gtk
  (package (inherit gnunet)
    (name "gnunet-gtk")
    (version (package-version gnunet))
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gnunet/gnunet-gtk-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1p38k1s6a2fmcfc9a7cf1zrdycm9h06kqdyand4s3k500nj6mb4g"))))
    (arguments
     `(#:configure-flags
       (list "--with-libunique"
             "--with-qrencode"
             (string-append "--with-gnunet="
                            (assoc-ref %build-inputs "gnunet")))))
    (inputs
     `(("gnunet" ,gnunet)
       ("libgcrypt" ,libgcrypt)
       ("gtk+" ,gtk+)
       ("libextractor" ,libextractor)
       ("glade3" ,glade3)
       ("qrencode" ,qrencode)
       ("libunique" ,libunique)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("libglade" ,libglade)))
    (synopsis "Graphical front-end tools for GNUnet")))
