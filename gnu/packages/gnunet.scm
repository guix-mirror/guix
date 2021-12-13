;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Sree Harsha Totakura <sreeharsha@totakura.in>
;;; Copyright © 2015, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2017, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
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
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
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
  #:use-module (gnu packages nss)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vim)
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
   (version "1.11")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/libextractor/libextractor-"
                                version ".tar.gz"))
            (sha256
             (base32
              "13xxv11mif3m0mpk7i43mljhhaqrj52kznm1qi3qb8s6hymk7xhn"))))
   (build-system gnu-build-system)
   ;; WARNING: Checks require /dev/shm to be in the build chroot, especially
   ;; not to be a symbolic link to /run/shm.
   ;; FIXME:
   ;; The following dependencies are all optional, but should be
   ;; available for maximum coverage:
   ;; * librpm (rpm)    ; investigate failure
   ;; * libtidy-html (tidy-html) ; investigate failure
   (inputs
    `(("exiv2" ,exiv2)
      ("bzip2" ,bzip2)
      ("flac" ,flac)
      ("ffmpeg" ,ffmpeg)
      ("file" ,file)                           ;libmagic, for the MIME plug-in
      ("glib" ,glib)
      ("giflib" ,giflib)
      ("gstreamer" ,gstreamer)
      ("gst-plugins-base" ,gst-plugins-base)
      ("gtk+" ,gtk+)
      ("libarchive" ,libarchive)
      ("libgsf" ,libgsf)
      ("libjpeg" ,libjpeg-turbo)
      ("libltdl" ,libltdl)
      ("libmpeg2" ,libmpeg2)
      ("libmp4v2" ,libmp4v2)
      ("libsmf" ,libsmf)
      ("libogg" ,libogg)
      ("libtiff" ,libtiff)
      ("libvorbis" ,libvorbis)
      ("zlib" ,zlib)))
   (native-inputs
    (list pkg-config))
   (outputs '("out"
              "static")) ; 420 KiB .a files
   (arguments
    `(#:configure-flags
      (list (string-append "--with-ltdl="
                           (assoc-ref %build-inputs "libltdl")))
      #:parallel-tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'configure 'fix-exiv2-tests
          ;; exiv2>=0.27.3 rounds geolocation
          ;; https://github.com/Exiv2/exiv2/pull/1107/commits/db1be4ae8e1077949fcb6a960e93069d6a41b395#diff-f3f55183ccbe956c720c86e61f708d9f
          (lambda _
            (substitute* "src/plugins/test_exiv2.c"
              (("17.585\\\\\" ") "18\\\"")
              (("21.713\\\\\" ") "22\\\""))
            #t))
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
   (version "0.9.73")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/libmicrohttpd/libmicrohttpd-"
                                version ".tar.gz"))
            (sha256
             (base32
              "0bh39irwzkv48fkw43skfgkk8ka3793bx1lm21sgw6zxi0djyyx3"))
            (patches (search-patches "libmicrohttpd-0.9.73-test-ssl3.patch"))))
   (build-system gnu-build-system)
   (inputs
    (list curl gnutls/dane libgcrypt openssl zlib))
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
   (version "7.70.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/gnunet/gnurl-" version ".tar.gz"))
            (sha256
             (base32
              "0px9la8v4bj1dzxb95fx3yxk0rcjqjrxpj733ga27cza45wwzkqa"))))
   (build-system gnu-build-system)
   (outputs '("out"
              "doc"))                             ; 1.8 MiB of man3 pages
   (inputs `(("gnutls" ,gnutls/dane)
             ("libidn2" ,libidn2)
             ("zlib" ,zlib)))
   (native-inputs
    (list libtool perl pkg-config python))
   (arguments
    `(#:configure-flags
      ;; All of these produce errors during configure.
      (list "--disable-ftp"
            "--disable-file"
            "--disable-ldap"
            "--disable-rtsp"
            "--disable-dict"
            "--disable-telnet"
            "--disable-tftp"
            "--disable-pop3"
            "--disable-imap"
            "--disable-smb"
            "--disable-smtp"
            "--disable-gopher"
            "--without-ssl"
            "--without-libpsl"
            "--without-librtmp"
            "--disable-ntlm-wb")
      #:phases
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
        ;; We have to patch runtests.pl in tests/ directory
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
   (home-page "https://gnunet.org/en/gnurl.html")))

(define-public gnunet
  (package
   (name "gnunet")
   (version "0.13.1")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnunet/gnunet-" version
                          ".tar.gz"))
      (sha256
       (base32
        "15jnca5zxng7r6m3qzq9lr73xxq0v6mvcp0lny3zrlkz5s2nmmq3"))))
   (build-system gnu-build-system)
   (inputs
    `(("bluez" ,bluez)
      ("glpk" ,glpk)
      ("gnurl" ,gnurl)
      ("gnutls" ,gnutls/dane)
      ("gstreamer" ,gstreamer)
      ("jansson" ,jansson)
      ("libextractor" ,libextractor)
      ("libidn" ,libidn2)
      ("libgcrypt" ,libgcrypt)
      ("libjpeg" ,libjpeg-turbo)
      ("libltdl" ,libltdl)
      ("libmicrohttpd" ,libmicrohttpd)
      ("libogg" ,libogg)
      ("libsodium" ,libsodium)
      ("libunistring" ,libunistring)
      ("miniupnpc" ,miniupnpc)
      ("opus" ,opus)
      ("pulseaudio" ,pulseaudio)
      ("sqlite" ,sqlite)
      ("zbar" ,zbar)
      ("zlib" ,zlib)))
   (native-inputs
    (list curl pkg-config python xxd
          (@ (gnu packages base) which)))
   (arguments
    '(#:parallel-tests? #f ; Parallel tests aren't supported.
      #:phases
      (modify-phases %standard-phases
        (add-after 'configure 'remove-failing-tests
          ;; These tests fail in Guix's building environment.
          (lambda _
            (substitute* "src/transport/Makefile"
              (("\\$\\(am__EXEEXT_15\\)") "") ; test_transport_api_https
              (("test_transport_api_manipulation_cfg\\$\\(EXEEXT\\) \\\\\n") "")
              (("test_transport_api_udp_nat\\$\\(EXEEXT\\) \\\\\n") "")
              (("test_transport_blacklisting_multiple_plugins\\$\\(EXEEXT\\) \\\\\n") ""))
            (substitute* "src/testbed/Makefile"
              (("test_testbed_api_2peers_1controller\\$\\(EXEEXT\\) \\\\\n") "")
              (("test_testbed_api_statistics\\$\\(EXEEXT\\) \\\\\n") "")
              (("test_testbed_api_test\\$\\(EXEEXT\\) \\\\\n") "")
              (("test_testbed_api_test_timeout\\$\\(EXEEXT\\) \\\\\n") "")
              (("test_testbed_api_topology\\$\\(EXEEXT\\) \\\\\n") "")
              (("test_testbed_api_topology_clique\\$\\(EXEEXT\\) \\\\\n") ""))
            (substitute* "src/topology/Makefile"
              (("^check_PROGRAMS.*") "\n")
              (("test_gnunet_daemon_topology\\$\\(EXEEXT\\)\n") ""))
            (substitute* "src/namestore/Makefile"
              (("\\$\\(am__append_2\\)") ""))
            (substitute* "src/gns/Makefile"
              (("\\$\\(am__append_4\\)") ""))
            (substitute* "contrib/Makefile"
              (("^check_PROGRAMS.*") "\n"))
            ;; 'test' from coreutils doesn't behave as the test expects.
            (substitute* '("src/gns/gnunet-gns-proxy-setup-ca.in"
                           "src/transport/gnunet-transport-certificate-creation.in")
              (("gnutls-certtool") "certtool"))
            #t))
        (add-before 'check 'set-env-var-for-tests
          (lambda _
            (setenv "LANG" "en_US.UTF-8")))
        ;; Swap 'check and 'install phases and add installed binaries to $PATH.
        (add-before 'check 'set-path-for-check
          (lambda* (#:key outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
             (setenv "GNUNET_PREFIX" (string-append out "/lib"))
             (setenv "PATH" (string-append (getenv "PATH") ":" out "/bin")))
           #t))
        (delete 'check)
        (add-after 'install 'check
          (assoc-ref %standard-phases 'check)))))
   (synopsis "Secure, decentralized, peer-to-peer networking framework")
   (description
     "GNUnet is a framework for secure peer-to-peer networking.  The
high-level goal is to provide a strong foundation of free software for a
global, distributed network that provides security and privacy.  GNUnet in
that sense aims to replace the current internet protocol stack.  Along with
an application for secure publication of files, it has grown to include all
kinds of basic applications for the foundation of a GNU internet.")
   (license license:agpl3+)
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
      (native-inputs (list pkg-config autoconf automake))
      (inputs (list guile-2.0 gnunet))
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
    (version "0.13.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gnunet/gnunet-gtk-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1zdzgq16h77w6ybwg3lqjsjr965np6iqvncqvkbj07glqd4wss0j"))))
    (arguments
     `(#:configure-flags
       (list "--with-libunique"
             "--with-qrencode"
             (string-append "--with-gnunet="
                            (assoc-ref %build-inputs "gnunet")))))
    (inputs
     `(("glade3" ,glade3)
       ("gnunet" ,gnunet)
       ("gnutls" ,gnutls/dane)
       ("gtk+" ,gtk+)
       ("libextractor" ,libextractor)
       ("libgcrypt" ,libgcrypt)
       ("libunique" ,libunique)
       ("qrencode" ,qrencode)))
    (native-inputs
     (list pkg-config libglade))
    (synopsis "Graphical front-end tools for GNUnet")
    (properties '((ftp-server . "ftp.gnu.org")
                  (ftp-directory . "/gnunet")))))
