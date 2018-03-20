;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
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

(define-module (gnu packages wget)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages web)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu))

(define-public wget
  (package
    (name "wget")
    (version "1.19.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/wget/wget-"
                          version ".tar.lz"))
      (sha256
       (base32
        "16jmcqcasx3q9k4azssryli9qyxfq0sfijw998g8zp58cnwzzh1g"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'check 'disable-https-tests
                    (lambda _
                      ;; XXX: Skip TLS tests, which fail with "The
                      ;; certificate's owner does not match hostname" as
                      ;; reported at:
                      ;; <https://lists.gnu.org/archive/html/bug-wget/2017-07/msg00012.html>.
                      ;; The problem appears to be due to a change in GnuTLS
                      ;; 3.5.12, whereby 'gnutls_x509_crt_check_hostname2' no
                      ;; longer matches IP address against the 'CN' or
                      ;; 'DNSname' fields of certificates.
                      (substitute* "testenv/Makefile"
                        (("SSL_TESTS=1") ""))
                      #t)))))
    (inputs
     `(("gnutls" ,gnutls)
       ("libidn2" ,libidn2)
       ("libpsl" ,libpsl)
       ("lzip" ,lzip)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("perl" ,perl)
       ("python" ,python)               ;for testenv suite
       ("perl-http-daemon" ,perl-http-daemon)
       ("perl-io-socket-ssl" ,perl-io-socket-ssl)))
    (home-page "https://www.gnu.org/software/wget/")
    (synopsis "Non-interactive command-line utility for downloading files")
    (description
     "GNU Wget is a non-interactive tool for fetching files using the HTTP,
HTTPS and FTP protocols.  It can resume interrupted downloads, use file name
wild cards, supports proxies and cookies, and it can convert absolute links
in downloaded documents to relative links.")
    (license gpl3+))) ; some files are under GPLv2+

(define-public wgetpaste
  (package
    (name "wgetpaste")
    (version "2.28")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://wgetpaste.zlin.dk/wgetpaste-"
                            version ".tar.bz2"))
        (sha256
         (base32
          "1hh9svyypqcvdg5mjxyyfzpdzhylhf7s7xq5dzglnm4injx3i3ak"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (zsh (string-append out "/share/zsh/site-functions")))
               (install-file "wgetpaste" bin)
               (install-file "_wgetpaste" zsh)))))
       #:tests? #f)) ; no test target
    (home-page "http://wgetpaste.zlin.dk/")
    (synopsis "Script that automates pasting to a number of pastebin services")
    (description
     "@code{wgetpaste} is an extremely simple command-line interface to various
online pastebin services.")
    (license public-domain)))

(define-public wget2
  (package
   (name "wget2")
   (version "1.0.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://gitlab.com/gnuwget/wget2.git")
           (commit "b45709d3d21714135ce79df6abbdcb704684063d")
           (recursive? #t))) ;; Needed for 'gnulib' git submodule.
     (file-name (string-append name "-" version "-checkout"))
     (sha256
      (base32
       "0ww84wwzmpyylkz8rnb6nk6f7x040132z81x52w7rjhk68p9mm24"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases (modify-phases %standard-phases
      (replace 'bootstrap
        (lambda _
          ;; Make sure all the files are writable so that ./bootstrap
          ;; can proceed.
          (for-each (lambda (file)
                      (chmod file #o755))
                      (find-files "."))
          (substitute* "./gnulib/gnulib-tool.py"
                       (("/usr/bin/python") (which "python3")))
          (zero? (system* "sh" "./bootstrap"
                          "--gnulib-srcdir=gnulib"
                          "--no-git")))))))
   (inputs `(("autoconf" ,autoconf)
             ("automake" ,automake)
             ("doxygen" ,doxygen)
             ("flex" ,flex)
             ("gettext" ,gettext-minimal)
             ("gnutls" ,gnutls/dane)
             ("libiconv" ,libiconv)
             ("libidn2" ,libidn2)
             ("libmicrohttpd" ,libmicrohttpd)
             ("libpsl" ,libpsl)
             ("libtool" ,libtool)
             ("pcre2" ,pcre2)
             ("python" ,python)))
   ;; TODO: Add libbrotlidec, libnghttp2.
   (native-inputs `(("pkg-config" ,pkg-config)))
   (home-page "https://gitlab.com/gnuwget/wget2")
   (synopsis "Successor of GNU Wget")
   (description "GNU Wget2 is the successor of GNU Wget, a file and recursive
website downloader.  Designed and written from scratch it wraps around libwget,
that provides the basic functions needed by a web client.")
   (license (list gpl3+ lgpl3+))))
