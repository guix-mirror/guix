;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
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

(define-module (gnu packages wget)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public wget
  (package
    (name "wget")
    (version "1.20.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/wget/wget-"
                          version ".tar.lz"))
      (sha256
       (base32
        "1frajd86ds8vz2hprq30wq8ya89z9dcxnwm8nwk12bbc47l7qq39"))))
    (build-system gnu-build-system)
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
    (license license:gpl3+))) ; some files are under GPLv2+

(define-public wgetpaste
  (package
    (name "wgetpaste")
    (version "2.29")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://wgetpaste.zlin.dk/wgetpaste-"
                            version ".tar.bz2"))
        (sha256
         (base32
          "1rp0wxr3zy7y2xp3azaadfghrx7g0m138f9qg6icjxkkz4vj9r22"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-after 'unpack 'remove-dead-paste-site
           ;; This phase is adaped from the following patch:
           ;; https://gitweb.gentoo.org/repo/gentoo.git/tree/app-text/wgetpaste/files/wgetpaste-remove-dead.patch
           (lambda _
             (substitute* "wgetpaste"
               (("-bpaste") "-dpaste")) ; dpaste blocks tor users
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (zsh (string-append out "/share/zsh/site-functions"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (install-file "wgetpaste" bin)
               (install-file "_wgetpaste" zsh)
               (install-file "LICENSE" doc)
               #t)))
         (add-after 'install 'wrap-program
           ;; /bin/wgetpaste prides itself on relying only on the following
           ;; inputs, and doesn't need to execute arbitrary commands, so
           ;; override PATH completely to detect any new dependencies early.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/wgetpaste")
                 `("PATH" ":" =
                   ,(delete-duplicates
                     (map (lambda (command) (dirname (which command)))
                          (list "bash" "mktemp" "sed" "sort" "tee" "tr"
                                "wget" "xclip")))))
               #t))))
       #:tests? #f))                    ; no test target
    (inputs
     `(("wget" ,wget)
       ("xclip" ,xclip)))
    (home-page "http://wgetpaste.zlin.dk/")
    (synopsis "Script that automates pasting to a number of pastebin services")
    (description
     "@code{wgetpaste} is an extremely simple command-line interface to various
online pastebin services.")
    (license license:expat)))

(define-public wget2
  (package
   (name "wget2")
   (version "1.99.2")
   (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/wget/wget2-" version ".tar.gz"))
       (sha256
        (base32
         "0qv55f4bablrlhc8bnic8g3mkk1kq44c4cphrk5jmv92z9aqzi6b"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'skip-network-tests
          (lambda _
            (substitute* "tests/Makefile.in"
              (("test-gpg-verify-no-file\\$\\(EXEEXT)") "")
              (("test-gpg-valid\\$\\(EXEEXT)") "")
              (("test-gpg-styles\\$\\(EXEEXT)") ""))
            #t)))
      #:configure-flags '("--enable-static=no")))
   (inputs
    `(("bzip2" ,bzip2)
      ("gnutls" ,gnutls/dane)
      ("gpgme" ,gpgme)
      ("libiconv" ,libiconv)
      ("libidn2" ,libidn2)
      ("libmicrohttpd" ,libmicrohttpd)
      ("libpsl" ,libpsl)
      ("pcre2" ,pcre2)
      ("zlib" ,zlib)))
   ;; TODO: Add libbrotlidec, libnghttp2.
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (home-page "https://gitlab.com/gnuwget/wget2")
   (synopsis "Successor of GNU Wget")
   (description "GNU Wget2 is the successor of GNU Wget, a file and recursive
website downloader.  Designed and written from scratch it wraps around libwget,
that provides the basic functions needed by a web client.")
   (properties '((ftp-directory . "/gnu/wget")))
   (license (list license:gpl3+ license:lgpl3+))))
