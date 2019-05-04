;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2019 Efraim Flashner <efraim@flashner.co.il>
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
    (version "1.20.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/wget/wget-"
                          version ".tar.lz"))
      (sha256
       (base32
        "0a29qsqxkk8145vkyy35q5a1wc7qzwx3qj3gmfrkmi9xs96yhqqg"))))
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
     '(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (zsh (string-append out "/share/zsh/site-functions")))
               (install-file "wgetpaste" bin)
               (install-file "_wgetpaste" zsh)
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
                                "wget")))))
               #t))))
       #:tests? #f))                    ; no test target
    (inputs
     `(("wget" ,wget)))
    (home-page "http://wgetpaste.zlin.dk/")
    (synopsis "Script that automates pasting to a number of pastebin services")
    (description
     "@code{wgetpaste} is an extremely simple command-line interface to various
online pastebin services.")
    (license public-domain)))

(define-public wget2
  (package
   (name "wget2")
   (version "1.99.1")
   (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://gitlab.com/gnuwget/wget2.git")
              (commit (string-append name "-" version))
              (recursive? #t))) ;; Needed for 'gnulib' git submodule.
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "15wxsnjhc6bzk7f60i1djmsarh1w31gwi5h2gh9k19ncwypfj5dm"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'skip-network-test
          (lambda _
            (substitute* "tests/Makefile.am"
              (("test-auth-digest\\$\\(EXEEXT)") ""))
            #t))
        (replace 'bootstrap
          (lambda _
            ;; Make sure all the files are writable so that ./bootstrap
            ;; can proceed.
            (for-each (lambda (file)
                        (chmod file #o755))
                        (find-files "."))
            (patch-shebang "./gnulib/gnulib-tool.py")
            ;; Remove unnecessary inputs from bootstrap.conf
            (substitute* "bootstrap.conf"
              (("flex.*") "")
              (("makeinfo.*") "")
              (("lzip.*") "")
              (("rsync.*") ""))
            (invoke "sh" "./bootstrap"
                    "--gnulib-srcdir=gnulib"
                    "--no-git"))))))
   (inputs
    `(("gnutls" ,gnutls/dane)
      ("libiconv" ,libiconv)
      ("libidn2" ,libidn2)
      ("libmicrohttpd" ,libmicrohttpd)
      ("libpsl" ,libpsl)
      ("pcre2" ,pcre2)))
   ;; TODO: Add libbrotlidec, libnghttp2.
   (native-inputs
    `(("autoconf" ,autoconf)
      ("automake" ,automake)
      ("flex" ,flex)
      ("gettext" ,gettext-minimal)
      ("libtool" ,libtool)
      ("pkg-config" ,pkg-config)
      ("python" ,python-2)))
   (home-page "https://gitlab.com/gnuwget/wget2")
   (synopsis "Successor of GNU Wget")
   (description "GNU Wget2 is the successor of GNU Wget, a file and recursive
website downloader.  Designed and written from scratch it wraps around libwget,
that provides the basic functions needed by a web client.")
   (license (list gpl3+ lgpl3+))))
