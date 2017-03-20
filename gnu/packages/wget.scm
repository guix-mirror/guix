;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages web)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public wget
  (package
    (name "wget")
    (version "1.19.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/wget/wget-"
                          version ".tar.xz"))
      (patches (search-patches "wget-CVE-2017-6508.patch"
                               "wget-fix-504-test-timeout.patch"))
      (sha256
       (base32
        "1ljcfhbkdsd0zjfm520rbl1ai62fc34i7c45sfj244l8f6b0p58c"))))
    (build-system gnu-build-system)
    (inputs
     `(("gnutls" ,gnutls)
       ("libidn2" ,libidn2)
       ("libpsl" ,libpsl)))
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
