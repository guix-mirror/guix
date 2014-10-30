;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages web)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public wget
  (package
    (name "wget")
    (version "1.16")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/wget/wget-"
                          version ".tar.xz"))
      (sha256
       (base32
        "1rxhr3jmgbwryzl51di4avqxw9m9j1z2aak8q1npns0p184xsqcj"))))
    (build-system gnu-build-system)
    ;; TODO: Add libpsl to inputs
    (inputs
     `(("gnutls" ,gnutls)
       ("libidn" ,libidn)))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python)               ;for testenv suite
       ("perl-http-daemon" ,perl-http-daemon)
       ("perl-io-socket-ssl" ,perl-io-socket-ssl)))
    (home-page "http://www.gnu.org/software/wget/")
    (synopsis "Non-interactive command-line utility for downloading files")
    (description
     "GNU Wget is a non-interactive tool for fetching files using the HTTP,
HTTPS and FTP protocols.  It can resume interrupted downloads, use filename
wild cards, supports proxies and cookies, and it can convert absolute links
in downloaded documents to relative links.")
    (license gpl3+))) ; some files are under GPLv2+
