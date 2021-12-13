;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Brant Gardner <brantcgardner@brantware.com>
;;; Copyright © 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages distributed)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

(define-public boinc-client
  (package
    (name "boinc-client")
    (version "7.16.17")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/boinc/boinc")
                    (commit (string-append "client_release/"
                                           (version-major+minor version)
                                           "/" version))))
              (file-name (git-file-name "boinc" version))
              (sha256
               (base32
                "1p8y3mnf5yfhavhqxwf9v68prg1601h8q1pllm5z89zh661di3mj"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--disable-server")))
    (inputs (list openssl
                  curl
                  wxwidgets
                  gtk+
                  gdk-pixbuf
                  libnotify
                  sqlite
                  python
                  python-pyserial))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (synopsis "Help cutting-edge science research using your computer")
    (description "BOINC is a platform for high-throughput computing on a large
scale (thousands or millions of computers).  It can be used for volunteer
computing (using consumer devices) or grid computing (using organizational
resources).  It supports virtualized, parallel, and GPU-based applications.")
    (home-page "https://boinc.berkeley.edu/")
    ;; BOINC is distributed as LGPL3+, with some individual modules under GPL3+.
    (license (list license:lgpl3+ license:gpl3+))))

(define-public boinc-server
  ;; XXX The server and client packages duplicate many files such as /lib.
  ;; TODO: consolidate them?
  (package (inherit boinc-client)
    (name "boinc-server")
    (arguments '(#:configure-flags '("--disable-client" "--disable-manager")
                 #:parallel-build? #f))
    (inputs `(("openssl" ,openssl)
              ("curl" ,curl)
              ("mariadb:dev" ,mariadb "dev")
              ("zlib" ,zlib)))
    (propagated-inputs `(("python" ,python-wrapper)
                         ("perl" ,perl)))))
