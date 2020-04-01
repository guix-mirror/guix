;;; GNU Guix --- Functional package management for GNU
;;;
;;; Copyright © 2020 Raghav Gururajan <raghavgururajan@disroot.org>
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

(define-module (gnu packages linphone)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages tls)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu))

(define-public bcunit
  (package
    (name "bcunit")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.linphone.org/releases/sources/" name
                       "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0ylchj8w98ic2fkqpxc6yk4s6s0h0ql2zsz5n49jd7126m4h8dqk"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ; No test target
       #:configure-flags
       (list
        "-DENABLE_STATIC=NO")))         ; Not required
    (synopsis "Belledonne Communications Unit Testing Framework")
    (description "BCUnit is a fork of the defunct project CUnit,
with several fixes and patches applied.  It is an unit testing
framework for writing, administering, and running unit tests in C.")
    (home-page "https://gitlab.linphone.org/BC/public/bcunit")
    (license license:lgpl2.0+)))

(define-public bctoolbox
  (package
    (name "bctoolbox")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.linphone.org/releases/sources/" name
                       "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "1a1i70pb4hhnykkwyhhc7fv67q556l8kprny8xzgfqpj1nby2ms6"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ; No test target
       #:configure-flags
       (list
        "-DENABLE_STATIC=OFF")))        ; Not required
    (inputs
     `(("bcunit" ,bcunit)
       ("mbedtls" ,mbedtls-apache)))
    (synopsis "Belledonne Communications Tool Box")
    (description "BcToolBox is an utilities library used by Belledonne
Communications softwares like belle-sip, mediastreamer2 and linphone.")
    (home-page "https://gitlab.linphone.org/BC/public/bctoolbox")
    (license license:gpl2+)))
