;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (gnu packages raspberry-pi)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages embedded)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define-public bcm2835
  (package
    (name "bcm2835")
    (version "1.64")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.airspayce.com/mikem/bcm2835/bcm2835-"
                    version ".tar.gz"))
              (sha256
               (base32
                "06s81540iz4vsh0cm6jwah2x0hih79v42pfa4pgr8kcbv56158h6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))    ; Would need to be root
    ;; doc/html docs would not be installed anyway.
    ;(native-inputs
    ; `(("doxygen", doxygen)))
    (synopsis "C library for Broadcom BCM 2835 as used in Raspberry Pi")
    (description "This package provides a C library for Broadcom BCM 2835 as
used in the Raspberry Pi")
    (home-page "http://www.airspayce.com/mikem/bcm2835/")
    (supported-systems '("armhf-linux" "aarch64-linux"))
    (license license:gpl3)))

