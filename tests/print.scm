;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (test-print)
  #:use-module (guix import print)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (srfi srfi-64))

(test-begin "print")

(define pkg
  (package
    (name "test")
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "file:///tmp/test-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "070pwb7brdcn1mfvplkd56vjc7lbz4iznzkqvfsakvgbv68k71ah"))))
    (build-system gnu-build-system)
    (home-page "http://gnu.org")
    (synopsis "Dummy")
    (description "This is a dummy package.")
    (license gpl3+)))

(test-equal "simple package"
  (package->code pkg)
  '(package
     (name "test")
     (version "1.2.3")
     (source (origin
               (method url-fetch)
               (uri (string-append "file:///tmp/test-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "070pwb7brdcn1mfvplkd56vjc7lbz4iznzkqvfsakvgbv68k71ah"))))
     (build-system gnu-build-system)
     (home-page "http://gnu.org")
     (synopsis "Dummy")
     (description "This is a dummy package.")
     (license gpl3+)))

(test-end "print")
