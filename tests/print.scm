;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2020 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages) #:select (search-patches))
  #:use-module (srfi srfi-64))

(define-syntax-rule (define-with-source object source expr)
  (begin
    (define object expr)
    (define source 'expr)))

(test-begin "print")

(define-with-source pkg pkg-source
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
    (build-system (@ (guix build-system gnu) gnu-build-system))
    (home-page "http://gnu.org")
    (synopsis "Dummy")
    (description "This is a dummy package.")
    (license license:gpl3+)))

(define-with-source pkg-with-inputs pkg-with-inputs-source
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
    (build-system (@ (guix build-system gnu) gnu-build-system))
    (inputs (list (@ (gnu packages base) coreutils)
                  `(,(@ (gnu packages base) glibc) "debug")))
    (home-page "http://gnu.org")
    (synopsis "Dummy")
    (description "This is a dummy package.")
    (license license:gpl3+)))

(define-with-source pkg-with-origin-input pkg-with-origin-input-source
  (package
    (name "test")
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "file:///tmp/test-"
                                        version ".tar.gz")
                         (string-append "http://example.org/test-"
                                        version ".tar.gz")))
              (sha256
               (base32
                "070pwb7brdcn1mfvplkd56vjc7lbz4iznzkqvfsakvgbv68k71ah"))
              (patches (search-patches "guile-linux-syscalls.patch"
                                       "guile-relocatable.patch"))))
    (build-system (@ (guix build-system gnu) gnu-build-system))
    (inputs
     `(("o" ,(origin
               (method url-fetch)
               (uri "http://example.org/somefile.txt")
               (sha256
                (base32
                 "0000000000000000000000000000000000000000000000000000"))))))
    (home-page "http://gnu.org")
    (synopsis "Dummy")
    (description "This is a dummy package.")
    (license license:gpl3+)))

(define-with-source pkg-with-origin-patch pkg-with-origin-patch-source
  (package
    (name "test")
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "file:///tmp/test-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "070pwb7brdcn1mfvplkd56vjc7lbz4iznzkqvfsakvgbv68k71ah"))
              (patches
               (list (origin
                       (method url-fetch)
                       (uri "http://example.org/x.patch")
                       (sha256
                        (base32
                         "0000000000000000000000000000000000000000000000000000")))))))
    (build-system (@ (guix build-system gnu) gnu-build-system))
    (home-page "http://gnu.org")
    (synopsis "Dummy")
    (description "This is a dummy package.")
    (license license:gpl3+)))

(define-with-source pkg-with-arguments pkg-with-arguments-source
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
    (build-system (@ (guix build-system gnu) gnu-build-system))
    (arguments
     `(#:disallowed-references (,(@ (gnu packages base) coreutils))))
    (home-page "http://gnu.org")
    (synopsis "Dummy")
    (description "This is a dummy package.")
    (license license:gpl3+)))

(test-equal "simple package"
  `(define-public test ,pkg-source)
  (package->code pkg))

(test-equal "package with inputs"
  `(define-public test ,pkg-with-inputs-source)
  (package->code pkg-with-inputs))

(test-equal "package with origin input"
  `(define-public test ,pkg-with-origin-input-source)
  (package->code pkg-with-origin-input))

(test-equal "package with origin patch"
  `(define-public test ,pkg-with-origin-patch-source)
  (package->code pkg-with-origin-patch))

(test-equal "package with arguments"
  `(define-public test ,pkg-with-arguments-source)
  (package->code pkg-with-arguments))

(test-end "print")
