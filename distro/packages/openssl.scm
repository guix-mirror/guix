;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Andreas Enge <andreas@enge.fr>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (distro packages openssl)
  #:use-module (distro)
  #:use-module (distro packages perl)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public openssl
  (package
   (name "openssl")
   (version "1.0.1c")
   (source (origin
            (method url-fetch)
            (uri (string-append "ftp://ftp.openssl.org/source/openssl-" version
                                ".tar.gz"))
            (sha256 (base32
                     "1gjy6a7d8nszi9wq8jdzx3cffn0nss23h3cw2ywlw4cb9v6v77ia"))))
   (build-system gnu-build-system)
   (inputs `(("perl" ,perl)))
   (arguments
    (lambda (system)
      `(#:parallel-build? #f
        #:parallel-tests? #f
        #:test-target "test"
        #:phases
        (alist-replace
         'configure
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
             (zero?
              (system* "./config"
                       "shared"                 ; build shared libraries
                       "--libdir=lib"
                       (string-append "--prefix=" out)))))
         (alist-cons-before
          'patch-source-shebangs 'patch-tests
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((bash (assoc-ref inputs "bash")))
             (substitute* (find-files "test" ".*")
               (("/bin/sh")
                (string-append bash "/bin/bash"))
               (("/bin/rm")
                "rm"))))
          %standard-phases)))))
   (synopsis "OpenSSL, an SSL/TLS implementation")
   (description
    "OpenSSL is an implementation of SSL/TLS")
   (license openssl)
   (home-page "http://www.openssl.org/")))
