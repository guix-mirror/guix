;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages openssl)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module ((guix licenses) #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public openssl
  (package
   (name "openssl")
   (version "1.0.1f")
   (source (origin
            (method url-fetch)
            (uri (string-append "ftp://ftp.openssl.org/source/openssl-" version
                                ".tar.gz"))
            (sha256 (base32
                     "0nnbr70dg67raqsqvlypzxa1v5xsv9gp91f9pavyckfn2w5sihkc"))))
   (build-system gnu-build-system)
   (native-inputs `(("perl" ,perl)))
   (arguments
    '(#:parallel-build? #f
      #:parallel-tests? #f
      #:test-target "test"
      #:phases
      (alist-replace
       'configure
       (lambda* (#:key outputs #:allow-other-keys)
         (let ((out (assoc-ref outputs "out")))
           (zero?
            (system* "./config"
                     "shared"                   ; build shared libraries
                     "--libdir=lib"
                     (string-append "--prefix=" out)))))
       (alist-cons-before
        'patch-source-shebangs 'patch-tests
        (lambda* (#:key inputs native-inputs #:allow-other-keys)
          (let ((bash (assoc-ref (or native-inputs inputs) "bash")))
            (substitute* (find-files "test" ".*")
              (("/bin/sh")
               (string-append bash "/bin/bash"))
              (("/bin/rm")
               "rm"))))
        %standard-phases))))
   (synopsis "OpenSSL, an SSL/TLS implementation")
   (description
    "OpenSSL is an implementation of SSL/TLS")
   (license license:openssl)
   (home-page "http://www.openssl.org/")))
