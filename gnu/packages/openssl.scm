;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl))

(define-public openssl
  (package
   (name "openssl")
   (version "1.0.1k")
   (source (origin
            (method url-fetch)
            (uri (string-append "ftp://ftp.openssl.org/source/openssl-" version
                                ".tar.gz"))
            (sha256
             (base32
              "0754wzmzr90hiiqs5cy6g3cf8as75ljkhppgyirfg26hpapax7wg"))))
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
   (synopsis "SSL/TLS implementation")
   (description
    "OpenSSL is an implementation of SSL/TLS")
   (license license:openssl)
   (home-page "http://www.openssl.org/")))

(define-public perl-net-ssleay
  (package
    (name "perl-net-ssleay")
    (version "1.66")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MI/MIKEM/"
                                  "Net-SSLeay-" version ".tar.gz"))
              (sha256
               (base32
                "0mxfdhz2fyc40a4myi1yfalf875v5wq1fm4qib9sj3chdm9zvy2v"))))
    (build-system perl-build-system)
    (inputs `(("openssl" ,openssl)))
    (arguments
     `(#:phases (alist-cons-before
                 'configure 'set-ssl-prefix
                 (lambda* (#:key inputs #:allow-other-keys)
                   (setenv "OPENSSL_PREFIX" (assoc-ref inputs "openssl")))
                 %standard-phases)))
    (synopsis "Perl extension for using OpenSSL")
    (description
     "This module offers some high level convenience functions for accessing
web pages on SSL servers (for symmetry, the same API is offered for accessing
http servers, too), an sslcat() function for writing your own clients, and
finally access to the SSL api of the SSLeay/OpenSSL package so you can write
servers or clients for more complicated applications.")
    (license (package-license perl))
    (home-page "http://search.cpan.org/~mikem/Net-SSLeay-1.66/")))


