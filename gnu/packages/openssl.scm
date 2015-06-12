;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
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
   (version "1.0.2b")
   (source (origin
            (method url-fetch)
            (uri (string-append "ftp://ftp.openssl.org/source/openssl-" version
                                ".tar.gz"))
            (sha256
             (base32
              "0gwf4fy1yqmai6wph0g9lh09iarwxaa70hm7jm0rf1qakz68im6m"))
            (patches (list (search-patch "openssl-runpath.patch")))))
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
   (native-search-paths
    ;; FIXME: These two variables must designate a single file or directory
    ;; and are not actually "search paths."  In practice it works OK in user
    ;; profiles because there's always just one item that matches the
    ;; specification.
    (list (search-path-specification
           (variable "SSL_CERT_DIR")
           (files '("etc/ssl/certs")))
          (search-path-specification
           (variable "SSL_CERT_FILE")
           (files '("etc/ssl/certs/ca-certificates.crt")))))
   (synopsis "SSL/TLS implementation")
   (description
    "OpenSSL is an implementation of SSL/TLS")
   (license license:openssl)
   (home-page "http://www.openssl.org/")))

(define-public perl-net-ssleay
  (package
    (name "perl-net-ssleay")
    (version "1.68")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MI/MIKEM/"
                                  "Net-SSLeay-" version ".tar.gz"))
              (sha256
               (base32
                "1m2wwzhjwsg0drlhp9w12fl6bsgj69v8gdz72jqrqll3qr7f408p"))
              (patches
               ;; XXX Try removing this patch for perl-net-ssleay > 1.68
               (list (search-patch "perl-net-ssleay-disable-ede-test.patch")))))
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


