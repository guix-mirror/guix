;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages)
  #:use-module (gnu packages gdbm)
  #:use-module (gnu packages mit-krb5)
  #:use-module (gnu packages openssl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public cyrus-sasl
  (package
   (name "cyrus-sasl")
   (version "2.1.26")
   (source (origin
            (method url-fetch)
            (uri (list (string-append
                        "http://cyrusimap.org/releases/cyrus-sasl-"
                        version ".tar.gz")
                       (string-append
                        "ftp://ftp.cyrusimap.org/cyrus-sasl/cyrus-sasl-"
                        version ".tar.gz")))
            (sha256 (base32
                     "1hvvbcsg21nlncbgs0cgn3iwlnb3vannzwsp6rwvnn9ba4v53g4g"))))
   (build-system gnu-build-system)
   (inputs `(("gdbm" ,gdbm)
             ("mit-krb5" ,mit-krb5)
             ("openssl" ,openssl)))
   (arguments
    '(#:configure-flags (list (string-append "--with-plugindir="
                                             (assoc-ref %outputs "out")
                                             "/lib/sasl2"))

      ;; The 'plugins' directory has shared source files, such as
      ;; 'plugin_common.c'.  When building the shared libraries there, libtool
      ;; ends up doing "ln -s plugin_common.lo plugin_common.o", which can
      ;; fail with EEXIST when building things in parallel.
      #:parallel-build? #f))
   (synopsis "Simple Authentication Security Layer implementation")
   (description
    "SASL (Simple Authentication Security Layer) is an Internet
standards-track method for remote computers to authenticate.  The Cyrus SASL
library makes supporting various SASL mechanisms easy for both client and
server writers.")
   (license (license:bsd-style "file://COPYING"
                       "See COPYING in the distribution."))
   (home-page "http://cyrusimap.web.cmu.edu/index.php")))
