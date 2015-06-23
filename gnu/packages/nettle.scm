;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages nettle)
  #:use-module (guix utils)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages m4))

(define-public nettle-2
  (package
    (name "nettle")
    (version "2.7.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/nettle/nettle-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0h2vap31yvi1a438d36lg1r1nllfx3y19r4rfxv7slrm6kafnwdw"))))
    (build-system gnu-build-system)
    (arguments
     ;; 'sexp-conv' and other programs need to have their RUNPATH point to
     ;; $libdir, which is not the case by default.  Work around it.
     '(#:configure-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))))
    (outputs '("out" "debug"))
    (native-inputs `(("m4" ,m4)))
    (propagated-inputs `(("gmp" ,gmp)))
    (home-page "http://www.lysator.liu.se/~nisse/nettle/")
    (synopsis "C library for low-level cryptographic functionality")
    (description
     "GNU Nettle is a low-level cryptographic library.  It is designed to
fit in easily in almost any context.  It can be easily included in
cryptographic toolkits for object-oriented languages or in applications
themselves.")
    (license gpl2+)))

(define-public nettle
  ;; This version is not API-compatible with version 2.  In particular, lsh
  ;; cannot use it yet.  So keep it separate.
  (package (inherit nettle-2)
    (version "3.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/nettle/nettle-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0k1x57zviysvi91lkk66cg8v819vywm5g5yqs22wppfqcifx5m2z"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after
          'configure 'disable-ifunc-init-method
          (lambda _
            ;; Work around problems with the ifunc initialization method in
            ;; nettle.  For details, see
            ;; <http://lists.lysator.liu.se/pipermail/nettle-bugs/2015/003389.html>
            ;; and <https://sourceware.org/ml/libc-help/2015-06/msg00010.html>.
            (substitute* "config.h"
              (("#define HAVE_LINK_IFUNC 1")
               "/* #undef HAVE_LINK_IFUNC */"))
            #t)))
       ,@(substitute-keyword-arguments (package-arguments nettle-2)
           ((#:configure-flags flags)
            ;; Build "fat" binaries where the right implementation is chosen
            ;; at run time based on CPU features (starting from 3.1.)
            `(cons "--enable-fat" ,flags)))))))
