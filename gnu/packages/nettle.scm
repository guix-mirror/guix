;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
                                              "/lib"))
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'move-static-libraries
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out"))
                            (slib (string-append (assoc-ref outputs "static")
                                                 "/lib")))
                        (mkdir-p slib)
                        (with-directory-excursion (string-append out "/lib")
                          (for-each (lambda (ar)
                                      (rename-file ar (string-append
                                                       slib "/"
                                                       (basename ar))))
                                    (find-files "." "\\.a$")))
                        #t))))))
    (outputs '("out" "debug" "static"))
    (native-inputs `(("m4" ,m4)))
    (propagated-inputs `(("gmp" ,gmp)))
    (home-page "https://www.lysator.liu.se/~nisse/nettle/")
    (synopsis "C library for low-level cryptographic functionality")
    (description
     "GNU Nettle is a low-level cryptographic library.  It is designed to
fit in easily in almost any context.  It can be easily included in
cryptographic toolkits for object-oriented languages or in applications
themselves.")
    (license gpl2+)))

(define-public nettle-3.5
  ;; This version is not API-compatible with version 2.  In particular, lsh
  ;; cannot use it yet.  So keep it separate.
  (package (inherit nettle-2)
    (version "3.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/nettle/nettle-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "06clvkdfxhlbagn4afssylmn5vrak59dlmnvy8b2xc31hycs3k3m"))))
    (arguments
     (substitute-keyword-arguments (package-arguments nettle-2)
       ((#:configure-flags flags)
        ;; Build "fat" binaries where the right implementation is chosen
        ;; at run time based on CPU features (starting from 3.1.)
        `(cons "--enable-fat" ,flags))))))

(define-public nettle-3.7
  (package (inherit nettle-3.5)
    (version "3.7.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/nettle/nettle-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0qpi1qp3bcvqdsaxy2pzg530db95x8qjahkynxgwvr6dy5760ald"))))))

;;; Upgrading Nettle on master would cause 10000+ packages to be rebuilt.
(define-public nettle nettle-3.5)
