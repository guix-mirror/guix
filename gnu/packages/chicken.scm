;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ekaitz Zarraga <ekaitz@elenq.tech>
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

(define-module (gnu packages chicken)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (gnu packages commencement)
  #:use-module ((guix licenses)
                #:select (bsd-3)))

(define-public chicken
  (package
    (name "chicken")
    (version "5.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://code.call-cc.org/releases/"
                                  version "/chicken-" version ".tar.gz"))
              (sha256
               (base32
                "0jsbp3kp0134f318j3wpd1n85gf8qzh034fn198gvazsv2l024aw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))

       ;; No `configure' script; run "make check" after "make install" as
       ;; prescribed by README.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (add-after 'install 'check
           (assoc-ref %standard-phases 'check)))

       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list "PLATFORM=linux"
                            (string-append "PREFIX=" out)
                            (string-append "VARDIR=" out "/var/lib")))

       ;; Parallel builds are not supported, as noted in README.
       #:parallel-build? #f))
    (propagated-inputs `(("gcc-toolchain", gcc-toolchain)))
    (home-page "http://www.call-cc.org/")
    (synopsis "R5RS Scheme implementation that compiles native code via C")
    (description
     "CHICKEN is a compiler for the Scheme programming language.  CHICKEN
produces portable and efficient C, supports almost all of the R5RS Scheme
language standard, and includes many enhancements and extensions.")
    (license bsd-3)))
