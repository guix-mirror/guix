;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (gnu packages grue-hunter)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl))

(define-public grue-hunter
  (package
    (name "grue-hunter")
    (version "1.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://jxself.org/" name ".tar.gz"))
      (sha256
       (base32
        "1hjcpy5439qs3v2zykis7hsi0i17zjs62gks3zd8mnfw9ni4i2h3"))))
    (build-system trivial-build-system) ; no Makefile.PL
    (arguments `(#:modules ((guix build utils))
                 #:builder
                 (begin
                   (use-modules (guix build utils))
                   (use-modules (srfi srfi-1))

                   (let* ((tarball (assoc-ref %build-inputs "tarball"))
                          (perl    (string-append (assoc-ref %build-inputs
                                                             "perl")
                                                  "/bin"))
                          (gunzip  (string-append (assoc-ref %build-inputs
                                                             "gzip")
                                                  "/bin/gunzip"))
                          (tar     (string-append (assoc-ref %build-inputs
                                                             "tar")
                                                  "/bin/tar"))
                          (out     (assoc-ref %outputs "out"))
                          (bin     (string-append out "/bin"))
                          (doc     (string-append out "/share/doc")))
                     (begin
                       (mkdir out)
                       (copy-file tarball "grue-hunter.tar.gz")
                       (zero? (system* gunzip "grue-hunter.tar.gz"))
                       (zero? (system* tar "xvf"  "grue-hunter.tar"))

                       (mkdir-p bin)
                       (copy-file "grue-hunter/gh.pl"
                                  (string-append bin "/grue-hunter"))
                       (patch-shebang (string-append bin "/grue-hunter")
                                      (list perl))

                       (mkdir-p doc)
                       (copy-file "grue-hunter/AGPLv3.txt"
                                  (string-append doc "/grue-hunter")))))))
    (inputs `(("perl" ,perl)
              ("tar" ,tar)
              ("gzip" ,gzip)
              ("tarball" ,source)))
    (home-page "http://jxself.org/grue-hunter.shtml")
    (synopsis "Text adventure game")
    (description
     "Grue Hunter is a text adventure game written in Perl.  You must make
your way through an underground cave system in search of the Grue.  Can you
capture it and get out alive?")
    (license agpl3+)))
