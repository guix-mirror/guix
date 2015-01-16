;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages tre)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses))

(define-public tre
  (package
    (name "tre")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri       
          (string-append "http://laurikari.net/tre/" name "-" version
                         ".tar.bz2"))
        (sha256
          (base32 "0n36cgqys59r2gmb7jzbqiwsy790v8nbxk82d2n2saz0rp145ild"))))

    (build-system gnu-build-system)
    (arguments
     `(#:phases (alist-cons-before
                 'check 'install-locales
                  (lambda _
                    ;; The tests require the availability of the
                    ;; 'en_US.ISO-8859-1' locale.
                    (setenv "LOCPATH" (getcwd))
                    (zero? (system* "localedef" "--no-archive"
                                    "--prefix" (getcwd) "-i" "en_US"
                                    "-f" "ISO-8859-1" "./en_US.ISO-8859-1")))
                 %standard-phases)))
    (synopsis "Approximate regex matching library and agrep utility")
    (description "Superset of the POSIX regex API, enabling approximate
matching.  Also ships a version of the agrep utility which behaves similar to
grep but features ineaxct matching.")
    (home-page "http://laurikari.net/tre")
    (license bsd-2)))
