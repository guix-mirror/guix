;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages adns)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public adns
  (package
    (name "adns")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "mirror://gnu/adns/adns-"
                                        version ".tar.gz")
                         (string-append
                          "http://www.chiark.greenend.org.uk/~ian/adns/ftp/adns-"
                          version ".tar.gz")))
              (sha256
               (base32
                "0hg89b5n84zjhzvbzrpvhl0hbm4s6d1z2pzllfis64ai656ypibz"))))
    (build-system gnu-build-system)
    (arguments
     ;; Make sure the programs under bin/ fine libadns.so.
     '(#:configure-flags (list (string-append "LDFLAGS=-Wl,-rpath -Wl,"
                                              (assoc-ref %outputs "out")
                                              "/lib"))

       ;; XXX: Tests expect real name resolution to work.
       #:tests? #f))
    (home-page "http://www.gnu.org/software/adns/")
    (synopsis "Asynchronous DNS client library and utilities")
    (description
     "GNU adns is a C library that provides easy-to-use DNS resolution
functionality.  The library is asynchronous, allowing several concurrent
calls.  The package also includes several command-line utilities for use in
scripts.")
    (license gpl3+)))
