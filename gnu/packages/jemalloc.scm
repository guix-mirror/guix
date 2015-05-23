;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
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

(define-module (gnu packages jemalloc)
  #:use-module ((guix licenses) #:select (bsd-2))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public jemalloc
  (package
    (name "jemalloc")
    (version "3.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.canonware.com/download/jemalloc/"
                    name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1zl4vxxjvhg72bdl53sl0idz9wp18c6yzjdmqcnwm09wvmcj2v71"))))
    (build-system gnu-build-system)
    (home-page "http://www.canonware.com/jemalloc/")
    (synopsis "General-purpose scalable concurrent malloc implementation")
    (description
     "This library providing a malloc(3) implementation that emphasizes
fragmentation avoidance and scalable concurrency support.")
    (license bsd-2)))
