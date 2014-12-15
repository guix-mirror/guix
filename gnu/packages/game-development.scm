;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Tomáš Čech <sleep_walker@suse.cz>
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

(define-module (gnu packages game-development)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages))

(define-public bullet
  (package
    (name "bullet")
    (version "2.82-r2704")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bullet.googlecode.com/files/bullet-"
                                  version ".tgz"))
              (sha256
               (base32
                "1lnfksxa9b1slyfcxys313ymsllvbsnxh9np06azkbgpfvmwkr37"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f ; no 'test' target
                 #:configure-flags (list
                                    (string-append
                                     "-DCMAKE_CXX_FLAGS=-fPIC "
                                     (or (getenv "CXXFLAGS") "")))))
    (home-page "http://bulletphysics.org/")
    (synopsis "3D physics engine library")
    (description
     "Bullet is a physics engine library usable for collision detection.  It
is used in some video games and movies.")
    (license zlib)))
