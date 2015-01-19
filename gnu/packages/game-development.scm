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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages compression))

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
    (license license:zlib)))

(define-public tiled
  (package
    (name "tiled")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/bjorn/tiled/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "03a15vbzjfwc8dpifbjvd0gnr208mzmdkgs2nlc8zq6z0a4h4jqd"))))
    (build-system gnu-build-system)
    (inputs `(("qt" ,qt)
              ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (alist-replace
        'configure
        (lambda* (#:key outputs #:allow-other-keys)
          (let ((out (assoc-ref outputs "out")))
            (system* "qmake"
                     (string-append "PREFIX=" out))))
        %standard-phases)))
    (home-page "http://www.mapeditor.org/")
    (synopsis "Tile map editor")
    (description
     "Tiled is a general purpose tile map editor.  It is meant to be used for
editing maps of any tile-based game, be it an RPG, a platformer or a Breakout
clone.")

    ;; As noted in 'COPYING', part of it is under GPLv2+, while the rest is
    ;; under BSD-2.
    (license license:gpl2+)))
