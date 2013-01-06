;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Ludovic Courtès <ludo@gnu.org>
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

(define-module (distro packages pkg-config)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public pkg-config
  (package
   (name "pkg-config")
   (version "0.27.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://pkgconfig.freedesktop.org/releases/pkg-config-"
                  version ".tar.gz"))
            (sha256
             (base32
              "05wc5nwkqz7saj2v33ydmz1y6jdg659dll4jjh91n41m63gx0qsg"))))
   (build-system gnu-build-system)
   (arguments `(#:configure-flags '("--with-internal-glib")))
   (home-page "http://www.freedesktop.org/wiki/Software/pkg-config")
   (license gpl2+)
   (synopsis "a helper tool used when compiling applications and
libraries")
   (description
    "pkg-config is a helper tool used when compiling applications and
libraries.  It helps you insert the correct compiler options on the
command line so an application can use gcc -o test test.c `pkg-config
--libs --cflags glib-2.0` for instance, rather than hard-coding values
on where to find glib (or other libraries). It is language-agnostic, so
it can be used for defining the location of documentation tools, for
instance.")))
