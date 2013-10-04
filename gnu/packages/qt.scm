;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages qt)
  #:use-module ((guix licenses) #:select (x11-style))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages bison))

(define-public libxkbcommon
  (package
    (name "libxkbcommon")
    (version "0.3.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://xkbcommon.org/download/" name "-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "13mk335r4dhi9qglzbp46ina1wz4qgcp8r7s06iq7j50pf0kb5ww"))))
    (build-system gnu-build-system)
    (inputs
     `(("bison" ,bison)))
    (home-page "http://xkbcommon.org/")
    (synopsis "library to handle keyboard descriptions")
    (description "Xkbcommon is a library to handle keyboard descriptions,
including loading them from disk, parsing them and handling their
state.  It is mainly meant for client toolkits, window systems, and other
system applications; currently that includes Wayland, kmscon, GTK+, Qt,
Clutter, and more.  Despite the name, it is not currently used by anything
X11 (yet).")
    (license (x11-style "file://COPYING"
                        "See 'COPYING' in the distribution."))))
