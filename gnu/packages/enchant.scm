;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Marek Benc <merkur32@gmail.com>
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

(define-module (gnu packages enchant)
  #:use-module (gnu packages)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses))

(define-public enchant
  (package
    (name "enchant")
    (version "1.6.0")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append "http://www.abisource.com/downloads/" name "/" version
                         "/" name "-" version ".tar.gz"))
        (sha256
          (base32 "0zq9yw1xzk8k9s6x83n1f9srzcwdavzazn3haln4nhp9wxxrxb1g"))))

    (build-system gnu-build-system)
    (inputs
      `(("aspell" ,aspell)    ;; Currently, the only supported backend in Guix
        ("glib" ,glib)))      ;; is aspell. (This information might be old)
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)))

    (synopsis "Multi-backend spell-checking library wrapper")
    (description
      "On the surface, Enchant appears to be a generic spell checking library.
Looking closer, you'll see the Enchant is more-or-less a fancy wrapper around
the dlopen() system call.

Enchant steps in to provide uniformity and conformity on top of these libraries,
and implement certain features that may be lacking in any individual provider
library.  Everything should \"just work\" for any and every definition of \"just
working.\"")
    (home-page "http://www.abisource.com/projects/enchant")
    (license lgpl2.1+)))
