;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Li-cheng (Andy) Tai <atai@atai.org>
;;
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

(define-module (gnu packages squirrel)
  #:use-module (gnu packages)
  #:use-module (gnu packages cmake)
  #:use-module (guix build-system cmake)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public squirrel
  (let ((commit "51137b84e66c4d526809fd8a59f4ba1d38138c76"))
    (package
      (name "squirrel")
      (version "3.1")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/albertodemichelis/squirrel.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0vbiv1q3qcy2vj8y0b9a2ckswl9ld398n3jnxijvwddrjgya5zav"))))
      (build-system cmake-build-system)
      (arguments
       '(#:tests? #f)) ; no tests
      (native-inputs
       `(("cmake" ,cmake)))
      (home-page "https://squirrel-lang.org/")
      (synopsis "High level imperative, object-oriented programming language")
      (description
       "Squirrel is a high level imperative, object-oriented programming
language, designed to be a light-weight scripting language that fits in the
size, memory bandwidth, and real-time requirements of applications like video
games.")
      (license license:expat))))
