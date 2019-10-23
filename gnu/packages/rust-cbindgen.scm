;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages rust-cbindgen)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cargo))

(define rust-ansi-term-0.11
  (package
    (name "rust-ansi-term")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ansi_term" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "16wpvrghvd0353584i1idnsgm0r3vchg8fyrm0x8ayv1rgvbljgf"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/ogham/rust-ansi-term")
    (synopsis "Library for ANSI terminal colours and styles")
    (description
     "This is a library for controlling colours and formatting, such as red bold
text or blue underlined text, on ANSI terminals.")
    (properties '((hidden? . #t)))
    (license license:expat)))
