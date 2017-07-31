;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages motti)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download))

(define-public motti
  (package
    (name "motti")
    (version "3.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://gnu/motti/motti-" version ".tar.gz"))
        (sha256
         (base32
          "0ag4gpxy42l30660b4f2lrp52xi5sik9s6frr7jfxqxjsf29lbb3"))))
    (build-system gnu-build-system)
    (synopsis "Multiplayer strategy game")
    (description
     "GNU Motti is a simple multiplayer strategy game played in a terminal.
The objective of the game is to conquer enemy capitals by occupying and
encircling territory.")
    (home-page "https://www.gnu.org/software/motti")
    (license gpl3+)))
