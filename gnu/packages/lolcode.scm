;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages lolcode)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (guix build-system cmake)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public lci
  (package
    (name "lci")
    (version "0.11.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/justinmeza/lci")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0syw60b93iajgh91ffchirwwhm2kix2753ibx845kyrhzggmdh2l"))))
    (build-system cmake-build-system)
    (inputs
     `(("readline" ,readline)))
    (native-inputs
     `(("python-2" ,python-2)))         ; for the tests
    (synopsis "LOLCODE interpreter written in C")
    (description
     "@code{lci} is a LOLCODE interpreter written in C and is designed to be
correct, portable, fast, and precisely documented.
@enumerate
@item correct: Every effort has been made to test lci's conformance to the
LOLCODE language specification.  Unit tests come packaged with the lci source code.
@item portable: lci follows the widely ported ANSI C specification allowing it
to compile on a broad range of systems.
@item fast: Much effort has gone into producing simple and efficient code
whenever possible to the extent that the above points are not compromized.
@end enumerate")
    (home-page "http://lolcode.org/")
    (license license:gpl3+)))
