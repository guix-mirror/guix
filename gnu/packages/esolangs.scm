;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Hendursaga <hendursaga@yahoo.com>
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

(define-module (gnu packages esolangs)
  #:use-module (gnu packages ncurses)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public cfunge
  (package
    (name "cfunge")
    (version "0.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/VorpalBlade/cfunge")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18ir0h10vxdb5jb57w5hjbgi8spjxg9x2148agadhhmbhsja02m7"))))
    (build-system cmake-build-system)
    (arguments
      ;; The tests are not designed to be run and evaluated automatically.
     '(#:tests? #f))
    (inputs
     `(("ncurses" ,ncurses)))
    (home-page "https://github.com/VorpalBlade/cfunge")
    (synopsis "Fast conforming Befunge93/98/109 interpreter in C")
    (description "@command{cfunge} is a fast conforming Befunge93/98/109 interpreter
written in C.  It supports several @dfn{fingerprints} (opt-in language extensions
identified by unique ID codes).")
    (license license:gpl3)))
