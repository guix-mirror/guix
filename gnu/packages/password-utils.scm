;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Steve Sprang <scs@stevesprang.com>
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

(define-module (gnu packages password-utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public pwgen
  (package
    (name "pwgen")
    (version "2.07")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/pwgen/pwgen-"
                           version ".tar.gz"))
       (sha256
        (base32 "0mhmw700kkh238fzivcwnwi94bj9f3h36yfh3k3j2v19b0zmjx7b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ; no test suite
    (home-page "http://pwgen.sourceforge.net/")
    (synopsis "Password generator")
    (description "Pwgen generates passwords which can be easily memorized by a
human.")
    (license license:gpl2)))
