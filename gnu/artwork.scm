;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu artwork)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:export (%artwork-repository))

;;; Commentary:
;;;
;;; Common place for the definition of the Guix artwork repository.
;;;
;;; Code:

(define %artwork-repository
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "git://git.savannah.gnu.org/guix/guix-artwork.git")
          (commit "94e6111")))
    (sha256
     (base32
      "0aax85mygpq4lbyn8sriafikyg54yss3lisin6pwkvlvp23a3v1j"))))

;;; artwork.scm ends here
