;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
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
  (let ((commit "e951905f79dcc92fba9cd05d9190afa3ee2d1d50"))
    (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://git.savannah.gnu.org/git/guix/guix-artwork.git")
             (commit commit)))
      (file-name (string-append "guix-artwork-" (string-take commit 7)
                                "-checkout"))
      (sha256
       (base32
        "1d79ackvs3bqk43ff91457qmdb6g4nwfknzs3dyrhni1ynk1i976")))))

;;; artwork.scm ends here
