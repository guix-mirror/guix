;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages unrtf)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages base))

(define-public unrtf
  (package
    (name "unrtf")
    (version "0.21.10")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/unrtf/unrtf-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1bil6z4niydz9gqm2j861dkxmqnpc8m7hvidsjbzz7x63whj17xl"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/unrtf/")
    (synopsis "Convert Rich Text Format documents to other formats")
    (description
     "GNU UnRTF converts text documents from RTF to HTML, LaTeX, or troff.
It supports changes in font characteristics, underlines and strikethroughs,
superscripts and subscripts, and more.")
    (license gpl3+)))
