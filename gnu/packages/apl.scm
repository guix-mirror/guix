;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (gnu packages apl)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((gnu packages gettext)
                #:renamer (symbol-prefix-proc 'guix:))
  #:use-module (gnu packages maths)
  #:use-module (gnu packages readline))

(define-public apl
  (package
    (name "apl")
    (version "1.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/apl/apl-" version ".tar.gz"))
      (sha256
       (base32
        "1myinxa0m3y4fanpxflfakfk3m1s8641wdlbwbs0vg5yp10xm0m3"))))
    (build-system gnu-build-system)
    (home-page "http://www.gnu.org/software/apl/")
    (inputs
     `(("gettext" ,guix:gettext)
       ("lapack" ,lapack)
       ("readline" ,readline)))
    (synopsis "APL interpreter")
    (description
     "GNU APL is a free interpreter for the programming language APL.  It is
an implementation of the ISO standard 13751.")
    (license gpl3+)))
