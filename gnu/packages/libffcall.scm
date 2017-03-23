;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Joshua S. Grant <jgrant@parenthetical.io>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages libffcall)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix cvs-download)
  #:use-module (guix build-system gnu))

(define-public libffcall
   (package
    (name "libffcall")
    (version "1.10+cvs-2015-01-15")
    (source
     (origin
       (method cvs-fetch)
       (uri (cvs-reference
             (root-directory
              ":pserver:anonymous@cvs.savannah.gnu.org:/sources/libffcall")
             (module "ffcall")
             (revision "2015-01-15")))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "1lwdskc2w4rr98x9flr2726lmj4190l16r0izg7gqxy50801wwgd"))))
    (build-system gnu-build-system)
    (arguments `(#:parallel-build? #f))
    (synopsis "Foreign function calls from interpreters")
    (description
     "GNU Libffcall is a collection of libraries that can be used to build
foreign function call interfaces in embedded interpreters.")
    (home-page "https://www.gnu.org/software/libffcall/")
    (supported-systems (delete "aarch64-linux" %supported-systems))
    (license l:gpl2+)))
