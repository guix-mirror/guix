;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Joshua S. Grant <jgrant@parenthetical.io>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages libffcall)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix utils) ; string-replace-substring
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu))

(define-public libffcall
   (package
    (name "libffcall")
    (version "1.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.savannah.gnu.org/r/libffcall.git")
             (commit (string-append
                       "ffcall-" (string-replace-substring version "." "-")))))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "0p7gg5k4ifcqpmrmdfmr2r4x909cn35g87iff539p6i8891fdfxf"))))
    (build-system gnu-build-system)
    (arguments `(#:parallel-build? #f))
    (synopsis "Foreign function calls from interpreters")
    (description
     "GNU Libffcall is a collection of libraries that can be used to build
foreign function call interfaces in embedded interpreters.")
    (home-page "https://www.gnu.org/software/libffcall/")
    (supported-systems (delete "aarch64-linux" %supported-systems))
    (license l:gpl2+)))
