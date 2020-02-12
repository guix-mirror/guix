;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Joshua S. Grant <jgrant@parenthetical.io>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
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
  #:use-module (guix build-system gnu))

(define-public libffcall
   (package
    (name "libffcall")
    (version "2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://gnu/libffcall/libffcall-" version ".tar.gz"))
       (sha256
        (base32
         "0iwcad6w78jp84vd6xaz5fwqm84n3cb42bdf5m5cj5xzpa5zp4d0"))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-build? #f
       #:configure-flags '("--disable-static")))
    (synopsis "Foreign function calls from interpreters")
    (description
     "GNU Libffcall is a collection of libraries that can be used to build
foreign function call interfaces in embedded interpreters.")
    (home-page "https://www.gnu.org/software/libffcall/")
    (license l:gpl2+)))
