;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages smalltalk)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libsigsegv)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages zip))

(define-public smalltalk
  (package
    (name "smalltalk")
    (version "3.2.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/smalltalk/smalltalk-"
                          version ".tar.xz"))
      (sha256
       (base32
        "1k2ssrapfzhngc7bg1zrnd9n2vyxp9c9m70byvsma6wapbvib6l1"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("libffi" ,libffi)
       ("libltdl" ,libltdl)
       ("libsigsegv" ,libsigsegv)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("zip" ,zip)))
    (arguments
     `(#:phases (alist-cons-before
                 'configure 'fix-libc
                 (lambda _
                   (let ((libc (assoc-ref %build-inputs "libc")))
                     (substitute* "libc.la.in"
                       (("@LIBC_SO_NAME@") "libc.so")
                       (("@LIBC_SO_DIR@")  (string-append libc "/lib")))))
                %standard-phases)))
    (home-page "http://smalltalk.gnu.org/")
    (synopsis "Smalltalk environment")
    (description
     "GNU Smalltalk is a free implementation of the Smalltalk language.  It
implements the ANSI standard for the language and also includes extra classes
such as ones for networking and GUI programming.")
    (license gpl2+)))
