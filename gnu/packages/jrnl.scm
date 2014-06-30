;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <address@hidden>
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

(define-module (gnu packages jrnl)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python))

(define-public jrnl
  (package
    (name "jrnl")
    (version "1.8.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/maebert/jrnl/archive/"
                          version ".tar.gz"))
      (sha256
       (base32
        "019ky09sj5i7frmca0imv4jm46mn3f4lzah2wmiwxh22cisj7ksn"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools" ,python-setuptools)
       ("behave" ,behave)))
    (inputs
     `(("python" ,python)
       ("python-keyring" ,python-keyring)
       ("python-pycrypto" ,python-pycrypto)
       ("python-pytz" ,python-pytz)
       ("python-tzlocal" ,python-tzlocal)
       ("python-six" ,python-six)
       ("python-dateutil" , python-dateutil-2)
       ("python-parsedatetime" ,python-parsedatetime)))
    (home-page "http://maebert.github.io/jrnl/")
    (synopsis "Personal journal application")
    (description
     "Jrnl is a command line journal application that stores your journal in a
plain text file.  Optionally, your journal can be encrypted using 256-bit AES
encryption.")
    (license x11)))
