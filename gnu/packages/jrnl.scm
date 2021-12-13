;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <address@hidden>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time))

(define-public jrnl
  (package
    (name "jrnl")
    (version "1.9.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/maebert/jrnl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bjjyfc3fnsy0h299s3jj42wnigj43xdyp5ppi1cvkqbwl369935"))))
    (build-system python-build-system)
    (native-inputs
     (list behave))
    (inputs
     (list python
           python-keyring
           python-pycrypto
           python-pytz
           python-tzlocal
           python-six
           python-dateutil
           python-parsedatetime))
    (home-page "https://maebert.github.io/jrnl/")
    (synopsis "Personal journal application")
    (description
     "Jrnl is a command line journal application that stores your journal in a
plain text file.  Optionally, your journal can be encrypted using 256-bit AES
encryption.")
    (license x11)))
