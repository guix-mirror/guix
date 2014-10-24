;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages cryptsetup)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages linux))

(define-public cryptsetup
  (package
   (name "cryptsetup")
   (version "1.6.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://cryptsetup.googlecode.com/files/cryptsetup-"
                                version ".tar.bz2"))
            (sha256
             (base32
              "170lalkhh2fa316d12i6r7jprm0yss3c949d91069sq37ik6xwxs"))))
   (build-system gnu-build-system)
   (inputs
    `(("libgcrypt" ,libgcrypt)
      ("lvm2" ,lvm2)
      ("util-linux" ,util-linux)
      ("popt" ,popt)))
   (native-inputs
      `(("python" ,python-wrapper)))
   (synopsis "Hard disk encryption tool")
   (description
    "LUKS (Linux Unified Key Setup)/Cryptsetup provides a standard on-disk
encryption format, which does not only facilitate compatibility among
distributions, but which also provides secure management of multiple user
passwords.  In contrast to existing solutions, LUKS stores all setup necessary
setup information in the partition header, enabling the users to transport
or migrate their data seamlessly.")
   (license license:gpl2)
   (home-page "http://code.google.com/p/cryptsetup/")))
