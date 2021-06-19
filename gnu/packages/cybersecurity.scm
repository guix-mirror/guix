;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 c4droid <c4droid@foxmail.com>
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

(define-module (gnu packages cybersecurity)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system python)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages time)
  #:use-module (gnu packages bioinformatics)      ;python-intervaltree
  #:use-module (gnu packages emulators))

(define-public ropgadget
  (package
    (name "ropgadget")
    (version "6.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ROPGadget" version))
       (sha256
        (base32 "0p4h8xi27xcicz8sq6xi40hbj99mcsnnla6ar2r17vqapbr5c3jc"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-capstone" ,python-capstone)))
    (home-page "http://shell-storm.org/project/ROPgadget/")
    (synopsis "Semiautomatic return oriented programming")
    (description
     "This tool lets you search for @acronym{ROP, Return Oriented Programming}
gadgets in binaries.  Some facilities are included for automatically generating
chains of gadgets to execute system calls.")
    (license license:bsd-3)))

(define-public pwntools
  (package
    (name "pwntools")
    (version "4.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pwntools" version))
       (sha256
        (base32
         "1qw7j0wwm1878aia08gyw5xljjr26qsbp45w65n4qff672sha5n5"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f))                 ;XXX: needs a specific version of unicorn
    (propagated-inputs
     `(("capstone" ,capstone)
       ("python-dateutil" ,python-dateutil)
       ("python-intervaltree" ,python-intervaltree)
       ("python-mako" ,python-mako)
       ("python-packaging" ,python-packaging)
       ("python-paramiko" ,python-paramiko)
       ("python-psutil" ,python-psutil)
       ("python-pyelftools" ,python-pyelftools)
       ("python-pygments" ,python-pygments)
       ("python-pyserial" ,python-pyserial)
       ("python-pysocks" ,python-pysocks)
       ("python-requests" ,python-requests)
       ("ropgadget" ,ropgadget)
       ("python-six" ,python-six)
       ("python-sortedcontainers"
        ,python-sortedcontainers)
       ("unicorn" ,unicorn)))
    (home-page "https://github.com/Gallopsled/pwntools")
    (synopsis
     "Capture-the-flag (CTF) framework and exploit development library")
    (description
     "Pwntools is a capture-the-flag (CTF) framework and exploit development library.
Written in Python, it is designed for rapid prototyping and development, and
intended to make exploit writing as simple as possible.")
    (license license:expat)))
