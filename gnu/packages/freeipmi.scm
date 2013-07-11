;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages freeipmi)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages gnupg))

(define-public freeipmi
  (package
    (name "freeipmi")
    (version "1.2.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/freeipmi/freeipmi-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0aqbjlqv8nh5nnfx5icyvcvws87xnwqjqsfszjx0jqb539snrc58"))))
    (build-system gnu-build-system)
    (inputs
     `(("readline" ,readline) ("libgcrypt" ,libgcrypt)))
    (home-page "http://www.gnu.org/software/freeipmi/")
    (synopsis "Platform management, including sensor and power monitoring")
    (description
     "GNU FreeIPMI provides in-band and out-of-band IPMI software based on the
IPMI v1.5/2.0 specification.  The IPMI specification defines a set of
interfaces for platform management and is implemented by a number vendors for
system management.  The features of IPMI that most users will be interested in
are sensor monitoring, system event monitoring, power control, and
serial-over-LAN (SOL).  The FreeIPMI tools and libraries listed below should
provide users with the ability to access and utilize these and many other
features.  A number of useful features for large HPC or cluster environments
have also been implemented into FreeIPMI.")
    (license gpl3+)))
