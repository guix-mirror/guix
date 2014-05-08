;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages mpi)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (srfi srfi-1))

(define-public openmpi
  (package
    (name "openmpi")
    (version "1.8.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://www.open-mpi.org/software/ompi/v"
                          (string-join (take (string-split version #\.) 2)
                                       ".")
                          "/downloads/openmpi-" version ".tar.bz2"))
      (sha256
       (base32
        "13z1q69f3qwmmhpglarfjminfy2yw4rfqr9jydjk5507q3mjf50p"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("gfortran" ,gfortran-4.8)))
    (arguments
     `(#:configure-flags '("--enable-static"
                           "--enable-oshmem"
                           ;; Thread support causes some applications to hang
                           ;; "--enable-event-thread-support"
                           ;; "--enable-opal-multi-threads"
                           ;; "--enable-orte-progress-threads"
                           ;; "--enable-mpi-thread-multiple"
                           "--enable-mpi-ext=all"
                           "--with-devel-headers")))
    (home-page "http://www.open-mpi.org")
    (synopsis "MPI-2 implementation")
    (description
     "The Open MPI Project is an MPI-2 implementation that is developed and
maintained by a consortium of academic, research, and industry partners.  Open
MPI is therefore able to combine the expertise, technologies, and resources
from all across the High Performance Computing community in order to build the
best MPI library available.  Open MPI offers advantages for system and
software vendors, application developers and computer science researchers.")
    ;; See file://LICENSE
    (license bsd-2)))
