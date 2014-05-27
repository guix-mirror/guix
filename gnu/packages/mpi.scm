;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module ((guix licenses)
                #:hide (expat))
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages valgrind)
  #:use-module (srfi srfi-1))

(define-public hwloc
  (package
    (name "hwloc")
    (version "1.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.open-mpi.org/software/hwloc/v"
                                  version "/downloads/hwloc-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0zjgiili2a8v63s8ly3a8qp8ibxv1jw3zbgm7diic3w1qgqiza14"))))
    (build-system gnu-build-system)
    (arguments
     ;; Enable libpci support, which effectively makes hwloc GPLv2+.
     '(#:configure-flags '("--enable-libpci")))
    (inputs
     `(("libx11" ,libx11)
       ("cairo" ,cairo)
       ("ncurses" ,ncurses)
       ("expat" ,expat)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     ;; 'hwloc.pc' refers to libpci and libnuma, hence the propagation.
     `(("numactl" ,numactl)
       ("pciutils" ,pciutils)))
    (home-page "http://www.open-mpi.org/projects/hwloc/")
    (synopsis "Abstraction of hardware architectures")
    (description
     "hwloc provides a portable abstraction (across OS,
versions, architectures, ...) of the hierarchical topology of modern
architectures, including NUMA memory nodes, sockets, shared caches, cores and
simultaneous multithreading.  It also gathers various attributes such as cache
and memory information.  It primarily aims at helping high-performance
computing applications with gathering information about the hardware so as to
exploit it accordingly and efficiently.

hwloc may display the topology in multiple convenient formats.  It also offers
a powerful programming interface to gather information about the hardware,
bind processes, and much more.")

    ;; But see above about linking against libpci.
    (license bsd-3)))

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
    (inputs
     `(("hwloc" ,hwloc)
       ("gfortran" ,gfortran-4.8)
       ("valgrind" ,valgrind)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags `("--enable-static"
                           "--enable-oshmem"
                           ;; Thread support causes some applications to hang
                           ;; "--enable-event-thread-support"
                           ;; "--enable-opal-multi-threads"
                           ;; "--enable-orte-progress-threads"
                           ;; "--enable-mpi-thread-multiple"
                           "--enable-mpi-ext=all"
                           "--with-devel-headers"
                           "--enable-debug"
                           "--enable-memchecker"
                           ,(string-append "--with-valgrind="
                                           (assoc-ref %build-inputs "valgrind"))
                           ,(string-append "--with-hwloc="
                                           (assoc-ref %build-inputs "hwloc")))))
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
