;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017 Dave Love <fx@gnu.org>
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

(define-module (gnu packages mpi)
  #:use-module (guix packages)
  #:use-module ((guix licenses)
                #:hide (expat))
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages valgrind)
  #:use-module (ice-9 match))

(define-public hwloc
  (package
    (name "hwloc")
    (version "1.11.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.open-mpi.org/software/hwloc/v"
                                  (version-major+minor version)
                                  "/downloads/hwloc-" version ".tar.bz2"))
              (sha256
               (base32
                "0karxv4r1r8sa7ki5aamlxdvyvz0bvzq4gdhq0yi5nc4a0k11vzc"))))
    (build-system gnu-build-system)
    (outputs '("out"           ;'lstopo' & co., depends on Cairo, libx11, etc.
               "lib"           ;small closure
               "debug"))
    (inputs
     `(("libx11" ,libx11)
       ("cairo" ,cairo)
       ("ncurses" ,ncurses)
       ("expat" ,expat)
       ,@(if (not (string-prefix? "armhf"
                                  (or (%current-target-system)
                                      (%current-system))))
             `(("numactl" ,numactl))
             '())))
    (propagated-inputs
     ;; hwloc.pc lists it in 'Requires.private'.
     `(("libpciaccess" ,libpciaccess)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags '("--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'refine-libnuma
           ;; Give -L arguments for libraries to avoid propagation
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out  (assoc-ref outputs "lib"))
                   (numa (assoc-ref inputs "numactl")))
               (substitute* (map (lambda (f) (string-append out "/" f))
                                 '("lib/pkgconfig/hwloc.pc" "lib/libhwloc.la"))
                 (("-lnuma" lib)
                  (string-append "-L" numa "/lib " lib))))))
         (add-after 'install 'avoid-circular-references
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((lib (assoc-ref outputs "lib")))
               ;; Suppress the 'prefix=' and 'exec_prefix=' lines so that the
               ;; "lib" output doesn't refer to "out".
               (substitute* (string-append lib "/lib/pkgconfig/hwloc.pc")
                 (("^.*prefix=.*$")
                  ""))
               #t))))))
    (home-page "https://www.open-mpi.org/projects/hwloc/")
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
    (license bsd-3)))

(define-public openmpi
  (package
    (name "openmpi")
    (version "1.10.7")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.open-mpi.org/software/ompi/v"
                          (version-major+minor version)
                          "/downloads/openmpi-" version ".tar.bz2"))
      (sha256
       (base32
        "142s1vny9gllkq336yafxayjgcirj2jv0ddabj879jgya7hyr2d0"))))
    (build-system gnu-build-system)
    (inputs
     `(("hwloc" ,hwloc "lib")
       ("gfortran" ,gfortran)
       ("libfabric" ,libfabric)
       ,@(match (%current-system)
                ((member (package-supported-systems psm))
                 `(("psm" ,psm)))
                (_ `()))
       ("rdma-core" ,rdma-core)
       ("valgrind" ,valgrind)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("perl" ,perl)))
    (outputs '("out" "debug"))
    (arguments
     `(#:configure-flags `("--enable-mpi-ext=affinity" ;cr doesn't work
                           "--enable-memchecker"
                           "--with-sge"

                           ;; VampirTrace is obsoleted by scorep and disabling
                           ;; it reduces the closure size considerably.
                           "--disable-vt"

                           ,(string-append "--with-valgrind="
                                           (assoc-ref %build-inputs "valgrind"))
                           ,(string-append "--with-hwloc="
                                           (assoc-ref %build-inputs "hwloc")))
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'remove-absolute
                    (lambda _
                      ;; Remove compiler absolute file names (OPAL_FC_ABSOLUTE
                      ;; etc.) to reduce the closure size.  See
                      ;; <https://lists.gnu.org/archive/html/guix-devel/2017-07/msg00388.html>
                      ;; and
                      ;; <https://www.mail-archive.com/users@lists.open-mpi.org//msg31397.html>.
                      (substitute* '("orte/tools/orte-info/param.c"
                                     "oshmem/tools/oshmem_info/param.c"
                                     "ompi/tools/ompi_info/param.c")
                        (("_ABSOLUTE") ""))
                      ;; Avoid valgrind (which pulls in gdb etc.).
                      (substitute*
                          '("./ompi/mca/io/romio/src/io_romio_component.c")
                        (("MCA_io_romio_COMPLETE_CONFIGURE_FLAGS")
                         "\"[elided to reduce closure]\""))
                      #t))
                  (add-before 'build 'scrub-timestamps ;reproducibility
                    (lambda _
                      (substitute* '("ompi/tools/ompi_info/param.c"
                                     "orte/tools/orte-info/param.c"
                                     "oshmem/tools/oshmem_info/param.c")
                        ((".*(Built|Configured) on.*") ""))
                      #t))
                  (add-after 'install 'remove-logs ;reproducibility
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (for-each delete-file (find-files out "config.log"))
                        #t))))))
    (home-page "http://www.open-mpi.org")
    (synopsis "MPI-3 implementation")
    (description
     "The Open MPI Project is an MPI-3 implementation that is developed and
maintained by a consortium of academic, research, and industry partners.  Open
MPI is therefore able to combine the expertise, technologies, and resources
from all across the High Performance Computing community in order to build the
best MPI library available.  Open MPI offers advantages for system and
software vendors, application developers and computer science researchers.")
    ;; See file://LICENSE
    (license bsd-2)))

(define-public openmpi-thread-multiple
  (package
    (inherit openmpi)
    (name "openmpi-thread-multiple")
    (arguments
     (substitute-keyword-arguments (package-arguments openmpi)
       ((#:configure-flags flags)
        `(cons "--enable-mpi-thread-multiple" ,flags))))
    (description " This version of Open@tie{}MPI has an implementation of
@code{MPI_Init_thread} that provides @code{MPI_THREAD_MULTIPLE}.  This won't
work correctly with all transports (such as @code{openib}), and the
performance is generally worse than the vanilla @code{openmpi} package, which
only provides @code{MPI_THREAD_FUNNELED}.")))
