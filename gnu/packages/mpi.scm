;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2018, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017 Dave Love <fx@gnu.org>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages fabric-management)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages valgrind)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public hwloc
  ;; Note: For now we keep 1.x as the default because many packages have yet
  ;; to migrate to 2.0.
  (package
    (name "hwloc")
    (version "1.11.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.open-mpi.org/software/hwloc/v"
                                  (version-major+minor version)
                                  "/downloads/hwloc-" version ".tar.bz2"))
              (sha256
               (base32
                "0za1b9lvrm3rhn0lrxja5f64r0aq1qs4m0pxn1ji2mbi8ndppyyx"))))
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
         (add-before 'check 'skip-linux-libnuma-test
           (lambda _
             ;; Arrange to skip 'tests/linux-libnuma', which fails on some
             ;; machines: <https://github.com/open-mpi/hwloc/issues/213>.
             (substitute* "tests/linux-libnuma.c"
               (("numa_available\\(\\)")
                "-1"))
             #t))
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

(define-public hwloc-2.0
  ;; Note: 2.0 isn't the default yet, see above.
  (package
    (inherit hwloc)
    (version "2.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.open-mpi.org/software/hwloc/v"
                                  (version-major+minor version)
                                  "/downloads/hwloc-" version ".tar.bz2"))
              (sha256
               (base32
                "09f7ajak8wv5issr0hw72vs3jkldc7crcc7z5fd34sspkvrsm4z3"))))

    ;; libnuma is no longer needed.
    (inputs (alist-delete "numactl" (package-inputs hwloc)))
    (arguments
     (substitute-keyword-arguments (package-arguments hwloc)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'skip-linux-libnuma-test
             (lambda _
               ;; Arrange to skip 'tests/hwloc/linux-libnuma', which fails on
               ;; some machines: <https://github.com/open-mpi/hwloc/issues/213>.
               (substitute* "tests/hwloc/linux-libnuma.c"
                 (("numa_available\\(\\)")
                  "-1"))
               #t))))))))

(define-public openmpi
  (package
    (name "openmpi")
    (version "4.0.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.open-mpi.org/software/ompi/v"
                          (version-major+minor version)
                          "/downloads/openmpi-" version ".tar.bz2"))
      (sha256
       (base32 "02cpzcp113gj5hb0j2xc0cqma2fn04i2i0bzf80r71120p9bdryc"))))
    (build-system gnu-build-system)
    (inputs
     `(("hwloc" ,hwloc "lib")
       ("gfortran" ,gfortran)
       ("libfabric" ,libfabric)
       ("libevent" ,libevent)
       ("opensm" ,opensm)
       ,@(if (and (not (%current-target-system))
                  (member (%current-system) (package-supported-systems psm)))
             `(("psm" ,psm))
             '())
       ,@(if (and (not (%current-target-system))
                  (member (%current-system) (package-supported-systems psm2)))
             `(("psm2" ,psm2))
             '())
       ("rdma-core" ,rdma-core)
       ("valgrind" ,valgrind)
       ("slurm" ,slurm)))              ;for PMI support (launching via "srun")
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("perl" ,perl)))
    (outputs '("out" "debug"))
    (arguments
     `(#:configure-flags `("--enable-mpi-ext=affinity" ;cr doesn't work
                           "--enable-memchecker"
                           "--with-sge"
                           "--with-valgrind"
                           "--with-hwloc=external"
                           "--with-libevent"

                           ;; InfiniBand support
                           "--enable-openib-control-hdr-padding"
                           "--enable-openib-dynamic-sl"
                           "--enable-openib-udcm"
                           "--enable-openib-rdmacm"
                           "--enable-openib-rdmacm-ibaddr"

                           ;; Enable support for SLURM's Process Manager
                           ;; Interface (PMI).
                           ,(string-append "--with-pmi="
                                           (assoc-ref %build-inputs "slurm")))
       #:phases (modify-phases %standard-phases
                  ;; opensm is needed for InfiniBand support.
                  (add-after 'unpack 'find-opensm-headers
                    (lambda* (#:key inputs #:allow-other-keys)
                      (setenv "C_INCLUDE_PATH"
                              (string-append (assoc-ref inputs "opensm")
                                             "/include/infiniband/:"
                                             (getenv "C_INCLUDE_PATH")))
                      (setenv "CPLUS_INCLUDE_PATH"
                              (string-append (assoc-ref inputs "opensm")
                                             "/include/infiniband/:"
                                             (getenv "CPLUS_INCLUDE_PATH")))
                      #t))
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
                          '("./ompi/mca/io/romio321/src/io_romio321_component.c")
                        (("MCA_io_romio321_COMPLETE_CONFIGURE_FLAGS")
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

;; TODO: javadoc files contain timestamps.
(define-public java-openmpi
  (package (inherit openmpi)
    (name "java-openmpi")
    (inputs
     `(("openmpi" ,openmpi)
       ,@(package-inputs openmpi)))
    (native-inputs
     `(("jdk" ,openjdk11 "jdk")
       ("zip" ,(@ (gnu packages compression) zip))
       ,@(package-native-inputs openmpi)))
    (outputs '("out"))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  ((guix build ant-build-system) #:prefix ant:)
                  (guix build utils))
       #:imported-modules ((guix build ant-build-system)
                           (guix build syscalls)
                           ,@%gnu-build-system-modules)
       ,@(substitute-keyword-arguments (package-arguments openmpi)
           ((#:configure-flags flags)
            `(cons "--enable-mpi-java" ,flags))
           ((#:make-flags flags ''())
            `(append '("-C" "ompi/mpi/java")
                     ,flags))
           ((#:phases phases)
            `(modify-phases ,phases
               ;; We could provide the location of the JDK in the configure
               ;; flags, but since the configure flags are embedded in the
               ;; info binaries that would leave a reference to the JDK in
               ;; the "out" output.  To avoid this we set JAVA_HOME.
               (add-after 'unpack 'set-JAVA_HOME
                 (lambda* (#:key inputs #:allow-other-keys)
                   (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))
                   #t))
               (add-after 'unpack 'link-with-existing-mpi-libraries
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "ompi/mpi/java/c/Makefile.in"
                     (("\\$\\(top_builddir\\)/ompi/lib@OMPI_LIBMPI_NAME@.la")
                      (string-append (assoc-ref inputs "openmpi") "/lib/libmpi.la")))
                   #t))
               (add-after 'install 'strip-jar-timestamps
                 (assoc-ref ant:%standard-phases 'strip-jar-timestamps)))))))
    (synopsis "Java bindings for MPI")))

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

;;; Build phase to be used for packages that execute MPI code.
(define-public %openmpi-setup
  '(lambda _
     ;; By default, running the test suite would fail because 'ssh' could not
     ;; be found in $PATH.  Define this variable to placate Open MPI without
     ;; adding a dependency on OpenSSH (the agent isn't used anyway.)
     (setenv "OMPI_MCA_plm_rsh_agent" (which "false"))
     ;; Allow oversubscription in case there are less physical cores available
     ;; in the build environment than the package wants while testing.
     (setenv "OMPI_MCA_rmaps_base_mapping_policy" "core:OVERSUBSCRIBE")
     #t))

(define-public python-mpi4py
  (package
    (name "python-mpi4py")
    (version "3.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "mpi4py" version))
        (sha256
          (base32
            "0ld8rjmsjr0dklvj2g1gr3ax32sdq0xjxyh0cspknc1i36waajb5"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'mpi-setup
           ,%openmpi-setup)
         (add-before 'check 'pre-check
           (lambda _
             ;; Skip BaseTestSpawn class (causes error 'ompi_dpm_dyn_init()
             ;; failed --> Returned "Unreachable"' in chroot environment).
             (substitute* "test/test_spawn.py"
               (("unittest.skipMPI\\('openmpi\\(<3.0.0\\)'\\)")
                "unittest.skipMPI('openmpi')"))
             #t)))))
    (inputs
     `(("openmpi" ,openmpi)))
    (home-page "https://bitbucket.org/mpi4py/mpi4py/")
    (synopsis "Python bindings for the Message Passing Interface standard")
    (description "MPI for Python (mpi4py) provides bindings of the Message
Passing Interface (MPI) standard for the Python programming language, allowing
any Python program to exploit multiple processors.

mpi4py is constructed on top of the MPI-1/MPI-2 specification and provides an
object oriented interface which closely follows MPI-2 C++ bindings.  It
supports point-to-point and collective communications of any picklable Python
object as well as optimized communications of Python objects (such as NumPy
arrays) that expose a buffer interface.")
    (license bsd-3)))
