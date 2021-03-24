;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2016, 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Amin Bandali <bandali@gnu.org>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Andrew Miloradovsky <andrew@interpretmath.pw>
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

(define-module (gnu packages fpga)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages libftdi))

(define-public abc
 (let ((commit "5ae4b975c49c")
       (revision "1"))
  (package
    (name "abc")
    (version (git-version "0.0" revision commit))
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://bitbucket.org/alanmi/abc/get/" commit ".zip"))
              (file-name (string-append name "-" version "-checkout.zip"))
              (sha256
               (base32
                "1syygi1x40rdryih3galr4q8yg1w5bvdzl75hd27v1xq0l5bz3d0"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("readline" ,readline)))
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-bin (string-append out "/bin")))
               (install-file "abc" out-bin)))))))
    (home-page "https://people.eecs.berkeley.edu/~alanmi/abc/")
    (synopsis "Sequential logic synthesis and formal verification")
    (description "ABC is a program for sequential logic synthesis and
formal verification.")
    (license
      (license:non-copyleft "https://fedoraproject.org/wiki/Licensing:MIT#Modern_Variants")))))

(define-public iverilog
  (package
    (name "iverilog")
    (version "10.3")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "ftp://ftp.icarus.com/pub/eda/verilog/v10/"
                              "verilog-" version ".tar.gz"))
              (sha256
               (base32
                "1vv88ckvfwq7mrysyjnilsrcrzm9d173kp9w5ivwh6rdw7klbgc6"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("flex" ,flex)
       ("bison" ,bison)
       ("ghostscript" ,ghostscript)))   ; ps2pdf
    (home-page "http://iverilog.icarus.com/")
    (synopsis "FPGA Verilog simulation and synthesis tool")
    (description "Icarus Verilog is a Verilog simulation and synthesis tool.
It operates as a compiler, compiling source code written in Verilog
(IEEE-1364) into some target format.
For batch simulation, the compiler can generate an intermediate form
called vvp assembly.
This intermediate form is executed by @command{vvp}.
For synthesis, the compiler generates netlists in the desired format.")
    ;; GPL2 only because of:
    ;; - ./driver/iverilog.man.in
    ;; - ./iverilog-vpi.man.in
    ;; - ./tgt-fpga/iverilog-fpga.man
    ;; - ./vvp/vvp.man.in
    ;; Otherwise would be GPL2+.
    ;; You have to accept both GPL2 and LGPL2.1+.
    (license (list license:gpl2 license:lgpl2.1+))))

(define-public yosys
  (package
    (name "yosys")
    (version "0.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cliffordwolf/yosys")
                    (commit (string-append "yosys-" version))
                    (recursive? #t))) ; for the ‘iverilog’ submodule
              (sha256
                (base32
                   "0lb9r055h8y1vj2z8gm4ip0v06j5mk7f9zx9gi67kkqb7g4rhjli"))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "Makefile"
                    (("ABCREV = .*") "ABCREV = default\n"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags (list "CC=gcc"
                          "CXX=g++"
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-paths
           (lambda _
             (substitute* "./passes/cmds/show.cc"
               (("exec xdot") (string-append "exec " (which "xdot")))
               (("dot -") (string-append (which "dot") " -"))
               (("fuser") (which "fuser")))
             #t))
         (replace 'configure
           (lambda* (#:key inputs (make-flags '()) #:allow-other-keys)
             (apply invoke "make" "config-gcc" make-flags)))
         (add-after 'configure 'prepare-abc
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((sourceabc (assoc-ref inputs "abc"))
                    (sourcebin (string-append sourceabc "/bin"))
                    (source (string-append sourcebin "/abc")))
                   (mkdir-p "abc")
                   (call-with-output-file "abc/Makefile"
                     (lambda (port)
                       (format port ".PHONY: all\nall:\n\tcp -f abc abc-default\n")))
                   (copy-file source "abc/abc")
                   (invoke "chmod" "+w" "abc/abc"))))
          (add-before 'check 'fix-iverilog-references
             (lambda* (#:key inputs native-inputs #:allow-other-keys)
               (let* ((xinputs (or native-inputs inputs))
                      (xdirname (assoc-ref xinputs "iverilog"))
                      (iverilog (string-append xdirname "/bin/iverilog")))
                     (substitute* '("./manual/CHAPTER_StateOfTheArt/synth.sh"
                                    "./manual/CHAPTER_StateOfTheArt/validate_tb.sh"
                                    "./techlibs/ice40/tests/test_bram.sh"
                                    "./techlibs/ice40/tests/test_ffs.sh"
                                    "./techlibs/xilinx/tests/bram1.sh"
                                    "./techlibs/xilinx/tests/bram2.sh"
                                    "./tests/bram/run-single.sh"
                                    "./tests/realmath/run-test.sh"
                                    "./tests/simple/run-test.sh"
                                    "./tests/techmap/mem_simple_4x1_runtest.sh"
                                    "./tests/tools/autotest.sh"
                                    "./tests/vloghtb/common.sh")
                        (("if ! which iverilog") "if ! true")
                        (("iverilog ") (string-append iverilog " "))
                        (("iverilog_bin=\".*\"") (string-append "iverilog_bin=\""
                                                                iverilog "\"")))
                     #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python)
       ("bison" ,bison)
       ("flex" ,flex)
       ("gawk" , gawk) ; for the tests and "make" progress pretty-printing
       ("tcl" ,tcl) ; tclsh for the tests
       ("iverilog" ,iverilog))) ; for the tests
    (inputs
     `(("tcl" ,tcl)
       ("readline" ,readline)
       ("libffi" ,libffi)
       ("graphviz" ,graphviz)
       ("psmisc" ,psmisc)
       ("xdot" ,xdot)
       ("abc" ,abc)))
    (propagated-inputs
     `(("z3" ,z3))) ; should be in path for yosys-smtbmc
    (home-page "http://www.clifford.at/yosys/")
    (synopsis "FPGA Verilog RTL synthesizer")
    (description "Yosys synthesizes Verilog-2005.")
    (license license:isc)))

(define-public icestorm
  (let ((commit "0ec00d892a91cc68e45479b46161f649caea2933")
        (revision "3"))
   (package
    (name "icestorm")
    (version (git-version "0.0" revision commit))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/cliffordwolf/icestorm")
                     (commit commit)))
              (file-name (git-file-name name version))
              (sha256
                (base32
                   "1qlh99fafb7xga702k64fmc9m700nsddrfgcq4x8qn8fplsb64f1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no unit tests that don't need an FPGA exist.
       #:make-flags (list "CC=gcc" "CXX=g++"
                          (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'remove-usr-local
            (lambda _
              (substitute* "iceprog/Makefile"
                (("-I/usr/local/include") "")
                (("-L/usr/local/lib") ""))
              #t))
          (add-after 'remove-usr-local 'fix-usr-local
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "icebox/icebox_vlog.py"
                (("/usr/local/share") (string-append (assoc-ref outputs "out") "/share")))
              #t))
          (delete 'configure))))
    (inputs
     `(("libftdi" ,libftdi)))
    (native-inputs
     `(("python-3" ,python)
       ("pkg-config" ,pkg-config)))
    (home-page "http://www.clifford.at/icestorm/")
    (synopsis "Project IceStorm - Lattice iCE40 FPGAs bitstream tools")
    (description "Project IceStorm - Lattice iCE40 FPGAs Bitstream Tools.
Includes the actual FTDI connector.")
    (license license:isc))))

(define-public nextpnr-ice40
  (let [(commit "fbe486df459909065d6852a7495a212dfd2accef")
        (revision "1")]
    (package
      (name "nextpnr-ice40")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "git://github.com/YosysHQ/nextpnr")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1fmxsywgs45g88ra7ips5s2niiiwrkyxdcy742ws18dfk2y4vi9c"))))
      (inputs
       `(("boost" ,boost)
         ("eigen" ,eigen)
         ("icestorm" ,icestorm)
         ("python" ,python)
         ("qtbase" ,qtbase)
         ("yosys" ,yosys)))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags `("-DARCH=ice40"
                             ,(string-append "-DICEBOX_ROOT="
                                             (assoc-ref %build-inputs "icestorm")
                                             "/share/icebox"))
         #:tests? #f))
      (synopsis "Place-and-Route tool for FPGAs")
      (description "Nextpnr aims to be a vendor neutral, timing driven,
FOSS FPGA place and route tool.")
      (home-page "https://github.com/YosysHQ/nextpnr")
      (license license:expat))))

(define-public arachne-pnr
  (let ((commit "840bdfdeb38809f9f6af4d89dd7b22959b176fdd")
        (revision "2"))
   (package
    (name "arachne-pnr")
    (version (string-append "0.0-" revision "-" (string-take commit 9)))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/YosysHQ/arachne-pnr")
                     (commit commit)))
              (file-name (git-file-name name version))
              (sha256
                (base32
                   "1dqvjvgvsridybishv4pnigw9gypxh7r7nrqp9z9qq92v7c5rxzl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags
       (list (string-append "DESTDIR=" (assoc-ref %outputs "out"))
             (string-append "ICEBOX=" (string-append
                                       (assoc-ref %build-inputs "icestorm")
                                       "/share/icebox")))
       #:phases (modify-phases %standard-phases
       (replace 'configure
         (lambda* (#:key outputs inputs #:allow-other-keys)
           (substitute* '("./tests/fsm/generate.py"
                          "./tests/combinatorial/generate.py")
             (("#!/usr/bin/python") "#!/usr/bin/python2"))
           #t)))))
    (inputs
     `(("icestorm" ,icestorm)))
    (native-inputs
     `(("git" ,git)  ; for determining its own version string
       ("yosys" ,yosys) ; for tests
       ("perl" ,perl) ; for shasum
       ("python-2" ,python-2))) ; for tests
    (home-page "https://github.com/YosysHQ/arachne-pnr")
    (synopsis "Place-and-Route tool for FPGAs")
    (description "Arachne-PNR is a Place-and-Route Tool For FPGAs.")
    (license license:gpl2))))

(define-public gtkwave
  (package
    (name "gtkwave")
    (version "3.3.108")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "mirror://sourceforge/gtkwave/"
                                 "gtkwave-" version "/"
                                 "gtkwave-" version ".tar.gz")
                  (string-append "http://gtkwave.sourceforge.net/"
                                 "gtkwave-" version ".tar.gz")))
       (sha256
        (base32 "0fzbap72zm4ka6n85j0873fpaarrx199ay0kjw1avrs20hs4gr7c"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gperf" ,gperf)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("tcl" ,tcl)
       ("tk" ,tk)
       ("gtk+-2" ,gtk+-2)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-tcl="
                            (assoc-ref %build-inputs "tcl")
                            "/lib")
             (string-append "--with-tk="
                            (assoc-ref %build-inputs "tk")
                            "/lib"))))

    (synopsis "Waveform viewer for FPGA simulator trace files")
    (description "This package is a waveform viewer for FPGA
simulator trace files (@dfn{FST}).")
    (home-page "http://gtkwave.sourceforge.net/")
    ;; Exception against free government use in tcl_np.c and tcl_np.h.
    (license (list license:gpl2+ license:expat license:tcl/tk))))

(define-public python-migen
  (package
    (name "python-migen")
    (version "0.9.2")
    (source
     (origin
       ;; Tests fail in the PyPI tarball due to missing files.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/m-labs/migen")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kq11if64zj84gv4w1q7l16fp17xjxl2wv5hc9dibr1z3m1gy67l"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-colorama" ,python-colorama)))
    (home-page "https://m-labs.hk/gateware/migen/")
    (synopsis "Python toolbox for building complex digital hardware")
    (description
     "Migen FHDL is a Python library that replaces the event-driven
paradigm of Verilog and VHDL with the notions of combinatorial and
synchronous statements, has arithmetic rules that make integers always
behave like mathematical integers, and allows the design's logic to be
constructed by a Python program.")
    (license license:bsd-2)))

(define-public python-myhdl
  (package
    (name "python-myhdl")
    (version "0.11")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "myhdl" version))
        (sha256
          (base32
            "04fi59cyn5dsci0ai7djg74ybkqfcjzhj1jfmac2xanbcrw9j3yk"))))
    (build-system python-build-system)
    (home-page "http://www.myhdl.org/")
    (synopsis "Python as a Hardware Description Language")
    (description "This package provides a library to turn Python into
a hardware description and verification language. ")
    (license license:lgpl2.1+)))

(define-public nvc
  (package
    (name "nvc")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/nickg/nvc.git")
                     (commit (string-append "r" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0dd1xany6qhh2qsfw8ba0ky7y86h19yr4hlk0r5i2bvwsg4355v9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f ; https://github.com/nickg/nvc/issues/409
       #:configure-flags
       '("--enable-vhpi")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'clean-up
           (lambda _
             (delete-file "autogen.sh")
             #t)))))
    (native-inputs
     `(("automake" ,automake)
       ("autoconf" ,autoconf)
       ("flex" ,flex)
       ("gettext" ,gnu-gettext)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("which" ,which)
       ("check" ,check))) ; for the tests
    (inputs
     `(("llvm" ,llvm-9)))
    (synopsis "VHDL compiler and simulator")
    (description "This package provides a VHDL compiler and simulator.")
    (home-page "https://github.com/nickg/nvc")
    (license license:gpl3+)))

(define-public systemc
  (package
    (name "systemc")
    (version "2.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://accellera.org/images/downloads/standards/"
             "systemc/systemc-" version ".tar.gz"))
       (sha256
        (base32 "0gvv3xmhiwx1izmzy06yslzqzh6ygrgmw53xqfmyvbz5a6ivk0ap"))))
    (native-inputs `(("perl" ,perl)))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--enable-debug")))
    (home-page "https://accellera.org/community/systemc")
    (synopsis "Library for event-driven simulation")
    (description
     "SystemC is a C++ library for modeling concurrent systems, and the
reference implementation of IEEE 1666-2011.  It provides a notion of timing as
well as an event-driven simulations environment.  Due to its concurrent and
sequential nature, SystemC allows the description and integration of complex
hardware and software components.  To some extent, SystemC can be seen as
a Hardware Description Language.  However, unlike VHDL or Verilog, SystemC
provides sophisticated mechanisms that offer high abstraction levels on
components interfaces.  This, in turn, facilitates the integration of systems
using different abstraction levels.")
    ;; homepages.cae.wisc.edu/~ece734/SystemC/Esperan_SystemC_tutorial.pdf
    (license license:asl2.0)))

(define-public verilator
  (package
    (name "verilator")
    (version "4.110")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/verilator/verilator")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lm2nyn7wzxj5y0ffwazhb4ygnmqf4d61sl937vmnmrpvdihsrrq"))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("bison" ,bison)
       ("flex" ,flex)
       ("gettext" ,gettext-minimal)
       ("python" ,python)))
    (inputs
     `(("perl" ,perl)
       ("systemc" ,systemc)))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "LDFLAGS=-L"
                            (assoc-ref %build-inputs "systemc")
                            "/lib-linux64"))
       #:make-flags
       (list (string-append "LDFLAGS=-L"
                            (assoc-ref %build-inputs "systemc")
                            "/lib-linux64"))
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _ (invoke "autoconf"))))
       #:test-target "test"))
    ;; #error "Something failed during ./configure as config_build.h is incomplete.
    ;; Perhaps you used autoreconf, don't." -- so we won't. ^^
    (home-page "https://www.veripool.org/projects/verilator/")
    (synopsis "Fast Verilog/SystemVerilog simulator")
    (description
     "Verilator is invoked with parameters similar to GCC or Synopsys’s VCS.
It ``Verilates'' the specified Verilog or SystemVerilog code by reading it,
performing lint checks, and optionally inserting assertion checks and
coverage-analysis points.  It outputs single- or multi-threaded @file{.cpp}
and @file{.h} files, the ``Verilated'' code.

The user writes a little C++/SystemC wrapper file, which instantiates the
Verilated model of the user’s top level module.  These C++/SystemC files are
then compiled by a C++ compiler (GCC/Clang/etc.).  The resulting executable
performs the design simulation.  Verilator also supports linking its generated
libraries, optionally encrypted, into other simulators.")
    (license license:lgpl3)))
