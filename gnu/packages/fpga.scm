;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2016, 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Amin Bandali <bandali@gnu.org>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages python)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages libftdi))

(define-public abc
 (let ((commit "5ae4b975c49c")
       (revision "1"))
  (package
    (name "abc")
    (version (string-append "0.0-" revision "-" (string-take commit 9)))
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
    (home-page "http://people.eecs.berkeley.edu/~alanmi/abc/")
    (synopsis "Sequential logic synthesis and formal verification")
    (description "ABC is a program for sequential logic synthesis and
formal verification.")
    (license
      (license:non-copyleft "https://fedoraproject.org/wiki/Licensing:MIT#Modern_Variants")))))

(define-public iverilog
  (package
    (name "iverilog")
    (version "10.2")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "ftp://ftp.icarus.com/pub/eda/verilog/v10/"
                              "verilog-" version ".tar.gz"))
              (sha256
                (base32
                   "0075x5nsxwkrgn7b3635il9kw7mslckaji518pdmwdrdn7fxppln"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("flex" ,flex)
       ("bison" ,bison)
       ("ghostscript" ,ghostscript))) ; ps2pdf
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
    (version "0.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cliffordwolf/yosys.git")
                    (commit (string-append "yosys-" version))
                    (recursive? #t))) ; for the ‘iverilog’ submodule
              (sha256
                (base32
                   "1qwbp8gynlklawzvpa4gdn2x0hs8zln0s3kxjqkhfcjfxffdcpvv"))
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
  (let ((commit "c0cbae88ab47a3879aacf80d53b6a85710682a6b")
        (revision "2"))
   (package
    (name "icestorm")
    (version (string-append "0.0-" revision "-" (string-take commit 9)))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/cliffordwolf/icestorm.git")
                     (commit commit)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
                (base32
                   "0bqm0rpywm64yvbq75klpyzb1g9sdsp1kvdlyqg4hvm8jw9w8lya"))))
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

(define-public arachne-pnr
  (let ((commit "840bdfdeb38809f9f6af4d89dd7b22959b176fdd")
        (revision "2"))
   (package
    (name "arachne-pnr")
    (version (string-append "0.0-" revision "-" (string-take commit 9)))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/YosysHQ/arachne-pnr.git")
                     (commit commit)))
              (file-name (string-append name "-" version "-checkout"))
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
    (version "3.3.100")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://gtkwave.sourceforge.net/"
                                  "gtkwave-" version ".tar.gz"))
              (sha256
               (base32
                "1z60i5nh8dz8j9ii63fwaw7k0p3x0scp91478cxmdv4xhp4njlxa"))))
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
