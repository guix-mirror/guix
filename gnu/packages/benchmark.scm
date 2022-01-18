;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017 Dave Love <fx@gnu.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2019 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2019, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu packages benchmark)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (ice-9 match))

(define-public fio
  (package
    (name "fio")
    (version "3.29")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://brick.kernel.dk/snaps/"
                                  "fio-" version ".tar.bz2"))
              (sha256
               (base32
                "11k7ksksnb8lcbz0qdc9g7zlzaa0515j7kx4mlhk75sfs43v9zxc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules (,@%gnu-build-system-modules
                  (ice-9 textual-ports))
       #:test-target "test"
       #:configure-flags '("--disable-native") ;don't generate code for the build CPU
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key (configure-flags ''()) outputs #:allow-other-keys)
             ;; The configure script doesn't understand some of the
             ;; GNU options, so we can't use the stock phase.
             (let ((out (assoc-ref outputs "out")))
               (apply invoke "./configure"
                      (string-append "--prefix=" out)
                      configure-flags))))
         ;; The main `fio` executable is fairly small and self contained.
         ;; Moving the auxiliary scripts to a separate output saves ~100 MiB
         ;; on the closure.
         (add-after 'install 'move-outputs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((oldbin (string-append (assoc-ref outputs "out") "/bin"))
                   (newbin (string-append (assoc-ref outputs "utils") "/bin"))
                   (script? (lambda* (file #:rest _)
                              (call-with-input-file file
                                (lambda (port)
                                  (char=? #\# (peek-char port)))))))
               (mkdir-p newbin)
               (for-each (lambda (file)
                           (link file (string-append newbin "/" (basename file)))
                           (delete-file file))
                         (find-files oldbin script?))))))))
    (outputs '("out" "utils"))
    (inputs
     (list libaio python zlib))
    (home-page "https://github.com/axboe/fio")
    (synopsis "Flexible I/O tester")
    (description
     "fio is a tool that will spawn a number of threads or processes doing a
particular type of I/O action as specified by the user.  The typical use of fio
is to write a job file matching the I/O load one wants to simulate.")
    ;; The software is distributed under the GPL2, but a handful of components
    ;; are covered by other licenses.
    (license (list license:gpl2 license:gpl2+ license:bsd-2
                   license:public-domain))))

(define-public intel-mpi-benchmarks/openmpi
  (package
    (name "intel-mpi-benchmarks")
    (version "2019.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/intel/mpi-benchmarks")
                    (commit (string-append "IMB-v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02hxbk9g9nl59bk5qcfl3djj7b58vsqys340m1xdbyqwcrbnahh9"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Some source configuration files in the original tarball
                  ;; have inappropriate execute permissions, which interferes
                  ;; with the install phase below.
                  (for-each (lambda (file) (chmod file #o444))
                            (find-files "WINDOWS" "."))
                  #t))))
    (build-system gnu-build-system)
    (inputs
     (list openmpi))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (define (benchmark? file stat)
               (and (string-prefix? "IMB-" (basename file))
                    (executable-file? file)))

             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (for-each (lambda (file)
                           (install-file file bin))
                         (find-files "." benchmark?))
               #t))))

       ;; The makefile doesn't express all the dependencies, it seems.
       #:parallel-build? #t

       #:make-flags '("CC=mpicc" "CXX=mpicxx")))
    (home-page "https://software.intel.com/en-us/articles/intel-mpi-benchmarks")
    (synopsis "Benchmarks for the Message Passing Interface (MPI)")
    (description
     "This package provides benchmarks for implementations of the @dfn{Message
Passing Interface} (MPI).  It contains MPI performance measurements for
point-to-point and global communication, and file, operations for a range of
message sizes.  The generated benchmark data fully characterize:

@itemize
@item
Performance of a cluster system, including node performance, network latency,
and throughput;
@item
Efficiency of the MPI implementation.
@end itemize")
    (license license:cpl1.0)))

(define-public multitime
  (package
    (name "multitime")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://tratt.net/laurie/src/"
                                  "multitime/releases/"
                                  "multitime-" version ".tar.gz"))
              (sha256
               (base32
                "0iyfsdrbyqa7a4ifrh19l9a48hgv7ld6m0d8yf9bkl12q0qw91fx"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f)) ; there are no tests
    (home-page "https://tratt.net/laurie/src/multitime/")
    (synopsis "Time command execution over multiple executions")
    (description
     "The @code{time} utility is a simple and often effective way of measuring
how long a command takes to run (wall time).  Unfortunately, running a command
once can give misleading timings.  @code{multitime} is, in essence, a simple
extension to @code{time} which runs a command multiple times and prints the
timing means, standard deviations, mins, medians, and maxes having done so.
This can give a much better understanding of the command's performance.")
    (license license:expat)))

(define-public benchmark
  (package
    (name "benchmark")
    (version "1.5.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/benchmark")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "030g4d8vpn2442dsap0qw86lsw7xfl36k0x0x9bn0vvm11qvjn8c"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("googletest-source" ,(package-source googletest))
       ("googletest" ,googletest)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-googletest
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "googletest-source")
                               "googletest"))))))
    (home-page "https://github.com/google/benchmark")
    (synopsis "Microbenchmark support library")
    (description
     "Benchmark is a library to benchmark code snippets, similar to unit
tests.")
    (license license:asl2.0)))

(define-public bonnie++
  (package
    (name "bonnie++")
    (version "1.98")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.coker.com.au/bonnie++/bonnie++-"
                                  version ".tgz"))
              (sha256
               (base32
                "010bmlmi0nrlp3aq7p624sfaj5a65lswnyyxk3cnz1bqig0cn2vf"))))
    (build-system gnu-build-system)
    (native-inputs
     (list perl))
    (arguments '(#:tests? #f)) ; there are no tests
    (home-page "https://doc.coker.com.au/projects/bonnie/")
    (synopsis "Hard drive and file system benchmark suite")
    (description
     "Bonnie++ is a benchmark suite that is aimed at performing a number of
simple tests of hard drive and file system performance.  Bonnie++ allows you to
benchmark how your file systems perform with respect to data read and write
speed, the number of seeks that can be performed per second, and the number of
file metadata operations that can be performed per second.")
    (license license:gpl2)))   ;GPL 2 only, see copyright.txt

(define-public python-locust
  (package
    (name "python-locust")
    (version "2.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "locust" version))
       (sha256
        (base32
         "1516z6z5pikybg7pma2cgxgj3wxaaky7z6d30mxf81wd4krbq16s"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "pytest" "locust"
                       "-k" (string-join
                             '(;; These tests return "non-zero exit status 1".
                               "not test_default_headless_spawn_options"
                               "not test_default_headless_spawn_options_with_shape"
                               "not test_headless_spawn_options_wo_run_time"
                               ;; These tests depend on networking.
                               "not test_html_report_option"
                               "not test_web_options"
                               ;; This test fails because of the warning "System open
                               ;; file limit '1024' is below minimum setting '10000'".
                               "not test_skip_logging"
                               ;; On some (slow?) machines, the following tests
                               ;; fail, with the processes returning exit code
                               ;; -15 instead of the expected 42 and 0,
                               ;; respectively (see:
                               ;; https://github.com/locustio/locust/issues/1708).
                               "not test_custom_exit_code"
                               "not test_webserver") " and "))))))))
    (propagated-inputs
     (list python-configargparse
           python-flask
           python-flask-basicauth
           python-flask-cors
           python-gevent
           python-geventhttpclient
           python-msgpack
           python-psutil
           python-pyzmq
           python-requests
           python-roundrobin
           python-typing-extensions
           python-werkzeug))
    (native-inputs
     (list python-mock python-pyquery python-pytest
           python-retry python-setuptools-scm))
    (home-page "https://locust.io/")
    (synopsis "Distributed load testing framework")
    (description "Locust is a performance testing tool that aims to be easy to
use, scriptable and scalable.  The test scenarios are described in plain
Python.  It provides a web-based user interface to visualize the results in
real-time, but can also be run non-interactively.  Locust is primarily geared
toward testing HTTP-based applications or services, but it can be customized to
test any system or protocol.

Note: Locust will complain if the available open file descriptors limit for
the user is too low.  To raise such limit on a Guix System, refer to
@samp{info guix --index-search=pam-limits-service}.")
    (license license:expat)))

(define-public interbench
  (package
    (name "interbench")
    (version "0.31")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ckolivas/interbench")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ifnw8vnkcgrksx7g5d9ii4kjppqnk32lvrybdybmibyvag6zfdc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-broken-makefile
           (lambda _
             ;; Remove erroneous "-lm" target
             (substitute* "Makefile"
               (("hackbench.o -lm") "hackbench.o"))))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "interbench" (string-append out "/bin"))
               (install-file "interbench.8" (string-append out "/share/man/man8"))))))))
    (home-page "http://users.on.net/~ckolivas/interbench/")
    (synopsis "Interactivity benchmark")
    (description "interbench is designed to benchmark interactivity on Linux.
It is designed to measure the effect of changes in Linux kernel design or
system configuration changes such as CPU, I/O scheduler and filesystem changes
and options.  With careful benchmarking, different hardware can be compared.")
    (license license:gpl2+)))

(define-public clpeak
  ;; Release 1.1.0 is too old for our opencl-clhpp. This commit supports
  ;; cl2.hpp.
  (let ((commit "6d59cb64997a53c35207b77a63d2e9f0e84de5fd"))
    (package
      (name "clpeak")
      (version (git-version "1.1.0" "0" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/krrishnarraj/clpeak.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                  (base32
                    "0qmhdjyhwl7gfgyqxsddqn6zpp3b57503m16h7jv6illy3lfvji1"))))
      (build-system cmake-build-system)
      (home-page "https://github.com/krrishnarraj/clpeak")
      (inputs
        (list opencl-clhpp opencl-icd-loader))
      (synopsis "OpenCL benchmark tool")
      (description
        "A synthetic benchmarking tool to measure peak capabilities of OpenCL
        devices.  It only measures the peak metrics that can be achieved using
        vector operations and does not represent a real-world use case.")
        (license license:unlicense))))
