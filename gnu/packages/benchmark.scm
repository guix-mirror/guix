;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Dave Love <fx@gnu.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2019 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Greg Hogan <code@greghogan.com>
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
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages storage)
  #:use-module (ice-9 match))

(define-public fio
  (package
    (name "fio")
    (version "3.26")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://brick.kernel.dk/snaps/"
                                  "fio-" version ".tar.bz2"))
              (sha256
               (base32
                "1114h60vw63bim872an33xpvjfib6sc9dwj9xvk0yw41xjzfpp06"))))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'patch-paths
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (gnuplot (string-append (assoc-ref inputs "gnuplot")
                                          "/bin/gnuplot")))
              (substitute* "tools/plot/fio2gnuplot"
                (("/usr/share/fio") (string-append out "/share/fio"))
                ;; FIXME (upstream): The 'gnuplot' executable is used inline
                ;; in various os.system() calls mixed with *.gnuplot filenames.
                (("; do gnuplot") (string-append "; do " gnuplot))
                (("gnuplot mymath") (string-append gnuplot " mymath"))
                (("gnuplot mygraph") (string-append gnuplot " mygraph")))
              #t)))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The configure script doesn't understand some of the
             ;; GNU options, so we can't use #:configure-flags.
             (let ((out (assoc-ref outputs "out")))
               (invoke "./configure"
                       (string-append "--prefix=" out))
               #t)))
         ;; The main `fio` executable is fairly small and self contained.
         ;; Moving the auxiliary python and gnuplot scripts to a separate
         ;; output saves almost 400 MiB on the closure.
         (add-after 'install 'move-outputs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((oldbin (string-append (assoc-ref outputs "out") "/bin"))
                   (newbin (string-append (assoc-ref outputs "utils") "/bin")))
               (mkdir-p newbin)
               (for-each (lambda (file)
                           (let ((src (string-append oldbin "/" file))
                                 (dst (string-append newbin "/" file)))
                             (link src dst)
                             (delete-file src)))
                         '("fio2gnuplot"  "fiologparser_hist.py"
                           "fiologparser.py"))
               ;; Make sure numpy et.al is found.
               (wrap-program (string-append newbin "/fiologparser_hist.py")
                 `("PYTHONPATH" ":" prefix (,(getenv "PYTHONPATH"))))
               #t))))))
    (outputs '("out" "utils"))
    (inputs
     `(("ceph" ,ceph "lib")
       ("libaio" ,libaio)
       ("gnuplot" ,gnuplot)
       ("zlib" ,zlib)
       ("python-numpy" ,python2-numpy)
       ("python-pandas" ,python2-pandas)
       ("python" ,python-2)))
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

;; Parameterized in anticipation of m(va)pich support
(define (intel-mpi-benchmarks mpi)
  (package
    (name (string-append "intel-mpi-benchmarks"
                         (if (string=? (package-name mpi) "openmpi")
                             ""
                             (string-append "-" (package-name mpi)))))
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
     `(("mpi" ,mpi)))
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

(define-public intel-mpi-benchmarks/openmpi
  (intel-mpi-benchmarks openmpi))

(define-public imb-openmpi
  (deprecated-package "imb-openmpi" intel-mpi-benchmarks/openmpi))

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
    (version "1.5.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/benchmark")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13rxagpzw6bal6ajlmrxlh9kgfvcixn6j734b2bvfqz7lch8n0pa"))))
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
                               "googletest")
             #t)))))
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
     `(("perl" ,perl)))
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
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "locust" version))
       (sha256
        (base32
         "0vmw151xcaznd2j85n96iyv9fniss0bkk91xn4maw2gwzym424xk"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'extend-PATH
           ;; Add the 'locust' script to PATH, which is used in the test
           ;; suite.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "PATH" (string-append out "/bin:"
                                             (getenv "PATH"))))))
         (replace 'check
           (lambda _
             (invoke "python" "-m" "pytest"
                     "-k" (string-join
                           (list
                            ;; These tests return "non-zero exit status 1".
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
                            "not test_webserver") " and ")))))))
    (propagated-inputs
     `(("python-configargparse" ,python-configargparse)
       ("python-flask" ,python-flask)
       ("python-flask-basicauth" ,python-flask-basicauth)
       ("python-gevent" ,python-gevent)
       ("python-geventhttpclient" ,python-geventhttpclient)
       ("python-msgpack" ,python-msgpack)
       ("python-psutil" ,python-psutil)
       ("python-pyzmq" ,python-pyzmq)
       ("python-requests" ,python-requests)
       ("python-werkzeug" ,python-werkzeug)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pyquery" ,python-pyquery)
       ("python-pytest" ,python-pytest))) ;for more easily skipping tests
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
