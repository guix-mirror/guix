;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Dave Love <fx@gnu.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages storage)
  #:use-module (ice-9 match))

(define-public fio
  (package
    (name "fio")
    (version "3.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://brick.kernel.dk/snaps/"
                                  "fio-" version ".tar.bz2"))
              (sha256
               (base32
                "0s8m0wcz5j6sa1hblj80wk3syy5b4shg7y3gabvm9xa3wj0lzasa"))))
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
(define (imb mpi)
  (package
    (name (string-append "imb-" (package-name mpi)))
    (version "2019.1")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/intel/mpi-benchmarks.git")
            (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256 (base32 "18hfdyvl5i172gadiq9si1qxif5rvic0lifxpbrr7s59ylg8f9c4"))))
    (build-system gnu-build-system)
    (inputs
     `(("mpi" ,mpi)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((mpi-home (assoc-ref inputs "mpi")))
               ;; Override default parallelism
               (substitute* "Makefile"
                 (("make -j[[:digit:]]+")
                  (format #f "make -j~d" (parallel-job-count))))
               (invoke "make" "SHELL=sh" "CC=mpicc" "CXX=mpic++"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (for-each
                (lambda (file)
                  (install-file file bin))
                '("IMB-IO" "IMB-EXT" "IMB-MPI1" "IMB-NBC" "IMB-RMA" "IMB-MT")))
             #t)))))
    (home-page "https://software.intel.com/en-us/articles/intel-mpi-benchmarks")
    (synopsis "Intel MPI Benchmarks")
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

(define-public imb-openmpi (imb openmpi))

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
