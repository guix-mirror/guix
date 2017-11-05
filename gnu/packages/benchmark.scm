;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Dave Love <fx@gnu.org>
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
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages python)
  #:use-module (gnu packages storage)
  #:use-module (ice-9 match))

(define-public fio
  (package
    (name "fio")
    (version "3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                       "http://brick.kernel.dk/snaps/"
                       "fio-" version ".tar.bz2"))
              (sha256
               (base32
                "1f5vina9bxn99drda8dhbxng8ypj4ny72xh6mp8rq955d0f8sq0z"))))
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
               (zero? (system* "./configure"
                               (string-append "--prefix=" out))))))
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
    (version "2017.2")
    (source
     (origin
      (method url-fetch)
      (uri (match (string-split version #\.)
             ((major minor)
              (string-append
               "https://software.intel.com/sites/default/files/managed/76/6c/IMB_"
               major "_Update" minor ".tgz"))))
      (sha256 (base32 "11nczxm686rsppmw9gjc2p2sxc0jniv5kv18yxm1lzp5qfh5rqyb"))))
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
               (zero?
                ;; Not safe for parallel build
                (system* "make" "-C" "imb/src" "-f" "make_mpich" "SHELL=sh"
                         (string-append "MPI_HOME=" mpi-home))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name))
                    (bin (string-append out "/bin")))
               (with-directory-excursion "imb/src"
                 (for-each
                  (lambda (file)
                    (install-file file bin))
                  '("IMB-IO" "IMB-EXT" "IMB-MPI1" "IMB-NBC" "IMB-RMA")))
               (mkdir-p doc)
               (with-directory-excursion "imb"
                 (copy-recursively "license" doc)))
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
