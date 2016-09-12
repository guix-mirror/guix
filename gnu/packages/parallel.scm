;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
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

(define-module (gnu packages parallel)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages freeipmi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web))

(define-public parallel
  (package
    (name "parallel")
    (version "20160822")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/parallel/parallel-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "1qdb7889w7v5amd0z4qg3v4hia0wj5vjly9qvm5lm5nlxg8bfrwq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-bin-sh
           (lambda _
             (for-each
              (lambda (file)
                (substitute* file
                  ;; Patch hard coded '/bin/sh' in the lin ending in:
                  ;; $Global::shell = $ENV{'PARALLEL_SHELL'} ||
                  ;;  parent_shell($$) || $ENV{'SHELL'} || "/bin/sh";
                  (("/bin/sh\\\";\n$") (string-append (which "sh") "\";\n"))
                  ;; Patch call to 'ps' and 'perl' commands.
                  ((" ps ") (string-append " " (which "ps") " "))
                  ((" perl -") (string-append " " (which "perl") " -"))))
              (list "src/parallel" "src/sem"))))
         (add-after 'install 'post-install-test
           (lambda* (#:key outputs #:allow-other-keys)
             (zero? (system* (string-append
                              (assoc-ref outputs "out") "/bin/parallel")
                             "echo"
                             ":::" "1" "2" "3")))))))
    (inputs
     `(("perl" ,perl)
       ("procps" ,procps)))
    (home-page "http://www.gnu.org/software/parallel/")
    (synopsis "Build and execute command lines in parallel")
    (description
     "GNU Parallel is a tool for executing shell jobs in parallel using one
or more computers.  Jobs can consist of single commands or of scripts
and they are executed on lists of files, hosts, users or other items.")
    (license license:gpl3+)))

(define-public slurm
  (package
   (name "slurm")
   (version "15.08.7.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/SchedMD/slurm/archive/slurm-"
                  (string-join (string-split version #\.) "-") ".tar.gz"))
            (file-name (string-append name "-" version ".tar.gz"))
            (sha256
             (base32
              "1rmi35l4img00dr4vic8cv8s7b6n1yx1mkq2s7kjf5hvqdh6s2ki"))
            (patches (search-patches
                      "slurm-configure-remove-nonfree-contribs.patch"))
            (modules '((guix build utils)))
            (snippet
             '(begin
                (delete-file-recursively "contribs")
                #t))))
   ;; FIXME: More optional inputs could be added,
   ;; in particular mysql and gtk+.
   (inputs `(("expect" ,expect)
             ("freeipmi" ,freeipmi)
             ("hwloc" ,hwloc)
             ("json-c" ,json-c)
             ("linux-pam" , linux-pam)
             ("munge" ,munge)
             ("numactl" ,numactl)
             ("openssl" ,openssl)
             ("perl" ,perl)
             ("python" ,python-wrapper)
             ("readline" ,readline)))
   (native-inputs
    `(("autoconf" ,autoconf)
      ("pkg-config" ,pkg-config)))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags
      (list "--enable-pam"
            (string-append "--with-freeipmi=" (assoc-ref %build-inputs "freeipmi"))
            (string-append "--with-hwloc=" (assoc-ref %build-inputs "hwloc"))
            (string-append "--with-json=" (assoc-ref %build-inputs "json-c"))
            (string-append "--with-munge=" (assoc-ref %build-inputs "munge"))
            (string-append "--with-ssl=" (assoc-ref %build-inputs "openssl")))
      #:phases
      (modify-phases %standard-phases
       (add-before
        'configure 'autogen
        (lambda _ (zero? (system* "autoconf"))))))) ; configure.ac was patched
   (home-page "http://slurm.schedmd.com/")
   (synopsis "Workload manager for cluster computing")
   (description
    "SLURM is a fault-tolerant and highly scalable cluster management and job
scheduling system for large and small clusters.  It allocates access to
resources (computer nodes) to users for some duration of time, provides a
framework for starting, executing, and monitoring work (typically a parallel
job) on a set of allocated nodes, and arbitrates contention for resources
by managing a queue of pending work.")
   (license license:gpl2+)))

(define-public slurm-drmaa
  (package
    (name "slurm-drmaa")
    (version "1.0.7")
    (source (origin
              (method url-fetch)
              (uri "http://apps.man.poznan.pl/trac/slurm-drmaa/downloads/9")
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0grw55hmny2mc4nc0y1arnvxd2k0dcdfn476kzs180fibjxgfw14"))))
    (build-system gnu-build-system)
    (inputs
     `(("slurm" ,slurm)))
    (native-inputs
     `(("which" ,which)))
    (home-page "http://apps.man.poznan.pl/trac/slurm-drmaa")
    (synopsis "Distributed resource management application API for SLURM")
    (description
     "PSNC DRMAA for Simple Linux Utility for Resource Management (SLURM) is
an implementation of Open Grid Forum DRMAA 1.0 (Distributed Resource
Management Application API) specification for submission and control of jobs
to SLURM.  Using DRMAA, grid applications builders, portal developers and ISVs
can use the same high-level API to link their software with different
cluster/resource management systems.")
    (license license:gpl3+)))
