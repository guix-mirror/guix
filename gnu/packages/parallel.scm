;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2020 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Roel Janssen <roel@gnu.org>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
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
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix utils) #:select (target-64bit?))
  #:use-module (guix packages)
  #:use-module (guix utils)
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
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web))

(define-public parallel
  (package
    (name "parallel")
    (version "20210322")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/parallel/parallel-"
                          version ".tar.bz2"))
      (sha256
       (base32 "152np0jg4n94sbl2p2fzxjfnssiyp5sg7r5wx6s8p893b921pxwq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-bin-sh
           (lambda _
             (for-each
              (lambda (file)
                (substitute* file
                  ;; Patch hard coded '/bin/sh' in the line ending in:
                  ;; $Global::shell = $ENV{'PARALLEL_SHELL'} ||
                  ;;  parent_shell($$) || $ENV{'SHELL'} || "/bin/sh";
                  (("/bin/sh\\\";\n$") (string-append (which "sh") "\";\n"))))
              (list "src/parallel" "src/sem"))
             #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/parallel")
                 `("PATH" ":" prefix
                   ,(map (lambda (input)
                           (string-append (assoc-ref inputs input) "/bin"))
                         '("perl"
                           "procps"))))
               #t)))
         (add-after 'wrap-program 'post-install-test
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke (string-append
                      (assoc-ref outputs "out") "/bin/parallel")
                     "echo"
                     ":::" "1" "2" "3"))))))
    (inputs
     `(("perl" ,perl)
       ("procps" ,procps)))
    (home-page "https://www.gnu.org/software/parallel/")
    (synopsis "Build and execute command lines in parallel")
    (description
     "GNU Parallel is a tool for executing shell jobs in parallel using one
or more computers.  Jobs can consist of single commands or of scripts
and they are executed on lists of files, hosts, users or other items.")
    (license license:gpl3+)))

(define-public xe
  (package
    (name "xe")
    (version "0.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/leahneukirchen/xe")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04jr8f6jcijr0bsmn8ajm0aj35qh9my3xjsaq64h8lwg5bpyn29x"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (synopsis "Execute a command for every argument")
    (description
     "The xe utility constructs command lines from specified arguments,
combining some of the best features of xargs(1) and apply(1).  Parallel
execution is also possible.")
    (home-page "https://github.com/leahneukirchen/xe")
    (license license:public-domain)))

(define-public slurm
  (package
   (name "slurm")
   (version "20.11.3")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://download.schedmd.com/slurm/slurm-"
                  version ".tar.bz2"))
            (sha256
             (base32
              "1s70x6yh60sx63dgmp5rlhq8jcz7kxv9pk8gbs9v1jg8zps5h5bk"))
            (modules '((guix build utils)))
            (snippet
             '(begin
                ;; According to
                ;; <https://lists.gnu.org/archive/html/guix-devel/2016-02/msg00534.html>
                ;; there are non-free bits under contribs/, though it's not
                ;; clear which ones.  libpmi is clearly free (it used to be
                ;; under src/api/), so remove all of contribs/ except
                ;; contribs/pmi/.
                (substitute* "configure.ac"
                  (("^[[:space:]]+contribs/(.*)$" all directory)
                   (if (and (string-prefix? "pmi" directory)
                            (not (string-prefix? "pmi2" directory)))
                       all
                       "")))

                (rename-file "contribs/pmi" "tmp-pmi")
                (delete-file-recursively "contribs")
                (mkdir "contribs")
                (rename-file "tmp-pmi" "contribs/pmi")
                #t))))
   ;; FIXME: More optional inputs could be added,
   ;; in particular mysql and gtk+.
   (inputs `(("freeipmi" ,freeipmi)
             ("hwloc" ,hwloc-2 "lib")
             ("json-c" ,json-c)
             ("linux-pam" , linux-pam)
             ("munge" ,munge)
             ("numactl" ,numactl)
             ("readline" ,readline)))
   (native-inputs
    `(("autoconf" ,autoconf)
      ("expect" ,expect)
      ("perl" ,perl)
      ("pkg-config" ,pkg-config)
      ("python" ,python-wrapper)))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags
      (list "--enable-pam" "--sysconfdir=/etc/slurm"
            "--disable-static"
            (string-append "--with-freeipmi=" (assoc-ref %build-inputs "freeipmi"))
            (string-append "--with-hwloc=" (assoc-ref %build-inputs "hwloc"))
            (string-append "--with-json=" (assoc-ref %build-inputs "json-c"))
            (string-append "--with-munge=" (assoc-ref %build-inputs "munge"))

            ;; 32-bit support is marked as deprecated and needs to be
            ;; explicitly enabled.
            ,@(if (target-64bit?) '() '("--enable-deprecated")))
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'autoconf
          (lambda _ (invoke "autoconf")))         ;configure.ac was patched
        (add-after 'install 'install-libpmi
          (lambda _
            ;; Open MPI expects libpmi to be provided by Slurm so install it.
            (invoke "make" "install" "-C" "contribs/pmi"))))))
   (home-page "https://slurm.schedmd.com/")
   (synopsis "Workload manager for cluster computing")
   (description
    "SLURM is a fault-tolerant and highly scalable cluster management and job
scheduling system for large and small clusters.  It allocates access to
resources (computer nodes) to users for some duration of time, provides a
framework for starting, executing, and monitoring work (typically a parallel
job) on a set of allocated nodes, and arbitrates contention for resources
by managing a queue of pending work.")
   (license (list license:bsd-2       ; src/common/log.[ch], src/common/uthash
                  license:expat       ; slurm/pmi.h
                  license:isc         ; src/common/strlcpy.c
                  license:lgpl2.1+    ; hilbert.[ch], src/common/slurm_time.h
                  license:zlib        ; src/common/strnatcmp.c
                  license:gpl2+))))   ; the rest, often with OpenSSL exception

;; The SLURM client/daemon protocol and file format changes from time to time
;; in incompatible ways, as noted in
;; <https://slurm.schedmd.com/troubleshoot.html#network>.  Thus, keep older
;; releases here.  See also <https://issues.guix.gnu.org/44387>.
;; As noted in the link, YY.MM is the release scheme, and the 'maintenance'
;; digit does not introduce incompatibilities.

(define-public slurm-20.02
  (package
    (inherit slurm)
    (version "20.02.6-1")
    (source (origin
              (inherit (package-source slurm))
              (method url-fetch)
              (uri (string-append
                    "https://download.schedmd.com/slurm/slurm-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0qj4blfymrd2ry2qmb58l3jbr4jwygc3adcfw7my27rippcijlyc"))))))

(define-public slurm-19.05
  (package
    (inherit slurm)
    (version "19.05.8")
    (source (origin
              (inherit (package-source slurm))
              (method url-fetch)
              (uri (string-append
                    "https://download.schedmd.com/slurm/slurm-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "10c9j4a9a6d4ibpf75006mn03p8xgpaprc247x2idakysjf2fw43"))))))

;; Same as Debian 10
(define-public slurm-18.08
  (package
    (inherit slurm)
    (version "18.08.9")
    (source
      (origin
        (inherit (package-source slurm))
        (uri (string-append
               "https://download.schedmd.com/slurm/slurm-"
               version ".tar.bz2"))
        (sha256
         (base32
          "1bgrpz75m7l4xhirsd0fvnkzlkrl8v2qpmjcz60barc5qm2kn457"))))))

(define-public slurm-drmaa
  (package
    (name "slurm-drmaa")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/natefoo/slurm-drmaa/releases/download/"
                    version "/slurm-drmaa-" version ".tar.gz"))
              (sha256
               (base32
                "0dn8ypqxdaq3k4jqwwx7msckxnmr6n2z5j68yffp50yy07ajbzjv"))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f)) ; The tests require "bats".
    (inputs
     `(("slurm" ,slurm)))
    (native-inputs
     `(("which" ,which)))
    (home-page "https://github.com/natefoo/slurm-drmaa")
    (synopsis "Distributed resource management application API for SLURM")
    (description
     "PSNC DRMAA for Simple Linux Utility for Resource Management (SLURM) is
an implementation of Open Grid Forum DRMAA 1.0 (Distributed Resource
Management Application API) specification for submission and control of jobs
to SLURM.  Using DRMAA, grid applications builders, portal developers and ISVs
can use the same high-level API to link their software with different
cluster/resource management systems.")
    (license license:gpl3+)))

(define-public python-slurm-magic
  (let ((commit "73dd1a2b85799f7dae4b3f1cd9027536eff0c4d7")
        (revision "0"))
    (package
      (name "python-slurm-magic")
      (version (git-version "0.0" revision commit))
      (home-page "https://github.com/NERSC/slurm-magic")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page)
                                    (commit commit)))
                (sha256
                 (base32
                  "19pp2vs0wm8mx0arz9n6lw9wgyv70w9wyi4y6b91qc5j3bz5igfs"))
                (file-name (git-file-name name version))))
      (build-system python-build-system)
      (arguments
       '(#:phases (modify-phases %standard-phases
                    (add-before 'build 'set-slurm-path
                      (lambda* (#:key inputs #:allow-other-keys)
                        ;; The '_execute' method tries to exec 'salloc'
                        ;; etc. from $PATH.  Record the absolute file name
                        ;; instead.
                        (let ((slurm (assoc-ref inputs "slurm")))
                          (substitute* "slurm_magic.py"
                            (("name = (.*)$" _ value)
                             (string-append "name = \""
                                            slurm "/bin/\" + "
                                            value "\n")))
                          #t))))))
      (inputs
       `(("slurm" ,slurm)))
      (propagated-inputs
       `(("python-ipython" ,python-ipython)
         ("python-pandas" ,python-pandas)))
      (synopsis "Control the SLURM batch scheduler from Jupyter Notebook")
      (description
       "This package implements Jupyter/IPython
@uref{http://ipython.readthedocs.io/en/stable/interactive/magics.html, magic
commands} for interacting with the SLURM workload manager.  SLURM magic simply
wraps command-line executables and the commands themselves should look like
their command-line counterparts.  Commands are spawned via @code{subprocess}
and output captured in the notebook.  Whatever arguments are accepted by a
SLURM command line executable are also accepted by the corresponding magic
command---e.g., @code{%salloc}, @code{%sbatch}, etc.")
      (license license:bsd-3))))
