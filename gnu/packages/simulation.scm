;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Paul Garlick <pgarlick@tourbillion-technology.com>
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

(define-module (gnu packages simulation)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1))

(define-public openfoam
  (package
    (name "openfoam")
    (version "4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://dl.openfoam.org/source/"
             (string-map (lambda (x) (if (eq? x #\.) #\- x)) version)))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cgxh4h2hf50qbvvdg5miwc2nympb0nrv3md96vb3gbs9vk8vq9d"))
       (patches (search-patches "openfoam-4.1-cleanup.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("boost" ,boost)
       ("cgal" ,cgal)
       ("flex" ,flex)
       ("git" ,git)
       ("gmp" ,gmp)
       ("libxt" ,libxt)
       ("metis" ,metis)
       ("mpfr" ,mpfr)
       ("ncurses" ,ncurses)
       ("readline" ,readline)
       ("scotch" ,pt-scotch32)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bison" ,bison)))
    (propagated-inputs
     `(("gzip" ,gzip)
       ("gnuplot" ,gnuplot)
       ("openmpi" ,openmpi)))
    (outputs '("debug"                  ;~60MB
               "out"))
    (arguments
     `( ;; Executable files and shared libraries are located in the 'platforms'
       ;; subdirectory.
       #:strip-directories (list (string-append
                                  "lib/OpenFOAM-" ,version
                                  "/platforms/linux64GccDPInt32Opt/bin")
                                 (string-append
                                  "lib/OpenFOAM-" ,version
                                  "/platforms/linux64GccDPInt32Opt/lib"))
       #:tests? #f                                ; no tests to run

       #:modules ((ice-9 ftw)
                  (ice-9 regex)
                  (guix build gnu-build-system)
                  (guix build utils))

       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'rename-build-directory
                    (lambda _
                      (chdir "..")
                      ;; Use 'OpenFOAM-version' convention to match the file
                      ;; name expectations in the build phase.
                      (let ((unpack-dir (string-append
                                         (getcwd) "/"
                                         (list-ref (scandir (getcwd) (lambda (name)
                                                                       (string-match "^OpenFOAM" name))) 0)))
                            (build-dir (string-append
                                        (getcwd) "/OpenFOAM-" ,version)))
                        (rename-file unpack-dir build-dir) ; rename build directory
                        (chdir (basename build-dir))) ; move to build directory
                      #t))
                  (delete 'configure)             ; no configure phase
                  (replace 'build
                    (lambda _
                      (let ((libraries '("boost" "cgal" "gmp" "metis" "mpfr" "scotch")))
                        ;; set variables to define store paths
                        (for-each (lambda (library)
                                    (setenv (string-append
                                             (string-upcase library) "_ROOT")
                                            (assoc-ref %build-inputs library))) libraries))
                      ;; set variables to define package versions
                      (setenv "SCOTCHVERSION" ,(package-version scotch))
                      (setenv "METISVERSION" ,(package-version metis))
                      ;; set variable to pass extra 'rpath' arguments to linker
                      (setenv "LDFLAGS"
                              (string-append
                               "-Wl,"
                               "-rpath=" %output "/lib/OpenFOAM-" ,version
                               "/platforms/linux64GccDPInt32Opt/lib,"
                               "-rpath=" %output "/lib/OpenFOAM-" ,version
                               "/platforms/linux64GccDPInt32Opt/lib/dummy"))
                      ;; compile OpenFOAM libraries and applications
                      (zero? (system (format #f
                                             "source ./etc/bashrc && ./Allwmake -j~a"
                                             (parallel-job-count))))))
                  (add-after 'build 'update-configuration-files
                    (lambda _
                      ;; record store paths and package versions in
                      ;; configuration files
                      (substitute* "etc/config.sh/CGAL"
                        (("$BOOST_ROOT") (getenv "BOOST_ROOT")))
                      (substitute* "etc/config.sh/CGAL"
                        (("$CGAL_ROOT") (getenv "CGAL_ROOT")))
                      (substitute* "etc/config.sh/metis"
                        (("$METIS_ROOT") (getenv "METIS_ROOT")))
                      (substitute* "etc/config.sh/metis"
                        (("$METISVERSION") (getenv "METISVERSION")))
                      (substitute* "etc/config.sh/scotch"
                        (("$SCOTCH_ROOT") (getenv "SCOTCH_ROOT")))
                      (substitute* "etc/config.sh/scotch"
                        (("$SCOTCHVERSION") (getenv "SCOTCHVERSION")))
                      (substitute* "etc/config.sh/settings"
                        (("$GMP_ROOT") (getenv "GMP_ROOT")))
                      (substitute* "etc/config.sh/settings"
                        (("$MPFR_ROOT") (getenv "MPFR_ROOT")))
                      ;; reset lockDir variable to refer to write-enabled
                      ;; directory
                      (substitute* "wmake/wmake"
                        (("        lockDir=.*$")
                         "        lockDir=$HOME/.$WM_PROJECT/.wmake\n"))
                      (substitute* "wmake/wmakeScheduler"
                        (("lockDir=.*$")
                         "lockDir=$HOME/.$WM_PROJECT/.wmake\n"))
                      (substitute* "wmake/wmakeSchedulerUptime"
                        (("lockDir=.*$")
                         "lockDir=$HOME/.$WM_PROJECT/.wmake\n"))
                      #t))
                  (add-after 'build 'cleanup
                    ;; Avoid unncessary, voluminous object and dep files.
                    (lambda _
                      (delete-file-recursively
                       "platforms/linux64GccDPInt32Opt/src")
                      (delete-file-recursively
                       "platforms/linux64GccDPInt32OptSYSTEMOPENMPI")
                      (for-each delete-file (find-files "." "\\.o$"))
                      #t))
                  (replace 'install
                    (lambda _
                      ;; use 'OpenFOAM-version' convention
                      (let ((install-dir (string-append
                                          %output "/lib/OpenFOAM-" ,version)))
                        (mkdir-p install-dir)     ; create install directory
                        ;; move contents of build directory to install directory
                        (copy-recursively "." install-dir))))
                  (add-after 'install 'add-symbolic-link
                    (lambda _
                      ;; add symbolic link for standard 'bin' directory
                      (symlink
                       (string-append "./lib/OpenFOAM-" ,version
                                      "/platforms/linux64GccDPInt32Opt/bin")
                       (string-append %output "/bin"))
                      #t)))))
    ;; Note:
    ;;  Tutorial files are installed read-only in /gnu/store.
    ;;  To allow write permissions on files copied from the store a
    ;;  'chmod' step is needed before running the applications.  For
    ;;  example, from a user's login:
    ;;  $ source $GUIX_PROFILE/lib/OpenFOAM-4.1/etc/bashrc
    ;;  $ mkdir -p $FOAM_RUN
    ;;  $ cd $FOAM_RUN
    ;;  $ cp -r $FOAM_TUTORIALS/incompressible/simpleFoam/pitzDaily .
    ;;  $ cd pitzDaily
    ;;  $ chmod -R u+w .
    ;;  $ blockMesh
    (synopsis "Framework for numerical simulation of fluid flow")
    (description "OpenFOAM provides a set of solvers and methods for tackling
problems in the field of Computational Fluid Dynamics (CFD).  It is written in
C++.  Governing equations such as the Navier-Stokes equations can be solved in
integral form.  Physical processes such as phase change, droplet transport and
chemical reaction can be modelled.  Numerical methods are included to deal with
sharp gradients, such as those encountered in flows with shock waves and flows
with gas/liquid interfaces.  Large problems may be split into smaller, connected
problems for efficient solution on parallel systems.")
    (license license:gpl3+)
    (home-page "https://openfoam.org")))
