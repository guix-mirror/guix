;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Konrad Hinsen <konrad.hinsen@fastmail.net>
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

(define-module (gnu packages chemistry)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (gnu packages gv)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages python)
  #:use-module (guix build-system python))

(define-public domainfinder
  (package
    (name "domainfinder")
    (version "2.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://bitbucket.org/khinsen/"
                           "domainfinder/downloads/DomainFinder-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1z26lsyf7xwnzwjvimmbla7ckipx6p734w7y0jk2a2fzci8fkdcr"))))
    (build-system python-build-system)
    (inputs
     `(("python-mmtk" ,python2-mmtk)))
    (arguments
     `(#:python ,python-2
       ;; No test suite
       #:tests? #f))
    (home-page "http://dirac.cnrs-orleans.fr/DomainFinder")
    (synopsis "Analysis of dynamical domains in proteins")
    (description "DomainFinder is an interactive program for the determination
and characterization of dynamical domains in proteins.  It can infer dynamical
domains by comparing two protein structures, or from normal mode analysis on a
single structure.  The software is currently not actively maintained and works
only with Python 2 and NumPy < 1.9.")
    (license license:cecill-c)))

(define with-numpy-1.8
  (package-input-rewriting `((,python2-numpy . ,python2-numpy-1.8))))

(define-public nmoldyn
  (package
    (name "nmoldyn")
    (version "3.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://bitbucket.org/khinsen/"
                           "nmoldyn3/downloads/nMOLDYN-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1mvmz3lkr217kdrd8cvdr1d82y58wp1403c9rnd943mijgq8xb5a"))))
    (build-system python-build-system)
    (inputs
     `(("python-matplotlib" ,(with-numpy-1.8 python2-matplotlib))
       ("python-scientific" ,python2-scientific)
       ("netcdf" ,netcdf)
       ("gv" ,gv)))
    (propagated-inputs
     `(("python-mmtk" ,python2-mmtk)))
    (arguments
     `(#:python ,python-2
       #:tests? #f  ; No test suite
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'create-linux2-directory
           (lambda _
             (mkdir-p "nMOLDYN/linux2")))
         (add-before 'build 'change-PDF-viewer
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "nMOLDYN/Preferences.py"
               ;; Set the paths for external executables, substituting
               ;; gv for acroread.
               ;; There is also vmd_path, but VMD is not free software
               ;; and Guix contains currently no free molecular viewer that
               ;; could be substituted.
               (("PREFERENCES\\['acroread_path'\\] = ''")
                (format "PREFERENCES['acroread_path'] = '~a'"
                        (which "gv")))
               (("PREFERENCES\\['ncdump_path'\\] = ''")
                (format "PREFERENCES['ncdump_path'] = '~a'"
                        (which "ncdump")))
               (("PREFERENCES\\['ncgen_path'\\] = ''")
                (format "PREFERENCES['ncgen_path'] = '~a'"
                        (which "ncgen3")))
               (("PREFERENCES\\['task_manager_path'\\] = ''")
                (format "PREFERENCES['task_manager_path'] = '~a'"
                        (which "task_manager")))
               ;; Show documentation as PDF
               (("PREFERENCES\\['documentation_style'\\] = 'html'")
                "PREFERENCES['documentation_style'] = 'pdf'") ))))))
    (home-page "http://dirac.cnrs-orleans.fr/nMOLDYN/")
    (synopsis "Analysis software for Molecular Dynamics trajectories")
    (description "nMOLDYN is an interactive analysis program for Molecular Dynamics
simulations.  It is especially designed for the computation and decomposition of
neutron scattering spectra, but also computes other quantities.  The software
is currently not actively maintained and works only with Python 2 and
NumPy < 1.9.")
    (license license:cecill)))
