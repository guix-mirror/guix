;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
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

(define-module (gnu packages mercury)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages pkg-config)
  #:use-module (ice-9 match)) ; match-lambda

;; NOTE: Mercury uses a tightly coupled fork of BDWGC and
;; libatomic-ops. When updating the package, please check the GitHub
;; repository to ensure that the submodule commit matches what is
;; provided.
(define (gc-fork package-name package-url
                 package-commit package-hash)
  (let ((commit package-commit))
    (package (inherit package-name)
             (source
              (origin
                (method git-fetch)
                (uri (git-reference
                      (url package-url)
                      (commit commit)))
                (sha256 (base32 package-hash)))))))

;; NOTE: Mercury /MUST/ bootstrap from a tarball release.
;; Once the bootstrapping compiler is established, this
;; minimal build can be used for further compiling Mercury
;; from a git checkout with additional grades enabled.
(define-public mercury-minimal
  (package
    (name "mercury-minimal")
    (version "20.06.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dl.mercurylang.org/release/mercury-srcdist-"
                    version ".tar.gz"))
              (sha256
               (base32
                "07qwkk871yxd4q1sw5xv26g8jrpvnpprmzvfd7zg7i142kl3l2gg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 match))
       #:tests? #f ; Tests are run on the stage-2 compiler.
       ;; TODO: Find a way to bypass all static linkages.
       #:configure-flags (list "--enable-minimal-install")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'replace-boehm-gc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (libgc (assoc-ref inputs "libgc"))
                   (libatomic-ops (assoc-ref inputs "libatomic-ops"))
                   (unpack (assoc-ref %standard-phases 'unpack))
                   (patch-source-shebangs
                    (assoc-ref %standard-phases 'patch-source-shebangs)))
               (map (match-lambda
                      ((src orig-name new-name)
                       (with-directory-excursion "."
                         (apply unpack (list #:source src)))
                       (delete-file-recursively new-name)
                       (invoke "mv" orig-name new-name)
                       (with-directory-excursion new-name
                         (apply patch-source-shebangs (list #:source src)))))
                    `((,libgc "source" "boehm_gc")))
               (map (match-lambda
                      ((src orig-name new-name)
                       (with-directory-excursion "."
                         (apply unpack (list #:source src)))
                       (delete-file-recursively new-name)
                       (invoke "mv" orig-name new-name)
                       (with-directory-excursion new-name
                         (apply patch-source-shebangs (list #:source src)))))
                    `((,libatomic-ops "source" "boehm_gc/libatomic_ops")))
               #t)))
         (add-after 'replace-boehm-gc 'patch-paths
           (lambda _
             (substitute*
                 (list "Makefile"
                       "Mmakefile"
                       "scripts/mercury_update_interface.in"
                       "scripts/mercury_config.in"
                       "scripts/mmake.in"
                       "scripts/Mmake.vars.in"
                       "scripts/mdb.in"
                       "scripts/rs6000_hack"
                       "scripts/fullarch"
                       "scripts/mmc.in"
                       "scripts/canonical_grade"
                       "scripts/mprof.in"
                       "scripts/gud.el"
                       "scripts/ml.in"
                       "scripts/canonical_grade.in"
                       "scripts/mdprof.in"
                       "scripts/vpath_find"
                       "scripts/mkfifo_using_mknod.in"
                       "scripts/prepare_install_dir.in"
                       "scripts/mprof_merge_runs"
                       "scripts/mtc"
                       "scripts/mgnuc.in"
                       "scripts/c2init.in"
                       "bindist/bindist.Makefile"
                       "boehm_gc/configure.ac"
                       "boehm_gc/Makefile.direct")
               (("/bin/sh") (which "sh"))
               (("/bin/pwd") (which "pwd"))
               (("/bin/rm") (which "rm")))
             #t)))))
    (native-inputs
     `(("texinfo" ,texinfo)
       ("flex" ,flex)
       ("tcsh" ,tcsh)
       ("bison" ,bison)
       ("readline" ,readline)
       ("libatomic-ops" ,(package-source
                          (gc-fork
                           libatomic-ops
                           "https://github.com/Mercury-Language/libatomic_ops.git"
                           "49b70d57f6922fd8be55a7dcb77955c8abfc9ae9"
                           "1flvwscsa6b2b8a38vhhcgl10bbkb5nnihw7s7iia60cinf7wcqm")))
       ("libgc" ,(package-source
                  (gc-fork
                   libgc-7
                   "https://github.com/Mercury-Language/bdwgc.git"
                   "43ac2ea45261ba0a715534e9da41b2504904c46a"
                   "0bmzmbs7id0ndyhy9xli6fhfad1shrim6vmy2k8m1nqr5wb31q76")))
       ("pkg-config" ,pkg-config)))
    (synopsis "Pure logic programming language (used only for
bootstrapping dependent Mercury)")
    (description "Mercury is a logic/functional programming language which
combines the clarity and expressiveness of declarative programming with advanced
static analysis and error detection features.  Its highly optimized execution
algorithm delivers efficiency far in excess of existing logic programming
systems, and close to conventional programming systems.  Mercury addresses
the problems of large-scale program development, allowing modularity,
separate compilation, and numerous optimization/time trade-offs.")
    (home-page "https://mercurylang.org")
    (license license:gpl2)))

;; NOTE: This package is quite large and will take an extensive
;; amount of time to compile, especially with bootcheck functionality
;; enabled. To ensure that we do not monopolize the CI & build servers,
;; please make sure that your changes are justified.
(define-public mercury
  (package (inherit mercury-minimal)
           (name "mercury")
           (version "20.06.1")
           (source
            (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Mercury-Language/mercury")
                    (commit (string-append
                             "version-"
                             (string-join (string-split version #\.) "_")))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1b6rmdinw8mj6n9sc7c75kkf42gd2k254rf51x4snlrqckxj7aaz"))))
           (arguments
            (substitute-keyword-arguments
                (package-arguments mercury-minimal)
              ;; TODO: Find a way to bypass all static linkages.
              ((#:configure-flags flags ''())
               `(list ""))
              ((#:tests? _) #f) ; FIXME: Many test-cases failing.
              ((#:phases phases)
               `(modify-phases ,phases
                  (replace 'patch-paths
                    (lambda _
                      (substitute*
                          (list "prepare.sh"
                                "Makefile"
                                "Mmakefile"
                                "scripts/mercury_update_interface.in"
                                "scripts/mercury_config.in"
                                "scripts/mmake.in"
                                "scripts/Mmake.vars.in"
                                "scripts/mdb.in"
                                "scripts/rs6000_hack"
                                "scripts/fullarch"
                                "scripts/mmc.in"
                                "scripts/mprof.in"
                                "scripts/gud.el"
                                "scripts/ml.in"
                                "scripts/canonical_grade.in"
                                "scripts/mdprof.in"
                                "scripts/vpath_find"
                                "scripts/mkfifo_using_mknod.in"
                                "scripts/prepare_install_dir.in"
                                "scripts/mprof_merge_runs"
                                "scripts/mtc"
                                "scripts/mgnuc.in"
                                "scripts/c2init.in"
                                "tools/bootcheck"
                                "boehm_gc/configure.ac"
                                "boehm_gc/Makefile.direct")
                        (("/bin/sh") (which "sh"))
                        (("/bin/pwd") (which "pwd"))
                        (("/bin/rm") (which "rm"))
                        (("boehm_gc/.git") "boehm_gc"))
                      #t))
                  (replace 'bootstrap
                    (lambda _
                      (invoke "./prepare.sh")
                      #t))))))
           ;; TODO: Uncomment phase when tests are enabled.
           ;; (replace 'check
           ;;   (lambda _
           ;;     (invoke "./tools/bootcheck")
           ;;     #t))
           ;;
           ;; TODO: The mercury configuration system determines
           ;; grade support by looking for available toolchains.
           ;; Eventually we need to add inputs for Java, Erlang,
           ;; C#, etc. in order to enable these extra grades.
           (native-inputs
            (modify-inputs (package-native-inputs mercury-minimal)
              (prepend mercury-minimal autoconf automake)))
           (synopsis "Pure logic programming language")))
