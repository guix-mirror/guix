;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
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

(define-module (guix build-system qt)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module ((guix build qt-utils)
                #:select (%qt-wrap-excluded-inputs))
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export (%qt-build-system-modules
            qt-build
            qt-build-system))

;; Commentary:
;;
;; This build system is an extension of the 'cmake-build-system'.  It
;; accommodates the needs of Qt and KDE applications by adding a phase run
;; after the 'install' phase:
;;
;; 'qt-wrap' phase:
;;
;; This phase looks for Qt5 plugin paths, QML paths and some XDG paths as well
;; as the corresponding environment variables.  If any of these is found in
;; the output or if respective environment variables are set, then all
;; programs in the output's "bin", "sbin", "libexec and "lib/libexec"
;; directories are wrapped in scripts defining the necessary environment
;; variables.
;;
;; Code:

(define %qt-build-system-modules
  ;; Build-side modules imported and used by default.
  `((guix build qt-build-system)
    (guix build qt-utils)
    ,@%cmake-build-system-modules))

(define (default-cmake)
  "Return the default CMake package."

  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages cmake))))
    (module-ref module 'cmake-minimal)))

;; This barely is a copy from (guix build-system cmake), only adjusted to use
;; the variables defined here.
(define* (lower name
                #:key source inputs native-inputs outputs system target
                (cmake (default-cmake))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    `(#:source #:cmake #:inputs #:native-inputs #:outputs
      ,@(if target '() '(#:target))))

  (bag
    (name name)
    (system system)
    (target target)
    (build-inputs `(,@(if source
                          `(("source" ,source))
                          '())
                    ,@`(("cmake" ,cmake))
                    ,@native-inputs
                    ,@(if target
                          ;; Use the standard cross inputs of
                          ;; 'gnu-build-system'.
                          (standard-cross-packages target 'host)
                          '())
                    ;; Keep the standard inputs of 'gnu-build-system'.
                    ,@(standard-packages)))
    (host-inputs inputs)

    ;; The cross-libc is really a target package, but for bootstrapping
    ;; reasons, we can't put it in 'host-inputs'.  Namely, 'cross-gcc' is a
    ;; native package, so it would end up using a "native" variant of
    ;; 'cross-libc' (built with 'gnu-build'), whereas all the other packages
    ;; would use a target variant (built with 'gnu-cross-build'.)
    (target-inputs (if target
                       (standard-cross-packages target 'target)
                       '()))
    (outputs outputs)
    (build (if target qt-cross-build qt-build))
    (arguments (strip-keyword-arguments private-keywords arguments))))


(define* (qt-build store name inputs
                   #:key (guile #f)
                   (outputs '("out")) (configure-flags ''())
                   (search-paths '())
                   (make-flags ''())
                   (out-of-source? #t)
                   (build-type "RelWithDebInfo")
                   (tests? #t)
                   (test-target "test")
                   (parallel-build? #t) (parallel-tests? #f)
                   (validate-runpath? #t)
                   (patch-shebangs? #t)
                   (strip-binaries? #t)
                   (strip-flags ''("--strip-debug"))
                   (strip-directories ''("lib" "lib64" "libexec"
                                         "bin" "sbin"))
                   (phases '(@ (guix build qt-build-system)
                               %standard-phases))
                   (qt-wrap-excluded-outputs ''())
                   (qt-wrap-excluded-inputs %qt-wrap-excluded-inputs)
                   (system (%current-system))
                   (imported-modules %qt-build-system-modules)
                   (modules '((guix build qt-build-system)
                              (guix build utils))))
  "Build SOURCE using CMAKE, and with INPUTS. This assumes that SOURCE
provides a 'CMakeLists.txt' file as its build system."
  (define builder
    `(begin
       (use-modules ,@modules)
       (qt-build #:source ,(match (assoc-ref inputs "source")
                                  (((? derivation? source))
                                   (derivation->output-path source))
                                  ((source)
                                   source)
                                  (source
                                   source))
                 #:system ,system
                 #:outputs %outputs
                 #:inputs %build-inputs
                 #:search-paths ',(map search-path-specification->sexp
                                       search-paths)
                 #:phases ,phases
                 #:qt-wrap-excluded-outputs ,qt-wrap-excluded-outputs
                 #:qt-wrap-excluded-inputs ,qt-wrap-excluded-inputs
                 #:configure-flags ,configure-flags
                 #:make-flags ,make-flags
                 #:out-of-source? ,out-of-source?
                 #:build-type ,build-type
                 #:tests? ,tests?
                 #:test-target ,test-target
                 #:parallel-build? ,parallel-build?
                 #:parallel-tests? ,parallel-tests?
                 #:validate-runpath? ,validate-runpath?
                 #:patch-shebangs? ,patch-shebangs?
                 #:strip-binaries? ,strip-binaries?
                 #:strip-flags ,strip-flags
                 #:strip-directories ,strip-directories)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system #:graft? #f))
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(gnu packages commencement)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system #:graft? #f)))))

  (build-expression->derivation store name builder
                                #:system system
                                #:inputs inputs
                                #:modules imported-modules
                                #:outputs outputs
                                #:guile-for-build guile-for-build))


;;;
;;; Cross-compilation.
;;;

(define* (qt-cross-build store name
                         #:key
                         target native-drvs target-drvs
                         (guile #f)
                         (outputs '("out"))
                         (configure-flags ''())
                         (search-paths '())
                         (native-search-paths '())
                         (make-flags ''())
                         (out-of-source? #t)
                         (build-type "RelWithDebInfo")
                         (tests? #f) ; nothing can be done
                         (test-target "test")
                         (parallel-build? #t) (parallel-tests? #f)
                         (validate-runpath? #t)
                         (patch-shebangs? #t)
                         (strip-binaries? #t)
                         (strip-flags ''("--strip-debug"
                                         "--enable-deterministic-archives"))
                         (strip-directories ''("lib" "lib64" "libexec"
                                               "bin" "sbin"))
                         (phases '(@ (guix build qt-build-system)
                                     %standard-phases))
                         (system (%current-system))
                         (build (nix-system->gnu-triplet system))
                         (imported-modules %qt-build-system-modules)
                         (modules '((guix build qt-build-system)
                                    (guix build utils))))
  "Cross-build NAME using CMAKE for TARGET, where TARGET is a GNU triplet and
with INPUTS.  This assumes that SOURCE provides a 'CMakeLists.txt' file as its
build system."
  (define builder
    `(begin
       (use-modules ,@modules)
       (let ()
         (define %build-host-inputs
           ',(map (match-lambda
                    ((name (? derivation? drv) sub ...)
                     `(,name . ,(apply derivation->output-path drv sub)))
                    ((name path)
                     `(,name . ,path)))
                  native-drvs))

         (define %build-target-inputs
           ',(map (match-lambda
                    ((name (? derivation? drv) sub ...)
                     `(,name . ,(apply derivation->output-path drv sub)))
                    ((name (? package? pkg) sub ...)
                     (let ((drv (package-cross-derivation store pkg
                                                          target system)))
                       `(,name . ,(apply derivation->output-path drv sub))))
                    ((name path)
                     `(,name . ,path)))
                  target-drvs))

         (qt-build #:source ,(match (assoc-ref native-drvs "source")
                                    (((? derivation? source))
                                     (derivation->output-path source))
                                    ((source)
                                     source)
                                    (source
                                     source))
                   #:system ,system
                   #:build ,build
                   #:target ,target
                   #:outputs %outputs
                   #:inputs %build-target-inputs
                   #:native-inputs %build-host-inputs
                   #:search-paths ',(map search-path-specification->sexp
                                         search-paths)
                   #:native-search-paths ',(map
                                            search-path-specification->sexp
                                            native-search-paths)
                   #:phases ,phases
                   #:configure-flags ,configure-flags
                   #:make-flags ,make-flags
                   #:out-of-source? ,out-of-source?
                   #:build-type ,build-type
                   #:tests? ,tests?
                   #:test-target ,test-target
                   #:parallel-build? ,parallel-build?
                   #:parallel-tests? ,parallel-tests?
                   #:validate-runpath? ,validate-runpath?
                   #:patch-shebangs? ,patch-shebangs?
                   #:strip-binaries? ,strip-binaries?
                   #:strip-flags ,strip-flags
                   #:strip-directories ,strip-directories))))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system #:graft? #f))
      (#f                               ; the default
       (let* ((distro (resolve-interface '(gnu packages commencement)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system #:graft? #f)))))

  (build-expression->derivation store name builder
                                #:system system
                                #:inputs (append native-drvs target-drvs)
                                #:outputs outputs
                                #:modules imported-modules
                                #:guile-for-build guile-for-build))

(define qt-build-system
  (build-system
    (name 'qt)
    (description
     "The CMake build system augmented with definition of suitable environment
variables for Qt and KDE in program wrappers.")
    (lower lower)))
