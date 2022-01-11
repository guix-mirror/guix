;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2015, 2020-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (guix build-system cmake)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export (%cmake-build-system-modules
            cmake-build
            cmake-build-system))

;; Commentary:
;;
;; Standard build procedure for packages using CMake. This is implemented as an
;; extension of `gnu-build-system'.
;;
;; Code:

(define %cmake-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build cmake-build-system)
    ,@%gnu-build-system-modules))

(define (default-cmake target)
  "Return the default CMake package."

  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages cmake))))
    (module-ref module
                (if target
                    'cmake-minimal-cross
                    'cmake-minimal))))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (cmake (default-cmake target))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    `(#:cmake #:inputs #:native-inputs
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
                    ,@(if target '() inputs)
                    ,@(if target
                          ;; Use the standard cross inputs of
                          ;; 'gnu-build-system'.
                          (standard-cross-packages target 'host)
                          '())
                    ;; Keep the standard inputs of 'gnu-build-system'.
                    ,@(standard-packages)))
    (host-inputs (if target inputs '()))

    ;; The cross-libc is really a target package, but for bootstrapping
    ;; reasons, we can't put it in 'host-inputs'.  Namely, 'cross-gcc' is a
    ;; native package, so it would end up using a "native" variant of
    ;; 'cross-libc' (built with 'gnu-build'), whereas all the other packages
    ;; would use a target variant (built with 'gnu-cross-build'.)
    (target-inputs (if target
                       (standard-cross-packages target 'target)
                       '()))
    (outputs outputs)
    (build (if target cmake-cross-build cmake-build))
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (cmake-build name inputs
                      #:key guile source
                      (outputs '("out")) (configure-flags ''())
                      (search-paths '())
                      (make-flags ''())
                      (out-of-source? #t)
                      (build-type "RelWithDebInfo")
                      (tests? #t)
                      (test-target "test")
                      (parallel-build? #t) (parallel-tests? #t)
                      (validate-runpath? #t)
                      (patch-shebangs? #t)
                      (strip-binaries? #t)
                      (strip-flags ''("--strip-debug"))
                      (strip-directories ''("lib" "lib64" "libexec"
                                            "bin" "sbin"))
                      (phases '%standard-phases)
                      (system (%current-system))
                      (substitutable? #t)
                      (imported-modules %cmake-build-system-modules)
                      (modules '((guix build cmake-build-system)
                                 (guix build utils))))
  "Build SOURCE using CMAKE, and with INPUTS. This assumes that SOURCE
provides a 'CMakeLists.txt' file as its build system."
  (define build
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          #$(with-build-variables inputs outputs
              #~(cmake-build #:source #+source
                             #:system #$system
                             #:outputs %outputs
                             #:inputs %build-inputs
                             #:search-paths '#$(sexp->gexp
                                                (map search-path-specification->sexp
                                                     search-paths))
                             #:phases #$(if (pair? phases)
                                            (sexp->gexp phases)
                                            phases)
                             #:configure-flags #$(if (pair? configure-flags)
                                                     (sexp->gexp configure-flags)
                                                     configure-flags)
                             #:make-flags #$make-flags
                             #:out-of-source? #$out-of-source?
                             #:build-type #$build-type
                             #:tests? #$tests?
                             #:test-target #$test-target
                             #:parallel-build? #$parallel-build?
                             #:parallel-tests? #$parallel-tests?
                             #:validate-runpath? #$validate-runpath?
                             #:patch-shebangs? #$patch-shebangs?
                             #:strip-binaries? #$strip-binaries?
                             #:strip-flags #$(sexp->gexp strip-flags)
                             #:strip-directories #$(sexp->gexp strip-directories))))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name build
                      #:system system
                      #:target #f
                      #:graft? #f
                      #:substitutable? substitutable?
                      #:guile-for-build guile)))


;;;
;;; Cross-compilation.
;;;

(define* (cmake-cross-build name
                            #:key
                            target
                            build-inputs target-inputs host-inputs
                            source guile
                            (outputs '("out"))
                            (configure-flags ''())
                            (search-paths '())
                            (native-search-paths '())
                            (make-flags ''())
                            (out-of-source? #t)
                            (build-type "RelWithDebInfo")
                            (tests? #f) ; nothing can be done
                            (test-target "test")
                            (parallel-build? #t) (parallel-tests? #t)
                            (validate-runpath? #t)
                            (patch-shebangs? #t)
                            (strip-binaries? #t)
                            (strip-flags ''("--strip-debug"
                                            "--enable-deterministic-archives"))
                            (strip-directories ''("lib" "lib64" "libexec"
                                                  "bin" "sbin"))
                            (phases '%standard-phases)
                            (substitutable? #t)
                            (system (%current-system))
                            (build (nix-system->gnu-triplet system))
                            (imported-modules %cmake-build-system-modules)
                            (modules '((guix build cmake-build-system)
                                       (guix build utils))))
  "Cross-build NAME using CMAKE for TARGET, where TARGET is a GNU triplet and
with INPUTS.  This assumes that SOURCE provides a 'CMakeLists.txt' file as its
build system."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          (define %build-host-inputs
            #+(input-tuples->gexp build-inputs))

          (define %build-target-inputs
            (append #$(input-tuples->gexp host-inputs)
                    #+(input-tuples->gexp target-inputs)))

          (define %build-inputs
            (append %build-host-inputs %build-target-inputs))

          (define %outputs
            #$(outputs->gexp outputs))

          (cmake-build #:source #+source
                       #:system #$system
                       #:build #$build
                       #:target #$target
                       #:outputs %outputs
                       #:inputs %build-target-inputs
                       #:native-inputs %build-host-inputs
                       #:search-paths '#$(map search-path-specification->sexp
                                              search-paths)
                       #:native-search-paths '#$(map
                                                 search-path-specification->sexp
                                                 native-search-paths)
                       #:phases #$phases
                       #:configure-flags #$configure-flags
                       #:make-flags #$make-flags
                       #:out-of-source? #$out-of-source?
                       #:build-type #$build-type
                       #:tests? #$tests?
                       #:test-target #$test-target
                       #:parallel-build? #$parallel-build?
                       #:parallel-tests? #$parallel-tests?
                       #:validate-runpath? #$validate-runpath?
                       #:patch-shebangs? #$patch-shebangs?
                       #:make-dynamic-linker-cache? #f ;cross-compiling
                       #:strip-binaries? #$strip-binaries?
                       #:strip-flags #$strip-flags
                       #:strip-directories #$strip-directories))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:target target
                      #:graft? #f
                      #:substitutable? substitutable?
                      #:guile-for-build guile)))

(define cmake-build-system
  (build-system
    (name 'cmake)
    (description "The standard CMake build system")
    (lower lower)))

;;; cmake.scm ends here
