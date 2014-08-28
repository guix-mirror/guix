;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
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
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export (cmake-build
            cmake-build-system))

;; Commentary:
;;
;; Standard build procedure for packages using CMake. This is implemented as an
;; extension of `gnu-build-system'.
;;
;; Code:

(define (default-cmake)
  "Return the default CMake package."

  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages cmake))))
    (module-ref module 'cmake)))

(define* (cmake-build store name source inputs
                     #:key (guile #f)
                     (outputs '("out")) (configure-flags ''())
                     (search-paths '())
                     (make-flags ''())
                     (cmake (default-cmake))
                     (out-of-source? #t)
                     (tests? #t)
                     (test-target "test")
                     (parallel-build? #t) (parallel-tests? #f)
                     (patch-shebangs? #t)
                     (strip-binaries? #t)
                     (strip-flags ''("--strip-debug"))
                     (strip-directories ''("lib" "lib64" "libexec"
                                           "bin" "sbin"))
                     (phases '(@ (guix build cmake-build-system)
                                 %standard-phases))
                     (system (%current-system))
                     (imported-modules '((guix build cmake-build-system)
                                         (guix build gnu-build-system)
                                         (guix build utils)))
                     (modules '((guix build cmake-build-system)
                                (guix build utils))))
  "Build SOURCE using CMAKE, and with INPUTS. This assumes that SOURCE
provides a 'CMakeLists.txt' file as its build system."
  (define builder
    `(begin
       (use-modules ,@modules)
       (cmake-build #:source ,(if (derivation? source)
                                  (derivation->output-path source)
                                  source)
                    #:system ,system
                    #:outputs %outputs
                    #:inputs %build-inputs
                    #:search-paths ',(map search-path-specification->sexp
                                          (append search-paths
                                                  (standard-search-paths)))
                    #:phases ,phases
                    #:configure-flags ,configure-flags
                    #:make-flags ,make-flags
                    #:out-of-source? ,out-of-source?
                    #:tests? ,tests?
                    #:test-target ,test-target
                    #:parallel-build? ,parallel-build?
                    #:parallel-tests? ,parallel-tests?
                    #:patch-shebangs? ,patch-shebangs?
                    #:strip-binaries? ,strip-binaries?
                    #:strip-flags ,strip-flags
                    #:strip-directories ,strip-directories)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system))
      ((and (? string?) (? derivation-path?))
       guile)
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(gnu packages commencement)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system)))))

  (let ((cmake (package-derivation store cmake system)))
    (build-expression->derivation store name builder
                                  #:system system
                                  #:inputs
                                  `(,@(if source
                                          `(("source" ,source))
                                          '())
                                    ("cmake" ,cmake)
                                    ,@inputs

                                    ;; Keep the standard inputs of
                                    ;; `gnu-build-system'.
                                    ,@(standard-inputs system))

                                  #:modules imported-modules
                                  #:outputs outputs
                                  #:guile-for-build guile-for-build)))

(define cmake-build-system
  (build-system (name 'cmake)
                (description "The standard CMake build system")
                (build cmake-build)))

;;; cmake.scm ends here
