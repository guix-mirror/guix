;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
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

(define-module (guix build-system julia)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (%julia-build-system-modules
            julia-build
            julia-build-system))

;; Commentary:
;;
;; Standard build procedure for Julia packages.
;;
;; Code:

(define %julia-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build julia-build-system)
    ,@%gnu-build-system-modules))

(define (default-julia)
  "Return the default Julia package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((julia-mod (resolve-interface '(gnu packages julia))))
    (module-ref julia-mod 'julia)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (julia (default-julia))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:julia #:inputs #:native-inputs))

  (and (not target)                        ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs
                        
                        ;; Keep the standard inputs of 'gnu-build-system'.
                        ,@(standard-packages)))
         (build-inputs `(("julia" ,julia)
                         ,@native-inputs))
         (outputs outputs)
         (build julia-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (julia-build name inputs
                      #:key source
                      (tests? #t)
                      (parallel-tests? #t)
                      (phases '%standard-phases)
                      (outputs '("out"))
                      (search-paths '())
                      (system (%current-system))
                      (guile #f)
                      (julia-package-name #f)
                      (julia-package-uuid #f)
                      (imported-modules %julia-build-system-modules)
                      (modules '((guix build julia-build-system)
                                 (guix build utils))))
  "Build SOURCE using Julia, and with INPUTS."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))
          (julia-build #:name #$name
                       #:source #+source
                       #:system #$system
                       #:tests? #$tests?
                       #:parallel-tests? #$parallel-tests?
                       #:phases #$phases
                       #:outputs #$(outputs->gexp outputs)
                       #:search-paths '#$(sexp->gexp
                                          (map search-path-specification->sexp
                                               search-paths))
                       #:inputs #$(input-tuples->gexp inputs)
                       #:julia-package-name #$julia-package-name
                       #:julia-package-uuid #$julia-package-uuid))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:guile-for-build guile)))

(define julia-build-system
  (build-system
   (name 'julia)
   (description "The build system for Julia packages")
   (lower lower)))

;;; julia.scm ends here
