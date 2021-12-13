;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build-system copy)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%copy-build-system-modules
            default-glibc
            lower
            copy-build
            copy-build-system))

;; Commentary:
;;
;; Standard build procedure for simple packages that don't require much
;; compilation, mostly just copying files around.  This is implemented as an
;; extension of `gnu-build-system'.
;;
;; Code:

(define %copy-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build copy-build-system)
    ,@%gnu-build-system-modules))

(define (default-glibc)
  "Return the default glibc package."
  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages base))))
    (module-ref module 'glibc)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (glibc (default-glibc))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME from the given arguments."
  (define private-keywords
    '(#:target #:inputs #:native-inputs))

  (bag
    (name name)
    (system system)
    (host-inputs `(,@(if source
                         `(("source" ,source))
                         '())
                   ,@inputs
                   ;; Keep the standard inputs of 'gnu-build-system'.
                   ,@(standard-packages)))
    (build-inputs native-inputs)
    (outputs outputs)
    (build copy-build)
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (copy-build name inputs
                     #:key
                     guile source
                     (outputs '("out"))
                     (install-plan ''(("." "./")))
                     (search-paths '())
                     (out-of-source? #t)
                     (validate-runpath? #t)
                     (patch-shebangs? #t)
                     (strip-binaries? #t)
                     (strip-flags ''("--strip-debug"))
                     (strip-directories ''("lib" "lib64" "libexec"
                                           "bin" "sbin"))
                     (phases '(@ (guix build copy-build-system)
                                 %standard-phases))
                     (system (%current-system))
                     (target #f)
                     (imported-modules %copy-build-system-modules)
                     (modules '((guix build copy-build-system)
                                (guix build utils))))
  "Build SOURCE using INSTALL-PLAN, and with INPUTS."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@modules)

          #$(with-build-variables inputs outputs
              #~(copy-build #:source #+source
                            #:system #$system
                            #:outputs %outputs
                            #:inputs %build-inputs
                            #:install-plan #$(if (pair? install-plan)
                                                 (sexp->gexp install-plan)
                                                 install-plan)
                            #:search-paths '#$(sexp->gexp
                                               (map search-path-specification->sexp
                                                    search-paths))
                            #:phases #$(if (pair? phases)
                                           (sexp->gexp phases)
                                           phases)
                            #:out-of-source? #$out-of-source?
                            #:validate-runpath? #$validate-runpath?
                            #:patch-shebangs? #$patch-shebangs?
                            #:strip-binaries? #$strip-binaries?
                            #:strip-flags #$(sexp->gexp strip-flags)
                            #:strip-directories #$(sexp->gexp strip-directories))))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:target #f
                      #:guile-for-build guile)))

(define copy-build-system
  (build-system
    (name 'copy)
    (description "The standard copy build system")
    (lower lower)))

;;; copy.scm ends here
