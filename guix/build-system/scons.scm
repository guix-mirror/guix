;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (guix build-system scons)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:export (%scons-build-system-modules
            scons-build
            scons-build-system))

;; Commentary:
;;
;; Standard build procedure for applications using SCons. This is implemented
;; as an extension of 'gnu-build-system'.
;;
;; Code:

(define %scons-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build scons-build-system)
    ,@%gnu-build-system-modules))

(define (default-scons)
  "Return the default SCons package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((python (resolve-interface '(gnu packages python-xyz))))
    (module-ref python 'scons)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (scons (default-scons))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:scons #:inputs #:native-inputs))

  (and (not target)                               ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs

                        ;; Keep the standard inputs of 'gnu-build-system'.
                        ,@(standard-packages)))
         (build-inputs `(("scons" ,scons)
                         ,@native-inputs))
         (outputs outputs)
         (build scons-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (scons-build name inputs
                      #:key
                      (source #f)
                      (tests? #t)
                      (scons-flags ''())
                      (build-targets #~'())
                      (test-target "test")
                      (install-targets #~'("install"))
                      (phases '%standard-phases)
                      (outputs '("out"))
                      (search-paths '())
                      (system (%current-system))
                      (guile #f)
                      (imported-modules %scons-build-system-modules)
                      (modules '((guix build scons-build-system)
                                 (guix build utils))))
  "Build SOURCE using SCons, and with INPUTS.  This assumes that SOURCE
provides a 'SConstruct' file as its build system."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          #$(with-build-variables inputs outputs
              #~(scons-build #:name #$name
                             #:source #+source
                             #:scons-flags #$(sexp->gexp scons-flags)
                             #:system #$system
                             #:build-targets #$build-targets
                             #:test-target #$test-target
                             #:tests? #$tests?
                             #:install-targets #$install-targets
                             #:phases #$(if (pair? phases)
                                            (sexp->gexp phases)
                                            phases)
                             #:outputs %outputs
                             #:inputs %build-inputs
                             #:search-paths
                             '#$(sexp->gexp
                                 (map search-path-specification->sexp
                                      search-paths)))))))

  (gexp->derivation name builder
                    #:system system
                    #:target #f
                    #:guile-for-build guile))

(define scons-build-system
  (build-system
    (name 'scons)
    (description "The standard SCons build system")
    (lower lower)))

;;; scons.scm ends here
