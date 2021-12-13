;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2020 Morgan Smith <Morgan.J.Smith@outlook.com>
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

(define-module (guix build-system emacs)
  #:use-module ((guix build emacs-build-system)
                #:select (%default-include %default-exclude))
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
  #:export (%emacs-build-system-modules
            emacs-build
            emacs-build-system)
  #:re-export (%default-include         ;for convenience
               %default-exclude))


;; Commentary:
;;
;; Standard build procedure for Emacs packages.  This is implemented as an
;; extension of 'gnu-build-system'.
;;
;; Code:

(define %emacs-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build emacs-build-system)
    (guix build emacs-utils)
    ,@%gnu-build-system-modules))

(define (default-emacs)
  "Return the default Emacs package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((emacs-mod (resolve-interface '(gnu packages emacs))))
    (module-ref emacs-mod 'emacs-minimal)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (emacs (default-emacs))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:emacs #:inputs #:native-inputs))

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
         (build-inputs `(("emacs" ,emacs)
                         ,@native-inputs))
         (outputs outputs)
         (build emacs-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (emacs-build name inputs
                      #:key source
                      (tests? #f)
                      (parallel-tests? #t)
                      (test-command ''("make" "check"))
                      (phases '%standard-phases)
                      (outputs '("out"))
                      (include (quote %default-include))
                      (exclude (quote %default-exclude))
                      (search-paths '())
                      (system (%current-system))
                      (guile #f)
                      (imported-modules %emacs-build-system-modules)
                      (modules '((guix build emacs-build-system)
                                 (guix build utils)
                                 (guix build emacs-utils))))
  "Build SOURCE using EMACS, and with INPUTS."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))
          (emacs-build #:name #$name
                       #:source #+source
                       #:system #$system
                       #:test-command #$test-command
                       #:tests? #$tests?
                       #:parallel-tests? #$parallel-tests?
                       #:phases #$phases
                       #:outputs #$(outputs->gexp outputs)
                       #:include #$include
                       #:exclude #$exclude
                       #:search-paths '#$(sexp->gexp
                                          (map search-path-specification->sexp
                                               search-paths))
                       #:inputs #$(input-tuples->gexp inputs)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:guile-for-build guile)))

(define emacs-build-system
  (build-system
    (name 'emacs)
    (description "The build system for Emacs packages")
    (lower lower)))

;;; emacs.scm ends here
