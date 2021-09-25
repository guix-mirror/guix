;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Liliana Marie Prikler <liliana.prikler@gmail.com>
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

(define-module (guix build-system renpy)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix memoization)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%renpy-build-system-modules
            default-renpy
            renpy-build
            renpy-build-system))

(define (default-renpy)
  "Return the default Ren'py package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((module (resolve-interface '(gnu packages game-development))))
    (module-ref module 'renpy)))

(define %renpy-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build renpy-build-system)
    (guix build json)
    (guix build python-build-system)
    ,@%gnu-build-system-modules))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (renpy (default-renpy))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:source #:target #:renpy #:inputs #:native-inputs))

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
         (build-inputs `(("renpy" ,renpy)
                         ,@native-inputs))
         (outputs outputs)
         (build renpy-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (renpy-build store name inputs
                       #:key
                       (phases '(@ (guix build renpy-build-system)
                                   %standard-phases))
                       (configure-flags ''())
                       (outputs '("out"))
                       (output "out")
                       (game "game")
                       (search-paths '())
                       (system (%current-system))
                       (guile #f)
                       (imported-modules %renpy-build-system-modules)
                       (modules '((guix build renpy-build-system)
                                  (guix build utils))))
  "Build SOURCE using RENPY, and with INPUTS."
  (define builder
    `(begin
       (use-modules ,@modules)
       (renpy-build #:name ,name
                    #:source ,(match (assoc-ref inputs "source")
                                (((? derivation? source))
                                 (derivation->output-path source))
                                ((source)
                                 source)
                                (source
                                 source))
                     #:configure-flags ,configure-flags
                     #:system ,system
                     #:phases ,phases
                     #:outputs %outputs
                     #:output ,output
                     #:game ,game
                     #:search-paths ',(map search-path-specification->sexp
                                           search-paths)
                     #:inputs %build-inputs)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system #:graft? #f))
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(gnu packages commencement)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system #:graft? #f)))))

  (build-expression->derivation store name builder
                                #:inputs inputs
                                #:system system
                                #:modules imported-modules
                                #:outputs outputs
                                #:guile-for-build guile-for-build))

(define renpy-build-system
  (build-system
    (name 'renpy)
    (description "The Ren'py build system")
    (lower lower)))
