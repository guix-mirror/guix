;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Petter <petter@mykolab.ch>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
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

(define-module (guix build-system go)
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export (%go-build-system-modules
            go-build
            go-build-system))

;; Commentary:
;;
;; Standard build procedure for packages using the Go build system.  It is
;; implemented as an extension of 'gnu-build-system'.
;;
;; Code:

(define %go-build-system-modules
  ;; Build-side modules imported and used by default.
  `((guix build go-build-system)
    (guix build union)
    ,@%gnu-build-system-modules))

(define (default-go)
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((go (resolve-interface '(gnu packages golang))))
    (module-ref go 'go)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (go (default-go))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:source #:target #:go #:inputs #:native-inputs))

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
         (build-inputs `(("go" ,go)
                         ,@native-inputs))
         (outputs outputs)
         (build go-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (go-build store name inputs
                   #:key
                   (phases '(@ (guix build go-build-system)
                               %standard-phases))
                   (outputs '("out"))
                   (search-paths '())
                   (install-source? #t)
                   (import-path "")
                   (unpack-path "")
                   (tests? #t)
                   (allow-go-reference? #f)
                   (system (%current-system))
                   (guile #f)
                   (imported-modules %go-build-system-modules)
                   (modules '((guix build go-build-system)
                              (guix build union)
                              (guix build utils))))
  (define builder
   `(begin
      (use-modules ,@modules)
      (go-build #:name ,name
                #:source ,(match (assoc-ref inputs "source")
                                 (((? derivation? source))
                                  (derivation->output-path source))
                                 ((source)
                                  source)
                                 (source
                                  source))
                #:system ,system
                #:phases ,phases
                #:outputs %outputs
                #:search-paths ',(map search-path-specification->sexp
                                      search-paths)
                #:install-source? ,install-source?
                #:import-path ,import-path
                #:unpack-path ,unpack-path
                #:tests? ,tests?
                #:allow-go-reference? ,allow-go-reference?
                #:inputs %build-inputs)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system #:graft? #f))
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(gnu packages commencement)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system
                             #:graft? #f)))))

  (build-expression->derivation store name builder
                                #:inputs inputs
                                #:system system
                                #:modules imported-modules
                                #:outputs outputs
                                #:guile-for-build guile-for-build))

(define go-build-system
  (build-system
    (name 'go)
    (description
     "Build system for Go programs")
    (lower lower)))
