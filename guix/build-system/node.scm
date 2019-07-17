;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Jelle Licht <jlicht@fsfe.org>
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

(define-module (guix build-system node)
  #:use-module (guix store)
  #:use-module (guix build json)
  #:use-module (guix build union)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:export (npm-meta-uri
            %node-build-system-modules
            node-build
            node-build-system))

(define (npm-meta-uri name)
  "Return a URI string for the metadata of node module NAME found in the npm
registry."
  (string-append "https://registry.npmjs.org/" name))

(define %node-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build node-build-system)
    (guix build json)
    (guix build union)
    ,@%gnu-build-system-modules)) ;; TODO: Might be not needed

(define (default-node)
  "Return the default Node package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((node (resolve-interface '(gnu packages node))))
    (module-ref node 'node)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (node (default-node))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:source #:target #:node #:inputs #:native-inputs))

  (and (not target)                    ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs

                        ;; Keep the standard inputs of 'gnu-build-system'.
                        ,@(standard-packages)))
         (build-inputs `(("node" ,node)
                         ,@native-inputs))
         (outputs outputs)
         (build node-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (node-build store name inputs
                     #:key
                     (npm-flags ''())
                     (tests? #t)
                     (phases '(@ (guix build node-build-system)
                                 %standard-phases))
                     (outputs '("out"))
                     (search-paths '())
                     (system (%current-system))
                     (guile #f)
                     (imported-modules %node-build-system-modules)
                     (modules '((guix build node-build-system)
				(guix build json)
				(guix build union)
                                (guix build utils))))
  "Build SOURCE using NODE and INPUTS."
  (define builder
    `(begin
       (use-modules ,@modules)
       (node-build #:name ,name
                   #:source ,(match (assoc-ref inputs "source")
                               (((? derivation? source))
                                (derivation->output-path source))
                               ((source)
                                source)
                               (source
                                source))
                   #:system ,system
                   #:npm-flags ,npm-flags
                   #:tests? ,tests?
                   #:phases ,phases
                   #:outputs %outputs
                   #:search-paths ',(map search-path-specification->sexp
                                         search-paths)
                   #:inputs %build-inputs)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system #:graft? #f))
      (#f
       (let* ((distro (resolve-interface '(gnu packages commencement)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system #:graft? #f)))))

  (build-expression->derivation store name builder
                                #:inputs inputs
                                #:system system
                                #:modules imported-modules
                                #:outputs outputs
                                #:guile-for-build guile-for-build))

(define node-build-system
  (build-system
    (name 'node)
    (description "The standard Node build system")
    (lower lower)))
