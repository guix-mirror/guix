;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
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

(define-module (guix build-system dune)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module ((guix build-system gnu) #:prefix gnu:)
  #:use-module ((guix build-system ocaml) #:prefix ocaml:)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%dune-build-system-modules
            dune-build
            dune-build-system))

;; Commentary:
;;
;; Standard build procedure for packages using dune. This is implemented as an
;; extension of `ocaml-build-system'.
;;
;; Code:

(define %dune-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build dune-build-system)
    ,@ocaml:%ocaml-build-system-modules))

(define (default-dune)
  "Return the default OCaml package."

  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages ocaml))))
    (module-ref module 'dune)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (dune (default-dune))
                (ocaml (ocaml:default-ocaml))
                (findlib (ocaml:default-findlib))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:source #:target #:dune #:findlib #:ocaml #:inputs #:native-inputs))

  (and (not target)                               ;XXX: no cross-compilation
       (let ((base (ocaml:lower name
                                #:source source
                                #:inputs inputs
                                #:native-inputs native-inputs
                                #:outputs outputs
                                #:system system
                                #:target target
                                #:ocaml ocaml
                                #:findlib findlib
                                arguments)))
         (bag
           (inherit base)
           (build-inputs `(("dune" ,dune)
                           ,@(bag-build-inputs base)))
           (build dune-build)
           (arguments (strip-keyword-arguments private-keywords arguments))))))

(define* (dune-build store name inputs
                     #:key (guile #f)
                     (outputs '("out"))
                     (search-paths '())
                     (build-flags ''())
                     (out-of-source? #t)
                     (jbuild? #f)
                     (package #f)
                     (tests? #t)
                     (test-flags ''())
                     (test-target "test")
                     (install-target "install")
                     (validate-runpath? #t)
                     (patch-shebangs? #t)
                     (strip-binaries? #t)
                     (strip-flags ''("--strip-debug"))
                     (strip-directories ''("lib" "lib64" "libexec"
                                           "bin" "sbin"))
                     (phases '(@ (guix build dune-build-system)
                                 %standard-phases))
                     (system (%current-system))
                     (imported-modules %dune-build-system-modules)
                     (modules '((guix build dune-build-system)
                                (guix build utils))))
  "Build SOURCE using OCAML, and with INPUTS. This assumes that SOURCE
provides a 'setup.ml' file as its build system."
  (define builder
    `(begin
       (use-modules ,@modules)
       (dune-build #:source ,(match (assoc-ref inputs "source")
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
                   #:test-flags ,test-flags
                   #:build-flags ,build-flags
                   #:out-of-source? ,out-of-source?
                   #:jbuild? ,jbuild?
                   #:package ,package
                   #:tests? ,tests?
                   #:test-target ,test-target
                   #:install-target ,install-target
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

(define dune-build-system
  (build-system
    (name 'dune)
    (description "The standard Dune build system")
    (lower lower)))

;;; dune.scm ends here
