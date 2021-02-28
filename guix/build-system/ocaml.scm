;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
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
(define-module (guix build-system ocaml)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%ocaml-build-system-modules
            package-with-ocaml4.07
            strip-ocaml4.07-variant
            package-with-ocaml4.09
            strip-ocaml4.09-variant
            default-findlib
            default-ocaml
            lower
            ocaml-build
            ocaml-build-system))

;; Commentary:
;;
;; Standard build procedure for packages using ocaml. This is implemented as an
;; extension of `gnu-build-system'.
;;
;; OCaml packages don't use a single standard for their build system. Some use
;; autotools, other use custom configure scripts with Makefiles, others use
;; oasis to generate the configure script and Makefile and lastly, some use
;; custom ocaml scripts.
;;
;; Each phase in the build system will try to figure out what the build system
;; is for that package. Most packages come with a custom configure script and
;; a Makefile that in turn call custom build tools. Packages built with oasis
;; will have a `setup.ml' file in the top directory, that can be used for all
;; phases. In that case the Makefile is here only to call that script. In case
;; the setup.ml do not work as expected, the @var{use-make} argument can be
;; used to ignore the setup.ml file and run make instead.
;;
;; Some packages use their own custom scripts, `pkg/pkg.ml' or
;; `pkg/build.ml'. They can be used here too.
;;
;; Code:

(define %ocaml-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build ocaml-build-system)
    ,@%gnu-build-system-modules))

(define (default-ocaml)
  "Return the default OCaml package."

  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages ocaml))))
    (module-ref module 'ocaml)))

(define (default-findlib)
  "Return the default OCaml-findlib package."

  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages ocaml))))
    (module-ref module 'ocaml-findlib)))

(define (default-dune-build-system)
  "Return the dune-build-system."

  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(guix build-system dune))))
    (module-ref module 'dune-build-system)))

(define (default-ocaml4.07)
  (let ((ocaml (resolve-interface '(gnu packages ocaml))))
    (module-ref ocaml 'ocaml-4.07)))

(define (default-ocaml4.07-findlib)
  (let ((module (resolve-interface '(gnu packages ocaml))))
    (module-ref module 'ocaml4.07-findlib)))

(define (default-ocaml4.07-dune)
  (let ((module (resolve-interface '(gnu packages ocaml))))
    (module-ref module 'ocaml4.07-dune)))

(define (default-ocaml4.09)
  (let ((ocaml (resolve-interface '(gnu packages ocaml))))
    (module-ref ocaml 'ocaml-4.09)))

(define (default-ocaml4.09-findlib)
  (let ((module (resolve-interface '(gnu packages ocaml))))
    (module-ref module 'ocaml4.09-findlib)))

(define (default-ocaml4.09-dune)
  (let ((module (resolve-interface '(gnu packages ocaml))))
    (module-ref module 'ocaml4.09-dune)))

(define* (package-with-explicit-ocaml ocaml findlib dune old-prefix new-prefix
                                       #:key variant-property)
  "Return a procedure of one argument, P.  The procedure creates a package
with the same fields as P, which is assumed to use OCAML-BUILD-SYSTEM, such
that it is compiled with OCAML and FINDLIB instead.  The inputs are changed
recursively accordingly.  If the name of P starts with OLD-PREFIX, this is
replaced by NEW-PREFIX; otherwise, NEW-PREFIX is prepended to the name.

When the package uses the DUNE-BUILD-SYSTEM, the procedure creates a package
with the same fields as P, such that it is compiled with OCAML, FINDLIB and DUNE
instead.

When VARIANT-PROPERTY is present, it is used as a key to search for
pre-defined variants of this transformation recorded in the 'properties' field
of packages.  The property value must be the promise of a package.  This is a
convenient way for package writers to force the transformation to use
pre-defined variants."
  (define package-variant
    (if variant-property
        (lambda (package)
          (assq-ref (package-properties package)
                    variant-property))
        (const #f)))

  (define (transform p)
    (cond
     ;; If VARIANT-PROPERTY is present, use that.
     ((package-variant p)
      => force)

     ;; Otherwise build the new package object graph.
     ((or (eq? (package-build-system p) ocaml-build-system)
          (eq? (package-build-system p) (default-dune-build-system)))
      (package
        (inherit p)
        (location (package-location p))
        (name (let ((name (package-name p)))
                (string-append new-prefix
                               (if (string-prefix? old-prefix name)
                                   (substring name
                                              (string-length old-prefix))
                                   name))))
        (arguments
         (let ((ocaml   (if (promise? ocaml) (force ocaml) ocaml))
               (findlib (if (promise? findlib) (force findlib) findlib))
               (dune (if (promise? dune) (force dune) dune)))
           (ensure-keyword-arguments (package-arguments p)
                                     `(#:ocaml   ,ocaml
                                       #:findlib ,findlib
                                       ,@(if (eq? (package-build-system p)
                                                  (default-dune-build-system))
                                             `(#:dune ,dune)
                                             '())))))))
     (else p)))

  (define (cut? p)
    (or (not (or (eq? (package-build-system p) ocaml-build-system)
                 (eq? (package-build-system p) (default-dune-build-system))))
        (package-variant p)))

  (package-mapping transform cut?))

(define package-with-ocaml4.07
  (package-with-explicit-ocaml (delay (default-ocaml4.07))
                               (delay (default-ocaml4.07-findlib))
                               (delay (default-ocaml4.07-dune))
                               "ocaml-" "ocaml4.07-"
                               #:variant-property 'ocaml4.07-variant))

(define (strip-ocaml4.07-variant p)
  "Remove the 'ocaml4.07-variant' property from P."
  (package
    (inherit p)
    (properties (alist-delete 'ocaml4.07-variant (package-properties p)))))

(define package-with-ocaml4.09
  (package-with-explicit-ocaml (delay (default-ocaml4.09))
                               (delay (default-ocaml4.09-findlib))
                               (delay (default-ocaml4.09-dune))
                               "ocaml-" "ocaml4.09-"
                               #:variant-property 'ocaml4.09-variant))

(define (strip-ocaml4.09-variant p)
  "Remove the 'ocaml4.09-variant' property from P."
  (package
    (inherit p)
    (properties (alist-delete 'ocaml4.09-variant (package-properties p)))))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (ocaml (default-ocaml))
                (findlib (default-findlib))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:ocaml #:findlib #:inputs #:native-inputs))

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
         (build-inputs `(("ocaml" ,ocaml)
                         ("findlib" ,findlib)
                         ,@native-inputs))
         (outputs outputs)
         (build ocaml-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (ocaml-build name inputs
                      #:key
                      guile source
                      (outputs '("out")) (configure-flags ''())
                      (search-paths '())
                      (make-flags ''())
                      (build-flags ''())
                      (out-of-source? #t)
                      (use-make? #f)
                      (tests? #t)
                      (test-flags ''("--enable-tests"))
                      (test-target "test")
                      (install-target "install")
                      (validate-runpath? #t)
                      (patch-shebangs? #t)
                      (strip-binaries? #t)
                      (strip-flags ''("--strip-debug"))
                      (strip-directories ''("lib" "lib64" "libexec"
                                            "bin" "sbin"))
                      (phases '(@ (guix build ocaml-build-system)
                                  %standard-phases))
                      (system (%current-system))
                      (imported-modules %ocaml-build-system-modules)
                      (modules '((guix build ocaml-build-system)
                                 (guix build utils))))
  "Build SOURCE using OCAML, and with INPUTS. This assumes that SOURCE
provides a 'setup.ml' file as its build system."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@modules)
          (ocaml-build #:source #$source
                       #:system #$system
                       #:outputs #$(outputs->gexp outputs)
                       #:inputs #$(input-tuples->gexp inputs)
                       #:search-paths '#$(map search-path-specification->sexp
                                              search-paths)
                       #:phases #$phases
                       #:configure-flags #$configure-flags
                       #:test-flags #$test-flags
                       #:make-flags #$make-flags
                       #:build-flags #$build-flags
                       #:out-of-source? #$out-of-source?
                       #:use-make? #$use-make?
                       #:tests? #$tests?
                       #:test-target #$test-target
                       #:install-target #$install-target
                       #:validate-runpath? #$validate-runpath?
                       #:patch-shebangs? #$patch-shebangs?
                       #:strip-binaries? #$strip-binaries?
                       #:strip-flags #$strip-flags
                       #:strip-directories #$strip-directories))))

  (gexp->derivation name builder
                    #:system system
                    #:target #f
                    #:guile-for-build guile))

(define ocaml-build-system
  (build-system
    (name 'ocaml)
    (description "The standard OCaml build system")
    (lower lower)))

;;; ocaml.scm ends here
