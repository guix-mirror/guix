;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Julien Lepiller <julien@lepiller.eu>
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
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export (%ocaml-build-system-modules
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

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (ocaml (default-ocaml))
                (findlib (default-findlib))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:source #:target #:ocaml #:findlib #:inputs #:native-inputs))

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

(define* (ocaml-build store name inputs
                      #:key (guile #f)
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
    `(begin
       (use-modules ,@modules)
       (ocaml-build #:source ,(match (assoc-ref inputs "source")
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
                    #:configure-flags ,configure-flags
                    #:test-flags ,test-flags
                    #:make-flags ,make-flags
                    #:build-flags ,build-flags
                    #:out-of-source? ,out-of-source?
                    #:use-make? ,use-make?
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

(define ocaml-build-system
  (build-system
    (name 'ocaml)
    (description "The standard OCaml build system")
    (lower lower)))

;;; ocaml.scm ends here
