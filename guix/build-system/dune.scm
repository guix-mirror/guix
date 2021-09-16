;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 pukkamustard <pukkamustard@posteo.net>
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
  #:use-module (guix gexp)
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

  ;; Flags that put dune into reproducible build mode.
  (define dune-release-flags
    (if (version>=? (package-version dune) "2.5.0")
        ;; For dune >= 2.5.0 this is just --release.
        ''("--release")
        ;; --release does not exist before 2.5.0.  Replace with flags compatible
        ;; with our old ocaml4.07-dune (1.11.3)
        ''("--root" "." "--ignore-promoted-rules" "--no-config"
           "--profile" "release")))

  (define private-keywords
    '(#:target #:dune #:findlib #:ocaml #:inputs #:native-inputs))

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
           (arguments (append
                       `(#:dune-release-flags ,dune-release-flags)
                       (strip-keyword-arguments private-keywords arguments)))))))

(define* (dune-build name inputs
                     #:key
                     guile source
                     (outputs '("out"))
                     (search-paths '())
                     (build-flags ''())
                     (out-of-source? #t)
                     (jbuild? #f)
                     (package #f)
                     (dune-release-flags ''())
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
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@modules)
          (dune-build #:source #$source
                      #:system #$system
                      #:outputs (list #$@(map (lambda (name)
                                                #~(cons #$name
                                                        (ungexp output name)))
                                              outputs))
                      #:inputs (map (lambda (tuple)
                                      (apply cons tuple))
                                    '#$inputs)
                      #:search-paths '#$(map search-path-specification->sexp
                                             search-paths)
                      #:phases #$phases
                      #:test-flags #$test-flags
                      #:build-flags #$build-flags
                      #:out-of-source? #$out-of-source?
                      #:jbuild? #$jbuild?
                      #:package #$package
                      #:dune-release-flags #$dune-release-flags
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

(define dune-build-system
  (build-system
    (name 'dune)
    (description "The standard Dune build system")
    (lower lower)))

;;; dune.scm ends here
