;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018-2019, 2021-2022 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build-system guile)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (%guile-build-system-modules
            guile-build-system))

(define %scheme-file-regexp
  ;; Regexp to match Scheme files.
  "\\.(scm|sls)$")

(define %guile-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build guile-build-system)
    ,@%gnu-build-system-modules))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (implicit-inputs? #t)
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."

  ;; Note: There's no #:guile argument (unlike, for instance,
  ;; 'ocaml-build-system' which has #:ocaml.)  This is so we can keep
  ;; procedures like 'package-for-guile-2.0' unchanged and simple.

  (define private-keywords
    '(#:target #:inputs #:native-inputs
      #:implicit-inputs?))

  (bag
    (name name)
    (system system) (target target)
    (host-inputs `(
                   ,@inputs))
    (build-inputs `(,@(if source
                          `(("source" ,source))
                          '())
                    ,@native-inputs
                    ,@(if implicit-inputs?
                          (map (cute assoc <> (standard-packages))
                               '("tar" "gzip" "bzip2" "xz" "locales"))
                          '())))
    (outputs outputs)
    (build (if target guile-cross-build guile-build))
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define %compile-flags
  ;; Flags passed to 'guild compile' by default.  We choose a common
  ;; denominator between Guile 2.0 and 2.2.
  ''("-Wunbound-variable" "-Warity-mismatch" "-Wformat"))

(define* (guile-build name inputs
                      #:key source
                      (guile #f)
                      (phases '%standard-phases)
                      (outputs '("out"))
                      (search-paths '())
                      (system (%current-system))
                      (source-directory ".")
                      not-compiled-file-regexp
                      (scheme-file-regexp %scheme-file-regexp)
                      (compile-flags %compile-flags)
                      (imported-modules %guile-build-system-modules)
                      (modules '((guix build guile-build-system)
                                 (guix build utils))))
  "Build SOURCE using Guile taken from the native inputs, and with INPUTS."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@modules)

          (guile-build #:name #$name
                       #:source #+source
                       #:source-directory #$source-directory
                       #:scheme-file-regexp #$scheme-file-regexp
                       #:not-compiled-file-regexp #$not-compiled-file-regexp
                       #:compile-flags #$compile-flags
                       #:phases #$phases
                       #:system #$system
                       #:outputs #$(outputs->gexp outputs)
                       #:inputs #$(input-tuples->gexp inputs)
                       #:search-paths '#$(map search-path-specification->sexp
                                              search-paths)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:target #f
                      #:guile-for-build guile)))

(define* (guile-cross-build name
                            #:key
                            (system (%current-system)) target
                            build-inputs target-inputs host-inputs
                            (guile #f)
                            source
                            (outputs '("out"))
                            (search-paths '())
                            (native-search-paths '())

                            (phases '%standard-phases)
                            (source-directory ".")
                            not-compiled-file-regexp
                            (compile-flags %compile-flags)
                            (imported-modules %guile-build-system-modules)
                            (modules '((guix build guile-build-system)
                                       (guix build utils))))
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@modules)

          (define %build-host-inputs
            #+(input-tuples->gexp build-inputs))

          (define %build-target-inputs
            (append #$(input-tuples->gexp host-inputs)
                    #+(input-tuples->gexp target-inputs)))

          (define %outputs
            #$(outputs->gexp outputs))

          (guile-build #:source #+source
                       #:system #$system
                       #:target #$target
                       #:outputs %outputs
                       #:source-directory #$source-directory
                       #:not-compiled-file-regexp #$not-compiled-file-regexp
                       #:compile-flags #$compile-flags
                       #:inputs %build-target-inputs
                       #:native-inputs %build-host-inputs
                       #:search-paths '#$(map search-path-specification->sexp
                                              search-paths)
                       #:native-search-paths '#$(map
                                                 search-path-specification->sexp
                                                 native-search-paths)
                       #:make-dynamic-linker-cache? #f ;cross-compiling
                       #:phases #$phases))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:target target
                      #:guile-for-build guile)))

(define guile-build-system
  (build-system
    (name 'guile)
    (description "The build system for simple Guile packages")
    (lower lower)))
