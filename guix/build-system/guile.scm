;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix derivations)
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

(define* (guile-build store name inputs
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
    `(begin
       (use-modules ,@modules)
       (guile-build #:name ,name
                    #:source ,(match (assoc-ref inputs "source")
                                (((? derivation? source))
                                 (derivation->output-path source))
                                ((source)
                                 source)
                                (source
                                 source))
                    #:source-directory ,source-directory
                    #:scheme-file-regexp ,scheme-file-regexp
                    #:not-compiled-file-regexp ,not-compiled-file-regexp
                    #:compile-flags ,compile-flags
                    #:phases ,phases
                    #:system ,system
                    #:outputs %outputs
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

(define* (guile-cross-build store name
                            #:key
                            (system (%current-system)) target
                            native-drvs target-drvs
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
    `(begin
       (use-modules ,@modules)

       (let ()
         (define %build-host-inputs
           ',(map (match-lambda
                    ((name (? derivation? drv) sub ...)
                     `(,name . ,(apply derivation->output-path drv sub)))
                    ((name path)
                     `(,name . ,path)))
                  native-drvs))

         (define %build-target-inputs
           ',(map (match-lambda
                    ((name (? derivation? drv) sub ...)
                     `(,name . ,(apply derivation->output-path drv sub)))
                    ((name (? package? pkg) sub ...)
                     (let ((drv (package-cross-derivation store pkg
                                                          target system)))
                       `(,name . ,(apply derivation->output-path drv sub))))
                    ((name path)
                     `(,name . ,path)))
                  target-drvs))

         (guile-build #:source ,(match (assoc-ref native-drvs "source")
                                  (((? derivation? source))
                                   (derivation->output-path source))
                                  ((source)
                                   source)
                                  (source
                                   source))
                      #:system ,system
                      #:target ,target
                      #:outputs %outputs
                      #:source-directory ,source-directory
                      #:not-compiled-file-regexp ,not-compiled-file-regexp
                      #:compile-flags ,compile-flags
                      #:inputs %build-target-inputs
                      #:native-inputs %build-host-inputs
                      #:search-paths ',(map search-path-specification->sexp
                                            search-paths)
                      #:native-search-paths ',(map
                                               search-path-specification->sexp
                                               native-search-paths)
                      #:phases ,phases))))

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
                                #:inputs (append native-drvs target-drvs)
                                #:outputs outputs
                                #:modules imported-modules
                                #:substitutable? substitutable?
                                #:guile-for-build guile-for-build))

(define guile-build-system
  (build-system
    (name 'guile)
    (description "The build system for simple Guile packages")
    (lower lower)))
