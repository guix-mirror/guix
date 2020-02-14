;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
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

(define-module (guix build-system copy)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%copy-build-system-modules
            default-glibc
            lower
            copy-build
            copy-build-system))

;; Commentary:
;;
;; Standard build procedure for simple packages that don't require much
;; compilation, mostly just copying files around.  This is implemented as an
;; extension of `gnu-build-system'.
;;
;; Code:

(define %copy-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build copy-build-system)
    ,@%gnu-build-system-modules))

(define (default-glibc)
  "Return the default glibc package."
  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages base))))
    (module-ref module 'glibc)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (glibc (default-glibc))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME from the given arguments."
  (define private-keywords
    '(#:source #:target #:inputs #:native-inputs))

  (bag
    (name name)
    (system system)
    (host-inputs `(,@(if source
                         `(("source" ,source))
                         '())
                   ,@inputs
                   ;; Keep the standard inputs of 'gnu-build-system'.
                   ,@(standard-packages)))
    (build-inputs native-inputs)
    (outputs outputs)
    (build copy-build)
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (copy-build store name inputs
                     #:key (guile #f)
                     (outputs '("out"))
                     (install-plan ''(("." (".") "./")))
                     (search-paths '())
                     (out-of-source? #t)
                     (validate-runpath? #t)
                     (patch-shebangs? #t)
                     (strip-binaries? #t)
                     (strip-flags ''("--strip-debug"))
                     (strip-directories ''("lib" "lib64" "libexec"
                                           "bin" "sbin"))
                     (phases '(@ (guix build copy-build-system)
                                 %standard-phases))
                     (system (%current-system))
                     (imported-modules %copy-build-system-modules)
                     (modules '((guix build copy-build-system)
                                (guix build utils))))
  "Build SOURCE using INSTALL-PLAN, and with INPUTS."
  (define builder
    `(begin
       (use-modules ,@modules)
       (copy-build #:source ,(match (assoc-ref inputs "source")
                               (((?  derivation? source))
                                (derivation->output-path source))
                               ((source)
                                source)
                               (source
                                source))
                   #:system ,system
                   #:outputs %outputs
                   #:inputs %build-inputs
                   #:install-plan ,install-plan
                   #:search-paths ',(map search-path-specification->sexp
                                         search-paths)
                   #:phases ,phases
                   #:out-of-source? ,out-of-source?
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

(define copy-build-system
  (build-system
    (name 'copy)
    (description "The standard copy build system")
    (lower lower)))

;;; copy.scm ends here
