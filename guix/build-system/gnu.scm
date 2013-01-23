;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build-system gnu)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-39)
  #:use-module (ice-9 match)
  #:export (gnu-build
            gnu-build-system
            package-with-explicit-inputs
            package-with-extra-configure-variable
            static-libgcc-package
            static-package))

;; Commentary:
;;
;; Standard build procedure for packages using the GNU Build System or
;; something compatible ("./configure && make && make install").
;;
;; Code:

(define* (package-with-explicit-inputs p boot-inputs
                                       #:optional
                                       (loc (current-source-location))
                                       #:key guile)
  "Rewrite P, which is assumed to use GNU-BUILD-SYSTEM, to take
BOOT-INPUTS as explicit inputs instead of the implicit default, and
return it.  Use GUILE to run the builder, or the distro's final Guile
when GUILE is #f."
  (define rewritten-input
    (match-lambda
     ((name (? package? p) sub-drv ...)
      (cons* name
             (package-with-explicit-inputs p boot-inputs #:guile guile)
             sub-drv))
     (x x)))

  (define boot-input-names
    (map car boot-inputs))

  (define (filtered-inputs inputs)
    (fold alist-delete inputs boot-input-names))

  (package (inherit p)
    (location (if (pair? loc) (source-properties->location loc) loc))
    (arguments
     (let ((args (package-arguments p)))
       `(#:guile ,guile
         #:implicit-inputs? #f ,@args)))
    (native-inputs (map rewritten-input
                        (filtered-inputs (package-native-inputs p))))
    (propagated-inputs (map rewritten-input
                            (filtered-inputs
                             (package-propagated-inputs p))))
    (inputs `(,@boot-inputs
              ,@(map rewritten-input
                     (filtered-inputs (package-inputs p)))))))

(define (package-with-extra-configure-variable p variable value)
  "Return a version of P with VARIABLE=VALUE specified as an extra
`configure' flag.  An example is LDFLAGS=-static.  If P already has
configure flags for VARIABLE, the associated value is augmented."
  (let loop ((p p))
    (define (rewritten-inputs inputs)
      (map (match-lambda
            ((name (? package? p) sub ...)
             `(,name ,(loop p) ,@sub))
            (input input))
           inputs))

    (package (inherit p)
      (arguments
       (let ((args (package-arguments p)))
         (substitute-keyword-arguments args
           ((#:configure-flags flags)
            (let* ((var= (string-append variable "="))
                   (len  (string-length var=)))
              `(cons ,(string-append var= value)
                     (map (lambda (flag)
                            (if (string-prefix? ,var= flag)
                                (string-append
                                 ,(string-append var= value " ")
                                 (substring flag ,len))
                                flag))
                          ,flags)))))))
      (inputs (rewritten-inputs (package-inputs p)))
      (propagated-inputs (rewritten-inputs (package-propagated-inputs p))))))

(define (static-libgcc-package p)
  "A version of P linked with `-static-gcc'."
  (package-with-extra-configure-variable p "LDFLAGS" "-static-libgcc"))

(define* (static-package p #:optional (loc (current-source-location)))
  "Return a statically-linked version of package P."
  (let ((args (package-arguments p)))
    (package (inherit p)
      (location (source-properties->location loc))
      (arguments
       (let ((a (default-keyword-arguments args
                  '(#:configure-flags '()
                    #:strip-flags #f))))
         (substitute-keyword-arguments a
           ((#:configure-flags flags)
            `(cons* "--disable-shared" "LDFLAGS=-static" ,flags))
           ((#:strip-flags _)
            ''("--strip-all"))))))))


(define %store
  ;; Store passed to STANDARD-INPUTS.
  (make-parameter #f))

(define standard-inputs
  (memoize
   (lambda (system)
     "Return the list of implicit standard inputs used with the GNU Build
System: GCC, GNU Make, Bash, Coreutils, etc."
     (map (match-lambda
           ((name pkg sub-drv ...)
            (cons* name (package-derivation (%store) pkg system) sub-drv))
           ((name (? derivation-path? path) sub-drv ...)
            (cons* name path sub-drv))
           (z
            (error "invalid standard input" z)))

          ;; Resolve (gnu packages base) lazily to hide circular dependency.
          (let* ((distro (resolve-module '(gnu packages base)))
                 (inputs (module-ref distro '%final-inputs)))
            (append inputs
                    (append-map (match-lambda
                                 ((name package _ ...)
                                  (package-transitive-propagated-inputs package)))
                                inputs)))))))

(define* (gnu-build store name source inputs
                    #:key (guile #f)
                    (outputs '("out")) (configure-flags ''())
                    (make-flags ''())
                    (patches ''()) (patch-flags ''("--batch" "-p1"))
                    (out-of-source? #f)
                    (path-exclusions ''())
                    (tests? #t)
                    (test-target "check")
                    (parallel-build? #t) (parallel-tests? #t)
                    (patch-shebangs? #t)
                    (strip-binaries? #t)
                    (strip-flags ''("--strip-debug"))
                    (strip-directories ''("lib" "lib64" "libexec"
                                          "bin" "sbin"))
                    (phases '%standard-phases)
                    (system (%current-system))
                    (implicit-inputs? #t)    ; useful when bootstrapping
                    (imported-modules '((guix build gnu-build-system)
                                        (guix build utils)))
                    (modules '((guix build gnu-build-system)
                               (guix build utils))))
  "Return a derivation called NAME that builds from tarball SOURCE, with
input derivation INPUTS, using the usual procedure of the GNU Build
System.  The builder is run with GUILE, or with the distro's final Guile
package if GUILE is #f or omitted.

The builder is run in a context where MODULES are used; IMPORTED-MODULES
specifies modules not provided by Guile itself that must be imported in
the builder's environment, from the host.  Note that we distinguish
between both, because for Guile's own modules like (ice-9 foo), we want
to use GUILE's own version of it, rather than import the user's one,
which could lead to gratuitous input divergence."
  (define builder
    `(begin
       (use-modules ,@modules)
       (gnu-build #:source ,(if (and source (derivation-path? source))
                                (derivation-path->output-path source)
                                source)
                  #:system ,system
                  #:outputs %outputs
                  #:inputs %build-inputs
                  #:patches ,patches
                  #:patch-flags ,patch-flags
                  #:phases ,phases
                  #:configure-flags ,configure-flags
                  #:make-flags ,make-flags
                  #:out-of-source? ,out-of-source?
                  #:path-exclusions ,path-exclusions
                  #:tests? ,tests?
                  #:test-target ,test-target
                  #:parallel-build? ,parallel-build?
                  #:parallel-tests? ,parallel-tests?
                  #:patch-shebangs? ,patch-shebangs?
                  #:strip-binaries? ,strip-binaries?
                  #:strip-flags ,strip-flags
                  #:strip-directories ,strip-directories)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system))
      ((and (? string?) (? derivation-path?))
       guile)
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(gnu packages base)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system)))))

  (build-expression->derivation store name system
                                builder
                                `(,@(if source
                                        `(("source" ,source))
                                        '())
                                  ,@inputs
                                  ,@(if implicit-inputs?
                                        (parameterize ((%store store))
                                          (standard-inputs system))
                                        '()))
                                #:outputs outputs
                                #:modules imported-modules
                                #:guile-for-build guile-for-build))

(define gnu-build-system
  (build-system (name 'gnu)
                (description
                 "The GNU Build System—i.e., ./configure && make && make install")
                (build gnu-build)))             ; TODO: add `gnu-cross-build'
