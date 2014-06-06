;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-39)
  #:use-module (ice-9 match)
  #:export (gnu-build
            gnu-build-system
            standard-search-paths
            standard-inputs
            package-with-explicit-inputs
            package-with-extra-configure-variable
            static-libgcc-package
            static-package
            dist-package))

;; Commentary:
;;
;; Standard build procedure for packages using the GNU Build System or
;; something compatible ("./configure && make && make install").
;;
;; Code:

(define %default-modules
  ;; Build-side modules imported and used by default.
  '((guix build gnu-build-system)
    (guix build utils)))

(define* (package-with-explicit-inputs p inputs
                                       #:optional
                                       (loc (current-source-location))
                                       #:key (native-inputs '())
                                       guile)
  "Rewrite P, which is assumed to use GNU-BUILD-SYSTEM, to take INPUTS and
NATIVE-INPUTS as explicit inputs instead of the implicit default, and return
it.  INPUTS and NATIVE-INPUTS can be either input lists or thunks; in the
latter case, they will be called in a context where the `%current-system' and
`%current-target-system' are suitably parametrized.  Use GUILE to run the
builder, or the distro's final Guile when GUILE is #f."
  (define inputs* inputs)
  (define native-inputs* native-inputs)

  (define (call inputs)
    (if (procedure? inputs)
        (inputs)
        inputs))

  (define (duplicate-filter inputs)
    (let ((names (match (call inputs)
                   (((name _ ...) ...)
                    name))))
      (lambda (inputs)
        (fold alist-delete inputs names))))

  (let loop ((p p))
    (define rewritten-input
      (memoize
       (match-lambda
        ((name (? package? p) sub-drv ...)
         ;; XXX: Check whether P's build system knows #:implicit-inputs, for
         ;; things like `cross-pkg-config'.
         (if (eq? (package-build-system p) gnu-build-system)
             (cons* name (loop p) sub-drv)
             (cons* name p sub-drv)))
        (x x))))

    (package (inherit p)
      (location (if (pair? loc) (source-properties->location loc) loc))
      (arguments
       (let ((args (package-arguments p)))
         `(#:guile ,guile
           #:implicit-inputs? #f
           ,@args)))
      (native-inputs
       (let ((filtered (duplicate-filter native-inputs*)))
        `(,@(call native-inputs*)
          ,@(map rewritten-input
                 (filtered (package-native-inputs p))))))
      (propagated-inputs
       (map rewritten-input
            (package-propagated-inputs p)))
      (inputs
       (let ((filtered (duplicate-filter inputs*)))
         `(,@(call inputs*)
           ,@(map rewritten-input
                  (filtered (package-inputs p)))))))))

(define (package-with-extra-configure-variable p variable value)
  "Return a version of P with VARIABLE=VALUE specified as an extra `configure'
flag, recursively.  An example is LDFLAGS=-static.  If P already has configure
flags for VARIABLE, the associated value is augmented."
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

(define* (static-package p #:optional (loc (current-source-location))
                         #:key (strip-all? #t))
  "Return a statically-linked version of package P.  If STRIP-ALL? is true,
use `--strip-all' as the arguments to `strip'."
  (package (inherit p)
    (location (source-properties->location loc))
    (arguments
     (let ((a (default-keyword-arguments (package-arguments p)
                '(#:configure-flags '()
                  #:strip-flags '("--strip-debug")))))
       (substitute-keyword-arguments a
         ((#:configure-flags flags)
          `(cons* "--disable-shared" "LDFLAGS=-static" ,flags))
         ((#:strip-flags flags)
          (if strip-all?
              ''("--strip-all")
              flags)))))))

(define* (dist-package p source)
  "Return a package that runs takes source files from the SOURCE directory,
runs `make distcheck' and whose result is one or more source tarballs."
  (let ((s source))
    (package (inherit p)
      (name (string-append (package-name p) "-dist"))
      (source s)
      (arguments
       ;; Use the right phases and modules.
       (let* ((args (default-keyword-arguments (package-arguments p)
                      `(#:phases #f
                        #:modules ,%default-modules
                        #:imported-modules ,%default-modules))))
         (substitute-keyword-arguments args
           ((#:modules modules)
            `((guix build gnu-dist)
              ,@modules))
           ((#:imported-modules modules)
            `((guix build gnu-dist)
              ,@modules))
           ((#:phases _)
            '%dist-phases))))
      (native-inputs
       ;; Add autotools & co. as inputs.
       (let ((ref (lambda (module var)
                    (module-ref (resolve-interface module) var))))
         `(("autoconf" ,(ref '(gnu packages autotools) 'autoconf))
           ("automake" ,(ref '(gnu packages autotools) 'automake))
           ("libtool"  ,(ref '(gnu packages autotools) 'libtool) "bin")
           ("gettext"  ,(ref '(gnu packages gettext) 'gnu-gettext))
           ("texinfo"  ,(ref '(gnu packages texinfo) 'texinfo))))))))


(define %store
  ;; Store passed to STANDARD-INPUTS.
  (make-parameter #f))

(define (standard-packages)
  "Return the list of (NAME PACKAGE OUTPUT) or (NAME PACKAGE) tuples of
standard packages used as implicit inputs of the GNU build system."

  ;; Resolve (gnu packages base) lazily to hide circular dependency.
  (let ((distro (resolve-module '(gnu packages base))))
    (module-ref distro '%final-inputs)))

(define* (inputs-search-paths inputs
                              #:optional (package->search-paths
                                          package-native-search-paths))
  "Return the <search-path-specification> objects for INPUTS, using
PACKAGE->SEARCH-PATHS to extract the search path specifications of a package."
  (append-map (match-lambda
               ((_ (? package? p) _ ...)
                (package->search-paths p))
               (_
                '()))
              inputs))

(define (standard-search-paths)
  "Return the list of <search-path-specification> for the standard (implicit)
inputs when doing a native build."
  (inputs-search-paths (standard-packages)))

(define (expand-inputs inputs system)
  "Expand INPUTS, which contains <package> objects, so that it contains only
derivations for SYSTEM.  Include propagated inputs in the result."
  (define input-package->derivation
    (match-lambda
     ((name pkg sub-drv ...)
      (cons* name (package-derivation (%store) pkg system) sub-drv))
     ((name (? derivation-path? path) sub-drv ...)
      (cons* name path sub-drv))
     (z
      (error "invalid standard input" z))))

  (map input-package->derivation
       (append inputs
               (append-map (match-lambda
                            ((name package _ ...)
                             (package-transitive-propagated-inputs package)))
                           inputs))))

(define standard-inputs
  (memoize
   (lambda (system)
     "Return the list of implicit standard inputs used with the GNU Build
System: GCC, GNU Make, Bash, Coreutils, etc."
     (expand-inputs (standard-packages) system))))

(define* (gnu-build store name source inputs
                    #:key (guile #f)
                    (outputs '("out"))
                    (search-paths '())
                    (configure-flags ''())
                    (make-flags ''())
                    (out-of-source? #f)
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
                    (imported-modules %default-modules)
                    (modules %default-modules)
                    allowed-references)
  "Return a derivation called NAME that builds from tarball SOURCE, with
input derivation INPUTS, using the usual procedure of the GNU Build
System.  The builder is run with GUILE, or with the distro's final Guile
package if GUILE is #f or omitted.

The builder is run in a context where MODULES are used; IMPORTED-MODULES
specifies modules not provided by Guile itself that must be imported in
the builder's environment, from the host.  Note that we distinguish
between both, because for Guile's own modules like (ice-9 foo), we want
to use GUILE's own version of it, rather than import the user's one,
which could lead to gratuitous input divergence.

ALLOWED-REFERENCES can be either #f, or a list of packages that the outputs
are allowed to refer to."
  (define implicit-inputs
    (and implicit-inputs?
         (parameterize ((%store store))
           (standard-inputs system))))

  (define implicit-search-paths
    (if implicit-inputs?
        (standard-search-paths)
        '()))

  (define canonicalize-reference
    (match-lambda
     ((? package? p)
      (derivation->output-path (package-derivation store p system)))
     (((? package? p) output)
      (derivation->output-path (package-derivation store p system)
                               output))
     ((? string? output)
      output)))

  (define builder
    `(begin
       (use-modules ,@modules)
       (gnu-build #:source ,(if (derivation? source)
                                (derivation->output-path source)
                                source)
                  #:system ,system
                  #:outputs %outputs
                  #:inputs %build-inputs
                  #:search-paths ',(map search-path-specification->sexp
                                        (append implicit-search-paths
                                                search-paths))
                  #:phases ,phases
                  #:configure-flags ,configure-flags
                  #:make-flags ,make-flags
                  #:out-of-source? ,out-of-source?
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
      ;; ((and (? string?) (? derivation-path?))
      ;;  guile)
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(gnu packages base)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system)))))

  (build-expression->derivation store name builder
                                #:system system
                                #:inputs
                                `(,@(if source
                                        `(("source" ,source))
                                        '())
                                  ,@inputs
                                  ,@(if implicit-inputs?
                                        implicit-inputs
                                        '()))
                                #:outputs (if strip-binaries?
                                              outputs
                                              (delete "debug" outputs))
                                #:modules imported-modules
                                #:allowed-references
                                (and allowed-references
                                     (map canonicalize-reference
                                          allowed-references))
                                #:guile-for-build guile-for-build))


;;;
;;; Cross-compilation.
;;;

(define standard-cross-packages
  (memoize
   (lambda (target kind)
     "Return the list of name/package tuples to cross-build for TARGET.  KIND
is one of `host' or `target'."
     (let* ((cross     (resolve-interface '(gnu packages cross-base)))
            (gcc       (module-ref cross 'cross-gcc))
            (binutils  (module-ref cross 'cross-binutils))
            (libc      (module-ref cross 'cross-libc)))
       (case kind
         ((host)
          `(("cross-gcc" ,(gcc target
                               (binutils target)
                               (libc target)))
            ("cross-binutils" ,(binutils target))
            ,@(standard-packages)))
         ((target)
          `(("cross-libc" ,(libc target)))))))))

(define standard-cross-inputs
  (memoize
   (lambda (system target kind)
     "Return the list of implicit standard inputs used with the GNU Build
System when cross-compiling for TARGET: GCC, GNU Make, Bash, Coreutils, etc."
     (expand-inputs (standard-cross-packages target kind) system))))

(define (standard-cross-search-paths target kind)
  "Return the list of <search-path-specification> for the standard (implicit)
inputs."
  (inputs-search-paths (append (standard-cross-packages target 'target)
                               (standard-cross-packages target 'host))
                       (case kind
                         ((host)   package-native-search-paths)
                         ((target) package-search-paths))))

(define* (gnu-cross-build store name target source inputs native-inputs
                          #:key
                          (guile #f)
                          (outputs '("out"))
                          (search-paths '())
                          (native-search-paths '())

                          (configure-flags ''())
                          (make-flags ''())
                          (out-of-source? #f)
                          (tests? #f)             ; nothing can be done
                          (test-target "check")
                          (parallel-build? #t) (parallel-tests? #t)
                          (patch-shebangs? #t)
                          (strip-binaries? #t)
                          (strip-flags ''("--strip-debug"))
                          (strip-directories ''("lib" "lib64" "libexec"
                                                "bin" "sbin"))
                          (phases '%standard-phases)
                          (system (%current-system))
                          (implicit-inputs? #t)
                          (imported-modules '((guix build gnu-build-system)
                                              (guix build utils)))
                          (modules '((guix build gnu-build-system)
                                     (guix build utils)))
                          allowed-references)
  "Cross-build NAME for TARGET, where TARGET is a GNU triplet.  INPUTS are
cross-built inputs, and NATIVE-INPUTS are inputs that run on the build
platform."

  (define implicit-host-inputs
    (and implicit-inputs?
         (parameterize ((%store store))
           (standard-cross-inputs system target 'host))))

  (define implicit-target-inputs
    (and implicit-inputs?
         (parameterize ((%store store))
           (standard-cross-inputs system target 'target))))

  (define implicit-host-search-paths
    (if implicit-inputs?
        (standard-cross-search-paths target 'host)
        '()))

  (define implicit-target-search-paths
    (if implicit-inputs?
        (standard-cross-search-paths target 'target)
        '()))

  (define canonicalize-reference
    (match-lambda
     ((? package? p)
      (derivation->output-path (package-cross-derivation store p system)))
     (((? package? p) output)
      (derivation->output-path (package-cross-derivation store p system)
                               output))
     ((? string? output)
      output)))

  (define builder
    `(begin
       (use-modules ,@modules)

       (let ()
         (define %build-host-inputs
           ',(map (match-lambda
                   ((name (? derivation? drv) sub ...)
                    `(,name . ,(apply derivation->output-path drv sub)))
                   ((name (? derivation-path? drv-path) sub ...)
                    `(,name . ,(apply derivation-path->output-path
                                      drv-path sub)))
                   ((name path)
                    `(,name . ,path)))
                  (append (or implicit-host-inputs '()) native-inputs)))

         (define %build-target-inputs
           ',(map (match-lambda
                   ((name (? derivation? drv) sub ...)
                    `(,name . ,(apply derivation->output-path drv sub)))
                   ((name (? derivation-path? drv-path) sub ...)
                    `(,name . ,(apply derivation-path->output-path
                                      drv-path sub)))
                   ((name path)
                    `(,name . ,path)))
                  (append (or implicit-target-inputs '()) inputs)))

         (gnu-build #:source ,(if (derivation? source)
                                  (derivation->output-path source)
                                  source)
                    #:system ,system
                    #:target ,target
                    #:outputs %outputs
                    #:inputs %build-target-inputs
                    #:native-inputs %build-host-inputs
                    #:search-paths ',(map search-path-specification->sexp
                                          (append implicit-target-search-paths
                                                  search-paths))
                    #:native-search-paths ',(map
                                             search-path-specification->sexp
                                             (append implicit-host-search-paths
                                                     native-search-paths))
                    #:phases ,phases
                    #:configure-flags ,configure-flags
                    #:make-flags ,make-flags
                    #:out-of-source? ,out-of-source?
                    #:tests? ,tests?
                    #:test-target ,test-target
                    #:parallel-build? ,parallel-build?
                    #:parallel-tests? ,parallel-tests?
                    #:patch-shebangs? ,patch-shebangs?
                    #:strip-binaries? ,strip-binaries?
                    #:strip-flags ,strip-flags
                    #:strip-directories ,strip-directories))))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system))
      ;; ((and (? string?) (? derivation-path?))
      ;;  guile)
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(gnu packages base)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system)))))

  (build-expression->derivation store name builder
                                #:system system
                                #:inputs
                                `(,@(if source
                                        `(("source" ,source))
                                        '())
                                  ,@inputs
                                  ,@(if implicit-inputs?
                                        implicit-target-inputs
                                        '())
                                  ,@native-inputs
                                  ,@(if implicit-inputs?
                                        implicit-host-inputs
                                        '()))
                                #:outputs (if strip-binaries?
                                              outputs
                                              (delete "debug" outputs))
                                #:modules imported-modules
                                #:allowed-references
                                (and allowed-references
                                     (map canonicalize-reference
                                          allowed-references))
                                #:guile-for-build guile-for-build))

(define gnu-build-system
  (build-system (name 'gnu)
                (description
                 "The GNU Build System—i.e., ./configure && make && make install")
                (build gnu-build)
                (cross-build gnu-cross-build)))
