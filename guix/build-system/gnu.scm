;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix memoization)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (%gnu-build-system-modules
            gnu-build
            gnu-build-system
            standard-packages
            standard-cross-packages
            package-with-explicit-inputs
            package-with-extra-configure-variable
            static-libgcc-package
            static-package
            dist-package
            package-with-restricted-references))

;; Commentary:
;;
;; Standard build procedure for packages using the GNU Build System or
;; something compatible ("./configure && make && make install").
;;
;; Code:

(define %gnu-build-system-modules
  ;; Build-side modules imported and used by default.
  '((guix build gnu-build-system)
    (guix build utils)
    (guix build gremlin)
    (guix elf)))

(define %default-modules
  ;; Modules in scope in the build-side environment.
  '((guix build gnu-build-system)
    (guix build utils)))

(define* (package-with-explicit-inputs/deprecated p inputs
                                                  #:optional
                                                  (loc (current-source-location))
                                                  #:key (native-inputs '())
                                                  guile)
  "This variant is deprecated because it is inefficient: it memoizes only
temporarily instead of memoizing across all transformations where INPUTS is
the same.

Rewrite P, which is assumed to use GNU-BUILD-SYSTEM, to take INPUTS and
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
      (mlambda (input)
        (match input
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
       ;; 'ensure-keyword-arguments' guarantees that this procedure is
       ;; idempotent.
       (ensure-keyword-arguments (package-arguments p)
                                 `(#:guile ,guile
                                   #:implicit-inputs? #f)))
      (replacement
       (let ((replacement (package-replacement p)))
         (and replacement
              (package-with-explicit-inputs replacement inputs loc
                                            #:native-inputs
                                            native-inputs
                                            #:guile guile))))
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

(define* (package-with-explicit-inputs* inputs #:optional guile)
  "Return a procedure that rewrites the given package and all its dependencies
so that they use INPUTS (a thunk) instead of implicit inputs."
  (define (duplicate-filter package-inputs)
    (let ((names (match (inputs)
                   (((name _ ...) ...)
                    name))))
      (fold alist-delete package-inputs names)))

  (define (add-explicit-inputs p)
    (if (and (eq? (package-build-system p) gnu-build-system)
             (not (memq #:implicit-inputs? (package-arguments p))))
        (package
          (inherit p)
          (inputs (append (inputs)
                          (duplicate-filter (package-inputs p))))
          (arguments
           (ensure-keyword-arguments (package-arguments p)
                                     `(#:implicit-inputs? #f
                                       #:guile ,guile))))
        p))

  (define (cut? p)
    (or (not (eq? (package-build-system p) gnu-build-system))
        (memq #:implicit-inputs? (package-arguments p))))

  (package-mapping add-explicit-inputs cut?))

(define package-with-explicit-inputs
  (case-lambda*
   ((inputs #:optional guile)
    (package-with-explicit-inputs* inputs guile))
   ((p inputs #:optional (loc (current-source-location))
       #:key (native-inputs '()) guile)
    ;; deprecated
    (package-with-explicit-inputs/deprecated p inputs
                                             loc
                                             #:native-inputs
                                             native-inputs
                                             #:guile guile))))

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
      (replacement
       (let ((replacement (package-replacement p)))
         (and replacement
              (package-with-extra-configure-variable replacement
                                                     variable value))))
      (inputs (rewritten-inputs (package-inputs p)))
      (propagated-inputs (rewritten-inputs (package-propagated-inputs p))))))

(define (static-libgcc-package p)
  "A version of P linked with `-static-gcc'."
  (package-with-extra-configure-variable p "LDFLAGS" "-static-libgcc"))

(define* (static-package p #:key (strip-all? #t))
  "Return a statically-linked version of package P.  If STRIP-ALL? is true,
use `--strip-all' as the arguments to `strip'."
  (package (inherit p)
    (arguments
     (let ((a (default-keyword-arguments (package-arguments p)
                '(#:configure-flags '()
                  #:strip-flags '("--strip-unneeded")))))
       (substitute-keyword-arguments a
         ((#:configure-flags flags)
          `(cons* "--disable-shared" "LDFLAGS=-static" ,flags))
         ((#:strip-flags flags)
          (if strip-all?
              ''("--strip-all")
              flags)))))
    (replacement (and=> (package-replacement p) static-package))))

(define* (dist-package p source #:key (phases '%dist-phases))
  "Return a package that runs takes source files from the SOURCE directory,
runs `make distcheck' and whose result is one or more source tarballs.  The
exact build phases are defined by PHASES."
  (let ((s source))
    (package (inherit p)
      (name (string-append (package-name p) "-dist"))
      (source s)
      (arguments
       ;; Use the right phases and modules.
       (substitute-keyword-arguments (package-arguments p)
         ((#:modules modules %default-modules)
          `((guix build gnu-dist)
            ,@modules))
         ((#:imported-modules modules %gnu-build-system-modules)
          `((guix build gnu-dist)
            ,@modules))
         ((#:phases _ #f)
          phases)))
      (native-inputs
       ;; Add autotools & co. as inputs.
       (let ((ref (lambda (module var)
                    (module-ref (resolve-interface module) var))))
         `(,@(package-native-inputs p)
           ("autoconf" ,(ref '(gnu packages autotools) 'autoconf-wrapper))
           ("automake" ,(ref '(gnu packages autotools) 'automake))
           ("libtool"  ,(ref '(gnu packages autotools) 'libtool))
           ("gettext"  ,(ref '(gnu packages gettext) 'gnu-gettext))
           ("texinfo"  ,(ref '(gnu packages texinfo) 'texinfo))))))))

(define (package-with-restricted-references p refs)
  "Return a package whose outputs are guaranteed to only refer to the packages
listed in REFS."
  (if (eq? (package-build-system p) gnu-build-system) ; XXX: dirty
      (package (inherit p)
        (arguments `(#:allowed-references ,refs
                     ,@(package-arguments p))))
      p))


(define (standard-packages)
  "Return the list of (NAME PACKAGE OUTPUT) or (NAME PACKAGE) tuples of
standard packages used as implicit inputs of the GNU build system."

  ;; Resolve (gnu packages commencement) lazily to hide circular dependency.
  (let ((distro (resolve-module '(gnu packages commencement))))
    (module-ref distro '%final-inputs)))

(define* (lower name
                #:key source inputs native-inputs outputs target
                (implicit-inputs? #t) (implicit-cross-inputs? #t)
                (strip-binaries? #t) system
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME from the given arguments."
  (define private-keywords
    `(#:inputs #:native-inputs #:outputs
      #:implicit-inputs? #:implicit-cross-inputs?
      ,@(if target '() '(#:target))))

  (bag
    (name name)
    (system system) (target target)
    (build-inputs `(,@(if source
                          `(("source" ,source))
                          '())
                    ,@native-inputs

                    ;; When not cross-compiling, ensure implicit inputs come
                    ;; last.  That way, libc headers come last, which allows
                    ;; #include_next to work correctly; see
                    ;; <https://bugs.gnu.org/30756>.
                    ,@(if target '() inputs)
                    ,@(if (and target implicit-cross-inputs?)
                          (standard-cross-packages target 'host)
                          '())
                    ,@(if implicit-inputs?
                          (standard-packages)
                          '())))
    (host-inputs (if target inputs '()))

    ;; The cross-libc is really a target package, but for bootstrapping
    ;; reasons, we can't put it in 'host-inputs'.  Namely, 'cross-gcc' is a
    ;; native package, so it would end up using a "native" variant of
    ;; 'cross-libc' (built with 'gnu-build'), whereas all the other packages
    ;; would use a target variant (built with 'gnu-cross-build'.)
    (target-inputs (if (and target implicit-cross-inputs?)
                       (standard-cross-packages target 'target)
                       '()))
    (outputs (if strip-binaries?
                 outputs
                 (delete "debug" outputs)))
    (build (if target gnu-cross-build gnu-build))
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define %license-file-regexp
  ;; Regexp matching license files.
  "^(COPYING.*|LICEN[CS]E.*|[Ll]icen[cs]e.*|Copy[Rr]ight(\\.(txt|md))?)$")

(define %bootstrap-scripts
  ;; Typical names of Autotools "bootstrap" scripts.
  #~%bootstrap-scripts)

(define %strip-flags
  #~'("--strip-unneeded" "--enable-deterministic-archives"))

(define %strip-directories
  #~'("lib" "lib64" "libexec" "bin" "sbin"))

(define* (gnu-build name inputs
                    #:key
                    guile source
                    (outputs '("out"))
                    (search-paths '())
                    (bootstrap-scripts %bootstrap-scripts)
                    (configure-flags ''())
                    (make-flags ''())
                    (out-of-source? #f)
                    (tests? #t)
                    (test-target "check")
                    (parallel-build? #t)
                    (parallel-tests? #t)
                    (patch-shebangs? #t)
                    (strip-binaries? #t)
                    (strip-flags %strip-flags)
                    (strip-directories %strip-directories)
                    (validate-runpath? #t)
                    (make-dynamic-linker-cache? #t)
                    (license-file-regexp %license-file-regexp)
                    (phases '%standard-phases)
                    (locale "en_US.utf8")
                    (system (%current-system))
                    (build (nix-system->gnu-triplet system))
                    (imported-modules %gnu-build-system-modules)
                    (modules %default-modules)
                    (substitutable? #t)
                    allowed-references
                    disallowed-references)
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

SUBSTITUTABLE? determines whether users may be able to use substitutes of the
returned derivations, or whether they should always build it locally.

ALLOWED-REFERENCES can be either #f, or a list of packages that the outputs
are allowed to refer to."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          #$(with-build-variables inputs outputs
              #~(gnu-build #:source #+source
                           #:system #$system
                           #:build #$build
                           #:outputs %outputs
                           #:inputs %build-inputs
                           #:search-paths '#$(sexp->gexp
                                              (map search-path-specification->sexp
                                                   search-paths))
                           #:phases #$(if (pair? phases)
                                          (sexp->gexp phases)
                                          phases)
                           #:locale #$locale
                           #:bootstrap-scripts #$bootstrap-scripts
                           #:configure-flags #$(if (pair? configure-flags)
                                                   (sexp->gexp configure-flags)
                                                   configure-flags)
                           #:make-flags #$(if (pair? make-flags)
                                              (sexp->gexp make-flags)
                                              make-flags)
                           #:out-of-source? #$out-of-source?
                           #:tests? #$tests?
                           #:test-target #$test-target
                           #:parallel-build? #$parallel-build?
                           #:parallel-tests? #$parallel-tests?
                           #:patch-shebangs? #$patch-shebangs?
                           #:license-file-regexp #$license-file-regexp
                           #:strip-binaries? #$strip-binaries?
                           #:validate-runpath? #$validate-runpath?
                           #:make-dynamic-linker-cache? #$make-dynamic-linker-cache?
                           #:license-file-regexp #$license-file-regexp
                           #:strip-flags #$strip-flags
                           #:strip-directories #$strip-directories)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    ;; Note: Always pass #:graft? #f.  Without it, ALLOWED-REFERENCES &
    ;; co. would be interpreted as referring to grafted packages.
    (gexp->derivation name builder
                      #:system system
                      #:target #f
                      #:graft? #f
                      #:substitutable? substitutable?
                      #:allowed-references allowed-references
                      #:disallowed-references disallowed-references
                      #:guile-for-build guile)))


;;;
;;; Cross-compilation.
;;;

(define standard-cross-packages
  (mlambda (target kind)
    "Return the list of name/package tuples to cross-build for TARGET.  KIND
is one of `host' or `target'."
    (let* ((cross     (resolve-interface '(gnu packages cross-base)))
           (gcc       (module-ref cross 'cross-gcc))
           (binutils  (module-ref cross 'cross-binutils))
           (libc      (module-ref cross 'cross-libc)))
      (case kind
        ((host)
         ;; Cross-GCC appears once here, so that it's in $PATH...
         `(("cross-gcc" ,(gcc target
                              #:xbinutils (binutils target)
                              #:libc (libc target)))
           ("cross-binutils" ,(binutils target))))
        ((target)
         (let ((libc (libc target)))
           ;; ... and once here, so that libstdc++ & co. are in
           ;; CROSS_CPLUS_INCLUDE_PATH, etc.
           `(("cross-gcc" ,(gcc target
                                #:xbinutils (binutils target)
                                #:libc libc))
             ("cross-libc" ,libc)

             ;; MinGW's libc doesn't have a "static" output.
             ,@(if (member "static" (package-outputs libc))
                   `(("cross-libc:static" ,libc "static"))
                   '()))))))))

(define* (gnu-cross-build name
                          #:key
                          target
                          build-inputs target-inputs host-inputs
                          guile source
                          (outputs '("out"))
                          (search-paths '())
                          (native-search-paths '())

                          (bootstrap-scripts %bootstrap-scripts)
                          (configure-flags ''())
                          (make-flags ''())
                          (out-of-source? #f)
                          (tests? #f)             ; nothing can be done
                          (test-target "check")
                          (parallel-build? #t) (parallel-tests? #t)
                          (patch-shebangs? #t)
                          (strip-binaries? #t)
                          (strip-flags %strip-flags)
                          (strip-directories %strip-directories)
                          (validate-runpath? #t)

                          ;; We run 'ldconfig' to generate ld.so.cache and it
                          ;; generally can't do that for cross-built binaries
                          ;; ("ldconfig: foo.so is for unknown machine 40.").
                          (make-dynamic-linker-cache? #f)

                          (license-file-regexp %license-file-regexp)
                          (phases '%standard-phases)
                          (locale "en_US.utf8")
                          (system (%current-system))
                          (build (nix-system->gnu-triplet system))
                          (imported-modules %gnu-build-system-modules)
                          (modules %default-modules)
                          (substitutable? #t)
                          allowed-references
                          disallowed-references)
  "Cross-build NAME for TARGET, where TARGET is a GNU triplet.  INPUTS are
cross-built inputs, and NATIVE-INPUTS are inputs that run on the build
platform."
  (define builder
    #~(begin
        (use-modules #$@(sexp->gexp modules))

        (define %build-host-inputs
          #+(input-tuples->gexp build-inputs))

        (define %build-target-inputs
          (append #$(input-tuples->gexp host-inputs)
                  #+(input-tuples->gexp target-inputs)))

        (define %build-inputs
          (append %build-host-inputs %build-target-inputs))

        (define %outputs
          #$(outputs->gexp outputs))

        (gnu-build #:source #+source
                   #:system #$system
                   #:build #$build
                   #:target #$target
                   #:outputs %outputs
                   #:inputs %build-target-inputs
                   #:native-inputs %build-host-inputs
                   #:search-paths '#$(sexp->gexp
                                      (map search-path-specification->sexp
                                           search-paths))
                   #:native-search-paths '#$(sexp->gexp
                                             (map
                                              search-path-specification->sexp
                                              native-search-paths))
                   #:phases #$phases
                   #:locale #$locale
                   #:bootstrap-scripts #$bootstrap-scripts
                   #:configure-flags #$configure-flags
                   #:make-flags #$make-flags
                   #:out-of-source? #$out-of-source?
                   #:tests? #$tests?
                   #:test-target #$test-target
                   #:parallel-build? #$parallel-build?
                   #:parallel-tests? #$parallel-tests?
                   #:patch-shebangs? #$patch-shebangs?
                   #:license-file-regexp #$license-file-regexp
                   #:strip-binaries? #$strip-binaries?
                   #:validate-runpath? #$validate-runpath?
                   #:make-dynamic-linker-cache? #$make-dynamic-linker-cache?
                   #:license-file-regexp #$license-file-regexp
                   #:strip-flags #$strip-flags
                   #:strip-directories #$strip-directories)))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:target target
                      #:graft? #f
                      #:modules imported-modules
                      #:substitutable? substitutable?
                      #:allowed-references allowed-references
                      #:disallowed-references disallowed-references
                      #:guile-for-build guile)))

(define gnu-build-system
  (build-system
    (name 'gnu)
    (description
     "The GNU Build System—i.e., ./configure && make && make install")
    (lower lower)))
