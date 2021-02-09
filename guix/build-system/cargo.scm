;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (guix build-system cargo)
  #:use-module (guix search-paths)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%cargo-build-system-modules
            %cargo-utils-modules
            cargo-build-system
            %crate-base-url
            crate-url
            crate-url?
            crate-uri))

(define %crate-base-url
  (make-parameter "https://crates.io"))
(define crate-url
  (string-append (%crate-base-url) "/api/v1/crates/"))
(define crate-url?
  (cut string-prefix? crate-url <>))

(define (crate-uri name version)
  "Return a URI string for the crate package hosted at crates.io corresponding
to NAME and VERSION."
  (string-append crate-url name "/" version "/download"))

(define (default-rust)
  "Return the default Rust package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((rust (resolve-interface '(gnu packages rust))))
    (module-ref rust 'rust)))

(define %cargo-utils-modules
  ;; Build-side modules imported by default.
  `((guix build cargo-utils)
    ,@%gnu-build-system-modules))

(define %cargo-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build cargo-build-system)
    (guix build json)
    ,@%cargo-utils-modules))

(define* (cargo-build store name inputs
                      #:key
                      (tests? #t)
                      (test-target #f)
                      (vendor-dir "guix-vendor")
                      (cargo-build-flags ''("--release"))
                      (cargo-test-flags ''("--release"))
                      (cargo-package-flags ''("--no-metadata" "--no-verify"))
                      (features ''())
                      (skip-build? #f)
                      (install-source? #t)
                      (phases '(@ (guix build cargo-build-system)
                                  %standard-phases))
                      (outputs '("out"))
                      (search-paths '())
                      (system (%current-system))
                      (guile #f)
                      (imported-modules %cargo-build-system-modules)
                      (modules '((guix build cargo-build-system)
                                 (guix build utils))))
  "Build SOURCE using CARGO, and with INPUTS."

  (define builder
    `(begin
       (use-modules ,@modules)
       (cargo-build #:name ,name
                    #:source ,(match (assoc-ref inputs "source")
                                (((? derivation? source))
                                 (derivation->output-path source))
                                ((source)
                                 source)
                                (source
                                 source))
                    #:system ,system
                    #:test-target ,test-target
                    #:vendor-dir ,vendor-dir
                    #:cargo-build-flags ,cargo-build-flags
                    #:cargo-test-flags ,cargo-test-flags
                    #:cargo-package-flags ,cargo-package-flags
                    #:features ,features
                    #:skip-build? ,skip-build?
                    #:install-source? ,install-source?
                    #:tests? ,(and tests? (not skip-build?))
                    #:phases ,phases
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

(define (package-cargo-inputs p)
  (apply
    (lambda* (#:key (cargo-inputs '()) #:allow-other-keys)
      cargo-inputs)
    (package-arguments p)))

(define (package-cargo-development-inputs p)
  (apply
    (lambda* (#:key (cargo-development-inputs '()) #:allow-other-keys)
      cargo-development-inputs)
    (package-arguments p)))

(define (crate-closure inputs)
  "Return the closure of INPUTS when considering the 'cargo-inputs' and
'cargod-dev-deps' edges.  Omit duplicate inputs, except for those
already present in INPUTS itself.

This is implemented as a breadth-first traversal such that INPUTS is
preserved, and only duplicate extracted inputs are removed.

Forked from ((guix packages) transitive-inputs) since this extraction
uses slightly different rules compared to the rest of Guix (i.e. we
do not extract the conventional inputs)."
  (define (seen? seen item)
    ;; FIXME: We're using pointer identity here, which is extremely sensitive
    ;; to memoization in package-producing procedures; see
    ;; <https://bugs.gnu.org/30155>.
    (vhash-assq item seen))

  (let loop ((inputs     inputs)
             (result     '())
             (propagated '())
             (first?     #t)
             (seen       vlist-null))
    (match inputs
      (()
       (if (null? propagated)
           (reverse result)
           (loop (reverse (concatenate propagated)) result '() #f seen)))
      (((and input (label (? package? package))) rest ...)
       (if (and (not first?) (seen? seen package))
           (loop rest result propagated first? seen)
           (loop rest
                 (cons input result)
                 (cons (package-cargo-inputs package)
                       propagated)
                 first?
                 (vhash-consq package package seen))))
      ((input rest ...)
       (loop rest (cons input result) propagated first? seen)))))

(define (expand-crate-sources cargo-inputs cargo-development-inputs)
  "Extract all transitive sources for CARGO-INPUTS and CARGO-DEVELOPMENT-INPUTS
along their 'cargo-inputs' edges.

Cargo requires all transitive crate dependencies' sources to be available
in its index, even if they are optional (this is so it can generate
deterministic Cargo.lock files regardless of the target platform or enabled
features). Thus we need all transitive crate dependencies for any cargo
dev-dependencies, but this is only needed when building/testing a crate directly
(i.e. we will never need transitive dev-dependencies for any dependency crates).

Another complication arises due potential dependency cycles from Guix's
perspective: Although cargo does not permit cyclic dependencies between crates,
however, it permits cycles to occur via dev-dependencies. For example, if crate
X depends on crate Y, crate Y's tests could pull in crate X to to verify
everything builds properly (this is a rare scenario, but it it happens for
example with the `proc-macro2` and `quote` crates). This is allowed by cargo
because tests are built as a pseudo-crate which happens to depend on the
X and Y crates, forming an acyclic graph.

We can side step this problem by only considering regular cargo dependencies
since they are guaranteed to not have cycles. We can further resolve any
potential dev-dependency cycles by extracting package sources (which never have
any dependencies and thus no cycles can exist).

There are several implications of this decision:
* Building a package definition does not require actually building/checking
any dependent crates. This can be a benefits:
 - For example, sometimes a crate may have an optional dependency on some OS
 specific package which cannot be built or run on the current system. This
 approach means that the build will not fail if cargo ends up internally ignoring
 the dependency.
 - It avoids waiting for quadratic builds from source: cargo always builds
 dependencies within the current workspace. This is largely due to Rust not
 having a stable ABI and other resolutions that cargo applies. This means that
 if we have a depencency chain of X -> Y -> Z and we build each definition
 independently the following will happen:
  * Cargo will build and test crate Z
  * Cargo will build crate Z in Y's workspace, then build and test Y
  * Cargo will build crates Y and Z in X's workspace, then build and test X
* But there are also some downsides with this approach:
  - If a dependent crate is subtly broken on the system (i.e. it builds but its
  tests fail) the consuming crates may build and test successfully but
  actually fail during normal usage (however, the CI will still build all
  packages which will give visibility in case packages suddenly break).
  - Because crates aren't declared as regular inputs, other Guix facilities
  such as tracking package graphs may not work by default (however, this is
  something that can always be extended or reworked in the future)."
  (filter-map
    (match-lambda
      ((label (? package? p))
       (list label (package-source p)))
      ((label input)
       (list label input)))
    (crate-closure (append cargo-inputs cargo-development-inputs))))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (rust (default-rust))
                (cargo-inputs '())
                (cargo-development-inputs '())
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."

  (define private-keywords
    '(#:source #:target #:rust #:inputs #:native-inputs #:outputs
      #:cargo-inputs #:cargo-development-inputs))

  (and (not target) ;; TODO: support cross-compilation
       (bag
         (name name)
         (system system)
         (target target)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs

                        ;; Keep the standard inputs of 'gnu-build-system'
                        ,@(standard-packages)))
         (build-inputs `(("cargo" ,rust "cargo")
                         ("rustc" ,rust)
                         ,@(expand-crate-sources cargo-inputs cargo-development-inputs)
                         ,@native-inputs))
         (outputs outputs)
         (build cargo-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define cargo-build-system
  (build-system
    (name 'cargo)
    (description
     "Cargo build system, to build Rust crates")
    (lower lower)))
