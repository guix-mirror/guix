;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Petter <petter@mykolab.ch>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2021-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
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

(define-module (guix build-system go)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (%go-build-system-modules
            go-build
            go-build-system

            go-pseudo-version?
            go-version->git-ref))

;; Commentary:
;;
;; Standard build procedure for packages using the Go build system.  It is
;; implemented as an extension of 'gnu-build-system'.
;;
;; Code:

(define %go-pseudo-version-rx
  ;; Match only the end of the version string; this is so that matching the
  ;; more complex leading semantic version pattern is not required.
  (make-regexp (string-append
                "([0-9]{14}-)"                 ;timestamp
                "([0-9A-Fa-f]{12})"            ;commit hash
                "(\\+incompatible)?$")))       ;optional +incompatible tag

(define (go-version->git-ref version)
  "Parse VERSION, a \"pseudo-version\" as defined at
<https://golang.org/ref/mod#pseudo-versions>, and extract the commit hash from
it, defaulting to full VERSION (stripped from the \"+incompatible\" suffix if
present) if a pseudo-version pattern is not recognized."
  ;; A module version like v1.2.3 is introduced by tagging a revision in the
  ;; underlying source repository.  Untagged revisions can be referred to
  ;; using a "pseudo-version" like v0.0.0-yyyymmddhhmmss-abcdefabcdef, where
  ;; the time is the commit time in UTC and the final suffix is the prefix of
  ;; the commit hash (see: https://golang.org/ref/mod#pseudo-versions).
  (let* ((version
          ;; If a source code repository has a v2.0.0 or later tag for a file
          ;; tree with no go.mod, the version is considered to be part of the
          ;; v1 module's available versions and is given an +incompatible
          ;; suffix
          ;; (see:https://golang.org/cmd/go/#hdr-Module_compatibility_and_semantic_versioning).
          (if (string-suffix? "+incompatible" version)
              (string-drop-right version 13)
              version))
         (match (regexp-exec %go-pseudo-version-rx version)))
    (if match
        (match:substring match 2)
        version)))

(define (go-pseudo-version? version)
  "True if VERSION is a Go pseudo-version, i.e., a version string made of a
commit hash and its date rather than a proper release tag."
  (regexp-exec %go-pseudo-version-rx version))

(define (go-target target)
    ;; Parse the nix-system equivalent of the target and set the
    ;; target for compilation accordingly.
    (match (string-split (gnu-triplet->nix-system target) #\-)
      ((arch os)
       (list (match arch
               ("aarch64" "arm64")
               ("armhf" "arm")
               ("powerpc64le" "ppc64le")
               ("powerpc64" "ppc64")
               ("i686" "386")
               ("x86_64" "amd64")
               ("mips64el" "mips64le")
               (_ arch))
             (match os
               ((or "mingw32" "cygwin") "windows")
               (_ os))))))

(define %go-build-system-modules
  ;; Build-side modules imported and used by default.
  `((guix build go-build-system)
    (guix build union)
    ,@%gnu-build-system-modules))

(define (default-go)
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((go (resolve-interface '(gnu packages golang))))
    (module-ref go 'go)))

(define (make-go-std)
  (module-ref (resolve-interface '(gnu packages golang)) 'make-go-std))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (go (default-go))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:go #:inputs #:native-inputs))

  (define inputs-with-cache
    ;; XXX: Avoid a circular dependency.  This should be rewritten with
    ;; 'package-mapping' or similar.
    (let ((go-std-name (string-append (package-name go) "-std")))
      (if (string-prefix? go-std-name name)
          inputs
          (cons `(,go-std-name ,((make-go-std) go)) inputs))))

  (bag
    (name name)
    (system system)
    (target target)
    (build-inputs `(,@(if source
                        `(("source" ,source))
                        '())
                     ,@`(("go" ,go))
                     ,@native-inputs
                     ,@(if target '() inputs-with-cache)
                     ,@(if target
                         ;; Use the standard cross inputs of
                         ;; 'gnu-build-system'.
                         (standard-cross-packages target 'host)
                         '())
                     ;; Keep the standard inputs of 'gnu-build-system'.
                     ,@(standard-packages)))
    (host-inputs (if target inputs-with-cache '()))

    ;; The cross-libc is really a target package, but for bootstrapping
    ;; reasons, we can't put it in 'host-inputs'.  Namely, 'cross-gcc' is a
    ;; native package, so it would end up using a "native" variant of
    ;; 'cross-libc' (built with 'gnu-build'), whereas all the other packages
    ;; would use a target variant (built with 'gnu-cross-build'.)
    (target-inputs (if target
                     (standard-cross-packages target 'target)
                     '()))

    (outputs outputs)
    (build (if target go-cross-build go-build))
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (go-build name inputs
                   #:key
                   source
                   (phases '%standard-phases)
                   (outputs '("out"))
                   (search-paths '())
                   (install-source? #t)
                   (import-path "")
                   (unpack-path "")
                   (build-flags ''())
                   (tests? #t)
                   (allow-go-reference? #f)
                   (system (%current-system))
                   (goarch #f)
                   (goos #f)
                   (guile #f)
                   (imported-modules %go-build-system-modules)
                   (modules '((guix build go-build-system)
                              (guix build union)
                              (guix build utils)))
                   (substitutable? #t))
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@modules)
          (go-build #:name #$name
                    #:source #+source
                    #:system #$system
                    #:phases #$phases
                    #:outputs #$(outputs->gexp outputs)
                    #:substitutable? #$substitutable?
                    #:goarch #$goarch
                    #:goos #$goos
                    #:search-paths '#$(sexp->gexp
                                       (map search-path-specification->sexp
                                            search-paths))
                    #:install-source? #$install-source?
                    #:import-path #$import-path
                    #:unpack-path #$unpack-path
                    #:build-flags #$build-flags
                    #:tests? #$tests?
                    #:allow-go-reference? #$allow-go-reference?
                    #:inputs #$(input-tuples->gexp inputs)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:guile-for-build guile)))

(define* (go-cross-build name
                         #:key
                         source target
                         build-inputs target-inputs host-inputs
                         (phases '%standard-phases)
                         (outputs '("out"))
                         (search-paths '())
                         (native-search-paths '())
                         (install-source? #t)
                         (import-path "")
                         (unpack-path "")
                         (build-flags ''())
                         (tests? #f)              ; nothing can be done
                         (allow-go-reference? #f)
                         (system (%current-system))
                         (goarch (first (go-target target)))
                         (goos (last (go-target target)))
                         (guile #f)
                         (imported-modules %go-build-system-modules)
                         (modules '((guix build go-build-system)
                                    (guix build union)
                                    (guix build utils)))
                         (substitutable? #t))
  "Cross-build NAME using GO, where TARGET is a GNU triplet and with INPUTS."
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

        (go-build #:name #$name
                  #:source #+source
                  #:system #$system
                  #:phases #$phases
                  #:outputs %outputs
                  #:target #$target
                  #:goarch #$goarch
                  #:goos #$goos
                  #:inputs %build-target-inputs
                  #:native-inputs %build-host-inputs
                  #:search-paths '#$(map search-path-specification->sexp
                                         search-paths)
                  #:native-search-paths '#$(map
                                            search-path-specification->sexp
                                            native-search-paths)
                  #:install-source? #$install-source?
                  #:import-path #$import-path
                  #:unpack-path #$unpack-path
                  #:build-flags #$build-flags
                  #:tests? #$tests?
                  #:make-dynamic-linker-cache? #f ;cross-compiling
                  #:allow-go-reference? #$allow-go-reference?
                  #:inputs %build-inputs)))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:target target
                      #:graft? #f
                      #:substitutable? substitutable?
                      #:guile-for-build guile)))

(define go-build-system
  (build-system
    (name 'go)
    (description
     "Build system for Go programs")
    (lower lower)))
