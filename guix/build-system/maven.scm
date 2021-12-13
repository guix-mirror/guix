;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build-system maven)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%maven-build-system-modules
            default-maven
            default-maven-plugins
            %default-exclude
            lower
            maven-build
            maven-build-system))

;; Commentary:
;;
;; Standard build procedure for Maven packages.  This is implemented as an
;; extension of `gnu-build-system'.
;;
;; Code:

(define %maven-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build maven-build-system)
    (guix build maven pom)
    ,@%gnu-build-system-modules))

(define (default-maven)
  "Return the default maven package."

  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages maven))))
    (module-ref module 'maven)))

(define (default-maven-compiler-plugin)
  "Return the default maven compiler plugin package."
  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages maven))))
    (module-ref module 'maven-compiler-plugin)))

(define (default-maven-jar-plugin)
  "Return the default maven jar plugin package."
  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages maven))))
    (module-ref module 'maven-jar-plugin)))

(define (default-maven-resources-plugin)
  "Return the default maven resources plugin package."
  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages maven))))
    (module-ref module 'maven-resources-plugin)))

(define (default-maven-surefire-plugin)
  "Return the default maven surefire plugin package."
  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages maven))))
    (module-ref module 'maven-surefire-plugin)))

(define (default-java-surefire-junit4)
  "Return the default surefire junit4 provider package."
  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages maven))))
    (module-ref module 'java-surefire-junit4)))

(define (default-maven-install-plugin)
  "Return the default maven install plugin package."
  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages maven))))
    (module-ref module 'maven-install-plugin)))

(define (default-jdk)
  "Return the default JDK package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((jdk-mod (resolve-interface '(gnu packages java))))
    (module-ref jdk-mod 'icedtea)))

(define (default-maven-plugins)
  `(("maven-compiler-plugin" ,(default-maven-compiler-plugin))
    ("maven-jar-plugin" ,(default-maven-jar-plugin))
    ("maven-resources-plugin" ,(default-maven-resources-plugin))
    ("maven-surefire-plugin" ,(default-maven-surefire-plugin))
    ("java-surefire-junit4" ,(default-java-surefire-junit4))
    ("maven-install-plugin" ,(default-maven-install-plugin))))

(define %default-exclude
  `(("org.apache.maven.plugins" .
      ("maven-release-plugin" "maven-site-plugin"))))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (maven (default-maven))
                (jdk (default-jdk))
                (maven-plugins (default-maven-plugins))
                (local-packages '())
                (exclude %default-exclude)
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:jdk #:maven #:maven-plugins #:inputs #:native-inputs))

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
         (build-inputs `(("maven" ,maven)
                         ("jdk" ,jdk "jdk")
                         ,@maven-plugins
                         ,@native-inputs))
         (outputs outputs)
         (build maven-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (maven-build name inputs
                      #:key
                      source (guile #f)
                      (outputs '("out"))
                      (search-paths '())
                      (out-of-source? #t)
                      (validate-runpath? #t)
                      (patch-shebangs? #t)
                      (strip-binaries? #t)
                      (exclude %default-exclude)
                      (local-packages '())
                      (tests? #t)
                      (strip-flags ''("--strip-debug"))
                      (strip-directories ''("lib" "lib64" "libexec"
                                            "bin" "sbin"))
                      (phases '%standard-phases)
                      (system (%current-system))
                      (imported-modules %maven-build-system-modules)
                      (modules '((guix build maven-build-system)
                                 (guix build maven pom)
                                 (guix build utils))))
  "Build SOURCE using PATCHELF, and with INPUTS. This assumes that SOURCE
provides its own binaries."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))
          (maven-build #:source #+source
                       #:system #$system
                       #:outputs #$(outputs->gexp outputs)
                       #:inputs #$(input-tuples->gexp inputs)
                       #:search-paths '#$(sexp->gexp
                                          (map search-path-specification->sexp
                                               search-paths))
                       #:phases #$phases
                       #:exclude '#$exclude
                       #:local-packages '#$local-packages
                       #:tests? #$tests?
                       #:out-of-source? #$out-of-source?
                       #:validate-runpath? #$validate-runpath?
                       #:patch-shebangs? #$patch-shebangs?
                       #:strip-binaries? #$strip-binaries?
                       #:strip-flags #$(sexp->gexp strip-flags)
                       #:strip-directories #$(sexp->gexp strip-directories)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:guile-for-build guile)))

(define maven-build-system
  (build-system
    (name 'maven)
    (description "The standard Maven build system")
    (lower lower)))

;;; maven.scm ends here
