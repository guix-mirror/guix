;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
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
  #:use-module (guix derivations)
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
    '(#:source #:target #:jdk #:maven #:maven-plugins #:inputs #:native-inputs))

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

(define* (maven-build store name inputs
                       #:key (guile #f)
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
                       (phases '(@ (guix build maven-build-system)
                                   %standard-phases))
                       (system (%current-system))
                       (imported-modules %maven-build-system-modules)
                       (modules '((guix build maven-build-system)
                                  (guix build maven pom)
                                  (guix build utils))))
  "Build SOURCE using PATCHELF, and with INPUTS. This assumes that SOURCE
provides its own binaries."
  (define builder
    `(begin
       (use-modules ,@modules)
       (maven-build #:source ,(match (assoc-ref inputs "source")
                                 (((? derivation? source))
                                  (derivation->output-path source))
                                 ((source)
                                  source)
                                 (source
                                  source))
                     #:system ,system
                     #:outputs %outputs
                     #:inputs %build-inputs
                     #:search-paths ',(map search-path-specification->sexp
                                           search-paths)
                     #:phases ,phases
                     #:exclude (quote ,exclude)
                     #:local-packages (quote ,local-packages)
                     #:tests? ,tests?
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

(define maven-build-system
  (build-system
    (name 'maven)
    (description "The standard Maven build system")
    (lower lower)))

;;; maven.scm ends here
