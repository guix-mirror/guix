;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2020, 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build-system clojure)
  #:use-module (guix build clojure-utils)
  #:use-module (guix build-system)
  #:use-module (guix build-system ant)
  #:use-module ((guix build-system gnu)
                #:select (standard-packages)
                #:prefix gnu:)

  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module ((guix search-paths)
                #:select
                ((search-path-specification->sexp . search-path-spec->sexp)))
  #:use-module (guix utils)

  #:use-module (ice-9 match)
  #:export (%clojure-build-system-modules
            clojure-build
            clojure-build-system))

;; Commentary:
;;
;; Standard build procedure for Clojure packages.
;;
;; Code:

(define-with-docs %clojure-build-system-modules
  "Build-side modules imported by default."
  `((guix build clojure-build-system)
    (guix build clojure-utils)
    (guix build guile-build-system)
    ,@%ant-build-system-modules))

(define %default-modules
  ;; Modules in scope in the build-side environment.
  '((guix build clojure-build-system)
    (guix build clojure-utils)
    (guix build utils)))

(define-with-docs %default-clojure
  "The default Clojure package."
  (delay (@* (gnu packages clojure) clojure)))

(define-with-docs %default-jdk
  "The default JDK package."
  (delay (@* (gnu packages java) icedtea)))

(define-with-docs %default-zip
  "The default ZIP package."
  (delay (@* (gnu packages compression) zip)))

(define* (lower name
                #:key
                source target
                inputs native-inputs
                (clojure (force %default-clojure))
                (jdk (force %default-jdk))
                (zip (force %default-zip))
                outputs system
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (let ((private-keywords '(#:target #:inputs #:native-inputs
                            #:clojure #:jdk #:zip)))

    (if target
        (error "No cross-compilation for clojure-build-system yet: LOWER"
               target) ; FIXME
        (bag (name name)
             (system system)
             (host-inputs `(,@(if source
                                  `(("source" ,source))
                                  '())
                            ,@inputs
                            ,@(gnu:standard-packages)))
             (build-inputs `(("clojure" ,clojure)
                             ("jdk" ,jdk "jdk")
                             ("zip" ,zip)
                             ,@native-inputs))
             (outputs outputs)
             (build clojure-build)
             (arguments (strip-keyword-arguments private-keywords
                                                 arguments))))))

(define* (clojure-build name inputs
                        #:key
                        source
                        (source-dirs `',%source-dirs)
                        (java-source-dirs `',%java-source-dirs)
                        (test-dirs `',%test-dirs)
                        (compile-dir %compile-dir)
                        (java-compile-dir %java-compile-dir)

                        (jar-names `',(package-name->jar-names name))
                        (main-class %main-class)
                        (omit-source? %omit-source?)

                        (aot-include `',%aot-include)
                        (aot-exclude `',%aot-exclude)

                        doc-dirs                  ; no sensible default
                        (doc-regex %doc-regex)

                        (tests? %tests?)
                        (test-include `',%test-include)
                        (test-exclude `',%test-exclude)

                        (phases '%standard-phases)
                        (outputs '("out"))
                        (search-paths '())
                        (system (%current-system))
                        (guile #f)

                        (imported-modules %clojure-build-system-modules)
                        (modules %default-modules))
  "Build SOURCE with INPUTS."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          (clojure-build #:name #$name
                         #:source #+source

                         #:source-dirs #$source-dirs
                         #:java-source-dirs #$java-source-dirs
                         #:test-dirs #$test-dirs
                         #:compile-dir #$compile-dir
                         #:java-compile-dir #$java-compile-dir
                         
                         #:jar-names #$jar-names
                         #:main-class #$main-class
                         #:omit-source? #$omit-source?

                         #:aot-include #$aot-include
                         #:aot-exclude #$aot-exclude

                         #:doc-dirs #$doc-dirs
                         #:doc-regex #$doc-regex

                         #:tests? #$tests?
                         #:test-include #$test-include
                         #:test-exclude #$test-exclude

                         #:phases #$phases
                         #:outputs #$(outputs->gexp outputs)
                         #:search-paths '#$(sexp->gexp
                                            (map search-path-spec->sexp
                                                 search-paths))
                         #:system #$system
                         #:inputs #$(input-tuples->gexp inputs)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:guile-for-build guile)))

(define clojure-build-system
  (build-system
    (name 'clojure)
    (description "Simple Clojure build system using plain old 'compile'")
    (lower lower)))

;;; clojure.scm ends here
