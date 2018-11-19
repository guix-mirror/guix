;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
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

  #:use-module (guix derivations)
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
  "Build-side modules imported and used by default."
  `((guix build clojure-build-system)
    (guix build clojure-utils)
    (guix build guile-build-system)
    ,@%ant-build-system-modules))

(define-with-docs %default-clojure
  "The default Clojure package."
  (delay (@* (gnu packages lisp) clojure)))

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
  (let ((private-keywords '(#:source #:target
                            #:inputs #:native-inputs
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

(define-with-docs source->output-path
  "Convert source input to output path."
  (match-lambda
    (((? derivation? source))
     (derivation->output-path source))
    ((source)
     source)
    (source
     source)))

(define-with-docs maybe-guile->guile
  "Find the right guile."
  (match-lambda
    ((and maybe-guile (? package?))
     maybe-guile)
    (#f ; default
     (@* (gnu packages commencement) guile-final))))

(define* (clojure-build store name inputs
                        #:key
                        (source-dirs `',%source-dirs)
                        (test-dirs `',%test-dirs)
                        (compile-dir %compile-dir)

                        (jar-names `',(package-name->jar-names name))
                        (main-class %main-class)
                        (omit-source? %omit-source?)

                        (aot-include `',%aot-include)
                        (aot-exclude `',%aot-exclude)

                        doc-dirs ; no sensible default
                        (doc-regex %doc-regex)

                        (tests? %tests?)
                        (test-include `',%test-include)
                        (test-exclude `',%test-exclude)

                        (phases '(@ (guix build clojure-build-system)
                                    %standard-phases))
                        (outputs '("out"))
                        (search-paths '())
                        (system (%current-system))
                        (guile #f)

                        (imported-modules %clojure-build-system-modules)
                        (modules %clojure-build-system-modules))
  "Build SOURCE with INPUTS."
  (let ((builder `(begin
                    (use-modules ,@modules)
                    (clojure-build #:name ,name
                                   #:source ,(source->output-path
                                              (assoc-ref inputs "source"))

                                   #:source-dirs ,source-dirs
                                   #:test-dirs ,test-dirs
                                   #:compile-dir ,compile-dir

                                   #:jar-names ,jar-names
                                   #:main-class ,main-class
                                   #:omit-source? ,omit-source?

                                   #:aot-include ,aot-include
                                   #:aot-exclude ,aot-exclude

                                   #:doc-dirs ,doc-dirs
                                   #:doc-regex ,doc-regex

                                   #:tests? ,tests?
                                   #:test-include ,test-include
                                   #:test-exclude ,test-exclude

                                   #:phases ,phases
                                   #:outputs %outputs
                                   #:search-paths ',(map search-path-spec->sexp
                                                         search-paths)
                                   #:system ,system
                                   #:inputs %build-inputs)))

        (guile-for-build (package-derivation store
                                             (maybe-guile->guile guile)
                                             system
                                             #:graft? #f)))

    (build-expression->derivation store name builder
                                  #:inputs inputs
                                  #:system system
                                  #:modules imported-modules
                                  #:outputs outputs
                                  #:guile-for-build guile-for-build)))

(define clojure-build-system
  (build-system
    (name 'clojure)
    (description "Simple Clojure build system using plain old 'compile'")
    (lower lower)))

;;; clojure.scm ends here
