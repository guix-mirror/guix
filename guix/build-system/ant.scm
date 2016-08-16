;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix build-system ant)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (%ant-build-system-modules
            ant-build
            ant-build-system))

;; Commentary:
;;
;; Standard build procedure for Java packages using Ant.
;;
;; Code:

(define %ant-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build ant-build-system)
    (guix build syscalls)
    ,@%gnu-build-system-modules))

(define (default-jdk)
  "Return the default JDK package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((jdk-mod (resolve-interface '(gnu packages java))))
    (module-ref jdk-mod 'icedtea)))

(define (default-ant)
  "Return the default Ant package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((jdk-mod (resolve-interface '(gnu packages java))))
    (module-ref jdk-mod 'ant)))

(define (default-zip)
  "Return the default ZIP package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((zip-mod (resolve-interface '(gnu packages zip))))
    (module-ref zip-mod 'zip)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (jdk (default-jdk))
                (ant (default-ant))
                (zip (default-zip))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:source #:target #:jdk #:ant #:zip #:inputs #:native-inputs))

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
         (build-inputs `(("jdk" ,jdk "jdk")
                         ("ant" ,ant)
                         ("zip" ,zip)
                         ,@native-inputs))
         (outputs outputs)
         (build ant-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (ant-build store name inputs
                    #:key
                    (tests? #t)
                    (test-target "tests")
                    (configure-flags ''())
                    (make-flags ''())
                    (build-target "jar")
                    (jar-name #f)
                    (phases '(@ (guix build ant-build-system)
                                %standard-phases))
                    (outputs '("out"))
                    (search-paths '())
                    (system (%current-system))
                    (guile #f)
                    (imported-modules %ant-build-system-modules)
                    (modules '((guix build ant-build-system)
                               (guix build utils))))
  "Build SOURCE with INPUTS."
  (define builder
    `(begin
       (use-modules ,@modules)
       (ant-build #:name ,name
                  #:source ,(match (assoc-ref inputs "source")
                              (((? derivation? source))
                               (derivation->output-path source))
                              ((source)
                               source)
                              (source
                               source))
                  #:make-flags ,make-flags
                  #:configure-flags ,configure-flags
                  #:system ,system
                  #:tests? ,tests?
                  #:test-target ,test-target
                  #:build-target ,build-target
                  #:jar-name ,jar-name
                  #:phases ,phases
                  #:outputs %outputs
                  #:search-paths ',(map search-path-specification->sexp
                                        search-paths)
                  #:inputs %build-inputs)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system #:graft? #f))
      (#f                               ; the default
       (let* ((distro (resolve-interface '(gnu packages commencement)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system #:graft? #f)))))

  (build-expression->derivation store name builder
                                #:inputs inputs
                                #:system system
                                #:modules imported-modules
                                #:outputs outputs
                                #:guile-for-build guile-for-build))

(define ant-build-system
  (build-system
    (name 'ant)
    (description "The standard Ant build system")
    (lower lower)))

;;; ant.scm ends here
