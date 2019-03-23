;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (guix build-system rakudo)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export (%rakudo-build-system-modules
            rakudo-build
            rakudo-build-system))

;; Commentary:
;;
;; Standard build and install procedure for packages using the Rakudo
;; build system to be installed as system libraries.  This is
;; implemented as an extension of `gnu-build-system'.
;;
;; Code:

(define %rakudo-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build rakudo-build-system)
    ,@%gnu-build-system-modules))

(define (default-rakudo)
  "Return the default Rakudo package."

  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages perl6))))
    (module-ref module 'rakudo)))

(define (default-prove6)
  "Return the default perl6-tap-harness package for tests."
  (let ((module (resolve-interface '(gnu packages perl6))))
    (module-ref module 'perl6-tap-harness)))

(define (default-zef)
  "Return the default perl6-zef package."
  (let ((module (resolve-interface '(gnu packages perl6))))
    (module-ref module 'perl6-zef)))

(define* (lower name
                #:key source inputs native-inputs outputs
                system target
                (rakudo (default-rakudo))
                (prove6 (default-prove6))
                (zef (default-zef))
                (with-prove6? #t)
                (with-zef? #t)
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:source #:target #:rakudo #:prove6 #:zef #:inputs #:native-inputs))

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
         (build-inputs `(("rakudo" ,rakudo)
                         ,@(if with-prove6?
                               `(("perl6-tap-harness" ,prove6)
                                 ,@(if with-zef?
                                       `(("perl6-zef" ,zef))
                                       '()))
                               '())
                         ,@native-inputs))
         (outputs outputs)
         (build rakudo-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (rakudo-build store name inputs
                       #:key
                       (search-paths '())
                       (tests? #t)
                       (phases '(@ (guix build rakudo-build-system)
                                   %standard-phases))
                       (outputs '("out"))
                       (system (%current-system))
                       (guile #f)
                       (with-zef? #t)
                       (with-prove6? #t)
                       (imported-modules %rakudo-build-system-modules)
                       (modules '((guix build rakudo-build-system)
                                  (guix build utils))))
  "Build SOURCE using PERL6, and with INPUTS."
  (define builder
    `(begin
       (use-modules ,@modules)
       (rakudo-build #:name ,name
                     #:source ,(match (assoc-ref inputs "source")
                                      (((? derivation? source))
                                       (derivation->output-path source))
                                      ((source)
                                       source)
                                      (source
                                        source))
                     #:search-paths ',(map search-path-specification->sexp
                                           search-paths)
                     #:phases ,phases
                     #:system ,system
                     #:tests? ,tests?
                     #:outputs %outputs
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
                                #:system system
                                #:inputs inputs
                                #:modules imported-modules
                                #:outputs outputs
                                #:guile-for-build guile-for-build))

(define rakudo-build-system
  (build-system
    (name 'rakudo)
    (description "The standard Rakudo build system")
    (lower lower)))

;;; rakudo.scm ends here
