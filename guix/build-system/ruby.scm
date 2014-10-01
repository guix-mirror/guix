;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
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

(define-module (guix build-system ruby)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages version-control)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (ruby-build
            ruby-build-system))

(define (default-ruby)
  "Return the default Ruby package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((ruby (resolve-interface '(gnu packages ruby))))
    (module-ref ruby 'ruby)))

(define* (ruby-build store name source inputs
                     #:key
                     (ruby (default-ruby))
                     (test-target "test")
                     (tests? #t)
                     (phases '(@ (guix build ruby-build-system)
                                 %standard-phases))
                     (outputs '("out"))
                     (search-paths '())
                     (system (%current-system))
                     (guile #f)
                     (imported-modules '((guix build ruby-build-system)
                                         (guix build gnu-build-system)
                                         (guix build utils)))
                     (modules '((guix build ruby-build-system)
                                (guix build utils))))
  "Build SOURCE using RUBY and INPUTS."
  (define ruby-search-paths
    (append (package-native-search-paths ruby)
            (standard-search-paths)))

  (define builder
    `(begin
       (use-modules ,@modules)
       (ruby-build #:name ,name
                   #:source ,(if (derivation? source)
                                 (derivation->output-path source)
                                 source)
                   #:system ,system
                   #:test-target ,test-target
                   #:tests? ,tests?
                   #:phases ,phases
                   #:outputs %outputs
                   #:search-paths ',(map search-path-specification->sexp
                                         (append ruby-search-paths
                                                 search-paths))
                   #:inputs %build-inputs)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system))
      (#f
       (let* ((distro (resolve-interface '(gnu packages commencement)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system)))))

  (let ((ruby (package-derivation store ruby system))
        (git (package-derivation store git system)))
    (build-expression->derivation store name builder
                                  #:inputs
                                  `(,@(if source
                                          `(("source" ,source))
                                          '())
                                    ("ruby" ,ruby)
                                    ,@inputs
                                    ;; Keep the standard inputs of
                                    ;; 'gnu-build-system'.
                                    ,@(standard-inputs system))
                                  #:system system
                                  #:modules imported-modules
                                  #:outputs outputs
                                  #:guile-for-build guile-for-build)))

(define ruby-build-system
  (build-system
   (name 'ruby)
   (description "The standard Ruby build system")
   (build ruby-build)))
