;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2019, 2020, 2021 Guillaume Le Vaillant <glv@posteo.net>
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

(define-module (guix build-system asdf)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix memoization)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix search-paths)
  #:use-module ((guix build utils)
                #:select ((package-name->name+version
                           . hyphen-separated-name->name+version)))
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (gnu packages)
  #:export (%asdf-build-system-modules
            %asdf-build-modules
            asdf-build
            asdf-build-system/sbcl
            asdf-build-system/ecl
            asdf-build-system/source
            sbcl-package->cl-source-package
            sbcl-package->ecl-package))

;; Commentary:
;;
;; Standard build procedure for asdf packages.  This is implemented as an
;; extension of 'gnu-build-system'.
;;
;; Code:

(define %asdf-build-system-modules
  ;; Imported build-side modules
  `((guix build asdf-build-system)
    (guix build lisp-utils)
    (guix build union)
    ,@%gnu-build-system-modules))

(define %asdf-build-modules
  ;; Used (visible) build-side modules
  '((guix build asdf-build-system)
    (guix build utils)
    (guix build union)
    (guix build lisp-utils)))

(define (default-lisp implementation)
  "Return the default package for the lisp IMPLEMENTATION."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((lisp-module (resolve-interface '(gnu packages lisp))))
    (module-ref lisp-module implementation)))

(define* (lower/source name
                       #:key source inputs outputs native-inputs system target
                       #:allow-other-keys
                       #:rest arguments)
  "Return a bag for NAME"
  (define private-keywords
    '(#:target #:inputs #:native-inputs))

  (and (not target)
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs
                        ,@(standard-packages)))
         (build-inputs native-inputs)
         (outputs outputs)
         (build asdf-build/source)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (asdf-build/source name inputs
                            #:key source outputs
                            (phases '%standard-phases/source)
                            (search-paths '())
                            (system (%current-system))
                            (guile #f)
                            (imported-modules %asdf-build-system-modules)
                            (modules %asdf-build-modules))
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))
          (asdf-build/source #:name #$name
                             #:source #+source
                             #:system #$system
                             #:phases #$phases
                             #:outputs #$(outputs->gexp outputs)
                             #:search-paths '#$(sexp->gexp
                                                (map search-path-specification->sexp
                                                     search-paths))
                             #:inputs #$(input-tuples->gexp inputs)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:guile-for-build guile)))

(define* (package-with-build-system from-build-system to-build-system
                                    from-prefix to-prefix
                                    #:key variant-property
                                    phases-transformer)
  "Return a precedure which takes a package PKG which uses FROM-BUILD-SYSTEM,
and returns one using TO-BUILD-SYSTEM.  If PKG was prefixed by FROM-PREFIX,
the resulting package will be prefixed by TO-PREFIX.  Inputs of PKG are
recursively transformed using the same rule.  The result's #:phases argument
will be modified by PHASES-TRANSFORMER, an S-expression which evaluates on the
build side to a procedure of one argument.

VARIANT-PROPERTY can be added to a package's properties to indicate that the
corresponding package promise should be used as the result of this
transformation.  This allows the result to differ from what the transformation
would otherwise produce.

If TO-BUILD-SYSTEM is asdf-build-system/source, the resulting package will be
set up using CL source package conventions."
  (define target-is-source? (eq? asdf-build-system/source to-build-system))

  (define (transform-package-name name)
    (if (string-prefix? from-prefix name)
        (let ((new-name (string-drop name (string-length from-prefix))))
          (if (string-prefix? to-prefix new-name)
              new-name
              (string-append to-prefix new-name)))
        name))

  (define (has-from-build-system? pkg)
    (and (package? pkg)
         (eq? from-build-system (package-build-system pkg))))

  (define (find-input-package pkg)
    (let* ((name (package-name pkg))
           (new-name (transform-package-name name))
           (pkgs (find-packages-by-name new-name)))
      (if (null? pkgs) #f (list-ref pkgs 0))))

  (define transform
    (mlambda (pkg)
      (define rewrite
        (match-lambda
          ((name content . rest)
           (let* ((is-package? (package? content))
                  (new-content (if is-package?
                                   (or (find-input-package content)
                                       (transform content))
                                   content)))
             `(,name ,new-content ,@rest)))))

      ;; Special considerations for source packages: CL inputs become
      ;; propagated, and un-handled arguments are removed.

      (define (new-propagated-inputs)
        (if target-is-source?
            (map rewrite
                 (append
                  (filter (match-lambda
                            ((_ input . _)
                             (has-from-build-system? input)))
                          (append (package-inputs pkg)
                                  ;; The native inputs might be needed just
                                  ;; to load the system.
                                  (package-native-inputs pkg)))
                  (package-propagated-inputs pkg)))

            (map rewrite (package-propagated-inputs pkg))))

      (define (new-inputs inputs-getter)
        (if target-is-source?
            (map rewrite
                 (filter (match-lambda
                           ((_ input . _)
                            (not (has-from-build-system? input))))
                         (inputs-getter pkg)))
            (map rewrite (inputs-getter pkg))))

      (define base-arguments
        (if target-is-source?
            (strip-keyword-arguments
             '(#:tests? #:asd-files #:lisp #:asd-systems #:test-asd-file)
             (package-arguments pkg))
            (package-arguments pkg)))

      (cond
       ((and variant-property
             (assoc-ref (package-properties pkg) variant-property))
        => force)

       ((has-from-build-system? pkg)
        (package
          (inherit pkg)
          (location (package-location pkg))
          (name (transform-package-name (package-name pkg)))
          (build-system to-build-system)
          (arguments
           (substitute-keyword-arguments base-arguments
             ((#:phases phases) (list phases-transformer phases))))
          (inputs (new-inputs package-inputs))
          (propagated-inputs (new-propagated-inputs))
          (native-inputs (append (if target-is-source?
                                     (list (list (package-name pkg) pkg))
                                     '())
                                 (new-inputs package-native-inputs)))
          (outputs (if target-is-source?
                       '("out")
                       (package-outputs pkg)))))
       (else pkg))))

  transform)

(define (strip-variant-as-necessary variant pkg)
  (define properties (package-properties pkg))
  (if (assoc variant properties)
      (package
        (inherit pkg)
        (properties (alist-delete variant properties)))
      pkg))

(define (lower lisp-type)
  (lambda* (name
            #:key source inputs outputs native-inputs system target
            (lisp (default-lisp (string->symbol lisp-type)))
            #:allow-other-keys
            #:rest arguments)
    "Return a bag for NAME"
    (define private-keywords
      '(#:target #:inputs #:native-inputs #:lisp))

    (and (not target)
         (bag
           (name name)
           (system system)
           (host-inputs `(,@(if source
                                `(("source" ,source))
                                '())
                          ,@inputs
                          ,@(standard-packages)))
           (build-inputs `((,lisp-type ,lisp)
                           ,@native-inputs))
           (outputs outputs)
           (build (asdf-build lisp-type))
           (arguments (strip-keyword-arguments private-keywords arguments))))))

(define (asdf-build lisp-type)
  (lambda* (name inputs
                 #:key source outputs
                 (tests? #t)
                 (asd-files ''())
                 (asd-systems ''())
                 (test-asd-file #f)
                 (phases '%standard-phases)
                 (search-paths '())
                 (system (%current-system))
                 (guile #f)
                 (imported-modules %asdf-build-system-modules)
                 (modules %asdf-build-modules))

    (define systems
      (if (null? (cadr asd-systems))
          ;; FIXME: Find a more reliable way to get the main system name.
          (let* ((lisp-prefix (string-append lisp-type "-"))
                 (package-name (hyphen-separated-name->name+version
                                (if (string-prefix? lisp-prefix name)
                                    (string-drop name
                                                 (string-length lisp-prefix))
                                    name))))
            `(quote ,(list package-name)))
          asd-systems))

    (define builder
      (with-imported-modules imported-modules
        #~(begin
            (use-modules #$@(sexp->gexp modules))
            (parameterize ((%lisp (search-input-file
                                   #$(input-tuples->gexp inputs)
                                   (string-append "bin/" #$lisp-type)))
                           (%lisp-type #$lisp-type))
              (asdf-build #:name #$name
                          #:source #+source
                          #:asd-files #$asd-files
                          #:asd-systems #$systems
                          #:test-asd-file #$test-asd-file
                          #:system #$system
                          #:tests? #$tests?
                          #:phases #$phases
                          #:outputs #$(outputs->gexp outputs)
                          #:search-paths '#$(sexp->gexp
                                             (map search-path-specification->sexp
                                                  search-paths))
                          #:inputs #$(input-tuples->gexp inputs))))))

    (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                    system #:graft? #f)))
      (gexp->derivation name builder
                        #:system system
                        #:guile-for-build guile))))

(define asdf-build-system/sbcl
  (build-system
    (name 'asdf/sbcl)
    (description "The build system for ASDF binary packages using SBCL")
    (lower (lower "sbcl"))))

(define asdf-build-system/ecl
  (build-system
    (name 'asdf/ecl)
    (description "The build system for ASDF binary packages using ECL")
    (lower (lower "ecl"))))

(define asdf-build-system/source
  (build-system
    (name 'asdf/source)
    (description "The build system for ASDF source packages")
    (lower lower/source)))

(define sbcl-package->cl-source-package
  (let* ((property 'cl-source-variant)
         (transformer
          (package-with-build-system asdf-build-system/sbcl
                                     asdf-build-system/source
                                     "sbcl-"
                                     "cl-"
                                     #:variant-property property
                                     #:phases-transformer
                                     '(const %standard-phases/source))))
    (lambda (pkg)
      (transformer
       (strip-variant-as-necessary property pkg)))))

(define sbcl-package->ecl-package
  (let* ((property 'ecl-variant)
         (transformer
          (package-with-build-system asdf-build-system/sbcl
                                     asdf-build-system/ecl
                                     "sbcl-"
                                     "ecl-"
                                     #:variant-property property
                                     #:phases-transformer
                                     'identity)))
    (lambda (pkg)
      (transformer
       (strip-variant-as-necessary property pkg)))))

;;; asdf.scm ends here
