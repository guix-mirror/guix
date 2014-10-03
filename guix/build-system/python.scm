;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (guix build-system python)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (package-with-python2
            python-build
            python-build-system))

;; Commentary:
;;
;; Standard build procedure for Python packages using 'setup.py'.  This is
;; implemented as an extension of 'gnu-build-system'.
;;
;; Code:

(define (default-python)
  "Return the default Python package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((python (resolve-interface '(gnu packages python))))
    (module-ref python 'python-wrapper)))

(define (default-python2)
  "Return the default Python 2 package."
  (let ((python (resolve-interface '(gnu packages python))))
    (module-ref python 'python-2)))

(define (package-with-explicit-python p python old-prefix new-prefix)
  "Create a package with the same fields as P, which is assumed to use
PYTHON-BUILD-SYSTEM, such that it is compiled with PYTHON instead.  The
inputs are changed recursively accordingly.  If the name of P starts with
OLD-PREFIX, this is replaced by NEW-PREFIX; otherwise, NEW-PREFIX is
prepended to the name."
  (let* ((build-system (package-build-system p))
         (rewrite-if-package
          (lambda (content)
            ;; CONTENT may be a file name, in which case it is returned, or a
            ;; package, which is rewritten with the new PYTHON and NEW-PREFIX.
            (if (package? content)
                (package-with-explicit-python content python
                                              old-prefix new-prefix)
                content)))
         (rewrite
           (match-lambda
             ((name content . rest)
              (append (list name (rewrite-if-package content)) rest)))))
    (package (inherit p)
      (name
        (let ((name (package-name p)))
          (if (eq? build-system python-build-system)
              (string-append new-prefix
                             (if (string-prefix? old-prefix name)
                                 (substring name (string-length old-prefix))
                                 name))
              name)))
      (arguments
        (let ((arguments (package-arguments p)))
          (if (eq? build-system python-build-system)
              (if (member #:python arguments)
                  (substitute-keyword-arguments arguments ((#:python p) python))
                  (append arguments `(#:python ,python)))
              arguments)))
      (inputs
        (map rewrite (package-inputs p)))
      (native-inputs
        (map rewrite (package-native-inputs p))))))

(define package-with-python2
  (cut package-with-explicit-python <> (default-python2) "python-" "python2-"))

(define* (lower name
                #:key source inputs native-inputs outputs target
                (python (default-python))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:source #:target #:python #:inputs #:native-inputs))

  (and (not target)                               ;XXX: no cross-compilation
       (bag
         (name name)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs

                        ;; Keep the standard inputs of 'gnu-build-system'.
                        ,@(standard-packages)))
         (build-inputs `(("python" ,python)
                         ,@native-inputs))
         (outputs outputs)
         (build python-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (python-build store name inputs
                       #:key
                       (tests? #t)
                       (test-target "test")
                       (configure-flags ''())
                       (phases '(@ (guix build python-build-system)
                                   %standard-phases))
                       (outputs '("out"))
                       (search-paths '())
                       (system (%current-system))
                       (guile #f)
                       (imported-modules '((guix build python-build-system)
                                           (guix build gnu-build-system)
                                           (guix build utils)))
                       (modules '((guix build python-build-system)
                                  (guix build utils))))
  "Build SOURCE using PYTHON, and with INPUTS.  This assumes that SOURCE
provides a 'setup.py' file as its build system."
  (define builder
    `(begin
       (use-modules ,@modules)
       (python-build #:name ,name
                     #:source ,(match (assoc-ref inputs "source")
                                 (((? derivation? source))
                                  (derivation->output-path source))
                                 ((source)
                                  source)
                                 (source
                                  source))
                     #:configure-flags ,configure-flags
                     #:system ,system
                     #:test-target ,test-target
                     #:tests? ,tests?
                     #:phases ,phases
                     #:outputs %outputs
                     #:search-paths ',(map search-path-specification->sexp
                                           search-paths)
                     #:inputs %build-inputs)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system))
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(gnu packages commencement)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system)))))

  (build-expression->derivation store name builder
                                #:inputs inputs
                                #:system system
                                #:modules imported-modules
                                #:outputs outputs
                                #:guile-for-build guile-for-build))

(define python-build-system
  (build-system
    (name 'python)
    (description "The standard Python build system")
    (lower lower)))

;;; python.scm ends here
