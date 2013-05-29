;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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
  #:export (python-build
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
    (module-ref python 'python)))

(define* (python-build store name source inputs
                       #:key
                       (python (default-python))
                       (python-version
                        (string-take (package-version (default-python)) 3))
                       (tests? #t)
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
                                  (guix build gnu-build-system)
                                  (guix build utils))))
  "Build SOURCE using PYTHON, and with INPUTS.  This assumes that SOURCE
provides a 'setup.py' file as its build system."
  (define python-search-paths
    (append (package-native-search-paths python)
            (standard-search-paths)))

  (define builder
    `(begin
       (use-modules ,@modules)
       (python-build #:name ,name
                     #:source ,(if (and source (derivation-path? source))
                                   (derivation-path->output-path source)
                                   source)
                     #:configure-flags ,configure-flags
                     #:system ,system
                     #:test-target "test"
                     #:tests? ,tests?
                     #:outputs %outputs
                     #:python-version ,python-version
                     #:search-paths ',(map search-path-specification->sexp
                                           (append python-search-paths
                                                   search-paths))
                     #:inputs %build-inputs)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system))
      ((and (? string?) (? derivation-path?))
       guile)
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(gnu packages base)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system)))))

  (let ((python (package-derivation store python system)))
    (build-expression->derivation store name system
                                  builder
                                  `(,@(if source
                                          `(("source" ,source))
                                          '())
                                    ("python" ,python)
                                    ,@inputs

                                    ;; Keep the standard inputs of
                                    ;; 'gnu-build-system'.
                                    ,@(standard-inputs system))

                                  #:modules imported-modules
                                  #:outputs outputs
                                  #:guile-for-build guile-for-build)))

(define python-build-system
  (build-system (name 'python)
                (description "The standard Python build system")
                (build python-build)))

;;; python.scm ends here
