;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix build-system minify)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (%minify-build-system-modules
            minify-build
            minify-build-system))

;; Commentary:
;;
;; Standard minification procedure for JavaScript files.
;;
;; Code:

(define %minify-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build minify-build-system)
    ,@%gnu-build-system-modules))

(define (default-uglify-js)
  "Return the default package to minify JavaScript source files."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((lisp-mod (resolve-interface '(gnu packages lisp))))
    (module-ref lisp-mod 'uglify-js)))

(define* (lower name
                #:key source inputs native-inputs outputs system
                (uglify-js (default-uglify-js))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:source #:target #:inputs #:native-inputs))

  (bag
    (name name)
    (system system)
    (host-inputs `(,@(if source
                         `(("source" ,source))
                         '())
                   ,@inputs
                   ,@(standard-packages)))
    (build-inputs `(("uglify-js" ,uglify-js)
                    ,@native-inputs))
    (outputs outputs)
    (build minify-build)
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (minify-build store name inputs
                       #:key
                       (javascript-files #f)
                       (phases '(@ (guix build minify-build-system)
                                   %standard-phases))
                       (outputs '("out"))
                       (system (%current-system))
                       search-paths
                       (guile #f)
                       (imported-modules %minify-build-system-modules)
                       (modules '((guix build minify-build-system)
                                  (guix build utils))))
  "Build SOURCE with INPUTS."
  (define builder
    `(begin
       (use-modules ,@modules)
       (minify-build #:name ,name
                     #:source ,(match (assoc-ref inputs "source")
                                 (((? derivation? source))
                                  (derivation->output-path source))
                                 ((source)
                                  source)
                                 (source
                                  source))
                     #:javascript-files ,javascript-files
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

(define minify-build-system
  (build-system
    (name 'minify)
    (description "The trivial JavaScript minification build system")
    (lower lower)))

;;; minify.scm ends here
