;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017, 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix build-system r)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (%r-build-system-modules
            r-build
            r-build-system
            cran-uri
            bioconductor-uri))

;; Commentary:
;;
;; Standard build procedure for R packages.
;;
;; Code:

(define (cran-uri name version)
  "Return a list of URI strings for the R package archive on CRAN for the
release corresponding to NAME and VERSION.  As only the most recent version is
available via the first URI, the second URI points to the archived version."
  (list (string-append "mirror://cran/src/contrib/"
                       name "_" version ".tar.gz")
        (string-append "mirror://cran/src/contrib/Archive/"
                       name "/" name "_" version ".tar.gz")))

(define* (bioconductor-uri name version #:optional type)
  "Return a URI string for the R package archive on Bioconductor for the
release corresponding to NAME and VERSION."
  (let ((type-url-part (match type
                         ('annotation "/data/annotation")
                         ('experiment "/data/experiment")
                         (_ "/bioc"))))
    (list (string-append "https://bioconductor.org/packages/release"
                         type-url-part
                         "/src/contrib/"
                         name "_" version ".tar.gz")
          ;; TODO: use %bioconductor-version from (guix import cran)
          (string-append "https://bioconductor.org/packages/3.14"
                         type-url-part
                         "/src/contrib/"
                         name "_" version ".tar.gz"))))

(define %r-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build r-build-system)
    ,@%gnu-build-system-modules))

(define (default-r)
  "Return the default R package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((r-mod (resolve-interface '(gnu packages statistics))))
    (module-ref r-mod 'r-minimal)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (r (default-r))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:r #:inputs #:native-inputs))

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
         (build-inputs `(("r" ,r)
                         ,@native-inputs))
         (outputs outputs)
         (build r-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (r-build name inputs
                  #:key
                  source
                  (tests? #t)
                  (test-target "tests")
                  (configure-flags ''())
                  (phases '%standard-phases)
                  (outputs '("out"))
                  (search-paths '())
                  (system (%current-system))
                  (guile #f)
                  (substitutable? #t)
                  (imported-modules %r-build-system-modules)
                  (modules '((guix build r-build-system)
                             (guix build utils))))
  "Build SOURCE with INPUTS."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))
          (r-build #:name #$name
                   #:source #+source
                   #:configure-flags #$configure-flags
                   #:system #$system
                   #:tests? #$tests?
                   #:test-target #$test-target
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
                      #:guile-for-build guile
                      #:substitutable? substitutable?)))

(define r-build-system
  (build-system
    (name 'r)
    (description "The standard R build system")
    (lower lower)))

;;; r.scm ends here
