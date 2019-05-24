;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix derivations)
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

(define (bioconductor-uri name version)
  "Return a URI string for the R package archive on Bioconductor for the
release corresponding to NAME and VERSION."
  (list (string-append "https://bioconductor.org/packages/release/bioc/src/contrib/"
                       name "_" version ".tar.gz")
        ;; TODO: use %bioconductor-version from (guix import cran)
        (string-append "https://bioconductor.org/packages/3.9/bioc/src/contrib/Archive/"
                       name "_" version ".tar.gz")))

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
    '(#:source #:target #:r #:inputs #:native-inputs))

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

(define* (r-build store name inputs
                  #:key
                  (tests? #t)
                  (test-target "tests")
                  (configure-flags ''())
                  (phases '(@ (guix build r-build-system)
                              %standard-phases))
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
    `(begin
       (use-modules ,@modules)
       (r-build #:name ,name
                #:source ,(match (assoc-ref inputs "source")
                            (((? derivation? source))
                             (derivation->output-path source))
                            ((source)
                             source)
                            (source
                             source))
                #:configure-flags ,configure-flags
                #:system ,system
                #:tests? ,tests?
                #:test-target ,test-target
                #:phases ,phases
                #:outputs %outputs
                #:search-paths ',(map search-path-specification->sexp
                                      search-paths)
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
                                #:inputs inputs
                                #:system system
                                #:modules imported-modules
                                #:outputs outputs
                                #:guile-for-build guile-for-build
                                #:substitutable? substitutable?))

(define r-build-system
  (build-system
    (name 'r)
    (description "The standard R build system")
    (lower lower)))

;;; r.scm ends here
