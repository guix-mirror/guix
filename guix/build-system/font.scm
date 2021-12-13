;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (guix build-system font)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:export (%font-build-system-modules
            font-build
            font-build-system))

;; Commentary:
;;
;; Standard build procedure for fonts.  This is implemented as an extension of
;; 'gnu-build-system'.
;;
;; Code:

(define %font-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build font-build-system)
    ,@%gnu-build-system-modules))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:inputs #:native-inputs))

  (bag
    (name name)
    (system system)
    (host-inputs `(,@(if source
                         `(("source" ,source))
                         '())
                   ,@inputs
                   ,(list "tar" (module-ref (resolve-interface '(gnu packages base)) 'tar))
                   ,@(let ((compression (resolve-interface '(gnu packages compression))))
                       (map (match-lambda
                              ((name package)
                               (list name (module-ref compression package))))
                            `(("gzip" gzip)
                              ("bzip2" bzip2)
                              ("unzip" unzip)
                              ("xz" xz))))))
    (build-inputs native-inputs)
    (outputs outputs)
    (build font-build)
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (font-build name inputs
                     #:key source
                     (tests? #t)
                     (test-target "test")
                     (configure-flags ''())
                     (phases '%standard-phases)
                     (outputs '("out"))
                     (search-paths '())
                     (system (%current-system))
                     (guile #f)
                     (imported-modules %font-build-system-modules)
                     (modules '((guix build font-build-system)
                                (guix build utils))))
  "Build SOURCE with INPUTS."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@modules)

          #$(with-build-variables inputs outputs
              #~(font-build #:name #$name
                            #:source #+source
                            #:configure-flags #$configure-flags
                            #:system #$system
                            #:test-target #$test-target
                            #:tests? #$tests?
                            #:phases #$(if (pair? phases)
                                           (sexp->gexp phases)
                                           phases)
                            #:outputs %outputs
                            #:search-paths '#$(sexp->gexp
                                               (map search-path-specification->sexp
                                                    search-paths))
                            #:inputs %build-inputs)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:target #f
                      #:guile-for-build guile)))

(define font-build-system
  (build-system
    (name 'font)
    (description "The build system for font packages")
    (lower lower)))

;;; font.scm ends here
