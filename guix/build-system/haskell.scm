;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2020 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2020 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
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

(define-module (guix build-system haskell)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix download)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (hackage-uri

            %haskell-build-system-modules
            haskell-build
            haskell-build-system))

;; Commentary:
;;
;; Standard build procedure for Haskell packages using 'Setup.hs'.  This is
;; implemented as an extension of 'gnu-build-system'.
;;
;; Code:

(define (hackage-uri name version)
  "Return a URI string for the Haskell package hosted on Hackage corresponding
to NAME and VERSION."
  (string-append "https://hackage.haskell.org/package/" name "/"
                 name "-" version ".tar.gz"))

(define %haskell-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build haskell-build-system)
    ,@%gnu-build-system-modules))

(define (default-haskell)
  "Return the default Haskell package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((haskell (resolve-interface '(gnu packages haskell))))
    (module-ref haskell 'ghc)))

(define (source-url->revision-url url revision)
  "Convert URL (a Hackage source URL) to the URL for the Cabal file at
version REVISION."
  (let* ((last-slash (string-rindex url #\/))
         (next-slash (string-rindex url #\/ 0 last-slash)))
    (string-append (substring url 0 next-slash)
                   (substring url last-slash (- (string-length url)
                                                (string-length ".tar.gz")))
                   "/revision/" revision ".cabal")))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (haskell (default-haskell))
                cabal-revision
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:haskell #:cabal-revision #:inputs #:native-inputs #:outputs))

  (define (cabal-revision->origin cabal-revision)
    (match cabal-revision
      ((revision hash)
       (origin
         (method url-fetch)
         (uri (source-url->revision-url (origin-uri source) revision))
         (sha256 (base32 hash))
         (file-name (string-append name "-" revision ".cabal"))))
      (#f #f)))

  (and (not target)                               ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@(match (cabal-revision->origin cabal-revision)
                            (#f '())
                            (revision `(("cabal-revision" ,revision))))
                        ,@inputs

                        ;; Keep the standard inputs of 'gnu-build-system'.
                        ,@(standard-packages)))
         (build-inputs `(("haskell" ,haskell)
                         ,@native-inputs))
         ;; XXX: this is a hack to get around issue #41569.
         (outputs (match outputs
                    (("out") (cons "static" outputs))
                    (_ outputs)))
         (build haskell-build)
         (arguments
          (substitute-keyword-arguments
              (strip-keyword-arguments private-keywords arguments)
            ((#:extra-directories extra-directories)
             `(list ,@(append-map
                       (lambda (name)
                         (match (assoc name inputs)
                           ((_ pkg)
                            (match (package-transitive-propagated-inputs pkg)
                              (((propagated-names . _) ...)
                               (cons name propagated-names))))))
                       extra-directories))))))))

(define* (haskell-build name inputs
                        #:key source
                        (haddock? #t)
                        (haddock-flags ''())
                        (tests? #t)
                        (test-target "test")
                        ;; FIXME: Parallel builds lead to indeterministic
                        ;; results, see <http://issues.guix.gnu.org/43843#3>.
                        (parallel-build? #f)
                        (configure-flags ''())
                        (extra-directories ''())
                        (phases '%standard-phases)
                        (outputs '("out" "static"))
                        (search-paths '())
                        (system (%current-system))
                        (guile #f)
                        (imported-modules %haskell-build-system-modules)
                        (modules '((guix build haskell-build-system)
                                   (guix build utils))))
  "Build SOURCE using HASKELL, and with INPUTS.  This assumes that SOURCE
provides a 'Setup.hs' file as its build system."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          #$(with-build-variables inputs outputs
              #~(haskell-build #:name #$name
                               #:source #+source

                               ;; XXX: INPUTS contains <gexp-input> records as
                               ;; opposed to raw lowerable objects, hence the
                               ;; use of ungexp-splicing.
                               #:cabal-revision
                               #$@(match (assoc-ref inputs "cabal-revision")
                                    (#f '(#f))
                                    (lst lst))

                               #:configure-flags #$configure-flags
                               #:extra-directories #$extra-directories
                               #:extra-directories #$extra-directories
                               #:haddock-flags #$haddock-flags
                               #:system #$system
                               #:test-target #$test-target
                               #:tests? #$tests?
                               #:parallel-build? #$parallel-build?
                               #:haddock? #$haddock?
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
                      #:guile-for-build guile)))

(define haskell-build-system
  (build-system
    (name 'haskell)
    (description "The standard Haskell build system")
    (lower lower)))

;;; haskell.scm ends here
