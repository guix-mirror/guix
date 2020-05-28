;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (guix build-system rebar3)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (%rebar3-build-system-modules
            rebar3-build
            rebar3-build-system))

;;
;; Standard build procedure for Erlang packages using Rebar3.
;;

(define %rebar3-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build rebar3-build-system)
    ,@%gnu-build-system-modules))

(define (default-rebar3)
  "Return the default Rebar3 package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((erlang-mod (resolve-interface '(gnu packages erlang))))
    (module-ref erlang-mod 'rebar3)))

(define (default-erlang)
  "Return the default Erlang package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((erlang-mod (resolve-interface '(gnu packages erlang))))
    (module-ref erlang-mod 'erlang)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (rebar (default-rebar3))
                (erlang (default-erlang))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:source #:target #:rebar #:inputs #:native-inputs))

  (and (not target)                               ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs))
         (build-inputs `(("rebar" ,rebar)
                         ("erlang" ,erlang) ;; for escriptize
                         ,@native-inputs
                         ;; Keep the standard inputs of 'gnu-build-system'.
                         ,@(standard-packages)))
         (outputs outputs)
         (build rebar3-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (rebar3-build store name inputs
                    #:key
                    (tests? #t)
                    (test-target "eunit")
                    (configure-flags ''())
                    (make-flags ''("skip_deps=true" "-vv"))
                    (build-target "compile")
                    ;; TODO: pkg-name
                    (phases '(@ (guix build rebar3-build-system)
                                %standard-phases))
                    (outputs '("out"))
                    (search-paths '())
                    (system (%current-system))
                    (guile #f)
                    (imported-modules %rebar3-build-system-modules)
                    (modules '((guix build rebar3-build-system)
                               (guix build utils))))
  "Build SOURCE with INPUTS."
  (define builder
    `(begin
       (use-modules ,@modules)
       (rebar3-build #:name ,name
                  #:source ,(match (assoc-ref inputs "source")
                              (((? derivation? source))
                               (derivation->output-path source))
                              ((source)
                               source)
                              (source
                               source))
                  #:make-flags ,make-flags
                  #:configure-flags ,configure-flags
                  #:system ,system
                  #:tests? ,tests?
                  #:test-target ,test-target
                  #:build-target ,build-target
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

(define rebar3-build-system
  (build-system
    (name 'rebar3)
    (description "The standard Rebar3 build system")
    (lower lower)))
