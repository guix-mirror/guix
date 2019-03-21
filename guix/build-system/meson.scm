;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
;;; Copyright © 2018, 2019 Marius Bakke <mbakke@fastmail.com>
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

(define-module (guix build-system meson)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export (%meson-build-system-modules
            meson-build-system))

;; Commentary:
;;
;; Standard build procedure for packages using Meson. This is implemented as an
;; extension of `gnu-build-system', with the option to turn on the glib/gtk
;; phases from `glib-or-gtk-build-system'.
;;
;; Code:

(define %meson-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build meson-build-system)
    ;; The modules from glib-or-gtk contains the modules from gnu-build-system,
    ;; so there is no need to import that too.
    ,@%glib-or-gtk-build-system-modules))

(define (default-ninja)
  "Return the default ninja package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((module (resolve-interface '(gnu packages ninja))))
    (module-ref module 'ninja)))

(define (default-meson)
  "Return the default meson package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((module (resolve-interface '(gnu packages build-tools))))
    (module-ref module 'meson-for-build)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (meson (default-meson))
                (ninja (default-ninja))
                (glib-or-gtk? #f)
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    `(#:source #:meson #:ninja #:inputs #:native-inputs #:outputs #:target))

  (and (not target) ;; TODO: add support for cross-compilation.
       (bag
         (name name)
         (system system)
         (build-inputs `(("meson" ,meson)
                         ("ninja" ,ninja)
                         ,@native-inputs))
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs
                        ;; Keep the standard inputs of 'gnu-build-system'.
                        ,@(standard-packages)))
         (outputs outputs)
         (build meson-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (meson-build store name inputs
                      #:key (guile #f)
                      (outputs '("out"))
                      (configure-flags ''())
                      (search-paths '())
                      (build-type "plain")
                      (tests? #t)
                      (test-target "test")
                      (glib-or-gtk? #f)
                      (parallel-build? #t)
                      (parallel-tests? #f)
                      (validate-runpath? #t)
                      (patch-shebangs? #t)
                      (strip-binaries? #t)
                      (strip-flags ''("--strip-debug"))
                      (strip-directories ''("lib" "lib64" "libexec"
                                            "bin" "sbin"))
                      (elf-directories ''("lib" "lib64" "libexec"
                                          "bin" "sbin"))
                      (phases '(@ (guix build meson-build-system)
                                  %standard-phases))
                      (system (%current-system))
                      (imported-modules %meson-build-system-modules)
                      (modules '((guix build meson-build-system)
                                 (guix build utils)))
                      allowed-references
                      disallowed-references)
  "Build SOURCE using MESON, and with INPUTS, assuming that SOURCE
has a 'meson.build' file."

  ;; TODO: Copied from build-system/gnu, factorize this!
  (define canonicalize-reference
    (match-lambda
     ((? package? p)
      (derivation->output-path (package-derivation store p system
                                                   #:graft? #f)))
     (((? package? p) output)
      (derivation->output-path (package-derivation store p system
                                                   #:graft? #f)
                               output))
     ((? string? output)
      output)))

  (define builder
    `(let ((build-phases (if ,glib-or-gtk?
                             ,phases
                             (modify-phases ,phases
                               (delete 'glib-or-gtk-compile-schemas)
                               (delete 'glib-or-gtk-wrap)))))
       (use-modules ,@modules)
       (meson-build #:source ,(match (assoc-ref inputs "source")
                                (((? derivation? source))
                                 (derivation->output-path source))
                                ((source)
                                 source)
                                (source
                                 source))
                    #:system ,system
                    #:outputs %outputs
                    #:inputs %build-inputs
                    #:search-paths ',(map search-path-specification->sexp
                                          search-paths)
                    #:phases build-phases
                    #:configure-flags ,configure-flags
                    #:build-type ,build-type
                    #:tests? ,tests?
                    #:test-target ,test-target
                    #:parallel-build? ,parallel-build?
                    #:parallel-tests? ,parallel-tests?
                    #:validate-runpath? ,validate-runpath?
                    #:patch-shebangs? ,patch-shebangs?
                    #:strip-binaries? ,strip-binaries?
                    #:strip-flags ,strip-flags
                    #:strip-directories ,strip-directories
                    #:elf-directories ,elf-directories)))

  (define guile-for-build
    (match guile
      ((? package?)
       (package-derivation store guile system #:graft? #f))
      (#f                                         ; the default
       (let* ((distro (resolve-interface '(gnu packages commencement)))
              (guile  (module-ref distro 'guile-final)))
         (package-derivation store guile system #:graft? #f)))))

  (build-expression->derivation store name builder
                                #:system system
                                #:inputs inputs
                                #:modules imported-modules
                                #:outputs outputs
                                #:guile-for-build guile-for-build
                                #:allowed-references
                                (and allowed-references
                                     (map canonicalize-reference
                                          allowed-references))
                                #:disallowed-references
                                (and disallowed-references
                                     (map canonicalize-reference
                                          disallowed-references))))

(define meson-build-system
  (build-system
    (name 'meson)
    (description "The standard Meson build system")
    (lower lower)))

;;; meson.scm ends here
