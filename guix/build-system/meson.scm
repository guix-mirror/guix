;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
;;; Copyright © 2018, 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export (%meson-build-system-modules
            meson-build-system
            make-cross-file))

;; Commentary:
;;
;; Standard build procedure for packages using Meson. This is implemented as an
;; extension of `gnu-build-system', with the option to turn on the glib/gtk
;; phases from `glib-or-gtk-build-system'.
;;
;; Code:

(define (make-machine-alist triplet)
  "Make an association list describing what should go into
the ‘host_machine’ section of the cross file when cross-compiling
for TRIPLET."
  `((system . ,(cond ((target-hurd? triplet) "gnu")
                     ((target-linux? triplet) "linux")
                     ((target-mingw? triplet) "windows")
                     (#t (error "meson: unknown operating system"))))
    (cpu_family . ,(cond ((target-x86-32? triplet) "x86")
                         ((target-x86-64? triplet) "x86_64")
                         ((target-arm32? triplet) "arm")
                         ((target-aarch64? triplet) "aarch64")
                         ((target-powerpc? triplet)
                          (if (target-64bit? triplet)
                              "ppc64"
                              "ppc"))
                         (#t (error "meson: unknown architecture"))))
    (cpu . ,(cond ((target-x86-32? triplet) ; i386, ..., i686
                   (substring triplet 0 4))
                  ((target-x86-64? triplet) "x86_64")
                  ((target-aarch64? triplet) "armv8-a")
                  ((target-arm32? triplet) "armv7")
                  ;; According to #mesonbuild on OFTC, there does not appear
                  ;; to be an official-ish list of CPU types recognised by
                  ;; Meson, the "cpu" field is not used by Meson itself and
                  ;; most software doesn't look at this field, except perhaps
                  ;; for selecting optimisations, so set it to something
                  ;; arbitrary.
                  (#t "strawberries")))
    (endian . ,(cond ((string-prefix? "powerpc64le-" triplet) "little")
                     ((string-prefix? "mips64el-" triplet) "little")
                     ((target-x86-32? triplet) "little")
                     ((target-x86-64? triplet) "little")
                     ;; At least in Guix.  Aarch64 and 32-bit arm
                     ;; have a big-endian mode as well.
                     ((target-arm? triplet) "little")
                     (#t (error "meson: unknown architecture"))))))

(define (make-binaries-alist triplet)
  "Make an associatoin list describing what should go into
the ‘binaries’ section of the cross file when cross-compiling for
TRIPLET."
  `((c . ,(cc-for-target triplet))
    (cpp . ,(cxx-for-target triplet))
    (pkgconfig . ,(pkg-config-for-target triplet))
    (objcopy . ,(string-append triplet "-objcopy"))
    (ar . ,(string-append triplet "-ar"))
    (ld . ,(string-append triplet "-ld"))
    (strip . ,(string-append triplet "-strip"))))

(define (make-cross-file triplet)
  (computed-file "cross-file"
    (with-imported-modules '((guix build meson-configuration))
      #~(begin
          (use-modules (guix build meson-configuration))
          (call-with-output-file #$output
            (lambda (port)
              (write-section-header port "host_machine")
              (write-assignments port '#$(make-machine-alist triplet))
              (write-section-header port "binaries")
              (write-assignments port '#$(make-binaries-alist triplet))))))))

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
    (module-ref module 'meson)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (meson (default-meson))
                (ninja (default-ninja))
                (glib-or-gtk? #f)
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    `(#:meson #:ninja #:inputs #:native-inputs #:outputs
      ,@(if target
            '()
            '(#:target))))

  (bag
    (name name)
    (system system) (target target)
    (build-inputs `(("meson" ,meson)
                    ("ninja" ,ninja)
                    ,@native-inputs
                    ,@(if target '() inputs)
                    ;; Keep the standard inputs of 'gnu-build-system'.
                    ,@(if target
                          (standard-cross-packages target 'host)
                          '())
                    ,@(standard-packages)))
    (host-inputs `(,@(if source
                         `(("source" ,source))
                         '())
                   ,@(if target inputs '())))
    ;; Keep the standard inputs of 'gnu-buid-system'.
    (target-inputs (if target
                       (standard-cross-packages target 'target)
                       '()))
    (outputs outputs)
    (build (if target meson-cross-build meson-build))
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (meson-build name inputs
                      #:key
                      guile source
                      (outputs '("out"))
                      (configure-flags ''())
                      (search-paths '())
                      (build-type "debugoptimized")
                      (tests? #t)
                      (test-options ''())
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
                      (phases '%standard-phases)
                      (system (%current-system))
                      (imported-modules %meson-build-system-modules)
                      (modules '((guix build meson-build-system)
                                 (guix build utils)))
                      allowed-references
                      disallowed-references)
  "Build SOURCE using MESON, and with INPUTS, assuming that SOURCE
has a 'meson.build' file."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          (define build-phases
            #$(let ((phases (if (pair? phases) (sexp->gexp phases) phases)))
                (if glib-or-gtk?
                    phases
                    #~(modify-phases #$phases
                        (delete 'glib-or-gtk-compile-schemas)
                        (delete 'glib-or-gtk-wrap)))))

          #$(with-build-variables inputs outputs
              #~(meson-build #:source #+source
                             #:system #$system
                             #:outputs %outputs
                             #:inputs %build-inputs
                             #:search-paths '#$(sexp->gexp
                                                (map search-path-specification->sexp
                                                     search-paths))
                             #:phases build-phases
                             #:configure-flags
                             #$(if (pair? configure-flags)
                                   (sexp->gexp configure-flags)
                                   configure-flags)
                             #:build-type #$build-type
                             #:tests? #$tests?
                             #:test-options #$(sexp->gexp test-options)
                             #:parallel-build? #$parallel-build?
                             #:parallel-tests? #$parallel-tests?
                             #:validate-runpath? #$validate-runpath?
                             #:patch-shebangs? #$patch-shebangs?
                             #:strip-binaries? #$strip-binaries?
                             #:strip-flags #$(sexp->gexp strip-flags)
                             #:strip-directories #$(sexp->gexp strip-directories)
                             #:elf-directories #$(sexp->gexp elf-directories))))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:target #f
                      #:graft? #f
                      #:substitutable? substitutable?
                      #:allowed-references allowed-references
                      #:disallowed-references disallowed-references
                      #:guile-for-build guile)))

(define* (meson-cross-build name
                            #:key
                            target
                            build-inputs host-inputs target-inputs
                            guile source
                            (outputs '("out"))
                            (configure-flags ''())
                            (search-paths '())
                            (native-search-paths '())

                            (build-type "debugoptimized")
                            (tests? #f)
                            (test-options ''())
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
                            ;; See 'gnu-cross-build' for why this needs to be
                            ;; disabled when cross-compiling.
                            (make-dynamic-linker-cache? #f)
                            (phases '%standard-phases)
                            (system (%current-system))
                            (imported-modules %meson-build-system-modules)
                            (modules '((guix build meson-build-system)
                                       (guix build utils)))
                            allowed-references
                            disallowed-references)
  "Cross-build SOURCE for TARGET using MESON, and with INPUTS, assuming that
SOURCE has a 'meson.build' file."
  (define cross-file
    (make-cross-file target))
  (define inputs
    (if (null? target-inputs)
        (input-tuples->gexp host-inputs)
        #~(append #$(input-tuples->gexp host-inputs)
              #+(input-tuples->gexp target-inputs))))
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          (define %build-host-inputs
            #+(input-tuples->gexp build-inputs))

          (define %build-target-inputs
            (append #$(input-tuples->gexp host-inputs)
                    #+(input-tuples->gexp target-inputs)))

          (define %build-inputs
            (append %build-host-inputs %build-target-inputs))

          (define %outputs
            #$(outputs->gexp outputs))

          (define build-phases
            #$(let ((phases (if (pair? phases) (sexp->gexp phases) phases)))
                (if glib-or-gtk?
                    phases
                    #~(modify-phases #$phases
                        (delete 'glib-or-gtk-compile-schemas)
                        (delete 'glib-or-gtk-wrap)))))

          ;; Do not use 'with-build-variables', as there should be
          ;; no reason to use %build-inputs and friends.
          (meson-build #:source #+source
                       #:system #$system
                       #:build #$(nix-system->gnu-triplet system)
                       #:target #$target
                       #:outputs #$(outputs->gexp outputs)
                       #:inputs #$inputs
                       #:native-inputs #+(input-tuples->gexp build-inputs)
                       #:search-paths '#$(sexp->gexp
                                          (map search-path-specification->sexp
                                               search-paths))
                       #:native-search-paths '#$(sexp->gexp
                                                 (map search-path-specification->sexp
                                                      native-search-paths))
                       #:phases build-phases
                       #:make-dynamic-linker-cache? #$make-dynamic-linker-cache?
                       #:configure-flags `("--cross-file" #+cross-file
                                           ,@#$(if (pair? configure-flags)
                                                   (sexp->gexp configure-flags)
                                                   configure-flags))
                       #:build-type #$build-type
                       #:tests? #$tests?
                       #:test-options #$(sexp->gexp test-options)
                       #:parallel-build? #$parallel-build?
                       #:parallel-tests? #$parallel-tests?
                       #:validate-runpath? #$validate-runpath?
                       #:patch-shebangs? #$patch-shebangs?
                       #:strip-binaries? #$strip-binaries?
                       #:strip-flags #$(sexp->gexp strip-flags)
                       #:strip-directories #$(sexp->gexp strip-directories)
                       #:elf-directories #$(sexp->gexp elf-directories)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:target target
                      #:graft? #f
                      #:substitutable? substitutable?
                      #:allowed-references allowed-references
                      #:disallowed-references disallowed-references
                      #:guile-for-build guile)))

(define meson-build-system
  (build-system
    (name 'meson)
    (description "The standard Meson build system")
    (lower lower)))

;;; meson.scm ends here
