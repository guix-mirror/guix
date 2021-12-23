;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (guix build-system linux-module)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export (%linux-module-build-system-modules
            linux-module-build
            linux-module-build-system))

;; Commentary:
;;
;; Code:

(define %linux-module-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build linux-module-build-system)
    ,@%gnu-build-system-modules))

(define (default-linux)
  "Return the default Linux package."

  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages linux))))
    (module-ref module 'linux-libre)))

(define (system->arch system)
  (let ((module (resolve-interface '(gnu packages linux))))
    ((module-ref module 'system->linux-architecture) system)))

(define (make-linux-module-builder linux)
  (package
    (inherit linux)
    (name (string-append (package-name linux) "-module-builder"))
    (inputs
     `(("linux" ,linux)))
    (arguments
     (substitute-keyword-arguments (package-arguments linux)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'build
              (lambda _
                (invoke "make" "modules_prepare")))
            (delete 'strip)             ; faster
            (replace 'install
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((out-lib-build (string-append #$output "/lib/modules/build")))
                  ;; Delete some huge items that we probably don't need.
                  ;; TODO: Only preserve the minimum, i.e. [Kbuild], Kconfig,
                  ;; scripts, include, ".config".
                  (copy-recursively "." out-lib-build)
                  (for-each (lambda (name)
                              (when (file-exists? name)
                                (delete-file-recursively name)))
                            (map (lambda (name)
                                   (string-append out-lib-build "/" name))
                                 '("arch"          ; 137 MB
                                   ;;"tools"       ; 44 MB built by our 'build phase
                                   "tools/testing" ; 14 MB
                                   "tools/perf"    ; 17 MB
                                   "drivers"       ; 600 MB
                                   "Documentation" ; 52 MB
                                   "fs"            ; 43 MB
                                   "net"           ; 33 MB
                                   "samples"       ; 2 MB
                                   "sound")))      ; 40 MB
                  ;; Reinstate arch/**/dts since "scripts/dtc" depends on it.
                  ;; Reinstate arch/**/include directories.
                  ;; Reinstate arch/**/Makefile.
                  ;; Reinstate arch/**/module.lds.
                  (for-each
                   (lambda (name)
                     (mkdir-p (dirname (string-append out-lib-build "/" name)))
                     (copy-recursively name
                                       (string-append out-lib-build "/" name)))
                   (append (find-files "arch" "^(dts|include)$"
                                       #:directories? #t)
                       (find-files "arch" "^(Makefile|module.lds)$")))
                  (let* ((linux #$(this-package-input "linux")))
                    (install-file (string-append linux "/System.map")
                                  out-lib-build)
                    (let ((source (string-append linux "/Module.symvers")))
                      (when (file-exists? source)
                        (install-file source out-lib-build)))))))))))))

(define* (lower name
                #:key source inputs native-inputs outputs
                system target
                (linux (default-linux))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    `(#:target #:gcc #:kmod #:linux #:inputs #:native-inputs
      ,@(if target '() '(#:target))))

  (bag
    (name name)
    (system system) (target target)
    (build-inputs `(,@(if source
                          `(("source" ,source))
                          '())
                    ,@native-inputs
                    ;; TODO: Remove "gmp", "mpfr", "mpc" since they are
                    ;; only needed to compile the gcc plugins.  Maybe
                    ;; remove "flex", "bison", "elfutils", "perl",
                    ;; "openssl".  That leaves very little ("bc", "gcc",
                    ;; "kmod").
                    ,@(package-native-inputs linux)
                    ,@(if target
                          ;; Use the standard cross inputs of
                          ;; 'gnu-build-system'.
                          (standard-cross-packages target 'host)
                          '())
                    ;; Keep the standard inputs of 'gnu-build-system'.
                    ,@(standard-packages)))
    (host-inputs `(,@inputs
                   ("linux" ,linux)
                   ("linux-module-builder"
                    ,(make-linux-module-builder linux))))
    (target-inputs (if target
                       (standard-cross-packages target 'target)
                       '()))
    (outputs outputs)
    (build (if target linux-module-build-cross linux-module-build))
    (arguments (strip-keyword-arguments private-keywords arguments))))

(define* (linux-module-build name inputs
                             #:key
                             source target
                             (search-paths '())
                             (tests? #t)
                             (phases '%standard-phases)
                             (outputs '("out"))
                             (make-flags ''())
                             (parallel-build? #t)
                             (system (%current-system))
                             (source-directory ".")
                             (guile #f)
                             (substitutable? #t)
                             (imported-modules
                              %linux-module-build-system-modules)
                             (modules '((guix build linux-module-build-system)
                                        (guix build utils))))
  "Build SOURCE using LINUX, and with INPUTS."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))
          #$(with-build-variables inputs outputs
              #~(linux-module-build #:name #$name
                                    #:source #+source
                                    #:source-directory #$source-directory
                                    #:search-paths '#$(sexp->gexp
                                                       (map search-path-specification->sexp
                                                            search-paths))
                                    #:phases #$phases
                                    #:system #$system
                                    #:target #$target
                                    #:arch #$(system->arch (or target system))
                                    #:tests? #$tests?
                                    #:outputs #$(outputs->gexp outputs)
                                    #:make-flags #$make-flags
                                    #:parallel-build? #$parallel-build?
                                    #:inputs #$(input-tuples->gexp inputs))))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:guile-for-build guile
                      #:substitutable? substitutable?)))

(define* (linux-module-build-cross
          name
          #:key
          source target
          build-inputs target-inputs host-inputs
          (guile #f)
          (outputs '("out"))
          (make-flags ''())
          (parallel-build? #t)
          (search-paths '())
          (native-search-paths '())
          (tests? #f)
          (phases '%standard-phases)
          (system (%current-system))
          (substitutable? #t)
          (imported-modules
           %linux-module-build-system-modules)
          (modules '((guix build linux-module-build-system)
                     (guix build utils))))
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))

          (define %build-host-inputs
            '#+(input-tuples->gexp build-inputs))

          (define %build-target-inputs
            (append #$(input-tuples->gexp host-inputs)
                    #+(input-tuples->gexp target-inputs)))

          (linux-module-build #:name #$name
                              #:source #+source
                              #:system #$system
                              #:target #$target
                              #:arch #$(system->arch (or target system))
                              #:outputs #$(outputs->gexp outputs)
                              #:make-flags #$make-flags
                              #:inputs %build-target-inputs
                              #:native-inputs %build-host-inputs
                              #:search-paths
                              '#$(sexp->gexp
                                  (map search-path-specification->sexp
                                       search-paths))
                              #:native-search-paths
                              '#$(map
                                  search-path-specification->sexp
                                  native-search-paths)
                              #:phases #$phases
                              #:tests? #$tests?))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:guile-for-build guile
                      #:substitutable? substitutable?)))

(define linux-module-build-system
  (build-system
    (name 'linux-module)
    (description "The Linux module build system")
    (lower lower)))

;;; linux-module.scm ends here
