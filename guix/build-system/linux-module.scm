;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
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
  #:use-module (guix derivations)
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
       `(modify-phases ,phases
          (replace 'build
            (lambda _
              (invoke "make" "modules_prepare")))
          (delete 'strip) ; faster.
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (out-lib-build (string-append out "/lib/modules/build")))
                ;; TODO: Only preserve the minimum, i.e. [Kbuild], Kconfig,
                ;; scripts, include, ".config".
                (copy-recursively "." out-lib-build)
                (let* ((linux (assoc-ref inputs "linux")))
                  (install-file (string-append linux "/System.map")
                                out-lib-build)
                  (let ((source (string-append linux "/Module.symvers")))
                    (if (file-exists? source)
                        (install-file source out-lib-build))))
                #t)))))))))

(define* (lower name
                #:key source inputs native-inputs outputs
                system target
                (linux (default-linux))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    `(#:source #:target #:gcc #:kmod #:linux #:inputs #:native-inputs
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

(define* (linux-module-build store name inputs
                             #:key
                             target
                             (search-paths '())
                             (tests? #t)
                             (phases '(@ (guix build linux-module-build-system)
                                         %standard-phases))
                             (outputs '("out"))
                             (system (%current-system))
                             (guile #f)
                             (substitutable? #t)
                             (imported-modules
                              %linux-module-build-system-modules)
                             (modules '((guix build linux-module-build-system)
                                        (guix build utils))))
  "Build SOURCE using LINUX, and with INPUTS."
  (define builder
    `(begin
       (use-modules ,@modules)
       (linux-module-build #:name ,name
                     #:source ,(match (assoc-ref inputs "source")
                                      (((? derivation? source))
                                       (derivation->output-path source))
                                      ((source)
                                       source)
                                      (source
                                        source))
                     #:search-paths ',(map search-path-specification->sexp
                                           search-paths)
                     #:phases ,phases
                     #:system ,system
                     #:target ,target
                     #:arch ,(system->arch (or target system))
                     #:tests? ,tests?
                     #:outputs %outputs
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
                                #:system system
                                #:inputs inputs
                                #:modules imported-modules
                                #:outputs outputs
                                #:guile-for-build guile-for-build
                                #:substitutable? substitutable?))

(define* (linux-module-build-cross
          store name
          #:key
          target native-drvs target-drvs
          (guile #f)
          (outputs '("out"))
          (search-paths '())
          (native-search-paths '())
          (tests? #f)
          (phases '(@ (guix build linux-module-build-system)
                      %standard-phases))
          (system (%current-system))
          (substitutable? #t)
          (imported-modules
           %linux-module-build-system-modules)
          (modules '((guix build linux-module-build-system)
                     (guix build utils))))
  (define builder
    `(begin
       (use-modules ,@modules)
       (let ()
         (define %build-host-inputs
           ',(map (match-lambda
                    ((name (? derivation? drv) sub ...)
                     `(,name . ,(apply derivation->output-path drv sub)))
                    ((name path)
                     `(,name . ,path)))
                  native-drvs))

         (define %build-target-inputs
           ',(map (match-lambda
                    ((name (? derivation? drv) sub ...)
                     `(,name . ,(apply derivation->output-path drv sub)))
                    ((name (? package? pkg) sub ...)
                     (let ((drv (package-cross-derivation store pkg
                                                          target system)))
                       `(,name . ,(apply derivation->output-path drv sub))))
                    ((name path)
                     `(,name . ,path)))
                  target-drvs))

         (linux-module-build #:name ,name
                             #:source ,(match (assoc-ref native-drvs "source")
                                         (((? derivation? source))
                                          (derivation->output-path source))
                                         ((source)
                                          source)
                                         (source
                                          source))
                             #:system ,system
                             #:target ,target
                             #:arch ,(system->arch (or target system))
                             #:outputs %outputs
                             #:inputs %build-target-inputs
                             #:native-inputs %build-host-inputs
                             #:search-paths
                             ',(map search-path-specification->sexp
                                    search-paths)
                             #:native-search-paths
                             ',(map
                                search-path-specification->sexp
                                native-search-paths)
                             #:phases ,phases
                             #:tests? ,tests?))))

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
                                #:inputs (append native-drvs target-drvs)
                                #:outputs outputs
                                #:modules imported-modules
                                #:guile-for-build guile-for-build
                                #:substitutable? substitutable?))

(define linux-module-build-system
  (build-system
    (name 'linux-module)
    (description "The Linux module build system")
    (lower lower)))

;;; linux-module.scm ends here
