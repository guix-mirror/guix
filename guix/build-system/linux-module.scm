;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
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

(define (default-kmod)
  "Return the default kmod package."

  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages linux))))
    (module-ref module 'kmod)))

(define (default-gcc)
  "Return the default gcc package."

  ;; Do not use `@' to avoid introducing circular dependencies.
  (let ((module (resolve-interface '(gnu packages gcc))))
    (module-ref module 'gcc-7)))

(define (make-linux-module-builder linux)
  (package
    (inherit linux)
    (name (string-append (package-name linux) "-module-builder"))
    (native-inputs
     `(("linux" ,linux)
       ,@(package-native-inputs linux)))
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
                ; TODO: Only preserve the minimum, i.e. [Kbuild], Kconfig, scripts, include, ".config".
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
    '(#:source #:target #:gcc #:kmod #:linux #:inputs #:native-inputs))

  (and (not target)                               ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs
                        ,@(standard-packages)))
         (build-inputs `(("linux" ,linux) ; for "Module.symvers".
                         ("linux-module-builder"
                         ,(make-linux-module-builder linux))
                         ,@native-inputs
                         ;; TODO: Remove "gmp", "mpfr", "mpc" since they are only needed to compile the gcc plugins.  Maybe remove "flex", "bison", "elfutils", "perl", "openssl".  That leaves very little ("bc", "gcc", "kmod").
                         ,@(package-native-inputs linux)))
         (outputs outputs)
         (build linux-module-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (linux-module-build store name inputs
                             #:key
                             (search-paths '())
                             (tests? #t)
                             (phases '(@ (guix build linux-module-build-system)
                                         %standard-phases))
                             (outputs '("out"))
                             (system (%current-system))
                             (guile #f)
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
                                #:guile-for-build guile-for-build))

(define linux-module-build-system
  (build-system
    (name 'linux-module)
    (description "The Linux module build system")
    (lower lower)))

;;; linux-module.scm ends here
