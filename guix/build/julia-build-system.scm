;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Nicolò Balzarotti <nicolo@nixo.xyz>
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


(define-module (guix build julia-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:export (%standard-phases
            julia-create-package-toml
            julia-build))

;; Commentary:
;;
;; Builder-side code of the standard build procedure for Julia packages.
;;
;; Code:

(define (invoke-julia code)
  (invoke "julia" "-e" code))

;; subpath where we store the package content
(define %package-path "/share/julia/packages/")

(define (generate-load-path inputs outputs)
  (string-append
   (string-join (map (match-lambda
                       ((_ . path)
                        (string-append path %package-path)))
                     ;; Restrict to inputs beginning with "julia-".
                     (filter (match-lambda
                               ((name . _)
                                (string-prefix? "julia-" name)))
                             inputs))
                ":")
   (string-append ":" (assoc-ref outputs "out") %package-path)
   ;; stdlib is always required to find Julia's standard libraries.
   ;; usually there are other two paths in this variable:
   ;; "@" and "@v#.#"
   ":@stdlib"))

(define* (install #:key source inputs outputs #:allow-other-keys)
  (let* ((out (assoc-ref outputs "out"))
         (package-dir (string-append out %package-path
                                     (string-append
                                      (strip-store-file-name source)))))
    (setenv "JULIA_LOAD_PATH" (generate-load-path inputs outputs))
    (mkdir-p package-dir)
    (copy-recursively source package-dir))
  #t)

;; TODO: Precompilation is working, but I don't know how to tell
;; julia to use use it. If (on rantime) we set HOME to
;; store path, julia tries to write files there (failing)
(define* (precompile #:key source inputs outputs #:allow-other-keys)
  (let* ((out (assoc-ref outputs "out"))
         (builddir (string-append out "/share/julia/"))
         (package (strip-store-file-name source)))
    (mkdir-p builddir)
    (setenv "JULIA_DEPOT_PATH" builddir)
    (setenv "JULIA_LOAD_PATH" (generate-load-path inputs outputs))
    ;; Actual precompilation
    (invoke-julia (string-append "using " package)))
  #t)

(define* (check #:key source inputs outputs #:allow-other-keys)
  (let* ((out (assoc-ref outputs "out"))
         (package (strip-store-file-name source))
         (builddir (string-append out "/share/julia/")))
    (setenv "JULIA_DEPOT_PATH" builddir)
    (setenv "JULIA_LOAD_PATH" (generate-load-path inputs outputs))
    (invoke-julia (string-append "using Pkg;Pkg.test(\"" package "\")")))
  #t)

(define (julia-create-package-toml outputs source
                                   name uuid version
                                   deps)
  "Some packages are not using the new Package.toml dependency specifications.
Write this file manually, so that Julia can find its dependencies."
  (let ((f (open-file
            (string-append
             (assoc-ref outputs "out")
             %package-path
             (string-append
              name "/Project.toml"))
            "w")))
    (display (string-append
              "
name = \"" name "\"
uuid = \"" uuid "\"
version = \"" version "\"
") f)
    (when (not (null? deps))
      (display "[deps]\n" f)
      (for-each (lambda dep
                  (display (string-append (car (car dep)) " = \"" (cdr (car dep)) "\"\n")
                           f))
                deps))
    (close-port f))
  #t)

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'check) ; tests must be run after installation
    (replace 'install install)
    (add-after 'install 'precompile precompile)
    ;; (add-after 'install 'check check)
    ;; TODO: In the future we could add a "system-image-generation" phase
    ;; where we use PackageCompiler.jl to speed up package loading times
    (delete 'configure)
    (delete 'bootstrap)
    (delete 'patch-usr-bin-file)
    (delete 'build)))

(define* (julia-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Julia package, applying all of PHASES in order."
  (apply gnu:gnu-build
         #:inputs inputs #:phases phases
         args))
