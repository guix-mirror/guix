;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2021 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2021, 2022 Simon Tournier <zimon.toutoune@gmail.com>
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
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (srfi srfi-1)
  #:export (%standard-phases
            julia-build))

;; Commentary:
;;
;; Builder-side code of the standard build procedure for Julia packages.
;;
;; Code:

(define (invoke-julia code)
  (invoke "julia" "-e" code))

;; subpath where we store the package content
(define %package-path "/share/julia/loadpath/")

(define (project.toml->name file)
  "Look for Julia package name in the TOML file FILE (usually named
Project.toml)."
  (call-with-input-file file
    (lambda (in)
      (let loop ((line (read-line in 'concat)))
        (if (eof-object? line)
            #f
            (let ((m (string-match "name\\s*=\\s*\"(.*)\"" line)))
              (if m (match:substring m 1)
                  (loop (read-line in 'concat)))))))))

(define (project.toml->uuid file)
  "Look for Julia package uuid in the TOML file FILE (usually named
Project.toml)."
  (call-with-input-file file
    (lambda (in)
      (let loop ((line (read-line in 'concat)))
        (if (eof-object? line)
            #f
            (let ((m (string-match "uuid\\s*=\\s*\"(.*)\"" line)))
              (if m (match:substring m 1)
                  (loop (read-line in 'concat)))))))))

(define* (install #:key source inputs outputs julia-package-name
                  #:allow-other-keys)
  (let* ((out (assoc-ref outputs "out"))
         (package-dir (string-append out %package-path
                                     (or
                                      julia-package-name
                                      (project.toml->name "Project.toml")))))
    (mkdir-p package-dir)
    (copy-recursively (getcwd) package-dir)))

(define* (precompile #:key source inputs outputs julia-package-name
                     #:allow-other-keys)
  (let* ((out (assoc-ref outputs "out"))
         (builddir (string-append out "/share/julia/"))
         (package (or julia-package-name (project.toml->name "Project.toml"))))
    (mkdir-p builddir)
    ;; With a patch, SOURCE_DATE_EPOCH is honored
    (setenv "SOURCE_DATE_EPOCH" "1")
    (setenv "JULIA_DEPOT_PATH" builddir)
    ;; Add new package dir to the load path.
    (setenv "JULIA_LOAD_PATH"
            (string-append builddir "loadpath/" ":"
                           (or (getenv "JULIA_LOAD_PATH")
                               "")))
    ;; Actual precompilation:
    (invoke-julia
     ;; When using Julia as a user, Julia writes precompile cache to the first
     ;; entry of the DEPOT_PATH list (by default, the home dir).  We want to
     ;; write it to the store, so let's push the store path as the first
     ;; element of DEPOT_PATH.  Once the cache file exists, this hack is not
     ;; needed anymore (like in the check phase).  If the user install new
     ;; packages, those will be installed and precompiled in the home dir.
     (string-append "pushfirst!(DEPOT_PATH, pop!(DEPOT_PATH)); using "
                    package))))

(define* (check #:key tests? source inputs outputs julia-package-name
                parallel-tests? #:allow-other-keys)
  (when tests?
    (let* ((out (assoc-ref outputs "out"))
           (package (or julia-package-name (project.toml->name "Project.toml")))
           (builddir (string-append out "/share/julia/"))
           (job-count (if parallel-tests?
                          (parallel-job-count)
                          1))
           ;; The --proc argument of Julia *adds* extra processors rather than
           ;; specify the exact count to use, so zero must be specified to
           ;; disable parallel processing...
           (additional-procs (max 0 (1- job-count))))
      ;; With a patch, SOURCE_DATE_EPOCH is honored
      (setenv "SOURCE_DATE_EPOCH" "1")
      (setenv "JULIA_DEPOT_PATH" builddir)
      (setenv "JULIA_LOAD_PATH"
              (string-append builddir "loadpath/" ":"
                             (or (getenv "JULIA_LOAD_PATH")
                                 "")))
      (setenv "JULIA_CPU_THREADS" (number->string job-count))
      (setenv "HOME" "/tmp")
      (apply invoke "julia"
             `("--depwarn=yes"
               ,@(if parallel-tests?
                     ;; XXX: ... but '--procs' doesn't accept 0 as a valid
                     ;; value, so just omit the argument entirely.
                     (list (string-append  "--procs="
                                           (number->string additional-procs)))
                     '())
               ,(string-append builddir "loadpath/"
                               package "/test/runtests.jl"))))))

(define* (link-depot #:key source inputs outputs
                     julia-package-name julia-package-uuid  #:allow-other-keys)
  (let* ((out (assoc-ref outputs "out"))
         (name+version (strip-store-file-name out))
         (version (last (string-split name+version #\-)))
         (package-name (or
                        julia-package-name
                        (project.toml->name "Project.toml")))
         (package-dir (string-append out %package-path package-name))
         (uuid (or julia-package-uuid (project.toml->uuid "Project.toml")))
         (pipe (open-pipe* OPEN_READ "julia" "-e"
                           (format #f "using Pkg;
println(Base.version_slug(Base.UUID(\"~a\"),
                          Base.SHA1(Pkg.GitTools.tree_hash(\".\"))))" uuid)))
         (slug (string-trim-right (get-string-all pipe))))
    ;; Few packages do not have the regular Project.toml file, then when they
    ;; are propagated, dependencies do not find them and an raise error.
    (unless (file-exists? "Project.toml")
        (julia-create-package-toml (getcwd)
                                   julia-package-name julia-package-uuid
                                   version
                                   #:file "Project.toml"))

    ;; When installing a package, julia looks first at in the JULIA_DEPOT_PATH
    ;; for a path like packages/PACKAGE/XXXX
    ;; Where XXXX is a slug encoding the package UUID and SHA1 of the files
    ;; Here we create a link with the correct path to enable julia to find the
    ;; package
    (mkdir-p (string-append out "/share/julia/packages/" package-name))
    (symlink package-dir (string-append out "/share/julia/packages/"
                                        package-name "/" slug))))

(define* (julia-create-package-toml location
                                    name uuid version
                                    #:optional
                                    (deps '())
                                    #:key
                                    (file "Project.toml"))
  "Some packages are not using the new Project.toml dependency specifications.
Write this FILE manually, so that Julia can find its dependencies."
  (let ((f (open-file
            (string-append location "/" file)
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
    (close-port f)))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (delete 'check) ; tests must be run after installation
    (replace 'install install)
    (add-after 'install 'precompile precompile)
    (add-after 'unpack 'link-depot link-depot)
    (add-after 'install 'check check)
    ;; TODO: In the future we could add a "system-image-generation" phase
    ;; where we use PackageCompiler.jl to speed up package loading times
    (delete 'configure)
    (delete 'bootstrap)
    (delete 'patch-usr-bin-file)
    (delete 'build)))

(define* (julia-build #:key inputs julia-package-name julia-package-uuid
                      (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Julia package, applying all of PHASES in order."
  (apply gnu:gnu-build
         #:inputs inputs #:phases phases
         #:julia-package-name julia-package-name
         #:julia-package-uuid julia-package-uuid
         args))
