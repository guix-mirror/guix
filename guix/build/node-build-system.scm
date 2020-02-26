;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Jelle Licht <jlicht@fsfe.org>
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

(define-module (guix build node-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build json)
  #:use-module (guix build union)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (%standard-phases
            node-build))

;; Commentary:
;;
;; Builder-side code of the standard Node/npm package build procedure.
;;
;; Code:

(define* (read-package-data #:key (filename "package.json"))
  (call-with-input-file filename
    (lambda (port)
      (read-json port))))

(define* (build #:key inputs #:allow-other-keys)
  (define (build-from-package-json? package-file)
    (let* ((package-data (read-package-data #:filename package-file))
           (scripts (assoc-ref package-data "scripts")))
      (assoc-ref scripts "build")))
  "Build a new node module using the appropriate build system."
  ;; XXX: Develop a more robust heuristic, allow override
  (cond ((file-exists? "gulpfile.js")
         (invoke "gulp"))
        ((file-exists? "gruntfile.js")
         (invoke "grunt"))
        ((file-exists? "Makefile")
         (invoke "make"))
        ((and (file-exists? "package.json")
              (build-from-package-json? "package.json"))
         (invoke "npm" "run" "build")))
  #t)

(define* (link-npm-dependencies #:key inputs #:allow-other-keys)
  (define (inputs->node-inputs inputs)
    "Filter the directory part from INPUTS."
    (filter (lambda (input)
              (match input
                ((name . _) (node-package? name))))
            inputs))
  (define (inputs->directories inputs)
    "Extract the directory part from INPUTS."
    (match inputs
      (((names . directories) ...)
       directories)))
  (define (make-node-path root)
    (string-append root "/lib/node_modules/"))

  (let ((input-node-directories (inputs->directories
                                 (inputs->node-inputs inputs))))
    (union-build "node_modules"
                 (map make-node-path input-node-directories))
    #t))

(define configure link-npm-dependencies)

(define* (check #:key tests? #:allow-other-keys)
  "Run 'npm test' if TESTS?"
  (if tests?
      ;; Should only be enabled once we know that there are tests
      (invoke "npm" "test"))
  #t)

(define (node-package? name)
  "Check if NAME correspond to the name of an Node package."
  (string-prefix? "node-" name))

(define* (install #:key outputs inputs #:allow-other-keys)
  "Install the node module to the output store item. The module itself is
installed in a subdirectory of @file{node_modules} and its runtime dependencies
as defined by @file{package.json} are symlinked into a @file{node_modules}
subdirectory of the module's directory. Additionally, binaries are installed in
the @file{bin} directory."
  (let* ((out                  (assoc-ref outputs "out"))
         (target               (string-append out "/lib"))
         (binaries             (string-append out "/bin"))
         (data                 (read-package-data))
         (modulename           (assoc-ref data "name"))
         (binary-configuration (match (assoc-ref data "bin")
				 (('@ configuration ...) configuration)
				 ((? string? configuration) configuration)
				 (#f #f)))
         (dependencies (match (assoc-ref data "dependencies")
                         (('@ deps ...) deps)
                         (#f #f))))
    (mkdir-p target)
    (copy-recursively "." (string-append target "/node_modules/" modulename))
    ;; Remove references to dependencies
    (delete-file-recursively
      (string-append target "/node_modules/" modulename "/node_modules"))
    (cond
      ((string? binary-configuration)
       (begin
         (mkdir-p binaries)
         (symlink (string-append target "/node_modules/" modulename "/"
				 binary-configuration)
                  (string-append binaries "/" modulename))))
      ((list? binary-configuration)
       (for-each
         (lambda (conf)
           (match conf
             ((key . value)
              (begin
                (mkdir-p (dirname (string-append binaries "/" key)))
                (symlink (string-append target "/node_modules/" modulename "/"
					value)
                         (string-append binaries "/" key))))))
         binary-configuration)))
    (when dependencies
      (mkdir-p
        (string-append target "/node_modules/" modulename "/node_modules"))
      (for-each
        (lambda (dependency)
          (let ((dependency (car dependency)))
            (symlink
              (string-append (assoc-ref inputs (string-append "node-" dependency))
                             "/lib/node_modules/" dependency)
              (string-append target "/node_modules/" modulename
                             "/node_modules/" dependency))))
        dependencies))
    #t))


(define %standard-phases
  (modify-phases gnu:%standard-phases
    (replace 'configure configure)
    (replace 'build build)
    (replace 'install install)
    (delete 'check)
    (add-after 'install 'check check)
    (delete 'strip)))

(define* (node-build #:key inputs (phases %standard-phases)
                     #:allow-other-keys #:rest args)
  (apply gnu:gnu-build #:inputs inputs #:phases phases args))
