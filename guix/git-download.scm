;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mathieu Lirzin <mthl@gnu.org>
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

(define-module (guix git-download)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:autoload   (guix build-system gnu) (standard-packages)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:export (git-reference
            git-reference?
            git-reference-url
            git-reference-commit
            git-reference-recursive?

            git-fetch
            git-version
            git-file-name
            git-predicate))

;;; Commentary:
;;;
;;; An <origin> method that fetches a specific commit from a Git repository.
;;; The repository URL and commit hash are specified with a <git-reference>
;;; object.
;;;
;;; Code:

(define-record-type* <git-reference>
  git-reference make-git-reference
  git-reference?
  (url        git-reference-url)
  (commit     git-reference-commit)
  (recursive? git-reference-recursive?   ; whether to recurse into sub-modules
              (default #f)))

(define (git-package)
  "Return the default Git package."
  (let ((distro (resolve-interface '(gnu packages version-control))))
    (module-ref distro 'git)))

(define* (git-fetch ref hash-algo hash
                    #:optional name
                    #:key (system (%current-system)) (guile (default-guile))
                    (git (git-package)))
  "Return a fixed-output derivation that fetches REF, a <git-reference>
object.  The output is expected to have recursive hash HASH of type
HASH-ALGO (a symbol).  Use NAME as the file name, or a generic name if #f."
  (define inputs
    ;; When doing 'git clone --recursive', we need sed, grep, etc. to be
    ;; available so that 'git submodule' works.
    (if (git-reference-recursive? ref)
        (standard-packages)
        '()))

  (define build
    (with-imported-modules '((guix build git)
                             (guix build utils))
      #~(begin
          (use-modules (guix build git)
                       (guix build utils)
                       (ice-9 match))

          ;; The 'git submodule' commands expects Coreutils, sed,
          ;; grep, etc. to be in $PATH.
          (set-path-environment-variable "PATH" '("bin")
                                         (match '#+inputs
                                           (((names dirs) ...)
                                            dirs)))

          (git-fetch (getenv "git url") (getenv "git commit")
                     #$output
                     #:recursive? (call-with-input-string
                                      (getenv "git recursive?")
                                    read)
                     #:git-command (string-append #+git "/bin/git")))))

  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation (or name "git-checkout") build

                      ;; Use environment variables and a fixed script name so
                      ;; there's only one script in store for all the
                      ;; downloads.
                      #:script-name "git-download"
                      #:env-vars
                      `(("git url" . ,(git-reference-url ref))
                        ("git commit" . ,(git-reference-commit ref))
                        ("git recursive?" . ,(object->string
                                              (git-reference-recursive? ref))))

                      #:system system
                      #:local-build? #t           ;don't offload repo cloning
                      #:hash-algo hash-algo
                      #:hash hash
                      #:recursive? #t
                      #:guile-for-build guile)))

(define (git-version version revision commit)
  "Return the version string for packages using git-download."
  (string-append version "-" revision "." (string-take commit 7)))

(define (git-file-name name version)
  "Return the file-name for packages using git-download."
  (string-append name "-" version "-checkout"))

(define (git-predicate directory)
  "Return a predicate that returns true if a file is part of the Git checkout
living at DIRECTORY.  Upon Git failure, return #f instead of a predicate.

The returned predicate takes two arguments FILE and STAT where FILE is an
absolute file name and STAT is the result of 'lstat'."
  (define (parent-directory? thing directory)
    ;; Return #t if DIRECTORY is the parent of THING.
    (or (string-suffix? thing directory)
        (and (string-index thing #\/)
             (parent-directory? (dirname thing) directory))))

  (let* ((pipe        (with-directory-excursion directory
                        (open-pipe* OPEN_READ "git" "ls-files")))
         (files       (let loop ((lines '()))
                        (match (read-line pipe)
                          ((? eof-object?)
                           (reverse lines))
                          (line
                           (loop (cons line lines))))))
         (inodes      (map (lambda (file)
                             (let ((stat (lstat file)))
                               (cons (stat:dev stat) (stat:ino stat))))
                           files))
         (status      (close-pipe pipe)))
    (and (zero? status)
         (lambda (file stat)
           (match (stat:type stat)
             ('directory
              ;; 'git ls-files' does not list directories, only regular files,
              ;; so we need this special trick.
              (any (lambda (f) (parent-directory? f file))
                   files))
             ((or 'regular 'symlink)
              ;; Comparing file names is always tricky business so we rely on
              ;; inode numbers instead
              (member (cons (stat:dev stat) (stat:ino stat))
                      inodes))
             (_
              #f))))))

;;; git-download.scm ends here
