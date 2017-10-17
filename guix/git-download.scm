;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
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
  #:use-module (guix modules)
  #:autoload   (guix build-system gnu) (standard-packages)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 vlist)
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

  (define zlib
    (module-ref (resolve-interface '(gnu packages compression)) 'zlib))

  (define config.scm
    (scheme-file "config.scm"
                 #~(begin
                     (define-module (guix config)
                       #:export (%libz))

                     (define %libz
                       #+(file-append zlib "/lib/libz")))))

  (define modules
    (cons `((guix config) => ,config.scm)
          (delete '(guix config)
                  (source-module-closure '((guix build git)
                                           (guix build utils)
                                           (guix build download-nar))))))

  (define build
    (with-imported-modules modules
      #~(begin
          (use-modules (guix build git)
                       (guix build utils)
                       (guix build download-nar)
                       (ice-9 match))

          ;; The 'git submodule' commands expects Coreutils, sed,
          ;; grep, etc. to be in $PATH.
          (set-path-environment-variable "PATH" '("bin")
                                         (match '#+inputs
                                           (((names dirs) ...)
                                            dirs)))

          (or (git-fetch (getenv "git url") (getenv "git commit")
                         #$output
                         #:recursive? (call-with-input-string
                                          (getenv "git recursive?")
                                        read)
                         #:git-command (string-append #+git "/bin/git"))
              (download-nar #$output)))))

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


;;;
;;; 'git-predicate'.
;;;

(define (files->directory-tree files)
  "Return a tree of vhashes representing the directory listed in FILES, a list
like '(\"a/b\" \"b/c/d\")."
  (fold (lambda (file result)
          (let loop ((file (string-split file #\/))
                     (result result))
            (match file
              ((_)
               result)
              ((directory children ...)
               (match (vhash-assoc directory result)
                 (#f
                  (vhash-cons directory (loop children vlist-null)
                              result))
                 ((_ . previous)
                  ;; XXX: 'vhash-delete' is O(n).
                  (vhash-cons directory (loop children previous)
                              (vhash-delete directory result)))))
              (()
               result))))
        vlist-null
        files))

(define (directory-in-tree? tree directory)
  "Return true if DIRECTORY, a string like \"a/b\", denotes a directory listed
in TREE."
  (let loop ((directory (string-split directory #\/))
             (tree       tree))
    (match directory
      (()
       #t)
      ((head . tail)
       (match (vhash-assoc head tree)
         ((_ . sub-tree) (loop tail sub-tree))
         (#f #f))))))

(define (git-predicate directory)
  "Return a predicate that returns true if a file is part of the Git checkout
living at DIRECTORY.  Upon Git failure, return #f instead of a predicate.

The returned predicate takes two arguments FILE and STAT where FILE is an
absolute file name and STAT is the result of 'lstat'."
  (let* ((pipe           (with-directory-excursion directory
                           (open-pipe* OPEN_READ "git" "ls-files")))
         (files          (let loop ((lines '()))
                           (match (read-line pipe)
                             ((? eof-object?)
                              (reverse lines))
                             (line
                              (loop (cons line lines))))))
         (directory-tree (files->directory-tree files))
         (inodes         (fold (lambda (file result)
                                 (let ((stat
                                        (lstat (string-append directory "/"
                                                              file))))
                                   (vhash-consv (stat:ino stat) (stat:dev stat)
                                                result)))
                               vlist-null
                               files))

         ;; Note: For this to work we must *not* call 'canonicalize-path' on
         ;; DIRECTORY or we would get discrepancies of the returned lambda is
         ;; called with a non-canonical file name.
         (prefix-length  (+ 1 (string-length directory)))

         (status         (close-pipe pipe)))
    (and (zero? status)
         (lambda (file stat)
           (match (stat:type stat)
             ('directory
              (directory-in-tree? directory-tree
                                  (string-drop file prefix-length)))
             ((or 'regular 'symlink)
              ;; Comparing file names is always tricky business so we rely on
              ;; inode numbers instead
              (match (vhash-assv (stat:ino stat) inodes)
                ((_ . dev) (= dev (stat:dev stat)))
                (#f        #f)))
             (_
              #f))))))

;;; git-download.scm ends here
