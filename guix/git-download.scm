;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
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
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix modules)
  #:autoload   (guix build-system gnu) (standard-packages)
  #:autoload   (git repository) (repository-open
                                 repository-close!
                                 repository-discover
                                 repository-head
                                 repository-working-directory)
  #:autoload   (git commit)     (commit-lookup commit-tree)
  #:autoload   (git reference)  (reference-target)
  #:autoload   (git tree)       (tree-list)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
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
    (module-ref distro 'git-minimal)))

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

        ;; The 'swh-download' procedure requires tar and gzip.
        `(("gzip" ,(module-ref (resolve-interface '(gnu packages compression))
                               'gzip))
          ("tar" ,(module-ref (resolve-interface '(gnu packages base))
                              'tar)))))

  (define guile-json
    (module-ref (resolve-interface '(gnu packages guile)) 'guile-json-4))

  (define guile-zlib
    (module-ref (resolve-interface '(gnu packages guile)) 'guile-zlib))

  (define gnutls
    (module-ref (resolve-interface '(gnu packages tls)) 'gnutls))

  (define modules
    (delete '(guix config)
            (source-module-closure '((guix build git)
                                     (guix build utils)
                                     (guix build download-nar)
                                     (guix swh)))))

  (define build
    (with-imported-modules modules
      (with-extensions (list guile-json gnutls   ;for (guix swh)
                             guile-zlib)
        #~(begin
            (use-modules (guix build git)
                         (guix build utils)
                         (guix build download-nar)
                         (guix swh)
                         (ice-9 match))

            (define recursive?
              (call-with-input-string (getenv "git recursive?") read))

            ;; The 'git submodule' commands expects Coreutils, sed,
            ;; grep, etc. to be in $PATH.
            (set-path-environment-variable "PATH" '("bin")
                                           (match '#+inputs
                                             (((names dirs outputs ...) ...)
                                              dirs)))

            (setvbuf (current-output-port) 'line)
            (setvbuf (current-error-port) 'line)

            (or (git-fetch (getenv "git url") (getenv "git commit")
                           #$output
                           #:recursive? recursive?
                           #:git-command (string-append #+git "/bin/git"))
                (download-nar #$output)

                ;; As a last resort, attempt to download from Software Heritage.
                ;; Disable X.509 certificate verification to avoid depending
                ;; on nss-certs--we're authenticating the checkout anyway.
                ;; XXX: Currently recursive checkouts are not supported.
                (and (not recursive?)
                     (parameterize ((%verify-swh-certificate? #f))
                       (format (current-error-port)
                               "Trying to download from Software Heritage...~%")
                       (swh-download (getenv "git url") (getenv "git commit")
                                     #$output))))))))

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
                      #:leaked-env-vars '("http_proxy" "https_proxy"
                                          "LC_ALL" "LC_MESSAGES" "LANG"
                                          "COLUMNS")

                      #:system system
                      #:local-build? #t           ;don't offload repo cloning
                      #:hash-algo hash-algo
                      #:hash hash
                      #:recursive? #t
                      #:guile-for-build guile)))

(define (git-version version revision commit)
  "Return the version string for packages using git-download."
  ;; git-version is almost exclusively executed while modules are being loaded.
  ;; This makes any errors hide their backtrace. Avoid the mysterious error
  ;; "Value out of range 0 to N: 7" when the commit ID is too short, which
  ;; can happen, for example, when the user swapped the revision and commit
  ;; arguments by mistake.
  (when (< (string-length commit) 7)
    (raise
      (condition
        (&message (message "git-version: commit ID unexpectedly short")))))
  (string-append version "-" revision "." (string-take commit 7)))

(define (git-file-name name version)
  "Return the file-name for packages using git-download."
  (string-append name "-" version "-checkout"))


;;;
;;; 'git-predicate'.
;;;

(define (git-file-list directory)
  "Return the list of files checked in in the Git repository at DIRECTORY.
The result is similar to that of the 'git ls-files' command, except that it
also includes directories, not just regular files.  The returned file names
are relative to DIRECTORY, which is not necessarily the root of the checkout."
  (let* (;; 'repository-working-directory' always returns a trailing "/",
         ;; so add one here to ease the comparisons below.
         (directory  (string-append (canonicalize-path directory) "/"))
         (dot-git    (repository-discover directory))
         (repository (repository-open dot-git))
         (workdir    (repository-working-directory repository))
         (head       (repository-head repository))
         (oid        (reference-target head))
         (commit     (commit-lookup repository oid))
         (tree       (commit-tree commit))
         (files      (tree-list tree)))
    (repository-close! repository)
    (if (string=? workdir directory)
        files
        (let ((relative (string-drop directory (string-length workdir))))
          (filter-map (lambda (file)
                        (and (string-prefix? relative file)
                             (string-drop file (string-length relative))))
                      files)))))

(define (git-predicate directory)
  "Return a predicate that returns true if a file is part of the Git checkout
living at DIRECTORY.  If DIRECTORY does not lie within a Git checkout, and
upon Git errors, return #f instead of a predicate.

The returned predicate takes two arguments FILE and STAT where FILE is an
absolute file name and STAT is the result of 'lstat'."
  (catch 'git-error
    (lambda ()
      (let* ((files  (git-file-list directory))
             (inodes (fold (lambda (file result)
                             (let ((stat
                                    (lstat (string-append directory "/"
                                                          file))))
                               (vhash-consv (stat:ino stat) (stat:dev stat)
                                            result)))
                           vlist-null
                           files)))
        (lambda (file stat)
          ;; Comparing file names is always tricky business so we rely on inode
          ;; numbers instead.
          (match (vhash-assv (stat:ino stat) inodes)
            ((_ . dev) (= dev (stat:dev stat)))
            (#f        #f)))))
    (const #f)))

;;; git-download.scm ends here
