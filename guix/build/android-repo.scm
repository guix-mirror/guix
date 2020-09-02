;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2016, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (guix build android-repo)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-34)
  #:use-module (ice-9 format)
  #:export (android-repo-fetch))

;;; Commentary:
;;;
;;; This is the build-side support code of (guix android-repo-download).
;;; It allows a multirepository managed by the git-repo tool to be cloned and
;;; checked out at a specific revision.
;;;
;;; Code:

(define* (android-repo-fetch manifest-url manifest-revision directory
                             #:key (git-repo-command "git-repo"))
  "Fetch packages according to the manifest at MANIFEST-URL with
MANIFEST-REVISION.  MANIFEST-REVISION must be either a revision
or a branch.  Return #t on success, #f otherwise."

  ;; Disable TLS certificate verification.  The hash of the checkout is known
  ;; in advance anyway.
  (setenv "GIT_SSL_NO_VERIFY" "true")

  (mkdir-p directory)

  (guard (c ((invoke-error? c)
             (format (current-error-port)
                     "android-repo-fetch: '~a~{ ~a~}' failed with exit code ~a~%"
                     (invoke-error-program c)
                     (invoke-error-arguments c)
                     (or (invoke-error-exit-status c) ;XXX: not quite accurate
                         (invoke-error-stop-signal c)
                         (invoke-error-term-signal c)))
             (delete-file-recursively directory)
             #f))
    (with-directory-excursion directory
      (invoke git-repo-command "init" "-u" manifest-url "-b" manifest-revision
              "--depth=1")
      (invoke git-repo-command "sync" "-c" "--fail-fast" "-v" "-j"
              (number->string (parallel-job-count)))

      ;; Delete vendor/**/.git, system/**/.git, toolchain/**/.git,
      ;; .repo/**/.git etc since they contain timestamps.
      (for-each delete-file-recursively
       (find-files "." "^\\.git$" #:directories? #t))

      ;; Delete git state directories since they contain timestamps.
      (for-each delete-file-recursively
       (find-files ".repo" "^.*\\.git$" #:directories? #t))

      ;; This file contains timestamps.
      (delete-file ".repo/.repo_fetchtimes.json")
      #t)))

;;; android-repo.scm ends here
