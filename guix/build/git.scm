;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2016 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build git)
  #:use-module (guix build utils)
  #:export (git-fetch))

;;; Commentary:
;;;
;;; This is the build-side support code of (guix git-download).  It allows a
;;; Git repository to be cloned and checked out at a specific commit.
;;;
;;; Code:

(define* (git-fetch url commit directory
                    #:key (git-command "git") recursive?)
  "Fetch COMMIT from URL into DIRECTORY.  COMMIT must be a valid Git commit
identifier.  When RECURSIVE? is true, all the sub-modules of URL are fetched,
recursively.  Return #t on success, #f otherwise."

  ;; Disable TLS certificate verification.  The hash of the checkout is known
  ;; in advance anyway.
  (setenv "GIT_SSL_NO_VERIFY" "true")

  (mkdir-p directory)

  (with-directory-excursion directory
    (invoke git-command "init")
    (invoke git-command "remote" "add" "origin" url)
    (if (zero? (system* git-command "fetch" "--depth" "1" "origin" commit))
        (invoke git-command "checkout" "FETCH_HEAD")
        (begin
          (setvbuf (current-output-port) 'line)
          (format #t "Failed to do a shallow fetch; retrying a full fetch...~%")
          (invoke git-command "fetch" "origin")
          (invoke git-command "checkout" commit)))
    (when recursive?
      ;; Now is the time to fetch sub-modules.
      (unless (zero? (system* git-command "submodule" "update"
                                          "--init" "--recursive"))
        (error "failed to fetch sub-modules" url))

      ;; In sub-modules, '.git' is a flat file, not a directory,
      ;; so we can use 'find-files' here.
      (for-each delete-file-recursively
                (find-files directory "^\\.git$")))

      ;; The contents of '.git' vary as a function of the current
      ;; status of the Git repo.  Since we want a fixed output, this
      ;; directory needs to be taken out.
      (delete-file-recursively ".git")
      #t))

;;; git.scm ends here
