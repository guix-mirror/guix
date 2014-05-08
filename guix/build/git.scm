;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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
                    #:key (git-command "git"))
  "Fetch COMMIT from URL into DIRECTORY.  COMMIT must be a valid Git commit
identifier.  Return #t on success, #f otherwise."

  ;; Disable TLS certificate verification.  The hash of the checkout is known
  ;; in advance anyway.
  (setenv "GIT_SSL_NO_VERIFY" "true")

  (and (zero? (system* git-command "clone" url directory))
       (with-directory-excursion directory
         (system* git-command "tag" "-l")
         (and (zero? (system* git-command "checkout" commit))
              (begin
                ;; The contents of '.git' vary as a function of the current
                ;; status of the Git repo.  Since we want a fixed output, this
                ;; directory needs to be taken out.
                (delete-file-recursively ".git")
                #t)))))

;;; git.scm ends here
