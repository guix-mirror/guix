;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

;;; Commentary:
;;;
;;; This scripts updates the definition of the 'guix' package in Guix for the
;;; current commit.  It requires Git to be installed.
;;;
;;; Code:

(use-modules (guix)
             (guix ui)
             (guix git-download)
             (guix upstream)
             (guix utils)
             (guix base32)
             (guix build utils)
             (guix scripts hash)
             (gnu packages package-management)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 regex)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-2)
             (srfi srfi-26))

(define %top-srcdir
  (string-append (current-source-directory) "/.."))

(define version-controlled?
  (git-predicate %top-srcdir))

(define (package-definition-location)
  "Return the source properties of the definition of the 'guix' package."
  (call-with-input-file (location-file (package-location guix))
    (lambda (port)
      (let loop ()
        (match (read port)
          ((? eof-object?)
           (error "definition of 'guix' package could not be found"
                  (port-filename port)))
          (('define-public 'guix value)
           (source-properties value))
          (_
           (loop)))))))

(define* (update-definition commit hash
                            #:key version old-hash)
  "Return a one-argument procedure that takes a string, the definition of the
'guix' package, and returns a string, the update definition for VERSION,
COMMIT."
  (define (linear-offset str line column)
    ;; Return the offset in characters to reach LINE and COLUMN (both
    ;; zero-indexed) in STR.
    (call-with-input-string str
      (lambda (port)
        (let loop ((offset 0))
          (cond ((and (= (port-column port) column)
                      (= (port-line port) line))
                 offset)
                ((eof-object? (read-char port))
                 (error "line and column not reached!"
                        str))
                (else
                 (loop (+ 1 offset))))))))

  (define (update-hash str)
    ;; Replace OLD-HASH with HASH in STR.
    (string-replace-substring str
                              (bytevector->nix-base32-string old-hash)
                              (bytevector->nix-base32-string hash)))

  (lambda (str)
    (match (call-with-input-string str read)
      (('let (('version old-version)
              ('commit old-commit)
              ('revision old-revision))
         defn)
       (let* ((location (source-properties defn))
              (line     (assq-ref location 'line))
              (column   0)
              (offset   (linear-offset str line column)))
         (string-append (format #f "(let ((version \"~a\")
        (commit \"~a\")
        (revision ~a))\n"
                                (or version old-version)
                                commit
                                (if (and version
                                         (not (string=? version old-version)))
                                    0
                                    (+ 1 old-revision)))
                        (string-drop (update-hash str) offset))))
      (exp
       (error "'guix' package definition is not as expected" exp)))))

(define (git-add-worktree directory commit)
  "Create a new git worktree at DIRECTORY, detached on commit COMMIT."
  (invoke "git" "worktree" "add" "--detach" directory commit))

(define-syntax-rule (with-temporary-git-worktree commit body ...)
  "Execute BODY in the context of a temporary git worktree created from COMMIT."
  (call-with-temporary-directory
   (lambda (tmp-directory)
     (dynamic-wind
       (lambda ()
         #t)
       (lambda ()
         (git-add-worktree tmp-directory commit)
         (with-directory-excursion tmp-directory body ...))
       (lambda ()
         (invoke "git" "worktree" "remove" "--force" tmp-directory))))))

(define %savannah-guix-git-repo-push-url-regexp
  "git.(savannah|sv).gnu.org/srv/git/guix.git \\(push\\)")

(define-syntax-rule (with-input-pipe-to-string prog arg ...)
  (let* ((input-pipe (open-pipe* OPEN_READ prog arg ...))
	 (output (get-string-all input-pipe))
	 (exit-val (status:exit-val (close-pipe input-pipe))))
    (unless (zero? exit-val)
      (error (format #f "Command ~s exited with non-zero exit status: ~s"
                     (string-join (list prog arg ...)) exit-val)))
    (string-trim-both output)))

(define (find-origin-remote)
  "Find the name of the git remote with the Savannah Guix git repo URL."
  (and-let* ((remotes (string-split (with-input-pipe-to-string
                                     "git" "remote" "-v")
                                    #\newline))
             (origin-entry (find (cut string-match
                                      %savannah-guix-git-repo-push-url-regexp
                                      <>)
                                 remotes)))
    (first (string-split origin-entry #\tab))))

(define (commit-already-pushed? remote commit)
  "True if COMMIT is found in the REMOTE repository."
  (not (string-null? (with-input-pipe-to-string
                      "git" "branch" "-r" "--contains" commit
                      (string-append remote "/master")))))


(define (main . args)
  (match args
    ((commit version)
     (with-directory-excursion %top-srcdir
       (or (getenv "GUIX_ALLOW_ME_TO_USE_PRIVATE_COMMIT")
           (commit-already-pushed? (find-origin-remote) commit)
           (leave (G_ "Commit ~a is not pushed upstream.  Aborting.~%") commit))
       (let* ((hash (with-temporary-git-worktree commit
                        (nix-base32-string->bytevector
                         (string-trim-both
                          (with-output-to-string
		            (lambda ()
		              (guix-hash "-rx" ".")))))))
              (location (package-definition-location))
              (old-hash (content-hash-value
                         (origin-hash (package-source guix)))))
         (edit-expression location
                          (update-definition commit hash
                                             #:old-hash old-hash
                                             #:version version)))))
    ((commit)
     ;; Automatically deduce the version and revision numbers.
     (main commit #f))))

(apply main (cdr (command-line)))
