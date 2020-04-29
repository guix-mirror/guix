;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix tests git)
  #:use-module (git)
  #:use-module ((guix git) #:select (with-repository))
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 control)
  #:export (git-command
            with-temporary-git-repository
            find-commit))

(define git-command
  (make-parameter "git"))

(define (call-with-environment-variables variables thunk)
  "Call THUNK with the environment VARIABLES set."
  (let ((environment (environ)))
    (dynamic-wind
      (lambda ()
        (for-each (match-lambda
                    ((variable value)
                     (setenv variable value)))
                  variables))
      thunk
      (lambda ()
        (environ environment)))))

(define-syntax-rule (with-environment-variables variables exp ...)
  "Evaluate EXP with the given environment VARIABLES set."
  (call-with-environment-variables variables
                                   (lambda () exp ...)))

(define (populate-git-repository directory directives)
  "Initialize a new Git checkout and repository in DIRECTORY and apply
DIRECTIVES.  Each element of DIRECTIVES is an sexp like:

  (add \"foo.txt\" \"hi!\")

Return DIRECTORY on success."

  ;; Note: As of version 0.2.0, Guile-Git lacks the necessary bindings to do
  ;; all this, so resort to the "git" command.
  (define (git command . args)
    ;; Make sure Git doesn't rely on the user's config.
    (call-with-temporary-directory
     (lambda (home)
       (call-with-output-file (string-append home "/.gitconfig")
         (lambda (port)
           (display "[user]
  email = charlie@example.org\n  name = Charlie Guix\n"
                    port)))

       (with-environment-variables
        `(("GIT_CONFIG_NOSYSTEM" "1")
          ("GIT_ATTR_NOSYSTEM" "1")
          ("HOME" ,home))
        (apply invoke (git-command) "-C" directory
               command args)))))

  (mkdir-p directory)
  (git "init")

  (let loop ((directives directives))
    (match directives
      (()
       directory)
      ((('add file contents) rest ...)
       (let ((file (string-append directory "/" file)))
         (mkdir-p (dirname file))
         (call-with-output-file file
           (lambda (port)
             (display (if (string? contents)
                          contents
                          (with-repository directory repository
                            (contents repository)))
                      port)))
         (git "add" file)
         (loop rest)))
      ((('commit text) rest ...)
       (git "commit" "-m" text)
       (loop rest))
      ((('tag name) rest ...)
       (git "tag" name)
       (loop rest))
      ((('branch name) rest ...)
       (git "branch" name)
       (loop rest))
      ((('checkout branch) rest ...)
       (git "checkout" branch)
       (loop rest))
      ((('merge branch message) rest ...)
       (git "merge" branch "-m" message)
       (loop rest)))))

(define (call-with-temporary-git-repository directives proc)
  (call-with-temporary-directory
   (lambda (directory)
     (populate-git-repository directory directives)
     (proc directory))))

(define-syntax-rule (with-temporary-git-repository directory
                                                   directives exp ...)
  "Evaluate EXP in a context where DIRECTORY contains a checkout populated as
per DIRECTIVES."
  (call-with-temporary-git-repository directives
                                      (lambda (directory)
                                        exp ...)))

(define (find-commit repository message)
  "Return the commit in REPOSITORY whose message includes MESSAGE, a string."
  (let/ec return
    (fold-commits (lambda (commit _)
                    (and (string-contains (commit-message commit)
                                          message)
                         (return commit)))
                  #f
                  repository)
    (error "commit not found" message)))
