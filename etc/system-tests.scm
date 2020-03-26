;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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

(use-modules (gnu tests)
             (gnu packages package-management)
             ((gnu ci) #:select (channel-source->package))
             ((guix git-download) #:select (git-predicate))
             ((guix utils) #:select (current-source-directory))
             (git)
             (ice-9 match))

(define (source-commit directory)
  "Return the commit of the head of DIRECTORY or #f if it could not be
determined."
  (let ((repository #f))
    (catch 'git-error
      (lambda ()
        (set! repository (repository-open directory))
        (let* ((head   (repository-head repository))
               (target (reference-target head))
               (commit (oid->string target)))
          (repository-close! repository)
          commit))
      (lambda _
        (when repository
          (repository-close! repository))
        #f))))

(define (tests-for-current-guix source commit)
  "Return a list of tests for perform, using Guix built from SOURCE, a channel
instance."
  ;; Honor the 'TESTS' environment variable so that one can select a subset
  ;; of tests to run in the usual way:
  ;;
  ;;   make check-system TESTS=installed-os
  (parameterize ((current-guix-package
                  (channel-source->package source #:commit commit)))
    (match (getenv "TESTS")
      (#f
       (all-system-tests))
      ((= string-tokenize (tests ...))
       (filter (lambda (test)
                 (member (system-test-name test) tests))
               (all-system-tests))))))

(define (system-test->manifest-entry test)
  "Return a manifest entry for TEST, a system test."
  (manifest-entry
    (name (string-append "test." (system-test-name test)))
    (version "0")
    (item test)))

(define (system-test-manifest)
  "Return a manifest containing all the system tests, or all those selected by
the 'TESTS' environment variable."
  (define source
    (string-append (current-source-directory) "/.."))

  (define commit
    ;; Fetch the current commit ID so we can potentially build the same
    ;; derivation as ci.guix.gnu.org.
    (source-commit source))

  ;; Intern SOURCE so that 'build-from-source' in (guix channels) sees
  ;; "fresh" file names and thus doesn't find itself loading .go files
  ;; from ~/.cache/guile when it loads 'build-aux/build-self.scm'.
  (let* ((source (local-file source
                             (if commit
                                 (string-append "guix-"
                                                (string-take commit 7))
                                 "guix-source")
                             #:recursive? #t
                             #:select?
                             (or (git-predicate source)
                                 (const #t))))
         (tests  (tests-for-current-guix source commit)))
    (format (current-error-port) "Selected ~a system tests...~%"
            (length tests))

    (manifest (map system-test->manifest-entry tests))))

;; Return the manifest.
(system-test-manifest)
