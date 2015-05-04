;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix search-paths)
  #:use-module (guix records)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (<search-path-specification>
            search-path-specification
            search-path-specification?
            search-path-specification-variable
            search-path-specification-files
            search-path-specification-separator
            search-path-specification-file-type
            search-path-specification-file-pattern

            search-path-specification->sexp
            sexp->search-path-specification
            evaluate-search-paths))

;;; Commentary:
;;;
;;; This module defines "search path specifications", which allow packages to
;;; declare environment variables that they use to define search paths.  For
;;; instance, GCC has the 'CPATH' variable, Guile has the 'GUILE_LOAD_PATH'
;;; variable, etc.
;;;
;;; Code:

;; The specification of a search path.
(define-record-type* <search-path-specification>
  search-path-specification make-search-path-specification
  search-path-specification?
  (variable     search-path-specification-variable) ;string
  (files        search-path-specification-files)    ;list of strings
  (separator    search-path-specification-separator ;string
                (default ":"))
  (file-type    search-path-specification-file-type ;symbol
                (default 'directory))
  (file-pattern search-path-specification-file-pattern ;#f | string
                (default #f)))

(define (search-path-specification->sexp spec)
  "Return an sexp representing SPEC, a <search-path-specification>.  The sexp
corresponds to the arguments expected by `set-path-environment-variable'."
  ;; Note that this sexp format is used both by build systems and in
  ;; (guix profiles), so think twice before you change it.
  (match spec
    (($ <search-path-specification> variable files separator type pattern)
     `(,variable ,files ,separator ,type ,pattern))))

(define (sexp->search-path-specification sexp)
  "Convert SEXP, which is as returned by 'search-path-specification->sexp', to
a <search-path-specification> object."
  (match sexp
    ((variable files separator type pattern)
     (search-path-specification
      (variable variable)
      (files files)
      (separator separator)
      (file-type type)
      (file-pattern pattern)))))

(define-syntax-rule (with-null-error-port exp)
  "Evaluate EXP with the error port pointing to the bit bucket."
  (with-error-to-port (%make-void-port "w")
    (lambda () exp)))

;; XXX: This procedure used to be in (guix utils) but since we want to be able
;; to use (guix search-paths) on the build side, we want to avoid the
;; dependency on (guix utils), and so this procedure is back here for now.
(define (string-tokenize* string separator)
  "Return the list of substrings of STRING separated by SEPARATOR.  This is
like `string-tokenize', but SEPARATOR is a string."
  (define (index string what)
    (let loop ((string string)
               (offset 0))
      (cond ((string-null? string)
             #f)
            ((string-prefix? what string)
             offset)
            (else
             (loop (string-drop string 1) (+ 1 offset))))))

  (define len
    (string-length separator))

  (let loop ((string string)
             (result  '()))
    (cond ((index string separator)
           =>
           (lambda (offset)
             (loop (string-drop string (+ offset len))
                   (cons (substring string 0 offset)
                         result))))
          (else
           (reverse (cons string result))))))

(define* (evaluate-search-paths search-paths directory
                                #:optional (getenv (const #f)))
  "Evaluate SEARCH-PATHS, a list of search-path specifications, for DIRECTORY,
and return a list of variable/value pairs.  Use GETENV to determine the
current settings and report only settings not already effective."
  (define search-path-definition
    (match-lambda
      (($ <search-path-specification> variable files separator
                                      type pattern)
       (let* ((values (or (and=> (getenv variable)
                                 (cut string-tokenize* <> separator))
                          '()))
              ;; Add a trailing slash to force symlinks to be treated as
              ;; directories when 'find-files' traverses them.
              (files  (if pattern
                          (map (cut string-append <> "/") files)
                          files))

              ;; XXX: Silence 'find-files' when it stumbles upon non-existent
              ;; directories (see
              ;; <http://lists.gnu.org/archive/html/guix-devel/2015-01/msg00269.html>.)
              (path   (with-null-error-port
                       (search-path-as-list files (list directory)
                                            #:type type
                                            #:pattern pattern))))
         (if (every (cut member <> values) path)
             #f                         ;VARIABLE is already set appropriately
             (cons variable (string-join path separator)))))))

  (filter-map search-path-definition search-paths))

;;; search-paths.scm ends here
