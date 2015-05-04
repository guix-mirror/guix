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
            sexp->search-path-specification))

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

;;; search-paths.scm ends here
