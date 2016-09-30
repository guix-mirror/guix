;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix modules)
  #:use-module ((guix utils) #:select (memoize))
  #:use-module (guix sets)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (source-module-closure
            live-module-closure
            guix-module-name?))

;;; Commentary:
;;;
;;; This module provides introspection tools for Guile modules at the source
;;; level.  Namely, it allows you to determine the closure of a module; it
;;; does so just by reading the 'define-module' clause of the module and its
;;; dependencies.  This is primarily useful as an argument to
;;; 'with-imported-modules'.
;;;
;;; Code:

(define (colon-symbol? obj)
  "Return true if OBJ is a symbol that starts with a colon."
  (and (symbol? obj)
       (string-prefix? ":" (symbol->string obj))))

(define (colon-symbol->keyword symbol)
  "Convert SYMBOL to a keyword after stripping its initial ':'."
  (symbol->keyword
   (string->symbol (string-drop (symbol->string symbol) 1))))

(define (extract-dependencies clauses)
  "Return the list of modules imported according to the given 'define-module'
CLAUSES."
  (let loop ((clauses clauses)
             (result  '()))
    (match clauses
      (()
       (reverse result))
      ((#:use-module (module (or #:select #:hide #:prefix #:renamer) _)
        rest ...)
       (loop rest (cons module result)))
      ((#:use-module module rest ...)
       (loop rest (cons module result)))
      ((#:autoload module _ rest ...)
       (loop rest (cons module result)))
      (((or #:export #:re-export #:export-syntax #:re-export-syntax
            #:replace #:version)
        _ rest ...)
       (loop rest result))
      (((or #:pure #:no-backtrace) rest ...)
       (loop rest result))
      (((? colon-symbol? symbol) rest ...)
       (loop (cons (colon-symbol->keyword symbol) rest)
             result)))))

(define module-file-dependencies
  (memoize
   (lambda (file)
     "Return the list of the names of modules that the Guile module in FILE
depends on."
     (call-with-input-file file
       (lambda (port)
         (match (read port)
           (('define-module name clauses ...)
            (extract-dependencies clauses))
           ;; XXX: R6RS 'library' form is ignored.
           (_
            '())))))))

(define (module-name->file-name module)
  "Return the file name for MODULE."
  (string-append (string-join (map symbol->string module) "/")
                 ".scm"))

(define (guix-module-name? name)
  "Return true if NAME (a list of symbols) denotes a Guix or GuixSD module."
  (match name
    (('guix _ ...) #t)
    (('gnu _ ...) #t)
    (_ #f)))

(define* (source-module-dependencies module #:optional (load-path %load-path))
  "Return the modules used by MODULE by looking at its source code."
  ;; The (system syntax) module is a special-case because it has no
  ;; corresponding source file (as of Guile 2.0.)
  (if (equal? module '(system syntax))
      '()
      (module-file-dependencies
       (search-path load-path
                    (module-name->file-name module)))))

(define* (module-closure modules
                         #:key
                         (select? guix-module-name?)
                         (dependencies source-module-dependencies))
  "Return the closure of MODULES, calling DEPENDENCIES to determine the list
of modules used by a given module.  MODULES and the result are a list of Guile
module names.  Only modules that match SELECT? are considered."
  (let loop ((modules modules)
             (result  '())
             (visited  (set)))
    (match modules
      (()
       (reverse result))
      ((module rest ...)
       (cond ((set-contains? visited module)
              (loop rest result visited))
             ((select? module)
              (loop (append (dependencies module) rest)
                    (cons module result)
                    (set-insert module visited)))
             (else
              (loop rest result visited)))))))

(define* (source-module-closure modules
                                #:optional (load-path %load-path)
                                #:key (select? guix-module-name?))
  "Return the closure of MODULES by reading 'define-module' forms in their
source code.  MODULES and the result are a list of Guile module names.  Only
modules that match SELECT?  are considered."
  (module-closure modules
                  #:dependencies (cut source-module-dependencies <> load-path)
                  #:select? select?))

(define* (live-module-closure modules
                              #:key (select? guix-module-name?))
  "Return the closure of MODULES, determined by looking at live (loaded)
module information.  MODULES and the result are a list of Guile module names.
Only modules that match SELECT? are considered."
  (define (dependencies module)
    (map module-name
         (delq the-scm-module (module-uses (resolve-module module)))))

  (module-closure modules
                  #:dependencies dependencies
                  #:select? select?))

;;; modules.scm ends here
