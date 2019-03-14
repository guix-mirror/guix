;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix memoization)
  #:use-module (guix sets)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:export (missing-dependency-error?
            missing-dependency-module
            missing-dependency-search-path

            file-name->module-name
            module-name->file-name

            source-module-dependencies
            source-module-closure
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

;; The error corresponding to a missing module.
(define-condition-type &missing-dependency-error &error
  missing-dependency-error?
  (module      missing-dependency-module)
  (search-path missing-dependency-search-path))

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
  (mlambda (file)
    "Return the list of the names of modules that the Guile module in FILE
depends on."
    (call-with-input-file file
      (lambda (port)
        (match (read port)
          (('define-module name clauses ...)
           (extract-dependencies clauses))
          ;; XXX: R6RS 'library' form is ignored.
          (_
           '()))))))

(define file-name->module-name
  (let ((not-slash (char-set-complement (char-set #\/))))
    (lambda (file)
      "Return the module name (a list of symbols) corresponding to FILE."
      (map string->symbol
           (string-tokenize (string-drop-right file 4) not-slash)))))

(define (module-name->file-name module)
  "Return the file name for MODULE."
  (string-append (string-join (map symbol->string module) "/")
                 ".scm"))

(define (guix-module-name? name)
  "Return true if NAME (a list of symbols) denotes a Guix module."
  (match name
    (('guix _ ...) #t)
    (('gnu _ ...) #t)
    (_ #f)))

(define %source-less-modules
  ;; These are modules that have no corresponding source files or a source
  ;; file different from what you'd expect.
  '((system syntax)                             ;2.0, defined in boot-9
    (ice-9 ports internal)                      ;2.2, defined in (ice-9 ports)
    (system syntax internal)))                  ;2.2, defined in boot-9

(define* (source-module-dependencies module #:optional (load-path %load-path))
  "Return the modules used by MODULE by looking at its source code."
  (if (member module %source-less-modules)
      '()
      (match (search-path load-path (module-name->file-name module))
        ((? string? file)
         (module-file-dependencies file))
        (#f
         (raise (condition (&missing-dependency-error
                            (module module)
                            (search-path load-path))))))))

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
