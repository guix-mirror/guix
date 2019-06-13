;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix discovery)
  #:use-module (guix i18n)
  #:use-module (guix modules)
  #:use-module (guix combinators)
  #:use-module (guix build syscalls)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 ftw)
  #:export (scheme-files
            scheme-modules
            scheme-modules*
            fold-modules
            all-modules
            fold-module-public-variables
            fold-module-public-variables*))

;;; Commentary:
;;;
;;; This module provides tools to discover Guile modules and the variables
;;; they export.
;;;
;;; Code:

(define* (scheme-files directory)
  "Return the list of Scheme files found under DIRECTORY, recursively.  The
returned list is sorted in alphabetical order.  Return the empty list if
DIRECTORY is not accessible."
  (define (entry-type name properties)
    (match (assoc-ref properties 'type)
      ('unknown
       (stat:type (lstat name)))
      ((? symbol? type)
       type)))

  ;; Use 'scandir*' so we can avoid an extra 'lstat' for each entry, as
  ;; opposed to Guile's 'scandir' or 'file-system-fold'.
  (fold-right (lambda (entry result)
                (match entry
                  (("." . _)
                   result)
                  ((".." . _)
                   result)
                  ((name . properties)
                   (let ((absolute (string-append directory "/" name)))
                     (case (entry-type absolute properties)
                       ((directory)
                        (append (scheme-files absolute) result))
                       ((regular)
                        (if (string-suffix? ".scm" name)
                            (cons absolute result)
                            result))
                       ((symlink)
                        (cond ((string-suffix? ".scm" name)
                               (cons absolute result))
                              ((stat absolute #f)
                               =>
                               (match-lambda
                                 (#f result)
                                 ((= stat:type 'directory)
                                  (append (scheme-files absolute)
                                          result))
                                 (_ result)))))
                       (else
                        result))))))
              '()
              (catch 'system-error
                (lambda ()
                  (scandir* directory))
                (lambda args
                  (let ((errno (system-error-errno args)))
                    (unless (= errno ENOENT)
                      (format (current-error-port) ;XXX
                              (G_ "cannot access `~a': ~a~%")
                              directory (strerror errno)))
                    '())))))

(define* (scheme-modules directory #:optional sub-directory
                         #:key (warn (const #f)))
  "Return the list of Scheme modules available under DIRECTORY.
Optionally, narrow the search to SUB-DIRECTORY.

WARN is called when a module could not be loaded.  It is passed the module
name and the exception key and arguments."
  (define prefix-len
    (string-length directory))

  (filter-map (lambda (file)
                (let* ((file   (substring file prefix-len))
                       (module (file-name->module-name file)))
                  (catch #t
                    (lambda ()
                      (resolve-interface module))
                    (lambda args
                      ;; Report the error, but keep going.
                      (warn module args)
                      #f))))
              (scheme-files (if sub-directory
                                (string-append directory "/" sub-directory)
                                directory))))

(define* (scheme-modules* directory #:optional sub-directory)
  "Return the list of module names found under SUB-DIRECTORY in DIRECTORY.
This is a source-only variant that does not try to load files."
  (let ((prefix (string-length directory)))
    (map (lambda (file)
           (file-name->module-name (string-drop file prefix)))
         (scheme-files (if sub-directory
                           (string-append directory "/" sub-directory)
                           directory)))))

(define* (fold-modules proc init path #:key (warn (const #f)))
  "Fold over all the Scheme modules present in PATH, a list of directories.
Call (PROC MODULE RESULT) for each module that is found."
  (fold (lambda (spec result)
          (match spec
            ((? string? directory)
             (fold proc result (scheme-modules directory #:warn warn)))
            ((directory . sub-directory)
             (fold proc result
                   (scheme-modules directory sub-directory
                                   #:warn warn)))))
        '()
        path))

(define* (all-modules path #:key (warn (const #f)))
  "Return the list of package modules found in PATH, a list of directories to
search.  Entries in PATH can be directory names (strings) or (DIRECTORY
. SUB-DIRECTORY) pairs, in which case modules are searched for beneath
SUB-DIRECTORY. Modules are listed in the order they appear on the path."
  (reverse (fold-modules cons '() path #:warn warn)))

(define (fold-module-public-variables* proc init modules)
  "Call (PROC MODULE SYMBOL VARIABLE) for each variable exported by one of MODULES,
using INIT as the initial value of RESULT.  It is guaranteed to never traverse
the same object twice."
  ;; Here SEEN is populated by variables; if two different variables refer to
  ;; the same object, we still let them through.
  (identity                                       ;discard second return value
   (fold2 (lambda (module result seen)
            (fold2 (lambda (sym+var result seen)
                     (match sym+var
                       ((sym . var)
                        (if (not (vhash-assq var seen))
                            (values (proc module sym var result)
                                    (vhash-consq var #t seen))
                            (values result seen)))))
                   result
                   seen
                   (module-map cons module)))
          init
          vlist-null
          modules)))

(define (fold-module-public-variables proc init modules)
  "Call (PROC OBJECT RESULT) for each variable exported by one of MODULES,
using INIT as the initial value of RESULT.  It is guaranteed to never traverse
the same object twice."
  ;; Note: here SEEN is populated by objects, not by variables.
  (identity   ; discard second return value
   (fold2 (lambda (module result seen)
            (fold2 (lambda (var result seen)
                     (if (not (vhash-assq var seen))
                         (values (proc var result)
                                 (vhash-consq var #t seen))
                         (values result seen)))
                   result
                   seen
                   (module-map (lambda (sym var)
                                 (false-if-exception (variable-ref var)))
                               module)))
          init
          vlist-null
          modules)))

;;; discovery.scm ends here
