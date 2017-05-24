;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix ui)
  #:use-module (guix combinators)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 ftw)
  #:export (scheme-modules
            fold-modules
            all-modules
            fold-module-public-variables))

;;; Commentary:
;;;
;;; This module provides tools to discover Guile modules and the variables
;;; they export.
;;;
;;; Code:

(define* (scheme-files directory)
  "Return the list of Scheme files found under DIRECTORY, recursively.  The
returned list is sorted in alphabetical order."

  ;; Sort entries so that 'fold-packages' works in a deterministic fashion
  ;; regardless of details of the underlying file system.
  (sort (file-system-fold (const #t)                 ;enter?
                          (lambda (path stat result) ;leaf
                            (if (string-suffix? ".scm" path)
                                (cons path result)
                                result))
                          (lambda (path stat result) ;down
                            result)
                          (lambda (path stat result) ;up
                            result)
                          (const #f)                 ;skip
                          (lambda (path stat errno result)
                            (unless (= ENOENT errno)
                              (warning (G_ "cannot access `~a': ~a~%")
                                       path (strerror errno)))
                            result)
                          '()
                          directory
                          stat)
        string<?))

(define file-name->module-name
  (let ((not-slash (char-set-complement (char-set #\/))))
    (lambda (file)
      "Return the module name (a list of symbols) corresponding to FILE."
      (map string->symbol
           (string-tokenize (string-drop-right file 4) not-slash)))))

(define* (scheme-modules directory #:optional sub-directory)
  "Return the list of Scheme modules available under DIRECTORY.
Optionally, narrow the search to SUB-DIRECTORY."
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
                      (warn-about-load-error module args)
                      #f))))
              (scheme-files (if sub-directory
                                (string-append directory "/" sub-directory)
                                directory))))

(define (fold-modules proc init path)
  "Fold over all the Scheme modules present in PATH, a list of directories.
Call (PROC MODULE RESULT) for each module that is found."
  (fold (lambda (spec result)
          (match spec
            ((? string? directory)
             (fold proc result (scheme-modules directory)))
            ((directory . sub-directory)
             (fold proc result
                   (scheme-modules directory sub-directory)))))
        '()
        path))

(define (all-modules path)
  "Return the list of package modules found in PATH, a list of directories to
search.  Entries in PATH can be directory names (strings) or (DIRECTORY
. SUB-DIRECTORY) pairs, in which case modules are searched for beneath
SUB-DIRECTORY."
  (fold-modules cons '() path))

(define (fold-module-public-variables proc init modules)
  "Call (PROC OBJECT RESULT) for each variable exported by one of MODULES,
using INIT as the initial value of RESULT.  It is guaranteed to never traverse
the same object twice."
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
