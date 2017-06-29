;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build store-copy)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ftw)
  #:export (read-reference-graph
            closure-size
            populate-store))

;;; Commentary:
;;;
;;; This module provides the tools to copy store items and their dependencies
;;; to another store.  It relies on the availability of "reference graph"
;;; files as produced by 'gexp->derivation' et al. with the
;;; #:references-graphs parameter.
;;;
;;; Code:

(define (read-reference-graph port)
  "Return a list of store paths from the reference graph at PORT.
The data at PORT is the format produced by #:references-graphs."
  (let loop ((line   (read-line port))
             (result '()))
    (cond ((eof-object? line)
           (delete-duplicates result))
          ((string-prefix? "/" line)
           (loop (read-line port)
                 (cons line result)))
          (else
           (loop (read-line port)
                 result)))))

(define (file-size file)
  "Return the size of bytes of FILE, entering it if FILE is a directory."
  (file-system-fold (const #t)
                    (lambda (file stat result)    ;leaf
                      (+ (stat:size stat) result))
                    (lambda (directory stat result) ;down
                      (+ (stat:size stat) result))
                    (lambda (directory stat result) ;up
                      result)
                    (lambda (file stat result)    ;skip
                      result)
                    (lambda (file stat errno result)
                      (format (current-error-port)
                              "file-size: ~a: ~a~%" file
                              (strerror errno))
                      result)
                    0
                    file
                    lstat))

(define (closure-size reference-graphs)
  "Return an estimate of the size of the closure described by
REFERENCE-GRAPHS, a list of reference-graph files."
  (define (graph-from-file file)
    (call-with-input-file file read-reference-graph))

  (define items
    (delete-duplicates (append-map graph-from-file reference-graphs)))

  (reduce + 0 (map file-size items)))

(define* (populate-store reference-graphs target)
  "Populate the store under directory TARGET with the items specified in
REFERENCE-GRAPHS, a list of reference-graph files."
  (define store
    (string-append target (%store-directory)))

  (define (things-to-copy)
    ;; Return the list of store files to copy to the image.
    (define (graph-from-file file)
      (call-with-input-file file read-reference-graph))

    (delete-duplicates (append-map graph-from-file reference-graphs)))

  (mkdir-p store)
  (chmod store #o1775)
  (for-each (lambda (thing)
              (copy-recursively thing
                                (string-append target thing)))
            (things-to-copy)))

;;; store-copy.scm ends here
