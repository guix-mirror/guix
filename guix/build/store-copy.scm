;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix sets)
  #:use-module (guix progress)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 vlist)
  #:export (store-info?
            store-info
            store-info-item
            store-info-deriver
            store-info-references

            read-reference-graph

            file-size
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

;; Information about a store item as produced by #:references-graphs.
(define-record-type <store-info>
  (store-info item deriver references)
  store-info?
  (item        store-info-item)                   ;string
  (deriver     store-info-deriver)                ;#f | string
  (references  store-info-references))            ;?

;; TODO: Factorize with that in (guix store).
(define (topological-sort nodes edges)
  "Return NODES in topological order according to EDGES.  EDGES must be a
one-argument procedure that takes a node and returns the nodes it is connected
to."
  (define (traverse)
    ;; Do a simple depth-first traversal of all of PATHS.
    (let loop ((nodes nodes)
               (visited (setq))
               (result '()))
      (match nodes
        ((head tail ...)
         (if (set-contains? visited head)
             (loop tail visited result)
             (call-with-values
                 (lambda ()
                   (loop (edges head)
                         (set-insert head visited)
                         result))
               (lambda (visited result)
                 (loop tail visited (cons head result))))))
        (()
         (values visited result)))))

  (call-with-values traverse
    (lambda (_ result)
      (reverse result))))

(define (read-reference-graph port)
  "Read the reference graph as produced by #:references-graphs from PORT and
return it as a list of <store-info> records in topological order--i.e., leaves
come first.  IOW, store items in the resulting list can be registered in the
order in which they appear.

The reference graph format consists of sequences of lines like this:

     FILE
     DERIVER
     NUMBER-OF-REFERENCES
     REF1
     ...
     REFN

It is meant as an internal format."
  (let loop ((result '())
             (table vlist-null)
             (referrers vlist-null))
    (match (read-line port)
      ((? eof-object?)
       ;; 'guix-daemon' gives us something that's in "reverse topological
       ;; order"--i.e., leaves (items with zero references) come last.  Here
       ;; we compute the topological order that we want: leaves come first.
       (let ((unreferenced? (lambda (item)
                              (let ((referrers (vhash-fold* cons '()
                                                            (store-info-item item)
                                                            referrers)))
                                (or (null? referrers)
                                    (equal? (list item) referrers))))))
         (topological-sort (filter unreferenced? result)
                           (lambda (item)
                             (map (lambda (item)
                                    (match (vhash-assoc item table)
                                      ((_ . node) node)))
                                  (store-info-references item))))))
      (item
       (let* ((deriver (match (read-line port)
                         ("" #f)
                         (line line)))
              (count   (string->number (read-line port)))
              (refs    (unfold-right (cut >= <> count)
                                     (lambda (n)
                                       (read-line port))
                                     1+
                                     0))
              (item    (store-info item deriver refs)))
         (loop (cons item result)
               (vhash-cons (store-info-item item) item table)
               (fold (cut vhash-cons <> item <>)
                     referrers
                     refs)))))))

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
    (map store-info-item
         (call-with-input-file file read-reference-graph)))

  (define items
    (delete-duplicates (append-map graph-from-file reference-graphs)))

  (reduce + 0 (map file-size items)))

(define (reset-permissions file)
  "Reset the permissions on FILE and its sub-directories so that they are all
read-only."
  ;; XXX: This procedure exists just to work around the inability of
  ;; 'copy-recursively' to preserve permissions.
  (file-system-fold (const #t)                    ;enter?
                    (lambda (file stat _)         ;leaf
                      (unless (eq? 'symlink (stat:type stat))
                        (chmod file
                               (if (zero? (logand (stat:mode stat)
                                                  #o100))
                                   #o444
                                   #o555))))
                    (const #t)                    ;down
                    (lambda (directory stat _)    ;up
                      (chmod directory #o555))
                    (const #f)                    ;skip
                    (const #f)                    ;error
                    #t
                    file
                    lstat))

(define* (populate-store reference-graphs target
                         #:key (log-port (current-error-port)))
  "Populate the store under directory TARGET with the items specified in
REFERENCE-GRAPHS, a list of reference-graph files."
  (define store
    (string-append target (%store-directory)))

  (define (things-to-copy)
    ;; Return the list of store files to copy to the image.
    (define (graph-from-file file)
      (map store-info-item
           (call-with-input-file file read-reference-graph)))

    (delete-duplicates (append-map graph-from-file reference-graphs)))

  (mkdir-p store)
  (chmod store #o1775)

  (let* ((things   (things-to-copy))
         (len      (length things))
         (progress (progress-reporter/bar len
                                          (format #f "copying ~a store items"
                                                  len)
                                          log-port)))
    (call-with-progress-reporter progress
      (lambda (report)
        (for-each (lambda (thing)
                    (copy-recursively thing
                                      (string-append target thing)
                                      #:keep-mtime? #t
                                      #:log (%make-void-port "w"))

                    ;; XXX: Since 'copy-recursively' doesn't allow us to
                    ;; preserve permissions, we have to traverse TARGET to
                    ;; make sure everything is read-only.
                    (reset-permissions (string-append target thing))
                    (report))
                  things)))))

;;; store-copy.scm ends here
