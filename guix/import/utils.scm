;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix import utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (guix hash)
  #:use-module (guix utils)
  #:use-module ((guix build download) #:prefix build:)
  #:export (factorize-uri

            hash-table->alist
            flatten
            assoc-ref*

            url-fetch))

(define (factorize-uri uri version)
  "Factorize URI, a package tarball URI as a string, such that any occurrences
of the string VERSION is replaced by the symbol 'version."
  (let ((version-rx (make-regexp (regexp-quote version))))
    (match (regexp-exec version-rx uri)
      (#f
       uri)
      (_
       (let ((indices (fold-matches version-rx uri
                                    '((0))
                                    (lambda (m result)
                                      (match result
                                        (((start) rest ...)
                                         `((,(match:end m))
                                           (,start . ,(match:start m))
                                           ,@rest)))))))
         (fold (lambda (index result)
                 (match index
                   ((start)
                    (cons (substring uri start)
                          result))
                   ((start . end)
                    (cons* (substring uri start end)
                           'version
                           result))))
               '()
               indices))))))

(define (hash-table->alist table)
  "Return an alist represenation of TABLE."
  (map (match-lambda
        ((key . (lst ...))
         (cons key
               (map (lambda (x)
                      (if (hash-table? x)
                          (hash-table->alist x)
                          x))
                    lst)))
        ((key . (? hash-table? table))
         (cons key (hash-table->alist table)))
        (pair pair))
       (hash-map->list cons table)))

(define (flatten lst)
  "Return a list that recursively concatenates all sub-lists of LST."
  (fold-right
   (match-lambda*
    (((sub-list ...) memo)
     (append (flatten sub-list) memo))
    ((elem memo)
     (cons elem memo)))
   '() lst))

(define (assoc-ref* alist key . rest)
  "Return the value for KEY from ALIST.  For each additional key specified,
recursively apply the procedure to the sub-list."
  (if (null? rest)
      (assoc-ref alist key)
      (apply assoc-ref* (assoc-ref alist key) rest)))

(define (url-fetch url file-name)
  "Save the contents of URL to FILE-NAME.  Return #f on failure."
  (parameterize ((current-output-port (current-error-port)))
    (build:url-fetch url file-name)))
