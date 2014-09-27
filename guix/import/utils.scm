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
  #:export (factorize-uri))

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
