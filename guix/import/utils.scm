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
  #:use-module (guix base32)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module ((guix build download) #:prefix build:)
  #:export (factorize-uri

            hash-table->alist
            flatten
            assoc-ref*

            url-fetch
            guix-hash-url

            string->license
            license->symbol

            snake-case
            beautify-description))

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

(define (guix-hash-url filename)
  "Return the hash of FILENAME in nix-base32 format."
  (bytevector->nix-base32-string (file-sha256 filename)))

(define (string->license str)
  "Convert the string STR into a license object."
  (match str
    ("GNU LGPL" license:lgpl2.0)
    ("GPL" license:gpl3)
    ((or "BSD" "BSD License") license:bsd-3)
    ((or "MIT" "MIT license" "Expat license") license:expat)
    ("Public domain" license:public-domain)
    ((or "Apache License, Version 2.0" "Apache 2.0") license:asl2.0)
    (_ #f)))

(define (license->symbol license)
  "Convert license to a symbol representing the variable the object is bound
to in the (guix licenses) module, or #f if there is no such known license."
  ;; TODO: Traverse list public variables in (guix licenses) instead so we
  ;; don't have to maintain a list manualy.
  (assoc-ref `((,license:lgpl2.0 . license:lgpl2.0)
               (,license:gpl3 . license:gpl3)
               (,license:bsd-3 . license:bsd-3)
               (,license:expat . license:expat)
               (,license:public-domain . license:public-domain)
               (,license:asl2.0 . license:asl2.0))
             license))

(define (snake-case str)
  "Return a downcased version of the string STR where underscores are replaced
with dashes."
  (string-join (string-split (string-downcase str) #\_) "-"))

(define (beautify-description description)
  "Improve the package DESCRIPTION by turning a beginning sentence fragment
into a proper sentence and by using two spaces between sentences."
  (let ((cleaned (if (string-prefix? "A " description)
                     (string-append "This package provides a"
                                    (substring description 1))
                     description)))
    ;; Use double spacing between sentences
    (regexp-substitute/global #f "\\. \\b"
                              cleaned 'pre ".  " 'post)))
