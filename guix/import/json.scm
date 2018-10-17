;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix import json)
  #:use-module (json)
  #:use-module (guix http-client)
  #:use-module (guix import utils)
  #:use-module (srfi srfi-34)
  #:export (json-fetch
            json-fetch-alist))

(define* (json-fetch url
                     ;; Note: many websites returns 403 if we omit a
                     ;; 'User-Agent' header.
                     #:key (headers `((user-agent . "GNU Guile")
                                      (Accept . "application/json"))))
  "Return a representation of the JSON resource URL (a list or hash table), or
#f if URL returns 403 or 404.  HEADERS is a list of HTTP headers to pass in
the query."
  (guard (c ((and (http-get-error? c)
                  (let ((error (http-get-error-code c)))
                    (or (= 403 error)
                        (= 404 error))))
             #f))
    (let* ((port   (http-fetch url #:headers headers))
           (result (json->scm port)))
      (close-port port)
      result)))

(define (json-fetch-alist url)
  "Return an alist representation of the JSON resource URL, or #f if URL
returns 403 or 404."
  (and=> (json-fetch url)
         hash-table->alist))
