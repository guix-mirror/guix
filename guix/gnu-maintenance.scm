;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2012 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix gnu-maintenance)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:export (official-gnu-packages))

(define (http-fetch uri)
  "Return a string containing the textual data at URI, a string."
  (let*-values (((resp data)
                (http-get (string->uri uri)))
               ((code)
                (response-code resp)))
    (case code
      ((200)
       data)
      (else
       (error "download failed:" uri code
              (response-reason-phrase resp))))))

(define %package-list-url
  (string-append "http://cvs.savannah.gnu.org/"
                 "viewvc/*checkout*/gnumaint/"
                 "gnupackages.txt?root=womb"))

(define (official-gnu-packages)
  "Return a list of GNU packages."
  (define %package-line-rx
    (make-regexp "^package: (.+)$"))

  (let ((lst (string-split (http-fetch %package-list-url) #\nl)))
    (filter-map (lambda (line)
                  (and=> (regexp-exec %package-line-rx line)
                         (cut match:substring <> 1)))
                lst)))
