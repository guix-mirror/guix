;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

;;;
;;; Download a binary file from an external source.
;;;

(use-modules (ice-9 match)
             (web uri)
             (web client)
             (rnrs io ports)
             (srfi srfi-11)
             (guix utils))

(define %url-base
  "http://www.fdn.fr/~lcourtes/software/guix/packages")

(define (file-name->uri file)
  "Return the URI for FILE."
  (match (string-tokenize file (char-set-complement (char-set #\/)))
    ((_ ... system basename)
     (string->uri (string-append %url-base "/" system
                                 "/20121219/" basename)))))

(match (command-line)
  ((_ file expected-hash)
   (let ((uri (file-name->uri file)))
     (format #t "downloading file `~a' from `~a'...~%"
             file (uri->string uri))
     (let*-values (((resp data) (http-get uri #:decode-body? #f))
                   ((hash)      (bytevector->base16-string (sha256 data)))
                   ((part)      (string-append file ".part")))
       (if (string=? expected-hash hash)
           (begin
             (call-with-output-file part
               (lambda (port)
                 (put-bytevector port data)))
             (rename-file part file))
           (begin
             (format (current-error-port)
                     "file at `~a' has SHA256 ~a; expected ~a~%"
                     (uri->string uri) hash expected-hash)
             (exit 1)))))))
