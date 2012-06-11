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

(define-module (guix build http)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-11)
  #:export (http-fetch))

;;; Commentary:
;;;
;;; Fetch data such as tarballs over HTTP (builder-side code).
;;;
;;; Code:

(define (http-fetch url file)
  "Fetch data from URL and write it to FILE.  Return FILE on success."

  ;; FIXME: Use a variant of `http-get' that returns a port instead of
  ;; loading everything in memory.
  (let-values (((resp bv)
                (http-get (string->uri url) #:decode-body? #f)))
    (call-with-output-file file
      (lambda (p)
        (put-bytevector p bv))))
  file)
