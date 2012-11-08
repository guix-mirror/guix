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
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-11)
  #:export (http-fetch))

;;; Commentary:
;;;
;;; Fetch data such as tarballs over HTTP (builder-side code).
;;;
;;; Code:

(define (open-connection-for-uri uri)
  "Return an open input/output port for a connection to URI.

This is the same as Guile's `open-socket-for-uri', except that we always
use a numeric port argument, to avoid the need to go through libc's NSS,
which is not available during bootstrap."
  (define addresses
    (let ((port (or (uri-port uri)
                    (case (uri-scheme uri)
                      ((http) 80)           ; /etc/services, not for me!
                      (else
                       (error "unsupported URI scheme" uri))))))
      (getaddrinfo (uri-host uri)
                   (number->string port)
                   AI_NUMERICSERV)))

  (let loop ((addresses addresses))
    (let* ((ai (car addresses))
           (s  (with-fluids ((%default-port-encoding #f))
                 (socket (addrinfo:fam ai) (addrinfo:socktype ai)
                         (addrinfo:protocol ai)))))
      (catch 'system-error
        (lambda ()
          (connect s (addrinfo:addr ai))

          ;; Buffer input and output on this port.
          (setvbuf s _IOFBF)
          ;; Enlarge the receive buffer.
          (setsockopt s SOL_SOCKET SO_RCVBUF (* 12 1024))
          s)
        (lambda args
          ;; Connection failed, so try one of the other addresses.
          (close s)
          (if (null? addresses)
              (apply throw args)
              (loop (cdr addresses))))))))

;; XXX: This is an awful hack to make sure the (set-port-encoding! p
;; "ISO-8859-1") call in `read-response' passes, even during bootstrap
;; where iconv is not available.
(module-define! (resolve-module '(web response))
                'set-port-encoding!
                (lambda (p e) #f))

(define (http-fetch url file)
  "Fetch data from URL and write it to FILE.  Return FILE on success."

  ;; FIXME: Use a variant of `http-get' that returns a port instead of
  ;; loading everything in memory.
  (let*-values (((uri)
                 (string->uri url))
                ((connection)
                 (open-connection-for-uri uri))
                ((resp bv)
                 (http-get uri #:port connection #:decode-body? #f))
                ((code)
                 (response-code resp)))
    (if (= 200 code)
        (begin
          (call-with-output-file file
            (lambda (p)
              (put-bytevector p bv)))
          file)
        (error "download failed" url
               code (response-reason-phrase resp)))))
