;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix tests http)
  #:use-module (ice-9 threads)
  #:use-module (web server)
  #:use-module (web server http)
  #:use-module (web response)
  #:use-module (srfi srfi-39)
  #:export (with-http-server
            call-with-http-server
            %http-server-port
            http-server-can-listen?
            %local-url))

;;; Commentary:
;;;
;;; Code to spawn a Web server for testing purposes.
;;;
;;; Code:

(define %http-server-port
  ;; TCP port to use for the stub HTTP server.
  (make-parameter 9999))

(define (open-http-server-socket)
  "Return a listening socket for the web server.  It is useful to export it so
that tests can check whether we succeeded opening the socket and tests skip if
needed."
  (catch 'system-error
    (lambda ()
      (let ((sock (socket PF_INET SOCK_STREAM 0)))
        (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
        (bind sock
              (make-socket-address AF_INET INADDR_LOOPBACK
                                   (%http-server-port)))
        sock))
    (lambda args
      (let ((err (system-error-errno args)))
        (format (current-error-port)
                "warning: cannot run Web server for tests: ~a~%"
                (strerror err))
        #f))))

(define (http-server-can-listen?)
  "Return #t if we managed to open a listening socket."
  (and=> (open-http-server-socket)
         (lambda (socket)
           (close-port socket)
           #t)))

(define (%local-url)
  ;; URL to use for 'home-page' tests.
  (string-append "http://localhost:" (number->string (%http-server-port))
                 "/foo/bar"))

(define* (call-with-http-server code data thunk
                                #:key (headers '()))
  "Call THUNK with an HTTP server running and returning CODE and DATA (a
string) on HTTP requests."
  (define (http-write server client response body)
    "Write RESPONSE."
    (let* ((response (write-response response client))
           (port     (response-port response)))
      (cond
       ((not body))                               ;pass
       (else
        (write-response-body response body)))
      (close-port port)
      (quit #t)                                   ;exit the server thread
      (values)))

  ;; Mutex and condition variable to synchronize with the HTTP server.
  (define %http-server-lock (make-mutex))
  (define %http-server-ready (make-condition-variable))

  (define (http-open . args)
    "Start listening for HTTP requests and signal %HTTP-SERVER-READY."
    (with-mutex %http-server-lock
      (let ((result (apply (@@ (web server http) http-open) args)))
        (signal-condition-variable %http-server-ready)
        result)))

  (define-server-impl stub-http-server
    ;; Stripped-down version of Guile's built-in HTTP server.
    http-open
    (@@ (web server http) http-read)
    http-write
    (@@ (web server http) http-close))

  (define (server-body)
    (define (handle request body)
      (values (build-response #:code code
                              #:reason-phrase "Such is life"
                              #:headers headers)
              data))

    (let ((socket (open-http-server-socket)))
      (catch 'quit
        (lambda ()
          (run-server handle stub-http-server
                      `(#:socket ,socket)))
        (lambda _
          (close-port socket)))))

  (with-mutex %http-server-lock
    (let ((server (make-thread server-body)))
      (wait-condition-variable %http-server-ready %http-server-lock)
      ;; Normally SERVER exits automatically once it has received a request.
      (thunk))))

(define-syntax with-http-server
  (syntax-rules ()
    ((_ (code headers) data body ...)
     (call-with-http-server code data (lambda () body ...)
                            #:headers headers))
    ((_ code data body ...)
     (call-with-http-server code data (lambda () body ...)))))

;;; http.scm ends here
