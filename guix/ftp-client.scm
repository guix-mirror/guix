;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2010, 2011, 2012 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix ftp-client)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-31)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:export (ftp-connection?
            ftp-connection-addrinfo

            ftp-open
            ftp-close
            ftp-chdir
            ftp-list
            ftp-retr))

;;; Commentary:
;;;
;;; Simple FTP client (RFC 959).
;;;
;;; Code:

;; TODO: Use SRFI-3{4,5} error conditions.

(define-record-type <ftp-connection>
  (%make-ftp-connection socket addrinfo)
  ftp-connection?
  (socket    ftp-connection-socket)
  (addrinfo  ftp-connection-addrinfo))

(define %ftp-ready-rx
  (make-regexp "^([0-9]{3}) (.+)$"))

(define (%ftp-listen port)
  (let loop ((line (read-line port)))
    (cond ((eof-object? line) (values line #f))
          ((regexp-exec %ftp-ready-rx line)
           =>
           (lambda (match)
             (values (string->number (match:substring match 1))
                     (match:substring match 2))))
          (else
           (loop (read-line port))))))

(define (%ftp-command command expected-code port)
  (format port "~A~A~A" command (string #\return) (string #\newline))
  (let-values (((code message) (%ftp-listen port)))
    (if (eqv? code expected-code)
        message
        (throw 'ftp-error port command code message))))

(define (%ftp-login user pass port)
  (let ((command (string-append "USER " user (string #\newline))))
    (display command port)
    (let-values (((code message) (%ftp-listen port)))
      (case code
        ((230)  #t)
        ((331) (%ftp-command (string-append "PASS " pass) 230 port))
        (else  (throw 'ftp-error port command code message))))))

(define (ftp-open host)
  "Open an FTP connection to HOST, and return it."
  (catch 'getaddrinfo-error
    (lambda ()
      (define addresses
        (getaddrinfo host "ftp"))

      (let loop ((addresses addresses))
        (let* ((ai (car addresses))
               (s  (socket (addrinfo:fam ai) (addrinfo:socktype ai)
                           (addrinfo:protocol ai))))

          (catch 'system-error
            (lambda ()
              (connect s (addrinfo:addr ai))
              (setvbuf s _IOLBF)
              (let-values (((code message) (%ftp-listen s)))
                (if (eqv? code 220)
                    (begin
                      ;;(%ftp-command "OPTS UTF8 ON" 200 s)
                      (%ftp-login "anonymous" "guix@example.com" s)
                      (%make-ftp-connection s ai))
                    (begin
                      (format (current-error-port)
                              "FTP to `~a' failed: ~A: ~A~%"
                              host code message)
                      (close s)
                      #f))))

            (lambda args
              ;; Connection failed, so try one of the other addresses.
              (close s)
              (if (null? addresses)
                  (apply throw args)
                  (loop (cdr addresses))))))))
    (lambda (key errcode)
      (format (current-error-port) "failed to resolve `~a': ~a~%"
              host (gai-strerror errcode))
      #f)))

(define (ftp-close conn)
  (close (ftp-connection-socket conn)))

(define (ftp-chdir conn dir)
  (%ftp-command (string-append "CWD " dir) 250
                (ftp-connection-socket conn)))

(define (ftp-pasv conn)
  (define %pasv-rx
    (make-regexp "([0-9]+),([0-9]+),([0-9]+),([0-9]+),([0-9]+),([0-9]+)"))

  (let ((message (%ftp-command "PASV" 227 (ftp-connection-socket conn))))
    (cond ((regexp-exec %pasv-rx message)
           =>
           (lambda (match)
             (+ (* (string->number (match:substring match 5)) 256)
                (string->number (match:substring match 6)))))
          (else
           (throw 'ftp-error conn "PASV" 227 message)))))

(define (address-with-port sa port)
  "Return a socket-address object based on SA, but with PORT."
  (let ((fam  (sockaddr:fam sa))
        (addr (sockaddr:addr sa)))
    (cond ((= fam AF_INET)
           (make-socket-address fam addr port))
          ((= fam AF_INET6)
           (make-socket-address fam addr port
                                (sockaddr:flowinfo sa)
                                (sockaddr:scopeid sa)))
          (else #f))))

(define* (ftp-list conn #:optional directory)
  (if directory
      (ftp-chdir conn directory))

  (let* ((port (ftp-pasv conn))
         (ai   (ftp-connection-addrinfo conn))
         (s    (socket (addrinfo:fam ai) (addrinfo:socktype ai)
                       (addrinfo:protocol ai))))
    (connect s (address-with-port (addrinfo:addr ai) port))
    (setvbuf s _IOLBF)

    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (%ftp-command "LIST" 150 (ftp-connection-socket conn))

        (let loop ((line   (read-line s))
                   (result '()))
          (cond ((eof-object? line) (reverse result))
                ((regexp-exec %ftp-ready-rx line)
                 =>
                 (lambda (match)
                   (let ((code (string->number (match:substring match 1))))
                     (if (= 126 code)
                         (reverse result)
                         (throw 'ftp-error conn "LIST" code)))))
                (else
                 (loop (read-line s)
                       (match (reverse (string-tokenize line))
                         ((file _ ... permissions)
                          (let ((type (case (string-ref permissions 0)
                                        ((#\d) 'directory)
                                        (else 'file))))
                            (cons (list file type) result)))
                         ((file _ ...)
                          (cons (cons file 'file) result))))))))
      (lambda ()
        (close s)
        (let-values (((code message) (%ftp-listen (ftp-connection-socket conn))))
          (or (eqv? code 226)
              (throw 'ftp-error conn "LIST" code message)))))))

(define* (ftp-retr conn file #:optional directory)
  "Retrieve FILE from DIRECTORY (or, if omitted, the current directory) from
FTP connection CONN.  Return a binary port to that file.  The returned port
must be closed before CONN can be used for other purposes."
  (if directory
      (ftp-chdir conn directory))

  ;; Ask for "binary mode".
  (%ftp-command "TYPE I" 200 (ftp-connection-socket conn))

  (let* ((port (ftp-pasv conn))
         (ai   (ftp-connection-addrinfo conn))
         (s    (with-fluids ((%default-port-encoding #f))
                 (socket (addrinfo:fam ai) (addrinfo:socktype ai)
                         (addrinfo:protocol ai)))))
    (define (terminate)
      (close s)
      (let-values (((code message) (%ftp-listen (ftp-connection-socket conn))))
        (or (eqv? code 226)
            (throw 'ftp-error conn "LIST" code message))))

    (connect s (address-with-port (addrinfo:addr ai) port))
    (setvbuf s _IOLBF)

    (%ftp-command (string-append "RETR " file)
                  150 (ftp-connection-socket conn))

    (make-custom-binary-input-port "FTP RETR port"
                                   (rec (read! bv start count)
                                        (match (get-bytevector-n! s bv
                                                                  start count)
                                       ((? eof-object?) 0)
                                       (0
                                        ;; Nothing available yet, so try
                                        ;; again.  This is important because
                                        ;; the return value of `read!' makes
                                        ;; it impossible to distinguish
                                        ;; between "not yet" and "EOF".
                                        (read! bv start count))
                                       (read read)))
                                   #f #f          ; no get/set position
                                   terminate)))

;;; ftp-client.scm ends here
