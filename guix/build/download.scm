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

(define-module (guix build download)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (guix ftp-client)
  #:use-module (guix build utils)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (url-fetch))

;;; Commentary:
;;;
;;; Fetch data such as tarballs over HTTP or FTP (builder-side code).
;;;
;;; Code:

(define* (progress-proc file size #:optional (log-port (current-output-port)))
  "Return a procedure to show the progress of FILE's download, which is
SIZE byte long.  The returned procedure is suitable for use as an
argument to `dump-port'.  The progress report is written to LOG-PORT."
  (if (number? size)
      (lambda (transferred cont)
        (let ((% (* 100.0 (/ transferred size))))
          (display #\cr log-port)
          (format log-port "~a\t~5,1f% of ~,1f KiB"
                  file % (/ size 1024.0))
          (flush-output-port log-port)
          (cont)))
      (lambda (transferred cont)
        (display #\cr log-port)
        (format log-port "~a\t~6,1f KiB transferred"
                file (/ transferred 1024.0))
        (flush-output-port log-port)
        (cont))))

(define (ftp-fetch uri file)
  "Fetch data from URI and write it to FILE.  Return FILE on success."
  (let* ((conn (ftp-open (uri-host uri)))
         (size (false-if-exception (ftp-size conn (uri-path uri))))
         (in   (ftp-retr conn (basename (uri-path uri))
                         (dirname (uri-path uri)))))
    (call-with-output-file file
      (lambda (out)
        (dump-port in out
                   #:buffer-size 65536            ; don't flood the log
                   #:progress (progress-proc (uri->string uri) size))))

    (ftp-close conn))
    (newline)
  file)

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
      (delete-duplicates (getaddrinfo (uri-host uri)
                                      (number->string port)
                                      AI_NUMERICSERV)
                         (lambda (ai1 ai2)
                           (equal? (addrinfo:addr ai1)
                                   (addrinfo:addr ai2))))))

  (let loop ((addresses addresses))
    (let* ((ai (car addresses))
           (s  (with-fluids ((%default-port-encoding #f))
                 ;; Restrict ourselves to TCP.
                 (socket (addrinfo:fam ai) SOCK_STREAM IPPROTO_IP))))
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
          (if (null? (cdr addresses))
              (apply throw args)
              (loop (cdr addresses))))))))

;; XXX: This is an awful hack to make sure the (set-port-encoding! p
;; "ISO-8859-1") call in `read-response' passes, even during bootstrap
;; where iconv is not available.
(module-define! (resolve-module '(web response))
                'set-port-encoding!
                (lambda (p e) #f))

;; XXX: Work around <http://bugs.gnu.org/13095>, present in Guile
;; up to 2.0.7.
(module-define! (resolve-module '(web client))
                'shutdown (const #f))

(define (http-fetch uri file)
  "Fetch data from URI and write it to FILE.  Return FILE on success."

  (let*-values (((connection)
                 (open-connection-for-uri uri))
                ((resp bv-or-port)
                 ;; XXX: `http-get*' was introduced in 2.0.7.  We know
                 ;; we're using it within the chroot, but
                 ;; `guix-download' might be using a different version.
                 ;; So keep this compatibility hack for now.
                 (if (module-defined? (resolve-interface '(web client))
                                      'http-get*)
                     (http-get* uri #:port connection #:decode-body? #f)
                     (http-get uri #:port connection #:decode-body? #f)))
                ((code)
                 (response-code resp))
                ((size)
                 (response-content-length resp)))
    (case code
      ((200)                                      ; OK
       (begin
         (call-with-output-file file
           (lambda (p)
             (if (port? bv-or-port)
                 (begin
                   (dump-port bv-or-port p
                              #:buffer-size 65536  ; don't flood the log
                              #:progress (progress-proc (uri->string uri)
                                                        size))
                   (newline))
                 (put-bytevector p bv-or-port))))
         file))
      ((302)                                      ; found (redirection)
       (let ((uri (response-location resp)))
         (format #t "following redirection to `~a'...~%"
                 (uri->string uri))
         (close connection)
         (http-fetch uri file)))
      (else
       (error "download failed" (uri->string uri)
              code (response-reason-phrase resp))))))


(define-syntax-rule (false-if-exception* body ...)
  "Like `false-if-exception', but print the exception on the error port."
  (catch #t
    (lambda ()
      body ...)
    (lambda (key . args)
      #f)
    (lambda (key . args)
      (print-exception (current-error-port) #f key args))))

(define* (url-fetch url file #:key (mirrors '()))
  "Fetch FILE from URL; URL may be either a single string, or a list of
string denoting alternate URLs for FILE.  Return #f on failure, and FILE
on success."
  (define (uri-vicinity dir file)
    ;; Concatenate DIR, slash, and FILE, keeping only one slash in between.
    ;; This is required by some HTTP servers.
    (string-append (string-trim-right dir #\/) "/"
                   (string-trim file #\/)))

  (define (maybe-expand-mirrors uri)
    (case (uri-scheme uri)
      ((mirror)
       (let ((kind (string->symbol (uri-host uri)))
             (path (uri-path uri)))
         (match (assoc-ref mirrors kind)
           ((mirrors ..1)
            (map (compose string->uri (cut uri-vicinity <> path))
                 mirrors))
           (_
            (error "unsupported URL mirror kind" kind uri)))))
      (else
       (list uri))))

  (define uri
    (append-map maybe-expand-mirrors
                (match url
                  ((_ ...) (map string->uri url))
                  (_       (list (string->uri url))))))

  (define (fetch uri file)
    (format #t "starting download of `~a' from `~a'...~%"
            file (uri->string uri))
    (case (uri-scheme uri)
      ((http)  (false-if-exception* (http-fetch uri file)))
      ((ftp)   (false-if-exception* (ftp-fetch uri file)))
      (else
       (format #t "skipping URI with unsupported scheme: ~s~%"
               uri)
       #f)))

  (setvbuf (current-output-port) _IOLBF)
  (setvbuf (current-error-port) _IOLBF)

  (let try ((uri uri))
    (match uri
      ((uri tail ...)
       (or (fetch uri file)
           (try tail)))
      (()
       (format (current-error-port) "failed to download ~s from ~s~%"
               file url)
       #f))))

;;; download.scm ends here
