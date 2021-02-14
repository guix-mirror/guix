;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix ftp-client)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-31)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:export (ftp-connection?
            ftp-connection-addrinfo

            connect*
            ftp-open
            ftp-close
            ftp-chdir
            ftp-size
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
  (let ((command (string-append "USER " user
                                (string #\return) (string #\newline))))
    (display command port)
    (let-values (((code message) (%ftp-listen port)))
      (case code
        ((230)  #t)
        ((331) (%ftp-command (string-append "PASS " pass) 230 port))
        (else  (throw 'ftp-error port command code message))))))

(define-syntax-rule (catch-EINPROGRESS body ...)
  (catch 'system-error
    (lambda ()
      body ...)
    (lambda args
      (unless (= (system-error-errno args) EINPROGRESS)
        (apply throw args)))))

;; XXX: For lack of a better place.
(define* (connect* s sockaddr #:optional timeout)
  "When TIMEOUT is omitted or #f, this procedure is equivalent to 'connect'.
When TIMEOUT is a number, it is the (possibly inexact) maximum number of
seconds to wait for the connection to succeed."
  (define (raise-error errno)
    (throw 'system-error 'connect* "~A"
           (list (strerror errno))
           (list errno)))

  (if timeout
      (let ((flags (fcntl s F_GETFL)))
        (fcntl s F_SETFL (logior flags O_NONBLOCK))
        (catch-EINPROGRESS (connect s sockaddr))
        (match (select '() (list s) (list s) timeout)
          ((() () ())
           ;; Time is up!
           (raise-error ETIMEDOUT))
          ((() (write) ())
           ;; Check for ECONNREFUSED and the likes.
           (fcntl s F_SETFL flags)
           (let ((errno (getsockopt s SOL_SOCKET SO_ERROR)))
             (unless (zero? errno)
               (raise-error errno))))
          ((() () (except))
           ;; Seems like this cannot really happen, but who knows.
           (let ((errno (getsockopt s SOL_SOCKET SO_ERROR)))
             (raise-error errno)))))
      (connect s sockaddr)))

(define* (ftp-open host #:optional (port "ftp")
                        #:key timeout
                              (username "anonymous")
                              (password "guix@example.com"))
  "Open an FTP connection to HOST on PORT (a service-identifying string,
or a TCP port number), and return it.

When TIMEOUT is not #f, it must be a (possibly inexact) number denoting the
maximum duration in seconds to wait for the connection to complete; passed
TIMEOUT, an ETIMEDOUT error is raised."
  ;; Using "ftp" for PORT instead of 21 allows 'getaddrinfo' to return only
  ;; TCP/IP addresses (otherwise it would return SOCK_DGRAM and SOCK_RAW
  ;; addresses as well.)  With our bootstrap Guile, which includes a
  ;; statically-linked NSS, resolving "ftp" works well, as long as
  ;; /etc/services is available.

  (define addresses
    (getaddrinfo host
                 (if (number? port) (number->string port) port)
                 (if (number? port)
                     (logior AI_ADDRCONFIG AI_NUMERICSERV)
                     AI_ADDRCONFIG)))

  (let loop ((addresses addresses))
    (match addresses
      ((ai rest ...)
       (let ((s (socket (addrinfo:fam ai)
                        ;; TCP/IP only
                        SOCK_STREAM IPPROTO_IP)))

         (catch 'system-error
           (lambda ()
             (connect* s (addrinfo:addr ai) timeout)
             (setvbuf s 'line)
             (let-values (((code message) (%ftp-listen s)))
               (if (eqv? code 220)
                   (begin
                     ;;(%ftp-command "OPTS UTF8 ON" 200 s)
                     (%ftp-login username password s)
                     (%make-ftp-connection s ai))
                   (begin
                     (close s)
                     (throw 'ftp-error s "log-in" code message)))))

           (lambda args
             ;; Connection failed, so try one of the other addresses.
             (close s)
             (if (null? rest)
                 (apply throw args)
                 (loop rest)))))))))

(define (ftp-close conn)
  (close (ftp-connection-socket conn)))

(define %char-set:not-slash
  (char-set-complement (char-set #\/)))

(define (ftp-chdir conn dir)
  "Change to directory DIR."

  ;; On ftp.gnupg.org, "PASV" right after "CWD /gcrypt/gnupg" hangs.  Doing
  ;; CWD in two steps works, so just do this.
  (let ((components (string-tokenize dir %char-set:not-slash)))
    (fold (lambda (dir result)
            (%ftp-command (string-append "CWD " dir) 250
                          (ftp-connection-socket conn)))
          #f
          (if (string-prefix? "/" dir)
              (cons "/" components)
              components))))

(define (ftp-size conn file)
  "Return the size in bytes of FILE."

  ;; Ask for "binary mode", otherwise some servers, such as sourceware.org,
  ;; fail with 550 ("SIZE not allowed in ASCII mode").
  (%ftp-command "TYPE I" 200 (ftp-connection-socket conn))

  (let ((message (%ftp-command (string-append "SIZE " file) 213
                               (ftp-connection-socket conn))))
    (string->number (string-trim-both message))))

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

(define (ftp-epsv conn)
  (let* ((message (%ftp-command "EPSV" 229 (ftp-connection-socket conn))))
    (string->number
     (match:substring (string-match "\\(...([0-9]+).\\)" message) 1))))

(define (ftp-passive conn)
  "Enter passive mode using EPSV or PASV, return a data connection port on
success."
  ;; IPv6 only works with EPSV, so try it first.
  (or (false-if-exception (ftp-epsv conn)) (ftp-pasv conn)))

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

(define* (ftp-list conn #:optional directory #:key timeout)
  (if directory
      (ftp-chdir conn directory))

  (let* ((port (ftp-passive conn))
         (ai   (ftp-connection-addrinfo conn))
         (s    (socket (addrinfo:fam ai) (addrinfo:socktype ai)
                       (addrinfo:protocol ai))))
    (connect* s (address-with-port (addrinfo:addr ai) port) timeout)
    (setvbuf s 'line)

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

(define* (ftp-retr conn file #:optional directory
                   #:key timeout)
  "Retrieve FILE from DIRECTORY (or, if omitted, the current directory) from
FTP connection CONN.  Return a binary port to that file.  The returned port
must be closed before CONN can be used for other purposes."
  (if directory
      (ftp-chdir conn directory))

  ;; Ask for "binary mode".
  (%ftp-command "TYPE I" 200 (ftp-connection-socket conn))

  (let* ((port (ftp-passive conn))
         (ai   (ftp-connection-addrinfo conn))
         (s    (with-fluids ((%default-port-encoding #f))
                 (socket (addrinfo:fam ai) (addrinfo:socktype ai)
                         (addrinfo:protocol ai)))))
    (define (terminate)
      (close s)
      (let-values (((code message) (%ftp-listen (ftp-connection-socket conn))))
        (or (eqv? code 226)
            (throw 'ftp-error conn "LIST" code message))))

    (connect* s (address-with-port (addrinfo:addr ai) port) timeout)
    (setvbuf s 'line)

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
