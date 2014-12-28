;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:export (open-connection-for-uri
            url-fetch
            progress-proc
            uri-abbreviation))

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

(define* (uri-abbreviation uri #:optional (max-length 42))
  "If URI's string representation is larger than MAX-LENGTH, return an
abbreviation of URI showing the scheme, host, and basename of the file."
  (define uri-as-string
    (uri->string uri))

  (define (elide-path)
    (let ((path (uri-path uri)))
      (string-append (symbol->string (uri-scheme uri)) "://"

                     ;; `file' URIs have no host part.
                     (or (uri-host uri) "")

                     (string-append "/.../" (basename path)))))

  (if (> (string-length uri-as-string) max-length)
      (let ((short (elide-path)))
        (if (< (string-length short) (string-length uri-as-string))
            short
            uri-as-string))
      uri-as-string))

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
                   #:progress (progress-proc (uri-abbreviation uri) size))))

    (ftp-close conn))
    (newline)
  file)

;; Autoload GnuTLS so that this module can be used even when GnuTLS is
;; not available.  At compile time, this yields "possibly unbound
;; variable" warnings, but these are OK: we know that the variables will
;; be bound if we need them, because (guix download) adds GnuTLS as an
;; input in that case.

;; XXX: Use this hack instead of #:autoload to avoid compilation errors.
;; See <http://bugs.gnu.org/12202>.
(module-autoload! (current-module)
                  '(gnutls) '(make-session connection-end/client))

(define add-weak-reference
  (let ((table (make-weak-key-hash-table)))
    (lambda (from to)
      "Hold a weak reference from FROM to TO."
      (hashq-set! table from to))))

(define (tls-wrap port server)
  "Return PORT wrapped in a TLS connection to SERVER.  SERVER must be a DNS
host name without trailing dot."
  (define (log level str)
    (format (current-error-port)
            "gnutls: [~a|~a] ~a" (getpid) level str))

  (let ((session (make-session connection-end/client)))

    ;; Some servers such as 'cloud.github.com' require the client to support
    ;; the 'SERVER NAME' extension.  However, 'set-session-server-name!' is
    ;; not available in older GnuTLS releases.  See
    ;; <http://bugs.gnu.org/18526> for details.
    (if (module-defined? (resolve-interface '(gnutls))
                         'set-session-server-name!)
        (set-session-server-name! session server-name-type/dns server)
        (format (current-error-port)
                "warning: TLS 'SERVER NAME' extension not supported~%"))

    (set-session-transport-fd! session (fileno port))
    (set-session-default-priority! session)
    (set-session-credentials! session (make-certificate-credentials))

    ;; Uncomment the following lines in case of debugging emergency.
    ;;(set-log-level! 10)
    ;;(set-log-procedure! log)

    (handshake session)
    (let ((record (session-record-port session)))
      ;; Since we use `fileno' above, the file descriptor behind PORT would be
      ;; closed when PORT is GC'd.  If we used `port->fdes', it would instead
      ;; never be closed.  So we use `fileno', but keep a weak reference to
      ;; PORT, so the file descriptor gets closed when RECORD is GC'd.
      (add-weak-reference record port)
      record)))

(define (open-connection-for-uri uri)
  "Return an open input/output port for a connection to URI.

This is the same as Guile's `open-socket-for-uri', except that we always
use a numeric port argument, to avoid the need to go through libc's NSS,
which is not available during bootstrap."
  (define addresses
    (let ((port (or (uri-port uri)
                    (case (uri-scheme uri)
                      ((http) 80)           ; /etc/services, not for me!
                      ((https) 443)
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

          (if (eq? 'https (uri-scheme uri))
              (tls-wrap s (uri-host uri))
              s))
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

  (define post-2.0.7?
    (or (> (string->number (major-version)) 2)
        (> (string->number (minor-version)) 0)
        (> (string->number (micro-version)) 7)
        (string>? (version) "2.0.7")))

  (define headers
    '(;; Some web sites, such as http://dist.schmorp.de, would block you if
      ;; there's no 'User-Agent' header, presumably on the assumption that
      ;; you're a spammer.  So work around that.
      (User-Agent . "GNU Guile")

      ;; Some servers, such as https://alioth.debian.org, return "406 Not
      ;; Acceptable" when not explicitly told that everything is accepted.
      (Accept . "*/*")))

  (let*-values (((connection)
                 (open-connection-for-uri uri))
                ((resp bv-or-port)
                 ;; XXX: `http-get*' was introduced in 2.0.7, and replaced by
                 ;; #:streaming? in 2.0.8.  We know we're using it within the
                 ;; chroot, but `guix-download' might be using a different
                 ;; version.  So keep this compatibility hack for now.
                 (if post-2.0.7?
                     (http-get uri #:port connection #:decode-body? #f
                               #:streaming? #t
                               #:headers headers)
                     (if (module-defined? (resolve-interface '(web client))
                                          'http-get*)
                         (http-get* uri #:port connection #:decode-body? #f
                                    #:headers headers)
                         (http-get uri #:port connection #:decode-body? #f
                                   #:extra-headers headers))))
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
                              #:buffer-size 65536 ; don't flood the log
                              #:progress (progress-proc (uri-abbreviation uri)
                                                        size))
                   (newline))
                 (put-bytevector p bv-or-port))))
         file))
      ((301                                       ; moved permanently
        302)                                      ; found (redirection)
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
      ((http https)
       (false-if-exception* (http-fetch uri file)))
      ((ftp)
       (false-if-exception* (ftp-fetch uri file)))
      (else
       (format #t "skipping URI with unsupported scheme: ~s~%"
               uri)
       #f)))

  ;; Make this unbuffered so 'progress-proc' works as expected.  _IOLBF means
  ;; '\n', not '\r', so it's not appropriate here.
  (setvbuf (current-output-port) _IONBF)

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
