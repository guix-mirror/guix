;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2012, 2015 Free Software Foundation, Inc.
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (guix http-client)
  #:use-module (web uri)
  #:use-module (web http)
  #:use-module ((web client) #:hide (open-socket-for-uri))
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix base64)
  #:autoload   (gcrypt hash) (sha256)
  #:autoload   (gnutls) (error/invalid-session)
  #:use-module ((guix build utils)
                #:select (mkdir-p dump-port))
  #:use-module ((guix build download)
                #:select (open-socket-for-uri
                          (open-connection-for-uri
                           . guix:open-connection-for-uri)
                          resolve-uri-reference))
  #:re-export (open-socket-for-uri)
  #:export (&http-get-error
            http-get-error?
            http-get-error-uri
            http-get-error-code
            http-get-error-reason

            http-fetch
            http-multiple-get

            %http-cache-ttl
            http-fetch/cached))

;;; Commentary:
;;;
;;; HTTP client portable among Guile versions, and with proper error condition
;;; reporting.
;;;
;;; Code:

;; HTTP GET error.
(define-condition-type &http-get-error &error
  http-get-error?
  (uri    http-get-error-uri)                     ; URI
  (code   http-get-error-code)                    ; integer
  (reason http-get-error-reason))                 ; string


(define* (http-fetch uri #:key port (text? #f) (buffered? #t)
                     (open-connection guix:open-connection-for-uri)
                     (keep-alive? #f)
                     (verify-certificate? #t)
                     (headers '((user-agent . "GNU Guile")))
                     (log-port (current-error-port))
                     timeout)
  "Return an input port containing the data at URI, and the expected number of
bytes available or #f.  If TEXT? is true, the data at URI is considered to be
textual.  Follow any HTTP redirection.  When BUFFERED? is #f, return an
unbuffered port, suitable for use in `filtered-port'.  HEADERS is an alist of
extra HTTP headers.

When KEEP-ALIVE? is true, the connection is marked as 'keep-alive' and PORT is
not closed upon completion.

When VERIFY-CERTIFICATE? is true, verify HTTPS server certificates.

TIMEOUT specifies the timeout in seconds for connection establishment; when
TIMEOUT is #f, connection establishment never times out.

Write information about redirects to LOG-PORT.

Raise an '&http-get-error' condition if downloading fails."
  (let loop ((uri (if (string? uri)
                      (string->uri uri)
                      uri)))
    (let ((port (or port (open-connection uri
                                          #:verify-certificate?
                                          verify-certificate?
                                          #:timeout timeout)))
          (headers (match (uri-userinfo uri)
                     ((? string? str)
                      (cons (cons 'Authorization
                                  (string-append "Basic "
                                                 (base64-encode
                                                  (string->utf8 str))))
                            headers))
                     (_ headers))))
      (unless (or buffered? (not (file-port? port)))
        (setvbuf port 'none))
      (let*-values (((resp data)
                     (http-get uri #:streaming? #t #:port port
                               #:keep-alive? keep-alive?
                               #:headers headers))
                    ((code)
                     (response-code resp)))
        (case code
          ((200)
           (values data (response-content-length resp)))
          ((301                                   ; moved permanently
            302                                   ; found (redirection)
            303                                   ; see other
            307                                   ; temporary redirection
            308)                                  ; permanent redirection
           (let ((uri (resolve-uri-reference (response-location resp) uri)))
             (close-port port)
             (format log-port (G_ "following redirection to `~a'...~%")
                     (uri->string uri))
             (loop uri)))
          (else
           (raise (condition (&http-get-error
                              (uri uri)
                              (code code)
                              (reason (response-reason-phrase resp)))
                             (&message
                              (message
                               (format
                                #f
                                (G_ "~a: HTTP download failed: ~a (~s)")
                                (uri->string uri) code
                                (response-reason-phrase resp))))))))))))

(define-syntax-rule (false-if-networking-error exp)
  "Return #f if EXP triggers a network related exception as can occur when
reusing stale cached connections."
  ;; FIXME: Duplicated from 'with-cached-connection'.
  (catch #t
    (lambda ()
      exp)
    (lambda (key . args)
      ;; If PORT was cached and the server closed the connection in the
      ;; meantime, we get EPIPE.  In that case, open a fresh connection and
      ;; retry.  We might also get 'bad-response or a similar exception from
      ;; (web response) later on, once we've sent the request, or a
      ;; ERROR/INVALID-SESSION from GnuTLS.
      (if (or (and (eq? key 'system-error)
                   (= EPIPE (system-error-errno `(,key ,@args))))
              (and (eq? key 'gnutls-error)
                   (eq? (first args) error/invalid-session))
              (memq key
                    '(bad-response bad-header bad-header-component)))
          #f
          (apply throw key args)))))

(define* (http-multiple-get base-uri proc seed requests
                            #:key port (verify-certificate? #t)
                            (open-connection guix:open-connection-for-uri)
                            (keep-alive? #t)
                            (batch-size 1000))
  "Send all of REQUESTS to the server at BASE-URI.  Call PROC for each
response, passing it the request object, the response, a port from which to
read the response body, and the previous result, starting with SEED, à la
'fold'.  Return the final result.

When PORT is specified, use it as the initial connection on which HTTP
requests are sent; otherwise call OPEN-CONNECTION to open a new connection for
a URI.  When KEEP-ALIVE? is false, close the connection port before
returning."
  (let connect ((port     port)
                (requests requests)
                (result   seed))
    (define batch
      (if (>= batch-size (length requests))
          requests
          (take requests batch-size)))

    ;; (format (current-error-port) "connecting (~a requests left)..."
    ;;         (length requests))
    (let ((p (or port (open-connection base-uri
                                       #:verify-certificate?
                                       verify-certificate?))))
      ;; For HTTPS, P is not a file port and does not support 'setvbuf'.
      (when (file-port? p)
        (setvbuf p 'block (expt 2 16)))

      ;; Send BATCH in a row.
      ;; XXX: Do our own caching to work around inefficiencies when
      ;; communicating over TLS: <http://bugs.gnu.org/22966>.
      (let-values (((buffer get) (open-bytevector-output-port)))
        ;; Inherit the HTTP proxying property from P.
        (set-http-proxy-port?! buffer (http-proxy-port? p))

        (unless (false-if-networking-error
                 (begin
                   (for-each (cut write-request <> buffer) batch)
                   (put-bytevector p (get))
                   (force-output p)
                   #t))
          ;; If PORT becomes unusable, open a fresh connection and retry.
          (close-port p)                          ; close the broken port
          (connect #f requests result)))

      ;; Now start processing responses.
      (let loop ((sent      batch)
                 (processed 0)
                 (result    result))
        (match sent
          (()
           (match (drop requests processed)
             (()
              (unless keep-alive?
                (close-port p))
              (reverse result))
             (remainder
              (connect p remainder result))))
          ((head tail ...)
           (match (false-if-networking-error (read-response p))
             ((? response? resp)
              (let* ((body   (response-body-port resp))
                     (result (proc head resp body result)))
                ;; The server can choose to stop responding at any time,
                ;; in which case we have to try again.  Check whether
                ;; that is the case.  Note that even upon "Connection:
                ;; close", we can read from BODY.
                (match (assq 'connection (response-headers resp))
                  (('connection 'close)
                   (close-port p)
                   (connect #f                    ;try again
                            (drop requests (+ 1 processed))
                            result))
                  (_
                   (loop tail (+ 1 processed) result)))))
             (#f
              (close-port p)
              (connect #f                         ; try again
                       (drop requests processed)
                       result)))))))))


;;;
;;; Caching.
;;;

(define %http-cache-ttl
  ;; Time-to-live in seconds of the HTTP cache of in ~/.cache/guix.
  (make-parameter
   (* 3600 (or (and=> (getenv "GUIX_HTTP_CACHE_TTL")
                      string->number*)
               36))))

(define (cache-file-for-uri uri)
  "Return the name of the file in the cache corresponding to URI."
  (let ((digest (sha256 (string->utf8 (uri->string uri)))))
    ;; Use the "URL" alphabet because it does not contain "/".
    (string-append (cache-directory) "/http/"
                   (base64-encode digest 0 (bytevector-length digest)
                                  #f #f base64url-alphabet))))

(define* (http-fetch/cached uri #:key (ttl (%http-cache-ttl)) text?
                            (write-cache dump-port)
                            (cache-miss (const #t))
                            (log-port (current-error-port))
                            (timeout 10))
  "Like 'http-fetch', return an input port, but cache its contents in
~/.cache/guix.  The cache remains valid for TTL seconds.

Call WRITE-CACHE with the HTTP input port and the cache output port to write
the data to cache.  Call CACHE-MISS with URI just before fetching data from
URI.

TIMEOUT specifies the timeout in seconds for connection establishment.

Write information about redirects to LOG-PORT."
  (let ((file (cache-file-for-uri uri)))
    (define (update-cache cache-port)
      (define cache-time
        (and cache-port
             (stat:mtime (stat cache-port))))

      (define headers
        `((user-agent . "GNU Guile")
          ,@(if cache-time
                `((if-modified-since
                   . ,(time-utc->date (make-time time-utc 0 cache-time))))
                '())))

      ;; Update the cache and return an input port.
      (guard (c ((http-get-error? c)
                 (if (= 304 (http-get-error-code c)) ;"Not Modified"
                     (begin
                       (utime file)               ;update FILE's mtime
                       cache-port)
                     (raise c))))
        (let ((port (http-fetch uri #:text? text?
                                #:log-port log-port
                                #:headers headers #:timeout timeout)))
          (cache-miss uri)
          (mkdir-p (dirname file))
          (when cache-port
            (close-port cache-port))
          (with-atomic-file-output file
            (cut write-cache port <>))
          (close-port port)
          (open-input-file file))))

    (define (old? port)
      ;; Return true if PORT has passed TTL.
      (let* ((s   (stat port))
             (now (current-time time-utc)))
        (< (+ (stat:mtime s) ttl) (time-second now))))

    (catch 'system-error
      (lambda ()
        (let ((port (open-input-file file)))
          (if (old? port)
              (update-cache port)
              port)))
      (lambda args
        (if (= ENOENT (system-error-errno args))
            (update-cache #f)
            (apply throw args))))))

;;; http-client.scm ends here
