;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2012, 2015 Free Software Foundation, Inc.
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
  #:use-module ((web client) #:hide (open-socket-for-uri))
  #:use-module (web response)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix base64)
  #:autoload   (guix hash) (sha256)
  #:use-module ((guix build utils)
                #:select (mkdir-p dump-port))
  #:use-module ((guix build download)
                #:select (open-socket-for-uri
                          open-connection-for-uri resolve-uri-reference))
  #:re-export (open-socket-for-uri)
  #:export (&http-get-error
            http-get-error?
            http-get-error-uri
            http-get-error-code
            http-get-error-reason

            http-fetch

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


(define-syntax when-guile<=2.0.5-or-otherwise-broken
  (lambda (s)
    (syntax-case s ()
      ((_ body ...)
       ;; Always emit BODY, regardless of VERSION, because sometimes this code
       ;; might be compiled with a recent Guile and run with 2.0.5---e.g.,
       ;; when using "guix pull".
       #'(begin body ...)))))

(when-guile<=2.0.5-or-otherwise-broken
 ;; Backport of Guile commits 312e79f8 ("Add HTTP Chunked Encoding support to
 ;; web modules."), 00d3ecf2 ("http: Do not buffer HTTP chunks."), and 53b8d5f
 ;; ("web: Gracefully handle premature EOF when reading chunk header.")

 (use-modules (ice-9 rdelim))

 (define %web-http
   (resolve-module '(web http)))

 ;; Chunked Responses
 (define (read-chunk-header port)
   "Read a chunk header from PORT and return the size in bytes of the
 upcoming chunk."
   (match (read-line port)
     ((? eof-object?)
      ;; Connection closed prematurely: there's nothing left to read.
      0)
     (str
      (let ((extension-start (string-index str
                                           (lambda (c)
                                             (or (char=? c #\;)
                                                 (char=? c #\return))))))
        (string->number (if extension-start       ; unnecessary?
                            (substring str 0 extension-start)
                            str)
                        16)))))

 (define* (make-chunked-input-port port #:key (keep-alive? #f))
   "Returns a new port which translates HTTP chunked transfer encoded
data from PORT into a non-encoded format. Returns eof when it has
read the final chunk from PORT. This does not necessarily mean
that there is no more data on PORT. When the returned port is
closed it will also close PORT, unless the KEEP-ALIVE? is true."
   (define (close)
     (unless keep-alive?
       (close-port port)))

   (define chunk-size 0)     ;size of the current chunk
   (define remaining 0)      ;number of bytes left from the current chunk
   (define finished? #f)     ;did we get all the chunks?

   (define (read! bv idx to-read)
     (define (loop to-read num-read)
       (cond ((or finished? (zero? to-read))
              num-read)
             ((zero? remaining)                    ;get a new chunk
              (let ((size (read-chunk-header port)))
                (set! chunk-size size)
                (set! remaining size)
                (if (zero? size)
                    (begin
                      (set! finished? #t)
                      num-read)
                    (loop to-read num-read))))
             (else                           ;read from the current chunk
              (let* ((ask-for (min to-read remaining))
                     (read    (get-bytevector-n! port bv (+ idx num-read)
                                                 ask-for)))
                (if (eof-object? read)
                    (begin                         ;premature termination
                      (set! finished? #t)
                      num-read)
                    (let ((left (- remaining read)))
                      (set! remaining left)
                      (when (zero? left)
                        ;; We're done with this chunk; read CR and LF.
                        (get-u8 port) (get-u8 port))
                      (loop (- to-read read)
                            (+ num-read read))))))))
     (loop to-read 0))

   (make-custom-binary-input-port "chunked input port" read! #f #f close))

 ;; Chunked encoding support in Guile <= 2.0.11 would load whole chunks in
 ;; memory---see <http://bugs.gnu.org/19939>.
 (when (module-variable %web-http 'read-chunk-body)
   (module-set! %web-http 'make-chunked-input-port make-chunked-input-port))

 (define (make-delimited-input-port port len keep-alive?)
   "Return an input port that reads from PORT, and makes sure that
exactly LEN bytes are available from PORT.  Closing the returned port
closes PORT, unless KEEP-ALIVE? is true."
   (define bytes-read 0)

   (define (fail)
     ((@@ (web response) bad-response)
      "EOF while reading response body: ~a bytes of ~a"
      bytes-read len))

   (define (read! bv start count)
     ;; Read at most LEN bytes in total.  HTTP/1.1 doesn't say what to do
     ;; when a server provides more than the Content-Length, but it seems
     ;; wise to just stop reading at LEN.
     (let ((count (min count (- len bytes-read))))
       (let loop ((ret (get-bytevector-n! port bv start count)))
         (cond ((eof-object? ret)
                (if (= bytes-read len)
                    0                              ; EOF
                    (fail)))
               ((and (zero? ret) (> count 0))
                ;; Do not return zero since zero means EOF, so try again.
                (loop (get-bytevector-n! port bv start count)))
               (else
                (set! bytes-read (+ bytes-read ret))
                ret)))))

   (define close
     (and (not keep-alive?)
          (lambda ()
            (close-port port))))

   (make-custom-binary-input-port "delimited input port" read! #f #f close))

 (define (read-header-line port)
   "Read an HTTP header line and return it without its final CRLF or LF.
Raise a 'bad-header' exception if the line does not end in CRLF or LF,
or if EOF is reached."
   (match (%read-line port)
     (((? string? line) . #\newline)
      ;; '%read-line' does not consider #\return a delimiter; so if it's
      ;; there, remove it.  We are more tolerant than the RFC in that we
      ;; tolerate LF-only endings.
      (if (string-suffix? "\r" line)
          (string-drop-right line 1)
          line))
     ((line . _)                                ;EOF or missing delimiter
      ((@@ (web http) bad-header) 'read-header-line line))))

 (unless (guile-version>? "2.0.11")
   ;; Guile <= 2.0.9 had a bug whereby 'response-body-port' would read more
   ;; than what 'content-length' says.  See Guile commit 802a25b.
   ;; Guile <= 2.0.11 had a bug whereby the 'close' method of the response
   ;; body port would fail with wrong-arg-num.  See Guile commit 5a10e41.
   (module-set! (resolve-module '(web response))
                'make-delimited-input-port make-delimited-input-port)

   ;; Guile <= 2.0.11 was affected by <http://bugs.gnu.org/22273>.  See Guile
   ;; commit 4c7732c.
   (when (module-variable %web-http 'read-line*)
     (module-set! %web-http 'read-line* read-header-line))))

;; XXX: Work around <http://bugs.gnu.org/13095>, present in Guile
;; up to 2.0.7.
(module-define! (resolve-module '(web client))
                'shutdown (const #f))

(define* (http-fetch uri #:key port (text? #f) (buffered? #t)
                     keep-alive?)
  "Return an input port containing the data at URI, and the expected number of
bytes available or #f.  If TEXT? is true, the data at URI is considered to be
textual.  Follow any HTTP redirection.  When BUFFERED? is #f, return an
unbuffered port, suitable for use in `filtered-port'.  When KEEP-ALIVE? is
true, send a 'Connection: keep-alive' HTTP header, in which case PORT may be
reused for future HTTP requests.

Raise an '&http-get-error' condition if downloading fails."
  (let loop ((uri (if (string? uri)
                      (string->uri uri)
                      uri)))
    (let ((port (or port (open-connection-for-uri uri)))
          (auth-header (match (uri-userinfo uri)
                         ((? string? str)
                          (list (cons 'Authorization
                                      (string-append "Basic "
                                                     (base64-encode
                                                      (string->utf8 str))))))
                         (_ '()))))
      (unless (or buffered? (not (file-port? port)))
        (setvbuf port _IONBF))
      (let*-values (((resp data)
                     ;; Try hard to use the API du jour to get an input port.
                     (if (guile-version>? "2.0.7")
                         (http-get uri #:streaming? #t #:port port
                                   #:keep-alive? #t
                                   #:headers auth-header) ; 2.0.9+
                         (http-get* uri #:decode-body? text?        ; 2.0.7
                                    #:keep-alive? #t
                                    #:port port #:headers auth-header)))
                    ((code)
                     (response-code resp)))
        (case code
          ((200)
           (values data (response-content-length resp)))
          ((301                                   ; moved permanently
            302)                                  ; found (redirection)
           (let ((uri (resolve-uri-reference (response-location resp) uri)))
             (close-port port)
             (format #t (_ "following redirection to `~a'...~%")
                     (uri->string uri))
             (loop uri)))
          (else
           (raise (condition (&http-get-error
                              (uri uri)
                              (code code)
                              (reason (response-reason-phrase resp)))
                             (&message
                              (message "download failed"))))))))))


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

(define* (http-fetch/cached uri #:key (ttl (%http-cache-ttl)) text?)
  "Like 'http-fetch', return an input port, but cache its contents in
~/.cache/guix.  The cache remains valid for TTL seconds."
  (let ((file (cache-file-for-uri uri)))
    (define (update-cache)
      ;; Update the cache and return an input port.
      (let ((port (http-fetch uri #:text? text?)))
        (mkdir-p (dirname file))
        (with-atomic-file-output file
          (cut dump-port port <>))
        (close-port port)
        (open-input-file file)))

    (define (old? port)
      ;; Return true if PORT has passed TTL.
      (let* ((s   (stat port))
             (now (current-time time-utc)))
        (< (+ (stat:mtime s) ttl) (time-second now))))

    (catch 'system-error
      (lambda ()
        (let ((port (open-input-file file)))
          (if (old? port)
              (begin
                (close-port port)
                (update-cache))
              port)))
      (lambda args
        (if (= ENOENT (system-error-errno args))
            (update-cache)
            (apply throw args))))))

;;; http-client.scm ends here
