;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2012 Free Software Foundation, Inc.
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
  #:use-module (guix utils)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:export (&http-get-error
            http-get-error?
            http-get-error-uri
            http-get-error-code
            http-get-error-reason

            open-socket-for-uri
            http-fetch))

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


(define-syntax when-guile<=2.0.5
  (lambda (s)
    (syntax-case s ()
      ((_ body ...)
       ;; Always emit BODY, regardless of VERSION, because sometimes this code
       ;; might be compiled with a recent Guile and run with 2.0.5---e.g.,
       ;; when using "guix pull".
       #'(begin body ...)))))

(when-guile<=2.0.5
 ;; Backport of Guile commit 312e79f8 ("Add HTTP Chunked Encoding support to
 ;; web modules.").

 (use-modules (ice-9 rdelim))

 ;; Chunked Responses
 (define (read-chunk-header port)
   (let* ((str (read-line port))
          (extension-start (string-index str (lambda (c) (or (char=? c #\;)
                                                             (char=? c #\return)))))
          (size (string->number (if extension-start ; unnecessary?
                                    (substring str 0 extension-start)
                                    str)
                                16)))
     size))

 (define (read-chunk port)
   (let ((size (read-chunk-header port)))
     (read-chunk-body port size)))

 (define (read-chunk-body port size)
   (let ((bv (get-bytevector-n port size)))
     (get-u8 port)                                ; CR
     (get-u8 port)                                ; LF
     bv))

 (define* (make-chunked-input-port port #:key (keep-alive? #f))
   "Returns a new port which translates HTTP chunked transfer encoded
data from PORT into a non-encoded format. Returns eof when it has
read the final chunk from PORT. This does not necessarily mean
that there is no more data on PORT. When the returned port is
closed it will also close PORT, unless the KEEP-ALIVE? is true."
   (define (next-chunk)
     (read-chunk port))
   (define finished? #f)
   (define (close)
     (unless keep-alive?
       (close-port port)))
   (define buffer #vu8())
   (define buffer-size 0)
   (define buffer-pointer 0)
   (define (read! bv idx to-read)
     (define (loop to-read num-read)
       (cond ((or finished? (zero? to-read))
              num-read)
             ((<= to-read (- buffer-size buffer-pointer))
              (bytevector-copy! buffer buffer-pointer
                                bv (+ idx num-read)
                                to-read)
              (set! buffer-pointer (+ buffer-pointer to-read))
              (loop 0 (+ num-read to-read)))
             (else
              (let ((n (- buffer-size buffer-pointer)))
                (bytevector-copy! buffer buffer-pointer
                                  bv (+ idx num-read)
                                  n)
                (set! buffer (next-chunk))
                (set! buffer-pointer 0)
                (set! buffer-size (bytevector-length buffer))
                (set! finished? (= buffer-size 0))
                (loop (- to-read n)
                      (+ num-read n))))))
     (loop to-read 0))
   (make-custom-binary-input-port "chunked input port" read! #f #f close))

 (define (read-response-body* r)
   "Reads the response body from @var{r}, as a bytevector.  Returns
 @code{#f} if there was no response body."
   (define bad-response
     (@@ (web response) bad-response))

   (if (member '(chunked) (response-transfer-encoding r))
       (let ((chunk-port (make-chunked-input-port (response-port r)
                                                  #:keep-alive? #t)))
         (get-bytevector-all chunk-port))
       (let ((nbytes (response-content-length r)))
         ;; Backport of Guile commit 84dfde82ae8f6ec247c1c147c1e2ae50b207bad9
         ;; ("fix response-body-port for responses without content-length").
         (if nbytes
             (let ((bv (get-bytevector-n (response-port r) nbytes)))
               (if (= (bytevector-length bv) nbytes)
                   bv
                   (bad-response "EOF while reading response body: ~a bytes of ~a"
                                 (bytevector-length bv) nbytes)))
             (get-bytevector-all (response-port r))))))

 ;; Install this patch only on Guile 2.0.5.
 (unless (guile-version>? "2.0.5")
   (module-set! (resolve-module '(web response))
                'read-response-body read-response-body*)))

;; XXX: Work around <http://bugs.gnu.org/13095>, present in Guile
;; up to 2.0.7.
(module-define! (resolve-module '(web client))
                'shutdown (const #f))

(define* (open-socket-for-uri uri #:key (buffered? #t))
  "Return an open port for URI.  When BUFFERED? is false, the returned port is
unbuffered."
  (let ((s ((@ (web client) open-socket-for-uri) uri)))
    (unless buffered?
      (setvbuf s _IONBF))
    s))

(define* (http-fetch uri #:key port (text? #f) (buffered? #t))
  "Return an input port containing the data at URI, and the expected number of
bytes available or #f.  If TEXT? is true, the data at URI is considered to be
textual.  Follow any HTTP redirection.  When BUFFERED? is #f, return an
unbuffered port, suitable for use in `filtered-port'.

Raise an '&http-get-error' condition if downloading fails."
  (let loop ((uri uri))
    (let ((port (or port
                    (open-socket-for-uri uri
                                         #:buffered? buffered?))))
      (let*-values (((resp data)
                     ;; Try hard to use the API du jour to get an input port.
                     ;; On Guile 2.0.5 and before, we can only get a string or
                     ;; bytevector, and not an input port.  Work around that.
                     (if (guile-version>? "2.0.7")
                         (http-get uri #:streaming? #t #:port port) ; 2.0.9+
                         (if (defined? 'http-get*)
                             (http-get* uri #:decode-body? text?
                                        #:port port) ; 2.0.7
                             (http-get uri #:decode-body? text?
                                       #:port port)))) ; 2.0.5-
                    ((code)
                     (response-code resp)))
        (case code
          ((200)
           (let ((len (response-content-length resp)))
             (cond ((not data)
                    (begin
                      ;; Guile 2.0.5 and earlier did not support chunked
                      ;; transfer encoding, which is required for instance when
                      ;; fetching %PACKAGE-LIST-URL (see
                      ;; <http://lists.gnu.org/archive/html/guile-devel/2011-09/msg00089.html>).
                      ;; Normally the `when-guile<=2.0.5' block above fixes
                      ;; that, but who knows what could happen.
                      (warning (_ "using Guile ~a, which does not support ~s encoding~%")
                               (version)
                               (response-transfer-encoding resp))
                      (leave (_ "download failed; use a newer Guile~%")
                             uri resp)))
                   ((string? data)                ; `http-get' from 2.0.5-
                    (values (open-input-string data) len))
                   ((bytevector? data)            ; likewise
                    (values (open-bytevector-input-port data) len))
                   (else                          ; input port
                    (values data len)))))
          ((301                                   ; moved permanently
            302)                                  ; found (redirection)
           (let ((uri (response-location resp)))
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

;;; http-client.scm ends here
