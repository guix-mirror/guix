;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
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
  #:use-module ((web client) #:hide (open-socket-for-uri))
  #:use-module (web response)
  #:use-module (guix ftp-client)
  #:use-module (guix build utils)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (open-socket-for-uri
            open-connection-for-uri
            resolve-uri-reference
            maybe-expand-mirrors
            url-fetch
            progress-proc
            uri-abbreviation))

;;; Commentary:
;;;
;;; Fetch data such as tarballs over HTTP or FTP (builder-side code).
;;;
;;; Code:

(define %http-receive-buffer-size
  ;; Size of the HTTP receive buffer.
  65536)

(define (duration->seconds duration)
  "Return the number of seconds represented by DURATION, a 'time-duration'
object, as an inexact number."
  (+ (time-second duration)
     (/ (time-nanosecond duration) 1e9)))

(define (throughput->string throughput)
  "Given THROUGHPUT, measured in bytes per second, return a string
representing it in a human-readable way."
  (if (> throughput 3e6)
      (format #f "~,2f MiB/s" (/ throughput (expt 2. 20)))
      (format #f "~,0f KiB/s" (/ throughput 1024.0))))

(define* (progress-proc file size #:optional (log-port (current-output-port)))
  "Return a procedure to show the progress of FILE's download, which is
SIZE byte long.  The returned procedure is suitable for use as an
argument to `dump-port'.  The progress report is written to LOG-PORT."
  ;; XXX: Because of <http://bugs.gnu.org/19939> this procedure is often not
  ;; called as frequently as we'd like too; this is especially bad with Nginx
  ;; on hydra.gnu.org, which returns whole nars as a single chunk.
  (let ((start-time #f))
    (let-syntax ((with-elapsed-time
                     (syntax-rules ()
                       ((_ elapsed body ...)
                        (let* ((now     (current-time time-monotonic))
                               (elapsed (and start-time
                                             (duration->seconds
                                              (time-difference now
                                                               start-time)))))
                          (unless start-time
                            (set! start-time now))
                          body ...)))))
      (if (number? size)
          (lambda (transferred cont)
            (with-elapsed-time elapsed
              (let ((%          (* 100.0 (/ transferred size)))
                    (throughput (if elapsed
                                    (/ transferred elapsed)
                                    0)))
                (display #\cr log-port)
                (format log-port "~a\t~5,1f% of ~,1f KiB (~a)"
                        file % (/ size 1024.0)
                        (throughput->string throughput))
                (flush-output-port log-port)
                (cont))))
          (lambda (transferred cont)
            (with-elapsed-time elapsed
              (let ((throughput (if elapsed
                                    (/ transferred elapsed)
                                    0)))
                (display #\cr log-port)
                (format log-port "~a\t~6,1f KiB transferred (~a)"
                        file (/ transferred 1024.0)
                        (throughput->string throughput))
                (flush-output-port log-port)
                (cont))))))))

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
                   #:buffer-size %http-receive-buffer-size
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

(define (open-socket-for-uri uri)
  "Return an open port for URI.  This variant works around
<http://bugs.gnu.org/15368> which affects Guile's 'open-socket-for-uri' up to
2.0.11 included."
  (define rmem-max
    ;; The maximum size for a receive buffer on Linux, see socket(7).
    "/proc/sys/net/core/rmem_max")

  (define buffer-size
    (if (file-exists? rmem-max)
        (call-with-input-file rmem-max read)
        126976))                    ;the default for Linux, per 'rmem_default'

  (let ((s ((@ (web client) open-socket-for-uri) uri)))
    ;; Work around <http://bugs.gnu.org/15368> by restoring a decent
    ;; buffer size.
    (setsockopt s SOL_SOCKET SO_RCVBUF buffer-size)
    s))

(define (open-connection-for-uri uri)
  "Like 'open-socket-for-uri', but also handle HTTPS connections."
  (define https?
    (eq? 'https (uri-scheme uri)))

  (let-syntax ((with-https-proxy
                (syntax-rules ()
                  ((_ exp)
                   ;; For HTTPS URIs, honor 'https_proxy', not 'http_proxy'.
                   ;; FIXME: Proxying is not supported for https.
                   (let ((thunk (lambda () exp)))
                     (if (and https?
                              (module-variable
                               (resolve-interface '(web client))
                               'current-http-proxy))
                         (parameterize ((current-http-proxy #f))
                           (when (and=> (getenv "https_proxy")
                                        (negate string-null?))
                             (format (current-error-port)
                                     "warning: 'https_proxy' is ignored~%"))
                           (thunk))
                         (thunk)))))))
    (with-https-proxy
     (let ((s (open-socket-for-uri uri)))
       ;; Buffer input and output on this port.
       (setvbuf s _IOFBF %http-receive-buffer-size)

       (if https?
           (tls-wrap s (uri-host uri))
           s)))))

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

;; XXX: Work around <http://bugs.gnu.org/19840>, present in Guile
;; up to 2.0.11.
(unless (or (> (string->number (major-version)) 2)
            (> (string->number (minor-version)) 0)
            (> (string->number (micro-version)) 11))
  (let ((var (module-variable (resolve-module '(web http))
                              'declare-relative-uri-header!)))
    ;; If 'declare-relative-uri-header!' doesn't exist, forget it.
    (when (and var (variable-bound? var))
      (let ((declare-relative-uri-header! (variable-ref var)))
        (declare-relative-uri-header! "Location")))))

(define (resolve-uri-reference ref base)
  "Resolve the URI reference REF, interpreted relative to the BASE URI, into a
target URI, according to the algorithm specified in RFC 3986 section 5.2.2.
Return the resulting target URI."

  (define (merge-paths base-path rel-path)
    (let* ((base-components (string-split base-path #\/))
           (base-directory-components (match base-components
                                        ((components ... last) components)
                                        (() '())))
           (base-directory (string-join base-directory-components "/")))
      (string-append base-directory "/" rel-path)))

  (define (remove-dot-segments path)
    (let loop ((in
                ;; Drop leading "." and ".." components from a relative path.
                ;; (absolute paths will start with a "" component)
                (drop-while (match-lambda
                              ((or "." "..") #t)
                              (_ #f))
                            (string-split path #\/)))
               (out '()))
      (match in
        (("." . rest)
         (loop rest out))
        ((".." . rest)
         (match out
           ((or () (""))
            (error "remove-dot-segments: too many '..' components" path))
           (_
            (loop rest (cdr out)))))
        ((component . rest)
         (loop rest (cons component out)))
        (()
         (string-join (reverse out) "/")))))

  (cond ((or (uri-scheme ref)
             (uri-host   ref))
         (build-uri (or (uri-scheme ref)
                        (uri-scheme base))
                    #:userinfo (uri-userinfo ref)
                    #:host     (uri-host     ref)
                    #:port     (uri-port     ref)
                    #:path     (remove-dot-segments (uri-path ref))
                    #:query    (uri-query    ref)
                    #:fragment (uri-fragment ref)))
        ((string-null? (uri-path ref))
         (build-uri (uri-scheme base)
                    #:userinfo (uri-userinfo base)
                    #:host     (uri-host     base)
                    #:port     (uri-port     base)
                    #:path     (remove-dot-segments (uri-path base))
                    #:query    (or (uri-query ref)
                                   (uri-query base))
                    #:fragment (uri-fragment ref)))
        (else
         (build-uri (uri-scheme base)
                    #:userinfo (uri-userinfo base)
                    #:host     (uri-host     base)
                    #:port     (uri-port     base)
                    #:path     (remove-dot-segments
                                (if (string-prefix? "/" (uri-path ref))
                                    (uri-path ref)
                                    (merge-paths (uri-path base)
                                                 (uri-path ref))))
                    #:query    (uri-query    ref)
                    #:fragment (uri-fragment ref)))))

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
                              #:buffer-size %http-receive-buffer-size
                              #:progress (progress-proc (uri-abbreviation uri)
                                                        size))
                   (newline))
                 (put-bytevector p bv-or-port))))
         file))
      ((301                                       ; moved permanently
        302)                                      ; found (redirection)
       (let ((uri (resolve-uri-reference (response-location resp) uri)))
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

(define (uri-vicinity dir file)
  "Concatenate DIR, slash, and FILE, keeping only one slash in between.
This is required by some HTTP servers."
  (string-append (string-trim-right dir #\/) "/"
                 (string-trim file #\/)))

(define (maybe-expand-mirrors uri mirrors)
  "If URI uses the 'mirror' scheme, expand it according to the MIRRORS alist.
Return a list of URIs."
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

(define* (url-fetch url file #:key (mirrors '()))
  "Fetch FILE from URL; URL may be either a single string, or a list of
string denoting alternate URLs for FILE.  Return #f on failure, and FILE
on success."
  (define uri
    (append-map (cut maybe-expand-mirrors <> mirrors)
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

;;; Local Variables:
;;; eval: (put 'with-elapsed-time 'scheme-indent-function 1)
;;; End:

;;; download.scm ends here
