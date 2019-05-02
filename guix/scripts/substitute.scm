;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2018 Kyle Meyer <kyle@kyleam.com>
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

(define-module (guix scripts substitute)
  #:use-module (guix ui)
  #:use-module ((guix store) #:hide (close-connection))
  #:use-module (guix utils)
  #:use-module (guix combinators)
  #:use-module (guix config)
  #:use-module (guix records)
  #:use-module ((guix serialization) #:select (restore-file))
  #:use-module (gcrypt hash)
  #:use-module (guix base32)
  #:use-module (guix base64)
  #:use-module (guix cache)
  #:use-module (gcrypt pk-crypto)
  #:use-module (guix pki)
  #:use-module ((guix build utils) #:select (mkdir-p dump-port))
  #:use-module ((guix build download)
                #:select (uri-abbreviation nar-uri-abbreviation
                          (open-connection-for-uri
                           . guix:open-connection-for-uri)
                          close-connection
                          store-path-abbreviation byte-count->string))
  #:use-module (guix progress)
  #:use-module ((guix build syscalls)
                #:select (set-thread-name))
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 vlist)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (web uri)
  #:use-module (web http)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (guix http-client)
  #:export (narinfo-signature->canonical-sexp

            narinfo?
            narinfo-path
            narinfo-uri
            narinfo-uri-base
            narinfo-compression
            narinfo-file-hash
            narinfo-file-size
            narinfo-hash
            narinfo-size
            narinfo-references
            narinfo-deriver
            narinfo-system
            narinfo-signature

            narinfo-hash->sha256

            lookup-narinfos
            lookup-narinfos/diverse
            read-narinfo
            write-narinfo

            substitute-urls
            guix-substitute))

;;; Comment:
;;;
;;; This is the "binary substituter".  It is invoked by the daemon do check
;;; for the existence of available "substitutes" (pre-built binaries), and to
;;; actually use them as a substitute to building things locally.
;;;
;;; If possible, substitute a binary for the requested store path, using a Nix
;;; "binary cache".  This program implements the Nix "substituter" protocol.
;;;
;;; Code:

(cond-expand
  (guile-2.2
   ;; Guile 2.2.2 has a bug whereby 'time-monotonic' objects have seconds and
   ;; nanoseconds swapped (fixed in Guile commit 886ac3e).  Work around it.
   (define time-monotonic time-tai))
  (else #t))

(define %narinfo-cache-directory
  ;; A local cache of narinfos, to avoid going to the network.  Most of the
  ;; time, 'guix substitute' is called by guix-daemon as root and stores its
  ;; cached data in /var/guix/….  However, when invoked from 'guix challenge'
  ;; as a user, it stores its cache in ~/.cache.
  (if (zero? (getuid))
      (or (and=> (getenv "XDG_CACHE_HOME")
                 (cut string-append <> "/guix/substitute"))
          (string-append %state-directory "/substitute/cache"))
      (string-append (cache-directory #:ensure? #f) "/substitute")))

(define %allow-unauthenticated-substitutes?
  ;; Whether to allow unchecked substitutes.  This is useful for testing
  ;; purposes, and should be avoided otherwise.
  (and (and=> (getenv "GUIX_ALLOW_UNAUTHENTICATED_SUBSTITUTES")
              (cut string-ci=? <> "yes"))
       (begin
         (warning (G_ "authentication and authorization of substitutes \
disabled!~%"))
         #t)))

(define %narinfo-ttl
  ;; Number of seconds during which cached narinfo lookups are considered
  ;; valid for substitute servers that do not advertise a TTL via the
  ;; 'Cache-Control' response header.
  (* 36 3600))

(define %narinfo-negative-ttl
  ;; Likewise, but for negative lookups---i.e., cached lookup failures (404).
  (* 3 3600))

(define %narinfo-transient-error-ttl
  ;; Likewise, but for transient errors such as 504 ("Gateway timeout").
  (* 10 60))

(define %narinfo-expired-cache-entry-removal-delay
  ;; How often we want to remove files corresponding to expired cache entries.
  (* 7 24 3600))

(define fields->alist
  ;; The narinfo format is really just like recutils.
  recutils->alist)

(define %fetch-timeout
  ;; Number of seconds after which networking is considered "slow".
  5)

(define %random-state
  (seed->random-state (+ (ash (cdr (gettimeofday)) 32) (getpid))))

(define-syntax-rule (with-timeout duration handler body ...)
  "Run BODY; when DURATION seconds have expired, call HANDLER, and run BODY
again."
  (begin
    (sigaction SIGALRM
      (lambda (signum)
        (sigaction SIGALRM SIG_DFL)
        handler))
    (alarm duration)
    (call-with-values
        (lambda ()
          (let try ()
            (catch 'system-error
              (lambda ()
                body ...)
              (lambda args
                ;; Before Guile v2.0.9-39-gfe51c7b, the SIGALRM triggers EINTR
                ;; because of the bug at
                ;; <http://lists.gnu.org/archive/html/guile-devel/2013-06/msg00050.html>.
                ;; When that happens, try again.  Note: SA_RESTART cannot be
                ;; used because of <http://bugs.gnu.org/14640>.
                (if (= EINTR (system-error-errno args))
                    (begin
                      ;; Wait a little to avoid bursts.
                      (usleep (random 3000000 %random-state))
                      (try))
                    (apply throw args))))))
      (lambda result
        (alarm 0)
        (sigaction SIGALRM SIG_DFL)
        (apply values result)))))

(define* (fetch uri #:key (buffered? #t) (timeout? #t))
  "Return a binary input port to URI and the number of bytes it's expected to
provide."
  (case (uri-scheme uri)
    ((file)
     (let ((port (open-file (uri-path uri)
                            (if buffered? "rb" "r0b"))))
       (values port (stat:size (stat port)))))
    ((http https)
     (guard (c ((http-get-error? c)
                (leave (G_ "download from '~a' failed: ~a, ~s~%")
                       (uri->string (http-get-error-uri c))
                       (http-get-error-code c)
                       (http-get-error-reason c))))
       ;; Test this with:
       ;;   sudo tc qdisc add dev eth0 root netem delay 1500ms
       ;; and then cancel with:
       ;;   sudo tc qdisc del dev eth0 root
       (let ((port #f))
         (with-timeout (if timeout?
                           %fetch-timeout
                           0)
           (begin
             (warning (G_ "while fetching ~a: server is somewhat slow~%")
                      (uri->string uri))
             (warning (G_ "try `--no-substitutes' if the problem persists~%")))
           (begin
             (when (or (not port) (port-closed? port))
               (set! port (guix:open-connection-for-uri
                           uri #:verify-certificate? #f))
               (unless (or buffered? (not (file-port? port)))
                 (setvbuf port 'none)))
             (http-fetch uri #:text? #f #:port port
                         #:verify-certificate? #f))))))
    (else
     (leave (G_ "unsupported substitute URI scheme: ~a~%")
            (uri->string uri)))))

(define-record-type <cache-info>
  (%make-cache-info url store-directory wants-mass-query?)
  cache-info?
  (url               cache-info-url)
  (store-directory   cache-info-store-directory)
  (wants-mass-query? cache-info-wants-mass-query?))

(define (download-cache-info url)
  "Download the information for the cache at URL.  On success, return a
<cache-info> object and a port on which to send further HTTP requests.  On
failure, return #f and #f."
  (define uri
    (string->uri (string-append url "/nix-cache-info")))

  (define (read-cache-info port)
    (alist->record (fields->alist port)
                   (cut %make-cache-info url <...>)
                   '("StoreDir" "WantMassQuery")))

  (catch #t
    (lambda ()
      (case (uri-scheme uri)
        ((file)
         (values (call-with-input-file (uri-path uri)
                   read-cache-info)
                 #f))
        ((http https)
         (let ((port (guix:open-connection-for-uri
                      uri
                      #:verify-certificate? #f
                      #:timeout %fetch-timeout)))
           (guard (c ((http-get-error? c)
                      (warning (G_ "while fetching '~a': ~a (~s)~%")
                               (uri->string (http-get-error-uri c))
                               (http-get-error-code c)
                               (http-get-error-reason c))
                      (close-connection port)
                      (warning (G_ "ignoring substitute server at '~s'~%") url)
                      (values #f #f)))
             (values (read-cache-info (http-fetch uri
                                                  #:verify-certificate? #f
                                                  #:port port
                                                  #:keep-alive? #t))
                     port))))))
    (lambda (key . args)
      (case key
        ((getaddrinfo-error system-error)
         ;; Silently ignore the error: probably due to lack of network access.
         (values #f #f))
        (else
         (apply throw key args))))))


(define-record-type <narinfo>
  (%make-narinfo path uri uri-base compression file-hash file-size nar-hash nar-size
                 references deriver system signature contents)
  narinfo?
  (path         narinfo-path)
  (uri          narinfo-uri)
  (uri-base     narinfo-uri-base)        ; URI of the cache it originates from
  (compression  narinfo-compression)
  (file-hash    narinfo-file-hash)
  (file-size    narinfo-file-size)
  (nar-hash     narinfo-hash)
  (nar-size     narinfo-size)
  (references   narinfo-references)
  (deriver      narinfo-deriver)
  (system       narinfo-system)
  (signature    narinfo-signature)      ; canonical sexp
  ;; The original contents of a narinfo file.  This field is needed because we
  ;; want to preserve the exact textual representation for verification purposes.
  ;; See <https://lists.gnu.org/archive/html/guix-devel/2014-02/msg00340.html>
  ;; for more information.
  (contents     narinfo-contents))

(define (narinfo-hash->sha256 hash)
  "If the string HASH denotes a sha256 hash, return it as a bytevector.
Otherwise return #f."
  (and (string-prefix? "sha256:" hash)
       (nix-base32-string->bytevector (string-drop hash 7))))

(define (narinfo-signature->canonical-sexp str)
  "Return the value of a narinfo's 'Signature' field as a canonical sexp."
  (match (string-split str #\;)
    ((version host-name sig)
     (let ((maybe-number (string->number version)))
       (cond ((not (number? maybe-number))
              (leave (G_ "signature version must be a number: ~s~%")
                     version))
             ;; Currently, there are no other versions.
             ((not (= 1 maybe-number))
              (leave (G_ "unsupported signature version: ~a~%")
                     maybe-number))
             (else
              (let ((signature (utf8->string (base64-decode sig))))
                (catch 'gcry-error
                  (lambda ()
                    (string->canonical-sexp signature))
                  (lambda (key proc err)
                    (leave (G_ "signature is not a valid \
s-expression: ~s~%")
                           signature))))))))
    (x
     (leave (G_ "invalid format of the signature field: ~a~%") x))))

(define (narinfo-maker str cache-url)
  "Return a narinfo constructor for narinfos originating from CACHE-URL.  STR
must contain the original contents of a narinfo file."
  (lambda (path url compression file-hash file-size nar-hash nar-size
                references deriver system signature)
    "Return a new <narinfo> object."
    (%make-narinfo path
                   ;; Handle the case where URL is a relative URL.
                   (or (string->uri url)
                       (string->uri (string-append cache-url "/" url)))
                   cache-url

                   compression file-hash
                   (and=> file-size string->number)
                   nar-hash
                   (and=> nar-size string->number)
                   (string-tokenize references)
                   (match deriver
                     ((or #f "") #f)
                     (_ deriver))
                   system
                   (false-if-exception
                    (and=> signature narinfo-signature->canonical-sexp))
                   str)))

(define* (assert-valid-signature narinfo signature hash
                                 #:optional (acl (current-acl)))
  "Bail out if SIGNATURE, a canonical sexp representing the signature of
NARINFO, doesn't match HASH, a bytevector containing the hash of NARINFO."
  (let ((uri (uri->string (narinfo-uri narinfo))))
    (signature-case (signature hash acl)
      (valid-signature #t)
      (invalid-signature
       (leave (G_ "invalid signature for '~a'~%") uri))
      (hash-mismatch
       (leave (G_ "hash mismatch for '~a'~%") uri))
      (unauthorized-key
       (leave (G_ "'~a' is signed with an unauthorized key~%") uri))
      (corrupt-signature
       (leave (G_ "signature on '~a' is corrupt~%") uri)))))

(define* (read-narinfo port #:optional url
                       #:key size)
  "Read a narinfo from PORT.  If URL is true, it must be a string used to
build full URIs from relative URIs found while reading PORT.  When SIZE is
true, read at most SIZE bytes from PORT; otherwise, read as much as possible.

No authentication and authorization checks are performed here!"
  (let ((str (utf8->string (if size
                               (get-bytevector-n port size)
                               (get-bytevector-all port)))))
    (alist->record (call-with-input-string str fields->alist)
                   (narinfo-maker str url)
                   '("StorePath" "URL" "Compression"
                     "FileHash" "FileSize" "NarHash" "NarSize"
                     "References" "Deriver" "System"
                     "Signature"))))

(define (narinfo-sha256 narinfo)
  "Return the sha256 hash of NARINFO as a bytevector, or #f if NARINFO lacks a
'Signature' field."
  (define %mandatory-fields
    ;; List of fields that must be signed.  If they are not signed, the
    ;; narinfo is considered unsigned.
    '("StorePath" "NarHash" "References"))

  (let ((contents (narinfo-contents narinfo)))
    (match (string-contains contents "Signature:")
      (#f #f)
      (index
       (let* ((above-signature (string-take contents index))
              (signed-fields (match (call-with-input-string above-signature
                                      fields->alist)
                               (((fields . values) ...) fields))))
         (and (every (cut member <> signed-fields) %mandatory-fields)
              (sha256 (string->utf8 above-signature))))))))

(define* (valid-narinfo? narinfo #:optional (acl (current-acl))
                         #:key verbose?)
  "Return #t if NARINFO's signature is not valid."
  (or %allow-unauthenticated-substitutes?
      (let ((hash      (narinfo-sha256 narinfo))
            (signature (narinfo-signature narinfo))
            (uri       (uri->string (narinfo-uri narinfo))))
        (and hash signature
             (signature-case (signature hash acl)
               (valid-signature #t)
               (invalid-signature
                (when verbose?
                  (format (current-error-port)
                          "invalid signature for substitute at '~a'~%"
                          uri))
                #f)
               (hash-mismatch
                (when verbose?
                  (format (current-error-port)
                          "hash mismatch for substitute at '~a'~%"
                          uri))
                #f)
               (unauthorized-key
                (when verbose?
                  (format (current-error-port)
                          "substitute at '~a' is signed by an \
unauthorized party~%"
                          uri))
                #f)
               (corrupt-signature
                (when verbose?
                  (format (current-error-port)
                          "corrupt signature for substitute at '~a'~%"
                          uri))
                #f))))))

(define (write-narinfo narinfo port)
  "Write NARINFO to PORT."
  (put-bytevector port (string->utf8 (narinfo-contents narinfo))))

(define (narinfo->string narinfo)
  "Return the external representation of NARINFO."
  (call-with-output-string (cut write-narinfo narinfo <>)))

(define (string->narinfo str cache-uri)
  "Return the narinfo represented by STR.  Assume CACHE-URI as the base URI of
the cache STR originates form."
  (call-with-input-string str (cut read-narinfo <> cache-uri)))

(define (narinfo-cache-file cache-url path)
  "Return the name of the local file that contains an entry for PATH.  The
entry is stored in a sub-directory specific to CACHE-URL."
  ;; The daemon does not sanitize its input, so PATH could be something like
  ;; "/gnu/store/foo".  Gracefully handle that.
  (match (store-path-hash-part path)
    (#f
     (leave (G_ "'~a' does not name a store item~%") path))
    ((? string? hash-part)
     (string-append %narinfo-cache-directory "/"
                    (bytevector->base32-string (sha256 (string->utf8 cache-url)))
                    "/" hash-part))))

(define (cached-narinfo cache-url path)
  "Check locally if we have valid info about PATH coming from CACHE-URL.
Return two values: a Boolean indicating whether we have valid cached info, and
that info, which may be either #f (when PATH is unavailable) or the narinfo
for PATH."
  (define now
    (current-time time-monotonic))

  (define cache-file
    (narinfo-cache-file cache-url path))

  (catch 'system-error
    (lambda ()
      (call-with-input-file cache-file
        (lambda (p)
          (match (read p)
            (('narinfo ('version 2)
                       ('cache-uri cache-uri)
                       ('date date) ('ttl ttl) ('value #f))
             ;; A cached negative lookup.
             (if (obsolete? date now ttl)
                 (values #f #f)
                 (values #t #f)))
            (('narinfo ('version 2)
                       ('cache-uri cache-uri)
                       ('date date) ('ttl ttl) ('value value))
             ;; A cached positive lookup
             (if (obsolete? date now ttl)
                 (values #f #f)
                 (values #t (string->narinfo value cache-uri))))
            (('narinfo ('version v) _ ...)
             (values #f #f))))))
    (lambda _
      (values #f #f))))

(define (cache-narinfo! cache-url path narinfo ttl)
  "Cache locally NARNIFO for PATH, which originates from CACHE-URL, with the
given TTL (a number of seconds or #f).  NARINFO may be #f, in which case it
indicates that PATH is unavailable at CACHE-URL."
  (define now
    (current-time time-monotonic))

  (define (cache-entry cache-uri narinfo)
    `(narinfo (version 2)
              (cache-uri ,cache-uri)
              (date ,(time-second now))
              (ttl ,(or ttl
                        (if narinfo %narinfo-ttl %narinfo-negative-ttl)))
              (value ,(and=> narinfo narinfo->string))))

  (let ((file (narinfo-cache-file cache-url path)))
    (mkdir-p (dirname file))
    (with-atomic-file-output file
      (lambda (out)
        (write (cache-entry cache-url narinfo) out))))

  narinfo)

(define (narinfo-request cache-url path)
  "Return an HTTP request for the narinfo of PATH at CACHE-URL."
  (let ((url (string-append cache-url "/" (store-path-hash-part path)
                            ".narinfo"))
        (headers '((User-Agent . "GNU Guile"))))
    (build-request (string->uri url) #:method 'GET #:headers headers)))

(define (at-most max-length lst)
  "If LST is shorter than MAX-LENGTH, return it; otherwise return its
MAX-LENGTH first elements."
  (let loop ((len 0)
             (lst lst)
             (result '()))
    (match lst
      (()
       (reverse result))
      ((head . tail)
       (if (>= len max-length)
           (reverse result)
           (loop (+ 1 len) tail (cons head result)))))))

(define* (http-multiple-get base-uri proc seed requests
                            #:key port (verify-certificate? #t))
  "Send all of REQUESTS to the server at BASE-URI.  Call PROC for each
response, passing it the request object, the response, a port from which to
read the response body, and the previous result, starting with SEED, à la
'fold'.  Return the final result.  When PORT is specified, use it as the
initial connection on which HTTP requests are sent."
  (let connect ((port     port)
                (requests requests)
                (result   seed))
    ;; (format (current-error-port) "connecting (~a requests left)..."
    ;;         (length requests))
    (let ((p (or port (guix:open-connection-for-uri
                       base-uri
                       #:verify-certificate?
                       verify-certificate?))))
      ;; For HTTPS, P is not a file port and does not support 'setvbuf'.
      (when (file-port? p)
        (setvbuf p 'block (expt 2 16)))

      ;; Send REQUESTS, up to a certain number, in a row.
      ;; XXX: Do our own caching to work around inefficiencies when
      ;; communicating over TLS: <http://bugs.gnu.org/22966>.
      (let-values (((buffer get) (open-bytevector-output-port)))
        ;; Inherit the HTTP proxying property from P.
        (set-http-proxy-port?! buffer (http-proxy-port? p))

        (for-each (cut write-request <> buffer)
                  (at-most 1000 requests))
        (put-bytevector p (get))
        (force-output p))

      ;; Now start processing responses.
      (let loop ((requests requests)
                 (result   result))
        (match requests
          (()
           (reverse result))
          ((head tail ...)
           (let* ((resp   (read-response p))
                  (body   (response-body-port resp))
                  (result (proc head resp body result)))
             ;; The server can choose to stop responding at any time, in which
             ;; case we have to try again.  Check whether that is the case.
             ;; Note that even upon "Connection: close", we can read from BODY.
             (match (assq 'connection (response-headers resp))
               (('connection 'close)
                (close-connection p)
                (connect #f tail result))         ;try again
               (_
                (loop tail result))))))))))       ;keep going

(define (read-to-eof port)
  "Read from PORT until EOF is reached.  The data are discarded."
  (dump-port port (%make-void-port "w")))

(define (narinfo-from-file file url)
  "Attempt to read a narinfo from FILE, using URL as the cache URL.  Return #f
if file doesn't exist, and the narinfo otherwise."
  (catch 'system-error
    (lambda ()
      (call-with-input-file file
        (cut read-narinfo <> url)))
    (lambda args
      (if (= ENOENT (system-error-errno args))
          #f
          (apply throw args)))))

(define (fetch-narinfos url paths)
  "Retrieve all the narinfos for PATHS from the cache at URL and return them."
  (define update-progress!
    (let ((done 0)
          (total (length paths)))
      (lambda ()
        (display "\r\x1b[K" (current-error-port)) ;erase current line
        (force-output (current-error-port))
        (format (current-error-port)
                (G_ "updating substitutes from '~a'... ~5,1f%")
                url (* 100. (/ done total)))
        (set! done (+ 1 done)))))

  (define hash-part->path
    (let ((mapping (fold (lambda (path result)
                           (vhash-cons (store-path-hash-part path) path
                                       result))
                         vlist-null
                         paths)))
      (lambda (hash)
        (match (vhash-assoc hash mapping)
          (#f #f)
          ((_ . path) path)))))

  (define (handle-narinfo-response request response port result)
    (let* ((code   (response-code response))
           (len    (response-content-length response))
           (cache  (response-cache-control response))
           (ttl    (and cache (assoc-ref cache 'max-age))))
      ;; Make sure to read no more than LEN bytes since subsequent bytes may
      ;; belong to the next response.
      (if (= code 200)                            ; hit
          (let ((narinfo (read-narinfo port url #:size len)))
            (cache-narinfo! url (narinfo-path narinfo) narinfo ttl)
            (update-progress!)
            (cons narinfo result))
          (let* ((path      (uri-path (request-uri request)))
                 (hash-part (basename
                             (string-drop-right path 8)))) ;drop ".narinfo"
            (if len
                (get-bytevector-n port len)
                (read-to-eof port))
            (cache-narinfo! url (hash-part->path hash-part) #f
                            (if (= 404 code)
                                ttl
                                %narinfo-transient-error-ttl))
            (update-progress!)
            result))))

  (define (do-fetch uri port)
    (case (and=> uri uri-scheme)
      ((http https)
       (let ((requests (map (cut narinfo-request url <>) paths)))
         (update-progress!)

         ;; Note: Do not check HTTPS server certificates to avoid depending on
         ;; the X.509 PKI.  We can do it because we authenticate narinfos,
         ;; which provides a much stronger guarantee.
         (let ((result (http-multiple-get uri
                                          handle-narinfo-response '()
                                          requests
                                          #:verify-certificate? #f
                                          #:port port)))
           (close-connection port)
           (newline (current-error-port))
           result)))
      ((file #f)
       (let* ((base  (string-append (uri-path uri) "/"))
              (files (map (compose (cut string-append base <> ".narinfo")
                                   store-path-hash-part)
                          paths)))
         (filter-map (cut narinfo-from-file <> url) files)))
      (else
       (leave (G_ "~s: unsupported server URI scheme~%")
              (if uri (uri-scheme uri) url)))))

  (let-values (((cache-info port)
                (download-cache-info url)))
    (and cache-info
         (if (string=? (cache-info-store-directory cache-info)
                       (%store-prefix))
             (do-fetch (string->uri url) port)    ;reuse PORT
             (begin
               (warning (G_ "'~a' uses different store '~a'; ignoring it~%")
                        url (cache-info-store-directory cache-info))
               (close-connection port)
               #f)))))

(define (lookup-narinfos cache paths)
  "Return the narinfos for PATHS, invoking the server at CACHE when no
information is available locally."
  (let-values (((cached missing)
                (fold2 (lambda (path cached missing)
                         (let-values (((valid? value)
                                       (cached-narinfo cache path)))
                           (if valid?
                               (if value
                                   (values (cons value cached) missing)
                                   (values cached missing))
                               (values cached (cons path missing)))))
                       '()
                       '()
                       paths)))
    (if (null? missing)
        cached
        (let ((missing (fetch-narinfos cache missing)))
          (append cached (or missing '()))))))

(define (equivalent-narinfo? narinfo1 narinfo2)
  "Return true if NARINFO1 and NARINFO2 are equivalent--i.e., if they describe
the same store item.  This ignores unnecessary metadata such as the Nar URL."
  (and (string=? (narinfo-hash narinfo1)
                 (narinfo-hash narinfo2))

       ;; The following is not needed if all we want is to download a valid
       ;; nar, but it's necessary if we want valid narinfo.
       (string=? (narinfo-path narinfo1)
                 (narinfo-path narinfo2))
       (equal? (narinfo-references narinfo1)
               (narinfo-references narinfo2))

       (= (narinfo-size narinfo1)
          (narinfo-size narinfo2))))

(define (lookup-narinfos/diverse caches paths authorized?)
  "Look up narinfos for PATHS on all of CACHES, a list of URLS, in that order.
That is, when a cache lacks an AUTHORIZED? narinfo, look it up in the next
cache, and so on.

Return a list of narinfos for PATHS or a subset thereof.  The returned
narinfos are either AUTHORIZED?, or they claim a hash that matches an
AUTHORIZED? narinfo."
  (define (select-hit result)
    (lambda (path)
      (match (vhash-fold* cons '() path result)
        ((one)
         one)
        ((several ..1)
         (let ((authorized (find authorized? (reverse several))))
           (and authorized
                (find (cut equivalent-narinfo? <> authorized)
                      several)))))))

  (let loop ((caches caches)
             (paths  paths)
             (result vlist-null)                  ;path->narinfo vhash
             (hits   '()))                        ;paths
    (match paths
      (()                                         ;we're done
       ;; Now iterate on all the HITS, and return exactly one match for each
       ;; hit: the first narinfo that is authorized, or that has the same hash
       ;; as an authorized narinfo, in the order of CACHES.
       (filter-map (select-hit result) hits))
      (_
       (match caches
         ((cache rest ...)
          (let* ((narinfos (lookup-narinfos cache paths))
                 (definite (map narinfo-path (filter authorized? narinfos)))
                 (missing  (lset-difference string=? paths definite))) ;XXX: perf
            (loop rest missing
                  (fold vhash-cons result
                        (map narinfo-path narinfos) narinfos)
                  (append definite hits))))
         (()                                      ;that's it
          (filter-map (select-hit result) hits)))))))

(define (lookup-narinfo caches path authorized?)
  "Return the narinfo for PATH in CACHES, or #f when no substitute for PATH
was found."
  (match (lookup-narinfos/diverse caches (list path) authorized?)
    ((answer) answer)
    (_        #f)))

(define (cached-narinfo-expiration-time file)
  "Return the expiration time for FILE, which is a cached narinfo."
  (catch 'system-error
    (lambda ()
      (call-with-input-file file
        (lambda (port)
          (match (read port)
            (('narinfo ('version 2) ('cache-uri uri)
                       ('date date) ('ttl ttl) ('value #f))
             (+ date ttl))
            (('narinfo ('version 2) ('cache-uri uri)
                       ('date date) ('ttl ttl) ('value value))
             (+ date ttl))
            (x
             0)))))
    (lambda args
      ;; FILE may have been deleted.
      0)))

(define (narinfo-cache-directories directory)
  "Return the list of narinfo cache directories (one per cache URL.)"
  (map (cut string-append directory "/" <>)
       (scandir %narinfo-cache-directory
                (lambda (item)
                  (and (not (member item '("." "..")))
                       (file-is-directory?
                        (string-append %narinfo-cache-directory
                                       "/" item)))))))

(define* (cached-narinfo-files #:optional
                               (directory %narinfo-cache-directory))
  "Return the list of cached narinfo files under DIRECTORY."
  (append-map (lambda (directory)
                (map (cut string-append directory "/" <>)
                     (scandir directory
                              (lambda (file)
                                (= (string-length file) 32)))))
              (narinfo-cache-directories directory)))

(define (progress-report-port reporter port)
  "Return a port that continuously reports the bytes read from PORT using
REPORTER, which should be a <progress-reporter> object."
  (match reporter
    (($ <progress-reporter> start report stop)
     (let* ((total 0)
            (read! (lambda (bv start count)
                     (let ((n (match (get-bytevector-n! port bv start count)
                                ((? eof-object?) 0)
                                (x x))))
                       (set! total (+ total n))
                       (report total)
                       n))))
       (start)
       (make-custom-binary-input-port "progress-port-proc"
                                      read! #f #f
                                      (lambda ()
                                        ;; XXX: Kludge!  When used through
                                        ;; 'decompressed-port', this port ends
                                        ;; up being closed twice: once in a
                                        ;; child process early on, and at the
                                        ;; end in the parent process.  Ignore
                                        ;; the early close so we don't output
                                        ;; a spurious "download-succeeded"
                                        ;; trace.
                                        (unless (zero? total)
                                          (stop))
                                        (close-port port)))))))

(define-syntax with-networking
  (syntax-rules ()
    "Catch DNS lookup errors and TLS errors and gracefully exit."
    ;; Note: no attempt is made to catch other networking errors, because DNS
    ;; lookup errors are typically the first one, and because other errors are
    ;; a subset of `system-error', which is harder to filter.
    ((_ exp ...)
     (catch #t
       (lambda () exp ...)
       (match-lambda*
         (('getaddrinfo-error error)
          (leave (G_ "host name lookup error: ~a~%")
                 (gai-strerror error)))
         (('gnutls-error error proc . rest)
          (let ((error->string (module-ref (resolve-interface '(gnutls))
                                           'error->string)))
            (leave (G_ "TLS error in procedure '~a': ~a~%")
                   proc (error->string error))))
         (args
          (apply throw args)))))))


;;;
;;; Help.
;;;

(define (show-help)
  (display (G_ "Usage: guix substitute [OPTION]...
Internal tool to substitute a pre-built binary to a local build.\n"))
  (display (G_ "
      --query            report on the availability of substitutes for the
                         store file names passed on the standard input"))
  (display (G_ "
      --substitute STORE-FILE DESTINATION
                         download STORE-FILE and store it as a Nar in file
                         DESTINATION"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))



;;;
;;; Daemon/substituter protocol.
;;;

(define (display-narinfo-data narinfo)
  "Write to the current output port the contents of NARINFO in the format
expected by the daemon."
  (format #t "~a\n~a\n~a\n"
          (narinfo-path narinfo)
          (or (and=> (narinfo-deriver narinfo)
                     (cute string-append (%store-prefix) "/" <>))
              "")
          (length (narinfo-references narinfo)))
  (for-each (cute format #t "~a/~a~%" (%store-prefix) <>)
            (narinfo-references narinfo))
  (format #t "~a\n~a\n"
          (or (narinfo-file-size narinfo) 0)
          (or (narinfo-size narinfo) 0)))

(define* (process-query command
                        #:key cache-urls acl)
  "Reply to COMMAND, a query as written by the daemon to this process's
standard input.  Use ACL as the access-control list against which to check
authorized substitutes."
  (define (valid? obj)
    (valid-narinfo? obj acl))

  (match (string-tokenize command)
    (("have" paths ..1)
     ;; Return the subset of PATHS available in CACHE-URLS.
     (let ((substitutable (lookup-narinfos/diverse cache-urls paths valid?)))
       (for-each (lambda (narinfo)
                   (format #t "~a~%" (narinfo-path narinfo)))
                 substitutable)
       (newline)))
    (("info" paths ..1)
     ;; Reply info about PATHS if it's in CACHE-URLS.
     (let ((substitutable (lookup-narinfos/diverse cache-urls paths valid?)))
       (for-each display-narinfo-data substitutable)
       (newline)))
    (wtf
     (error "unknown `--query' command" wtf))))

(define* (process-substitution store-item destination
                               #:key cache-urls acl print-build-trace?)
  "Substitute STORE-ITEM (a store file name) from CACHE-URLS, and write it to
DESTINATION as a nar file.  Verify the substitute against ACL."
  (let* ((narinfo (lookup-narinfo cache-urls store-item
                                  (cut valid-narinfo? <> acl)))
         (uri     (and=> narinfo narinfo-uri)))
    (unless uri
      (leave (G_ "no valid substitute for '~a'~%")
             store-item))

    ;; Tell the daemon what the expected hash of the Nar itself is.
    (format #t "~a~%" (narinfo-hash narinfo))

    (unless print-build-trace?
      (format (current-error-port)
              (G_ "Downloading ~a...~%") (uri->string uri)))

    (let*-values (((raw download-size)
                   ;; Note that Hydra currently generates Nars on the fly
                   ;; and doesn't specify a Content-Length, so
                   ;; DOWNLOAD-SIZE is #f in practice.
                   (fetch uri #:buffered? #f #:timeout? #f))
                  ((progress)
                   (let* ((comp     (narinfo-compression narinfo))
                          (dl-size  (or download-size
                                        (and (equal? comp "none")
                                             (narinfo-size narinfo))))
                          (reporter (if print-build-trace?
                                        (progress-reporter/trace
                                         destination
                                         (uri->string uri) dl-size
                                         (current-error-port))
                                        (progress-reporter/file
                                         (uri->string uri) dl-size
                                         (current-error-port)
                                         #:abbreviation nar-uri-abbreviation))))
                     (progress-report-port reporter raw)))
                  ((input pids)
                   ;; NOTE: This 'progress' port of current process will be
                   ;; closed here, while the child process doing the
                   ;; reporting will close it upon exit.
                   (decompressed-port (and=> (narinfo-compression narinfo)
                                             string->symbol)
                                      progress)))
      ;; Unpack the Nar at INPUT into DESTINATION.
      (restore-file input destination)
      (close-port input)

      ;; Wait for the reporter to finish.
      (every (compose zero? cdr waitpid) pids)

      ;; Skip a line after what 'progress-reporter/file' printed, and another
      ;; one to visually separate substitutions.
      (display "\n\n" (current-error-port)))))


;;;
;;; Entry point.
;;;

(define (check-acl-initialized)
  "Warn if the ACL is uninitialized."
  (define (singleton? acl)
    ;; True if ACL contains just the user's public key.
    (and (file-exists? %public-key-file)
         (let ((key (call-with-input-file %public-key-file
                      (compose string->canonical-sexp
                               read-string))))
           (match acl
             ((thing)
              (equal? (canonical-sexp->string thing)
                      (canonical-sexp->string key)))
             (_
              #f)))))

  (let ((acl (acl->public-keys (current-acl))))
    (when (or (null? acl) (singleton? acl))
      (warning (G_ "ACL for archive imports seems to be uninitialized, \
substitutes may be unavailable\n")))))

(define (daemon-options)
  "Return a list of name/value pairs denoting build daemon options."
  (define %not-newline
    (char-set-complement (char-set #\newline)))

  (match (getenv "_NIX_OPTIONS")
    (#f                           ;should not happen when called by the daemon
     '())
    (newline-separated
     ;; Here we get something of the form "OPTION1=VALUE1\nOPTION2=VALUE2\n".
     (filter-map (lambda (option=value)
                   (match (string-index option=value #\=)
                     (#f                          ;invalid option setting
                      #f)
                     (equal-sign
                      (cons (string-take option=value equal-sign)
                            (string-drop option=value (+ 1 equal-sign))))))
                 (string-tokenize newline-separated %not-newline)))))

(define (find-daemon-option option)
  "Return the value of build daemon option OPTION, or #f if it could not be
found."
  (assoc-ref (daemon-options) option))

(define %default-substitute-urls
  (match (and=> (or (find-daemon-option "untrusted-substitute-urls") ;client
                    (find-daemon-option "substitute-urls"))          ;admin
                string-tokenize)
    ((urls ...)
     urls)
    (#f
     ;; This can only happen when this script is not invoked by the
     ;; daemon.
     '("http://ci.guix.gnu.org"))))

(define substitute-urls
  ;; List of substitute URLs.
  (make-parameter %default-substitute-urls))

(define (client-terminal-columns)
  "Return the number of columns in the client's terminal, if it is known, or a
default value."
  (or (and=> (or (find-daemon-option "untrusted-terminal-columns")
                 (find-daemon-option "terminal-columns"))
             (lambda (str)
               (let ((number (string->number str)))
                 (and number (max 20 (- number 1))))))
      80))

(define (validate-uri uri)
  (unless (string->uri uri)
    (leave (G_ "~a: invalid URI~%") uri)))

(define (guix-substitute . args)
  "Implement the build daemon's substituter protocol."
  (define print-build-trace?
    (match (or (find-daemon-option "untrusted-print-extended-build-trace")
               (find-daemon-option "print-extended-build-trace"))
      (#f #f)
      ((= string->number number) (> number 0))
      (_ #f)))

  (mkdir-p %narinfo-cache-directory)
  (maybe-remove-expired-cache-entries %narinfo-cache-directory
                                      cached-narinfo-files
                                      #:entry-expiration
                                      cached-narinfo-expiration-time
                                      #:cleanup-period
                                      %narinfo-expired-cache-entry-removal-delay)
  (check-acl-initialized)

  ;; Starting from commit 22144afa in Nix, we are allowed to bail out directly
  ;; when we know we cannot substitute, but we must emit a newline on stdout
  ;; when everything is alright.
  (when (null? (substitute-urls))
    (exit 0))

  ;; Say hello (see above.)
  (newline)
  (force-output (current-output-port))

  ;; Sanity-check SUBSTITUTE-URLS so we can provide a meaningful error message.
  (for-each validate-uri (substitute-urls))

  ;; Attempt to install the client's locale, mostly so that messages are
  ;; suitably translated.
  (match (or (find-daemon-option "untrusted-locale")
             (find-daemon-option "locale"))
    (#f     #f)
    (locale (false-if-exception (setlocale LC_ALL locale))))

  (catch 'system-error
    (lambda ()
      (set-thread-name "guix substitute"))
    (const #t))                                   ;GNU/Hurd lacks 'prctl'

  (with-networking
   (with-error-handling                           ; for signature errors
     (match args
       (("--query")
        (let ((acl (current-acl)))
          (let loop ((command (read-line)))
            (or (eof-object? command)
                (begin
                  (process-query command
                                 #:cache-urls (substitute-urls)
                                 #:acl acl)
                  (loop (read-line)))))))
       (("--substitute" store-path destination)
        ;; Download STORE-PATH and add store it as a Nar in file DESTINATION.
        ;; Specify the number of columns of the terminal so the progress
        ;; report displays nicely.
        (parameterize ((current-terminal-columns (client-terminal-columns)))
          (process-substitution store-path destination
                                #:cache-urls (substitute-urls)
                                #:acl (current-acl)
                                #:print-build-trace? print-build-trace?)))
       ((or ("-V") ("--version"))
        (show-version-and-exit "guix substitute"))
       (("--help")
        (show-help))
       (opts
        (leave (G_ "~a: unrecognized options~%") opts))))))

;;; Local Variables:
;;; eval: (put 'with-timeout 'scheme-indent-function 1)
;;; End:

;;; substitute.scm ends here
