;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2018 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2020 Christopher Baines <mail@cbaines.net>
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
  #:use-module (guix scripts)
  #:use-module (guix narinfo)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix combinators)
  #:use-module (guix config)
  #:use-module (guix records)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module ((guix serialization) #:select (restore-file dump-file))
  #:autoload   (guix store deduplication) (dump-file/deduplicate)
  #:autoload   (guix scripts discover) (read-substitute-urls)
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
                          store-path-abbreviation byte-count->string))
  #:autoload   (gnutls) (error/invalid-session)
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
  #:export (lookup-narinfos
            lookup-narinfos/diverse

            %allow-unauthenticated-substitutes?
            %error-to-file-descriptor-4?

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

(define (warn-about-missing-authentication)
  (warning (G_ "authentication and authorization of substitutes \
disabled!~%"))
  #t)

(define %allow-unauthenticated-substitutes?
  ;; Whether to allow unchecked substitutes.  This is useful for testing
  ;; purposes, and should be avoided otherwise.
  (make-parameter
   (and=> (getenv "GUIX_ALLOW_UNAUTHENTICATED_SUBSTITUTES")
          (cut string-ci=? <> "yes"))))

(define %narinfo-ttl
  ;; Number of seconds during which cached narinfo lookups are considered
  ;; valid for substitute servers that do not advertise a TTL via the
  ;; 'Cache-Control' response header.
  (* 36 3600))

(define %narinfo-negative-ttl
  ;; Likewise, but for negative lookups---i.e., cached lookup failures (404).
  (* 1 3600))

(define %narinfo-transient-error-ttl
  ;; Likewise, but for transient errors such as 504 ("Gateway timeout").
  (* 10 60))

(define %narinfo-expired-cache-entry-removal-delay
  ;; How often we want to remove files corresponding to expired cache entries.
  (* 7 24 3600))

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

(define* (fetch uri #:key (buffered? #t) (timeout? #t)
                (keep-alive? #f) (port #f))
  "Return a binary input port to URI and the number of bytes it's expected to
provide.

When PORT is true, use it as the underlying I/O port for HTTP transfers; when
PORT is false, open a new connection for URI.  When KEEP-ALIVE? is true, the
connection (typically PORT) is kept open once data has been fetched from URI."
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
       (let ((port port))
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
                           uri #:verify-certificate? #f)))
             (unless (or buffered? (not (file-port? port)))
               (setvbuf port 'none))
             (http-fetch uri #:text? #f #:port port
                         #:keep-alive? keep-alive?
                         #:verify-certificate? #f))))))
    (else
     (leave (G_ "unsupported substitute URI scheme: ~a~%")
            (uri->string uri)))))

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
  "If LST is shorter than MAX-LENGTH, return it and the empty list; otherwise
return its MAX-LENGTH first elements and its tail."
  (let loop ((len 0)
             (lst lst)
             (result '()))
    (match lst
      (()
       (values (reverse result) '()))
      ((head . tail)
       (if (>= len max-length)
           (values (reverse result) lst)
           (loop (+ 1 len) tail (cons head result)))))))

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
      (at-most batch-size requests))

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

        (for-each (cut write-request <> buffer)
                  batch)
        (put-bytevector p (get))
        (force-output p))

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
           (let* ((resp   (read-response p))
                  (body   (response-body-port resp))
                  (result (proc head resp body result)))
             ;; The server can choose to stop responding at any time, in which
             ;; case we have to try again.  Check whether that is the case.
             ;; Note that even upon "Connection: close", we can read from BODY.
             (match (assq 'connection (response-headers resp))
               (('connection 'close)
                (close-port p)
                (connect #f                       ;try again
                         (drop requests (+ 1 processed))
                         result))
               (_
                (loop tail (+ 1 processed) result)))))))))) ;keep going

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

(define %unreachable-hosts
  ;; Set of names of unreachable hosts.
  (make-hash-table))

(define* (open-connection-for-uri/maybe uri
                                        #:key
                                        fresh?
                                        (time %fetch-timeout))
  "Open a connection to URI via 'open-connection-for-uri/cached' and return a
port to it, or, if connection failed, print a warning and return #f.  Pass
#:fresh? to 'open-connection-for-uri/cached'."
  (define host
    (uri-host uri))

  (catch #t
    (lambda ()
      (open-connection-for-uri/cached uri #:timeout time
                                      #:fresh? fresh?))
    (match-lambda*
      (('getaddrinfo-error error)
       (unless (hash-ref %unreachable-hosts host)
         (hash-set! %unreachable-hosts host #t)   ;warn only once
         (warning (G_ "~a: host not found: ~a~%")
                  host (gai-strerror error)))
       #f)
      (('system-error . args)
       (unless (hash-ref %unreachable-hosts host)
         (hash-set! %unreachable-hosts host #t)
         (warning (G_ "~a: connection failed: ~a~%") host
                  (strerror
                   (system-error-errno `(system-error ,@args)))))
       #f)
      (args
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
      (update-progress!)

      ;; Make sure to read no more than LEN bytes since subsequent bytes may
      ;; belong to the next response.
      (if (= code 200)                            ; hit
          (let ((narinfo (read-narinfo port url #:size len)))
            (if (string=? (dirname (narinfo-path narinfo))
                          (%store-prefix))
                (begin
                  (cache-narinfo! url (narinfo-path narinfo) narinfo ttl)
                  (cons narinfo result))
                result))
          (let* ((path      (uri-path (request-uri request)))
                 (hash-part (basename
                             (string-drop-right path 8)))) ;drop ".narinfo"
            (if len
                (get-bytevector-n port len)
                (read-to-eof port))
            (cache-narinfo! url (hash-part->path hash-part) #f
                            (if (or (= 404 code) (= 202 code))
                                ttl
                                %narinfo-transient-error-ttl))
            result))))

  (define (do-fetch uri)
    (case (and=> uri uri-scheme)
      ((http https)
       ;; Note: Do not check HTTPS server certificates to avoid depending
       ;; on the X.509 PKI.  We can do it because we authenticate
       ;; narinfos, which provides a much stronger guarantee.
       (let* ((requests (map (cut narinfo-request url <>) paths))
              (result   (call-with-cached-connection uri
                          (lambda (port)
                            (if port
                                (begin
                                  (update-progress!)
                                  (http-multiple-get uri
                                                     handle-narinfo-response '()
                                                     requests
                                                     #:open-connection
                                                     open-connection-for-uri/cached
                                                     #:verify-certificate? #f
                                                     #:port port))
                                '()))
                          open-connection-for-uri/maybe)))
         (newline (current-error-port))
         result))
      ((file #f)
       (let* ((base  (string-append (uri-path uri) "/"))
              (files (map (compose (cut string-append base <> ".narinfo")
                                   store-path-hash-part)
                          paths)))
         (filter-map (cut narinfo-from-file <> url) files)))
      (else
       (leave (G_ "~s: unsupported server URI scheme~%")
              (if uri (uri-scheme uri) url)))))

  (do-fetch (string->uri url)))

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

  (let-values (((uri compression file-size) (narinfo-best-uri narinfo)))
    (format #t "~a\n~a\n"
            (or file-size 0)
            (or (narinfo-size narinfo) 0))))

(define* (process-query command
                        #:key cache-urls acl)
  "Reply to COMMAND, a query as written by the daemon to this process's
standard input.  Use ACL as the access-control list against which to check
authorized substitutes."
  (define valid?
    (if (%allow-unauthenticated-substitutes?)
        (begin
          (warn-about-missing-authentication)

          (const #t))
        (lambda (obj)
          (valid-narinfo? obj acl))))

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

(define %max-cached-connections
  ;; Maximum number of connections kept in cache by
  ;; 'open-connection-for-uri/cached'.
  16)

(define open-connection-for-uri/cached
  (let ((cache '()))
    (lambda* (uri #:key fresh? timeout verify-certificate?)
      "Return a connection for URI, possibly reusing a cached connection.
When FRESH? is true, delete any cached connections for URI and open a new one.
Return #f if URI's scheme is 'file' or #f.

When true, TIMEOUT is the maximum number of milliseconds to wait for
connection establishment.  When VERIFY-CERTIFICATE? is true, verify HTTPS
server certificates."
      (define host (uri-host uri))
      (define scheme (uri-scheme uri))
      (define key (list host scheme (uri-port uri)))

      (and (not (memq scheme '(file #f)))
           (match (assoc-ref cache key)
             (#f
              ;; Open a new connection to URI and evict old entries from
              ;; CACHE, if any.
              (let-values (((socket)
                            (guix:open-connection-for-uri
                             uri
                             #:verify-certificate? verify-certificate?
                             #:timeout timeout))
                           ((new-cache evicted)
                            (at-most (- %max-cached-connections 1) cache)))
                (for-each (match-lambda
                            ((_ . port)
                             (false-if-exception (close-port port))))
                          evicted)
                (set! cache (alist-cons key socket new-cache))
                socket))
             (socket
              (if (or fresh? (port-closed? socket))
                  (begin
                    (false-if-exception (close-port socket))
                    (set! cache (alist-delete key cache))
                    (open-connection-for-uri/cached uri #:timeout timeout
                                                    #:verify-certificate?
                                                    verify-certificate?))
                  (begin
                    ;; Drain input left from the previous use.
                    (drain-input socket)
                    socket))))))))

(define* (call-with-cached-connection uri proc
                                      #:optional
                                      (open-connection
                                       open-connection-for-uri/cached))
  (let ((port (open-connection uri)))
    (catch #t
      (lambda ()
        (proc port))
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
                (memq key '(bad-response bad-header bad-header-component)))
            (proc (open-connection uri #:fresh? #t))
            (apply throw key args))))))

(define-syntax-rule (with-cached-connection uri port exp ...)
  "Bind PORT with EXP... to a socket connected to URI."
  (call-with-cached-connection uri (lambda (port) exp ...)))

(define* (process-substitution store-item destination
                               #:key cache-urls acl
                               deduplicate? print-build-trace?)
  "Substitute STORE-ITEM (a store file name) from CACHE-URLS, and write it to
DESTINATION as a nar file.  Verify the substitute against ACL, and verify its
hash against what appears in the narinfo.  When DEDUPLICATE? is true, and if
DESTINATION is in the store, deduplicate its files.  Print a status line on
the current output port."
  (define narinfo
    (lookup-narinfo cache-urls store-item
                    (if (%allow-unauthenticated-substitutes?)
                        (const #t)
                        (cut valid-narinfo? <> acl))))

  (define destination-in-store?
    (string-prefix? (string-append (%store-prefix) "/")
                    destination))

  (define (dump-file/deduplicate* . args)
    ;; Make sure deduplication looks at the right store (necessary in test
    ;; environments).
    (apply dump-file/deduplicate
           (append args (list #:store (%store-prefix)))))

  (unless narinfo
    (leave (G_ "no valid substitute for '~a'~%")
           store-item))

  (let-values (((uri compression file-size)
                (narinfo-best-uri narinfo)))
    (unless print-build-trace?
      (format (current-error-port)
              (G_ "Downloading ~a...~%") (uri->string uri)))

    (let*-values (((raw download-size)
                   ;; 'guix publish' without '--cache' doesn't specify a
                   ;; Content-Length, so DOWNLOAD-SIZE is #f in this case.
                   (with-cached-connection uri port
                     (fetch uri #:buffered? #f #:timeout? #f
                            #:port port
                            #:keep-alive? #t)))
                  ((progress)
                   (let* ((dl-size  (or download-size
                                        (and (equal? compression "none")
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
                     ;; Keep RAW open upon completion so we can later reuse
                     ;; the underlying connection.
                     (progress-report-port reporter raw #:close? #f)))
                  ((input pids)
                   ;; NOTE: This 'progress' port of current process will be
                   ;; closed here, while the child process doing the
                   ;; reporting will close it upon exit.
                   (decompressed-port (string->symbol compression)
                                      progress))

                  ;; Compute the actual nar hash as we read it.
                  ((algorithm expected)
                   (narinfo-hash-algorithm+value narinfo))
                  ((hashed get-hash)
                   (open-hash-input-port algorithm input)))
      ;; Unpack the Nar at INPUT into DESTINATION.
      (restore-file hashed destination
                    #:dump-file (if (and destination-in-store?
                                         deduplicate?)
                                    dump-file/deduplicate*
                                    dump-file))
      (close-port hashed)
      (close-port input)

      ;; Wait for the reporter to finish.
      (every (compose zero? cdr waitpid) pids)

      ;; Skip a line after what 'progress-reporter/file' printed, and another
      ;; one to visually separate substitutions.
      (display "\n\n" (current-error-port))

      ;; Check whether we got the data announced in NARINFO.
      (let ((actual (get-hash)))
        (if (bytevector=? actual expected)
            ;; Tell the daemon that we're done.
            (format (current-output-port) "success ~a ~a~%"
                    (narinfo-hash narinfo) (narinfo-size narinfo))
            ;; The actual data has a different hash than that in NARINFO.
            (format (current-output-port) "hash-mismatch ~a ~a ~a~%"
                    (hash-algorithm-name algorithm)
                    (bytevector->nix-base32-string expected)
                    (bytevector->nix-base32-string actual)))))))


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

;; In order to prevent using large number of discovered local substitute
;; servers, limit the local substitute urls list size.
(define %max-substitute-urls 50)

(define* (randomize-substitute-urls urls
                                    #:key
                                    (max %max-substitute-urls))
  "Return a list containing MAX urls from URLS, picked randomly. If URLS list
is shorter than MAX elements, then it is directly returned."
  (define (random-item list)
    (list-ref list (random (length list))))

  (if (<= (length urls) max)
      urls
      (let loop ((res '())
                 (urls urls))
        (if (eq? (length res) max)
            res
            (let ((url (random-item urls)))
              (loop (cons url res) (delete url urls)))))))

(define %local-substitute-urls
  ;; If the following option is passed to the daemon, use the substitutes list
  ;; provided by "guix discover" process.
  (let* ((option (find-daemon-option "discover"))
         (discover? (and option (string=? option "yes"))))
    (if discover?
     (randomize-substitute-urls (read-substitute-urls))
     '())))

(define substitute-urls
  ;; List of substitute URLs.
  (make-parameter (append %local-substitute-urls
                          %default-substitute-urls)))

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

(define %error-to-file-descriptor-4?
  ;; Whether to direct 'current-error-port' to file descriptor 4 like
  ;; 'guix-daemon' expects.
  (make-parameter #t))

(define-command (guix-substitute . args)
  (category internal)
  (synopsis "implement the build daemon's substituter protocol")

  (define print-build-trace?
    (match (or (find-daemon-option "untrusted-print-extended-build-trace")
               (find-daemon-option "print-extended-build-trace"))
      (#f #f)
      ((= string->number number) (> number 0))
      (_ #f)))

  (define deduplicate?
    (find-daemon-option "deduplicate"))

  ;; The daemon's agent code opens file descriptor 4 for us and this is where
  ;; stderr should go.
  (parameterize ((current-error-port (if (%error-to-file-descriptor-4?)
                                         (fdopen 4 "wl")
                                         (current-error-port))))
    ;; Redirect diagnostics to file descriptor 4 as well.
    (guix-warning-port (current-error-port))

    (mkdir-p %narinfo-cache-directory)
    (maybe-remove-expired-cache-entries %narinfo-cache-directory
                                        cached-narinfo-files
                                        #:entry-expiration
                                        cached-narinfo-expiration-time
                                        #:cleanup-period
                                        %narinfo-expired-cache-entry-removal-delay)
    (check-acl-initialized)

    ;; Sanity-check SUBSTITUTE-URLS so we can provide a meaningful error
    ;; message.
    (for-each validate-uri (substitute-urls))

    ;; Attempt to install the client's locale so that messages are suitably
    ;; translated.  LC_CTYPE must be a UTF-8 locale; it's the case by default
    ;; so don't change it.
    (match (or (find-daemon-option "untrusted-locale")
               (find-daemon-option "locale"))
      (#f     #f)
      (locale (false-if-exception (setlocale LC_MESSAGES locale))))

    (catch 'system-error
      (lambda ()
        (set-thread-name "guix substitute"))
      (const #t))                                 ;GNU/Hurd lacks 'prctl'

    (with-networking
     (with-error-handling                         ; for signature errors
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
         (("--substitute")
          ;; Download STORE-PATH and store it as a Nar in file DESTINATION.
          ;; Specify the number of columns of the terminal so the progress
          ;; report displays nicely.
          (parameterize ((current-terminal-columns (client-terminal-columns)))
            (let loop ()
              (match (read-line)
                ((? eof-object?)
                 #t)
                ((= string-tokenize ("substitute" store-path destination))
                 (process-substitution store-path destination
                                       #:cache-urls (substitute-urls)
                                       #:acl (current-acl)
                                       #:deduplicate? deduplicate?
                                       #:print-build-trace?
                                       print-build-trace?)
                 (loop))))))
         ((or ("-V") ("--version"))
          (show-version-and-exit "guix substitute"))
         (("--help")
          (show-help))
         (opts
          (leave (G_ "~a: unrecognized options~%") opts)))))))

;;; Local Variables:
;;; eval: (put 'with-timeout 'scheme-indent-function 1)
;;; eval: (put 'with-cached-connection 'scheme-indent-function 2)
;;; eval: (put 'call-with-cached-connection 'scheme-indent-function 1)
;;; End:

;;; substitute.scm ends here
