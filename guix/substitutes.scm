;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix substitutes)
  #:use-module (guix narinfo)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix combinators)
  #:use-module (guix config)
  #:use-module (guix records)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (gcrypt hash)
  #:use-module (guix base32)
  #:use-module (guix base64)
  #:use-module (guix cache)
  #:use-module (gcrypt pk-crypto)
  #:use-module (guix pki)
  #:use-module ((guix build utils) #:select (mkdir-p dump-port))
  #:use-module ((guix build download)
                #:select ((open-connection-for-uri
                           . guix:open-connection-for-uri)
                          resolve-uri-reference))
  #:use-module (guix progress)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 vlist)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (guix http-client)
  #:export (%narinfo-cache-directory

            call-with-connection-error-handling

            lookup-narinfos
            lookup-narinfos/diverse))

(define %narinfo-ttl
  ;; Number of seconds during which cached narinfo lookups are considered
  ;; valid for substitute servers that do not advertise a TTL via the
  ;; 'Cache-Control' response header.
  (* 36 3600))

(define %narinfo-negative-ttl
  ;; Likewise, but for negative lookups---i.e., cached lookup failures (404).
  (* 10 60))

(define %narinfo-transient-error-ttl
  ;; Likewise, but for transient errors such as 504 ("Gateway timeout").
  (* 5 60))

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

(define %unreachable-hosts
  ;; Set of names of unreachable hosts.
  (make-hash-table))

(define* (call-with-connection-error-handling uri proc)
  "Call PROC, and catch if a connection fails, print a warning and return #f."
  (define host
    (uri-host uri))

  (catch #t
    proc
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

(define (narinfo-request cache-url path)
  "Return an HTTP request for the narinfo of PATH at CACHE-URL."
  ;; Ensure BASE has a trailing slash so that REF is correct regardless of
  ;; whether the user-provided CACHE-URL has a trailing slash.
  (let* ((base (string->uri (if (string-suffix? "/" cache-url)
                                cache-url
                                (string-append cache-url "/"))))
         (ref (build-relative-ref
               #:path (string-append (store-path-hash-part path) ".narinfo")))
         (url (resolve-uri-reference ref base))
         (headers '((User-Agent . "GNU Guile"))))
    (build-request url #:method 'GET #:headers headers)))

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

(define* (fetch-narinfos url paths
                         #:key
                         (open-connection guix:open-connection-for-uri)
                         (make-progress-reporter
                          (const progress-reporter/silent)))
  "Retrieve all the narinfos for PATHS from the cache at URL and return them."
  (define progress-reporter
    (make-progress-reporter (length paths)
                            #:url url))

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

  (define (read-to-eof port)
    "Read from PORT until EOF is reached.  The data are discarded."
    (dump-port port (%make-void-port "w")))

  (define (handle-narinfo-response request response port result)
    (let* ((code   (response-code response))
           (len    (response-content-length response))
           (cache  (response-cache-control response))
           (ttl    (and cache (assoc-ref cache 'max-age))))
      (progress-reporter-report! progress-reporter)

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
              (result   (begin
                          (start-progress-reporter! progress-reporter)
                          (call-with-connection-error-handling
                           uri
                           (lambda ()
                             (http-multiple-get uri
                                                handle-narinfo-response '()
                                                requests
                                                #:open-connection open-connection
                                                #:verify-certificate? #f))))))
         (stop-progress-reporter! progress-reporter)
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

(define* (lookup-narinfos cache paths
                          #:key (open-connection guix:open-connection-for-uri)
                          (make-progress-reporter
                           (const progress-reporter/silent)))
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
    (values (if (null? missing)
                cached
                (let ((missing (fetch-narinfos cache missing
                                               #:open-connection open-connection
                                               #:make-progress-reporter
                                               make-progress-reporter)))
                  (append cached (or missing '()))))
            (length missing))))

(define* (lookup-narinfos/diverse caches paths authorized?
                                  #:key (open-connection
                                         guix:open-connection-for-uri)
                                  (make-progress-reporter
                                   (const progress-reporter/silent)))
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
          (let* ((narinfos (lookup-narinfos cache paths
                                            #:open-connection open-connection
                                            #:make-progress-reporter
                                            make-progress-reporter))
                 (definite (map narinfo-path (filter authorized? narinfos)))
                 (missing  (lset-difference string=? paths definite))) ;XXX: perf
            (loop rest missing
                  (fold vhash-cons result
                        (map narinfo-path narinfos) narinfos)
                  (append definite hits))))
         (()                                      ;that's it
          (filter-map (select-hit result) hits)))))))

;;; substitutes.scm ends here
