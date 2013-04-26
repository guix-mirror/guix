;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts substitute-binary)
  #:use-module (guix ui)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix config)
  #:use-module (guix nar)
  #:use-module ((guix build utils) #:select (mkdir-p))
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (web uri)
  #:use-module (guix web)
  #:export (guix-substitute-binary))

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
  ;; A local cache of narinfos, to avoid going to the network.
  (or (and=> (getenv "XDG_CACHE_HOME")
             (cut string-append <> "/guix/substitute-binary"))
      (string-append %state-directory "/substitute-binary/cache")))

(define %narinfo-ttl
  ;; Number of seconds during which cached narinfo lookups are considered
  ;; valid.
  (* 24 3600))

(define %narinfo-negative-ttl
  ;; Likewise, but for negative lookups---i.e., cached lookup failures.
  (* 3 3600))

(define %narinfo-expired-cache-entry-removal-delay
  ;; How often we want to remove files corresponding to expired cache entries.
  (* 7 24 3600))

(define (with-atomic-file-output file proc)
  "Call PROC with an output port for the file that is going to replace FILE.
Upon success, FILE is atomically replaced by what has been written to the
output port, and PROC's result is returned."
  (let* ((template (string-append file ".XXXXXX"))
         (out      (mkstemp! template)))
    (with-throw-handler #t
      (lambda ()
        (let ((result (proc out)))
          (close out)
          (rename-file template file)
          result))
      (lambda (key . args)
        (false-if-exception (delete-file template))))))

(define (fields->alist port)
  "Read recutils-style record from PORT and return them as a list of key/value
pairs."
  (define field-rx
    (make-regexp "^([[:graph:]]+): (.*)$"))

  (let loop ((line   (read-line port))
             (result '()))
    (cond ((eof-object? line)
           (reverse result))
          ((regexp-exec field-rx line)
           =>
           (lambda (match)
             (loop (read-line port)
                   (alist-cons (match:substring match 1)
                               (match:substring match 2)
                               result))))
          (else
           (error "unmatched line" line)))))

(define (alist->record alist make keys)
  "Apply MAKE to the values associated with KEYS in ALIST."
  (let ((args (map (cut assoc-ref alist <>) keys)))
    (apply make args)))

(define (object->fields object fields port)
  "Write OBJECT (typically a record) as a series of recutils-style fields to
PORT, according to FIELDS.  FIELDS must be a list of field name/getter pairs."
  (let loop ((fields fields))
    (match fields
      (()
       object)
      (((field . get) rest ...)
       (format port "~a: ~a~%" field (get object))
       (loop rest)))))

(define (fetch uri)
  "Return a binary input port to URI and the number of bytes it's expected to
provide."
  (case (uri-scheme uri)
    ((file)
     (let ((port (open-input-file (uri-path uri))))
       (values port (stat:size (stat port)))))
    ((http)
     (http-fetch uri #:text? #f))))

(define-record-type <cache>
  (%make-cache url store-directory wants-mass-query?)
  cache?
  (url               cache-url)
  (store-directory   cache-store-directory)
  (wants-mass-query? cache-wants-mass-query?))

(define (open-cache url)
  "Open the binary cache at URL.  Return a <cache> object on success, or #f on
failure."
  (define (download-cache-info url)
    ;; Download the `nix-cache-info' from URL, and return its contents as an
    ;; list of key/value pairs.
    (and=> (false-if-exception (fetch (string->uri url)))
           fields->alist))

  (and=> (download-cache-info (string-append url "/nix-cache-info"))
         (lambda (properties)
           (alist->record properties
                          (cut %make-cache url <...>)
                          '("StoreDir" "WantMassQuery")))))

(define-record-type <narinfo>
  (%make-narinfo path uri compression file-hash file-size nar-hash nar-size
                 references deriver system)
  narinfo?
  (path         narinfo-path)
  (uri          narinfo-uri)
  (compression  narinfo-compression)
  (file-hash    narinfo-file-hash)
  (file-size    narinfo-file-size)
  (nar-hash     narinfo-hash)
  (nar-size     narinfo-size)
  (references   narinfo-references)
  (deriver      narinfo-deriver)
  (system       narinfo-system))

(define (narinfo-maker cache-url)
  "Return a narinfo constructor for narinfos originating from CACHE-URL."
  (lambda (path url compression file-hash file-size nar-hash nar-size
                references deriver system)
    "Return a new <narinfo> object."
    (%make-narinfo path

                   ;; Handle the case where URL is a relative URL.
                   (or (string->uri url)
                       (string->uri (string-append cache-url "/" url)))

                   compression file-hash
                   (and=> file-size string->number)
                   nar-hash
                   (and=> nar-size string->number)
                   (string-tokenize references)
                   (match deriver
                     ((or #f "") #f)
                     (_ deriver))
                   system)))

(define* (read-narinfo port #:optional url)
  "Read a narinfo from PORT in its standard external form.  If URL is true, it
must be a string used to build full URIs from relative URIs found while
reading PORT."
  (alist->record (fields->alist port)
                 (narinfo-maker url)
                 '("StorePath" "URL" "Compression"
                   "FileHash" "FileSize" "NarHash" "NarSize"
                   "References" "Deriver" "System")))

(define (write-narinfo narinfo port)
  "Write NARINFO to PORT."
  (define (empty-string-if-false x)
    (or x ""))

  (define (number-or-empty-string x)
    (if (number? x)
        (number->string x)
        ""))

  (object->fields narinfo
                  `(("StorePath" . ,narinfo-path)
                    ("URL" . ,(compose uri->string narinfo-uri))
                    ("Compression" . ,narinfo-compression)
                    ("FileHash" . ,(compose empty-string-if-false
                                            narinfo-file-hash))
                    ("FileSize" . ,(compose number-or-empty-string
                                            narinfo-file-size))
                    ("NarHash" . ,(compose empty-string-if-false
                                           narinfo-hash))
                    ("NarSize" . ,(compose number-or-empty-string
                                           narinfo-size))
                    ("References" . ,(compose string-join narinfo-references))
                    ("Deriver" . ,(compose empty-string-if-false
                                           narinfo-deriver))
                    ("System" . ,narinfo-system))
                  port))

(define (narinfo->string narinfo)
  "Return the external representation of NARINFO."
  (call-with-output-string (cut write-narinfo narinfo <>)))

(define (string->narinfo str)
  "Return the narinfo represented by STR."
  (call-with-input-string str (cut read-narinfo <>)))

(define (fetch-narinfo cache path)
  "Return the <narinfo> record for PATH, or #f if CACHE does not hold PATH."
  (define (download url)
    ;; Download the `nix-cache-info' from URL, and return its contents as an
    ;; list of key/value pairs.
    (false-if-exception (fetch (string->uri url))))

  (and (string=? (cache-store-directory cache) (%store-prefix))
       (and=> (download (string-append (cache-url cache) "/"
                                       (store-path-hash-part path)
                                       ".narinfo"))
              (cute read-narinfo <> (cache-url cache)))))

(define (obsolete? date now ttl)
  "Return #t if DATE is obsolete compared to NOW + TTL seconds."
  (time>? (subtract-duration now (make-time time-duration 0 ttl))
          (make-time time-monotonic 0 date)))

(define (lookup-narinfo cache path)
  "Check locally if we have valid info about PATH, otherwise go to CACHE and
check what it has."
  (define now
    (current-time time-monotonic))

  (define cache-file
    (string-append %narinfo-cache-directory "/"
                   (store-path-hash-part path)))

  (define (cache-entry narinfo)
    `(narinfo (version 0)
              (date ,(time-second now))
              (value ,(and=> narinfo narinfo->string))))

  (let*-values (((valid? cached)
                 (catch 'system-error
                   (lambda ()
                     (call-with-input-file cache-file
                       (lambda (p)
                         (match (read p)
                           (('narinfo ('version 0) ('date date)
                                      ('value #f))
                            ;; A cached negative lookup.
                            (if (obsolete? date now %narinfo-negative-ttl)
                                (values #f #f)
                                (values #t #f)))
                           (('narinfo ('version 0) ('date date)
                                      ('value value))
                            ;; A cached positive lookup
                            (if (obsolete? date now %narinfo-ttl)
                                (values #f #f)
                                (values #t (string->narinfo value))))))))
                   (lambda _
                     (values #f #f)))))
    (if valid?
        cached                                    ; including negative caches
        (let ((narinfo (and=> (force cache)
                              (cut fetch-narinfo <> path))))
          (with-atomic-file-output cache-file
            (lambda (out)
              (write (cache-entry narinfo) out)))
          narinfo))))

(define (remove-expired-cached-narinfos)
  "Remove expired narinfo entries from the cache.  The sole purpose of this
function is to make sure `%narinfo-cache-directory' doesn't grow
indefinitely."
  (define now
    (current-time time-monotonic))

  (define (expired? file)
    (catch 'system-error
      (lambda ()
        (call-with-input-file file
          (lambda (port)
            (match (read port)
              (('narinfo ('version 0) ('date date)
                         ('value #f))
               (obsolete? date now %narinfo-negative-ttl))
              (('narinfo ('version 0) ('date date)
                         ('value _))
               (obsolete? date now %narinfo-ttl))
              (_ #t)))))
      (lambda args
        ;; FILE may have been deleted.
        #t)))

  (for-each (lambda (file)
              (let ((file (string-append %narinfo-cache-directory
                                         "/" file)))
                (when (expired? file)
                  ;; Wrap in `false-if-exception' because FILE might have been
                  ;; deleted in the meantime (TOCTTOU).
                  (false-if-exception (delete-file file)))))
            (scandir %narinfo-cache-directory
                     (lambda (file)
                       (= (string-length file) 32)))))

(define (maybe-remove-expired-cached-narinfo)
  "Remove expired narinfo entries from the cache if deemed necessary."
  (define now
    (current-time time-monotonic))

  (define expiry-file
    (string-append %narinfo-cache-directory "/last-expiry-cleanup"))

  (define last-expiry-date
    (or (false-if-exception
         (call-with-input-file expiry-file read))
        0))

  (when (obsolete? last-expiry-date now %narinfo-expired-cache-entry-removal-delay)
    (remove-expired-cached-narinfos)
    (call-with-output-file expiry-file
      (cute write (time-second now) <>))))

(define (filtered-port command input)
  "Return an input port (and PID) where data drained from INPUT is filtered
through COMMAND.  INPUT must be a file input port."
  (let ((i+o (pipe)))
    (match (primitive-fork)
      (0
       (close-port (car i+o))
       (close-port (current-input-port))
       (dup2 (fileno input) 0)
       (close-port (current-output-port))
       (dup2 (fileno (cdr i+o)) 1)
       (apply execl (car command) command))
      (child
       (close-port (cdr i+o))
       (values (car i+o) child)))))

(define (decompressed-port compression input)
  "Return an input port where INPUT is decompressed according to COMPRESSION."
  (match compression
    ("none"  (values input #f))
    ("bzip2" (filtered-port `(,%bzip2 "-dc") input))
    ("xz"    (filtered-port `(,%xz "-dc") input))
    ("gzip"  (filtered-port `(,%gzip "-dc") input))
    (else    (error "unsupported compression scheme" compression))))

(define %cache-url
  (or (getenv "GUIX_BINARY_SUBSTITUTE_URL")
      "http://hydra.gnu.org"))


;;;
;;; Entry point.
;;;

(define (guix-substitute-binary . args)
  "Implement the build daemon's substituter protocol."
  (mkdir-p %narinfo-cache-directory)
  (maybe-remove-expired-cached-narinfo)
  (match args
    (("--query")
     (let ((cache (delay (open-cache %cache-url))))
       (let loop ((command (read-line)))
         (or (eof-object? command)
             (begin
               (match (string-tokenize command)
                 (("have" paths ..1)
                  ;; Return the subset of PATHS available in CACHE.
                  (let ((substitutable
                         (if cache
                             (par-map (cut lookup-narinfo cache <>)
                                      paths)
                             '())))
                    (for-each (lambda (narinfo)
                                (when narinfo
                                  (format #t "~a~%" (narinfo-path narinfo))))
                              (filter narinfo? substitutable))
                    (newline)))
                 (("info" paths ..1)
                  ;; Reply info about PATHS if it's in CACHE.
                  (let ((substitutable
                         (if cache
                             (par-map (cut lookup-narinfo cache <>)
                                      paths)
                             '())))
                    (for-each (lambda (narinfo)
                                (format #t "~a\n~a\n~a\n"
                                        (narinfo-path narinfo)
                                        (or (and=> (narinfo-deriver narinfo)
                                                   (cute string-append
                                                         (%store-prefix) "/"
                                                         <>))
                                            "")
                                        (length (narinfo-references narinfo)))
                                (for-each (cute format #t "~a/~a~%"
                                                (%store-prefix) <>)
                                          (narinfo-references narinfo))
                                (format #t "~a\n~a\n"
                                        (or (narinfo-file-size narinfo) 0)
                                        (or (narinfo-size narinfo) 0)))
                              (filter narinfo? substitutable))
                    (newline)))
                 (wtf
                  (error "unknown `--query' command" wtf)))
               (loop (read-line)))))))
    (("--substitute" store-path destination)
     ;; Download STORE-PATH and add store it as a Nar in file DESTINATION.
     (let* ((cache   (delay (open-cache %cache-url)))
            (narinfo (lookup-narinfo cache store-path))
            (uri     (narinfo-uri narinfo)))
       ;; Tell the daemon what the expected hash of the Nar itself is.
       (format #t "~a~%" (narinfo-hash narinfo))

       (let*-values (((raw download-size)
                      (fetch uri))
                     ((input pid)
                      (decompressed-port (narinfo-compression narinfo)
                                         raw)))
         ;; Note that Hydra currently generates Nars on the fly and doesn't
         ;; specify a Content-Length, so DOWNLOAD-SIZE is #f in practice.
         (format (current-error-port)
                 (_ "downloading `~a' from `~a'~:[~*~; (~,1f KiB)~]...~%")
                 store-path (uri->string uri)
                 download-size
                 (and=> download-size (cut / <> 1024.0)))

         ;; Unpack the Nar at INPUT into DESTINATION.
         (restore-file input destination)
         (or (not pid) (zero? (cdr (waitpid pid)))))))
    (("--version")
     (show-version-and-exit "guix substitute-binary"))))

;;; substitute-binary.scm ends here
