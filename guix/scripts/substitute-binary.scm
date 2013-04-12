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
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (web response)
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

(define (fetch uri)
  "Return a binary input port to URI and the number of bytes it's expected to
provide."
  (case (uri-scheme uri)
    ((file)
     (let ((port (open-input-file (uri-path uri))))
       (values port (stat:size (stat port)))))
    ((http)
     (let*-values (((resp port)
                    ;; XXX: `http-get*' was introduced in 2.0.7, and deprecated
                    ;; in 2.0.8 (!).  Assume it is available here.
                    (if (version>? "2.0.7" (version))
                        (http-get* uri #:decode-body? #f)
                        (http-get uri #:streaming? #t)))
                   ((code)
                    (response-code resp))
                   ((size)
                    (response-content-length resp)))
       (case code
         ((200)                                   ; OK
          (values port size))
         ((301                                    ; moved permanently
           302)                                   ; found (redirection)
          (let ((uri (response-location resp)))
            (format #t "following redirection to `~a'...~%"
                    (uri->string uri))
            (fetch uri)))
         (else
          (error "download failed" (uri->string uri)
                 code (response-reason-phrase resp))))))))

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

(define (fetch-narinfo cache path)
  "Return the <narinfo> record for PATH, or #f if CACHE does not hold PATH."
  (define (download url)
    ;; Download the `nix-cache-info' from URL, and return its contents as an
    ;; list of key/value pairs.
    (and=> (false-if-exception (fetch (string->uri url)))
           fields->alist))

  (and=> (download (string-append (cache-url cache) "/"
                                  (store-path-hash-part path)
                                  ".narinfo"))
         (lambda (properties)
           (alist->record properties (narinfo-maker (cache-url cache))
                          '("StorePath" "URL" "Compression"
                            "FileHash" "FileSize" "NarHash" "NarSize"
                            "References" "Deriver" "System")))))

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
  (match args
    (("--query")
     (let ((cache (open-cache %cache-url)))
       (let loop ((command (read-line)))
         (or (eof-object? command)
             (begin
               (match (string-tokenize command)
                 (("have" paths ..1)
                  ;; Return the subset of PATHS available in CACHE.
                  (let ((substitutable
                         (if cache
                             (par-map (cut fetch-narinfo cache <>)
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
                             (par-map (cut fetch-narinfo cache <>)
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
     (let* ((cache   (open-cache %cache-url))
            (narinfo (fetch-narinfo cache store-path))
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
