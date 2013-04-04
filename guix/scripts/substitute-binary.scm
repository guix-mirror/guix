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
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads)
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
  (case (uri-scheme uri)
    ((file)
     (open-input-file (uri-path uri)))
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
          port)
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
  (%make-narinfo path url compression file-hash file-size nar-hash nar-size
                 references deriver system)
  narinfo?
  (path         narinfo-path)
  (url          narinfo-url)
  (compression  narinfo-compression)
  (file-hash    narinfo-file-hash)
  (file-size    narinfo-file-size)
  (nar-hash     narinfo-hash)
  (nar-size     narinfo-size)
  (references   narinfo-references)
  (deriver      narinfo-deriver)
  (system       narinfo-system))

(define (make-narinfo path url compression file-hash file-size nar-hash nar-size
                      references deriver system)
  "Return a new <narinfo> object."
  (%make-narinfo path url compression file-hash
                 (and=> file-size string->number)
                 nar-hash
                 (and=> nar-size string->number)
                 (string-tokenize references)
                 (match deriver
                   ((or #f "") #f)
                   (_ deriver))
                 system))

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
           (alist->record properties make-narinfo
                          '("StorePath" "URL" "Compression"
                            "FileHash" "FileSize" "NarHash" "NarSize"
                            "References" "Deriver" "System")))))

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
                              substitutable)
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
                              substitutable)
                    (newline)))
                 (wtf
                  (error "unknown `--query' command" wtf)))
               (loop (read-line)))))))
    (("--substitute" store-path destination)
     ;; Download PATH and add it to the store.
     ;; TODO: Implement.
     (format (current-error-port) "substitution not implemented yet~%")
     #f)
    (("--version")
     (show-version-and-exit "guix substitute-binary"))))

;;; substitute-binary.scm ends here
