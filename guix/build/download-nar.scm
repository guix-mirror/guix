;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix build download-nar)
  #:use-module (guix build download)
  #:use-module (guix build utils)
  #:use-module ((guix serialization) #:hide (dump-port*))
  #:use-module (guix zlib)
  #:use-module (guix progress)
  #:use-module (web uri)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (download-nar))

;;; Commentary:
;;;
;;; Download a normalized archive or "nar", similar to what 'guix substitute'
;;; does.  The intent here is to use substitute servers as content-addressed
;;; mirrors of VCS checkouts.  This is mostly useful for users who have
;;; disabled substitutes.
;;;
;;; Code:

(define (urls-for-item item)
  "Return the fallback nar URL for ITEM--e.g.,
\"/gnu/store/cabbag3…-foo-1.2-checkout\"."
  ;; Here we hard-code nar URLs without checking narinfos.  That's probably OK
  ;; though.  Use berlin.guix.gnu.org instead of its ci.guix.gnu.org front end to
  ;; avoid sending these requests to CDN providers without user consent.
  ;; TODO: Use HTTPS?  The downside is the extra dependency.
  (let ((bases '("http://berlin.guix.gnu.org"))
        (item  (basename item)))
    (append (map (cut string-append <> "/nar/gzip/" item) bases)
            (map (cut string-append <> "/nar/" item) bases))))

(define (restore-gzipped-nar port item size)
  "Restore the gzipped nar read from PORT, of SIZE bytes (compressed), to
ITEM."
  ;; Since PORT is typically a non-file port (for instance because 'http-get'
  ;; returns a delimited port), create a child process so we're back to a file
  ;; port that can be passed to 'call-with-gzip-input-port'.
  (match (pipe)
    ((input . output)
     (match (primitive-fork)
       (0
        (dynamic-wind
          (const #t)
          (lambda ()
            (close-port output)
            (close-port port)
            (catch #t
              (lambda ()
                (call-with-gzip-input-port input
                  (cut restore-file <> item)))
              (lambda (key . args)
                (print-exception (current-error-port)
                                 (stack-ref (make-stack #t) 1)
                                 key args)
                (primitive-exit 1))))
          (lambda ()
            (primitive-exit 0))))
       (child
        (close-port input)
        (dump-port* port output
                    #:reporter (progress-reporter/file item size
                                                       #:abbreviation
                                                       store-path-abbreviation))
        (close-port output)
        (newline)
        (match (waitpid child)
          ((_ . status)
           (unless (zero? status)
             (error "nar decompression failed" status)))))))))

(define (download-nar item)
  "Download and extract the normalized archive for ITEM.  Return #t on
success, #f otherwise."
  ;; Let progress reports go through.
  (setvbuf (current-error-port) 'none)
  (setvbuf (current-output-port) 'none)

  (let loop ((urls (urls-for-item item)))
    (match urls
      ((url rest ...)
       (format #t "Trying content-addressed mirror at ~a...~%"
               (uri-host (string->uri url)))
       (let-values (((port size)
                     (catch #t
                       (lambda ()
                         (http-fetch (string->uri url)))
                       (lambda args
                         (values #f #f)))))
         (if (not port)
             (loop rest)
             (begin
               (if size
                   (format #t "Downloading from ~a (~,2h MiB)...~%" url
                           (/ size (expt 2 20.)))
                   (format #t "Downloading from ~a...~%" url))
               (if (string-contains url "/gzip")
                   (restore-gzipped-nar port item size)
                   (begin
                     ;; FIXME: Add progress report.
                     (restore-file port item)
                     (close-port port)))
               #t))))
      (()
       #f))))
