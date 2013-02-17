;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts download)
  #:use-module (guix ui)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix base32)
  #:use-module ((guix download) #:select (%mirrors))
  #:use-module (guix build download)
  #:use-module (web uri)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:export (guix-download))

(define (call-with-temporary-output-file proc)
  (let* ((template (string-copy "guix-download.XXXXXX"))
         (out      (mkstemp! template)))
    (dynamic-wind
      (lambda ()
        #t)
      (lambda ()
        (proc template out))
      (lambda ()
        (false-if-exception (delete-file template))))))

(define (fetch-and-store store fetch name)
  "Call FETCH for URI, and pass it the name of a file to write to; eventually,
copy data from that port to STORE, under NAME.  Return the resulting
store path."
  (call-with-temporary-output-file
   (lambda (temp port)
     (let ((result
            (parameterize ((current-output-port (current-error-port)))
              (fetch temp))))
       (close port)
       (and result
            (add-to-store store name #f "sha256" temp))))))

;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  `((format . ,bytevector->nix-base32-string)))

(define (show-help)
  (display (_ "Usage: guix download [OPTION]... URL
Download the file at URL, add it to the store, and print its store path
and the hash of its contents.\n"))
  (format #t (_ "
  -f, --format=FMT       write the hash in the given format (default: `nix-base32')"))
  (newline)
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specifications of the command-line options.
  (list (option '(#\f "format") #t #f
                (lambda (opt name arg result)
                  (define fmt-proc
                    (match arg
                      ("nix-base32"
                       bytevector->nix-base32-string)
                      ("base32"
                       bytevector->base32-string)
                      ((or "base16" "hex" "hexadecimal")
                       bytevector->base16-string)
                      (x
                       (format (current-error-port)
                               "unsupported hash format: ~a~%" arg))))

                  (alist-cons 'format fmt-proc
                              (alist-delete 'format result))))

        (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix download")))))


;;;
;;; Entry point.
;;;

(define (guix-download . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (args-fold args %options
               (lambda (opt name arg result)
                 (leave (_ "~A: unrecognized option~%") name))
               (lambda (arg result)
                 (alist-cons 'argument arg result))
               %default-options))

  (let* ((opts  (parse-options))
         (store (open-connection))
         (arg   (assq-ref opts 'argument))
         (uri   (or (string->uri arg)
                    (leave (_ "guix-download: ~a: failed to parse URI~%")
                           arg)))
         (path  (case (uri-scheme uri)
                  ((file)
                   (add-to-store store (basename (uri-path uri))
                                 #f "sha256" (uri-path uri)))
                  (else
                   (fetch-and-store store
                                    (cut url-fetch arg <>
                                         #:mirrors %mirrors)
                                    (basename (uri-path uri))))))
         (hash  (call-with-input-file
                    (or path
                        (leave (_ "guix-download: ~a: download failed~%")
                               arg))
                  (compose sha256 get-bytevector-all)))
         (fmt   (assq-ref opts 'format)))
    (format #t "~a~%~a~%" path (fmt hash))
    #t))
