;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (guix scripts hash)
  #:use-module (guix base32)
  #:use-module (guix hash)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (rnrs io ports)
  #:use-module (rnrs files)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:export (guix-hash))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  `((format . ,bytevector->nix-base32-string)))

(define (show-help)
  (display (_ "Usage: guix hash [OPTION] FILE
Return the cryptographic hash of FILE.

Supported formats: 'nix-base32' (default), 'base32', and 'base16'
('hex' and 'hexadecimal' can be used as well).\n"))
  (format #t (_ "
  -f, --format=FMT       write the hash in the given format"))
  (newline)
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specification of the command-line options.
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
                       (leave (_ "unsupported hash format: ~a~%")
                              arg))))

                  (alist-cons 'format fmt-proc
                              (alist-delete 'format result))))

        (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix hash")))))



;;;
;;; Entry point.
;;;

(define (guix-hash . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (args-fold* args %options
                (lambda (opt name arg result)
                  (leave (_ "unrecognized option: ~a~%")
                         name))
                (lambda (arg result)
                  (alist-cons 'argument arg result))
                %default-options))

  (define (eof->null x)
    (if (eof-object? x)
        #vu8()
        x))

  (let* ((opts (parse-options))
         (args (filter-map (match-lambda
                            (('argument . value)
                             value)
                            (_ #f))
                           (reverse opts)))
         (fmt  (assq-ref opts 'format)))

    (match args
      ((file)
       (catch 'system-error
         (lambda ()
           (format #t "~a~%"
                   (call-with-input-file file
                     (compose fmt sha256 eof->null get-bytevector-all))))
         (lambda args
           (leave (_ "~a~%")
                  (strerror (system-error-errno args))))))
      (_
       (leave (_ "wrong number of arguments~%"))))))
