;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2014, 2016-2017, 2020-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
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
  #:use-module (gcrypt hash)
  #:use-module (guix serialization)
  #:use-module (guix ui)
  #:use-module (guix hash)
  #:use-module (guix scripts)
  #:use-module (guix base16)
  #:use-module (guix base32)
  #:autoload   (guix base64) (base64-encode)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs files)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:autoload   (disarchive git-hash) (git-hash-file git-hash-directory)
  #:export (guix-hash))


;;;
;;; Serializers
;;;

(define* (nar-hash file #:optional
                   (algorithm (assoc-ref %default-options 'hash-algorithm))
                   select?)
  (file-hash* file #:algorithm algorithm #:select? select? #:recursive? #true))

(define* (default-hash file #:optional
                       (algorithm (assoc-ref %default-options 'hash-algorithm))
                       select?)
  (match file
    ("-" (port-hash algorithm (current-input-port)))
    (_ (file-hash* file #:algorithm algorithm #:recursive? #false))))

(define* (git-hash file #:optional
                       (algorithm (assoc-ref %default-options 'hash-algorithm))
                       select?)
  (define directory?
    (case (stat:type (stat file))
      ((directory) #t)
      (else #f)))
  (if directory?
      (git-hash-directory file algorithm #:select? select?)
      (git-hash-file file algorithm)))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  `((format . ,bytevector->nix-base32-string)
    (hash-algorithm . ,(hash-algorithm sha256))
    (serializer . ,default-hash)))

(define (show-help)
  (display (G_ "Usage: guix hash [OPTION] FILE
Return the cryptographic hash of FILE.\n"))
  (newline)
  (display (G_ "\
Supported formats: 'base64', 'nix-base32' (default), 'base32',
and 'base16' ('hex' and 'hexadecimal' can be used as well).\n"))
  (format #t (G_ "
  -x, --exclude-vcs      exclude version control directories"))
  (format #t (G_ "
  -H, --hash=ALGORITHM   use the given hash ALGORITHM"))
  (format #t (G_ "
  -f, --format=FMT       write the hash in the given format"))
  (format #t (G_ "
  -S, --serializer=TYPE  compute the hash on FILE according to TYPE serialization"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specification of the command-line options.
  (list (option '(#\x "exclude-vcs") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'exclude-vcs? #t result)))
        (option '(#\H "hash") #t #f
                (lambda (opt name arg result)
                  (match (lookup-hash-algorithm (string->symbol arg))
                    (#f
                     (leave (G_ "~a: unknown hash algorithm~%") arg))
                    (algo
                     (alist-cons 'hash-algorithm algo result)))))
        (option '(#\f "format") #t #f
                (lambda (opt name arg result)
                  (define fmt-proc
                    (match arg
                      ("base64"
                       base64-encode)
                      ("nix-base32"
                       bytevector->nix-base32-string)
                      ("base32"
                       bytevector->base32-string)
                      ((or "base16" "hex" "hexadecimal")
                       bytevector->base16-string)
                      (x
                       (leave (G_ "unsupported hash format: ~a~%")
                              arg))))

                  (alist-cons 'format fmt-proc
                              (alist-delete 'format result))))
        (option '(#\r "recursive") #f #f
                (lambda (opt name arg result)
                  (unless (eqv? name #\r)
                    (warning (G_ "'--recursive' is deprecated, \
use '--serializer=nar' instead~%")))
                  (alist-cons 'serializer nar-hash
                              (alist-delete 'serializer result))))
        (option '(#\S "serializer") #t #f
                (lambda (opt name arg result)
                  (define serializer-proc
                    (match arg
                      ("none"
                       default-hash)
                      ("nar"
                       nar-hash)
                      ("git"
                       git-hash)
                      (x
                       (leave (G_ "unsupported serializer type: ~a~%")
                              arg))))

                  (alist-cons 'serializer serializer-proc
                              (alist-delete 'serializer result))))
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

(define-command (guix-hash . args)
  (category packaging)
  (synopsis "compute the cryptographic hash of a file")

  (define (parse-options)
    ;; Return the alist of option values.
    (parse-command-line args %options (list %default-options)
                        #:build-options? #f))

  (let* ((opts (parse-options))
         (args (filter-map (match-lambda
                            (('argument . value)
                             value)
                            (_ #f))
                           (reverse opts)))
         (fmt  (assq-ref opts 'format))
         (select? (if (assq-ref opts 'exclude-vcs?)
                      (negate vcs-file?)
                      (const #t)))
         (algorithm (assoc-ref opts 'hash-algorithm))
         (serializer (assoc-ref opts 'serializer)))

    (define (file-hash file)
      ;; Compute the hash of FILE.
     ;; Catch and gracefully report possible error
      (catch 'system-error
        (lambda _
          (with-error-handling
            (serializer file algorithm select?)))
        (lambda args
          (leave (G_ "~a ~a~%")
                 file
                 (strerror (system-error-errno args))))))

    (define (formatted-hash thing)
      (fmt (file-hash thing)))

    (match args
      (()
       (leave (G_ "no arguments specified~%")))
      (_
       (for-each
        (compose (cute format #t "~a~%" <>) formatted-hash)
        args)))))
