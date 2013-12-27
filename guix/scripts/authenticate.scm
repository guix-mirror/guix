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

(define-module (guix scripts authenticate)
  #:use-module (guix config)
  #:use-module (guix utils)
  #:use-module (guix pk-crypto)
  #:use-module (guix ui)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:export (guix-authenticate))

;;; Commentary:
;;;
;;; This program is used internally by the daemon to sign exported archive
;;; (the 'export-paths' RPC), and to authenticate imported archives (the
;;; 'import-paths' RPC.)
;;;
;;; Code:

(define (read-canonical-sexp file)
  "Read a gcrypt sexp from FILE and return it."
  (call-with-input-file file
    (compose string->canonical-sexp get-string-all)))

(define (read-hash-data file)
  "Read sha256 hash data from FILE and return it as a gcrypt sexp."
  (let* ((hex (call-with-input-file file get-string-all))
         (bv  (base16-string->bytevector (string-trim-both hex))))
    (bytevector->hash-data bv)))


;;;
;;; Entry point with 'openssl'-compatible interface.  We support this
;;; interface because that's what the daemon expects, and we want to leave it
;;; unmodified currently.
;;;

(define (guix-authenticate . args)
  (match args
    (("rsautl" "-sign" "-inkey" key "-in" hash-file)
     ;; Sign the hash in HASH-FILE with KEY, and return an sexp that includes
     ;; both the hash and the actual signature.
     (let* ((secret-key (read-canonical-sexp key))
            (data       (read-hash-data hash-file)))
       (format #t
               "(guix-signature ~a (payload ~a))"
               (canonical-sexp->string (sign data secret-key))
               (canonical-sexp->string data))
       #t))
    (("rsautl" "-verify" "-inkey" key "-pubin" "-in" signature-file)
     ;; Read the signature as produced above, check it against KEY, and print
     ;; the signed data to stdout upon success.
     (let* ((public-key (read-canonical-sexp key))
            (sig+data   (read-canonical-sexp signature-file))
            (data       (find-sexp-token sig+data 'payload))
            (signature  (find-sexp-token sig+data 'sig-val)))
       (if (and data signature)
           (if (verify signature data public-key)
               (begin
                 (display (bytevector->base16-string
                           (hash-data->bytevector data)))
                 #t)                              ; success
               (begin
                 (format (current-error-port)
                         "error: invalid signature: ~a~%"
                         (canonical-sexp->string signature))
                 (exit 1)))
           (begin
             (format (current-error-port)
                     "error: corrupt signature data: ~a~%"
                     (canonical-sexp->string sig+data))
             (exit 1)))))
    (("--help")
     (display (_ "Usage: guix authenticate OPTION...
Sign or verify the signature on the given file.  This tool is meant to
be used internally by 'guix-daemon'.\n")))
    (("--version")
     (show-version-and-exit "guix authenticate"))
    (else
     (leave (_ "wrong arguments")))))

;;; authenticate.scm ends here
