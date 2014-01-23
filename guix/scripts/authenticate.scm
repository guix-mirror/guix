;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix pki)
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
            (public-key (if (string-suffix? ".sec" key)
                            (read-canonical-sexp
                             (string-append (string-drop-right key 4) ".pub"))
                            (leave
                             (_ "cannot find public key for secret key '~a'~%")
                             key)))
            (data       (read-hash-data hash-file))
            (signature  (signature-sexp data secret-key public-key)))
       (display (canonical-sexp->string signature))
       #t))
    (("rsautl" "-verify" "-inkey" _ "-pubin" "-in" signature-file)
     ;; Read the signature as produced above, check whether its public key is
     ;; authorized, and verify the signature, and print the signed data to
     ;; stdout upon success.
     (let* ((signature (read-canonical-sexp signature-file))
            (subject   (signature-subject signature))
            (data      (signature-signed-data signature)))
       (if (and data subject)
           (if (authorized-key? subject)
               (if (valid-signature? signature)
                   (let ((hash (hash-data->bytevector data)))
                     (display (bytevector->base16-string hash))
                     #t)                          ; success
                   (leave (_ "error: invalid signature: ~a~%")
                          (canonical-sexp->string signature)))
               (leave (_ "error: unauthorized public key: ~a~%")
                      (canonical-sexp->string subject)))
           (leave (_ "error: corrupt signature data: ~a~%")
                  (canonical-sexp->string signature)))))
    (("--help")
     (display (_ "Usage: guix authenticate OPTION...
Sign or verify the signature on the given file.  This tool is meant to
be used internally by 'guix-daemon'.\n")))
    (("--version")
     (show-version-and-exit "guix authenticate"))
    (else
     (leave (_ "wrong arguments")))))

;;; authenticate.scm ends here
