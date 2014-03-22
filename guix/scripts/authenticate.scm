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

(define read-canonical-sexp
  ;; Read a gcrypt sexp from a port and return it.
  (compose string->canonical-sexp get-string-all))

(define (read-hash-data port key-type)
  "Read sha256 hash data from PORT and return it as a gcrypt sexp.  KEY-TYPE
is a symbol representing the type of public key algo being used."
  (let* ((hex (get-string-all port))
         (bv  (base16-string->bytevector (string-trim-both hex))))
    (bytevector->hash-data bv #:key-type key-type)))

(define (sign-with-key key-file port)
  "Sign the hash read from PORT with KEY-FILE, and write an sexp that includes
both the hash and the actual signature."
  (let* ((secret-key (call-with-input-file key-file read-canonical-sexp))
         (public-key (if (string-suffix? ".sec" key-file)
                         (call-with-input-file
                             (string-append (string-drop-right key-file 4)
                                            ".pub")
                           read-canonical-sexp)
                         (leave
                          (_ "cannot find public key for secret key '~a'~%")
                          key-file)))
         (data       (read-hash-data port (key-type public-key)))
         (signature  (signature-sexp data secret-key public-key)))
    (display (canonical-sexp->string signature))
    #t))

(define (validate-signature port)
  "Read the signature from PORT (which is as produced above), check whether
its public key is authorized, verify the signature, and print the signed data
to stdout upon success."
  (let* ((signature (read-canonical-sexp port))
         (subject   (signature-subject signature))
         (data      (signature-signed-data signature)))
    (if (and data subject)
        (if (authorized-key? subject)
            (if (valid-signature? signature)
                (let ((hash (hash-data->bytevector data)))
                  (display (bytevector->base16-string hash))
                  #t)                              ; success
                (leave (_ "error: invalid signature: ~a~%")
                       (canonical-sexp->string signature)))
            (leave (_ "error: unauthorized public key: ~a~%")
                   (canonical-sexp->string subject)))
        (leave (_ "error: corrupt signature data: ~a~%")
               (canonical-sexp->string signature)))))

;;;
;;; Entry point with 'openssl'-compatible interface.  We support this
;;; interface because that's what the daemon expects, and we want to leave it
;;; unmodified currently.
;;;

(define (guix-authenticate . args)
  (match args
    ;; As invoked by guix-daemon.
    (("rsautl" "-sign" "-inkey" key "-in" hash-file)
     (call-with-input-file hash-file
       (lambda (port)
         (sign-with-key key port))))
    ;; As invoked by Nix/Crypto.pm (used by Hydra.)
    (("rsautl" "-sign" "-inkey" key)
     (sign-with-key key (current-input-port)))
    ;; As invoked by guix-daemon.
    (("rsautl" "-verify" "-inkey" _ "-pubin" "-in" signature-file)
     (call-with-input-file signature-file
       (lambda (port)
         (validate-signature port))))
    ;; As invoked by Nix/Crypto.pm (used by Hydra.)
    (("rsautl" "-verify" "-inkey" _ "-pubin")
     (validate-signature (current-input-port)))
    (("--help")
     (display (_ "Usage: guix authenticate OPTION...
Sign or verify the signature on the given file.  This tool is meant to
be used internally by 'guix-daemon'.\n")))
    (("--version")
     (show-version-and-exit "guix authenticate"))
    (else
     (leave (_ "wrong arguments")))))

;;; authenticate.scm ends here
