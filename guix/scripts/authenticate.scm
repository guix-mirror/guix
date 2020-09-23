;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2020 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix scripts)
  #:use-module (guix base16)
  #:use-module (gcrypt pk-crypto)
  #:use-module (guix pki)
  #:use-module (guix ui)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 rdelim)
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
  (compose string->canonical-sexp read-string))

(define (sign-with-key key-file sha256)
  "Sign the hash SHA256 (a bytevector) with KEY-FILE, and write an sexp that
includes both the hash and the actual signature."
  (let* ((secret-key (call-with-input-file key-file read-canonical-sexp))
         (public-key (if (string-suffix? ".sec" key-file)
                         (call-with-input-file
                             (string-append (string-drop-right key-file 4)
                                            ".pub")
                           read-canonical-sexp)
                         (leave
                          (G_ "cannot find public key for secret key '~a'~%")
                          key-file)))
         (data       (bytevector->hash-data sha256
                                            #:key-type (key-type public-key)))
         (signature  (signature-sexp data secret-key public-key)))
    (display (canonical-sexp->string signature))
    #t))

(define (validate-signature signature)
  "Validate SIGNATURE, a canonical sexp.  Check whether its public key is
authorized, verify the signature, and print the signed data to stdout upon
success."
  (let* ((subject (signature-subject signature))
         (data    (signature-signed-data signature)))
    (if (and data subject)
        (if (authorized-key? subject)
            (if (valid-signature? signature)
                (let ((hash (hash-data->bytevector data)))
                  (display (bytevector->base16-string hash))
                  #t)                              ; success
                (leave (G_ "error: invalid signature: ~a~%")
                       (canonical-sexp->string signature)))
            (leave (G_ "error: unauthorized public key: ~a~%")
                   (canonical-sexp->string subject)))
        (leave (G_ "error: corrupt signature data: ~a~%")
               (canonical-sexp->string signature)))))


;;;
;;; Entry point.
;;;

(define-command (guix-authenticate . args)
  (category internal)
  (synopsis "sign or verify signatures on normalized archives (nars)")

  ;; Signature sexps written to stdout may contain binary data, so force
  ;; ISO-8859-1 encoding so that things are not mangled.  See
  ;; <http://bugs.gnu.org/17312> for details.
  (set-port-encoding! (current-output-port) "ISO-8859-1")
  (set-port-conversion-strategy! (current-output-port) 'error)

  ;; Same goes for input ports.
  (with-fluids ((%default-port-encoding "ISO-8859-1")
                (%default-port-conversion-strategy 'error))
    (match args
      (("sign" key-file hash)
       (sign-with-key key-file (base16-string->bytevector hash)))
      (("verify" signature-file)
       (call-with-input-file signature-file
         (lambda (port)
           (validate-signature (string->canonical-sexp
                                (read-string port))))))

      (("--help")
       (display (G_ "Usage: guix authenticate OPTION...
Sign or verify the signature on the given file.  This tool is meant to
be used internally by 'guix-daemon'.\n")))
      (("--version")
       (show-version-and-exit "guix authenticate"))
      (else
       (leave (G_ "wrong arguments"))))))

;;; authenticate.scm ends here
