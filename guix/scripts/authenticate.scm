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
  #:use-module (guix diagnostics)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-71)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 iconv)
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

(define (load-key-pair key-file)
  "Load the key pair whose secret key lives at KEY-FILE.  Return a pair of
canonical sexps representing those keys."
  (catch 'system-error
    (lambda ()
      (let* ((secret-key (call-with-input-file key-file read-canonical-sexp))
             (public-key (call-with-input-file
                             (string-append (string-drop-right key-file 4)
                                            ".pub")
                           read-canonical-sexp)))
        (cons public-key secret-key)))
    (lambda args
      (let ((errno (system-error-errno args)))
        (raise
         (formatted-message
          (G_ "failed to load key pair at '~a': ~a~%")
          key-file (strerror errno)))))))

(define (sign-with-key public-key secret-key sha256)
  "Sign the hash SHA256 (a bytevector) with SECRET-KEY (a canonical sexp), and
return the signature as a canonical sexp that includes SHA256, PUBLIC-KEY, and
the actual signature."
  (let ((data (bytevector->hash-data sha256
                                     #:key-type (key-type public-key))))
    (signature-sexp data secret-key public-key)))

(define (validate-signature signature acl)
  "Validate SIGNATURE, a canonical sexp.  Check whether its public key is
authorized in ACL, verify the signature, and return the signed data (a
bytevector) upon success."
  (let* ((subject (signature-subject signature))
         (data    (signature-signed-data signature)))
    (if (and data subject)
        (if (authorized-key? subject acl)
            (if (valid-signature? signature)
                (hash-data->bytevector data)      ; success
                (raise
                 (formatted-message (G_ "invalid signature: ~a")
                                    (canonical-sexp->string signature))))
            (raise
             (formatted-message (G_ "unauthorized public key: ~a")
                                (canonical-sexp->string subject))))
        (raise
         (formatted-message (G_ "corrupt signature data: ~a")
                            (canonical-sexp->string signature))))))

(define (read-command port)
  "Read a command from PORT and return the command and arguments as a list of
strings.  Return the empty list when the end-of-file is reached.

Commands are newline-terminated and must look something like this:

  COMMAND 3:abc 5:abcde 1:x

where COMMAND is an alphanumeric sequence and the remainder is the command
arguments.  Each argument is written as its length (in characters), followed
by colon, followed by the given number of characters."
  (define (consume-whitespace port)
    (let ((chr (lookahead-u8 port)))
      (when (eqv? chr (char->integer #\space))
        (get-u8 port)
        (consume-whitespace port))))

  (match (read-delimited " \t\n\r" port)
    ((? eof-object?)
     '())
    (command
     (let loop ((result (list command)))
       (consume-whitespace port)
       (let ((next (lookahead-u8 port)))
         (cond ((eqv? next (char->integer #\newline))
                (get-u8 port)
                (reverse result))
               ((eof-object? next)
                (reverse result))
               (else
                (let* ((len (string->number (read-delimited ":" port)))
                       (str (bytevector->string
                             (get-bytevector-n port len)
                             "ISO-8859-1" 'error)))
                  (loop (cons str result))))))))))

(define-syntax define-enumerate-type              ;TODO: factorize
  (syntax-rules ()
    ((_ name->int (name id) ...)
     (define-syntax name->int
       (syntax-rules (name ...)
         ((_ name) id) ...)))))

;; Codes used when reply to requests.
(define-enumerate-type reply-code
  (success 0)
  (command-not-found 404)
  (command-failed 500))


;;;
;;; Entry point.
;;;

(define-command (guix-authenticate . args)
  (category internal)
  (synopsis "sign or verify signatures on normalized archives (nars)")

  (define (send-reply code str)
    ;; Send CODE and STR as a reply to our client.
    (let ((bv (string->bytevector str "ISO-8859-1" 'error)))
      (format #t "~a ~a:" code (bytevector-length bv))
      (put-bytevector (current-output-port) bv)
      (force-output (current-output-port))))

  (define (call-with-reply thunk)
    ;; Send a reply for the result of THUNK or for any exception raised during
    ;; its execution.
    (guard (c ((formatted-message? c)
               (send-reply (reply-code command-failed)
                           (apply format #f
                                  (G_ (formatted-message-string c))
                                  (formatted-message-arguments c)))))
      (send-reply (reply-code success) (thunk))))

  (define-syntax-rule (with-reply exp ...)
    (call-with-reply (lambda () exp ...)))

  ;; Signature sexps written to stdout may contain binary data, so force
  ;; ISO-8859-1 encoding so that things are not mangled.  See
  ;; <http://bugs.gnu.org/17312> for details.
  (set-port-encoding! (current-output-port) "ISO-8859-1")
  (set-port-conversion-strategy! (current-output-port) 'error)

  ;; Same goes for input ports.
  (with-fluids ((%default-port-encoding "ISO-8859-1")
                (%default-port-conversion-strategy 'error))
    (match args
      (("--help")
       (display (G_ "Usage: guix authenticate OPTION...
Sign data or verify signatures.  This tool is meant to be used internally by
'guix-daemon'.\n")))
      (("--version")
       (show-version-and-exit "guix authenticate"))
      (()
       (let ((acl (current-acl)))
         (let loop ((key-pairs vlist-null))
           ;; Read a request on standard input and reply.
           (match (read-command (current-input-port))
             (("sign" signing-key (= base16-string->bytevector hash))
              (let* ((key-pairs keys
                                (match (vhash-assoc signing-key key-pairs)
                                  ((_ . keys)
                                   (values key-pairs keys))
                                  (#f
                                   (let ((keys (load-key-pair signing-key)))
                                     (values (vhash-cons signing-key keys
                                                         key-pairs)
                                             keys))))))
                (with-reply (canonical-sexp->string
                             (match keys
                               ((public . secret)
                                (sign-with-key public secret hash)))))
                (loop key-pairs)))
             (("verify" signature)
              (with-reply (bytevector->base16-string
                           (validate-signature
                            (string->canonical-sexp signature)
                            acl)))
              (loop key-pairs))
             (()
              (exit 0))
             (commands
              (warning (G_ "~s: invalid command; ignoring~%") commands)
              (send-reply (reply-code command-not-found)
                          "invalid command")
              (loop key-pairs))))))
      (_
       (leave (G_ "wrong arguments~%"))))))

;;; authenticate.scm ends here
