;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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

(define-module (guix nar)
  #:use-module (guix utils)
  #:use-module (guix serialization)
  #:use-module ((guix build utils)
                #:select (delete-file-recursively with-directory-excursion))
  #:use-module (guix store)
  #:use-module (guix ui)                          ; for '_'
  #:use-module (guix hash)
  #:use-module (guix pki)
  #:use-module (guix pk-crypto)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (nar-invalid-hash-error?
            nar-invalid-hash-error-expected
            nar-invalid-hash-error-actual

            nar-signature-error?
            nar-signature-error-signature

            restore-file-set))

;;; Comment:
;;;
;;; Read and write Nix archives, aka. ‘nar’.
;;;
;;; Code:

(define-condition-type &nar-signature-error &nar-error
  nar-signature-error?
  (signature nar-signature-error-signature))      ; faulty signature or #f

(define-condition-type &nar-invalid-hash-error &nar-signature-error
  nar-invalid-hash-error?
  (expected  nar-invalid-hash-error-expected)     ; expected hash (a bytevector)
  (actual    nar-invalid-hash-error-actual))      ; actual hash



;;;
;;; Restoring a file set into the store.
;;;

;; The code below accesses the store directly and is meant to be run from
;; "build hooks", which cannot invoke the daemon's 'import-paths' RPC since
;; (1) the locks on the files to be restored as already held, and (2) the
;; $NIX_HELD_LOCKS hackish environment variable cannot be set.
;;
;; So we're really duplicating that functionality of the daemon (well, until
;; most of the daemon is in Scheme :-)).  But note that we do use a couple of
;; RPCs for functionality not available otherwise, like 'valid-path?'.

(define (lock-store-file file)
  "Acquire exclusive access to FILE, a store file."
  (call-with-output-file (string-append file ".lock")
    (cut fcntl-flock <> 'write-lock)))

(define (unlock-store-file file)
  "Release access to FILE."
  (call-with-input-file (string-append file ".lock")
    (cut fcntl-flock <> 'unlock)))

(define* (finalize-store-file source target
                              #:key (references '()) deriver (lock? #t))
  "Rename SOURCE to TARGET and register TARGET as a valid store item, with
REFERENCES and DERIVER.  When LOCK? is true, acquire exclusive locks on TARGET
before attempting to register it; otherwise, assume TARGET's locks are already
held."

  ;; XXX: Currently we have to call out to the daemon to check whether TARGET
  ;; is valid.
  (with-store store
    (unless (valid-path? store target)
      (when lock?
        (lock-store-file target))

      (unless (valid-path? store target)
        ;; If FILE already exists, delete it (it's invalid anyway.)
        (when (file-exists? target)
          (delete-file-recursively target))

        ;; Install the new TARGET.
        (rename-file source target)

        ;; Register TARGET.  As a side effect, it resets the timestamps of all
        ;; its files, recursively, and runs a deduplication pass.
        (register-path target
                       #:references references
                       #:deriver deriver))

      (when lock?
        (unlock-store-file target)))))

(define (temporary-store-file)
  "Return the file name of a temporary file created in the store."
  (let* ((template (string-append (%store-prefix) "/guix-XXXXXX"))
         (port     (mkstemp! template)))
    (close-port port)
    template))

(define-syntax-rule (with-temporary-store-file name body ...)
  "Evaluate BODY with NAME bound to the file name of a temporary store item
protected from GC."
  (let loop ((name (temporary-store-file)))
    (with-store store
      ;; Add NAME to the current process' roots.  (Opening this connection to
      ;; the daemon allows us to reuse its code that deals with the
      ;; per-process roots file.)
      (add-temp-root store name)

      ;; There's a window during which GC could delete NAME.  Try again when
      ;; that happens.
      (if (file-exists? name)
          (begin
            (delete-file name)
            body ...)
          (loop (temporary-store-file))))))

(define* (restore-one-item port
                           #:key acl (verify-signature? #t) (lock? #t)
                           (log-port (current-error-port)))
  "Restore one store item from PORT; return its file name on success."

  (define (assert-valid-signature signature hash file)
    ;; Bail out if SIGNATURE, which must be a string as produced by
    ;; 'canonical-sexp->string', doesn't match HASH, a bytevector containing
    ;; the expected hash for FILE.
    (let ((signature (catch 'gcry-error
                       (lambda ()
                         (string->canonical-sexp signature))
                       (lambda (key proc err)
                         (raise (condition
                                 (&message
                                  (message "signature is not a valid \
s-expression"))
                                 (&nar-signature-error
                                  (file file)
                                  (signature signature) (port port))))))))
      (signature-case (signature hash (current-acl))
        (valid-signature #t)
        (invalid-signature
         (raise (condition
                 (&message (message "invalid signature"))
                 (&nar-signature-error
                  (file file) (signature signature) (port port)))))
        (hash-mismatch
         (raise (condition (&message (message "invalid hash"))
                           (&nar-invalid-hash-error
                            (port port) (file file)
                            (signature signature)
                            (expected (hash-data->bytevector
                                       (signature-signed-data signature)))
                            (actual hash)))))
        (unauthorized-key
         (raise (condition (&message (message "unauthorized public key"))
                           (&nar-signature-error
                            (signature signature) (file file) (port port)))))
        (corrupt-signature
         (raise (condition
                 (&message (message "corrupt signature data"))
                 (&nar-signature-error
                  (signature signature) (file file) (port port))))))))

  (define %export-magic
    ;; Number used to identify genuine file set archives.
    #x4558494e)

  (define port*
    ;; Keep that one around, for error conditions.
    port)

  (let-values (((port get-hash)
                (open-sha256-input-port port)))
    (with-temporary-store-file temp
      (restore-file port temp)

      (let ((magic (read-int port)))
        (unless (= magic %export-magic)
          (raise (condition
                  (&message (message "corrupt file set archive"))
                  (&nar-read-error
                   (port port*) (file #f) (token #f))))))

      (let ((file     (read-store-path port))
            (refs     (read-store-path-list port))
            (deriver  (read-string port))
            (hash     (get-hash))
            (has-sig? (= 1 (read-int port))))
        (format log-port
                (_ "importing file or directory '~a'...~%")
                file)

        (let ((sig (and has-sig? (read-string port))))
          (when verify-signature?
            (if sig
                (begin
                  (assert-valid-signature sig hash file)
                  (format log-port
                          (_ "found valid signature for '~a'~%")
                          file)
                  (finalize-store-file temp file
                                       #:references refs
                                       #:deriver deriver
                                       #:lock? lock?))
                (raise (condition
                        (&message (message "imported file lacks \
a signature"))
                        (&nar-signature-error
                         (port port*) (file file) (signature #f))))))
          file)))))

(define* (restore-file-set port
                           #:key (verify-signature? #t) (lock? #t)
                           (log-port (current-error-port)))
  "Restore the file set read from PORT to the store.  The format of the data
on PORT must be as created by 'export-paths'---i.e., a series of Nar-formatted
archives with interspersed meta-data joining them together, possibly with a
digital signature at the end.  Log progress to LOG-PORT.  Return the list of
files restored.

When LOCK? is #f, assume locks for the files to be restored are already held.
This is the case when the daemon calls a build hook.

Note that this procedure accesses the store directly, so it's only meant to be
used by the daemon's build hooks since they cannot call back to the daemon
while the locks are held."
  (define acl
    (current-acl))

  (let loop ((n     (read-long-long port))
             (files '()))
    (case n
      ((0)
       (reverse files))
      ((1)
       (let ((file
              (restore-one-item port
                                #:acl acl #:verify-signature? verify-signature?
                                #:lock? lock? #:log-port log-port)))
         (loop (read-long-long port)
               (cons file files))))
      (else
       ;; Neither 0 nor 1.
       (raise (condition
               (&message (message "invalid inter-file archive mark"))
               (&nar-read-error
                (port port) (file #f) (token #f))))))))

;;; Local Variables:
;;; eval: (put 'with-temporary-store-file 'scheme-indent-function 1)
;;; End:

;;; nar.scm ends here
