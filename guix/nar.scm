;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:export (nar-error?
            nar-error-port
            nar-error-file

            nar-read-error?
            nar-read-error-token

            nar-invalid-hash-error?
            nar-invalid-hash-error-expected
            nar-invalid-hash-error-actual

            nar-signature-error?
            nar-signature-error-signature

            write-file
            restore-file

            restore-file-set))

;;; Comment:
;;;
;;; Read and write Nix archives, aka. ‘nar’.
;;;
;;; Code:

(define-condition-type &nar-error &error      ; XXX: inherit from &nix-error ?
  nar-error?
  (file  nar-error-file)                       ; file we were restoring, or #f
  (port  nar-error-port))                      ; port from which we read

(define-condition-type &nar-read-error &nar-error
  nar-read-error?
  (token nar-read-error-token))                 ; faulty token, or #f

(define-condition-type &nar-signature-error &nar-error
  nar-signature-error?
  (signature nar-signature-error-signature))      ; faulty signature or #f

(define-condition-type &nar-invalid-hash-error &nar-signature-error
  nar-invalid-hash-error?
  (expected  nar-invalid-hash-error-expected)     ; expected hash (a bytevector)
  (actual    nar-invalid-hash-error-actual))      ; actual hash


(define (dump in out size)
  "Copy SIZE bytes from IN to OUT."
  (define buf-size 65536)
  (define buf (make-bytevector buf-size))

  (let loop ((left size))
    (if (<= left 0)
        0
        (let ((read (get-bytevector-n! in buf 0 (min left buf-size))))
          (if (eof-object? read)
              left
              (begin
                (put-bytevector out buf 0 read)
                (loop (- left read))))))))

(define (write-contents file p size)
  "Write SIZE bytes from FILE to output port P."
  (define (call-with-binary-input-file file proc)
    ;; Open FILE as a binary file.  This avoids scan-for-encoding, and thus
    ;; avoids any initial buffering.  Disable file name canonicalization to
    ;; avoid stat'ing like crazy.
    (with-fluids ((%file-port-name-canonicalization #f))
      (let ((port (open-file file "rb")))
        (dynamic-wind
          (const #t)
          (cut proc port)
          (lambda ()
            (close-port port))))))

  (write-string "contents" p)
  (write-long-long size p)
  (call-with-binary-input-file file
    ;; Use `sendfile' when available (Guile 2.0.8+).
    (if (and (compile-time-value (defined? 'sendfile))
             (file-port? p))
        (cut sendfile p <> size 0)
        (cut dump <> p size)))
  (write-padding size p))

(define (read-contents in out)
  "Read the contents of a file from the Nar at IN, write it to OUT, and return
the size in bytes."
  (define executable?
    (match (read-string in)
      ("contents"
       #f)
      ("executable"
       (match (list (read-string in) (read-string in))
         (("" "contents") #t)
         (x (raise
             (condition (&message
                         (message "unexpected executable file marker"))
                        (&nar-read-error (port in)
                                         (file #f)
                                         (token x))))))
       #t)
      (x
       (raise
        (condition (&message (message "unsupported nar file type"))
                   (&nar-read-error (port in) (file #f) (token x)))))))

  (let ((size (read-long-long in)))
    ;; Note: `sendfile' cannot be used here because of port buffering on IN.
    (dump in out size)

    (when executable?
      (chmod out #o755))
    (let ((m (modulo size 8)))
      (unless (zero? m)
        (get-bytevector-n in (- 8 m))))
    size))

(define %archive-version-1
  ;; Magic cookie for Nix archives.
  "nix-archive-1")

(define (write-file file port)
  "Write the contents of FILE to PORT in Nar format, recursing into
sub-directories of FILE as needed."
  (define p port)

  (write-string %archive-version-1 p)

  (let dump ((f file))
    (let ((s (lstat f)))
      (write-string "(" p)
      (case (stat:type s)
        ((regular)
         (write-string "type" p)
         (write-string "regular" p)
         (if (not (zero? (logand (stat:mode s) #o100)))
             (begin
               (write-string "executable" p)
               (write-string "" p)))
         (write-contents f p (stat:size s)))
        ((directory)
         (write-string "type" p)
         (write-string "directory" p)
         (let ((entries (remove (cut member <> '("." ".."))
                                (scandir f))))
           (for-each (lambda (e)
                       (let ((f (string-append f "/" e)))
                         (write-string "entry" p)
                         (write-string "(" p)
                         (write-string "name" p)
                         (write-string e p)
                         (write-string "node" p)
                         (dump f)
                         (write-string ")" p)))
                     entries)))
        ((symlink)
         (write-string "type" p)
         (write-string "symlink" p)
         (write-string "target" p)
         (write-string (readlink f) p))
        (else
         (raise (condition (&message (message "unsupported file type"))
                           (&nar-error (file f) (port port))))))
      (write-string ")" p))))

(define (restore-file port file)
  "Read a file (possibly a directory structure) in Nar format from PORT.
Restore it as FILE."
  (let ((signature (read-string port)))
    (unless (equal? signature %archive-version-1)
      (raise
       (condition (&message (message "invalid nar signature"))
                  (&nar-read-error (port port)
                                   (token signature)
                                   (file #f))))))

  (let restore ((file file))
    (define (read-eof-marker)
      (match (read-string port)
        (")" #t)
        (x (raise
            (condition
             (&message (message "invalid nar end-of-file marker"))
             (&nar-read-error (port port) (file file) (token x)))))))

    (match (list (read-string port) (read-string port) (read-string port))
      (("(" "type" "regular")
       (call-with-output-file file (cut read-contents port <>))
       (read-eof-marker))
      (("(" "type" "symlink")
       (match (list (read-string port) (read-string port))
         (("target" target)
          (symlink target file)
          (read-eof-marker))
         (x (raise
             (condition
              (&message (message "invalid symlink tokens"))
              (&nar-read-error (port port) (file file) (token x)))))))
      (("(" "type" "directory")
       (let ((dir file))
         (mkdir dir)
         (let loop ((prefix (read-string port)))
           (match prefix
             ("entry"
              (match (list (read-string port)
                           (read-string port) (read-string port)
                           (read-string port))
                (("(" "name" file "node")
                 (restore (string-append dir "/" file))
                 (match (read-string port)
                   (")" #t)
                   (x
                    (raise
                     (condition
                      (&message
                       (message "unexpected directory entry termination"))
                      (&nar-read-error (port port)
                                       (file file)
                                       (token x))))))
                 (loop (read-string port)))))
             (")" #t)                             ; done with DIR
             (x
              (raise
               (condition
                (&message (message "unexpected directory inter-entry marker"))
                (&nar-read-error (port port) (file file) (token x)))))))))
      (x
       (raise
        (condition
         (&message (message "unsupported nar entry type"))
         (&nar-read-error (port port) (file file) (token x))))))))


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
        ;; its files, recursively.  However, it doesn't attempt to deduplicate
        ;; its files like 'importPaths' does (FIXME).
        (register-path target
                       #:references references
                       #:deriver deriver))

      (when lock?
        (unlock-store-file target)))))

(define (temporary-store-directory)
  "Return the file name of a temporary directory created in the store that is
protected from garbage collection."
  (let* ((template (string-append (%store-prefix) "/guix-XXXXXX"))
         (port     (mkstemp! template)))
    (close-port port)
    (with-store store
      (add-temp-root store template))

    ;; There's a small window during which the GC could delete the file.  Try
    ;; again if that happens.
    (if (file-exists? template)
        (begin
          ;; It's up to the caller to create that file or directory.
          (delete-file template)
          template)
        (temporary-store-directory))))

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
  (define %export-magic
    ;; Number used to identify genuine file set archives.
    #x4558494e)

  (define port*
    ;; Keep that one around, for error conditions.
    port)

  (define (assert-valid-signature signature hash file)
    ;; Bail out if SIGNATURE, an sexp, doesn't match HASH, a bytevector
    ;; containing the expected hash for FILE.
    (let* ((signature (catch 'gcry-error
                        (lambda ()
                          (string->canonical-sexp signature))
                        (lambda (err . _)
                          (raise (condition
                                  (&message
                                   (message "signature is not a valid \
s-expression"))
                                  (&nar-signature-error
                                   (file file)
                                   (signature signature) (port port)))))))
           (subject   (signature-subject signature))
           (data      (signature-signed-data signature)))
      (if (and data subject)
          (if (authorized-key? subject)
              (if (equal? (hash-data->bytevector data) hash)
                  (unless (valid-signature? signature)
                    (raise (condition
                            (&message (message "invalid signature"))
                            (&nar-signature-error
                             (file file) (signature signature) (port port)))))
                  (raise (condition (&message (message "invalid hash"))
                                    (&nar-invalid-hash-error
                                     (port port) (file file)
                                     (signature signature)
                                     (expected (hash-data->bytevector data))
                                     (actual hash)))))
              (raise (condition (&message (message "unauthorized public key"))
                                (&nar-signature-error
                                 (signature signature) (file file) (port port)))))
          (raise (condition
                  (&message (message "corrupt signature data"))
                  (&nar-signature-error
                   (signature signature) (file file) (port port)))))))

  (let loop ((n     (read-long-long port))
             (files '()))
    (case n
      ((0)
       (reverse files))
      ((1)
       (let-values (((port get-hash)
                     (open-sha256-input-port port)))
         (let ((temp (temporary-store-directory)))
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
                                            #:lock? lock?)
                       (loop (read-long-long port)
                             (cons file files)))
                     (raise (condition
                             (&message (message "imported file lacks \
a signature"))
                             (&nar-signature-error
                              (port port*) (file file) (signature #f)))))))))))
      (else
       ;; Neither 0 nor 1.
       (raise (condition
               (&message (message "invalid inter-file archive mark"))
               (&nar-read-error
                (port port) (file #f) (token #f))))))))

;;; nar.scm ends here
