;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix git-authenticate)
  #:use-module (git)
  #:use-module (guix base16)
  #:use-module ((guix git) #:select (false-if-git-not-found))
  #:use-module (guix i18n)
  #:use-module (guix openpgp)
  #:use-module ((guix utils)
                #:select (cache-directory with-atomic-file-output))
  #:use-module ((guix build utils)
                #:select (mkdir-p))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:autoload   (ice-9 pretty-print) (pretty-print)
  #:export (read-authorizations
            commit-signing-key
            commit-authorized-keys
            authenticate-commit
            authenticate-commits
            load-keyring-from-reference
            previously-authenticated-commits
            cache-authenticated-commit

            git-authentication-error?
            git-authentication-error-commit
            unsigned-commit-error?
            unauthorized-commit-error?
            unauthorized-commit-error-signing-key
            signature-verification-error?
            signature-verification-error-keyring
            signature-verification-error-signature
            missing-key-error?
            missing-key-error-signature))

;;; Commentary:
;;;
;;; This module provides tools to authenticate a range of Git commits.  A
;;; commit is considered "authentic" if and only if it is signed by an
;;; authorized party.  Parties authorized to sign a commit are listed in the
;;; '.guix-authorizations' file of the parent commit.
;;;
;;; Code:

(define-condition-type &git-authentication-error &error
  git-authentication-error?
  (commit  git-authentication-error-commit))

(define-condition-type &unsigned-commit-error &git-authentication-error
  unsigned-commit-error?)

(define-condition-type &unauthorized-commit-error &git-authentication-error
  unauthorized-commit-error?
  (signing-key unauthorized-commit-error-signing-key))

(define-condition-type &signature-verification-error &git-authentication-error
  signature-verification-error?
  (signature signature-verification-error-signature)
  (keyring   signature-verification-error-keyring))

(define-condition-type &missing-key-error &git-authentication-error
  missing-key-error?
  (signature missing-key-error-signature))


(define (commit-signing-key repo commit-id keyring)
  "Return the OpenPGP key that signed COMMIT-ID (an OID).  Raise an exception
if the commit is unsigned, has an invalid signature, or if its signing key is
not in KEYRING."
  (let-values (((signature signed-data)
                (catch 'git-error
                  (lambda ()
                    (commit-extract-signature repo commit-id))
                  (lambda _
                    (values #f #f)))))
    (unless signature
      (raise (condition
              (&unsigned-commit-error (commit commit-id))
              (&message
               (message (format #f (G_ "commit ~a lacks a signature")
                                (oid->string commit-id)))))))

    (let ((signature (string->openpgp-packet signature)))
      (with-fluids ((%default-port-encoding "UTF-8"))
        (let-values (((status data)
                      (verify-openpgp-signature signature keyring
                                                (open-input-string signed-data))))
          (match status
            ('bad-signature
             ;; There's a signature but it's invalid.
             (raise (condition
                     (&signature-verification-error (commit commit-id)
                                                    (signature signature)
                                                    (keyring keyring))
                     (&message
                      (message (format #f (G_ "signature verification failed \
for commit ~a")
                                       (oid->string commit-id)))))))
            ('missing-key
             (raise (condition
                     (&missing-key-error (commit commit-id)
                                         (signature signature))
                     (&message
                      (message (format #f (G_ "could not authenticate \
commit ~a: key ~a is missing")
                                       (oid->string commit-id)
                                       data))))))
            ('good-signature data)))))))

(define (read-authorizations port)
  "Read authorizations in the '.guix-authorizations' format from PORT, and
return a list of authorized fingerprints."
  (match (read port)
    (('authorizations ('version 0)
                      (((? string? fingerprints) _ ...) ...)
                      _ ...)
     (map (lambda (fingerprint)
            (base16-string->bytevector
             (string-downcase (string-filter char-set:graphic fingerprint))))
          fingerprints))))

(define* (commit-authorized-keys repository commit
                                 #:optional (default-authorizations '()))
  "Return the list of OpenPGP fingerprints authorized to sign COMMIT, based on
authorizations listed in its parent commits.  If one of the parent commits
does not specify anything, fall back to DEFAULT-AUTHORIZATIONS."
  (define (parents-have-authorizations-file? commit)
    ;; Return true if at least one of the parents of COMMIT has the
    ;; '.guix-authorizations' file.
    (find (lambda (commit)
            (false-if-git-not-found
             (tree-entry-bypath (commit-tree commit)
                                ".guix-authorizations")))
          (commit-parents commit)))

  (define (assert-parents-lack-authorizations commit)
    ;; If COMMIT removes the '.guix-authorizations' file found in one of its
    ;; parents, raise an error.
    (when (parents-have-authorizations-file? commit)
      (raise (condition
              (&unauthorized-commit-error (commit (commit-id commit))
                                          (signing-key #f))
              (&message
               (message (format #f (G_ "commit ~a attempts \
to remove '.guix-authorizations' file")
                                (oid->string (commit-id commit)))))))))

  (define (commit-authorizations commit)
    (catch 'git-error
      (lambda ()
        (let* ((tree  (commit-tree commit))
               (entry (tree-entry-bypath tree ".guix-authorizations"))
               (blob  (blob-lookup repository (tree-entry-id entry))))
          (read-authorizations
           (open-bytevector-input-port (blob-content blob)))))
      (lambda (key error)
        (if (= (git-error-code error) GIT_ENOTFOUND)
            (begin
              ;; Prevent removal of '.guix-authorizations' since it would make
              ;; it trivial to force a fallback to DEFAULT-AUTHORIZATIONS.
              (assert-parents-lack-authorizations commit)
              default-authorizations)
            (throw key error)))))

  (match (commit-parents commit)
    (() default-authorizations)
    (parents
     (apply lset-intersection bytevector=?
            (map commit-authorizations parents)))))

(define* (authenticate-commit repository commit keyring
                              #:key (default-authorizations '()))
  "Authenticate COMMIT from REPOSITORY and return the signing key fingerprint.
Raise an error when authentication fails.  If one of the parent commits does
not specify anything, fall back to DEFAULT-AUTHORIZATIONS."
  (define id
    (commit-id commit))

  (define signing-key
    (commit-signing-key repository id keyring))

  (unless (member (openpgp-public-key-fingerprint signing-key)
                  (commit-authorized-keys repository commit
                                          default-authorizations))
    (raise (condition
            (&unauthorized-commit-error (commit id)
                                        (signing-key signing-key))
            (&message
             (message (format #f (G_ "commit ~a not signed by an authorized \
key: ~a")
                              (oid->string id)
                              (openpgp-format-fingerprint
                               (openpgp-public-key-fingerprint
                                signing-key))))))))

  signing-key)

(define (load-keyring-from-blob repository oid keyring)
  "Augment KEYRING with the keyring available in the blob at OID, which may or
may not be ASCII-armored."
  (let* ((blob (blob-lookup repository oid))
         (port (open-bytevector-input-port (blob-content blob))))
    (get-openpgp-keyring (if (port-ascii-armored? port)
                             (open-bytevector-input-port (read-radix-64 port))
                             port)
                         keyring)))

(define (load-keyring-from-reference repository reference)
  "Load the '.key' files from the tree at REFERENCE in REPOSITORY and return
an OpenPGP keyring."
  (let* ((reference (branch-lookup repository reference BRANCH-ALL))
         (target    (reference-target reference))
         (commit    (commit-lookup repository target))
         (tree      (commit-tree commit)))
    (fold (lambda (name keyring)
            (if (string-suffix? ".key" name)
                (let ((entry (tree-entry-bypath tree name)))
                  (load-keyring-from-blob repository
                                          (tree-entry-id entry)
                                          keyring))
                keyring))
          %empty-keyring
          (tree-list tree))))

(define* (authenticate-commits repository commits
                               #:key
                               (default-authorizations '())
                               (keyring-reference "keyring")
                               (report-progress (const #t)))
  "Authenticate COMMITS, a list of commit objects, calling REPORT-PROGRESS for
each of them.  Return an alist showing the number of occurrences of each key.
The OpenPGP keyring is loaded from KEYRING-REFERENCE in REPOSITORY."
  (define keyring
    (load-keyring-from-reference repository keyring-reference))

  (fold (lambda (commit stats)
          (report-progress)
          (let ((signer (authenticate-commit repository commit keyring
                                             #:default-authorizations
                                             default-authorizations)))
            (match (assq signer stats)
              (#f          (cons `(,signer . 1) stats))
              ((_ . count) (cons `(,signer . ,(+ count 1))
                                 (alist-delete signer stats))))))
        '()
        commits))


;;;
;;; Caching.
;;;

(define (authenticated-commit-cache-file)
  "Return the name of the file that contains the cache of
previously-authenticated commits."
  (string-append (cache-directory) "/authentication/channels/guix"))

(define (previously-authenticated-commits)
  "Return the previously-authenticated commits as a list of commit IDs (hex
strings)."
  (catch 'system-error
    (lambda ()
      (call-with-input-file (authenticated-commit-cache-file)
        read))
    (lambda args
      (if (= ENOENT (system-error-errno args))
          '()
          (apply throw args)))))

(define (cache-authenticated-commit commit-id)
  "Record in ~/.cache COMMIT-ID and its closure as authenticated (only
COMMIT-ID is written to cache, though)."
  (define %max-cache-length
    ;; Maximum number of commits in cache.
    200)

  (let ((lst  (delete-duplicates
               (cons commit-id (previously-authenticated-commits))))
        (file (authenticated-commit-cache-file)))
    (mkdir-p (dirname file))
    (with-atomic-file-output file
      (lambda (port)
        (let ((lst (if (> (length lst) %max-cache-length)
                       (take lst %max-cache-length) ;truncate
                       lst)))
          (chmod port #o600)
          (display ";; List of previously-authenticated commits.\n\n"
                   port)
          (pretty-print lst port))))))
