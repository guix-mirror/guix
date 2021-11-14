;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2010, 2011, 2013, 2014, 2016, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (guix gnupg)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 i18n)
  #:use-module (srfi srfi-1)
  #:use-module (guix i18n)
  #:use-module ((guix utils) #:select (config-directory))
  #:use-module ((guix build utils) #:select (mkdir-p))
  #:export (%gpg-command
            %openpgp-key-server
            current-keyring
            gnupg-verify
            gnupg-verify*
            gnupg-status-good-signature?
            gnupg-status-missing-key?))

;;; Commentary:
;;;
;;; GnuPG interface.
;;;
;;; Code:

(define %gpg-command
  ;; The GnuPG 2.x command-line program name.
  (make-parameter (or (getenv "GUIX_GPG_COMMAND") "gpg")))

(define %gpgv-command
  ;; The 'gpgv' program.
  (make-parameter (or (getenv "GUIX_GPGV_COMMAND") "gpgv")))

(define current-keyring
  ;; The default keyring of "trusted keys".
  (make-parameter (string-append (config-directory #:ensure? #f)
                                 "/gpg/trustedkeys.kbx")))

(define %openpgp-key-server
  ;; The default key server.  It defaults to #f, which causes GnuPG to use the
  ;; one it is configured with.
  (make-parameter #f))

;; Regexps for status lines.  See file `doc/DETAILS' in GnuPG.

(define sigid-rx
  (make-regexp
   "^\\[GNUPG:\\] SIG_ID ([A-Za-z0-9+/]+) ([[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}) ([[:digit:]]+)"))
(define goodsig-rx
  (make-regexp "^\\[GNUPG:\\] GOODSIG ([[:xdigit:]]+) (.+)$"))
(define validsig-rx
  (make-regexp
   "^\\[GNUPG:\\] VALIDSIG ([[:xdigit:]]+) ([[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}) ([[:digit:]]+) .*$"))
(define expkeysig-rx                    ; good signature, but expired key
  (make-regexp "^\\[GNUPG:\\] EXPKEYSIG ([[:xdigit:]]+) (.*)$"))
(define revkeysig-rx                    ; good signature, but revoked key
  (make-regexp "^\\[GNUPG:\\] REVKEYSIG ([[:xdigit:]]+) (.*)$"))
(define errsig-rx
  ;; Note: The fingeprint part (the last element of the line) appeared in
  ;; GnuPG 2.2.7 according to 'doc/DETAILS', and it may be missing.
  (make-regexp
   "^\\[GNUPG:\\] ERRSIG ([[:xdigit:]]+) ([^ ]+) ([^ ]+) ([^ ]+) ([[:digit:]]+) ([[:digit:]]+)(.*)"))


(define* (gnupg-verify sig file
                       #:optional (keyring (current-keyring)))
  "Verify signature SIG for FILE against the keys in KEYRING.  All the keys in
KEYRING as assumed to be \"trusted\", whether or not they expired or were
revoked.  Return a status s-exp if GnuPG failed."

  (define (maybe-fingerprint str)
    (match (string-trim-both str)
      ((or "-" "") #f)
      (fpr         fpr)))

  (define (status-line->sexp line)
    (cond ((regexp-exec sigid-rx line)
           =>
           (lambda (match)
             `(signature-id ,(match:substring match 1) ; sig id
                            ,(match:substring match 2) ; date
                            ,(string->number           ; timestamp
                              (match:substring match 3)))))
          ((regexp-exec goodsig-rx line)
           =>
           (lambda (match)
             `(good-signature ,(match:substring match 1)    ; key id
                              ,(match:substring match 2)))) ; user name
          ((regexp-exec validsig-rx line)
           =>
           (lambda (match)
             `(valid-signature ,(match:substring match 1) ; fingerprint
                               ,(match:substring match 2) ; sig creation date
                               ,(string->number           ; timestamp
                                 (match:substring match 3)))))
          ((regexp-exec expkeysig-rx line)
           =>
           (lambda (match)
             `(expired-key-signature ,(match:substring match 1) ; fingerprint
                                     ,(match:substring match 2)))) ; user name
          ((regexp-exec revkeysig-rx line)
           =>
           (lambda (match)
             `(revoked-key-signature ,(match:substring match 1) ; fingerprint
                                     ,(match:substring match 2)))) ; user name
          ((regexp-exec errsig-rx line)
           =>
           (lambda (match)
             `(signature-error ,(match:substring match 1) ; key id
                               ,(match:substring match 2) ; pubkey algo
                               ,(match:substring match 3) ; hash algo
                               ,(match:substring match 4) ; sig class
                               ,(string->number           ; timestamp
                                 (match:substring match 5))
                               ,(let ((rc
                                       (string->number ; return code
                                        (match:substring match 6))))
                                  (case rc
                                    ((9) 'missing-key)
                                    ((4) 'unknown-algorithm)
                                    (else rc)))
                               ,(maybe-fingerprint ; fingerprint or #f
                                 (match:substring match 7)))))
          (else
           `(unparsed-line ,line))))

  (define (parse-status input)
    (let loop ((line   (read-line input))
               (result '()))
      (if (eof-object? line)
          (reverse result)
          (loop (read-line input)
                (cons (status-line->sexp line) result)))))

  (let* ((pipe   (open-pipe* OPEN_READ (%gpgv-command) "--status-fd=1"
                             "--keyring" keyring sig file))
         (status (parse-status pipe)))
    ;; Ignore PIPE's exit status since STATUS above should contain all the
    ;; info we need.
    (close-pipe pipe)
    status))

(define (gnupg-status-good-signature? status)
  "If STATUS, as returned by `gnupg-verify', denotes a good signature, return
a fingerprint/user pair; return #f otherwise."
  (match (assq 'valid-signature status)
    (('valid-signature fingerprint date timestamp)
     (match (or (assq 'good-signature status)
                (assq 'expired-key-signature status)
                (assq 'revoked-key-signature status))
       ((_ key-id user) (cons fingerprint user))
       (_ #f)))
    (_
     #f)))

(define (gnupg-status-missing-key? status)
  "If STATUS denotes a missing-key error, then return the fingerprint of the
missing key or its key id if the fingerprint is unavailable."
  (any (lambda (sexp)
         (match sexp
           (('signature-error key-id _ ... 'missing-key fingerprint)
            (or fingerprint key-id))
           (_ #f)))
       status))

(define* (gnupg-receive-keys fingerprint/key-id
                             #:key server (keyring (current-keyring)))
  "Download FINGERPRINT/KEY-ID from SERVER if specified, otherwise from
GnuPG's default/configured one.  The key is added to KEYRING."
  (unless (file-exists? keyring)
    (mkdir-p (dirname keyring))
    (call-with-output-file keyring (const #t))) ;create an empty keybox

  (zero? (apply system*
                `(,(%gpg-command)
                  ,@(if server
                        (list "--keyserver" server)
                        '())
                  "--no-default-keyring" "--keyring" ,keyring
                  "--recv-keys" ,fingerprint/key-id))))

(define* (gnupg-verify* sig file
                        #:key
                        (key-download 'interactive)
                        server
                        (keyring (current-keyring)))
  "Like `gnupg-verify', but try downloading the public key if it's missing.
Return two values: 'valid-signature and a fingerprint/name pair upon success,
'missing-key and a fingerprint if the key could not be found, and
'invalid-signature with a fingerprint if the signature is invalid.

KEY-DOWNLOAD specifies a download policy for missing OpenPGP keys; allowed
values: 'always', 'never', and 'interactive' (default).  Return a
fingerprint/user name pair on success and #f otherwise."
  (let ((status (gnupg-verify sig file)))
    (match (gnupg-status-good-signature? status)
      ((fingerprint . user)
       (values 'valid-signature (cons fingerprint user)))
      (#f
       (let ((missing (gnupg-status-missing-key? status)))
         (define (download-and-try-again)
           ;; Download the missing key and try again.
           (if (gnupg-receive-keys missing #:server server #:keyring keyring)
               (match (gnupg-status-good-signature?
                       (gnupg-verify sig file keyring))
                 (#f
                  (values 'invalid-signature missing))
                 ((fingerprint . user)
                  (values 'valid-signature
                          (cons fingerprint user))))
               (values 'missing-key missing)))

         (define (receive?)
           (let ((answer
                  (begin
                    (format #t (G_ "Would you like to add this key \
to keyring '~a'?~%")
                            keyring)
                    (read-line))))
             (string-match (locale-yes-regexp) answer)))

         (case key-download
           ((never)
            (values 'missing-key missing))
           ((always)
            (download-and-try-again))
           (else
            (if (receive?)
                (download-and-try-again)
                (values 'missing-key missing)))))))))

;;; gnupg.scm ends here
