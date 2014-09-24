;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2010, 2011, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
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
  #:use-module (guix ui)
  #:export (%gpg-command
            %openpgp-key-server
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
  (make-parameter "gpg2"))

(define %openpgp-key-server
  ;; The default key server.  Note that keys.gnupg.net appears to be
  ;; unreliable.
  (make-parameter "pgp.mit.edu"))

(define (gnupg-verify sig file)
  "Verify signature SIG for FILE.  Return a status s-exp if GnuPG failed."

  (define (status-line->sexp line)
    ;; See file `doc/DETAILS' in GnuPG.
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
    (define errsig-rx
      (make-regexp
       "^\\[GNUPG:\\] ERRSIG ([[:xdigit:]]+) ([^ ]+) ([^ ]+) ([^ ]+) ([[:digit:]]+) ([[:digit:]]+)"))

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
          ((regexp-exec errsig-rx line)
           =>
           (lambda (match)
             `(signature-error ,(match:substring match 1) ; key id or fingerprint
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
                                    (else rc))))))
          (else
           `(unparsed-line ,line))))

  (define (parse-status input)
    (let loop ((line   (read-line input))
               (result '()))
      (if (eof-object? line)
          (reverse result)
          (loop (read-line input)
                (cons (status-line->sexp line) result)))))

  (let* ((pipe   (open-pipe* OPEN_READ (%gpg-command) "--status-fd=1"
                             "--verify" sig file))
         (status (parse-status pipe)))
    ;; Ignore PIPE's exit status since STATUS above should contain all the
    ;; info we need.
    (close-pipe pipe)
    status))

(define (gnupg-status-good-signature? status)
  "If STATUS, as returned by `gnupg-verify', denotes a good signature, return
a key-id/user pair; return #f otherwise."
  (any (lambda (sexp)
         (match sexp
           (((or 'good-signature 'expired-key-signature) key-id user)
            (cons key-id user))
           (_ #f)))
       status))

(define (gnupg-status-missing-key? status)
  "If STATUS denotes a missing-key error, then return the key-id of the
missing key."
  (any (lambda (sexp)
         (match sexp
           (('signature-error key-id _ ...)
            key-id)
           (_ #f)))
       status))

(define (gnupg-receive-keys key-id server)
  (system* (%gpg-command) "--keyserver" server "--recv-keys" key-id))

(define* (gnupg-verify* sig file
                        #:key (key-download 'interactive)
                              (server (%openpgp-key-server)))
  "Like `gnupg-verify', but try downloading the public key if it's missing.
Return #t if the signature was good, #f otherwise.  KEY-DOWNLOAD specifies a
download policy for missing OpenPGP keys; allowed values: 'always', 'never',
and 'interactive' (default)."
  (let ((status (gnupg-verify sig file)))
    (or (gnupg-status-good-signature? status)
        (let ((missing (gnupg-status-missing-key? status)))
          (define (download-and-try-again)
            ;; Download the missing key and try again.
            (begin
              (gnupg-receive-keys missing server)
              (gnupg-status-good-signature? (gnupg-verify sig file))))

          (define (receive?)
            (let ((answer
                   (begin (format #t (_ "~a~a~%")
                                  "Would you like to download this key "
                                  "and add it to your keyring?")
                          (read-line))))
              (string-match (locale-yes-regexp) answer)))

          (and missing
               (case key-download
                 ((never) #f)
                 ((always)
                  (download-and-try-again))
                 (else
                  (and (receive?)
                       (download-and-try-again)))))))))

;;; gnupg.scm ends here
