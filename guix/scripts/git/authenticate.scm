;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts git authenticate)
  #:use-module (git)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix git-authenticate)
  #:autoload   (guix openpgp) (openpgp-format-fingerprint
                               openpgp-public-key-fingerprint)
  #:use-module ((guix channels) #:select (openpgp-fingerprint))
  #:use-module ((guix git) #:select (with-git-error-handling))
  #:use-module (guix progress)
  #:use-module (guix base64)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (guix-git-authenticate))

;;; Commentary:
;;;
;;; Authenticate a Git checkout by reading '.guix-authorizations' files and
;;; following the "authorizations invariant" also used by (guix channels).
;;;
;;; Code:

(define %options
  ;; Specifications of the command-line options.
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix git authenticate")))

        (option '(#\r "repository") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'directory arg result)))
        (option '(#\e "end") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'end-commit (string->oid arg) result)))
        (option '(#\k "keyring") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'keyring-reference arg result)))
        (option '("cache-key") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'cache-key arg result)))
        (option '("historical-authorizations") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'historical-authorizations arg
                              result)))
        (option '("stats") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'show-stats? #t result)))))

(define %default-options
  '((directory . ".")
    (keyring-reference . "keyring")))

(define (show-stats stats)
  "Display STATS, an alist containing commit signing stats as returned by
'authenticate-repository'."
  (format #t (G_ "Signing statistics:~%"))
  (for-each (match-lambda
              ((signer . count)
               (format #t "  ~a ~10d~%"
                       (openpgp-format-fingerprint
                        (openpgp-public-key-fingerprint signer))
                       count)))
            (sort stats
                  (match-lambda*
                    (((_ . count1) (_ . count2))
                     (> count1 count2))))))

(define (show-help)
  (display (G_ "Usage: guix git authenticate COMMIT SIGNER [OPTIONS...]
Authenticate the given Git checkout using COMMIT/SIGNER as its introduction.\n"))
  (display (G_ "
  -r, --repository=DIRECTORY
                         open the Git repository at DIRECTORY"))
  (display (G_ "
  -k, --keyring=REFERENCE
                         load keyring from REFERENCE, a Git branch"))
  (display (G_ "
      --stats            display commit signing statistics upon completion"))
  (display (G_ "
      --cache-key=KEY    cache authenticated commits under KEY"))
  (display (G_ "
      --historical-authorizations=FILE
                         read historical authorizations from FILE"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))


;;;
;;; Entry point.
;;;

(define (guix-git-authenticate . args)
  (define options
    (parse-command-line args %options (list %default-options)
                        #:build-options? #f))

  (define (command-line-arguments lst)
    (reverse (filter-map (match-lambda
                           (('argument . arg) arg)
                           (_ #f))
                         lst)))

  (define commit-short-id
    (compose (cut string-take <> 7) oid->string commit-id))

  (define (make-reporter start-commit end-commit commits)
    (format (current-error-port)
            (G_ "Authenticating commits ~a to ~a (~h new \
commits)...~%")
            (commit-short-id start-commit)
            (commit-short-id end-commit)
            (length commits))

    (if (isatty? (current-error-port))
        (progress-reporter/bar (length commits))
        progress-reporter/silent))

  (with-error-handling
    (with-git-error-handling
     (match (command-line-arguments options)
       ((commit signer)
        (let* ((directory   (assoc-ref options 'directory))
               (show-stats? (assoc-ref options 'show-stats?))
               (keyring     (assoc-ref options 'keyring-reference))
               (repository  (repository-open directory))
               (end         (match (assoc-ref options 'end-commit)
                              (#f  (reference-target
                                    (repository-head repository)))
                              (oid oid)))
               (history     (match (assoc-ref options 'historical-authorizations)
                              (#f '())
                              (file (call-with-input-file file
                                      read-authorizations))))
               (cache-key   (or (assoc-ref options 'cache-key)
                                (repository-cache-key repository))))
          (define stats
            (authenticate-repository repository (string->oid commit)
                                     (openpgp-fingerprint signer)
                                     #:end end
                                     #:keyring-reference keyring
                                     #:historical-authorizations history
                                     #:cache-key cache-key
                                     #:make-reporter make-reporter))

          (when (and show-stats? (not (null? stats)))
            (show-stats stats))))
       (_
        (leave (G_ "wrong number of arguments; \
expected COMMIT and SIGNER~%")))))))
