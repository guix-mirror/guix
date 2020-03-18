;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 David Thompson <davet@gnu.org>
;;; Copyright © 2019 Jakob L. Kreuze <zerodaysfordays@sdf.org>
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

(define-module (guix scripts deploy)
  #:use-module (gnu machine)
  #:use-module (guix discovery)
  #:use-module (guix scripts)
  #:use-module (guix scripts build)
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix grafts)
  #:use-module (guix status)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-37)
  #:export (guix-deploy))

;;; Commentary:
;;;
;;; This program provides a command-line interface to (gnu machine), allowing
;;; users to perform remote deployments through specification files.
;;;
;;; Code:



(define (show-help)
  (display (G_ "Usage: guix deploy [OPTION] FILE...
Perform the deployment specified by FILE.\n"))
  (show-build-options-help)
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (display (G_ "
  -v, --verbosity=LEVEL  use the given verbosity LEVEL"))
  (show-bug-report-information))

(define %options
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix deploy")))

         (option '(#\s "system") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'system arg
                               (alist-delete 'system result eq?))))
         (option '(#\v "verbosity") #t #f
                 (lambda (opt name arg result)
                   (let ((level (string->number* arg)))
                     (alist-cons 'verbosity level
                                 (alist-delete 'verbosity result)))))

         %standard-build-options))

(define %default-options
  ;; Alist of default option values.
  `((verbosity . 1)
    (debug . 0)
    (graft? . #t)
    (substitutes? . #t)
    (offload? . #t)
    (print-build-trace? . #t)
    (print-extended-build-trace? . #t)
    (multiplexed-build-output? . #t)))

(define (load-source-file file)
  "Load FILE as a user module."
  (let* ((guix-path (dirname (search-path %load-path "guix.scm")))
         (environment-modules (scheme-modules* guix-path "gnu/machine"))
         (module (make-user-module (append '((gnu) (gnu machine))
                                           environment-modules))))
    (load* file module)))

(define (guix-deploy . args)
  (define (handle-argument arg result)
    (alist-cons 'file arg result))

  (let* ((opts (parse-command-line args %options (list %default-options)
                                   #:argument-handler handle-argument))
         (file (assq-ref opts 'file))
         (machines (or (and file (load-source-file file)) '())))
    (with-status-verbosity (assoc-ref opts 'verbosity)
      (with-store store
        (set-build-options-from-command-line store opts)
        (with-build-handler (build-notifier #:use-substitutes?
                                            (assoc-ref opts 'substitutes?))
          (for-each (lambda (machine)
                      (info (G_ "deploying to ~a...~%")
                            (machine-display-name machine))
                      (parameterize ((%graft? (assq-ref opts 'graft?)))
                        (guard (c ((message-condition? c)
                                   (report-error (G_ "failed to deploy ~a: ~a~%")
                                                 (machine-display-name machine)
                                                 (condition-message c)))
                                  ((deploy-error? c)
                                   (when (deploy-error-should-roll-back c)
                                     (info (G_ "rolling back ~a...~%")
                                           (machine-display-name machine))
                                     (run-with-store store (roll-back-machine machine)))
                                   (apply throw (deploy-error-captured-args c))))
                          (run-with-store store (deploy-machine machine)))))
                    machines))))))
