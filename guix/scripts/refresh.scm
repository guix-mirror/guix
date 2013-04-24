;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts refresh)
  #:use-module (guix ui)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix gnu-maintenance)
  #:use-module (gnu packages)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (rnrs io ports)
  #:export (guix-refresh))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  '())

(define %options
  ;; Specification of the command-line options.
  (list (option '(#\n "dry-run") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'dry-run? #t result)))

        (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix refresh")))))

(define (show-help)
  (display (_ "Usage: guix refresh [OPTION]... PACKAGE...
Update package definitions to match the latest upstream version.\n"))
  (display (_ "
  -n, --dry-run          do not build the derivations"))
  (newline)
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))


;;;
;;; Entry point.
;;;

(define (guix-refresh . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (args-fold args %options
               (lambda (opt name arg result)
                 (leave (_ "~A: unrecognized option~%") name))
               (lambda (arg result)
                 (alist-cons 'argument arg result))
               %default-options))

  (let* ((opts     (parse-options))
         (dry-run? (assoc-ref opts 'dry-run?))
         (packages (match (concatenate
                           (filter-map (match-lambda
                                        (('argument . value)
                                         (let ((p (find-packages-by-name value)))
                                           (unless p
                                             (leave (_ "~a: no package by that name")
                                                    value))
                                           p))
                                        (_ #f))
                                       opts))
                     (()                          ; default to all packages
                      ;; TODO: Keep only the newest of each package.
                      (fold-packages cons '()))
                     (some                        ; user-specified packages
                      some))))
   (with-error-handling
     (if dry-run?
         (for-each (lambda (package)
                     (match (false-if-exception (package-update-path package))
                       ((new-version . directory)
                        (let ((loc (or (package-field-location package 'version)
                                       (package-location package))))
                          (format (current-error-port)
                                  (_ "~a: ~a would be upgraded from ~a to ~a~%")
                                  (location->string loc)
                                  (package-name package) (package-version package)
                                  new-version)))
                       (_ #f)))
                   packages)
         (let ((store (open-connection)))
           (for-each (lambda (package)
                       (let-values (((version tarball)
                                     (catch #t
                                       (lambda ()
                                         (package-update store package))
                                       (lambda _
                                         (values #f #f))))
                                    ((loc)
                                     (or (package-field-location package
                                                                 'version)
                                         (package-location package))))
                         (when version
                           (format (current-error-port)
                                   (_ "~a: ~a: updating from version ~a to version ~a...~%")
                                   (location->string loc) (package-name package)
                                   (package-version package) version)
                           (let ((hash (call-with-input-file tarball
                                         (compose sha256 get-bytevector-all))))
                             (update-package-source package version hash)))))
                     packages))))))
