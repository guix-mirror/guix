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
  #:use-module ((gnu packages base) #:select (%final-inputs))
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
  (list (option '(#\u "update") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'update? #t result)))
        (option '(#\s "select") #t #f
                (lambda (opt name arg result)
                  (match arg
                    ((or "core" "non-core")
                     (alist-cons 'select (string->symbol arg)
                                 result))
                    (x
                     (leave (_ "~a: invalid selection; expected `core' or `non-core'")
                            arg)))))

        (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix refresh")))))

(define (show-help)
  (display (_ "Usage: guix refresh [OPTION]... PACKAGE...
Update package definitions to match the latest upstream version.

When PACKAGE... is given, update only the specified packages.  Otherwise
update all the packages of the distribution, or the subset thereof
specified with `--select'.\n"))
  (display (_ "
  -u, --update           update source files in place"))
  (display (_ "
  -s, --select=SUBSET    select all the packages in SUBSET, one of
                         `core' or `non-core'"))
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
    (args-fold* args %options
                (lambda (opt name arg result)
                  (leave (_ "~A: unrecognized option~%") name))
                (lambda (arg result)
                  (alist-cons 'argument arg result))
                %default-options))

  (define core-package?
    (let* ((input->package (match-lambda
                            ((name (? package? package) _ ...) package)
                            (_ #f)))
           (final-inputs   (map input->package %final-inputs))
           (core           (append final-inputs
                                   (append-map (compose (cut filter-map input->package <>)
                                                        package-transitive-inputs)
                                               final-inputs)))
           (names          (delete-duplicates (map package-name core))))
      (lambda (package)
        "Return true if PACKAGE is likely a \"core package\"---i.e., one whose
update would trigger a complete rebuild."
        ;; Compare by name because packages in base.scm basically inherit
        ;; other packages.  So, even if those packages are not core packages
        ;; themselves, updating them would also update those who inherit from
        ;; them.
        ;; XXX: Fails to catch MPFR/MPC, whose *source* is used as input.
        (member (package-name package) names))))

  (let* ((opts     (parse-options))
         (update?  (assoc-ref opts 'update?))
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
                      (let ((select? (match (assoc-ref opts 'select)
                                       ('core core-package?)
                                       ('non-core (negate core-package?))
                                       (_ (const #t)))))
                        ;; TODO: Keep only the newest of each package.
                        (fold-packages (lambda (package result)
                                         (if (select? package)
                                             (cons package result)
                                             result))
                                       '())))
                     (some                        ; user-specified packages
                      some))))
    (with-error-handling
      (if update?
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
                      packages))
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
                    packages)))))
