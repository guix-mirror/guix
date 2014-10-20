;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module (guix hash)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix gnu-maintenance)
  #:use-module (guix gnupg)
  #:use-module (gnu packages)
  #:use-module ((gnu packages commencement) #:select (%final-inputs))
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 format)
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
                     (leave (_ "~a: invalid selection; expected `core' or `non-core'~%")
                            arg)))))
        (option '(#\l "list-dependent") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'list-dependent? #t result)))

        (option '("key-server") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'key-server arg result)))
        (option '("gpg") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'gpg-command arg result)))
        (option '("key-download") #t #f
                (lambda (opt name arg result)
                  (match arg
                    ((or "interactive" "always" "never")
                     (alist-cons 'key-download (string->symbol arg)
                                 result))
                    (_
                     (leave (_ "unsupported policy: ~a~%")
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
  (display (_ "
  -l, --list-dependent   list top-level dependent packages that would need to
                         be rebuilt as a result of upgrading PACKAGE..."))
  (newline)
  (display (_ "
      --key-server=HOST  use HOST as the OpenPGP key server"))
  (display (_ "
      --gpg=COMMAND      use COMMAND as the GnuPG 2.x command"))
  (display (_ "
      --key-download=POLICY
                         handle missing OpenPGP keys according to POLICY:
                         'always', 'never', and 'interactive', which is also
                         used when 'key-download' is not specified"))
  (newline)
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define* (update-package store package #:key (key-download 'interactive))
  "Update the source file that defines PACKAGE with the new version.
KEY-DOWNLOAD specifies a download policy for missing OpenPGP keys; allowed
values: 'interactive' (default), 'always', and 'never'."
  (let-values (((version tarball)
                (catch #t
                  (lambda ()
                    (package-update store package #:key-download key-download))
                  (lambda _
                    (values #f #f))))
               ((loc)
                (or (package-field-location package
                                            'version)
                    (package-location package))))
    (when version
      (if (and=> tarball file-exists?)
          (begin
            (format (current-error-port)
                    (_ "~a: ~a: updating from version ~a to version ~a...~%")
                    (location->string loc)
                    (package-name package)
                    (package-version package) version)
            (let ((hash (call-with-input-file tarball
                          port-sha256)))
              (update-package-source package version hash)))
          (warning (_ "~a: version ~a could not be \
downloaded and authenticated; not updating")
                   (package-name package) version)))))



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

  (define (keep-newest package lst)
    ;; If a newer version of PACKAGE is already in LST, return LST; otherwise
    ;; return LST minus the other version of PACKAGE in it, plus PACKAGE.
    (let ((name (package-name package)))
      (match (find (lambda (p)
                     (string=? (package-name p) name))
                   lst)
        ((? package? other)
         (if (version>? (package-version other) (package-version package))
             lst
             (cons package (delq other lst))))
        (_
         (cons package lst)))))

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

  (let* ((opts            (parse-options))
         (update?         (assoc-ref opts 'update?))
         (list-dependent? (assoc-ref opts 'list-dependent?))
         (key-download    (assoc-ref opts 'key-download))
         (packages
          (match (concatenate
                  (filter-map (match-lambda
                               (('argument . value)
                                (let ((p (find-packages-by-name value)))
                                  (when (null? p)
                                    (leave (_ "~a: no package by that name~%")
                                           value))
                                  p))
                               (_ #f))
                              opts))
                 (()                          ; default to all packages
                  (let ((select? (match (assoc-ref opts 'select)
                                        ('core core-package?)
                                        ('non-core (negate core-package?))
                                        (_ (const #t)))))
                    (fold-packages (lambda (package result)
                                     (if (select? package)
                                         (keep-newest package result)
                                         result))
                                   '())))
                 (some                        ; user-specified packages
                  some))))
    (with-error-handling
      (cond
       (list-dependent?
        (let* ((rebuilds (map package-full-name
                              (package-covering-dependents packages)))
               (total-dependents
                (length (package-transitive-dependents packages))))
          (if (= total-dependents 0)
              (format (current-output-port)
                      (N_ "No dependents other than itself: ~{~a~}~%"
                          "No dependents other than themselves: ~{~a~^ ~}~%"
                          (length packages))
                      (map package-full-name packages))
              (format (current-output-port)
                      (N_ (N_ "A single dependent package: ~2*~{~a~}~%"
                              "Building the following package would ensure ~d \
dependent packages are rebuilt; ~*~{~a~^ ~}~%"
                              total-dependents)
                          "Building the following ~d packages would ensure ~d \
dependent packages are rebuilt: ~{~a~^ ~}~%"
                          (length rebuilds))
                      (length rebuilds) total-dependents rebuilds))))
       (update?
        (let ((store (open-connection)))
          (parameterize ((%openpgp-key-server
                          (or (assoc-ref opts 'key-server)
                              (%openpgp-key-server)))
                         (%gpg-command
                          (or (assoc-ref opts 'gpg-command)
                              (%gpg-command))))
            (for-each
             (cut update-package store <> #:key-download key-download)
             packages))))
       (else
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
                  packages))))))
