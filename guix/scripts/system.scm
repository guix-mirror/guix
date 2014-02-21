;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts system)
  #:use-module (guix ui)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix monads)
  #:use-module (guix scripts build)
  #:use-module (gnu system vm)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:export (guix-system))

(define %user-module
  ;; Module in which the machine description file is loaded.
  (let ((module (make-fresh-user-module)))
    (for-each (lambda (iface)
                (module-use! module (resolve-interface iface)))
              '((gnu system)
                (gnu services)
                (gnu system shadow)))
    module))

(define (read-operating-system file)
  "Read the operating-system declaration from FILE and return it."
  ;; TODO: Factorize.
  (catch #t
    (lambda ()
      ;; Avoid ABI incompatibility with the <operating-system> record.
      (set! %fresh-auto-compile #t)

      (save-module-excursion
       (lambda ()
         (set-current-module %user-module)
         (primitive-load file))))
    (lambda args
      (match args
        (('system-error . _)
         (let ((err (system-error-errno args)))
           (leave (_ "failed to open operating system file '~a': ~a~%")
                  file (strerror err))))
        (_
         (leave (_ "failed to load machine file '~a': ~s~%")
                file args))))))


;;;
;;; Options.
;;;

(define (show-help)
  (display (_ "Usage: guix system [OPTION] ACTION FILE
Build the operating system declared in FILE according to ACTION.\n"))
  (display (_ "Currently the only valid value for ACTION is 'vm', which builds
a virtual machine of the given operating system.\n"))
  (show-build-options-help)
  (newline)
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specifications of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix system")))
         (option '(#\n "dry-run") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'dry-run? #t result)))
         %standard-build-options))

(define %default-options
  ;; Alist of default option values.
  `((system . ,(%current-system))
    (substitutes? . #t)
    (build-hook? . #t)
    (max-silent-time . 3600)
    (verbosity . 0)))


;;;
;;; Entry point.
;;;

(define (guix-system . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (args-fold* args %options
                (lambda (opt name arg result)
                  (leave (_ "~A: unrecognized option~%") name))
                (lambda (arg result)
                  (if (assoc-ref result 'action)
                      (let ((previous (assoc-ref result 'argument)))
                        (if previous
                            (leave (_ "~a: extraneous argument~%") previous)
                            (alist-cons 'argument arg result)))
                      (let ((action (string->symbol arg)))
                        (case action
                          ((vm) (alist-cons 'action action result))
                          (else (leave (_ "~a: unknown action~%")
                                       action))))))
                %default-options))

  (with-error-handling
    (let* ((opts  (parse-options))
           (file  (assoc-ref opts 'argument))
           (os    (if file
                      (read-operating-system file)
                      (leave (_ "no configuration file specified~%"))))
           (mdrv  (system-qemu-image/shared-store-script os))
           (store (open-connection))
           (dry?  (assoc-ref opts 'dry-run?))
           (drv   (run-with-store store mdrv)))
      (set-build-options-from-command-line store opts)
      (show-what-to-build store (list drv)
                          #:dry-run? dry?
                          #:use-substitutes? (assoc-ref opts 'substitutes?))

      (unless dry?
        (build-derivations store (list drv))
        (display (derivation->output-path drv))
        (newline)))))
