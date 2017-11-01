;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Deck Pickard <deck.r.pickard@gmail.com>
;;; Copyright © 2015, 2016 Alex Kost <alezost@gmail.com>
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

(define-module (guix scripts)
  #:use-module (guix grafts)
  #:use-module (guix utils)
  #:use-module (guix ui)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:export (args-fold*
            parse-command-line
            maybe-build
            build-package
            build-package-source
            %distro-age-warning
            warn-about-old-distro))

;;; Commentary:
;;;
;;; General code for Guix scripts.
;;;
;;; Code:

(define (args-fold* options unrecognized-option-proc operand-proc . seeds)
  "A wrapper on top of `args-fold' that does proper user-facing error
reporting."
  (catch 'misc-error
    (lambda ()
      (apply args-fold options unrecognized-option-proc
             operand-proc seeds))
    (lambda (key proc msg args . rest)
      ;; XXX: MSG is not i18n'd.
      (leave (G_ "invalid argument: ~a~%")
             (apply format #f msg args)))))

(define (environment-build-options)
  "Return additional build options passed as environment variables."
  (arguments-from-environment-variable "GUIX_BUILD_OPTIONS"))

(define %default-argument-handler
  ;; The default handler for non-option command-line arguments.
  (lambda (arg result)
    (alist-cons 'argument arg result)))

(define* (parse-command-line args options seeds
                             #:key
                             (build-options? #t)
                             (argument-handler %default-argument-handler))
  "Parse the command-line arguments ARGS according to OPTIONS (a list of
SRFI-37 options) and return the result, seeded by SEEDS.  When BUILD-OPTIONS?
is true, also pass arguments passed via the 'GUIX_BUILD_OPTIONS' environment
variable.  Command-line options take precedence those passed via
'GUIX_BUILD_OPTIONS'.

ARGUMENT-HANDLER is called for non-option arguments, like the 'operand-proc'
parameter of 'args-fold'."
  (define (parse-options-from args seeds)
    ;; Actual parsing takes place here.
    (apply args-fold* args options
           (lambda (opt name arg . rest)
             (leave (G_ "~A: unrecognized option~%") name))
           argument-handler
           seeds))

  (call-with-values
      (lambda ()
        (if build-options?
            (parse-options-from (environment-build-options) seeds)
            (apply values seeds)))
    (lambda seeds
      ;; ARGS take precedence over what the environment variable specifies.
      (parse-options-from args seeds))))

(define* (maybe-build drvs
                      #:key dry-run? use-substitutes?)
  "Show what will/would be built, and actually build DRVS, unless DRY-RUN? is
true."
  (with-monad %store-monad
    (>>= (show-what-to-build* drvs
                              #:dry-run? dry-run?
                              #:use-substitutes? use-substitutes?)
         (lambda (_)
           (if dry-run?
               (return #f)
               (built-derivations drvs))))))

(define* (build-package package
                        #:key dry-run? (use-substitutes? #t)
                        #:allow-other-keys
                        #:rest build-options)
  "Build PACKAGE using BUILD-OPTIONS acceptable by 'set-build-options'.
Show what and how will/would be built."
  (mlet %store-monad ((grafting? ((lift0 %graft? %store-monad))))
    (apply set-build-options*
           #:use-substitutes? use-substitutes?
           (strip-keyword-arguments '(#:dry-run?) build-options))
    (mlet %store-monad ((derivation (package->derivation
                                     package #:graft? (and (not dry-run?)
                                                           grafting?))))
      (mbegin %store-monad
        (maybe-build (list derivation)
                     #:use-substitutes? use-substitutes?
                     #:dry-run? dry-run?)
        (return (show-derivation-outputs derivation))))))

(define* (build-package-source package
                               #:key dry-run? (use-substitutes? #t)
                               #:allow-other-keys
                               #:rest build-options)
  "Build PACKAGE source using BUILD-OPTIONS."
  (mbegin %store-monad
    (apply set-build-options*
           #:use-substitutes? use-substitutes?
           (strip-keyword-arguments '(#:dry-run?) build-options))
    (mlet %store-monad ((derivation (origin->derivation
                                     (package-source package))))
      (mbegin %store-monad
        (maybe-build (list derivation)
                     #:use-substitutes? use-substitutes?
                     #:dry-run? dry-run?)
        (return (show-derivation-outputs derivation))))))

(define %distro-age-warning
  ;; The age (in seconds) above which we warn that the distro is too old.
  (make-parameter (match (and=> (getenv "GUIX_DISTRO_AGE_WARNING")
                                string->duration)
                    (#f  (* 7 24 3600))
                    (age (time-second age)))))

(define* (warn-about-old-distro #:optional (old (%distro-age-warning))
                                #:key (suggested-command
                                       "guix package -u"))
  "Emit a warning if Guix is older than OLD seconds."
  (let-syntax ((false-if-not-found
                (syntax-rules ()
                  ((_ exp)
                   (catch 'system-error
                     (lambda ()
                       exp)
                     (lambda args
                       (if (= ENOENT (system-error-errno args))
                           #f
                           (apply throw args))))))))
    (define (seconds->days seconds)
      (round (/ seconds (* 3600 24))))

    (define age
      (match (false-if-not-found
              (lstat (string-append (config-directory #:ensure? #f)
                                    "/latest")))
        (#f    #f)
        (stat  (- (time-second (current-time time-utc))
                  (stat:mtime stat)))))

    (when (and age (>= age old))
      (warning (N_ "Your Guix installation is ~a day old.\n"
                   "Your Guix installation is ~a days old.\n"
                   (seconds->days age))
               (seconds->days age)))
    (when (or (not age) (>= age old))
      (warning (G_ "Consider running 'guix pull' followed by
'~a' to get up-to-date packages and security updates.\n")
               suggested-command)
      (newline (guix-warning-port)))))

;;; scripts.scm ends here
