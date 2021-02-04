;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2017, 2018, 2019, 2020, 2021, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Deck Pickard <deck.r.pickard@gmail.com>
;;; Copyright © 2015, 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
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
  #:use-module ((guix profiles) #:select (%profile-directory))
  #:autoload   (guix describe) (current-profile-date)
  #:use-module (guix build syscalls)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:export (synopsis
            category
            define-command
            %command-categories

            args-fold*
            parse-command-line
            maybe-build
            build-package
            build-package-source
            %distro-age-warning
            warn-about-old-distro
            %disk-space-warning
            warn-about-disk-space))

;;; Commentary:
;;;
;;; General code for Guix scripts.
;;;
;;; Code:

;; Syntactic keywords.
(define synopsis 'command-synopsis)
(define category 'command-category)

(define-syntax define-command-categories
  (syntax-rules (G_)
    "Define command categories."
    ((_ name assert-valid (identifiers (G_ synopses)) ...)
     (begin
       (define-public identifiers
         ;; Define and export syntactic keywords.
         (list 'syntactic-keyword-for-command-category))
       ...

       (define-syntax assert-valid
         ;; Validate at expansion time that we're passed a valid category.
         (syntax-rules (identifiers ...)
           ((_ identifiers) #t)
           ...))

       (define name
         ;; Alist mapping category name to synopsis.
         `((identifiers . synopses) ...))))))

;; Command categories.
(define-command-categories %command-categories
  assert-valid-command-category
  (main        (G_ "main commands"))
  (development (G_ "software development commands"))
  (packaging   (G_ "packaging commands"))
  (plumbing    (G_ "plumbing commands"))
  (internal    (G_ "internal commands"))
  (extension   (G_ "extension commands")))

(define-syntax define-command
  (syntax-rules (category synopsis)
    "Define the given command as a procedure along with its synopsis and,
optionally, its category.  The synopsis becomes the docstring of the
procedure, but both the category and synopsis are meant to be read (parsed) by
'guix help'."
    ;; The (synopsis ...) form is here so that xgettext sees those strings as
    ;; translatable.
    ((_ (name . args)
        (synopsis doc) body ...)
     (define (name . args)
       doc
       body ...))
    ((_ (name . args)
        (category cat) (synopsis doc)
        body ...)
     (begin
       (assert-valid-command-category cat)
       (define (name . args)
         doc
         body ...)))))

(define (option-hint guess options)
  "Return the closest long-name OPTIONS from GUESS,
according to'string-distance'."
  (define (options->long-names options)
    (filter string? (append-map option-names options)))
  (match guess
    ((? string?)
     (match (string-split guess #\=)
       ((name rest ...)
        (string-closest name (options->long-names options) #:threshold 3))))
    (_ #f)))

(define (args-fold* args options unrecognized-option-proc operand-proc . seeds)
  "A wrapper on top of `args-fold' that does proper user-facing error
reporting."
  (catch 'misc-error
    (lambda ()
      (apply args-fold args options unrecognized-option-proc
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
             (let ((hint (option-hint name options)))
               (report-error (G_ "~A: unrecognized option~%") name)
               (when hint
                 (display-hint
                  (format #f (G_ "Did you mean @code{~a}?~%") hint)))
               (exit 1)))
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
  (define (seconds->days seconds)
    (round (/ seconds (* 3600 24))))

  (define age
    (match (current-profile-date)
      (#f    #f)
      (date  (- (time-second (current-time time-utc))
                date))))

  (when (and age (>= age old))
    (warning (N_ "Your Guix installation is ~a day old.\n"
                 "Your Guix installation is ~a days old.\n"
                 (seconds->days age))
             (seconds->days age)))
  (when (and (or (not age) (>= age old))
             (not (getenv "GUIX_UNINSTALLED")))
    (warning (G_ "Consider running 'guix pull' followed by
'~a' to get up-to-date packages and security updates.\n")
             suggested-command)
    (newline (guix-warning-port))))

(define %disk-space-warning
  ;; Return a pair of absolute threshold (number of bytes) and relative
  ;; threshold (fraction between 0 and 1) for the free disk space below which
  ;; a warning is emitted.
  ;; GUIX_DISK_SPACE_WARNING can contain both thresholds.  A value in [0;100)
  ;; is a relative threshold, otherwise it's absolute.  The following
  ;; example values are valid:
  ;; - 1GiB;10%      ;1 GiB absolute, and 10% relative.
  ;; - 15G           ;15 GiB absolute, and default relative.
  ;; - 99%           ;99% relative, and default absolute.
  ;; - 99            ;Same.
  ;; - 100           ;100 absolute, and default relative.
  (let* ((default-absolute-threshold (size->number "5GiB"))
         (default-relative-threshold 0.05)
         (percentage->float (lambda (percentage)
                              (or (and=> (string->number
                                          (car (string-split percentage #\%)))
                                         (lambda (n) (/ n 100.0)))
                                  default-relative-threshold)))
         (size->number* (lambda (size)
                          (or (false-if-exception (size->number size))
                              default-absolute-threshold)))
         (absolute? (lambda (size)
                      (not (or (string-suffix? "%" size)
                               (false-if-exception (< (size->number size) 100)))))))
    (make-parameter
     (match (getenv "GUIX_DISK_SPACE_WARNING")
       (#f (list default-absolute-threshold
                 default-relative-threshold))
       (env-string (match (string-split env-string #\;)
                     ((threshold)
                      (if (absolute? threshold)
                          (list (size->number* threshold)
                                default-relative-threshold)
                          (list default-absolute-threshold
                                (percentage->float threshold))))
                     ((threshold1 threshold2)
                      (if (absolute? threshold1)
                          (list (size->number* threshold1)
                                (percentage->float threshold2))
                          (list (size->number* threshold2)
                                (percentage->float threshold1))))))))))

(define* (warn-about-disk-space #:optional profile
                                #:key
                                (thresholds (%disk-space-warning)))
  "Display a hint about 'guix gc' if less than THRESHOLD of /gnu/store is
available.
THRESHOLDS is a pair (ABSOLUTE-THRESHOLD . RELATIVE-THRESHOLD)."
  (define GiB (expt 2 30))

  (let* ((stats      (statfs (%store-prefix)))
         (block-size (file-system-block-size stats))
         (available  (* block-size (file-system-blocks-available stats)))
         (total      (* block-size (file-system-block-count stats)))
         (relative-threshold-in-bytes (* total (cadr thresholds)))
         (absolute-threshold-in-bytes (car thresholds)))
    (when (< available (min relative-threshold-in-bytes
                            absolute-threshold-in-bytes))
      (warning (G_ "only ~,1f GiB of free space available on ~a~%")
               (/ available 1. GiB) (%store-prefix))
      (display-hint (format #f (G_ "Consider deleting old profile
generations and collecting garbage, along these lines:

@example
guix gc --delete-generations=1m
@end example\n"))))))

;;; scripts.scm ends here
