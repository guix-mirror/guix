;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
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

(define-module (guix scripts environment)
  #:use-module (guix ui)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix utils)
  #:use-module (guix monads)
  #:use-module (guix build utils)
  #:use-module (guix scripts build)
  #:use-module (gnu packages)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-98)
  #:export (guix-environment))

(define (for-each-search-path proc inputs derivations pure?)
  "Apply PROC for each native search path in INPUTS in addition to 'PATH'.
Use the output paths of DERIVATIONS to build each search path.  When PURE? is
#t, the existing search path value is ignored.  Otherwise, the existing search
path value is appended."
  (let ((paths (map derivation->output-path derivations)))
    (for-each (match-lambda
               (($ <search-path-specification>
                   variable directories separator)
                (let* ((current (getenv variable))
                       (path    (search-path-as-list directories paths))
                       (value   (list->search-path-as-string path separator)))
                  (proc variable
                        (if (and current (not pure?))
                            (string-append value separator current)
                            value)))))
              (cons* (search-path-specification
                      (variable "PATH")
                      (files '("bin" "sbin")))
                     (delete-duplicates
                      (append-map package-native-search-paths inputs))))))

;; Protect some env vars from purification.  Borrowed from nix-shell.
(define %precious-variables
  '("HOME" "USER" "LOGNAME" "DISPLAY" "TERM" "TZ" "PAGER"))

(define (purify-environment)
  "Unset almost all environment variables.  A small number of variables such
as 'HOME' and 'USER' are left untouched."
  (for-each unsetenv
            (remove (cut member <> %precious-variables)
                    (match (get-environment-variables)
                      (((names . _) ...)
                       names)))))

(define (create-environment inputs derivations pure?)
  "Set the needed environment variables for all packages within INPUTS.  When
PURE? is #t, unset the variables in the current environment.  Otherwise,
augment existing enviroment variables with additional search paths."
  (when pure? (purify-environment))
  (for-each-search-path setenv inputs derivations pure?))

(define (show-search-paths inputs derivations pure?)
  "Display the needed search paths to build an environment that contains the
packages within INPUTS.  When PURE? is #t, do not augment existing environment
variables with additional search paths."
  (for-each-search-path (lambda (variable value)
                          (format #t "export ~a=\"~a\"~%" variable value))
                        inputs derivations pure?))

(define (show-help)
  (display (_ "Usage: guix environment [OPTION]... PACKAGE...
Build an environment that includes the dependencies of PACKAGE and execute a
shell command in that environment.\n"))
  (display (_ "
  -e, --expression=EXPR  create environment for the package that EXPR
                         evaluates to"))
  (display (_ "
  -l, --load=FILE        create environment for the package that the code within
                         FILE evaluates to"))
  (display (_ "
  -E, --exec=COMMAND     execute COMMAND in new environment"))
  (display (_ "
      --pure             unset existing environment variables"))
  (display (_ "
      --search-paths     display needed environment variable definitions"))
  (newline)
  (show-build-options-help)
  (newline)
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %default-options
  ;; Default to opening a new shell.
  `((exec . ,(or (getenv "SHELL") "/bin/sh"))
    (substitutes? . #t)
    (max-silent-time . 3600)
    (verbosity . 0)))

(define %options
  ;; Specification of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix environment")))
         (option '("pure") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'pure #t result)))
         (option '(#\E "exec") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'exec arg result)))
         (option '("search-paths") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'search-paths #t result)))
         (option '(#\l "load") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'load arg result)))
         (option '(#\e "expression") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'expression arg result)))
         (option '(#\n "dry-run") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'dry-run? #t result)))
         %standard-build-options))

(define (pick-all alist key)
  "Return a list of values in ALIST associated with KEY."
  (define same-key? (cut eq? key <>))

  (fold (lambda (pair memo)
          (match pair
            (((? same-key? k) . v)
             (cons v memo))
            (_ memo)))
        '() alist))

(define (options/resolve-packages opts)
  "Return OPTS with package specification strings replaced by actual
packages."
  (map (match-lambda
        (('package . (? string? spec))
         `(package . ,(specification->package spec)))
        (('expression . str)
         (match (read/eval str)
           ((? package? p)
            `(package . ,p))))
        (('load . file)
         `(package . ,(load (string-append (getcwd) "/" file))))
        (opt opt))
       opts))

(define (packages->transitive-inputs packages)
  "Return a list of the transitive inputs for all PACKAGES."
  (define (transitive-inputs package)
    (filter-map (match-lambda
                 ((_ (? package? package)) package)
                 (_ #f))
                (bag-transitive-inputs
                 (package->bag package))))
  (delete-duplicates
   (append-map transitive-inputs packages)))

;; TODO: Deduplicate these.
(define show-what-to-build*
  (store-lift show-what-to-build))

(define set-build-options-from-command-line*
  (store-lift set-build-options-from-command-line))

(define (build-inputs inputs opts)
  "Build the packages in INPUTS using the build options in OPTS."
  (let ((substitutes? (assoc-ref opts 'substitutes?))
        (dry-run? (assoc-ref opts 'dry-run?)))
    (mlet* %store-monad ((drvs (sequence %store-monad
                                         (map package->derivation inputs))))
      (mbegin %store-monad
        (show-what-to-build* drvs
                             #:use-substitutes? substitutes?
                             #:dry-run? dry-run?)
        (if dry-run?
            (return #f)
            (mbegin %store-monad
              (set-build-options-from-command-line* opts)
              (built-derivations drvs)
              (return drvs)))))))

;; Entry point.
(define (guix-environment . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (append (parse-options-from args)
            (parse-options-from (environment-build-options))))

  (define (parse-options-from args)
    ;; Actual parsing takes place here.
    (args-fold* args %options
                (lambda (opt name arg result)
                  (leave (_ "~A: unrecognized option~%") name))
                (lambda (arg result)
                  (alist-cons 'package arg result))
                %default-options))

  (with-store store
    (let* ((opts (parse-options))
           (pure? (assoc-ref opts 'pure))
           (command (assoc-ref opts 'exec))
           (inputs (packages->transitive-inputs
                    (pick-all (options/resolve-packages opts) 'package)))
           (drvs (run-with-store store (build-inputs inputs opts))))
      (cond ((assoc-ref opts 'dry-run?)
             #t)
            ((assoc-ref opts 'search-paths)
             (show-search-paths inputs drvs pure?))
            (else
             (create-environment inputs drvs pure?)
             (system command))))))
