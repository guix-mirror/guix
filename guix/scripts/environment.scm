;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix search-paths)
  #:use-module (guix utils)
  #:use-module (guix monads)
  #:use-module (guix scripts build)
  #:use-module (gnu packages)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-98)
  #:export (guix-environment))

(define (evaluate-input-search-paths inputs derivations)
  "Evaluate the native search paths of INPUTS, a list of packages, of the
outputs of DERIVATIONS, and return a list of search-path/value pairs."
  (let ((directories (append-map (lambda (drv)
                                   (map (match-lambda
                                          ((_ . output)
                                           (derivation-output-path output)))
                                        (derivation-outputs drv)))
                                 derivations))
        (paths       (cons $PATH
                           (delete-duplicates
                            (append-map package-native-search-paths
                                        inputs)))))
    (evaluate-search-paths paths directories)))

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
  (for-each (match-lambda
              ((($ <search-path-specification> variable _ separator) . value)
               (let ((current (getenv variable)))
                 (setenv variable
                         (if (and current (not pure?))
                             (string-append value separator current)
                             value)))))
            (evaluate-input-search-paths inputs derivations)))

(define (show-search-paths inputs derivations pure?)
  "Display the needed search paths to build an environment that contains the
packages within INPUTS.  When PURE? is #t, do not augment existing environment
variables with additional search paths."
  (for-each (match-lambda
              ((search-path . value)
               (display
                (search-path-definition search-path value
                                        #:kind (if pure? 'exact 'prefix)))
               (newline)))
            (evaluate-input-search-paths inputs derivations)))

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
      --ad-hoc           include all specified packages in the environment instead
                         of only their inputs"))
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
         (option '("ad-hoc") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'ad-hoc? #t result)))
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
                 ((or (_ (? package? package))
                      (_ (? package? package) _))
                  package)
                 (_ #f))
                (bag-transitive-inputs
                 (package->bag package))))
  (delete-duplicates
   (append-map transitive-inputs packages)))

(define (packages+propagated-inputs packages)
  "Return a list containing PACKAGES plus all of their propagated inputs."
  (delete-duplicates
   (append packages
           (map (match-lambda
                  ((or (_ (? package? package))
                       (_ (? package? package) _))
                   package)
                  (_ #f))
                (append-map package-transitive-propagated-inputs
                            packages)))))

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
  (define (handle-argument arg result)
    (alist-cons 'package arg result))

  (with-error-handling
    (let* ((opts     (parse-command-line args %options (list %default-options)
                                         #:argument-handler handle-argument))
           (pure?    (assoc-ref opts 'pure))
           (ad-hoc?  (assoc-ref opts 'ad-hoc?))
           (command  (assoc-ref opts 'exec))
           (packages (pick-all (options/resolve-packages opts) 'package))
           (inputs   (if ad-hoc?
                         (packages+propagated-inputs packages)
                         (packages->transitive-inputs packages))))
      (with-store store
        (define drvs
          (run-with-store store
            (mbegin %store-monad
              (set-guile-for-build (default-guile))
              (build-inputs inputs opts))))

        (cond ((assoc-ref opts 'dry-run?)
               #t)
              ((assoc-ref opts 'search-paths)
               (show-search-paths inputs drvs pure?))
              (else
               (create-environment inputs drvs pure?)
               (system command)))))))
