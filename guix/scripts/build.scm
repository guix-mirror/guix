;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Mark H Weaver <mhw@netris.org>
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

(define-module (guix scripts build)
  #:use-module (guix ui)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-37)
  #:autoload   (gnu packages) (find-packages-by-name
                               find-newest-available-packages)
  #:export (guix-build))

(define %store
  (make-parameter #f))

(define (derivations-from-package-expressions exp system source?)
  "Eval EXP and return the corresponding derivation path for SYSTEM.
When SOURCE? is true, return the derivations of the package sources."
  (let ((p (eval exp (current-module))))
    (if (package? p)
        (if source?
            (let ((source (package-source p))
                  (loc    (package-location p)))
              (if source
                  (package-source-derivation (%store) source)
                  (leave (_ "~a: error: package `~a' has no source~%")
                         (location->string loc) (package-name p))))
            (package-derivation (%store) p system))
        (leave (_ "expression `~s' does not evaluate to a package~%")
               exp))))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  `((system . ,(%current-system))
    (substitutes? . #t)
    (verbosity . 0)))

(define (show-help)
  (display (_ "Usage: guix build [OPTION]... PACKAGE-OR-DERIVATION...
Build the given PACKAGE-OR-DERIVATION and return their output paths.\n"))
  (display (_ "
  -e, --expression=EXPR  build the package EXPR evaluates to"))
  (display (_ "
  -S, --source           build the packages' source derivations"))
  (display (_ "
  -s, --system=SYSTEM    attempt to build for SYSTEM--e.g., \"i686-linux\""))
  (display (_ "
  -d, --derivations      return the derivation paths of the given packages"))
  (display (_ "
  -K, --keep-failed      keep build tree of failed builds"))
  (display (_ "
  -n, --dry-run          do not build the derivations"))
  (display (_ "
      --no-substitutes   build instead of resorting to pre-built substitutes"))
  (display (_ "
  -c, --cores=N          allow the use of up to N CPU cores for the build"))
  (display (_ "
  -r, --root=FILE        make FILE a symlink to the result, and register it
                         as a garbage collector root"))
  (display (_ "
      --verbosity=LEVEL  use the given verbosity LEVEL"))
  (newline)
  (display (_ "
  -h, --help             display this help and exit"))
  (display (_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specifications of the command-line options.
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix build")))

        (option '(#\S "source") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'source? #t result)))
        (option '(#\s "system") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'system arg
                              (alist-delete 'system result eq?))))
        (option '(#\d "derivations") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'derivations-only? #t result)))
        (option '(#\e "expression") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'expression
                              (call-with-input-string arg read)
                              result)))
        (option '(#\K "keep-failed") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'keep-failed? #t result)))
        (option '(#\c "cores") #t #f
                (lambda (opt name arg result)
                  (let ((c (false-if-exception (string->number arg))))
                    (if c
                        (alist-cons 'cores c result)
                        (leave (_ "~a: not a number~%") arg)))))
        (option '(#\n "dry-run") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'dry-run? #t result)))
        (option '("no-substitutes") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'substitutes? #f
                              (alist-delete 'substitutes? result))))
        (option '(#\r "root") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'gc-root arg result)))
        (option '("verbosity") #t #f
                (lambda (opt name arg result)
                  (let ((level (string->number arg)))
                    (alist-cons 'verbosity level
                                (alist-delete 'verbosity result)))))))


;;;
;;; Entry point.
;;;

(define (guix-build . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (args-fold args %options
               (lambda (opt name arg result)
                 (leave (_ "~A: unrecognized option~%") name))
               (lambda (arg result)
                 (alist-cons 'argument arg result))
               %default-options))

  (define (register-root paths root)
    ;; Register ROOT as an indirect GC root for all of PATHS.
    (let* ((root (string-append (canonicalize-path (dirname root))
                                "/" root)))
     (catch 'system-error
       (lambda ()
         (match paths
           ((path)
            (symlink path root)
            (add-indirect-root (%store) root))
           ((paths ...)
            (fold (lambda (path count)
                    (let ((root (string-append root "-" (number->string count))))
                      (symlink path root)
                      (add-indirect-root (%store) root))
                    (+ 1 count))
                  0
                  paths))))
       (lambda args
         (format (current-error-port)
                 (_ "failed to create GC root `~a': ~a~%")
                 root (strerror (system-error-errno args)))
         (exit 1)))))

  (define newest-available-packages
    (memoize find-newest-available-packages))

  (define (find-best-packages-by-name name version)
    (if version
        (find-packages-by-name name version)
        (match (vhash-assoc name (newest-available-packages))
          ((_ version pkgs ...) pkgs)
          (#f '()))))

  (define (find-package request)
    ;; Return a package matching REQUEST.  REQUEST may be a package
    ;; name, or a package name followed by a hyphen and a version
    ;; number.  If the version number is not present, return the
    ;; preferred newest version.
    (let-values (((name version)
                  (package-name->name+version request)))
      (match (find-best-packages-by-name name version)
        ((p)                                      ; one match
         p)
        ((p x ...)                                ; several matches
         (format (current-error-port)
                 (_ "warning: ambiguous package specification `~a'~%")
                 request)
         (format (current-error-port)
                 (_ "warning: choosing ~a from ~a~%")
                 (package-full-name p)
                 (location->string (package-location p)))
         p)
        (_                                        ; no matches
         (if version
             (leave (_ "~A: package not found for version ~a~%")
                    name version)
             (leave (_ "~A: unknown package~%") name))))))

  (with-error-handling
    (let ((opts (parse-options)))
      (parameterize ((%store (open-connection)))
        (let* ((src? (assoc-ref opts 'source?))
               (sys  (assoc-ref opts 'system))
               (drv  (filter-map (match-lambda
                                  (('expression . exp)
                                   (derivations-from-package-expressions exp sys
                                                                         src?))
                                  (('argument . (? derivation-path? drv))
                                   drv)
                                  (('argument . (? string? x))
                                   (let ((p (find-package x)))
                                     (if src?
                                         (let ((s (package-source p)))
                                           (package-source-derivation
                                            (%store) s))
                                         (package-derivation (%store) p sys))))
                                  (_ #f))
                                 opts))
               (roots (filter-map (match-lambda
                                   (('gc-root . root) root)
                                   (_ #f))
                                  opts)))

          (show-what-to-build (%store) drv (assoc-ref opts 'dry-run?))

          ;; TODO: Add more options.
          (set-build-options (%store)
                             #:keep-failed? (assoc-ref opts 'keep-failed?)
                             #:build-cores (or (assoc-ref opts 'cores) 0)
                             #:use-substitutes? (assoc-ref opts 'substitutes?)
                             #:verbosity (assoc-ref opts 'verbosity))

          (if (assoc-ref opts 'derivations-only?)
              (begin
                (format #t "~{~a~%~}" drv)
                (for-each (cut register-root <> <>)
                          (map list drv) roots))
              (or (assoc-ref opts 'dry-run?)
                  (and (build-derivations (%store) drv)
                       (for-each (lambda (d)
                                   (let ((drv (call-with-input-file d
                                                read-derivation)))
                                     (format #t "~{~a~%~}"
                                             (map (match-lambda
                                                   ((out-name . out)
                                                    (derivation-path->output-path
                                                     d out-name)))
                                                  (derivation-outputs drv)))))
                                 drv)
                       (for-each (cut register-root <> <>)
                                 (map (lambda (drv)
                                        (map cdr
                                             (derivation-path->output-paths drv)))
                                      drv)
                                 roots)))))))))
