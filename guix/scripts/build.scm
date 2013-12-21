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
  #:use-module (guix monads)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-37)
  #:autoload   (gnu packages) (find-best-packages-by-name)
  #:export (guix-build))

(define (derivation-from-expression store str package-derivation
                                    system source?)
  "Read/eval STR and return the corresponding derivation path for SYSTEM.
When SOURCE? is true and STR evaluates to a package, return the derivation of
the package source; otherwise, use PACKAGE-DERIVATION to compute the
derivation of a package."
  (match (read/eval str)
    ((? package? p)
     (if source?
         (let ((source (package-source p)))
           (if source
               (package-source-derivation store source)
               (leave (_ "package `~a' has no source~%")
                      (package-name p))))
         (package-derivation store p system)))
    ((? procedure? proc)
     (run-with-store store (proc) #:system system))))

(define (specification->package spec)
  "Return a package matching SPEC.  SPEC may be a package name, or a package
name followed by a hyphen and a version number.  If the version number is not
present, return the preferred newest version."
  (let-values (((name version)
                (package-name->name+version spec)))
    (match (find-best-packages-by-name name version)
      ((p)                                      ; one match
       p)
      ((p x ...)                                ; several matches
       (warning (_ "ambiguous package specification `~a'~%") spec)
       (warning (_ "choosing ~a from ~a~%")
                (package-full-name p)
                (location->string (package-location p)))
       p)
      (_                                        ; no matches
       (if version
           (leave (_ "~A: package not found for version ~a~%")
                  name version)
           (leave (_ "~A: unknown package~%") name))))))

(define (register-root store paths root)
  "Register ROOT as an indirect GC root for all of PATHS."
  (let* ((root (string-append (canonicalize-path (dirname root))
                              "/" root)))
    (catch 'system-error
      (lambda ()
        (match paths
          ((path)
           (symlink path root)
           (add-indirect-root store root))
          ((paths ...)
           (fold (lambda (path count)
                   (let ((root (string-append root
                                              "-"
                                              (number->string count))))
                     (symlink path root)
                     (add-indirect-root store root))
                   (+ 1 count))
                 0
                 paths))))
      (lambda args
        (leave (_ "failed to create GC root `~a': ~a~%")
               root (strerror (system-error-errno args)))))))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  `((system . ,(%current-system))
    (substitutes? . #t)
    (max-silent-time . 3600)
    (verbosity . 0)))

(define (show-help)
  (display (_ "Usage: guix build [OPTION]... PACKAGE-OR-DERIVATION...
Build the given PACKAGE-OR-DERIVATION and return their output paths.\n"))
  (display (_ "
  -e, --expression=EXPR  build the package or derivation EXPR evaluates to"))
  (display (_ "
  -S, --source           build the packages' source derivations"))
  (display (_ "
  -s, --system=SYSTEM    attempt to build for SYSTEM--e.g., \"i686-linux\""))
  (display (_ "
      --target=TRIPLET   cross-build for TRIPLET--e.g., \"armel-linux-gnu\""))
  (display (_ "
  -d, --derivations      return the derivation paths of the given packages"))
  (display (_ "
  -K, --keep-failed      keep build tree of failed builds"))
  (display (_ "
  -n, --dry-run          do not build the derivations"))
  (display (_ "
      --fallback         fall back to building when the substituter fails"))
  (display (_ "
      --no-substitutes   build instead of resorting to pre-built substitutes"))
  (display (_ "
      --max-silent-time=SECONDS
                         mark the build as failed after SECONDS of silence"))
  (display (_ "
  -c, --cores=N          allow the use of up to N CPU cores for the build"))
  (display (_ "
  -r, --root=FILE        make FILE a symlink to the result, and register it
                         as a garbage collector root"))
  (display (_ "
      --verbosity=LEVEL  use the given verbosity LEVEL"))
  (display (_ "
      --log-file         return the log file names for the given derivations"))
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
        (option '("target") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'target arg
                              (alist-delete 'target result eq?))))
        (option '(#\d "derivations") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'derivations-only? #t result)))
        (option '(#\e "expression") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'expression arg result)))
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
        (option '("fallback") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'fallback? #t
                              (alist-delete 'fallback? result))))
        (option '("no-substitutes") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'substitutes? #f
                              (alist-delete 'substitutes? result))))
        (option '("max-silent-time") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'max-silent-time (string->number* arg)
                              result)))
        (option '(#\r "root") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'gc-root arg result)))
        (option '("verbosity") #t #f
                (lambda (opt name arg result)
                  (let ((level (string->number arg)))
                    (alist-cons 'verbosity level
                                (alist-delete 'verbosity result)))))
        (option '("log-file") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'log-file? #t result)))))

(define (options->derivations store opts)
  "Given OPTS, the result of 'args-fold', return a list of derivations to
build."
  (define package->derivation
    (match (assoc-ref opts 'target)
      (#f package-derivation)
      (triplet
       (cut package-cross-derivation <> <> triplet <>))))

  (define src? (assoc-ref opts 'source?))
  (define sys  (assoc-ref opts 'system))

  (filter-map (match-lambda
               (('expression . str)
                (derivation-from-expression store str package->derivation
                                            sys src?))
               (('argument . (? derivation-path? drv))
                (call-with-input-file drv read-derivation))
               (('argument . (? store-path?))
                ;; Nothing to do; maybe for --log-file.
                #f)
               (('argument . (? string? x))
                (let ((p (specification->package x)))
                  (if src?
                      (let ((s (package-source p)))
                        (package-source-derivation store s))
                      (package->derivation store p sys))))
               (_ #f))
              opts))


;;;
;;; Entry point.
;;;

(define (guix-build . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (args-fold* args %options
                (lambda (opt name arg result)
                  (leave (_ "~A: unrecognized option~%") name))
                (lambda (arg result)
                  (alist-cons 'argument arg result))
                %default-options))

  (with-error-handling
    ;; Ask for absolute file names so that .drv file names passed from the
    ;; user to 'read-derivation' are absolute when it returns.
    (with-fluids ((%file-port-name-canonicalization 'absolute))
      (let* ((opts  (parse-options))
             (store (open-connection))
             (drv   (options->derivations store opts))
             (roots (filter-map (match-lambda
                                 (('gc-root . root) root)
                                 (_ #f))
                                opts)))

        (unless (assoc-ref opts 'log-file?)
          (show-what-to-build store drv
                              #:use-substitutes? (assoc-ref opts 'substitutes?)
                              #:dry-run? (assoc-ref opts 'dry-run?)))

        ;; TODO: Add more options.
        (set-build-options store
                           #:keep-failed? (assoc-ref opts 'keep-failed?)
                           #:build-cores (or (assoc-ref opts 'cores) 0)
                           #:fallback? (assoc-ref opts 'fallback?)
                           #:use-substitutes? (assoc-ref opts 'substitutes?)
                           #:max-silent-time (assoc-ref opts 'max-silent-time)
                           #:verbosity (assoc-ref opts 'verbosity))

        (cond ((assoc-ref opts 'log-file?)
               (for-each (lambda (file)
                           (let ((log (log-file store file)))
                             (if log
                                 (format #t "~a~%" log)
                                 (leave (_ "no build log for '~a'~%")
                                        file))))
                         (delete-duplicates
                          (append (map derivation-file-name drv)
                                  (filter-map (match-lambda
                                               (('argument
                                                 . (? store-path? file))
                                                file)
                                               (_ #f))
                                              opts)))))
              ((assoc-ref opts 'derivations-only?)
               (format #t "~{~a~%~}" (map derivation-file-name drv))
               (for-each (cut register-root store <> <>)
                         (map (compose list derivation-file-name) drv)
                         roots))
              ((not (assoc-ref opts 'dry-run?))
               (and (build-derivations store drv)
                    (for-each (lambda (d)
                                (format #t "~{~a~%~}"
                                        (map (match-lambda
                                              ((out-name . out)
                                               (derivation->output-path
                                                d out-name)))
                                             (derivation-outputs d))))
                              drv)
                    (for-each (cut register-root store <> <>)
                              (map (lambda (drv)
                                     (map cdr
                                          (derivation->output-paths drv)))
                                   drv)
                              roots))))))))
