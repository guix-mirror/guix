;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix gexp)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-37)
  #:autoload   (gnu packages) (specification->package %package-module-path)
  #:autoload   (guix download) (download-to-store)
  #:export (%standard-build-options
            set-build-options-from-command-line
            show-build-options-help

            guix-build))

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

(define (package-with-source store p uri)
  "Return a package based on P but with its source taken from URI.  Extract
the new package's version number from URI."
  (define (numeric-extension? file-name)
    ;; Return true if FILE-NAME ends with digits.
    (string-every char-set:hex-digit (file-extension file-name)))

  (define (tarball-base-name file-name)
    ;; Return the "base" of FILE-NAME, removing '.tar.gz' or similar
    ;; extensions.
    ;; TODO: Factorize.
    (cond ((numeric-extension? file-name)
           file-name)
          ((string=? (file-extension file-name) "tar")
           (file-sans-extension file-name))
          (else
           (tarball-base-name (file-sans-extension file-name)))))

  (let ((base (tarball-base-name (basename uri))))
    (let-values (((name version)
                  (package-name->name+version base)))
      (package (inherit p)
               (version (or version (package-version p)))
               (source (download-to-store store uri))))))


;;;
;;; Standard command-line build options.
;;;

(define (show-build-options-help)
  "Display on the current output port help about the standard command-line
options handled by 'set-build-options-from-command-line', and listed in
'%standard-build-options'."
  (display (_ "
  -L, --load-path=DIR    prepend DIR to the package module search path"))
  (display (_ "
  -K, --keep-failed      keep build tree of failed builds"))
  (display (_ "
  -n, --dry-run          do not build the derivations"))
  (display (_ "
      --fallback         fall back to building when the substituter fails"))
  (display (_ "
      --no-substitutes   build instead of resorting to pre-built substitutes"))
  (display (_ "
      --no-build-hook    do not attempt to offload builds via the build hook"))
  (display (_ "
      --max-silent-time=SECONDS
                         mark the build as failed after SECONDS of silence"))
  (display (_ "
      --timeout=SECONDS  mark the build as failed after SECONDS of activity"))
  (display (_ "
      --verbosity=LEVEL  use the given verbosity LEVEL"))
  (display (_ "
  -c, --cores=N          allow the use of up to N CPU cores for the build"))
  (display (_ "
  -M, --max-jobs=N       allow at most N build jobs")))

(define (set-build-options-from-command-line store opts)
  "Given OPTS, an alist as returned by 'args-fold' given
'%standard-build-options', set the corresponding build options on STORE."
  ;; TODO: Add more options.
  (set-build-options store
                     #:keep-failed? (assoc-ref opts 'keep-failed?)
                     #:build-cores (or (assoc-ref opts 'cores) 0)
                     #:max-build-jobs (or (assoc-ref opts 'max-jobs) 1)
                     #:fallback? (assoc-ref opts 'fallback?)
                     #:use-substitutes? (assoc-ref opts 'substitutes?)
                     #:use-build-hook? (assoc-ref opts 'build-hook?)
                     #:max-silent-time (assoc-ref opts 'max-silent-time)
                     #:timeout (assoc-ref opts 'timeout)
                     #:print-build-trace (assoc-ref opts 'print-build-trace?)
                     #:verbosity (assoc-ref opts 'verbosity)))

(define %standard-build-options
  ;; List of standard command-line options for tools that build something.
  (list (option '(#\L "load-path") #t #f
                (lambda (opt name arg result . rest)
                  ;; XXX: Imperatively modify the search paths.
                  (%package-module-path (cons arg (%package-module-path)))
                  (set! %load-path (cons arg %load-path))
                  (set! %load-compiled-path (cons arg %load-compiled-path))

                  (apply values (cons result rest))))
        (option '(#\K "keep-failed") #f #f
                (lambda (opt name arg result . rest)
                  (apply values
                         (alist-cons 'keep-failed? #t result)
                         rest)))
        (option '("fallback") #f #f
                (lambda (opt name arg result . rest)
                  (apply values
                         (alist-cons 'fallback? #t
                                     (alist-delete 'fallback? result))
                         rest)))
        (option '("no-substitutes") #f #f
                (lambda (opt name arg result . rest)
                  (apply values
                         (alist-cons 'substitutes? #f
                                     (alist-delete 'substitutes? result))
                         rest)))
        (option '("no-build-hook") #f #f
                (lambda (opt name arg result . rest)
                  (apply values
                         (alist-cons 'build-hook? #f
                                     (alist-delete 'build-hook? result))
                         rest)))
        (option '("max-silent-time") #t #f
                (lambda (opt name arg result . rest)
                  (apply values
                         (alist-cons 'max-silent-time (string->number* arg)
                                     result)
                         rest)))
        (option '("timeout") #t #f
                (lambda (opt name arg result . rest)
                  (apply values
                         (alist-cons 'timeout (string->number* arg) result)
                         rest)))
        (option '("verbosity") #t #f
                (lambda (opt name arg result . rest)
                  (let ((level (string->number arg)))
                    (apply values
                           (alist-cons 'verbosity level
                                       (alist-delete 'verbosity result))
                           rest))))
        (option '(#\c "cores") #t #f
                (lambda (opt name arg result . rest)
                  (let ((c (false-if-exception (string->number arg))))
                    (if c
                        (apply values (alist-cons 'cores c result) rest)
                        (leave (_ "not a number: '~a' option argument: ~a~%")
                               name arg)))))
        (option '(#\M "max-jobs") #t #f
                (lambda (opt name arg result . rest)
                  (let ((c (false-if-exception (string->number arg))))
                    (if c
                        (apply values (alist-cons 'max-jobs c result) rest)
                        (leave (_ "not a number: '~a' option argument: ~a~%")
                               name arg)))))))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  `((system . ,(%current-system))
    (graft? . #t)
    (substitutes? . #t)
    (build-hook? . #t)
    (print-build-trace? . #t)
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
      --with-source=SOURCE
                         use SOURCE when building the corresponding package"))
  (display (_ "
      --no-grafts        do not graft packages"))
  (display (_ "
  -d, --derivations      return the derivation paths of the given packages"))
  (display (_ "
  -r, --root=FILE        make FILE a symlink to the result, and register it
                         as a garbage collector root"))
  (display (_ "
      --log-file         return the log file names for the given derivations"))
  (newline)
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
         (option '(#\n "dry-run") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'dry-run? #t result)))
         (option '(#\r "root") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'gc-root arg result)))
         (option '("log-file") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'log-file? #t result)))
         (option '("with-source") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'with-source arg result)))
         (option '("no-grafts") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'graft? #f
                               (alist-delete 'graft? result eq?))))

         %standard-build-options))

(define (options->derivations store opts)
  "Given OPTS, the result of 'args-fold', return a list of derivations to
build."
  (define package->derivation
    (match (assoc-ref opts 'target)
      (#f package-derivation)
      (triplet
       (cut package-cross-derivation <> <> triplet <>))))

  (define src?   (assoc-ref opts 'source?))
  (define sys    (assoc-ref opts 'system))
  (define graft? (assoc-ref opts 'graft?))

  (parameterize ((%graft? graft?))
    (let ((opts (options/with-source store
                                     (options/resolve-packages store opts))))
      (filter-map (match-lambda
                   (('argument . (? package? p))
                    (if src?
                        (let ((s (package-source p)))
                          (package-source-derivation store s))
                        (package->derivation store p sys)))
                   (('argument . (? derivation? drv))
                    drv)
                   (('argument . (? derivation-path? drv))
                    (call-with-input-file drv read-derivation))
                   (('argument . (? store-path?))
                    ;; Nothing to do; maybe for --log-file.
                    #f)
                   (_ #f))
                  opts))))

(define (options/resolve-packages store opts)
  "Return OPTS with package specification strings replaced by actual
packages."
  (define system
    (or (assoc-ref opts 'system) (%current-system)))

  (map (match-lambda
        (('argument . (? string? spec))
         (if (store-path? spec)
             `(argument . ,spec)
             `(argument . ,(specification->package spec))))
        (('expression . str)
         (match (read/eval str)
           ((? package? p)
            `(argument . ,p))
           ((? procedure? proc)
            (let ((drv (run-with-store store (proc) #:system system)))
              `(argument . ,drv)))
           ((? gexp? gexp)
            (let ((drv (run-with-store store
                         (gexp->derivation "gexp" gexp
                                           #:system system))))
              `(argument . ,drv)))))
        (opt opt))
       opts))

(define (options/with-source store opts)
  "Process with 'with-source' options in OPTS, replacing the relevant package
arguments with packages that use the specified source."
  (define new-sources
    (filter-map (match-lambda
                 (('with-source . uri)
                  (cons (package-name->name+version (basename uri))
                        uri))
                 (_ #f))
                opts))

  (let loop ((opts    opts)
             (sources new-sources)
             (result  '()))
    (match opts
      (()
       (unless (null? sources)
         (warning (_ "sources do not match any package:~{ ~a~}~%")
                  (match sources
                    (((name . uri) ...)
                     uri))))
       (reverse result))
      ((('argument . (? package? p)) tail ...)
       (let ((source (assoc-ref sources (package-name p))))
         (loop tail
               (alist-delete (package-name p) sources)
               (alist-cons 'argument
                           (if source
                               (package-with-source store p source)
                               p)
                           result))))
      ((('with-source . _) tail ...)
       (loop tail sources result))
      ((head tail ...)
       (loop tail sources (cons head result))))))


;;;
;;; Entry point.
;;;

(define (guix-build . args)
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

        (set-build-options-from-command-line store opts)
        (unless (assoc-ref opts 'log-file?)
          (show-what-to-build store drv
                              #:use-substitutes? (assoc-ref opts 'substitutes?)
                              #:dry-run? (assoc-ref opts 'dry-run?)))

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
