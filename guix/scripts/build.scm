;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix scripts)
  #:autoload   (guix import json) (json->scheme-file)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix memoization)
  #:use-module (guix grafts)

  #:use-module (guix utils)

  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:use-module (guix profiles)
  #:use-module (guix diagnostics)
  #:autoload   (guix http-client) (http-fetch http-get-error?)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-37)
  #:use-module (gnu packages)
  #:use-module ((guix status) #:select (with-status-verbosity))
  #:use-module ((guix progress) #:select (current-terminal-columns))
  #:use-module ((guix build syscalls) #:select (terminal-columns))
  #:use-module (guix transformations)
  #:export (log-url

            %standard-build-options
            set-build-options-from-command-line
            set-build-options-from-command-line*
            show-build-options-help

            guix-build
            register-root
            register-root*))

(define %default-log-urls
  ;; Default base URLs for build logs.
  '("http://ci.guix.gnu.org/log"))

;; XXX: The following procedure cannot be in (guix store) because of the
;; dependency on (guix derivations).
(define* (log-url store file #:key (base-urls %default-log-urls))
  "Return a URL under one of the BASE-URLS where a build log for FILE can be
found.  Return #f if no build log was found."
  (define (valid-url? url)
    ;; Probe URL and return #t if it is accessible.
    (catch #t
      (lambda ()
        (guard (c ((http-get-error? c) #f))
          (close-port (http-fetch url #:buffered? #f))
          #t))
      (match-lambda*
        (('getaddrinfo-error . _)
         #f)
        (('tls-certificate-error args ...)
         (report-error (G_ "cannot access build log at '~a':~%") url)
         (print-exception (current-error-port) #f
                          'tls-certificate-error args)
         (exit 1))
        ((key . args)
         (apply throw key args)))))

  (define (find-url file)
    (let ((base (basename file)))
      (any (lambda (base-url)
             (let ((url (string-append base-url "/" base)))
               (and (valid-url? url) url)))
           base-urls)))

  (cond ((derivation-path? file)
         (catch 'system-error
           (lambda ()
             ;; Usually we'll have more luck with the output file name since
             ;; the deriver that was used by the server could be different, so
             ;; try one of the output file names.
             (let ((drv (read-derivation-from-file file)))
               (or (find-url (derivation->output-path drv))
                   (find-url file))))
           (lambda args
             ;; As a last resort, try the .drv.
             (if (= ENOENT (system-error-errno args))
                 (find-url file)
                 (apply throw args)))))
        (else
         (find-url file))))

(define (register-root store paths root)
  "Register ROOT as an indirect GC root for all of PATHS."
  (let* ((root (if (string-prefix? "/" root)
                   root
                   (string-append (canonicalize-path (dirname root))
                                  "/" (basename root)))))
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
        (leave (G_ "failed to create GC root `~a': ~a~%")
               root (strerror (system-error-errno args)))))))

(define register-root*
  (store-lift register-root))


;;;
;;; Standard command-line build options.
;;;

(define (show-build-options-help)
  "Display on the current output port help about the standard command-line
options handled by 'set-build-options-from-command-line', and listed in
'%standard-build-options'."
  (display (G_ "
  -L, --load-path=DIR    prepend DIR to the package module search path"))
  (display (G_ "
  -K, --keep-failed      keep build tree of failed builds"))
  (display (G_ "
  -k, --keep-going       keep going when some of the derivations fail"))
  (display (G_ "
  -n, --dry-run          do not build the derivations"))
  (display (G_ "
      --fallback         fall back to building when the substituter fails"))
  (display (G_ "
      --no-substitutes   build instead of resorting to pre-built substitutes"))
  (display (G_ "
      --substitute-urls=URLS
                         fetch substitute from URLS if they are authorized"))
  (display (G_ "
      --no-grafts        do not graft packages"))
  (display (G_ "
      --no-offload       do not attempt to offload builds"))
  (display (G_ "
      --max-silent-time=SECONDS
                         mark the build as failed after SECONDS of silence"))
  (display (G_ "
      --timeout=SECONDS  mark the build as failed after SECONDS of activity"))
  (display (G_ "
      --rounds=N         build N times in a row to detect non-determinism"))
  (display (G_ "
  -c, --cores=N          allow the use of up to N CPU cores for the build"))
  (display (G_ "
  -M, --max-jobs=N       allow at most N build jobs"))
  (display (G_ "
      --debug=LEVEL      produce debugging output at LEVEL")))

(define (set-build-options-from-command-line store opts)
  "Given OPTS, an alist as returned by 'args-fold' given
'%standard-build-options', set the corresponding build options on STORE."

  ;; '--keep-failed' has no effect when talking to a remote daemon.  Catch the
  ;; case where GUIX_DAEMON_SOCKET=guix://….
  (when (and (assoc-ref opts 'keep-failed?)
             (let* ((socket (store-connection-socket store))
                    (peer   (catch 'system-error
                              (lambda ()
                                (and (file-port? socket)
                                     (getpeername socket)))
                              (const #f))))
               (and peer (not (= AF_UNIX (sockaddr:fam peer))))))
    (warning (G_ "'--keep-failed' ignored since you are \
talking to a remote daemon\n")))

  (set-build-options store
                     #:keep-failed? (assoc-ref opts 'keep-failed?)
                     #:keep-going? (assoc-ref opts 'keep-going?)
                     #:rounds (assoc-ref opts 'rounds)
                     #:build-cores (assoc-ref opts 'cores)
                     #:max-build-jobs (assoc-ref opts 'max-jobs)
                     #:fallback? (assoc-ref opts 'fallback?)
                     #:use-substitutes? (assoc-ref opts 'substitutes?)
                     #:substitute-urls (assoc-ref opts 'substitute-urls)
                     #:offload? (and (assoc-ref opts 'offload?)
                                     (not (assoc-ref opts 'keep-failed?)))
                     #:max-silent-time (assoc-ref opts 'max-silent-time)
                     #:timeout (assoc-ref opts 'timeout)
                     #:print-build-trace (assoc-ref opts 'print-build-trace?)
                     #:print-extended-build-trace?
                     (assoc-ref opts 'print-extended-build-trace?)
                     #:multiplexed-build-output?
                     (assoc-ref opts 'multiplexed-build-output?)
                     #:verbosity (assoc-ref opts 'debug)))

(define set-build-options-from-command-line*
  (store-lift set-build-options-from-command-line))

(define %standard-build-options
  ;; List of standard command-line options for tools that build something.
  (list (option '(#\L "load-path") #t #f
                (lambda (opt name arg result . rest)
                  ;; XXX: Imperatively modify the search paths.
                  (%package-module-path (cons arg (%package-module-path)))
                  (%patch-path (cons arg (%patch-path)))
                  (set! %load-path (cons arg %load-path))
                  (set! %load-compiled-path (cons arg %load-compiled-path))

                  (apply values (cons result rest))))
        (option '(#\K "keep-failed") #f #f
                (lambda (opt name arg result . rest)
                  (apply values
                         (alist-cons 'keep-failed? #t result)
                         rest)))
        (option '(#\k "keep-going") #f #f
                (lambda (opt name arg result . rest)
                  (apply values
                         (alist-cons 'keep-going? #t result)
                         rest)))
        (option '("rounds") #t #f
                (lambda (opt name arg result . rest)
                  (apply values
                         (alist-cons 'rounds (string->number* arg)
                                     result)
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
        (option '("substitute-urls") #t #f
                (lambda (opt name arg result . rest)
                  (apply values
                         (alist-cons 'substitute-urls
                                     (string-tokenize arg)
                                     (alist-delete 'substitute-urls result))
                         rest)))
        (option '("no-grafts") #f #f
                (lambda (opt name arg result . rest)
                  (apply values
                         (alist-cons 'graft? #f
                                     (alist-delete 'graft? result eq?))
                         rest)))
        (option '("no-offload" "no-build-hook") #f #f
                (lambda (opt name arg result . rest)
                  (when (string=? name "no-build-hook")
                    (warning (G_ "'--no-build-hook' is deprecated; \
use '--no-offload' instead~%")))

                  (apply values
                         (alist-cons 'offload? #f
                                     (alist-delete 'offload? result))
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
        (option '("debug") #t #f
                (lambda (opt name arg result . rest)
                  (let ((level (string->number* arg)))
                    (apply values
                           (alist-cons 'debug level
                                       (alist-delete 'debug result))
                           rest))))
        (option '(#\c "cores") #t #f
                (lambda (opt name arg result . rest)
                  (let ((c (false-if-exception (string->number arg))))
                    (if c
                        (apply values (alist-cons 'cores c result) rest)
                        (leave (G_ "not a number: '~a' option argument: ~a~%")
                               name arg)))))
        (option '(#\M "max-jobs") #t #f
                (lambda (opt name arg result . rest)
                  (let ((c (false-if-exception (string->number arg))))
                    (if c
                        (apply values (alist-cons 'max-jobs c result) rest)
                        (leave (G_ "not a number: '~a' option argument: ~a~%")
                               name arg)))))))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  `((build-mode . ,(build-mode normal))
    (graft? . #t)
    (substitutes? . #t)
    (offload? . #t)
    (print-build-trace? . #t)
    (print-extended-build-trace? . #t)
    (multiplexed-build-output? . #t)
    (verbosity . 3)
    (debug . 0)))

(define (show-help)
  (display (G_ "Usage: guix build [OPTION]... PACKAGE-OR-DERIVATION...
Build the given PACKAGE-OR-DERIVATION and return their output paths.\n"))
  (display (G_ "
  -e, --expression=EXPR  build the package or derivation EXPR evaluates to"))
  (display (G_ "
  -f, --file=FILE        build the package or derivation that the code within
                         FILE evaluates to"))
  (display (G_ "
  -m, --manifest=FILE    build the packages that the manifest given in FILE
                         evaluates to"))
  (display (G_ "
  -S, --source           build the packages' source derivations"))
  (display (G_ "
      --sources[=TYPE]   build source derivations; TYPE may optionally be one
                         of \"package\", \"all\" (default), or \"transitive\""))
  (display (G_ "
  -s, --system=SYSTEM    attempt to build for SYSTEM--e.g., \"i686-linux\""))
  (display (G_ "
      --target=TRIPLET   cross-build for TRIPLET--e.g., \"armel-linux-gnu\""))
  (display (G_ "
  -d, --derivations      return the derivation paths of the given packages"))
  (display (G_ "
      --check            rebuild items to check for non-determinism issues"))
  (display (G_ "
      --repair           repair the specified items"))
  (display (G_ "
  -r, --root=FILE        make FILE a symlink to the result, and register it
                         as a garbage collector root"))
  (display (G_ "
  -v, --verbosity=LEVEL  use the given verbosity LEVEL"))
  (display (G_ "
  -q, --quiet            do not show the build log"))
  (display (G_ "
      --log-file         return the log file names for the given derivations"))
  (newline)
  (show-build-options-help)
  (newline)
  (show-transformation-options-help)
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
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
                   (alist-cons 'source #t result)))
         (option '("sources") #f #t
                 (lambda (opt name arg result)
                   (match arg
                     ("package"
                      (alist-cons 'source #t result))
                     ((or "all" #f)
                      (alist-cons 'source package-direct-sources result))
                     ("transitive"
                      (alist-cons 'source package-transitive-sources result))
                     (else
                      (leave (G_ "invalid argument: '~a' option argument: ~a, ~
must be one of 'package', 'all', or 'transitive'~%")
                             name arg)))))
         (option '("check") #f #f
                 (lambda (opt name arg result . rest)
                   (apply values
                          (alist-cons 'build-mode (build-mode check)
                                      result)
                          rest)))
         (option '("repair") #f #f
                 (lambda (opt name arg result . rest)
                   (apply values
                          (alist-cons 'build-mode (build-mode repair)
                                      result)
                          rest)))
         (option '(#\s "system") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'system arg result)))
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
         (option '(#\f "file") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'file arg result)))
         (option '(#\m "manifest") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'manifest arg result)))
         (option '(#\n "dry-run") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'dry-run? #t result)))
         (option '(#\r "root") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'gc-root arg result)))
         (option '(#\v "verbosity") #t #f
                 (lambda (opt name arg result)
                   (let ((level (string->number* arg)))
                     (alist-cons 'verbosity level
                                 (alist-delete 'verbosity result)))))
         (option '(#\q "quiet") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'verbosity 0
                               (alist-delete 'verbosity result))))
         (option '("log-file") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'log-file? #t result)))

         (append %transformation-options
                 %standard-build-options)))

(define (options->things-to-build opts)
  "Read the arguments from OPTS and return a list of high-level objects to
build---packages, gexps, derivations, and so on."
  (define (validate-type x)
    (unless (or (derivation? x) (file-like? x) (gexp? x) (procedure? x))
      (raise (make-compound-condition
               (formatted-message (G_ "~s: not something we can build~%") x)
               (condition
                (&fix-hint
                 (hint
                   (if (unspecified? x)
                       (G_ "If you build from a file, make sure the last Scheme
expression returns a package value.  @code{define-public} defines a variable,
but returns @code{#<unspecified>}.  To fix this, add a Scheme expression at
the end of the file that consists only of the package's variable name you
defined, as in this example:

@example
(define-public my-package
  (package
    ...))

my-package
@end example")
                       (G_ "If you build from a file, make sure the last
Scheme expression returns a package, gexp, derivation or a list of such
values.")))))))))

  (define (ensure-list x)
    (let ((lst (match x
                 ((x ...) x)
                 (x       (list x)))))
      (for-each validate-type lst)
      lst))

  (append-map (match-lambda
                (('argument . (? string? spec))
                 (cond ((derivation-path? spec)
                        (catch 'system-error
                          (lambda ()
                            ;; Ask for absolute file names so that .drv file
                            ;; names passed from the user to 'read-derivation'
                            ;; are absolute when it returns.
                            (let ((spec (canonicalize-path spec)))
                              (list (read-derivation-from-file spec))))
                          (lambda args
                            ;; Non-existent .drv files can be substituted down
                            ;; the road, so don't error out.
                            (if (= ENOENT (system-error-errno args))
                                '()
                                (apply throw args)))))
                       ((store-path? spec)
                        ;; Nothing to do; maybe for --log-file.
                        '())
                       (else
                        (list (specification->package spec)))))
                (('file . file)
                 (let ((file (or (and (string-suffix? ".json" file)
                                      (json->scheme-file file))
                                 file)))
                   (ensure-list (load* file (make-user-module '())))))
                (('manifest . manifest)
                 (map manifest-entry-item
                      (manifest-entries
                       (load* manifest
                              (make-user-module '((guix profiles) (gnu)))))))
                (('expression . str)
                 (ensure-list (read/eval str)))
                (('argument . (? derivation? drv))
                 drv)
                (_ '()))
              opts))

(define (options->derivations store opts)
  "Given OPTS, the result of 'args-fold', return a list of derivations to
build."
  (define transform
    (options->transformation opts))

  (define package->derivation
    (match (assoc-ref opts 'target)
      (#f package-derivation)
      (triplet
       (cut package-cross-derivation <> <> triplet <>))))

  (define src    (assoc-ref opts 'source))
  (define graft? (assoc-ref opts 'graft?))
  (define systems
    (match (filter-map (match-lambda
                         (('system . system) system)
                         (_ #f))
                       opts)
      (()      (list (%current-system)))
      (systems systems)))

  (define things-to-build
    (map transform (options->things-to-build opts)))

  (define (compute-derivation obj system)
    ;; Compute the derivation of OBJ for SYSTEM.
    (match obj
      ((? package? p)
       (let ((p (or (and graft? (package-replacement p)) p)))
         (match src
           (#f
            (list (package->derivation store p system)))
           (#t
            (match (package-source p)
              (#f
               (warning (package-location p)
                        (G_ "package '~a' has no source~%")
                        (package-name p))
               '())
              (s
               (list (package-source-derivation store s)))))
           (proc
            (map (cut package-source-derivation store <>)
                 (proc p))))))
      ((? derivation? drv)
       (list drv))
      ((? procedure? proc)
       (list (run-with-store store
               (mbegin %store-monad
                 (set-guile-for-build (default-guile))
                 (proc))
               #:system system)))
      ((? file-like? obj)
       (list (run-with-store store
               (lower-object obj system
                             #:target (assoc-ref opts 'target))
               #:system system)))
      ((? gexp? gexp)
       (list (run-with-store store
               (mbegin %store-monad
                 (set-guile-for-build (default-guile))
                 (gexp->derivation "gexp" gexp
                                   #:system system))
               #:system system)))))

  ;; We may get 'unbound-variable' errors while evaluating the 'inputs' fields
  ;; of user packages.  Since 'guix build' is the primary tool for people
  ;; testing new packages, report such errors gracefully.
  (with-unbound-variable-handling
   (parameterize ((%graft? graft?))
     (append-map (lambda (system)
                   (concatenate
                    (map/accumulate-builds store
                                           (cut compute-derivation <> system)
                                           things-to-build)))
                 systems))))

(define (show-build-log store file urls)
  "Show the build log for FILE, falling back to remote logs from URLS if
needed."
  (let ((log (or (log-file store file)
                 (log-url store file #:base-urls urls))))
    (if log
        (format #t "~a~%" log)
        (leave (G_ "no build log for '~a'~%") file))))


;;;
;;; Entry point.
;;;

(define-command (guix-build . args)
  (category packaging)
  (synopsis "build packages or derivations without installing them")

  (define opts
    (parse-command-line args %options
                        (list %default-options)))

  (define graft?
    (assoc-ref opts 'graft?))

  (with-error-handling
    (with-status-verbosity (assoc-ref opts 'verbosity)
      (with-store store
        ;; Set the build options before we do anything else.
        (set-build-options-from-command-line store opts)

        (with-build-handler (build-notifier #:use-substitutes?
                                            (assoc-ref opts 'substitutes?)
                                            #:verbosity
                                            (assoc-ref opts 'verbosity)
                                            #:dry-run?
                                            (assoc-ref opts 'dry-run?))
          (parameterize ((current-terminal-columns (terminal-columns))

                         ;; Set grafting upfront in case the user's input
                         ;; depends on it (e.g., a manifest or code snippet that
                         ;; calls 'gexp->derivation').
                         (%graft?                  graft?))
            (let* ((mode  (assoc-ref opts 'build-mode))
                   (drv   (options->derivations store opts))
                   (urls  (map (cut string-append <> "/log")
                               (if (assoc-ref opts 'substitutes?)
                                   (or (assoc-ref opts 'substitute-urls)
                                       ;; XXX: This does not necessarily match the
                                       ;; daemon's substitute URLs.
                                       %default-substitute-urls)
                                   '())))
                   (items (filter-map (match-lambda
                                        (('argument . (? store-path? file))
                                         ;; If FILE is a .drv that's not in
                                         ;; store, keep it so that it can be
                                         ;; substituted.
                                         (and (or (not (derivation-path? file))
                                                  (not (file-exists? file)))
                                              file))
                                        (_ #f))
                                      opts))
                   (roots (filter-map (match-lambda
                                        (('gc-root . root) root)
                                        (_ #f))
                                      opts)))

              (cond ((assoc-ref opts 'log-file?)
                     ;; Pass 'show-build-log' the output file names, not the
                     ;; derivation file names, because there can be several
                     ;; derivations leading to the same output.
                     (for-each (cut show-build-log store <> urls)
                               (delete-duplicates
                                (append (map derivation->output-path drv)
                                        items))))
                    ((assoc-ref opts 'derivations-only?)
                     (format #t "~{~a~%~}" (map derivation-file-name drv))
                     (for-each (cut register-root store <> <>)
                               (map (compose list derivation-file-name) drv)
                               roots))
                    (else
                     (and (build-derivations store (append drv items)
                                             mode)
                          (for-each show-derivation-outputs drv)
                          (for-each (cut register-root store <> <>)
                                    (map (lambda (drv)
                                           (map cdr
                                                (derivation->output-paths drv)))
                                         drv)
                                    roots)))))))))))
