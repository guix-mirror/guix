;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix scripts)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix grafts)

  #:use-module (guix utils)

  ;; Use the procedure that destructures "NAME-VERSION" forms.
  #:use-module ((guix build utils)
                #:select ((package-name->name+version
                           . hyphen-package-name->name+version)))

  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:autoload   (guix http-client) (http-fetch http-get-error?)
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
  #:autoload   (guix git-download) (git-reference?)
  #:autoload   (guix git) (git-checkout?)
  #:use-module ((guix status) #:select (with-status-verbosity))
  #:use-module ((guix progress) #:select (current-terminal-columns))
  #:use-module ((guix build syscalls) #:select (terminal-columns))
  #:export (%standard-build-options
            set-build-options-from-command-line
            set-build-options-from-command-line*
            show-build-options-help

            %transformation-options
            options->transformation
            show-transformation-options-help

            guix-build
            register-root
            register-root*))

(define %default-log-urls
  ;; Default base URLs for build logs.
  '("http://ci.guix.info/log"))

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

(define (numeric-extension? file-name)
  "Return true if FILE-NAME ends with digits."
  (string-every char-set:hex-digit (file-extension file-name)))

(define (tarball-base-name file-name)
  "Return the \"base\" of FILE-NAME, removing '.tar.gz' or similar
extensions."
  ;; TODO: Factorize.
  (cond ((not (file-extension file-name))
         file-name)
        ((numeric-extension? file-name)
         file-name)
        ((string=? (file-extension file-name) "tar")
         (file-sans-extension file-name))
        ((file-extension file-name)
         =>
         (match-lambda
           ("scm" file-name)
           (else  (tarball-base-name (file-sans-extension file-name)))))
        (else
         file-name)))

(define* (package-with-source store p uri #:optional version)
  "Return a package based on P but with its source taken from URI.  Extract
the new package's version number from URI."
  (let ((base (tarball-base-name (basename uri))))
    (let-values (((_ version*)
                  (hyphen-package-name->name+version base)))
      (package (inherit p)
               (version (or version version*
                            (package-version p)))

               ;; Use #:recursive? #t to allow for directories.
               (source (download-to-store store uri
                                          #:recursive? #t))

               ;; Override the replacement, otherwise '--with-source' would
               ;; have no effect.
               (replacement #f)))))


;;;
;;; Transformations.
;;;

(define (transform-package-source sources)
  "Return a transformation procedure that replaces package sources with the
matching URIs given in SOURCES."
  (define new-sources
    (map (lambda (uri)
           (match (string-index uri #\=)
             (#f
              ;; Determine the package name and version from URI.
              (call-with-values
                  (lambda ()
                    (hyphen-package-name->name+version
                     (tarball-base-name (basename uri))))
                (lambda (name version)
                  (list name version uri))))
             (index
              ;; What's before INDEX is a "PKG@VER" or "PKG" spec.
              (call-with-values
                  (lambda ()
                    (package-name->name+version (string-take uri index)))
                (lambda (name version)
                  (list name version
                        (string-drop uri (+ 1 index))))))))
         sources))

  (lambda (store obj)
    (let loop ((sources  new-sources)
               (result   '()))
      (match obj
        ((? package? p)
         (match (assoc-ref sources (package-name p))
           ((version source)
            (package-with-source store p source version))
           (#f
            p)))
        (_
         obj)))))

(define (evaluate-replacement-specs specs proc)
  "Parse SPECS, a list of strings like \"guile=guile@2.1\" and return a list
of package spec/procedure pairs as expected by 'package-input-rewriting/spec'.
PROC is called with the package to be replaced and its replacement according
to SPECS.  Raise an error if an element of SPECS uses invalid syntax, or if a
package it refers to could not be found."
  (define not-equal
    (char-set-complement (char-set #\=)))

  (map (lambda (spec)
         (match (string-tokenize spec not-equal)
           ((spec new)
            (cons spec
                  (let ((new (specification->package new)))
                    (lambda (old)
                      (proc old new)))))
           (x
            (leave (G_ "invalid replacement specification: ~s~%") spec))))
       specs))

(define (transform-package-inputs replacement-specs)
  "Return a procedure that, when passed a package, replaces its direct
dependencies according to REPLACEMENT-SPECS.  REPLACEMENT-SPECS is a list of
strings like \"guile=guile@2.1\" meaning that, any dependency on a package
called \"guile\" must be replaced with a dependency on a version 2.1 of
\"guile\"."
  (let* ((replacements (evaluate-replacement-specs replacement-specs
                                                   (lambda (old new)
                                                     new)))
         (rewrite      (package-input-rewriting/spec replacements)))
    (lambda (store obj)
      (if (package? obj)
          (rewrite obj)
          obj))))

(define (transform-package-inputs/graft replacement-specs)
  "Return a procedure that, when passed a package, replaces its direct
dependencies according to REPLACEMENT-SPECS.  REPLACEMENT-SPECS is a list of
strings like \"gnutls=gnutls@3.5.4\" meaning that packages are built using the
current 'gnutls' package, after which version 3.5.4 is grafted onto them."
  (define (set-replacement old new)
    (package (inherit old) (replacement new)))

  (let* ((replacements (evaluate-replacement-specs replacement-specs
                                                   set-replacement))
         (rewrite      (package-input-rewriting/spec replacements)))
    (lambda (store obj)
      (if (package? obj)
          (rewrite obj)
          obj))))

(define %not-equal
  (char-set-complement (char-set #\=)))

(define (package-git-url package)
  "Return the URL of the Git repository for package, or raise an error if
the source of PACKAGE is not fetched from a Git repository."
  (let ((source (package-source package)))
    (cond ((and (origin? source)
                (git-reference? (origin-uri source)))
           (git-reference-url (origin-uri source)))
          ((git-checkout? source)
           (git-checkout-url source))
          (else
           (leave (G_ "the source of ~a is not a Git reference~%")
                  (package-full-name package))))))

(define (evaluate-git-replacement-specs specs proc)
  "Parse SPECS, a list of strings like \"guile=stable-2.2\", and return a list
of package pairs, where (PROC PACKAGE URL BRANCH-OR-COMMIT) returns the
replacement package.  Raise an error if an element of SPECS uses invalid
syntax, or if a package it refers to could not be found."
  (map (lambda (spec)
         (match (string-tokenize spec %not-equal)
           ((spec branch-or-commit)
            (define (replace old)
              (let* ((source (package-source old))
                     (url    (package-git-url old)))
                (proc old url branch-or-commit)))

            (cons spec replace))
           (x
            (leave (G_ "invalid replacement specification: ~s~%") spec))))
       specs))

(define (transform-package-source-branch replacement-specs)
  "Return a procedure that, when passed a package, replaces its direct
dependencies according to REPLACEMENT-SPECS.  REPLACEMENT-SPECS is a list of
strings like \"guile-next=stable-3.0\" meaning that packages are built using
'guile-next' from the latest commit on its 'stable-3.0' branch."
  (define (replace old url branch)
    (package
      (inherit old)
      (version (string-append "git." (string-map (match-lambda
                                                   (#\/ #\-)
                                                   (chr chr))
                                                 branch)))
      (source (git-checkout (url url) (branch branch)
                            (recursive? #t)))))

  (let* ((replacements (evaluate-git-replacement-specs replacement-specs
                                                       replace))
         (rewrite      (package-input-rewriting/spec replacements)))
    (lambda (store obj)
      (if (package? obj)
          (rewrite obj)
          obj))))

(define (transform-package-source-commit replacement-specs)
  "Return a procedure that, when passed a package, replaces its direct
dependencies according to REPLACEMENT-SPECS.  REPLACEMENT-SPECS is a list of
strings like \"guile-next=cabba9e\" meaning that packages are built using
'guile-next' from commit 'cabba9e'."
  (define (replace old url commit)
    (package
      (inherit old)
      (version (string-append "git."
                              (if (< (string-length commit) 7)
                                  commit
                                  (string-take commit 7))))
      (source (git-checkout (url url) (commit commit)
                            (recursive? #t)))))

  (let* ((replacements (evaluate-git-replacement-specs replacement-specs
                                                       replace))
         (rewrite      (package-input-rewriting/spec replacements)))
    (lambda (store obj)
      (if (package? obj)
          (rewrite obj)
          obj))))

(define (transform-package-source-git-url replacement-specs)
  "Return a procedure that, when passed a package, replaces its dependencies
according to REPLACEMENT-SPECS.  REPLACEMENT-SPECS is a list of strings like
\"guile-json=https://gitthing.com/…\" meaning that packages are built using
a checkout of the Git repository at the given URL."
  (define replacements
    (map (lambda (spec)
           (match (string-tokenize spec %not-equal)
             ((spec url)
              (cons spec
                    (lambda (old)
                      (package
                        (inherit old)
                        (source (git-checkout (url url)
                                              (recursive? #t)))))))))
         replacement-specs))

  (define rewrite
    (package-input-rewriting/spec replacements))

  (lambda (store obj)
    (if (package? obj)
        (rewrite obj)
        obj)))

(define %transformations
  ;; Transformations that can be applied to things to build.  The car is the
  ;; key used in the option alist, and the cdr is the transformation
  ;; procedure; it is called with two arguments: the store, and a list of
  ;; things to build.
  `((with-source . ,transform-package-source)
    (with-input  . ,transform-package-inputs)
    (with-graft  . ,transform-package-inputs/graft)
    (with-branch . ,transform-package-source-branch)
    (with-commit . ,transform-package-source-commit)
    (with-git-url . ,transform-package-source-git-url)))

(define %transformation-options
  ;; The command-line interface to the above transformations.
  (let ((parser (lambda (symbol)
                  (lambda (opt name arg result . rest)
                    (apply values
                           (alist-cons symbol arg result)
                           rest)))))
    (list (option '("with-source") #t #f
                  (parser 'with-source))
          (option '("with-input") #t #f
                  (parser 'with-input))
          (option '("with-graft") #t #f
                  (parser 'with-graft))
          (option '("with-branch") #t #f
                  (parser 'with-branch))
          (option '("with-commit") #t #f
                  (parser 'with-commit))
          (option '("with-git-url") #t #f
                  (parser 'with-git-url)))))

(define (show-transformation-options-help)
  (display (G_ "
      --with-source=SOURCE
                         use SOURCE when building the corresponding package"))
  (display (G_ "
      --with-input=PACKAGE=REPLACEMENT
                         replace dependency PACKAGE by REPLACEMENT"))
  (display (G_ "
      --with-graft=PACKAGE=REPLACEMENT
                         graft REPLACEMENT on packages that refer to PACKAGE"))
  (display (G_ "
      --with-branch=PACKAGE=BRANCH
                         build PACKAGE from the latest commit of BRANCH"))
  (display (G_ "
      --with-commit=PACKAGE=COMMIT
                         build PACKAGE from COMMIT"))
  (display (G_ "
      --with-git-url=PACKAGE=URL
                         build PACKAGE from the repository at URL")))


(define (options->transformation opts)
  "Return a procedure that, when passed an object to build (package,
derivation, etc.), applies the transformations specified by OPTS."
  (define applicable
    ;; List of applicable transformations as symbol/procedure pairs in the
    ;; order in which they appear on the command line.
    (filter-map (match-lambda
                  ((key . value)
                   (match (any (match-lambda
                                 ((k . proc)
                                  (and (eq? k key) proc)))
                               %transformations)
                     (#f
                      #f)
                     (transform
                      ;; XXX: We used to pass TRANSFORM a list of several
                      ;; arguments, but we now pass only one, assuming that
                      ;; transform composes well.
                      (cons key (transform (list value)))))))
                (reverse opts)))

  (lambda (store obj)
    (fold (match-lambda*
            (((name . transform) obj)
             (let ((new (transform store obj)))
               (when (eq? new obj)
                 (warning (G_ "transformation '~a' had no effect on ~a~%")
                          name
                          (if (package? obj)
                              (package-full-name obj)
                              obj)))
               new)))
          obj
          applicable)))


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
      --no-build-hook    do not attempt to offload builds via the build hook"))
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
  ;; TODO: Add more options.
  (set-build-options store
                     #:keep-failed? (assoc-ref opts 'keep-failed?)
                     #:keep-going? (assoc-ref opts 'keep-going?)
                     #:rounds (assoc-ref opts 'rounds)
                     #:build-cores (assoc-ref opts 'cores)
                     #:max-build-jobs (assoc-ref opts 'max-jobs)
                     #:fallback? (assoc-ref opts 'fallback?)
                     #:use-substitutes? (assoc-ref opts 'substitutes?)
                     #:substitute-urls (assoc-ref opts 'substitute-urls)
                     #:use-build-hook? (assoc-ref opts 'build-hook?)
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
  `((system . ,(%current-system))
    (build-mode . ,(build-mode normal))
    (graft? . #t)
    (substitutes? . #t)
    (build-hook? . #t)
    (print-build-trace? . #t)
    (print-extended-build-trace? . #t)
    (multiplexed-build-output? . #t)
    (verbosity . 2)
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
         (option '(#\f "file") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'file arg result)))
         (option '(#\n "dry-run") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'dry-run? #t (alist-cons 'graft? #f result))))
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
      (leave (G_ "~s: not something we can build~%") x)))

  (define (ensure-list x)
    (let ((lst (match x
                 ((x ...) x)
                 (x       (list x)))))
      (for-each validate-type lst)
      lst))

  (append-map (match-lambda
                (('argument . (? string? spec))
                 (cond ((derivation-path? spec)
                        (list (read-derivation-from-file spec)))
                       ((store-path? spec)
                        ;; Nothing to do; maybe for --log-file.
                        '())
                       (else
                        (list (specification->package spec)))))
                (('file . file)
                 (ensure-list (load* file (make-user-module '()))))
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
  (define system (assoc-ref opts 'system))
  (define graft? (assoc-ref opts 'graft?))

  ;; We may get 'unbound-variable' errors while evaluating the 'inputs' fields
  ;; of user packages.  Since 'guix build' is the primary tool for people
  ;; testing new packages, report such errors gracefully.
  (with-unbound-variable-handling
   (parameterize ((%graft? graft?))
     (append-map (match-lambda
                   ((? package? p)
                    (let ((p (or (and graft? (package-replacement p)) p)))
                      (match src
                        (#f
                         (list (package->derivation store p system)))
                        (#t
                         (match (package-source p)
                           (#f
                            (format (current-error-port)
                                    (G_ "~a: warning: \
package '~a' has no source~%")
                                    (location->string (package-location p))
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
                            #:system system))))
                 (map (cut transform store <>)
                      (options->things-to-build opts))))))

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

(define (guix-build . args)
  (define opts
    (parse-command-line args %options
                        (list %default-options)))

  (with-error-handling
    ;; Ask for absolute file names so that .drv file names passed from the
    ;; user to 'read-derivation' are absolute when it returns.
    (with-fluids ((%file-port-name-canonicalization 'absolute))
      (with-status-verbosity (assoc-ref opts 'verbosity)
        (with-store store
          ;; Set the build options before we do anything else.
          (set-build-options-from-command-line store opts)

          (parameterize ((current-terminal-columns (terminal-columns)))
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
                                         file)
                                        (_ #f))
                                      opts))
                   (roots (filter-map (match-lambda
                                        (('gc-root . root) root)
                                        (_ #f))
                                      opts)))

              (unless (or (assoc-ref opts 'log-file?)
                          (assoc-ref opts 'derivations-only?))
                (show-what-to-build store drv
                                    #:use-substitutes?
                                    (assoc-ref opts 'substitutes?)
                                    #:dry-run? (assoc-ref opts 'dry-run?)
                                    #:mode mode))

              (cond ((assoc-ref opts 'log-file?)
                     (for-each (cut show-build-log store <> urls)
                               (delete-duplicates
                                (append (map derivation-file-name drv)
                                        items))))
                    ((assoc-ref opts 'derivations-only?)
                     (format #t "~{~a~%~}" (map derivation-file-name drv))
                     (for-each (cut register-root store <> <>)
                               (map (compose list derivation-file-name) drv)
                               roots))
                    ((not (assoc-ref opts 'dry-run?))
                     (and (build-derivations store drv mode)
                          (for-each show-derivation-outputs drv)
                          (for-each (cut register-root store <> <>)
                                    (map (lambda (drv)
                                           (map cdr
                                                (derivation->output-paths drv)))
                                         drv)
                                    roots)))))))))))
