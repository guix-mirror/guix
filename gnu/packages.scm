;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module ((guix ftp-client) #:select (ftp-open))
  #:use-module (guix gnu-maintenance)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-39)
  #:export (search-patch
            search-bootstrap-binary
            %patch-directory
            %bootstrap-binaries-path
            %package-module-path

            fold-packages

            find-packages-by-name
            find-best-packages-by-name
            find-newest-available-packages

            package-direct-dependents
            package-transitive-dependents
            package-covering-dependents

            check-package-freshness

            specification->package))

;;; Commentary:
;;;
;;; General utilities for the software distribution---i.e., the modules under
;;; (gnu packages ...).
;;;
;;; Code:

;; By default, we store patches and bootstrap binaries alongside Guile
;; modules.  This is so that these extra files can be found without
;; requiring a special setup, such as a specific installation directory
;; and an extra environment variable.  One advantage of this setup is
;; that everything just works in an auto-compilation setting.

(define %patch-path
  (make-parameter
   (map (cut string-append <> "/gnu/packages/patches")
        %load-path)))

(define %bootstrap-binaries-path
  (make-parameter
   (map (cut string-append <> "/gnu/packages/bootstrap")
        %load-path)))

(define (search-patch file-name)
  "Search the patch FILE-NAME."
  (search-path (%patch-path) file-name))

(define (search-bootstrap-binary file-name system)
  "Search the bootstrap binary FILE-NAME for SYSTEM."
  (search-path (%bootstrap-binaries-path)
               (string-append system "/" file-name)))

(define %distro-root-directory
  ;; Absolute file name of the module hierarchy.
  (dirname (search-path %load-path "guix.scm")))

(define %package-module-path
  ;; Search path for package modules.  Each item must be either a directory
  ;; name or a pair whose car is a directory and whose cdr is a sub-directory
  ;; to narrow the search.
  (let* ((not-colon   (char-set-complement (char-set #\:)))
         (environment (string-tokenize (or (getenv "GUIX_PACKAGE_PATH") "")
                                       not-colon)))
    ;; Automatically add items from $GUIX_PACKAGE_PATH to Guile's search path.
    (for-each (lambda (directory)
                (set! %load-path (cons directory %load-path))
                (set! %load-compiled-path
                      (cons directory %load-compiled-path)))
              environment)

    (make-parameter
     (append environment `((,%distro-root-directory . "gnu/packages"))))))

(define* (scheme-files directory)
  "Return the list of Scheme files found under DIRECTORY."
  (file-system-fold (const #t)                    ; enter?
                    (lambda (path stat result)    ; leaf
                      (if (string-suffix? ".scm" path)
                          (cons path result)
                          result))
                    (lambda (path stat result)    ; down
                      result)
                    (lambda (path stat result)    ; up
                      result)
                    (const #f)                    ; skip
                    (lambda (path stat errno result)
                      (warning (_ "cannot access `~a': ~a~%")
                               path (strerror errno))
                      result)
                    '()
                    directory
                    stat))

(define file-name->module-name
  (let ((not-slash (char-set-complement (char-set #\/))))
    (lambda (file)
      "Return the module name (a list of symbols) corresponding to FILE."
      (map string->symbol
           (string-tokenize (string-drop-right file 4) not-slash)))))

(define* (package-modules directory #:optional sub-directory)
  "Return the list of modules that provide packages for the distribution.
Optionally, narrow the search to SUB-DIRECTORY."
  (define prefix-len
    (string-length directory))

  (filter-map (lambda (file)
                (let ((file (substring file prefix-len)))
                  (false-if-exception
                   (resolve-interface (file-name->module-name file)))))
              (scheme-files (if sub-directory
                                (string-append directory "/" sub-directory)
                                directory))))

(define* (all-package-modules #:optional (path (%package-module-path)))
  "Return the list of package modules found in PATH, a list of directories to
search."
  (fold-right (lambda (spec result)
                (match spec
                  ((? string? directory)
                   (append (package-modules directory) result))
                  ((directory . sub-directory)
                   (append (package-modules directory sub-directory)
                           result))))
              '()
              path))

(define (fold-packages proc init)
  "Call (PROC PACKAGE RESULT) for each available package, using INIT as
the initial value of RESULT.  It is guaranteed to never traverse the
same package twice."
  (identity   ; discard second return value
   (fold2 (lambda (module result seen)
            (fold2 (lambda (var result seen)
                     (if (and (package? var)
                              (not (vhash-assq var seen)))
                         (values (proc var result)
                                 (vhash-consq var #t seen))
                         (values result seen)))
                   result
                   seen
                   (module-map (lambda (sym var)
                                 (false-if-exception (variable-ref var)))
                               module)))
          init
          vlist-null
          (all-package-modules))))

(define find-packages-by-name
  (let ((packages (delay
                    (fold-packages (lambda (p r)
                                     (vhash-cons (package-name p) p r))
                                   vlist-null))))
    (lambda* (name #:optional version)
      "Return the list of packages with the given NAME.  If VERSION is not #f,
then only return packages whose version is equal to VERSION."
      (let ((matching (vhash-fold* cons '() name (force packages))))
        (if version
            (filter (lambda (package)
                      (string=? (package-version package) version))
                    matching)
            matching)))))

(define find-newest-available-packages
  (memoize
   (lambda ()
     "Return a vhash keyed by package names, and with
associated values of the form

  (newest-version newest-package ...)

where the preferred package is listed first."

     ;; FIXME: Currently, the preferred package is whichever one
     ;; was found last by 'fold-packages'.  Find a better solution.
     (fold-packages (lambda (p r)
                      (let ((name    (package-name p))
                            (version (package-version p)))
                        (match (vhash-assoc name r)
                          ((_ newest-so-far . pkgs)
                           (case (version-compare version newest-so-far)
                             ((>) (vhash-cons name `(,version ,p) r))
                             ((=) (vhash-cons name `(,version ,p ,@pkgs) r))
                             ((<) r)))
                          (#f (vhash-cons name `(,version ,p) r)))))
                    vlist-null))))

(define (find-best-packages-by-name name version)
  "If version is #f, return the list of packages named NAME with the highest
version numbers; otherwise, return the list of packages named NAME and at
VERSION."
  (if version
      (find-packages-by-name name version)
      (match (vhash-assoc name (find-newest-available-packages))
        ((_ version pkgs ...) pkgs)
        (#f '()))))


(define* (vhash-refq vhash key #:optional (dflt #f))
  "Look up KEY in the vhash VHASH, and return the value (if any) associated
with it.  If KEY is not found, return DFLT (or `#f' if no DFLT argument is
supplied).  Uses `eq?' for equality testing."
  (or (and=> (vhash-assq key vhash) cdr)
      dflt))

(define package-dependencies
  (memoize
   (lambda ()
     "Return a vhash keyed by package, and with associated values that are a
list of packages that depend on that package."
     (fold-packages
      (lambda (package dag)
        (fold
         (lambda (in d)
           ;; Insert a graph edge from each of package's inputs to package.
           (vhash-consq in
                        (cons package (vhash-refq d in '()))
                        (vhash-delq in d)))
         dag
         (match (package-direct-inputs package)
           (((labels packages . _) ...)
            packages) )))
      vlist-null))))

(define (package-direct-dependents packages)
  "Return a list of packages from the distribution that directly depend on the
packages in PACKAGES."
  (delete-duplicates
   (concatenate
    (map (lambda (p)
           (vhash-refq (package-dependencies) p '()))
         packages))))

(define (package-transitive-dependents packages)
  "Return the transitive dependent packages of the distribution packages in
PACKAGES---i.e. the dependents of those packages, plus their dependents,
recursively."
  (let ((dependency-dag (package-dependencies)))
    (fold-tree
     cons '()
     (lambda (node) (vhash-refq dependency-dag node))
     ;; Start with the dependents to avoid including PACKAGES in the result.
     (package-direct-dependents packages))))

(define (package-covering-dependents packages)
  "Return a minimal list of packages from the distribution whose dependencies
include all of PACKAGES and all packages that depend on PACKAGES."
  (let ((dependency-dag (package-dependencies)))
    (fold-tree-leaves
     cons '()
     (lambda (node) (vhash-refq dependency-dag node))
     ;; Start with the dependents to avoid including PACKAGES in the result.
     (package-direct-dependents packages))))


(define %sigint-prompt
  ;; The prompt to jump to upon SIGINT.
  (make-prompt-tag "interruptible"))

(define (call-with-sigint-handler thunk handler)
  "Call THUNK and return its value.  Upon SIGINT, call HANDLER with the signal
number in the context of the continuation of the call to this function, and
return its return value."
  (call-with-prompt %sigint-prompt
                    (lambda ()
                      (sigaction SIGINT
                        (lambda (signum)
                          (sigaction SIGINT SIG_DFL)
                          (abort-to-prompt %sigint-prompt signum)))
                      (dynamic-wind
                        (const #t)
                        thunk
                        (cut sigaction SIGINT SIG_DFL)))
                    (lambda (k signum)
                      (handler signum))))

(define-syntax-rule (waiting exp fmt rest ...)
  "Display the given message while EXP is being evaluated."
  (let* ((message (format #f fmt rest ...))
         (blank   (make-string (string-length message) #\space)))
    (display message (current-error-port))
    (force-output (current-error-port))
    (call-with-sigint-handler
     (lambda ()
       (dynamic-wind
         (const #f)
         (lambda () exp)
         (lambda ()
           ;; Clear the line.
           (display #\cr (current-error-port))
           (display blank (current-error-port))
           (display #\cr (current-error-port))
           (force-output (current-error-port)))))
     (lambda (signum)
       (format (current-error-port) "  interrupted by signal ~a~%" SIGINT)
       #f))))

(define ftp-open*
  ;; Memoizing version of `ftp-open'.  The goal is to avoid initiating a new
  ;; FTP connection for each package, esp. since most of them are to the same
  ;; server.  This has a noticeable impact when doing "guix upgrade -u".
  (memoize ftp-open))

(define (check-package-freshness package)
  "Check whether PACKAGE has a newer version available upstream, and report
it."
  ;; TODO: Automatically inject the upstream version when desired.

  (catch #t
    (lambda ()
      (when (false-if-exception (gnu-package? package))
        (let ((name      (package-name package))
              (full-name (package-full-name package)))
          (match (waiting (latest-release name
                                          #:ftp-open ftp-open*
                                          #:ftp-close (const #f))
                          (_ "looking for the latest release of GNU ~a...") name)
            ((latest-version . _)
             (when (version>? latest-version full-name)
               (format (current-error-port)
                       (_ "~a: note: using ~a \
but ~a is available upstream~%")
                       (location->string (package-location package))
                       full-name latest-version)))
            (_ #t)))))
    (lambda (key . args)
      ;; Silently ignore networking errors rather than preventing
      ;; installation.
      (case key
        ((getaddrinfo-error ftp-error) #f)
        (else (apply throw key args))))))

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
