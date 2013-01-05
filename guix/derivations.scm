;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012, 2013 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix derivations)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix base32)
  #:export (<derivation>
            derivation?
            derivation-outputs
            derivation-inputs
            derivation-sources
            derivation-system
            derivation-builder-arguments
            derivation-builder-environment-vars
            derivation-prerequisites
            derivation-prerequisites-to-build

            <derivation-output>
            derivation-output?
            derivation-output-path
            derivation-output-hash-algo
            derivation-output-hash

            <derivation-input>
            derivation-input?
            derivation-input-path
            derivation-input-sub-derivations

            fixed-output-derivation?
            derivation-hash

            read-derivation
            write-derivation
            derivation-path->output-path
            derivation-path->output-paths
            derivation

            %guile-for-build
            build-expression->derivation
            imported-files))

;;;
;;; Nix derivations, as implemented in Nix's `derivations.cc'.
;;;

(define-record-type <derivation>
  (make-derivation outputs inputs sources system builder args env-vars)
  derivation?
  (outputs  derivation-outputs)      ; list of name/<derivation-output> pairs
  (inputs   derivation-inputs)       ; list of <derivation-input>
  (sources  derivation-sources)      ; list of store paths
  (system   derivation-system)       ; string
  (builder  derivation-builder)      ; store path
  (args     derivation-builder-arguments)         ; list of strings
  (env-vars derivation-builder-environment-vars)) ; list of name/value pairs

(define-record-type <derivation-output>
  (make-derivation-output path hash-algo hash)
  derivation-output?
  (path       derivation-output-path)             ; store path
  (hash-algo  derivation-output-hash-algo)        ; symbol | #f
  (hash       derivation-output-hash))            ; bytevector | #f

(define-record-type <derivation-input>
  (make-derivation-input path sub-derivations)
  derivation-input?
  (path            derivation-input-path)             ; store path
  (sub-derivations derivation-input-sub-derivations)) ; list of strings

(define (fixed-output-derivation? drv)
  "Return #t if DRV is a fixed-output derivation, such as the result of a
download with a fixed hash (aka. `fetchurl')."
  (match drv
    (($ <derivation>
        (($ <derivation-output> _ (? symbol?) (? string?))))
     #t)
    (_ #f)))

(define (derivation-prerequisites drv)
  "Return the list of derivation-inputs required to build DRV, recursively."
  (let loop ((drv    drv)
             (result '()))
    (let ((inputs (remove (cut member <> result)  ; XXX: quadratic
                          (derivation-inputs drv))))
      (fold loop
            (append inputs result)
            (map (lambda (i)
                   (call-with-input-file (derivation-input-path i)
                     read-derivation))
                 inputs)))))

(define (derivation-prerequisites-to-build store drv)
  "Return the list of derivation-inputs required to build DRV and not already
available in STORE, recursively."
  (define input-built?
    (match-lambda
     (($ <derivation-input> path sub-drvs)
      (let ((out (map (cut derivation-path->output-path path <>)
                      sub-drvs)))
        (any (cut valid-path? store <>) out)))))

  (let loop ((drv    drv)
             (result '()))
    (let ((inputs (remove (lambda (i)
                            (or (member i result) ; XXX: quadratic
                                (input-built? i)))
                          (derivation-inputs drv))))
      (fold loop
            (append inputs result)
            (map (lambda (i)
                   (call-with-input-file (derivation-input-path i)
                     read-derivation))
                 inputs)))))

(define (read-derivation drv-port)
  "Read the derivation from DRV-PORT and return the corresponding
<derivation> object."

  (define comma (string->symbol ","))

  (define (ununquote x)
    (match x
      (('unquote x) (ununquote x))
      ((x ...)      (map ununquote x))
      (_            x)))

  (define (outputs->alist x)
    (fold-right (lambda (output result)
                  (match output
                    ((name path "" "")
                     (alist-cons name
                                 (make-derivation-output path #f #f)
                                 result))
                    ((name path hash-algo hash)
                     ;; fixed-output
                     (let ((algo (string->symbol hash-algo))
                           (hash (base16-string->bytevector hash)))
                       (alist-cons name
                                   (make-derivation-output path algo hash)
                                   result)))))
                '()
                x))

  (define (make-input-drvs x)
    (fold-right (lambda (input result)
                  (match input
                    ((path (sub-drvs ...))
                     (cons (make-derivation-input path sub-drvs)
                           result))))
                '()
                x))

  ;; The contents of a derivation are typically ASCII, but choosing
  ;; UTF-8 allows us to take the fast path for Guile's `scm_getc'.
  (set-port-encoding! drv-port "UTF-8")

  (let loop ((exp    (read drv-port))
             (result '()))
    (match exp
      ((? eof-object?)
       (let ((result (reverse result)))
         (match result
           (('Derive ((outputs ...) (input-drvs ...)
                      (input-srcs ...)
                      (? string? system)
                      (? string? builder)
                      ((? string? args) ...)
                      ((var value) ...)))
            (make-derivation (outputs->alist outputs)
                             (make-input-drvs input-drvs)
                             input-srcs
                             system builder args
                             (fold-right alist-cons '() var value)))
           (_
            (error "failed to parse derivation" drv-port result)))))
      ((? (cut eq? <> comma))
       (loop (read drv-port) result))
      (_
       (loop (read drv-port)
             (cons (ununquote exp) result))))))

(define (write-derivation drv port)
  "Write the ATerm-like serialization of DRV to PORT.  See Section 2.4 of
Eelco Dolstra's PhD dissertation for an overview of a previous version of
that form."

  ;; Make sure we're using the faster implementation.
  (define format simple-format)

  (define (list->string lst)
    (string-append "[" (string-join lst ",") "]"))

  (define (write-list lst)
    (display (list->string lst) port))

  (define (coalesce-duplicate-inputs inputs)
    ;; Return a list of inputs, such that when INPUTS contains the same DRV
    ;; twice, they are coalesced, with their sub-derivations merged.  This is
    ;; needed because Nix itself keeps only one of them.
    (fold (lambda (input result)
            (match input
              (($ <derivation-input> path sub-drvs)
               ;; XXX: quadratic
               (match (find (match-lambda
                             (($ <derivation-input> p s)
                              (string=? p path)))
                            result)
                 (#f
                  (cons input result))
                 ((and dup ($ <derivation-input> _ sub-drvs2))
                  ;; Merge DUP with INPUT.
                  (let ((sub-drvs (delete-duplicates
                                   (append sub-drvs sub-drvs2))))
                    (cons (make-derivation-input path sub-drvs)
                          (delq dup result))))))))
          '()
          inputs))

  ;; Note: lists are sorted alphabetically, to conform with the behavior of
  ;; C++ `std::map' in Nix itself.

  (match drv
    (($ <derivation> outputs inputs sources
        system builder args env-vars)
     (display "Derive(" port)
     (write-list (map (match-lambda
                       ((name . ($ <derivation-output> path hash-algo hash))
                        (format #f "(~s,~s,~s,~s)"
                                name path
                                (or (and=> hash-algo symbol->string) "")
                                (or (and=> hash bytevector->base16-string)
                                    ""))))
                      (sort outputs
                            (lambda (o1 o2)
                              (string<? (car o1) (car o2))))))
     (display "," port)
     (write-list (map (match-lambda
                       (($ <derivation-input> path sub-drvs)
                        (format #f "(~s,~a)" path
                                (list->string (map object->string
                                                   (sort sub-drvs string<?))))))
                      (sort (coalesce-duplicate-inputs inputs)
                            (lambda (i1 i2)
                              (string<? (derivation-input-path i1)
                                        (derivation-input-path i2))))))
     (display "," port)
     (write-list (map object->string (sort sources string<?)))
     (format port ",~s,~s," system builder)
     (write-list (map object->string args))
     (display "," port)
     (write-list (map (match-lambda
                       ((name . value)
                        (format #f "(~s,~s)" name value)))
                      (sort env-vars
                            (lambda (e1 e2)
                              (string<? (car e1) (car e2))))))
     (display ")" port))))

(define derivation-path->output-path
  ;; This procedure is called frequently, so memoize it.
  (memoize
   (lambda* (path #:optional (output "out"))
     "Read the derivation from PATH (`/nix/store/xxx.drv'), and return the store
path of its output OUTPUT."
     (let* ((drv     (call-with-input-file path read-derivation))
            (outputs (derivation-outputs drv)))
       (and=> (assoc-ref outputs output) derivation-output-path)))))

(define (derivation-path->output-paths path)
  "Read the derivation from PATH (`/nix/store/xxx.drv'), and return the
list of name/path pairs of its outputs."
  (let* ((drv     (call-with-input-file path read-derivation))
         (outputs (derivation-outputs drv)))
    (map (match-lambda
          ((name . output)
           (cons name (derivation-output-path output))))
         outputs)))


;;;
;;; Derivation primitive.
;;;

(define (compressed-hash bv size)                 ; `compressHash'
  "Given the hash stored in BV, return a compressed version thereof that fits
in SIZE bytes."
  (define new (make-bytevector size 0))
  (define old-size (bytevector-length bv))
  (let loop ((i 0))
    (if (= i old-size)
        new
        (let* ((j (modulo i size))
               (o (bytevector-u8-ref new j)))
          (bytevector-u8-set! new j
                              (logxor o (bytevector-u8-ref bv i)))
          (loop (+ 1 i))))))

(define derivation-hash            ; `hashDerivationModulo' in derivations.cc
  (memoize
   (lambda (drv)
    "Return the hash of DRV, modulo its fixed-output inputs, as a bytevector."
    (match drv
      (($ <derivation> ((_ . ($ <derivation-output> path
                                (? symbol? hash-algo) (? bytevector? hash)))))
       ;; A fixed-output derivation.
       (sha256
        (string->utf8
         (string-append "fixed:out:" (symbol->string hash-algo)
                        ":" (bytevector->base16-string hash)
                        ":" path))))
      (($ <derivation> outputs inputs sources
          system builder args env-vars)
       ;; A regular derivation: replace the path of each input with that
       ;; input's hash; return the hash of serialization of the resulting
       ;; derivation.
       (let* ((inputs (map (match-lambda
                            (($ <derivation-input> path sub-drvs)
                             (let ((hash (call-with-input-file path
                                           (compose bytevector->base16-string
                                                    derivation-hash
                                                    read-derivation))))
                               (make-derivation-input hash sub-drvs))))
                           inputs))
              (drv    (make-derivation outputs inputs sources
                                       system builder args env-vars)))
         (sha256
          (string->utf8 (call-with-output-string
                         (cut write-derivation drv <>))))))))))

(define (store-path type hash name)               ; makeStorePath
  "Return the store path for NAME/HASH/TYPE."
  (let* ((s (string-append type ":sha256:"
                           (bytevector->base16-string hash) ":"
                           (%store-prefix) ":" name))
         (h (sha256 (string->utf8 s)))
         (c (compressed-hash h 20)))
    (string-append (%store-prefix) "/"
                   (bytevector->nix-base32-string c) "-"
                   name)))

(define (output-path output hash name)            ; makeOutputPath
  "Return an output path for OUTPUT (the name of the output as a string) of
the derivation called NAME with hash HASH."
  (store-path (string-append "output:" output) hash
              (if (string=? output "out")
                  name
                  (string-append name "-" output))))

(define* (derivation store name system builder args env-vars inputs
                     #:key (outputs '("out")) hash hash-algo hash-mode)
  "Build a derivation with the given arguments.  Return the resulting
store path and <derivation> object.  When HASH, HASH-ALGO, and HASH-MODE
are given, a fixed-output derivation is created---i.e., one whose result is
known in advance, such as a file download."
  (define direct-store-path?
    (let ((len (+ 1 (string-length (%store-prefix)))))
      (lambda (p)
        ;; Return #t if P is a store path, and not a sub-directory of a
        ;; store path.  This predicate is needed because files *under* a
        ;; store path are not valid inputs.
        (and (store-path? p)
             (not (string-index (substring p len) #\/))))))

  (define (add-output-paths drv)
    ;; Return DRV with an actual store path for each of its output and the
    ;; corresponding environment variable.
    (match drv
      (($ <derivation> outputs inputs sources
          system builder args env-vars)
       (let* ((drv-hash (derivation-hash drv))
              (outputs  (map (match-lambda
                              ((output-name . ($ <derivation-output>
                                                 _ algo hash))
                               (let ((path (output-path output-name
                                                        drv-hash name)))
                                 (cons output-name
                                       (make-derivation-output path algo
                                                               hash)))))
                             outputs)))
         (make-derivation outputs inputs sources system builder args
                          (map (match-lambda
                                ((name . value)
                                 (cons name
                                       (or (and=> (assoc-ref outputs name)
                                                  derivation-output-path)
                                           value))))
                               env-vars))))))

  (define (env-vars-with-empty-outputs)
    ;; Return a variant of ENV-VARS where each OUTPUTS is associated with an
    ;; empty string, even outputs that do not appear in ENV-VARS.
    (let ((e (map (match-lambda
                   ((name . val)
                    (if (member name outputs)
                        (cons name "")
                        (cons name val))))
                  env-vars)))
      (fold (lambda (output-name env-vars)
              (if (assoc output-name env-vars)
                  env-vars
                  (append env-vars `((,output-name . "")))))
            e
            outputs)))

  (let* ((outputs    (map (lambda (name)
                            ;; Return outputs with an empty path.
                            (cons name
                                  (make-derivation-output "" hash-algo hash)))
                          outputs))
         (inputs     (map (match-lambda
                           (((? direct-store-path? input))
                            (make-derivation-input input '("out")))
                           (((? direct-store-path? input) sub-drvs ...)
                            (make-derivation-input input sub-drvs))
                           ((input . _)
                            (let ((path (add-to-store store
                                                      (basename input)
                                                      #t #t "sha256" input)))
                              (make-derivation-input path '()))))
                          (delete-duplicates inputs)))
         (env-vars   (env-vars-with-empty-outputs))
         (drv-masked (make-derivation outputs
                                      (filter (compose derivation-path?
                                                       derivation-input-path)
                                              inputs)
                                      (filter-map (lambda (i)
                                                    (let ((p (derivation-input-path i)))
                                                      (and (not (derivation-path? p))
                                                           p)))
                                                  inputs)
                                      system builder args env-vars))
         (drv        (add-output-paths drv-masked)))

    ;; (write-derivation drv-masked (current-error-port))
    ;; (newline (current-error-port))
    (values (add-text-to-store store (string-append name ".drv")
                               (call-with-output-string
                                (cut write-derivation drv <>))
                               (map derivation-input-path
                                    inputs))
            drv)))


;;;
;;; Guile-based builders.
;;;

(define %guile-for-build
  ;; The derivation of the Guile to be used within the build environment,
  ;; when using `build-expression->derivation'.
  (make-parameter #f))

(define (parent-directories file-name)
  "Return the list of parent dirs of FILE-NAME, in the order in which an
`mkdir -p' implementation would make them."
  (let ((not-slash (char-set-complement (char-set #\/))))
    (reverse
     (fold (lambda (dir result)
             (match result
               (()
                (list dir))
               ((prev _ ...)
                (cons (string-append prev "/" dir)
                      result))))
           '()
           (remove (cut string=? <> ".")
                   (string-tokenize (dirname file-name) not-slash))))))

(define* (imported-files store files
                         #:key (name "file-import")
                         (system (%current-system))
                         (guile (%guile-for-build)))
  "Return a derivation that imports FILES into STORE.  FILES must be a list
of (FINAL-PATH . FILE-NAME) pairs; each FILE-NAME is read from the file
system, imported, and appears under FINAL-PATH in the resulting store path."
  (let* ((files   (map (match-lambda
                        ((final-path . file-name)
                         (list final-path
                               (add-to-store store (basename final-path) #t #f
                                             "sha256" file-name))))
                       files))
         (builder
          `(begin
             (mkdir %output) (chdir %output)
             ,@(append-map (match-lambda
                            ((final-path store-path)
                             (append (match (parent-directories final-path)
                                       (() '())
                                       ((head ... tail)
                                        (append (map (lambda (d)
                                                       `(false-if-exception
                                                         (mkdir ,d)))
                                                     head)
                                                `((or (file-exists? ,tail)
                                                      (mkdir ,tail))))))
                                     `((symlink ,store-path ,final-path)))))
                           files))))
    (build-expression->derivation store name system
                                  builder files
                                  #:guile-for-build guile)))

(define* (imported-modules store modules
                           #:key (name "module-import")
                           (system (%current-system))
                           (guile (%guile-for-build)))
  "Return a derivation that contains the source files of MODULES, a list of
module names such as `(ice-9 q)'.  All of MODULES must be in the current
search path."
  ;; TODO: Determine the closure of MODULES, build the `.go' files,
  ;; canonicalize the source files through read/write, etc.
  (let ((files (map (lambda (m)
                      (let ((f (string-append
                                (string-join (map symbol->string m) "/")
                                ".scm")))
                        (cons f (search-path %load-path f))))
                    modules)))
    (imported-files store files #:name name #:system system
                    #:guile guile)))

(define* (compiled-modules store modules
                           #:key (name "module-import-compiled")
                           (system (%current-system))
                           (guile (%guile-for-build)))
  "Return a derivation that builds a tree containing the `.go' files
corresponding to MODULES.  All the MODULES are built in a context where
they can refer to each other."
  (let* ((module-drv (imported-modules store modules
                                       #:system system
                                       #:guile guile))
         (module-dir (derivation-path->output-path module-drv))
         (files      (map (lambda (m)
                            (let ((f (string-join (map symbol->string m)
                                                  "/")))
                              (cons (string-append f ".go")
                                    (string-append module-dir "/" f ".scm"))))
                      modules)))
    (define builder
      `(begin
         (use-modules (system base compile))
         (let ((out (assoc-ref %outputs "out")))
           (mkdir out)
           (chdir out))

         (set! %load-path
               (cons ,module-dir %load-path))

         ,@(map (match-lambda
                 ((output . input)
                  (let ((make-parent-dirs (map (lambda (dir)
                                                 `(unless (file-exists? ,dir)
                                                    (mkdir ,dir)))
                                               (parent-directories output))))
                   `(begin
                      ,@make-parent-dirs
                      (compile-file ,input
                                    #:output-file ,output
                                    #:opts %auto-compilation-options)))))
                files)))

    (build-expression->derivation store name system builder
                                  `(("modules" ,module-drv))
                                  #:guile-for-build guile)))

(define* (build-expression->derivation store name system exp inputs
                                       #:key (outputs '("out"))
                                       hash hash-algo
                                       (env-vars '())
                                       (modules '())
                                       guile-for-build)
  "Return a derivation that executes Scheme expression EXP as a builder for
derivation NAME.  INPUTS must be a list of (NAME DRV-PATH SUB-DRV) tuples;
when SUB-DRV is omitted, \"out\" is assumed.  EXP is evaluated in an
environment where %OUTPUT is bound to the main output path, %OUTPUTS is bound
to a list of output/path pairs, and where %BUILD-INPUTS is bound to an alist
of string/output-path pairs made from INPUTS.  Optionally, ENV-VARS is a list
of string pairs specifying the name and value of environment variables
visible to the builder.  The builder terminates by passing the result of EXP
to `exit'; thus, when EXP returns #f, the build is considered to have
failed.

EXP is built using GUILE-FOR-BUILD (a derivation).  When GUILE-FOR-BUILD is
omitted or is #f, the value of the `%guile-for-build' fluid is used instead."
  (define guile-drv
    (or guile-for-build (%guile-for-build)))

  (define guile
    (string-append (derivation-path->output-path guile-drv)
                   "/bin/guile"))

  (define module-form?
    (match-lambda
      (((or 'define-module 'use-modules) _ ...) #t)
      (_ #f)))

  (define source-path
    ;; When passed an input that is a source, return its path; otherwise
    ;; return #f.
    (match-lambda
     ((_ path _ ...)
      (and (not (derivation-path? path))
           path))))

  (let* ((prologue `(begin
                      ,@(match exp
                          ((_ ...)
                           ;; Module forms must appear at the top-level so
                           ;; that any macros they export can be expanded.
                           (filter module-form? exp))
                          (_ `(,exp)))

                      (define %output (getenv "out"))
                      (define %outputs
                        (map (lambda (o)
                               (cons o (getenv o)))
                             ',outputs))
                      (define %build-inputs
                        ',(map (match-lambda
                                ((name drv . rest)
                                 (let ((sub (match rest
                                              (() "out")
                                              ((x) x))))
                                   (cons name
                                         (if (derivation-path? drv)
                                             (derivation-path->output-path drv
                                                                           sub)
                                             drv)))))
                               inputs))

                      ,@(if (null? modules)
                            '()
                            ;; Remove our own settings.
                            '((unsetenv "GUILE_LOAD_COMPILED_PATH")))

                      ;; Guile sets it, but remove it to avoid conflicts when
                      ;; building Guile-using packages.
                      (unsetenv "LD_LIBRARY_PATH")))
         (builder  (add-text-to-store store
                                      (string-append name "-guile-builder")
                                      (string-append
                                       (object->string prologue)
                                       (object->string
                                        `(exit
                                          ,(match exp
                                             ((_ ...)
                                              (remove module-form? exp))
                                             (_ `(,exp))))))

                                      ;; The references don't really matter
                                      ;; since the builder is always used in
                                      ;; conjunction with the drv that needs
                                      ;; it.  For clarity, we add references
                                      ;; to the subset of INPUTS that are
                                      ;; sources, avoiding references to other
                                      ;; .drv; otherwise, BUILDER's hash would
                                      ;; depend on those, even if they are
                                      ;; fixed-output.
                                      (filter-map source-path inputs)))

         (mod-drv  (and (pair? modules)
                        (imported-modules store modules
                                          #:guile guile-drv
                                          #:system system)))
         (mod-dir  (and mod-drv
                        (derivation-path->output-path mod-drv)))
         (go-drv   (and (pair? modules)
                        (compiled-modules store modules
                                          #:guile guile-drv
                                          #:system system)))
         (go-dir   (and go-drv
                        (derivation-path->output-path go-drv))))
    (derivation store name system guile
                `("--no-auto-compile"
                  ,@(if mod-dir `("-L" ,mod-dir) '())
                  ,builder)

                ;; When MODULES is non-empty, shamelessly clobber
                ;; $GUILE_LOAD_COMPILED_PATH.
                (if go-dir
                    `(("GUILE_LOAD_COMPILED_PATH" . ,go-dir)
                      ,@(alist-delete "GUILE_LOAD_COMPILED_PATH"
                                      env-vars))
                    env-vars)

                `((,(or guile-for-build (%guile-for-build)))
                  (,builder)
                  ,@(map cdr inputs)
                  ,@(if mod-drv `((,mod-drv) (,go-drv)) '()))
                #:hash hash #:hash-algo hash-algo
                #:outputs outputs)))
