;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2019, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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

(define-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (guix grafts)
  #:use-module (guix utils)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (gexp
            gexp?
            sexp->gexp
            with-imported-modules
            with-extensions
            let-system
            gexp->approximate-sexp

            gexp-input
            gexp-input?
            gexp-input-thing
            gexp-input-output
            gexp-input-native?

            assume-valid-file-name
            local-file
            local-file?
            local-file-file
            local-file-absolute-file-name
            local-file-name
            local-file-recursive?
            local-file-select?

            plain-file
            plain-file?
            plain-file-name
            plain-file-content

            computed-file
            computed-file?
            computed-file-name
            computed-file-gexp
            computed-file-options

            program-file
            program-file?
            program-file-name
            program-file-gexp
            program-file-guile
            program-file-module-path

            scheme-file
            scheme-file?
            scheme-file-name
            scheme-file-gexp

            file-append
            file-append?
            file-append-base
            file-append-suffix

            raw-derivation-file
            raw-derivation-file?

            with-parameters
            parameterized?

            load-path-expression
            gexp-modules

            lower-gexp
            lowered-gexp?
            lowered-gexp-sexp
            lowered-gexp-inputs
            lowered-gexp-sources
            lowered-gexp-guile
            lowered-gexp-load-path
            lowered-gexp-load-compiled-path

            with-build-variables
            input-tuples->gexp
            outputs->gexp

            gexp->derivation
            gexp->file
            gexp->script
            text-file*
            mixed-text-file
            file-union
            directory-union

            imported-files
            imported-modules
            compiled-modules

            define-gexp-compiler
            gexp-compiler?
            file-like?
            lower-object

            &gexp-error
            gexp-error?
            &gexp-input-error
            gexp-input-error?
            gexp-error-invalid-input))

;;; Commentary:
;;;
;;; This module implements "G-expressions", or "gexps".  Gexps are like
;;; S-expressions (sexps), with two differences:
;;;
;;;   1. References (un-quotations) to derivations or packages in a gexp are
;;;      replaced by the corresponding output file name; in addition, the
;;;      'ungexp-native' unquote-like form allows code to explicitly refer to
;;;      the native code of a given package, in case of cross-compilation;
;;;
;;;   2. Gexps embed information about the derivations they refer to.
;;;
;;; Gexps make it easy to write to files Scheme code that refers to store
;;; items, or to write Scheme code to build derivations.
;;;
;;; Code:

;; "G expressions".
(define-record-type <gexp>
  (make-gexp references modules extensions proc location)
  gexp?
  (references gexp-references)                    ;list of <gexp-input>
  (modules    gexp-self-modules)                  ;list of module names
  (extensions gexp-self-extensions)               ;list of lowerable things
  (proc       gexp-proc)                          ;procedure
  (location   %gexp-location))                    ;location alist

(define (gexp-location gexp)
  "Return the source code location of GEXP."
  (and=> (%gexp-location gexp) source-properties->location))

(define* (gexp->approximate-sexp gexp)
  "Return the S-expression corresponding to GEXP, but do not lower anything.
As a result, the S-expression will be approximate if GEXP has references."
  (define (gexp-like? thing)
    (or (gexp? thing) (gexp-input? thing)))
  (apply (gexp-proc gexp)
         (map (lambda (reference)
                (match reference
                  (($ <gexp-input> thing output native)
                   (if (gexp-like? thing)
                       (gexp->approximate-sexp thing)
                       ;; Simply returning 'thing' won't work in some
                       ;; situations; see 'write-gexp' below.
                       '(*approximate*)))
                  (_ '(*approximate*))))
              (gexp-references gexp))))

(define (write-gexp gexp port)
  "Write GEXP on PORT."
  (display "#<gexp " port)

  ;; Try to write the underlying sexp.  Now, this trick doesn't work when
  ;; doing things like (ungexp-splicing (gexp ())) because GEXP's procedure
  ;; tries to use 'append' on that, which fails with wrong-type-arg.
  (false-if-exception
   (write (apply (gexp-proc gexp)
                 (gexp-references gexp))
          port))

  (let ((loc (gexp-location gexp)))
    (when loc
      (format port " ~a" (location->string loc))))

  (format port " ~a>"
          (number->string (object-address gexp) 16)))

(set-record-type-printer! <gexp> write-gexp)

(define (gexp-with-hidden-inputs gexp inputs)
  "Add INPUTS, a list of <gexp-input>, to the references of GEXP.  These are
\"hidden inputs\" because they do not actually appear in the expansion of GEXP
returned by 'gexp->sexp'."
  (make-gexp (append inputs (gexp-references gexp))
             (gexp-self-modules gexp)
             (gexp-self-extensions gexp)
             (let ((extra (length inputs)))
               (lambda args
                 (apply (gexp-proc gexp) (drop args extra))))
             (gexp-location gexp)))


;;;
;;; Methods.
;;;

;; Compiler for a type of objects that may be introduced in a gexp.
(define-record-type <gexp-compiler>
  (gexp-compiler type lower expand)
  gexp-compiler?
  (type       gexp-compiler-type)                 ;record type descriptor
  (lower      gexp-compiler-lower)
  (expand     gexp-compiler-expand))              ;#f | DRV -> sexp

(define-condition-type &gexp-error &error
  gexp-error?)

(define-condition-type &gexp-input-error &gexp-error
  gexp-input-error?
  (input gexp-error-invalid-input))


(define %gexp-compilers
  ;; 'eq?' mapping of record type descriptor to <gexp-compiler>.
  (make-hash-table 20))

(define (default-expander thing obj output)
  "This is the default expander for \"things\" that appear in gexps.  It
returns its output file name of OBJ's OUTPUT."
  (match obj
    ((? derivation? drv)
     (derivation->output-path drv output))
    ((? string? file)
     file)
    ((? self-quoting? obj)
     obj)))

(define (register-compiler! compiler)
  "Register COMPILER as a gexp compiler."
  (hashq-set! %gexp-compilers
              (gexp-compiler-type compiler) compiler))

(define (lookup-compiler object)
  "Search for a compiler for OBJECT.  Upon success, return the three argument
procedure to lower it; otherwise return #f."
  (and=> (hashq-ref %gexp-compilers (struct-vtable object))
         gexp-compiler-lower))

(define (file-like? object)
  "Return #t if OBJECT leads to a file in the store once unquoted in a
G-expression; otherwise return #f."
  (and (struct? object) (->bool (lookup-compiler object))))

(define (lookup-expander object)
  "Search for an expander for OBJECT.  Upon success, return the three argument
procedure to expand it; otherwise return #f."
  (and=> (hashq-ref %gexp-compilers (struct-vtable object))
         gexp-compiler-expand))

(define* (lower-object obj
                       #:optional (system (%current-system))
                       #:key (target 'current))
  "Return as a value in %STORE-MONAD the derivation or store item
corresponding to OBJ for SYSTEM, cross-compiling for TARGET if TARGET is true.
OBJ must be an object that has an associated gexp compiler, such as a
<package>."
  (mlet %store-monad ((target (if (eq? target 'current)
                                  (current-target-system)
                                  (return target)))
                      (graft? (grafting?)))
    (let loop ((obj obj))
      (match (lookup-compiler obj)
        (#f
         (raise (condition (&gexp-input-error (input obj)))))
        (lower
         ;; Cache in STORE the result of lowering OBJ.  If OBJ is a
         ;; derivation, bypass the cache.
         (if (derivation? obj)
             (return obj)
             (mcached (mlet %store-monad ((lowered (lower obj system target)))
                        (if (and (struct? lowered)
                                 (not (derivation? lowered)))
                            (loop lowered)
                            (return lowered)))
                      obj
                      system target graft?)))))))

(define* (lower+expand-object obj
                              #:optional (system (%current-system))
                              #:key target (output "out"))
  "Return as a value in %STORE-MONAD the output of object OBJ expands to for
SYSTEM and TARGET.  Object such as <package>, <file-append>, or <plain-file>
expand to file names, but it's possible to expand to a plain data type."
  (let loop ((obj obj)
             (expand (and (struct? obj) (lookup-expander obj))))
    (match (lookup-compiler obj)
      (#f
       (raise (condition (&gexp-input-error (input obj)))))
      (lower
       (mlet* %store-monad ((graft?  (grafting?))
                            (lowered (if (derivation? obj)
                                         (return obj)
                                         (mcached (lower obj system target)
                                                  obj
                                                  system target graft?))))
         ;; LOWER might return something that needs to be further
         ;; lowered.
         (if (struct? lowered)
             ;; If we lack an expander, delegate to that of LOWERED.
             (if (not expand)
                 (loop lowered (lookup-expander lowered))
                 (return (expand obj lowered output)))
             (if (not expand)                     ;self-quoting
                 (return lowered)
                 (return (expand obj lowered output)))))))))

(define-syntax define-gexp-compiler
  (syntax-rules (=> compiler expander)
    "Define NAME as a compiler for objects matching PREDICATE encountered in
gexps.

In the simplest form of the macro, BODY must return (1) a derivation for
a record of the specified type, for SYSTEM and TARGET (the latter of which is
#f except when cross-compiling), (2) another record that can itself be
compiled down to a derivation, or (3) an object of a primitive data type.

The more elaborate form allows you to specify an expander:

  (define-gexp-compiler something-compiler <something>
    compiler => (lambda (param system target) ...)
    expander => (lambda (param drv output) ...))

The expander specifies how an object is converted to its sexp representation."
    ((_ (name (param record-type) system target) body ...)
     (define-gexp-compiler name record-type
       compiler => (lambda (param system target) body ...)
       expander => default-expander))
    ((_ name record-type
        compiler => compile
        expander => expand)
     (begin
       (define name
         (gexp-compiler record-type compile expand))
       (register-compiler! name)))))

(define-gexp-compiler (derivation-compiler (drv <derivation>) system target)
  ;; Derivations are the lowest-level representation, so this is the identity
  ;; compiler.
  (with-monad %store-monad
    (return drv)))

;; Expand to a raw ".drv" file for the lowerable object it wraps.  In other
;; words, this gives the raw ".drv" file instead of its build result.
(define-record-type <raw-derivation-file>
  (raw-derivation-file obj)
  raw-derivation-file?
  (obj  raw-derivation-file-object))              ;lowerable object

(define-gexp-compiler raw-derivation-file-compiler <raw-derivation-file>
  compiler => (lambda (obj system target)
                (mlet %store-monad ((obj (lower-object
                                          (raw-derivation-file-object obj)
                                          system #:target target)))
                  ;; Returning the .drv file name instead of the <derivation>
                  ;; record ensures that 'lower-gexp' will classify it as a
                  ;; "source" and not as an "input".
                  (return (if (derivation? obj)
                              (derivation-file-name obj)
                              obj))))
  expander => (lambda (obj lowered output)
                (if (derivation? lowered)
                    (derivation-file-name lowered)
                    lowered)))


;;;
;;; System dependencies.
;;;

;; Binding form for the current system and cross-compilation target.
(define-record-type <system-binding>
  (system-binding proc)
  system-binding?
  (proc system-binding-proc))

(define-syntax let-system
  (syntax-rules ()
    "Introduce a system binding in a gexp.  The simplest form is:

  (let-system system
    (cond ((string=? system \"x86_64-linux\") ...)
          (else ...)))

which binds SYSTEM to the currently targeted system.  The second form is
similar, but it also shows the cross-compilation target:

  (let-system (system target)
    ...)

Here TARGET is bound to the cross-compilation triplet or #f."
    ((_ (system target) exp0 exp ...)
     (system-binding (lambda (system target)
                       exp0 exp ...)))
    ((_ system exp0 exp ...)
     (system-binding (lambda (system target)
                       exp0 exp ...)))))

(define-gexp-compiler system-binding-compiler <system-binding>
  compiler => (lambda (binding system target)
                (match binding
                  (($ <system-binding> proc)
                   (with-monad %store-monad
                     ;; PROC is expected to return a lowerable object.
                     ;; 'lower-object' takes care of residualizing it to a
                     ;; derivation or similar.
                     (return (proc system target))))))

  ;; Delegate to the expander of the object returned by PROC.
  expander => #f)


;;;
;;; File declarations.
;;;

;; A local file name.  FILE is the file name the user entered, which can be a
;; relative file name, and ABSOLUTE is a promise that computes its canonical
;; absolute file name.  We keep it in a promise to compute it lazily and avoid
;; repeated 'stat' calls.
(define-record-type <local-file>
  (%%local-file file absolute name recursive? select?)
  local-file?
  (file       local-file-file)                    ;string
  (absolute   %local-file-absolute-file-name)     ;promise string
  (name       local-file-name)                    ;string
  (recursive? local-file-recursive?)              ;Boolean
  (select?    local-file-select?))                ;string stat -> Boolean

(define (true file stat) #t)

(define* (%local-file file promise #:optional (name (basename file))
                      #:key
                      (literal? #t) location
                      recursive? (select? true))
  ;; This intermediate procedure is part of our ABI, but the underlying
  ;; %%LOCAL-FILE is not.
  (when (and (not literal?) (not (string-prefix? "/" file)))
    (warning (and=> location source-properties->location)
             (G_ "resolving '~a' relative to current directory~%")
             file))
  (%%local-file file promise name recursive? select?))

(define (absolute-file-name file directory)
  "Return the canonical absolute file name for FILE, which lives in the
vicinity of DIRECTORY."
  (canonicalize-path
   (cond ((string-prefix? "/" file) file)
         ((not directory) file)
         ((string-prefix? "/" directory)
          (string-append directory "/" file))
         (else file))))

(define-syntax-rule (assume-valid-file-name file)
  "This is a syntactic keyword to tell 'local-file' that it can assume that
the given file name is valid, even if it's not a string literal, and thus not
warn about it."
  file)

(define-syntax local-file
  (lambda (s)
    "Return an object representing local file FILE to add to the store; this
object can be used in a gexp.  If FILE is a relative file name, it is looked
up relative to the source file where this form appears.  FILE will be added to
the store under NAME--by default the base name of FILE.

When RECURSIVE? is true, the contents of FILE are added recursively; if FILE
designates a flat file and RECURSIVE? is true, its contents are added, and its
permission bits are kept.

When RECURSIVE? is true, call (SELECT?  FILE STAT) for each directory entry,
where FILE is the entry's absolute file name and STAT is the result of
'lstat'; exclude entries for which SELECT? does not return true.

This is the declarative counterpart of the 'interned-file' monadic procedure.
It is implemented as a macro to capture the current source directory where it
appears."
    (syntax-case s (assume-valid-file-name)
      ((_ file rest ...)
       (string? (syntax->datum #'file))
       ;; FILE is a literal, so resolve it relative to the source directory.
       #'(%local-file file
                      (delay (absolute-file-name file (current-source-directory)))
                      rest ...))
      ((_ (assume-valid-file-name file) rest ...)
       ;; FILE is not a literal, so resolve it relative to the current
       ;; directory.  Since the user declared FILE is valid, do not pass
       ;; #:literal? #f so that we do not warn about it later on.
       #'(%local-file file
                      (delay (absolute-file-name file (getcwd)))
                      rest ...))
      ((_ file rest ...)
       ;; Resolve FILE relative to the current directory.
       (with-syntax ((location (datum->syntax s (syntax-source s))))
        #`(%local-file file
                       (delay (absolute-file-name file (getcwd)))
                       rest ...
                       #:location 'location
                       #:literal? #f)))           ;warn if FILE is relative
      ((_)
       #'(syntax-error "missing file name"))
      (id
       (identifier? #'id)
       ;; XXX: We could return #'(lambda (file . rest) ...).  However,
       ;; (syntax-source #'id) is #f so (current-source-directory) would not
       ;; work.  Thus, simply forbid this form.
       #'(syntax-error
          "'local-file' is a macro and cannot be used like this")))))

(define (local-file-absolute-file-name file)
  "Return the absolute file name for FILE, a <local-file> instance.  A
'system-error' exception is raised if FILE could not be found."
  (force (%local-file-absolute-file-name file)))

(define-gexp-compiler (local-file-compiler (file <local-file>) system target)
  ;; "Compile" FILE by adding it to the store.
  (match file
    (($ <local-file> file (= force absolute) name recursive? select?)
     ;; Canonicalize FILE so that if it's a symlink, it is resolved.  Failing
     ;; to do that, when RECURSIVE? is #t, we could end up creating a dangling
     ;; symlink in the store, and when RECURSIVE? is #f 'add-to-store' would
     ;; just throw an error, both of which are inconvenient.
     (interned-file absolute name
                    #:recursive? recursive? #:select? select?))))

(define-record-type <plain-file>
  (%plain-file name content references)
  plain-file?
  (name        plain-file-name)                   ;string
  (content     plain-file-content)                ;string or bytevector
  (references  plain-file-references))            ;list (currently unused)

(define (plain-file name content)
  "Return an object representing a text file called NAME with the given
CONTENT (a string) to be added to the store.

This is the declarative counterpart of 'text-file'."
  ;; XXX: For now just ignore 'references' because it's not clear how to use
  ;; them in a declarative context.
  (%plain-file name content '()))

(define-gexp-compiler (plain-file-compiler (file <plain-file>) system target)
  ;; "Compile" FILE by adding it to the store.
  (match file
    (($ <plain-file> name (and (? string?) content) references)
     (text-file name content references))
    (($ <plain-file> name (and (? bytevector?) content) references)
     (binary-file name content references))))

(define-record-type <computed-file>
  (%computed-file name gexp guile options)
  computed-file?
  (name       computed-file-name)                 ;string
  (gexp       computed-file-gexp)                 ;gexp
  (guile      computed-file-guile)                ;<package>
  (options    computed-file-options))             ;list of arguments

(define* (computed-file name gexp
                        #:key guile (local-build? #t) (options '()))
  "Return an object representing the store item NAME, a file or directory
computed by GEXP.  When LOCAL-BUILD? is #t (the default), it ensures the
corresponding derivation is built locally.  OPTIONS may be used to pass
additional arguments to 'gexp->derivation'.

This is the declarative counterpart of 'gexp->derivation'."
  (let ((options* `(#:local-build? ,local-build? ,@options)))
    (%computed-file name gexp guile options*)))

(define-gexp-compiler (computed-file-compiler (file <computed-file>)
                                              system target)
  ;; Compile FILE by returning a derivation whose build expression is its
  ;; gexp.
  (match file
    (($ <computed-file> name gexp guile options)
     (if guile
         (mlet %store-monad ((guile (lower-object guile system
                                                  #:target target)))
           (apply gexp->derivation name gexp #:guile-for-build guile
                  #:system system #:target target options))
         (apply gexp->derivation name gexp
                #:system system #:target target options)))))

(define-record-type <program-file>
  (%program-file name gexp guile path)
  program-file?
  (name       program-file-name)                  ;string
  (gexp       program-file-gexp)                  ;gexp
  (guile      program-file-guile)                 ;package
  (path       program-file-module-path))          ;list of strings

(define* (program-file name gexp #:key (guile #f) (module-path %load-path))
  "Return an object representing the executable store item NAME that runs
GEXP.  GUILE is the Guile package used to execute that script.  Imported
modules of GEXP are looked up in MODULE-PATH.

This is the declarative counterpart of 'gexp->script'."
  (%program-file name gexp guile module-path))

(define-gexp-compiler (program-file-compiler (file <program-file>)
                                             system target)
  ;; Compile FILE by returning a derivation that builds the script.
  (match file
    (($ <program-file> name gexp guile module-path)
     (gexp->script name gexp
                   #:module-path module-path
                   #:guile (or guile (default-guile))
                   #:system system
                   #:target target))))

(define-record-type <scheme-file>
  (%scheme-file name gexp splice? load-path?)
  scheme-file?
  (name       scheme-file-name)                  ;string
  (gexp       scheme-file-gexp)                  ;gexp
  (splice?    scheme-file-splice?)               ;Boolean
  (load-path? scheme-file-set-load-path?))       ;Boolean

(define* (scheme-file name gexp #:key splice? (set-load-path? #t))
  "Return an object representing the Scheme file NAME that contains GEXP.

This is the declarative counterpart of 'gexp->file'."
  (%scheme-file name gexp splice? set-load-path?))

(define-gexp-compiler (scheme-file-compiler (file <scheme-file>)
                                            system target)
  ;; Compile FILE by returning a derivation that builds the file.
  (match file
    (($ <scheme-file> name gexp splice? set-load-path?)
     (gexp->file name gexp
                 #:set-load-path? set-load-path?
                 #:splice? splice?
                 #:system system
                 #:target target))))

;; Appending SUFFIX to BASE's output file name.
(define-record-type <file-append>
  (%file-append base suffix)
  file-append?
  (base   file-append-base)                    ;<package> | <derivation> | ...
  (suffix file-append-suffix))                 ;list of strings

(define (write-file-append file port)
  (match file
    (($ <file-append> base suffix)
     (format port "#<file-append ~s ~s>" base
             (string-join suffix)))))

(set-record-type-printer! <file-append> write-file-append)

(define (file-append base . suffix)
  "Return a <file-append> object that expands to the concatenation of BASE and
SUFFIX."
  (%file-append base suffix))

(define-gexp-compiler file-append-compiler <file-append>
  compiler => (lambda (obj system target)
                (match obj
                  (($ <file-append> base _)
                   (lower-object base system #:target target))))
  expander => (lambda (obj lowered output)
                (match obj
                  (($ <file-append> base suffix)
                   (let* ((expand (or (lookup-expander base)
                                      (lookup-expander lowered)))
                          (base   (expand base lowered output)))
                     (string-append base (string-concatenate suffix)))))))

;; Representation of SRFI-39 parameter settings in the dynamic scope of an
;; object lowering.
(define-record-type <parameterized>
  (parameterized bindings thunk)
  parameterized?
  (bindings parameterized-bindings)             ;list of parameter/value pairs
  (thunk    parameterized-thunk))               ;thunk

(define-syntax-rule (with-parameters ((param value) ...) body ...)
  "Bind each PARAM to the corresponding VALUE for the extent during which BODY
is lowered.  Consider this example:

  (with-parameters ((%current-system \"x86_64-linux\"))
    coreutils)

It returns a <parameterized> object that ensures %CURRENT-SYSTEM is set to
x86_64-linux when COREUTILS is lowered."
  (parameterized (list (list param (lambda () value)) ...)
                 (lambda ()
                   body ...)))

(define-gexp-compiler compile-parameterized <parameterized>
  compiler =>
  (lambda (parameterized system target)
    (match (parameterized-bindings parameterized)
      (((parameters values) ...)
       (let ((fluids (map parameter-fluid parameters))
             (thunk  (parameterized-thunk parameterized)))
         ;; Install the PARAMETERS for the dynamic extent of THUNK.
         (with-fluids* fluids
           (map (lambda (thunk) (thunk)) values)
           (lambda ()
             ;; Special-case '%current-system' and '%current-target-system' to
             ;; make sure we get the desired effect.
             (let ((system (if (memq %current-system parameters)
                               (%current-system)
                               system))
                   (target (if (memq %current-target-system parameters)
                               (%current-target-system)
                               target)))
               (lower-object (thunk) system #:target target))))))))

  expander => (lambda (parameterized lowered output)
                (match (parameterized-bindings parameterized)
                  (((parameters values) ...)
                   (let ((fluids (map parameter-fluid parameters))
                         (thunk  (parameterized-thunk parameterized)))
                     ;; Install the PARAMETERS for the dynamic extent of THUNK.
                     (with-fluids* fluids
                       (map (lambda (thunk) (thunk)) values)
                       (lambda ()
                         ;; Delegate to the expander of the wrapped object.
                         (let* ((base   (thunk))
                                (expand (lookup-expander base)))
                           (expand base lowered output)))))))))


;;;
;;; Inputs & outputs.
;;;

;; The input of a gexp.
(define-record-type <gexp-input>
  (%gexp-input thing output native?)
  gexp-input?
  (thing     gexp-input-thing)       ;<package> | <origin> | <derivation> | ...
  (output    gexp-input-output)      ;string
  (native?   gexp-input-native?))    ;Boolean

(define (write-gexp-input input port)
  (match input
    (($ <gexp-input> thing output #f)
     (format port "#<gexp-input ~s:~a>" thing output))
    (($ <gexp-input> thing output #t)
     (format port "#<gexp-input native ~s:~a>" thing output))))

(set-record-type-printer! <gexp-input> write-gexp-input)

(define* (gexp-input thing                        ;convenience procedure
                     #:optional (output "out")
                     #:key native?)
  "Return a new <gexp-input> for the OUTPUT of THING; NATIVE? determines
whether this should be considered a \"native\" input or not."
  (%gexp-input thing output native?))

;; Reference to one of the derivation's outputs, for gexps used in
;; derivations.
(define-record-type <gexp-output>
  (gexp-output name)
  gexp-output?
  (name gexp-output-name))

(define (write-gexp-output output port)
  (match output
    (($ <gexp-output> name)
     (format port "#<gexp-output ~a>" name))))

(set-record-type-printer! <gexp-output> write-gexp-output)

(define* (gexp-attribute gexp self-attribute #:optional (equal? equal?)
                         #:key (validate (const #t)))
  "Recurse on GEXP and the expressions it refers to, summing the items
returned by SELF-ATTRIBUTE, a procedure that takes a gexp.  Use EQUAL? as the
second argument to 'delete-duplicates'.  Pass VALIDATE every gexp and
attribute that is traversed."
  (if (gexp? gexp)
      (delete-duplicates
       (append (let ((attribute (self-attribute gexp)))
                 (validate gexp attribute)
                 attribute)
               (reverse
                (fold (lambda (input result)
                        (match input
                          (($ <gexp-input> (? gexp? exp))
                           (append (gexp-attribute exp self-attribute
                                                   #:validate validate)
                                   result))
                          (($ <gexp-input> (lst ...))
                           (fold/tree (lambda (obj result)
                                        (match obj
                                          ((? gexp? exp)
                                           (append (gexp-attribute exp self-attribute
                                                                   #:validate validate)
                                                   result))
                                          (_
                                           result)))
                                      result
                                      lst))
                          (_
                           result)))
                      '()
                      (gexp-references gexp))))
       equal?)
      '()))                                       ;plain Scheme data type

(define (gexp-modules gexp)
  "Return the list of Guile module names GEXP relies on.  If (gexp? GEXP) is
false, meaning that GEXP is a plain Scheme object, return the empty list."
  (define (module=? m1 m2)
    ;; Return #t when M1 equals M2.  Special-case '=>' specs because their
    ;; right-hand side may not be comparable with 'equal?': it's typically a
    ;; file-like object that embeds a gexp, which in turn embeds closure;
    ;; those closures may be 'eq?' when running compiled code but are unlikely
    ;; to be 'eq?' when running on 'eval'.  Ignore the right-hand side to
    ;; avoid this discrepancy.
    (match m1
      (((name1 ...) '=> _)
       (match m2
         (((name2 ...) '=> _) (equal? name1 name2))
         (_ #f)))
      (_
       (equal? m1 m2))))

  (define (validate-modules gexp modules)
    ;; Warn if MODULES, imported by GEXP, contains modules that in general
    ;; should not be imported from the host because they vary from user to
    ;; user and may thus be a source of non-reproducibility.  This includes
    ;; (guix config) as well as modules that come with Guile.
    (match (filter (match-lambda
                     ((or ('guix 'config) ('ice-9 . _)) #t)
                     (_ #f))
                   modules)
      (() #t)
      (suspects
       (warning (gexp-location gexp)
                (N_ "importing module~{ ~a~} from the host~%"
                    "importing modules~{ ~a~} from the host~%"
                    (length suspects))
                suspects))))

  (gexp-attribute gexp gexp-self-modules module=?
                  #:validate validate-modules))

(define (gexp-extensions gexp)
  "Return the list of Guile extensions (packages) GEXP relies on.  If (gexp?
GEXP) is false, meaning that GEXP is a plain Scheme object, return the empty
list."
  (gexp-attribute gexp gexp-self-extensions))

(define (self-quoting? x)
  (letrec-syntax ((one-of (syntax-rules ()
                            ((_) #f)
                            ((_ pred rest ...)
                             (or (pred x)
                                 (one-of rest ...))))))
    (one-of symbol? string? keyword? pair? null? array?
            number? boolean? char?)))

(define (lower-inputs inputs system target)
  "Turn any object from INPUTS into a derivation input for SYSTEM or a store
item (a \"source\"); return the corresponding input list as a monadic value.
When TARGET is true, use it as the cross-compilation target triplet."
  (define (store-item? obj)
    (and (string? obj) (store-path? obj)))

  (define filterm
    (lift1 (cut filter ->bool <>) %store-monad))

  (with-monad %store-monad
    (>>= (mapm/accumulate-builds
          (match-lambda
            (($ <gexp-input> (? store-item? item))
             (return item))
            (($ <gexp-input> thing output native?)
             (mlet %store-monad ((obj (lower-object thing system
                                                    #:target
                                                    (and (not native?)
                                                         target))))
               (return (match obj
                         ((? derivation? drv)
                          (derivation-input drv (list output)))
                         ((? store-item? item)
                          item)
                         ((? self-quoting?)
                          ;; Some inputs such as <system-binding> can lower to
                          ;; a self-quoting object that FILTERM will filter
                          ;; out.
                          #f))))))
          inputs)
         filterm)))

(define* (lower-reference-graphs graphs #:key system target)
  "Given GRAPHS, a list of (FILE-NAME INPUT ...) lists for use as a
#:reference-graphs argument, lower it such that each INPUT is replaced by the
corresponding <derivation-input> or store item."
  (define tuple->gexp-input
    (match-lambda
      ((thing)
       (%gexp-input thing "out" (not target)))
      ((thing output)
       (%gexp-input thing output (not target)))))

  (match graphs
    (((file-names . inputs) ...)
     (mlet %store-monad ((inputs (lower-inputs (map tuple->gexp-input inputs)
                                               system target)))
       (return (map cons file-names inputs))))))

(define* (lower-references lst #:key system target)
  "Based on LST, a list of output names and packages, return a list of output
names and file names suitable for the #:allowed-references argument to
'derivation'."
  (with-monad %store-monad
    (define lower
      (match-lambda
       ((? string? output)
        (return output))
       (($ <gexp-input> thing output native?)
        (mlet %store-monad ((drv (lower-object thing system
                                               #:target (if native?
                                                            #f target))))
          (return (derivation->output-path drv output))))
       (thing
        (mlet %store-monad ((drv (lower-object thing system
                                               #:target target)))
          (return (derivation->output-path drv))))))

    (mapm/accumulate-builds lower lst)))

(define default-guile-derivation
  ;; Here we break the abstraction by talking to the higher-level layer.
  ;; Thus, do the resolution lazily to hide the circular dependency.
  (let ((proc (delay
                (let ((iface (resolve-interface '(guix packages))))
                  (module-ref iface 'default-guile-derivation)))))
    (lambda (system)
      ((force proc) system))))

;; Representation of a gexp instantiated for a given target and system.
;; It's an intermediate representation between <gexp> and <derivation>.
(define-record-type <lowered-gexp>
  (lowered-gexp sexp inputs sources guile load-path load-compiled-path)
  lowered-gexp?
  (sexp                lowered-gexp-sexp)         ;sexp
  (inputs              lowered-gexp-inputs)       ;list of <derivation-input>
  (sources             lowered-gexp-sources)      ;list of store items
  (guile               lowered-gexp-guile)        ;<derivation-input> | #f
  (load-path           lowered-gexp-load-path)    ;list of store items
  (load-compiled-path  lowered-gexp-load-compiled-path)) ;list of store items

(define* (imported+compiled-modules modules system
                                    #:key (extensions '())
                                    deprecation-warnings guile
                                    (module-path %load-path))
  "Return a pair where the first element is the imported MODULES and the
second element is the derivation to compile them."
  (mcached equal?
           (mlet %store-monad ((modules  (if (pair? modules)
                                             (imported-modules modules
                                                               #:guile guile
                                                               #:system system
                                                               #:module-path module-path)
                                             (return #f)))
                               (compiled (if (pair? modules)
                                             (compiled-modules modules
                                                               #:system system
                                                               #:module-path module-path
                                                               #:extensions extensions
                                                               #:guile guile
                                                               #:deprecation-warnings
                                                               deprecation-warnings)
                                             (return #f))))
             (return (cons modules compiled)))
           modules
           system extensions guile deprecation-warnings module-path))

(define (sexp->string sexp)
  "Like 'object->string', but deterministic and slightly faster."
  ;; Explicitly use UTF-8 for determinism, and also because UTF-8 output is
  ;; faster.
  (with-fluids ((%default-port-encoding "UTF-8"))
    (call-with-output-string
     (lambda (port)
       (write sexp port)))))

(define* (lower-gexp exp
                     #:key
                     (module-path %load-path)
                     (system (%current-system))
                     (target 'current)
                     (graft? (%graft?))
                     (guile-for-build (%guile-for-build))
                     (effective-version "3.0")

                     deprecation-warnings)
  "*Note: This API is subject to change; use at your own risk!*

Lower EXP, a gexp, instantiating it for SYSTEM and TARGET.  Return a
<lowered-gexp> ready to be used.

Lowered gexps are an intermediate representation that's useful for
applications that deal with gexps outside in a way that is disconnected from
derivations--e.g., code evaluated for its side effects."
  (define %modules
    (delete-duplicates (gexp-modules exp)))

  (define (search-path modules extensions suffix)
    (append (match modules
              ((? derivation? drv)
               (list (derivation->output-path drv)))
              (#f
               '())
              ((? store-path? item)
               (list item)))
            (map (lambda (extension)
                   (string-append (match extension
                                    ((? derivation? drv)
                                     (derivation->output-path drv))
                                    ((? store-path? item)
                                     item))
                                  suffix))
                 extensions)))

  (mlet* %store-monad ( ;; The following binding forces '%current-system' and
                       ;; '%current-target-system' to be looked up at >>=
                       ;; time.
                       (graft?    (set-grafting graft?))

                       (system -> (or system (%current-system)))
                       (target -> (if (eq? target 'current)
                                      (%current-target-system)
                                      target))
                       (guile     (if guile-for-build
                                      (return guile-for-build)
                                      (default-guile-derivation system)))
                       (inputs   (lower-inputs (gexp-inputs exp)
                                               system target))
                       (sexp     (gexp->sexp exp system target))
                       (extensions -> (gexp-extensions exp))
                       (exts     (mapm %store-monad
                                       (lambda (obj)
                                         (lower-object obj system
                                                       #:target #f))
                                       extensions))
                       (modules+compiled (imported+compiled-modules
                                          %modules system
                                          #:extensions extensions
                                          #:deprecation-warnings
                                          deprecation-warnings
                                          #:guile guile
                                          #:module-path module-path))
                       (modules ->  (car modules+compiled))
                       (compiled -> (cdr modules+compiled)))
    (define load-path
      (search-path modules exts
                   (string-append "/share/guile/site/" effective-version)))

    (define load-compiled-path
      (search-path compiled exts
                   (string-append "/lib/guile/" effective-version
                                  "/site-ccache")))

    (mbegin %store-monad
      (set-grafting graft?)                       ;restore the initial setting
      (return (lowered-gexp sexp
                            `(,@(if (derivation? modules)
                                    (list (derivation-input modules))
                                    '())
                              ,@(if compiled
                                    (list (derivation-input compiled))
                                    '())
                              ,@(map derivation-input exts)
                              ,@(filter derivation-input? inputs))
                            (filter string? (cons modules inputs))
                            (derivation-input guile '("out"))
                            load-path
                            load-compiled-path)))))

(define* (gexp->derivation name exp
                           #:key
                           system (target 'current)
                           hash hash-algo recursive?
                           (env-vars '())
                           (modules '())
                           (module-path %load-path)
                           (guile-for-build (%guile-for-build))
                           (effective-version "3.0")
                           (graft? (%graft?))
                           references-graphs
                           allowed-references disallowed-references
                           leaked-env-vars
                           local-build? (substitutable? #t)
                           (properties '())
                           deprecation-warnings
                           (script-name (string-append name "-builder")))
  "Return a derivation NAME that runs EXP (a gexp) with GUILE-FOR-BUILD (a
derivation) on SYSTEM; EXP is stored in a file called SCRIPT-NAME.  When
TARGET is true, it is used as the cross-compilation target triplet for
packages referred to by EXP.

MODULES is deprecated in favor of 'with-imported-modules'.  Its meaning is to
make MODULES available in the evaluation context of EXP; MODULES is a list of
names of Guile modules searched in MODULE-PATH to be copied in the store,
compiled, and made available in the load path during the execution of
EXP---e.g., '((guix build utils) (guix build gnu-build-system)).

EFFECTIVE-VERSION determines the string to use when adding extensions of
EXP (see 'with-extensions') to the search path---e.g., \"2.2\".

GRAFT? determines whether packages referred to by EXP should be grafted when
applicable.

When REFERENCES-GRAPHS is true, it must be a list of tuples of one of the
following forms:

  (FILE-NAME PACKAGE)
  (FILE-NAME PACKAGE OUTPUT)
  (FILE-NAME DERIVATION)
  (FILE-NAME DERIVATION OUTPUT)
  (FILE-NAME STORE-ITEM)

The right-hand-side of each element of REFERENCES-GRAPHS is automatically made
an input of the build process of EXP.  In the build environment, each
FILE-NAME contains the reference graph of the corresponding item, in a simple
text format.

ALLOWED-REFERENCES must be either #f or a list of output names and packages.
In the latter case, the list denotes store items that the result is allowed to
refer to.  Any reference to another store item will lead to a build error.
Similarly for DISALLOWED-REFERENCES, which can list items that must not be
referenced by the outputs.

DEPRECATION-WARNINGS determines whether to show deprecation warnings while
compiling modules.  It can be #f, #t, or 'detailed.

The other arguments are as for 'derivation'."
  (define outputs (gexp-outputs exp))
  (define requested-graft? graft?)

  (define (graphs-file-names graphs)
    ;; Return a list of (FILE-NAME . STORE-PATH) pairs made from GRAPHS.
    (map (match-lambda
           ((file-name . (? derivation-input? input))
            (cons file-name (first (derivation-input-output-paths input))))
           ((file-name . (? string? item))
            (cons file-name item)))
         graphs))

  (define (add-modules exp modules)
    (if (null? modules)
        exp
        (make-gexp (gexp-references exp)
                   (append modules (gexp-self-modules exp))
                   (gexp-self-extensions exp)
                   (gexp-proc exp)
                   (gexp-location exp))))

  (mlet* %store-monad ( ;; The following binding forces '%current-system' and
                       ;; '%current-target-system' to be looked up at >>=
                       ;; time.
                       (graft?    (set-grafting graft?))

                       (system -> (or system (%current-system)))
                       (target -> (if (eq? target 'current)
                                      (%current-target-system)
                                      target))
                       (exp ->    (add-modules exp modules))
                       (lowered   (lower-gexp exp
                                              #:module-path module-path
                                              #:system system
                                              #:target target
                                              #:graft? requested-graft?
                                              #:guile-for-build
                                              guile-for-build
                                              #:effective-version
                                              effective-version
                                              #:deprecation-warnings
                                              deprecation-warnings))

                       (graphs   (if references-graphs
                                     (lower-reference-graphs references-graphs
                                                             #:system system
                                                             #:target target)
                                     (return #f)))
                       (allowed  (if allowed-references
                                     (lower-references allowed-references
                                                       #:system system
                                                       #:target target)
                                     (return #f)))
                       (disallowed (if disallowed-references
                                       (lower-references disallowed-references
                                                         #:system system
                                                         #:target target)
                                       (return #f)))
                       (guile -> (lowered-gexp-guile lowered))
                       (builder  (text-file script-name
                                            (sexp->string
                                             (lowered-gexp-sexp lowered)))))
    (mbegin %store-monad
      (set-grafting graft?)                       ;restore the initial setting
      (raw-derivation name
                      (string-append (derivation-input-output-path guile)
                                     "/bin/guile")
                      `("--no-auto-compile"
                        ,@(append-map (lambda (directory)
                                        `("-L" ,directory))
                                      (lowered-gexp-load-path lowered))
                        ,@(append-map (lambda (directory)
                                        `("-C" ,directory))
                                      (lowered-gexp-load-compiled-path lowered))
                        ,builder)
                      #:outputs outputs
                      #:env-vars env-vars
                      #:system system
                      #:inputs `(,guile
                                 ,@(lowered-gexp-inputs lowered)
                                 ,@(match graphs
                                     (((_ . inputs) ...)
                                      (filter derivation-input? inputs))
                                     (#f '())))
                      #:sources `(,builder
                                  ,@(if (and (string? modules)
                                             (store-path? modules))
                                        (list modules)
                                        '())
                                  ,@(lowered-gexp-sources lowered)
                                  ,@(match graphs
                                      (((_ . inputs) ...)
                                       (filter string? inputs))
                                      (#f '())))

                      #:hash hash #:hash-algo hash-algo #:recursive? recursive?
                      #:references-graphs (and=> graphs graphs-file-names)
                      #:allowed-references allowed
                      #:disallowed-references disallowed
                      #:leaked-env-vars leaked-env-vars
                      #:local-build? local-build?
                      #:substitutable? substitutable?
                      #:properties properties))))

(define (fold/tree proc seed lst)
  "Like 'fold', but recurse into sub-lists of LST and accept improper lists."
  (let loop ((obj lst)
             (result seed))
    (match obj
      ((head . tail)
       (loop tail (loop head result)))
      (_
       (proc obj result)))))

(define (gexp-inputs exp)
  "Return the list of <gexp-input> for EXP."
  (define set-gexp-input-native?
    (match-lambda
      (($ <gexp-input> thing output)
       (%gexp-input thing output #t))))

  (define (interesting? obj)
    (or (file-like? obj)
        (and (string? obj) (direct-store-path? obj))))

  (define (add-reference-inputs ref result)
    (match ref
      (($ <gexp-input> (? gexp? exp) _ #t)
       (append (map set-gexp-input-native? (gexp-inputs exp))
               result))
      (($ <gexp-input> (? gexp? exp) _ #f)
       (append (gexp-inputs exp) result))
      (($ <gexp-input> (? string? str))
       (if (direct-store-path? str)
           (cons ref result)
           result))
      (($ <gexp-input> (? struct? thing) output n?)
       (if (lookup-compiler thing)
           ;; THING is a derivation, or a package, or an origin, etc.
           (cons ref result)
           result))
      (($ <gexp-input> (? pair? lst) output n?)
       ;; XXX: Scan LST for inputs.  Inherit N?.
       (fold/tree (lambda (obj result)
                    (match obj
                      ((? gexp-input? x)
                       (cons (%gexp-input (gexp-input-thing x)
                                          (gexp-input-output x)
                                          n?)
                             result))
                      ((? interesting? x)
                       (cons (%gexp-input x "out" n?) result))
                      ((? gexp? x)
                       (append (gexp-inputs x) result))
                      (_
                       result)))
                  result
                  lst))
      (_
       ;; Ignore references to other kinds of objects.
       result)))

  (fold-right add-reference-inputs
              '()
              (gexp-references exp)))

(define (gexp-outputs exp)
  "Return the outputs referred to by EXP as a list of strings."
  (define (add-reference-output ref result)
    (match ref
      (($ <gexp-output> name)
       (cons name result))
      (($ <gexp-input> (? gexp? exp))
       (append (gexp-outputs exp) result))
      (($ <gexp-input> (? pair? lst))
       ;; XXX: Scan LST for outputs.
       (fold/tree (lambda (obj result)
                    (match obj
                      (($ <gexp-output> name) (cons name result))
                      ((? gexp? x) (append (gexp-outputs x) result))
                      (_ result)))
                  result
                  lst))
      (_
       result)))

  (delete-duplicates
   (fold add-reference-output '() (gexp-references exp))))

(define (gexp->sexp exp system target)
  "Return (monadically) the sexp corresponding to EXP for the given OUTPUT,
and in the current monad setting (system type, etc.)"
  (define* (reference->sexp ref #:optional native?)
    (with-monad %store-monad
      (match ref
        (($ <gexp-output> output)
         ;; Output file names are not known in advance but the daemon defines
         ;; an environment variable for each of them at build time, so use
         ;; that trick.
         (return `((@ (guile) getenv) ,output)))
        (($ <gexp-input> (? gexp? exp) output n?)
         (gexp->sexp exp
                     system (if (or n? native?) #f target)))
        (($ <gexp-input> (refs ...) output n?)
         (mapm %store-monad
               (lambda (ref)
                 ;; XXX: Automatically convert REF to an gexp-input.
                 (if (or (symbol? ref) (number? ref)
                         (boolean? ref) (null? ref) (array? ref))
                     (return ref)
                     (reference->sexp
                      (if (gexp-input? ref)
                          ref
                          (%gexp-input ref "out" n?))
                      (or n? native?))))
               refs))
        (($ <gexp-input> (? struct? thing) output n?)
         (let ((target (if (or n? native?) #f target)))
           (lower+expand-object thing system
                                #:target target
                                #:output output)))
        (($ <gexp-input> (? self-quoting? x))
         (return x))
        (($ <gexp-input> x)
         (raise (condition (&gexp-input-error (input x)))))
        (x
         (return x)))))

  (mlet %store-monad
      ((args (mapm %store-monad
                   reference->sexp (gexp-references exp))))
    (return (apply (gexp-proc exp) args))))

(define-syntax-parameter current-imported-modules
  ;; Current list of imported modules.
  (identifier-syntax '()))

(define-syntax-rule (with-imported-modules modules body ...)
  "Mark the gexps defined in BODY... as requiring MODULES in their execution
environment."
  (syntax-parameterize ((current-imported-modules
                         (identifier-syntax modules)))
    body ...))

(define-syntax-parameter current-imported-extensions
  ;; Current list of extensions.
  (identifier-syntax '()))

(define-syntax-rule (with-extensions extensions body ...)
  "Mark the gexps defined in BODY... as requiring EXTENSIONS in their
execution environment."
  (syntax-parameterize ((current-imported-extensions
                         (identifier-syntax extensions)))
    body ...))

(define-syntax gexp
  (lambda (s)
    (define (collect-escapes exp)
      ;; Return all the 'ungexp' present in EXP.
      (let loop ((exp    exp)
                 (result '()))
        (syntax-case exp (ungexp
                          ungexp-splicing
                          ungexp-native
                          ungexp-native-splicing)
          ((ungexp _)
           (cons exp result))
          ((ungexp _ _)
           (cons exp result))
          ((ungexp-splicing _ ...)
           (cons exp result))
          ((ungexp-native _ ...)
           (cons exp result))
          ((ungexp-native-splicing _ ...)
           (cons exp result))
          ((exp0 . exp)
           (let ((result (loop #'exp0 result)))
             (loop  #'exp result)))
          (_
           result))))

    (define (escape->ref exp)
      ;; Turn 'ungexp' form EXP into a "reference".
      (syntax-case exp (ungexp ungexp-splicing
                        ungexp-native ungexp-native-splicing
                        output)
        ((ungexp output)
         #'(gexp-output "out"))
        ((ungexp output name)
         #'(gexp-output name))
        ((ungexp thing)
         #'(%gexp-input thing "out" #f))
        ((ungexp drv-or-pkg out)
         #'(%gexp-input drv-or-pkg out #f))
        ((ungexp-splicing lst)
         #'(%gexp-input lst "out" #f))
        ((ungexp-native thing)
         #'(%gexp-input thing "out" #t))
        ((ungexp-native drv-or-pkg out)
         #'(%gexp-input drv-or-pkg out #t))
        ((ungexp-native-splicing lst)
         #'(%gexp-input lst "out" #t))))

    (define (substitute-ungexp exp substs)
      ;; Given EXP, an 'ungexp' or 'ungexp-native' form, substitute it with
      ;; the corresponding form in SUBSTS.
      (match (assoc exp substs)
        ((_ id)
         id)
        (_                                        ;internal error
         (with-syntax ((exp exp))
           #'(syntax-error "error: no 'ungexp' substitution" exp)))))

    (define (substitute-ungexp-splicing exp substs)
      (syntax-case exp ()
        ((exp rest ...)
         (match (assoc #'exp substs)
           ((_ id)
            (with-syntax ((id id))
              #`(append id
                        #,(substitute-references #'(rest ...) substs))))
           (_
            #'(syntax-error "error: no 'ungexp-splicing' substitution"
                            exp))))))

    (define (substitute-references exp substs)
      ;; Return a variant of EXP where all the cars of SUBSTS have been
      ;; replaced by the corresponding cdr.
      (syntax-case exp (ungexp ungexp-native
                        ungexp-splicing ungexp-native-splicing)
        ((ungexp _ ...)
         (substitute-ungexp exp substs))
        ((ungexp-native _ ...)
         (substitute-ungexp exp substs))
        (((ungexp-splicing _ ...) rest ...)
         (substitute-ungexp-splicing exp substs))
        (((ungexp-native-splicing _ ...) rest ...)
         (substitute-ungexp-splicing exp substs))
        ((exp0 . exp)
         #`(cons #,(substitute-references #'exp0 substs)
                 #,(substitute-references #'exp substs)))
        (x #''x)))

    (syntax-case s (ungexp output)
      ((_ exp)
       (let* ((escapes (delete-duplicates (collect-escapes #'exp)))
              (formals (generate-temporaries escapes))
              (sexp    (substitute-references #'exp (zip escapes formals)))
              (refs    (map escape->ref escapes)))
         #`(make-gexp (list #,@refs)
                      current-imported-modules
                      current-imported-extensions
                      (lambda #,formals
                        #,sexp)
                      (current-source-location)))))))


;;;
;;; Module handling.
;;;

(define %utils-module
  ;; This file provides 'mkdir-p', needed to implement 'imported-files' and
  ;; other primitives below.  Note: We give the file name relative to this
  ;; file you are currently reading; 'search-path' could return a file name
  ;; relative to the current working directory.
  (local-file "build/utils.scm"
              "build-utils.scm"))

(define* (imported-files/derivation files
                                    #:key (name "file-import")
                                    (symlink? #f)
                                    (system (%current-system))
                                    (guile (%guile-for-build)))
  "Return a derivation that imports FILES into STORE.  FILES must be a list
of (FINAL-PATH . FILE) pairs.  Each FILE is mapped to FINAL-PATH in the
resulting store path.  FILE can be either a file name, or a file-like object,
as returned by 'local-file' for example.  If SYMLINK? is true, create symlinks
to the source files instead of copying them."
  (define file-pair
    (match-lambda
     ((final-path . (? string? file-name))
      (mlet %store-monad ((file (interned-file file-name
                                               (basename final-path))))
        (return (list final-path file))))
     ((final-path . file-like)
      (mlet %store-monad ((file (lower-object file-like system)))
        (return (list final-path file))))))

  (mlet %store-monad ((files (mapm %store-monad file-pair files)))
    (define build
      (gexp
       (begin
         (primitive-load (ungexp %utils-module))  ;for 'mkdir-p'
         (use-modules (ice-9 match))

         (mkdir (ungexp output)) (chdir (ungexp output))
         (for-each (match-lambda
                    ((final-path store-path)
                     (mkdir-p (dirname final-path))
                     ((ungexp (if symlink? 'symlink 'copy-file))
                      store-path final-path)))
                   '(ungexp files)))))

    ;; TODO: Pass FILES as an environment variable so that BUILD remains
    ;; exactly the same regardless of FILES: less disk space, and fewer
    ;; 'add-to-store' RPCs.
    (gexp->derivation name build
                      #:system system
                      #:guile-for-build guile
                      #:local-build? #t
                      #:substitutable? #f

                      ;; Avoid deprecation warnings about the use of the _IO*
                      ;; constants in (guix build utils).
                      #:env-vars
                      '(("GUILE_WARN_DEPRECATED" . "no")))))

(define* (imported-files files
                         #:key (name "file-import")
                         ;; The following parameters make sense when creating
                         ;; an actual derivation.
                         (system (%current-system))
                         (guile (%guile-for-build)))
  "Import FILES into the store and return the resulting derivation or store
file name (a derivation is created if and only if some elements of FILES are
file-like objects and not local file names.)  FILES must be a list
of (FINAL-PATH . FILE) pairs.  Each FILE is mapped to FINAL-PATH in the
resulting store path.  FILE can be either a file name, or a file-like object,
as returned by 'local-file' for example."
  (if (any (match-lambda
             ((_ . (? struct? source)) #t)
             (_ #f))
           files)
      (imported-files/derivation files #:name name
                                 #:symlink? derivation?
                                 #:system system #:guile guile)
      (interned-file-tree `(,name directory
                                  ,@(file-mapping->tree files)))))

(define* (imported-modules modules
                           #:key (name "module-import")
                           (system (%current-system))
                           (guile (%guile-for-build))
                           (module-path %load-path))
  "Return a derivation that contains the source files of MODULES, a list of
module names such as `(ice-9 q)'.  All of MODULES must be either names of
modules to be found in the MODULE-PATH search path, or a module name followed
by an arrow followed by a file-like object.  For example:

  (imported-modules `((guix build utils)
                      (guix gcrypt)
                      ((guix config) => ,(scheme-file …))))

In this example, the first two modules are taken from MODULE-PATH, and the
last one is created from the given <scheme-file> object."
  (let ((files (map (match-lambda
                      (((module ...) '=> file)
                       (cons (module->source-file-name module)
                             file))
                      ((module ...)
                       (let ((f (module->source-file-name module)))
                         (cons f (search-path* module-path f)))))
                    modules)))
    (imported-files files #:name name
                    #:system system
                    #:guile guile)))

(define* (compiled-modules modules
                           #:key (name "module-import-compiled")
                           (system (%current-system))
                           target
                           (guile (%guile-for-build))
                           (module-path %load-path)
                           (extensions '())
                           (deprecation-warnings #f)
                           (optimization-level 1))
  "Return a derivation that builds a tree containing the `.go' files
corresponding to MODULES.  All the MODULES are built in a context where
they can refer to each other.  When TARGET is true, cross-compile MODULES for
TARGET, a GNU triplet."
  (define total (length modules))

  (mlet %store-monad ((modules (imported-modules modules
                                                 #:system system
                                                 #:guile guile
                                                 #:module-path
                                                 module-path))
                      (extensions (mapm %store-monad
                                        (lambda (extension)
                                          (lower-object extension system
                                                        #:target #f))
                                        extensions)))
    (define build
      (gexp-with-hidden-inputs
       (gexp
        (begin
          (primitive-load (ungexp %utils-module)) ;for 'mkdir-p'

          (use-modules (ice-9 ftw)
                       (ice-9 format)
                       (srfi srfi-1)
                       (srfi srfi-26)
                       (system base target)
                       (system base compile))

          (define modules
            (getenv "modules"))

          (define total
            (string->number (getenv "module count")))

          (define extensions
            (string-split (getenv "extensions") #\space))

          (define target
            (getenv "target"))

          (define optimization-level
            (string->number (getenv "optimization level")))

          (define optimizations-for-level
            (or (and=> (false-if-exception
                        (resolve-interface '(system base optimize)))
                       (lambda (iface)
                         (module-ref iface 'optimizations-for-level))) ;Guile 3.0
                (const '())))

          (define (regular? file)
            (not (member file '("." ".."))))

          (define (process-entry entry output processed)
            (if (file-is-directory? entry)
                (let ((output (string-append output "/" (basename entry))))
                  (mkdir-p output)
                  (process-directory entry output processed))
                (let* ((base   (basename entry ".scm"))
                       (output (string-append output "/" base ".go")))
                  (format #t "[~2@a/~2@a] Compiling '~a'...~%"
                          (+ 1 processed total)
                          (* total 2)
                          entry)

                  (with-target (or target %host-type)
                               (lambda ()
                                 (compile-file entry
                                               #:output-file output
                                               #:opts
                                               `(,@%auto-compilation-options
                                                 ,@(optimizations-for-level
                                                    optimization-level)))))

                  (+ 1 processed))))

          (define (process-directory directory output processed)
            (let ((entries (map (cut string-append directory "/" <>)
                                (scandir directory regular?))))
              (fold (cut process-entry <> output <>)
                    processed
                    entries)))

          (define* (load-from-directory directory
                                        #:optional (loaded 0))
            "Load all the source files found in DIRECTORY."
            ;; XXX: This works around <https://bugs.gnu.org/15602>.
            (let ((entries (map (cut string-append directory "/" <>)
                                (scandir directory regular?))))
              (fold (lambda (file loaded)
                      (if (file-is-directory? file)
                          (load-from-directory file loaded)
                          (begin
                            (format #t "[~2@a/~2@a] Loading '~a'...~%"
                                    (+ 1 loaded) (* 2 total)
                                    file)
                            (save-module-excursion
                             (lambda ()
                               (primitive-load file)))
                            (+ 1 loaded))))
                    loaded
                    entries)))

          (setvbuf (current-output-port)
                   (cond-expand (guile-2.2 'line) (else _IOLBF)))

          (define mkdir-p
            ;; Capture 'mkdir-p'.
            (@ (guix build utils) mkdir-p))

          ;; Remove environment variables for internal consumption.
          (unsetenv "modules")
          (unsetenv "module count")
          (unsetenv "extensions")
          (unsetenv "target")
          (unsetenv "optimization level")

          ;; Add EXTENSIONS to the search path.
          (set! %load-path
            (append (map (lambda (extension)
                           (string-append extension
                                          "/share/guile/site/"
                                          (effective-version)))
                         extensions)
                    %load-path))
          (set! %load-compiled-path
            (append (map (lambda (extension)
                           (string-append extension "/lib/guile/"
                                          (effective-version)
                                          "/site-ccache"))
                         extensions)
                    %load-compiled-path))

          (set! %load-path (cons modules %load-path))

          ;; Above we loaded our own (guix build utils) but now we may need to
          ;; load a compile a different one.  Thus, force a reload.
          (let ((utils (string-append modules
                                      "/guix/build/utils.scm")))
            (when (file-exists? utils)
              (load utils)))

          (mkdir (ungexp output))
          (chdir modules)

          (load-from-directory ".")
          (process-directory "." (ungexp output) 0)))
       (append (map gexp-input extensions)
               (list (gexp-input modules)))))

    (gexp->derivation name build
                      #:script-name "compile-modules"
                      #:system system
                      #:target target
                      #:guile-for-build guile
                      #:local-build? #t
                      #:env-vars
                      `(("modules"
                         . ,(if (derivation? modules)
                                (derivation->output-path modules)
                                modules))
                        ("module count" . ,(number->string total))
                        ("extensions"
                         . ,(string-join
                             (map (match-lambda
                                    ((? derivation? drv)
                                     (derivation->output-path drv))
                                    ((? string? str) str))
                                  extensions)))
                        ("optimization level"
                         . ,(number->string optimization-level))
                        ,@(if target
                              `(("target" . ,target))
                              '())
                        ,@(case deprecation-warnings
                            ((#f)
                             '(("GUILE_WARN_DEPRECATED" . "no")))
                            ((detailed)
                             '(("GUILE_WARN_DEPRECATED" . "detailed")))
                            (else
                             '()))))))


;;;
;;; Convenience procedures.
;;;

(define (default-guile)
  ;; Lazily resolve 'guile-3.0' (not 'guile-final' because this is for
  ;; programs returned by 'program-file' and we don't want to keep references
  ;; to several Guile packages).  This module must not refer to (gnu …)
  ;; modules directly, to avoid circular dependencies, hence this hack.
  (module-ref (resolve-interface '(gnu packages guile))
              'guile-3.0))

(define* (load-path-expression modules #:optional (path %load-path)
                               #:key (extensions '()) system target
                               (guile (default-guile)))
  "Return as a monadic value a gexp that sets '%load-path' and
'%load-compiled-path' to point to MODULES, a list of module names.  MODULES
are searched for in PATH.  Return #f when MODULES and EXTENSIONS are empty.
Assume MODULES are compiled with GUILE."
  (if (and (null? modules) (null? extensions))
      (with-monad %store-monad
        (return #f))
      (mlet* %store-monad ((guile    (lower-object guile system #:target #f))
                           (compiled (compiled-modules modules
                                                       #:guile guile
                                                       #:extensions extensions
                                                       #:module-path path
                                                       #:system system
                                                       #:target target))
                           (modules  (imported-modules modules
                                                       #:guile guile
                                                       #:module-path path
                                                       #:system system)))
        (return
         (gexp (eval-when (expand load eval)
                 ;; Augment the load paths and delete duplicates.  Do that
                 ;; without loading (srfi srfi-1) or anything.
                 (let ((extensions '((ungexp-splicing extensions)))
                       (prepend (lambda (items lst)
                                  ;; This is O(N²) but N is typically small.
                                  (let loop ((items items)
                                             (lst lst))
                                    (if (null? items)
                                        lst
                                        (loop (cdr items)
                                              (cons (car items)
                                                    (delete (car items) lst))))))))
                   (set! %load-path
                     (prepend (cons (ungexp modules)
                                    (map (lambda (extension)
                                           (string-append extension
                                                          "/share/guile/site/"
                                                          (effective-version)))
                                         extensions))
                              %load-path))
                   (set! %load-compiled-path
                     (prepend (cons (ungexp compiled)
                                    (map (lambda (extension)
                                           (string-append extension
                                                          "/lib/guile/"
                                                          (effective-version)
                                                          "/site-ccache"))
                                         extensions))
                              %load-compiled-path)))))))))

(define* (input-tuples->gexp inputs #:key native?)
  "Given INPUTS, a list of label/gexp-input tuples, return a gexp that expands
to an input alist."
  (define references
    (map (match-lambda
           ((label input) input))
         inputs))

  (define labels
    (match inputs
      (((labels . _) ...)
       labels)))

  (define (proc . args)
    (cons 'quote (list (map cons labels args))))

  ;; This gexp is more efficient than an equivalent hand-written gexp: fewer
  ;; allocations, no need to scan long list-valued <gexp-input> records in
  ;; search of file-like objects, etc.
  (make-gexp references '() '() proc
             (source-properties inputs)))

(define (outputs->gexp outputs)
  "Given OUTPUTS, a list of output names, return a gexp that expands to an
output alist."
  (define references
    (map gexp-output outputs))

  (define (proc . args)
    `(list ,@(map (lambda (name)
                    `(cons ,name ((@ (guile) getenv) ,name)))
                  outputs)))

  ;; This gexp is more efficient than an equivalent hand-written gexp.
  (make-gexp references '() '() proc
             (source-properties outputs)))

(define (with-build-variables inputs outputs body)
  "Return a gexp that surrounds BODY with a definition of the legacy
'%build-inputs', '%outputs', and '%output' variables based on INPUTS, a list
of name/gexp-input tuples, and OUTPUTS, a list of strings."

  ;; These two variables are defined for backward compatibility.  They are
  ;; used by package expressions.  These must be top-level defines so that
  ;; 'use-modules' form in BODY that are required for macro expansion work as
  ;; expected.
  (gexp (begin
          (define %build-inputs
            (ungexp (input-tuples->gexp inputs)))
          (define %outputs
            (ungexp (outputs->gexp outputs)))
          (define %output
            (assoc-ref %outputs "out"))

          (ungexp body))))

(define (sexp->gexp sexp)
  "Turn SEXP into a gexp without any references.

Using this is a way for the caller to tell that SEXP doesn't need to be
scanned for file-like objects, thereby reducing processing costs.  This is
particularly useful if SEXP is a long list or a deep tree."
  (make-gexp '() '() '()
             (lambda () sexp)
             (source-properties sexp)))

(define* (gexp->script name exp
                       #:key (guile (default-guile))
                       (module-path %load-path)
                       (system (%current-system))
                       (target 'current))
  "Return an executable script NAME that runs EXP using GUILE, with EXP's
imported modules in its search path.  Look up EXP's modules in MODULE-PATH."
  (mlet* %store-monad ((target (if (eq? target 'current)
                                   (current-target-system)
                                   (return target)))
                       (set-load-path
                        (load-path-expression (gexp-modules exp)
                                              module-path
                                              #:guile guile
                                              #:extensions
                                              (gexp-extensions exp)
                                              #:system system
                                              #:target target))
                       (guile-for-build
                        (lower-object guile system #:target #f)))
    (gexp->derivation name
                      (gexp
                       (call-with-output-file (ungexp output)
                         (lambda (port)
                           ;; Note: that makes a long shebang.  When the store
                           ;; is /gnu/store, that fits within the 128-byte
                           ;; limit imposed by Linux, but that may go beyond
                           ;; when running tests.
                           (format port
                                   "#!~a/bin/guile --no-auto-compile~%!#~%"
                                   (ungexp guile))

                           (ungexp-splicing
                            (if set-load-path
                                (gexp ((write '(ungexp set-load-path) port)))
                                (gexp ())))

                           (write '(ungexp exp) port)
                           (chmod port #o555))))
                      #:system system
                      #:target target
                      #:module-path module-path
                      #:guile-for-build guile-for-build

                      ;; These derivations are not worth offloading or
                      ;; substituting.
                      #:local-build? #t
                      #:substitutable? #f)))

(define* (gexp->file name exp #:key
                     (set-load-path? #t)
                     (module-path %load-path)
                     (splice? #f)
                     (system (%current-system))
                     (target 'current))
  "Return a derivation that builds a file NAME containing EXP.  When SPLICE?
is true, EXP is considered to be a list of expressions that will be spliced in
the resulting file.

When SET-LOAD-PATH? is true, emit code in the resulting file to set
'%load-path' and '%load-compiled-path' to honor EXP's imported modules.
Lookup EXP's modules in MODULE-PATH."
  (define modules (gexp-modules exp))
  (define extensions (gexp-extensions exp))

  (mlet* %store-monad
      ((target (if (eq? target 'current)
                   (current-target-system)
                   (return target)))
       (no-load-path? -> (or (not set-load-path?)
                             (and (null? modules)
                                  (null? extensions))))
       (set-load-path
        (load-path-expression modules module-path
                              #:extensions extensions
                              #:system system
                              #:target target)))
    (if no-load-path?
        (gexp->derivation name
                          (gexp
                           (call-with-output-file (ungexp output)
                             (lambda (port)
                               (for-each
                                (lambda (exp)
                                  (write exp port))
                                '(ungexp (if splice?
                                             exp
                                             (gexp ((ungexp exp)))))))))
                          #:local-build? #t
                          #:substitutable? #f
                          #:system system
                          #:target target)
        (gexp->derivation name
                          (gexp
                           (call-with-output-file (ungexp output)
                             (lambda (port)
                               (write '(ungexp set-load-path) port)
                               (for-each
                                (lambda (exp)
                                  (write exp port))
                                '(ungexp (if splice?
                                             exp
                                             (gexp ((ungexp exp)))))))))
                          #:module-path module-path
                          #:local-build? #t
                          #:substitutable? #f
                          #:system system
                          #:target target))))

(define* (text-file* name #:rest text)
  "Return as a monadic value a derivation that builds a text file containing
all of TEXT.  TEXT may list, in addition to strings, objects of any type that
can be used in a gexp: packages, derivations, local file objects, etc.  The
resulting store file holds references to all these."
  (define builder
    (gexp (call-with-output-file (ungexp output "out")
            (lambda (port)
              (display (string-append (ungexp-splicing text)) port)))))

  (gexp->derivation name builder
                    #:local-build? #t
                    #:substitutable? #f))

(define* (mixed-text-file name #:rest text)
  "Return an object representing store file NAME containing TEXT.  TEXT is a
sequence of strings and file-like objects, as in:

  (mixed-text-file \"profile\"
                   \"export PATH=\" coreutils \"/bin:\" grep \"/bin\")

This is the declarative counterpart of 'text-file*'."
  (define build
    (gexp (call-with-output-file (ungexp output "out")
            (lambda (port)
              (set-port-encoding! port "UTF-8")
              (display (string-append (ungexp-splicing text)) port)))))

  (computed-file name build))

(define (file-union name files)
  "Return a <computed-file> that builds a directory containing all of FILES.
Each item in FILES must be a two-element list where the first element is the
file name to use in the new directory, and the second element is a gexp
denoting the target file.  Here's an example:

  (file-union \"etc\"
              `((\"hosts\" ,(plain-file \"hosts\"
                                        \"127.0.0.1 localhost\"))
                (\"bashrc\" ,(plain-file \"bashrc\"
                                         \"alias ls='ls --color'\"))
                (\"libvirt/qemu.conf\" ,(plain-file \"qemu.conf\" \"\"))))

This yields an 'etc' directory containing these two files."
  (computed-file name
                 (with-imported-modules '((guix build utils))
                   (gexp
                    (begin
                      (use-modules (guix build utils))

                      (mkdir (ungexp output))
                      (chdir (ungexp output))
                      (ungexp-splicing
                       (map (match-lambda
                              ((target source)
                               (gexp
                                (begin
                                  ;; Stat the source to abort early if it does
                                  ;; not exist.
                                  (stat (ungexp source))

                                  (mkdir-p (dirname (ungexp target)))
                                  (symlink (ungexp source)
                                           (ungexp target))))))
                            files)))))))

(define* (directory-union name things
                          #:key (copy? #f) (quiet? #f)
                          (resolve-collision 'warn-about-collision))
  "Return a directory that is the union of THINGS, where THINGS is a list of
file-like objects denoting directories.  For example:

  (directory-union \"guile+emacs\" (list guile emacs))

yields a directory that is the union of the 'guile' and 'emacs' packages.

Call RESOLVE-COLLISION when several files collide, passing it the list of
colliding files.  RESOLVE-COLLISION must return the chosen file or #f, in
which case the colliding entry is skipped altogether.

When HARD-LINKS? is true, create hard links instead of symlinks.  When QUIET?
is true, the derivation will not print anything."
  (define symlink
    (if copy?
        (gexp (lambda (old new)
                (if (file-is-directory? old)
                    (symlink old new)
                    (copy-file old new))))
        (gexp symlink)))

  (define log-port
    (if quiet?
        (gexp (%make-void-port "w"))
        (gexp (current-error-port))))

  (match things
    ((one)
     ;; Only one thing; return it.
     one)
    (_
     (computed-file name
                    (with-imported-modules '((guix build union))
                      (gexp (begin
                              (use-modules (guix build union)
                                           (srfi srfi-1)) ;for 'first' and 'last'

                              (union-build (ungexp output)
                                           '(ungexp things)

                                           #:log-port (ungexp log-port)
                                           #:symlink (ungexp symlink)
                                           #:resolve-collision
                                           (ungexp resolve-collision)))))))))


;;;
;;; Syntactic sugar.
;;;

(eval-when (expand load eval)
  (define* (read-ungexp chr port #:optional native?)
    "Read an 'ungexp' or 'ungexp-splicing' form from PORT.  When NATIVE? is
true, use 'ungexp-native' and 'ungexp-native-splicing' instead."
    (define unquote-symbol
      (match (peek-char port)
        (#\@
         (read-char port)
         (if native?
             'ungexp-native-splicing
             'ungexp-splicing))
        (_
         (if native?
             'ungexp-native
             'ungexp))))

    (match (read port)
      ((? symbol? symbol)
       (let ((str (symbol->string symbol)))
         (match (string-index-right str #\:)
           (#f
            `(,unquote-symbol ,symbol))
           (colon
            (let ((name   (string->symbol (substring str 0 colon)))
                  (output (substring str (+ colon 1))))
              `(,unquote-symbol ,name ,output))))))
      (x
       `(,unquote-symbol ,x))))

  (define (read-gexp chr port)
    "Read a 'gexp' form from PORT."
    `(gexp ,(read port)))

  ;; Extend the reader
  (read-hash-extend #\~ read-gexp)
  (read-hash-extend #\$ read-ungexp)
  (read-hash-extend #\+ (cut read-ungexp <> <> #t)))

;;; gexp.scm ends here
