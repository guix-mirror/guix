;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Jan Nieuwenhuizen <janneke@gnu.org>
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
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:export (gexp
            gexp?
            with-imported-modules
            with-extensions

            gexp-input
            gexp-input?

            local-file
            local-file?
            local-file-file
            local-file-absolute-file-name
            local-file-name
            local-file-recursive?

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

            load-path-expression
            gexp-modules

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

            lower-inputs

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
  (make-gexp references modules extensions proc)
  gexp?
  (references gexp-references)                    ;list of <gexp-input>
  (modules    gexp-self-modules)                  ;list of module names
  (extensions gexp-self-extensions)               ;list of lowerable things
  (proc       gexp-proc))                         ;procedure

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
  (format port " ~a>"
          (number->string (object-address gexp) 16)))

(set-record-type-printer! <gexp> write-gexp)


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
     file)))

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
                       #:key target)
  "Return as a value in %STORE-MONAD the derivation or store item
corresponding to OBJ for SYSTEM, cross-compiling for TARGET if TARGET is true.
OBJ must be an object that has an associated gexp compiler, such as a
<package>."
  (match (lookup-compiler obj)
    (#f
     (raise (condition (&gexp-input-error (input obj)))))
    (lower
     ;; Cache in STORE the result of lowering OBJ.
     (mlet %store-monad ((graft? (grafting?)))
       (mcached (let ((lower (lookup-compiler obj)))
                  (lower obj system target))
                obj
                system target graft?)))))

(define-syntax define-gexp-compiler
  (syntax-rules (=> compiler expander)
    "Define NAME as a compiler for objects matching PREDICATE encountered in
gexps.

In the simplest form of the macro, BODY must return a derivation for PARAM, an
object that matches PREDICATE, for SYSTEM and TARGET (the latter of which is
#f except when cross-compiling.)

The more elaborate form allows you to specify an expander:

  (define-gexp-compiler something something?
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
                      #:key recursive? (select? true))
  ;; This intermediate procedure is part of our ABI, but the underlying
  ;; %%LOCAL-FILE is not.
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
    (syntax-case s ()
      ((_ file rest ...)
       #'(%local-file file
                      (delay (absolute-file-name file (current-source-directory)))
                      rest ...))
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
                        #:key guile (options '(#:local-build? #t)))
  "Return an object representing the store item NAME, a file or directory
computed by GEXP.  OPTIONS is a list of additional arguments to pass
to 'gexp->derivation'.

This is the declarative counterpart of 'gexp->derivation'."
  (%computed-file name gexp guile options))

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
                   #:guile (or guile (default-guile))))))

(define-record-type <scheme-file>
  (%scheme-file name gexp splice?)
  scheme-file?
  (name       scheme-file-name)                  ;string
  (gexp       scheme-file-gexp)                  ;gexp
  (splice?    scheme-file-splice?))              ;Boolean

(define* (scheme-file name gexp #:key splice?)
  "Return an object representing the Scheme file NAME that contains GEXP.

This is the declarative counterpart of 'gexp->file'."
  (%scheme-file name gexp splice?))

(define-gexp-compiler (scheme-file-compiler (file <scheme-file>)
                                            system target)
  ;; Compile FILE by returning a derivation that builds the file.
  (match file
    (($ <scheme-file> name gexp splice?)
     (gexp->file name gexp #:splice? splice?))))

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
                   (let* ((expand (lookup-expander base))
                          (base   (expand base lowered output)))
                     (string-append base (string-concatenate suffix)))))))


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

(define* (gexp-attribute gexp self-attribute #:optional (equal? equal?))
  "Recurse on GEXP and the expressions it refers to, summing the items
returned by SELF-ATTRIBUTE, a procedure that takes a gexp.  Use EQUAL? as the
second argument to 'delete-duplicates'."
  (if (gexp? gexp)
      (delete-duplicates
       (append (self-attribute gexp)
               (append-map (match-lambda
                             (($ <gexp-input> (? gexp? exp))
                              (gexp-attribute exp self-attribute))
                             (($ <gexp-input> (lst ...))
                              (append-map (lambda (item)
                                            (if (gexp? item)
                                                (gexp-attribute item
                                                                self-attribute)
                                                '()))
                                          lst))
                             (_
                              '()))
                           (gexp-references gexp)))
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

  (gexp-attribute gexp gexp-self-modules module=?))

(define (gexp-extensions gexp)
  "Return the list of Guile extensions (packages) GEXP relies on.  If (gexp?
GEXP) is false, meaning that GEXP is a plain Scheme object, return the empty
list."
  (gexp-attribute gexp gexp-self-extensions))

(define* (lower-inputs inputs
                       #:key system target)
  "Turn any package from INPUTS into a derivation for SYSTEM; return the
corresponding input list as a monadic value.  When TARGET is true, use it as
the cross-compilation target triplet."
  (with-monad %store-monad
    (mapm %store-monad
          (match-lambda
            (((? struct? thing) sub-drv ...)
             (mlet %store-monad ((drv (lower-object
                                       thing system #:target target)))
               (return `(,drv ,@sub-drv))))
            (input
             (return input)))
          inputs)))

(define* (lower-reference-graphs graphs #:key system target)
  "Given GRAPHS, a list of (FILE-NAME INPUT ...) lists for use as a
#:reference-graphs argument, lower it such that each INPUT is replaced by the
corresponding derivation."
  (match graphs
    (((file-names . inputs) ...)
     (mlet %store-monad ((inputs (lower-inputs inputs
                                               #:system system
                                               #:target target)))
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

    (mapm %store-monad lower lst)))

(define default-guile-derivation
  ;; Here we break the abstraction by talking to the higher-level layer.
  ;; Thus, do the resolution lazily to hide the circular dependency.
  (let ((proc (delay
                (let ((iface (resolve-interface '(guix packages))))
                  (module-ref iface 'default-guile-derivation)))))
    (lambda (system)
      ((force proc) system))))

(define* (gexp->derivation name exp
                           #:key
                           system (target 'current)
                           hash hash-algo recursive?
                           (env-vars '())
                           (modules '())
                           (module-path %load-path)
                           (guile-for-build (%guile-for-build))
                           (effective-version "2.2")
                           (graft? (%graft?))
                           references-graphs
                           allowed-references disallowed-references
                           leaked-env-vars
                           local-build? (substitutable? #t)
                           (properties '())

                           ;; TODO: This parameter is transitional; it's here
                           ;; to avoid a full rebuild.  Remove it on the next
                           ;; rebuild cycle.
                           (pre-load-modules? #t)

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
  (define %modules
    (delete-duplicates
     (append modules (gexp-modules exp))))
  (define outputs (gexp-outputs exp))

  (define (graphs-file-names graphs)
    ;; Return a list of (FILE-NAME . STORE-PATH) pairs made from GRAPHS.
    (map (match-lambda
           ;; TODO: Remove 'derivation?' special cases.
           ((file-name (? derivation? drv))
            (cons file-name (derivation->output-path drv)))
           ((file-name (? derivation? drv) sub-drv)
            (cons file-name (derivation->output-path drv sub-drv)))
           ((file-name thing)
            (cons file-name thing)))
         graphs))

  (define (extension-flags extension)
    `("-L" ,(string-append (derivation->output-path extension)
                           "/share/guile/site/" effective-version)
      "-C" ,(string-append (derivation->output-path extension)
                           "/lib/guile/" effective-version "/site-ccache")))

  (mlet* %store-monad ( ;; The following binding forces '%current-system' and
                       ;; '%current-target-system' to be looked up at >>=
                       ;; time.
                       (graft?    (set-grafting graft?))

                       (system -> (or system (%current-system)))
                       (target -> (if (eq? target 'current)
                                      (%current-target-system)
                                      target))
                       (normals  (lower-inputs (gexp-inputs exp)
                                               #:system system
                                               #:target target))
                       (natives  (lower-inputs (gexp-native-inputs exp)
                                               #:system system
                                               #:target #f))
                       (inputs -> (append normals natives))
                       (sexp     (gexp->sexp exp
                                             #:system system
                                             #:target target))
                       (builder  (text-file script-name
                                            (object->string sexp)))
                       (extensions -> (gexp-extensions exp))
                       (exts     (mapm %store-monad
                                       (lambda (obj)
                                         (lower-object obj system))
                                       extensions))
                       (modules  (if (pair? %modules)
                                     (imported-modules %modules
                                                       #:system system
                                                       #:module-path module-path
                                                       #:guile guile-for-build)
                                     (return #f)))
                       (compiled (if (pair? %modules)
                                     (compiled-modules %modules
                                                       #:system system
                                                       #:module-path module-path
                                                       #:extensions extensions
                                                       #:guile guile-for-build
                                                       #:pre-load-modules?
                                                       pre-load-modules?
                                                       #:deprecation-warnings
                                                       deprecation-warnings)
                                     (return #f)))
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
                       (guile    (if guile-for-build
                                     (return guile-for-build)
                                     (default-guile-derivation system))))
    (mbegin %store-monad
      (set-grafting graft?)                       ;restore the initial setting
      (raw-derivation name
                      (string-append (derivation->output-path guile)
                                     "/bin/guile")
                      `("--no-auto-compile"
                        ,@(if (pair? %modules)
                              `("-L" ,(if (derivation? modules)
                                          (derivation->output-path modules)
                                          modules)
                                "-C" ,(derivation->output-path compiled))
                              '())
                        ,@(append-map extension-flags exts)
                        ,builder)
                      #:outputs outputs
                      #:env-vars env-vars
                      #:system system
                      #:inputs `((,guile)
                                 (,builder)
                                 ,@(if modules
                                       `((,modules) (,compiled) ,@inputs)
                                       inputs)
                                 ,@(map list exts)
                                 ,@(match graphs
                                     (((_ . inputs) ...) inputs)
                                     (_ '())))
                      #:hash hash #:hash-algo hash-algo #:recursive? recursive?
                      #:references-graphs (and=> graphs graphs-file-names)
                      #:allowed-references allowed
                      #:disallowed-references disallowed
                      #:leaked-env-vars leaked-env-vars
                      #:local-build? local-build?
                      #:substitutable? substitutable?
                      #:properties properties))))

(define* (gexp-inputs exp #:key native?)
  "Return the input list for EXP.  When NATIVE? is true, return only native
references; otherwise, return only non-native references."
  (define (add-reference-inputs ref result)
    (match ref
      (($ <gexp-input> (? gexp? exp) _ #t)
       (if native?
           (append (gexp-inputs exp)
                   (gexp-inputs exp #:native? #t)
                   result)
           result))
      (($ <gexp-input> (? gexp? exp) _ #f)
       (append (gexp-inputs exp #:native? native?)
               result))
      (($ <gexp-input> (? string? str))
       (if (direct-store-path? str)
           (cons `(,str) result)
           result))
      (($ <gexp-input> (? struct? thing) output n?)
       (if (and (eqv? n? native?) (lookup-compiler thing))
           ;; THING is a derivation, or a package, or an origin, etc.
           (cons `(,thing ,output) result)
           result))
      (($ <gexp-input> (lst ...) output n?)
       (fold-right add-reference-inputs result
                   ;; XXX: For now, automatically convert LST to a list of
                   ;; gexp-inputs.  Inherit N?.
                   (map (match-lambda
                          ((? gexp-input? x)
                           (%gexp-input (gexp-input-thing x)
                                        (gexp-input-output x)
                                        n?))
                          (x
                           (%gexp-input x "out" n?)))
                        lst)))
      (_
       ;; Ignore references to other kinds of objects.
       result)))

  (fold-right add-reference-inputs
              '()
              (gexp-references exp)))

(define gexp-native-inputs
  (cut gexp-inputs <> #:native? #t))

(define (gexp-outputs exp)
  "Return the outputs referred to by EXP as a list of strings."
  (define (add-reference-output ref result)
    (match ref
      (($ <gexp-output> name)
       (cons name result))
      (($ <gexp-input> (? gexp? exp))
       (append (gexp-outputs exp) result))
      (($ <gexp-input> (lst ...) output native?)
       ;; XXX: Automatically convert LST.
       (add-reference-output (map (match-lambda
                                   ((? gexp-input? x) x)
                                   (x (%gexp-input x "out" native?)))
                                  lst)
                             result))
      ((lst ...)
       (fold-right add-reference-output result lst))
      (_
       result)))

  (delete-duplicates
   (add-reference-output (gexp-references exp) '())))

(define* (gexp->sexp exp #:key
                     (system (%current-system))
                     (target (%current-target-system)))
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
                     #:system system
                     #:target (if (or n? native?) #f target)))
        (($ <gexp-input> (refs ...) output n?)
         (mapm %store-monad
               (lambda (ref)
                 ;; XXX: Automatically convert REF to an gexp-input.
                 (reference->sexp
                  (if (gexp-input? ref)
                      ref
                      (%gexp-input ref "out" n?))
                  (or n? native?)))
               refs))
        (($ <gexp-input> (? struct? thing) output n?)
         (let ((target (if (or n? native?) #f target))
               (expand (lookup-expander thing)))
           (mlet %store-monad ((obj (lower-object thing system
                                                  #:target target)))
             ;; OBJ must be either a derivation or a store file name.
             (return (expand thing obj output)))))
        (($ <gexp-input> x)
         (return x))
        (x
         (return x)))))

  (mlet %store-monad
      ((args (mapm %store-monad
                   reference->sexp (gexp-references exp))))
    (return (apply (gexp-proc exp) args))))

(define (syntax-location-string s)
  "Return a string representing the source code location of S."
  (let ((props (syntax-source s)))
    (if props
        (let ((file   (assoc-ref props 'filename))
              (line   (and=> (assoc-ref props 'line) 1+))
              (column (assoc-ref props 'column)))
          (if file
              (simple-format #f "~a:~a:~a"
                             file line column)
              (simple-format #f "~a:~a" line column)))
        "<unknown location>")))

(define-syntax-rule (define-syntax-parameter-once name proc)
  ;; Like 'define-syntax-parameter' but ensure the top-level binding for NAME
  ;; does not get redefined.  This works around a race condition in a
  ;; multi-threaded context with Guile <= 2.2.4: <https://bugs.gnu.org/27476>.
  (eval-when (load eval expand compile)
    (define name
      (if (module-locally-bound? (current-module) 'name)
          (module-ref (current-module) 'name)
          (make-syntax-transformer 'name 'syntax-parameter
                                   (list proc))))))

(define-syntax-parameter-once current-imported-modules
  ;; Current list of imported modules.
  (identifier-syntax '()))

(define-syntax-rule (with-imported-modules modules body ...)
  "Mark the gexps defined in BODY... as requiring MODULES in their execution
environment."
  (syntax-parameterize ((current-imported-modules
                         (identifier-syntax modules)))
    body ...))

(define-syntax-parameter-once current-imported-extensions
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
                        #,sexp)))))))


;;;
;;; Module handling.
;;;

(define %not-slash
  (char-set-complement (char-set #\/)))

(define (file-mapping->tree mapping)
  "Convert MAPPING, an alist like:

  ((\"guix/build/utils.scm\" . \"…/utils.scm\"))

to a tree suitable for 'interned-file-tree'."
  (let ((mapping (map (match-lambda
                        ((destination . source)
                         (cons (string-tokenize destination
                                                %not-slash)
                               source)))
                      mapping)))
    (fold (lambda (pair result)
            (match pair
              ((destination . source)
               (let loop ((destination destination)
                          (result result))
                 (match destination
                   ((file)
                    (let* ((mode (stat:mode (stat source)))
                           (type (if (zero? (logand mode #o100))
                                     'regular
                                     'executable)))
                      (alist-cons file
                                  `(,type (file ,source))
                                  result)))
                   ((file rest ...)
                    (let ((directory (assoc-ref result file)))
                      (alist-cons file
                                  `(directory
                                    ,@(loop rest
                                            (match directory
                                              (('directory . entries) entries)
                                              (#f '()))))
                                  (if directory
                                      (alist-delete file result)
                                      result)))))))))
          '()
          mapping)))

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
                           (guile (%guile-for-build))
                           (module-path %load-path)
                           (extensions '())
                           (deprecation-warnings #f)

                           ;; TODO: This flag is here to prevent a full
                           ;; rebuild.  Remove it on the next rebuild cycle.
                           (pre-load-modules? #t))
  "Return a derivation that builds a tree containing the `.go' files
corresponding to MODULES.  All the MODULES are built in a context where
they can refer to each other."
  (define total (length modules))

  (mlet %store-monad ((modules (imported-modules modules
                                                 #:system system
                                                 #:guile guile
                                                 #:module-path
                                                 module-path)))
    (define build
      (gexp
       (begin
         (primitive-load (ungexp %utils-module))  ;for 'mkdir-p'

         (use-modules (ice-9 ftw)
                      (ice-9 format)
                      (srfi srfi-1)
                      (srfi srfi-26)
                      (system base compile))

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
                         (+ 1 processed
                              (ungexp-splicing (if pre-load-modules?
                                                   (gexp ((ungexp total)))
                                                   (gexp ()))))
                         (ungexp (* total (if pre-load-modules? 2 1)))
                         entry)
                 (compile-file entry
                               #:output-file output
                               #:opts %auto-compilation-options)
                 (+ 1 processed))))

         (define (process-directory directory output processed)
           (let ((entries (map (cut string-append directory "/" <>)
                               (scandir directory regular?))))
             (fold (cut process-entry <> output <>)
                   processed
                   entries)))

         (setvbuf (current-output-port)
                  (cond-expand (guile-2.2 'line) (else _IOLBF)))

         (define mkdir-p
           ;; Capture 'mkdir-p'.
           (@ (guix build utils) mkdir-p))

         ;; Add EXTENSIONS to the search path.
         (set! %load-path
           (append (map (lambda (extension)
                          (string-append extension
                                         "/share/guile/site/"
                                         (effective-version)))
                        '((ungexp-native-splicing extensions)))
                   %load-path))
         (set! %load-compiled-path
           (append (map (lambda (extension)
                          (string-append extension "/lib/guile/"
                                         (effective-version)
                                         "/site-ccache"))
                        '((ungexp-native-splicing extensions)))
                   %load-compiled-path))

         (set! %load-path (cons (ungexp modules) %load-path))

         ;; Above we loaded our own (guix build utils) but now we may need to
         ;; load a compile a different one.  Thus, force a reload.
         (let ((utils (string-append (ungexp modules)
                                     "/guix/build/utils.scm")))
           (when (file-exists? utils)
             (load utils)))

         (mkdir (ungexp output))
         (chdir (ungexp modules))

         (ungexp-splicing
          (if pre-load-modules?
              (gexp ((define* (load-from-directory directory
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
                                               (+ 1 loaded)
                                               (ungexp (* 2 total))
                                               file)
                                       (save-module-excursion
                                        (lambda ()
                                          (primitive-load file)))
                                       (+ 1 loaded))))
                               loaded
                               entries)))

                     (load-from-directory ".")))
              (gexp ())))

         (process-directory "." (ungexp output) 0))))

    ;; TODO: Pass MODULES as an environment variable.
    (gexp->derivation name build
                      #:system system
                      #:guile-for-build guile
                      #:local-build? #t
                      #:env-vars
                      (case deprecation-warnings
                        ((#f)
                         '(("GUILE_WARN_DEPRECATED" . "no")))
                        ((detailed)
                         '(("GUILE_WARN_DEPRECATED" . "detailed")))
                        (else
                         '())))))


;;;
;;; Convenience procedures.
;;;

(define (default-guile)
  ;; Lazily resolve 'guile-2.2' (not 'guile-final' because this is for
  ;; programs returned by 'program-file' and we don't want to keep references
  ;; to several Guile packages).  This module must not refer to (gnu …)
  ;; modules directly, to avoid circular dependencies, hence this hack.
  (module-ref (resolve-interface '(gnu packages guile))
              'guile-2.2))

(define* (load-path-expression modules #:optional (path %load-path)
                               #:key (extensions '()))
  "Return as a monadic value a gexp that sets '%load-path' and
'%load-compiled-path' to point to MODULES, a list of module names.  MODULES
are searched for in PATH.  Return #f when MODULES and EXTENSIONS are empty."
  (if (and (null? modules) (null? extensions))
      (with-monad %store-monad
        (return #f))
      (mlet %store-monad ((modules  (imported-modules modules
                                                      #:module-path path))
                          (compiled (compiled-modules modules
                                                      #:extensions extensions
                                                      #:module-path path)))
        (return (gexp (eval-when (expand load eval)
                        (set! %load-path
                          (cons (ungexp modules)
                                (append (map (lambda (extension)
                                               (string-append extension
                                                              "/share/guile/site/"
                                                              (effective-version)))
                                             '((ungexp-native-splicing extensions)))
                                        %load-path)))
                        (set! %load-compiled-path
                          (cons (ungexp compiled)
                                (append (map (lambda (extension)
                                               (string-append extension
                                                              "/lib/guile/"
                                                              (effective-version)
                                                              "/site-ccache"))
                                             '((ungexp-native-splicing extensions)))
                                        %load-compiled-path)))))))))

(define* (gexp->script name exp
                       #:key (guile (default-guile))
                       (module-path %load-path))
  "Return an executable script NAME that runs EXP using GUILE, with EXP's
imported modules in its search path.  Look up EXP's modules in MODULE-PATH."
  (mlet %store-monad ((set-load-path
                       (load-path-expression (gexp-modules exp)
                                             module-path
                                             #:extensions
                                             (gexp-extensions exp))))
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
                      #:module-path module-path)))

(define* (gexp->file name exp #:key
                     (set-load-path? #t)
                     (module-path %load-path)
                     (splice? #f))
  "Return a derivation that builds a file NAME containing EXP.  When SPLICE?
is true, EXP is considered to be a list of expressions that will be spliced in
the resulting file.

When SET-LOAD-PATH? is true, emit code in the resulting file to set
'%load-path' and '%load-compiled-path' to honor EXP's imported modules.
Lookup EXP's modules in MODULE-PATH."
  (define modules (gexp-modules exp))
  (define extensions (gexp-extensions exp))

  (if (or (not set-load-path?)
          (and (null? modules) (null? extensions)))
      (gexp->derivation name
                        (gexp
                         (call-with-output-file (ungexp output)
                           (lambda (port)
                             (for-each (lambda (exp)
                                         (write exp port))
                                       '(ungexp (if splice?
                                                    exp
                                                    (gexp ((ungexp exp)))))))))
                        #:local-build? #t
                        #:substitutable? #f)
      (mlet %store-monad ((set-load-path
                           (load-path-expression modules module-path
                                                 #:extensions extensions)))
        (gexp->derivation name
                          (gexp
                           (call-with-output-file (ungexp output)
                             (lambda (port)
                               (write '(ungexp set-load-path) port)
                               (for-each (lambda (exp)
                                           (write exp port))
                                         '(ungexp (if splice?
                                                      exp
                                                      (gexp ((ungexp exp)))))))))
                          #:module-path module-path
                          #:local-build? #t
                          #:substitutable? #f))))

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
