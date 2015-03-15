;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (gexp
            gexp?

            gexp-input
            gexp-input?

            gexp->derivation
            gexp->file
            gexp->script
            text-file*
            imported-files
            imported-modules
            compiled-modules))

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
  (make-gexp references natives proc)
  gexp?
  (references gexp-references)                    ; ((DRV-OR-PKG OUTPUT) ...)
  (natives    gexp-native-references)             ; ((DRV-OR-PKG OUTPUT) ...)
  (proc       gexp-proc))                         ; procedure

(define (write-gexp gexp port)
  "Write GEXP on PORT."
  (display "#<gexp " port)

  ;; Try to write the underlying sexp.  Now, this trick doesn't work when
  ;; doing things like (ungexp-splicing (gexp ())) because GEXP's procedure
  ;; tries to use 'append' on that, which fails with wrong-type-arg.
  (false-if-exception
   (write (apply (gexp-proc gexp)
                 (append (gexp-references gexp)
                         (gexp-native-references gexp)))
          port))
  (format port " ~a>"
          (number->string (object-address gexp) 16)))

(set-record-type-printer! <gexp> write-gexp)

;; The input of a gexp.
(define-record-type <gexp-input>
  (%gexp-input thing output native?)
  gexp-input?
  (thing     gexp-input-thing)       ;<package> | <origin> | <derivation> | ...
  (output    gexp-input-output)      ;string
  (native?   gexp-input-native?))    ;Boolean

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

(define raw-derivation
  (store-lift derivation))

(define* (lower-inputs inputs
                       #:key system target)
  "Turn any package from INPUTS into a derivation for SYSTEM; return the
corresponding input list as a monadic value.  When TARGET is true, use it as
the cross-compilation target triplet."
  (with-monad %store-monad
    (sequence %store-monad
              (map (match-lambda
                    (((? package? package) sub-drv ...)
                     (mlet %store-monad
                         ((drv (if target
                                   (package->cross-derivation package target
                                                              system)
                                   (package->derivation package system))))
                       (return `(,drv ,@sub-drv))))
                    (((? origin? origin) sub-drv ...)
                     (mlet %store-monad ((drv (origin->derivation origin)))
                       (return `(,drv ,@sub-drv))))
                    (input
                     (return input)))
                   inputs))))

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
  ;; XXX: Currently outputs other than "out" are not supported, and things
  ;; other than packages aren't either.
  (with-monad %store-monad
    (define lower
      (match-lambda
       ((? string? output)
        (return output))
       ((? package? package)
        (mlet %store-monad ((drv
                             (if target
                                 (package->cross-derivation package target
                                                            #:system system
                                                            #:graft? #f)
                                 (package->derivation package system
                                                      #:graft? #f))))
          (return (derivation->output-path drv))))))

    (sequence %store-monad (map lower lst))))

(define* (gexp->derivation name exp
                           #:key
                           system (target 'current)
                           hash hash-algo recursive?
                           (env-vars '())
                           (modules '())
                           (module-path %load-path)
                           (guile-for-build (%guile-for-build))
                           (graft? (%graft?))
                           references-graphs
                           allowed-references
                           local-build?)
  "Return a derivation NAME that runs EXP (a gexp) with GUILE-FOR-BUILD (a
derivation) on SYSTEM.  When TARGET is true, it is used as the
cross-compilation target triplet for packages referred to by EXP.

Make MODULES available in the evaluation context of EXP; MODULES is a list of
names of Guile modules searched in MODULE-PATH to be copied in the store,
compiled, and made available in the load path during the execution of
EXP---e.g., '((guix build utils) (guix build gnu-build-system)).

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

The other arguments are as for 'derivation'."
  (define %modules modules)
  (define outputs (gexp-outputs exp))

  (define (graphs-file-names graphs)
    ;; Return a list of (FILE-NAME . STORE-PATH) pairs made from GRAPHS.
    (map (match-lambda
           ((file-name (? derivation? drv))
            (cons file-name (derivation->output-path drv)))
           ((file-name (? derivation? drv) sub-drv)
            (cons file-name (derivation->output-path drv sub-drv)))
           ((file-name thing)
            (cons file-name thing)))
         graphs))

  (mlet* %store-monad (;; The following binding forces '%current-system' and
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
                       (builder  (text-file (string-append name "-builder")
                                            (object->string sexp)))
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
                                                       #:guile guile-for-build)
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
                       (guile    (if guile-for-build
                                     (return guile-for-build)
                                     (package->derivation (default-guile)
                                                          system))))
    (mbegin %store-monad
      (set-grafting graft?)                       ;restore the initial setting
      (raw-derivation name
                      (string-append (derivation->output-path guile)
                                     "/bin/guile")
                      `("--no-auto-compile"
                        ,@(if (pair? %modules)
                              `("-L" ,(derivation->output-path modules)
                                "-C" ,(derivation->output-path compiled))
                              '())
                        ,builder)
                      #:outputs outputs
                      #:env-vars env-vars
                      #:system system
                      #:inputs `((,guile)
                                 (,builder)
                                 ,@(if modules
                                       `((,modules) (,compiled) ,@inputs)
                                       inputs)
                                 ,@(match graphs
                                     (((_ . inputs) ...) inputs)
                                     (_ '())))
                      #:hash hash #:hash-algo hash-algo #:recursive? recursive?
                      #:references-graphs (and=> graphs graphs-file-names)
                      #:allowed-references allowed
                      #:local-build? local-build?))))

(define* (gexp-inputs exp #:optional (references gexp-references))
  "Return the input list for EXP, using REFERENCES to get its list of
references."
  (define (add-reference-inputs ref result)
    (match ref
      (($ <gexp-input> (? derivation? drv) output)
       (cons `(,drv ,output) result))
      (($ <gexp-input> (? package? pkg) output)
       (cons `(,pkg ,output) result))
      (($ <gexp-input> (? origin? o))
       (cons `(,o "out") result))
      (($ <gexp-input> (? gexp? exp))
       (append (gexp-inputs exp references) result))
      (($ <gexp-input> (? string? str))
       (if (direct-store-path? str)
           (cons `(,str) result)
           result))
      (($ <gexp-input> ((? package? p) (? string? output)) _ native?)
       ;; XXX: For now, for backward-compatibility, automatically convert a
       ;; pair like this to an gexp-input for OUTPUT of P.
       (add-reference-inputs (gexp-input p output native?) result))
      (($ <gexp-input> (lst ...) output native?)
       (fold-right add-reference-inputs result
                   ;; XXX: For now, automatically convert LST to a list of
                   ;; gexp-inputs.
                   (map (match-lambda
                         ((? gexp-input? x) x)
                         (x (%gexp-input x "out" native?)))
                        lst)))
      (_
       ;; Ignore references to other kinds of objects.
       result)))

  (fold-right add-reference-inputs
              '()
              (references exp)))

(define gexp-native-inputs
  (cut gexp-inputs <> gexp-native-references))

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
        (($ <gexp-input> (? derivation? drv) output)
         (return (derivation->output-path drv output)))
        (($ <gexp-input> (? package? p) output n?)
         (package-file p
                       #:output output
                       #:system system
                       #:target (if (or n? native?) #f target)))
        (($ <gexp-input> ((? package? p) (? string? output)) _ n?)
         ;; XXX: For backward compatibility, automatically interpret such a
         ;; pair.
         (package-file p
                       #:output output
                       #:system system
                       #:target (if (or n? native?) #f target)))
        (($ <gexp-input> (? origin? o) output)
         (mlet %store-monad ((drv (origin->derivation o)))
           (return (derivation->output-path drv output))))
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
         (sequence %store-monad
                   (map (lambda (ref)
                          ;; XXX: Automatically convert REF to an gexp-input.
                          (reference->sexp
                           (if (gexp-input? ref)
                               ref
                               (%gexp-input ref "out" n?))
                           native?))
                        refs)))
        (($ <gexp-input> x)
         (return x))
        (x
         (return x)))))

  (mlet %store-monad
      ((args (sequence %store-monad
                       (append (map reference->sexp (gexp-references exp))
                               (map (cut reference->sexp <> #t)
                                    (gexp-native-references exp))))))
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

(define-syntax gexp
  (lambda (s)
    (define (collect-escapes exp)
      ;; Return all the 'ungexp' present in EXP.
      (let loop ((exp    exp)
                 (result '()))
        (syntax-case exp (ungexp ungexp-splicing)
          ((ungexp _)
           (cons exp result))
          ((ungexp _ _)
           (cons exp result))
          ((ungexp-splicing _ ...)
           (cons exp result))
          ((exp0 exp ...)
           (let ((result (loop #'exp0 result)))
             (fold loop result #'(exp ...))))
          (_
           result))))

    (define (collect-native-escapes exp)
      ;; Return all the 'ungexp-native' forms present in EXP.
      (let loop ((exp    exp)
                 (result '()))
        (syntax-case exp (ungexp-native ungexp-native-splicing)
          ((ungexp-native _)
           (cons exp result))
          ((ungexp-native _ _)
           (cons exp result))
          ((ungexp-native-splicing _ ...)
           (cons exp result))
          ((exp0 exp ...)
           (let ((result (loop #'exp0 result)))
             (fold loop result #'(exp ...))))
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
        (_
         #'(syntax-error "error: no 'ungexp' substitution"
                         #'ref))))

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
                            #'ref))))))

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
        ((exp0 exp ...)
         #`(cons #,(substitute-references #'exp0 substs)
                 #,(substitute-references #'(exp ...) substs)))
        (x #''x)))

    (syntax-case s (ungexp output)
      ((_ exp)
       (let* ((normals (delete-duplicates (collect-escapes #'exp)))
              (natives (delete-duplicates (collect-native-escapes #'exp)))
              (escapes (append normals natives))
              (formals (generate-temporaries escapes))
              (sexp    (substitute-references #'exp (zip escapes formals)))
              (refs    (map escape->ref normals))
              (nrefs   (map escape->ref natives)))
         #`(make-gexp (list #,@refs) (list #,@nrefs)
                      (lambda #,formals
                        #,sexp)))))))


;;;
;;; Module handling.
;;;

(define %mkdir-p-definition
  ;; The code for 'mkdir-p' is copied from (guix build utils).  We use it in
  ;; derivations that cannot use the #:modules argument of 'gexp->derivation'
  ;; precisely because they implement that functionality.
  (gexp
   (define (mkdir-p dir)
     (define absolute?
       (string-prefix? "/" dir))

     (define not-slash
       (char-set-complement (char-set #\/)))

     (let loop ((components (string-tokenize dir not-slash))
                (root       (if absolute? "" ".")))
       (match components
         ((head tail ...)
          (let ((path (string-append root "/" head)))
            (catch 'system-error
              (lambda ()
                (mkdir path)
                (loop tail path))
              (lambda args
                (if (= EEXIST (system-error-errno args))
                    (loop tail path)
                    (apply throw args))))))
         (() #t))))))

(define* (imported-files files
                         #:key (name "file-import")
                         (system (%current-system))
                         (guile (%guile-for-build)))
  "Return a derivation that imports FILES into STORE.  FILES must be a list
of (FINAL-PATH . FILE-NAME) pairs; each FILE-NAME is read from the file
system, imported, and appears under FINAL-PATH in the resulting store path."
  (define file-pair
    (match-lambda
     ((final-path . file-name)
      (mlet %store-monad ((file (interned-file file-name
                                               (basename final-path))))
        (return (list final-path file))))))

  (mlet %store-monad ((files (sequence %store-monad
                                       (map file-pair files))))
    (define build
      (gexp
       (begin
         (use-modules (ice-9 match))

         (ungexp %mkdir-p-definition)

         (mkdir (ungexp output)) (chdir (ungexp output))
         (for-each (match-lambda
                    ((final-path store-path)
                     (mkdir-p (dirname final-path))
                     (symlink store-path final-path)))
                   '(ungexp files)))))

    ;; TODO: Pass FILES as an environment variable so that BUILD remains
    ;; exactly the same regardless of FILES: less disk space, and fewer
    ;; 'add-to-store' RPCs.
    (gexp->derivation name build
                      #:system system
                      #:guile-for-build guile
                      #:local-build? #t)))

(define search-path*
  ;; A memoizing version of 'search-path' so 'imported-modules' does not end
  ;; up looking for the same files over and over again.
  (memoize search-path))

(define* (imported-modules modules
                           #:key (name "module-import")
                           (system (%current-system))
                           (guile (%guile-for-build))
                           (module-path %load-path))
  "Return a derivation that contains the source files of MODULES, a list of
module names such as `(ice-9 q)'.  All of MODULES must be in the MODULE-PATH
search path."
  ;; TODO: Determine the closure of MODULES, build the `.go' files,
  ;; canonicalize the source files through read/write, etc.
  (let ((files (map (lambda (m)
                      (let ((f (string-append
                                (string-join (map symbol->string m) "/")
                                ".scm")))
                        (cons f (search-path* module-path f))))
                    modules)))
    (imported-files files #:name name #:system system
                    #:guile guile)))

(define* (compiled-modules modules
                           #:key (name "module-import-compiled")
                           (system (%current-system))
                           (guile (%guile-for-build))
                           (module-path %load-path))
  "Return a derivation that builds a tree containing the `.go' files
corresponding to MODULES.  All the MODULES are built in a context where
they can refer to each other."
  (mlet %store-monad ((modules (imported-modules modules
                                                 #:system system
                                                 #:guile guile
                                                 #:module-path
                                                 module-path)))
    (define build
      (gexp
       (begin
         (use-modules (ice-9 ftw)
                      (ice-9 match)
                      (srfi srfi-26)
                      (system base compile))

         (ungexp %mkdir-p-definition)

         (define (regular? file)
           (not (member file '("." ".."))))

         (define (process-directory directory output)
           (let ((entries (map (cut string-append directory "/" <>)
                               (scandir directory regular?))))
             (for-each (lambda (entry)
                         (if (file-is-directory? entry)
                             (let ((output (string-append output "/"
                                                          (basename entry))))
                               (mkdir-p output)
                               (process-directory entry output))
                             (let* ((base   (string-drop-right
                                             (basename entry)
                                             4)) ;.scm
                                    (output (string-append output "/" base
                                                           ".go")))
                               (compile-file entry
                                             #:output-file output
                                             #:opts
                                             %auto-compilation-options))))
                       entries)))

         (set! %load-path (cons (ungexp modules) %load-path))
         (mkdir (ungexp output))
         (chdir (ungexp modules))
         (process-directory "." (ungexp output)))))

    ;; TODO: Pass MODULES as an environment variable.
    (gexp->derivation name build
                      #:system system
                      #:guile-for-build guile
                      #:local-build? #t)))


;;;
;;; Convenience procedures.
;;;

(define (default-guile)
  ;; Lazily resolve 'guile-final'.  This module must not refer to (gnu …)
  ;; modules directly, to avoid circular dependencies, hence this hack.
  (module-ref (resolve-interface '(gnu packages commencement))
              'guile-final))

(define* (gexp->script name exp
                       #:key (modules '()) (guile (default-guile)))
  "Return an executable script NAME that runs EXP using GUILE with MODULES in
its search path."
  (mlet %store-monad ((modules  (imported-modules modules))
                      (compiled (compiled-modules modules)))
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

                           ;; Write the 'eval-when' form so that it can be
                           ;; compiled.
                           (write
                            '(eval-when (expand load eval)
                               (set! %load-path
                                    (cons (ungexp modules) %load-path))
                               (set! %load-compiled-path
                                     (cons (ungexp compiled)
                                           %load-compiled-path)))
                            port)
                           (write '(ungexp exp) port)
                           (chmod port #o555)))))))

(define (gexp->file name exp)
  "Return a derivation that builds a file NAME containing EXP."
  (gexp->derivation name
                    (gexp
                     (call-with-output-file (ungexp output)
                       (lambda (port)
                         (write '(ungexp exp) port))))
                    #:local-build? #t))

(define* (text-file* name #:rest text)
  "Return as a monadic value a derivation that builds a text file containing
all of TEXT.  TEXT may list, in addition to strings, packages, derivations,
and store file names; the resulting store file holds references to all these."
  (define builder
    (gexp (call-with-output-file (ungexp output "out")
            (lambda (port)
              (display (string-append (ungexp-splicing text)) port)))))

  (gexp->derivation name builder))


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
