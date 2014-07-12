;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module ((guix store)
                #:select (direct-store-path?))
  #:use-module (guix monads)
  #:use-module ((guix derivations)
                #:select (derivation? derivation->output-path
                          %guile-for-build derivation))
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (gexp
            gexp?
            gexp->derivation
            gexp->file
            gexp->script))

;;; Commentary:
;;;
;;; This module implements "G-expressions", or "gexps".  Gexps are like
;;; S-expressions (sexps), with two differences:
;;;
;;;   1. References (un-quotations) to derivations or packages in a gexp are
;;;      replaced by the corresponding output file name;
;;;
;;;   2. Gexps embed information about the derivations they refer to.
;;;
;;; Gexps make it easy to write to files Scheme code that refers to store
;;; items, or to write Scheme code to build derivations.
;;;
;;; Code:

;; "G expressions".
(define-record-type <gexp>
  (make-gexp references proc)
  gexp?
  (references gexp-references)                    ; ((DRV-OR-PKG OUTPUT) ...)
  (proc       gexp-proc))                         ; procedure

(define (write-gexp gexp port)
  "Write GEXP on PORT."
  (display "#<gexp " port)
  (write (apply (gexp-proc gexp) (gexp-references gexp)) port)
  (format port " ~a>"
          (number->string (object-address gexp) 16)))

(set-record-type-printer! <gexp> write-gexp)

;; Reference to one of the derivation's outputs, for gexps used in
;; derivations.
(define-record-type <output-ref>
  (output-ref name)
  output-ref?
  (name output-ref-name))

(define raw-derivation
  (store-lift derivation))

(define (lower-inputs inputs)
  "Turn any package from INPUTS into a derivation; return the corresponding
input list as a monadic value."
  (with-monad %store-monad
    (sequence %store-monad
              (map (match-lambda
                    (((? package? package) sub-drv ...)
                     (mlet %store-monad ((drv (package->derivation package)))
                       (return `(,drv ,@sub-drv))))
                    (((? origin? origin) sub-drv ...)
                     (mlet %store-monad ((drv (origin->derivation origin)))
                       (return `(,drv ,@sub-drv))))
                    (input
                     (return input)))
                   inputs))))

(define* (gexp->derivation name exp
                           #:key
                           system
                           hash hash-algo recursive?
                           (env-vars '())
                           (modules '())
                           (guile-for-build (%guile-for-build))
                           references-graphs
                           local-build?)
  "Return a derivation NAME that runs EXP (a gexp) with GUILE-FOR-BUILD (a
derivation) on SYSTEM.

Make MODULES available in the evaluation context of EXP; MODULES is a list of
names of Guile modules from the current search path to be copied in the store,
compiled, and made available in the load path during the execution of
EXP---e.g., '((guix build utils) (guix build gnu-build-system)).

The other arguments are as for 'derivation'."
  (define %modules modules)
  (define outputs (gexp-outputs exp))

  (mlet* %store-monad ((inputs   (lower-inputs (gexp-inputs exp)))
                       (system -> (or system (%current-system)))
                       (sexp     (gexp->sexp exp))
                       (builder  (text-file (string-append name "-builder")
                                            (object->string sexp)))
                       (modules  (if (pair? %modules)
                                     (imported-modules %modules
                                                       #:system system
                                                       #:guile guile-for-build)
                                     (return #f)))
                       (compiled (if (pair? %modules)
                                     (compiled-modules %modules
                                                       #:system system
                                                       #:guile guile-for-build)
                                     (return #f)))
                       (guile    (if guile-for-build
                                     (return guile-for-build)
                                     (package->derivation (default-guile)
                                                          system))))
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
                                     inputs))
                    #:hash hash #:hash-algo hash-algo #:recursive? recursive?
                    #:references-graphs references-graphs
                    #:local-build? local-build?)))

(define (gexp-inputs exp)
  "Return the input list for EXP."
  (define (add-reference-inputs ref result)
    (match ref
      (((? derivation?) (? string?))
       (cons ref result))
      (((? package?) (? string?))
       (cons ref result))
      (((? origin?) (? string?))
       (cons ref result))
      ((? gexp? exp)
       (append (gexp-inputs exp) result))
      (((? string? file))
       (if (direct-store-path? file)
           (cons ref result)
           result))
      ((refs ...)
       (fold-right add-reference-inputs result refs))
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
      (($ <output-ref> name)
       (cons name result))
      ((? gexp? exp)
       (append (gexp-outputs exp) result))
      (_
       result)))

  (fold-right add-reference-output
              '()
              (gexp-references exp)))

(define* (gexp->sexp exp)
  "Return (monadically) the sexp corresponding to EXP for the given OUTPUT,
and in the current monad setting (system type, etc.)"
  (define (reference->sexp ref)
    (with-monad %store-monad
      (match ref
        (((? derivation? drv) (? string? output))
         (return (derivation->output-path drv output)))
        (((? package? p) (? string? output))
         (package-file p #:output output))
        (((? origin? o) (? string? output))
         (mlet %store-monad ((drv (origin->derivation o)))
           (return (derivation->output-path drv output))))
        (($ <output-ref> output)
         ;; Output file names are not known in advance but the daemon defines
         ;; an environment variable for each of them at build time, so use
         ;; that trick.
         (return `((@ (guile) getenv) ,output)))
        ((? gexp? exp)
         (gexp->sexp exp))
        (((? string? str))
         (return (if (direct-store-path? str) str ref)))
        ((refs ...)
         (sequence %store-monad (map reference->sexp refs)))
        (x
         (return x)))))

  (mlet %store-monad
      ((args (sequence %store-monad
                       (map reference->sexp (gexp-references exp)))))
    (return (apply (gexp-proc exp) args))))

(define (canonicalize-reference ref)
  "Return a canonical variant of REF, which adds any missing output part in
package/derivation references."
  (match ref
    ((? package? p)
     `(,p "out"))
    ((? origin? o)
     `(,o "out"))
    ((? derivation? d)
     `(,d "out"))
    (((? package?) (? string?))
     ref)
    (((? origin?) (? string?))
     ref)
    (((? derivation?) (? string?))
     ref)
    ((? string? s)
     (if (direct-store-path? s) `(,s) s))
    ((refs ...)
     (map canonicalize-reference refs))
    (x x)))

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

    (define (escape->ref exp)
      ;; Turn 'ungexp' form EXP into a "reference".
      (syntax-case exp (ungexp ungexp-splicing output)
        ((ungexp output)
         #'(output-ref "out"))
        ((ungexp output name)
         #'(output-ref name))
        ((ungexp thing)
         #'thing)
        ((ungexp drv-or-pkg out)
         #'(list drv-or-pkg out))
        ((ungexp-splicing lst)
         #'lst)))

    (define (substitute-references exp substs)
      ;; Return a variant of EXP where all the cars of SUBSTS have been
      ;; replaced by the corresponding cdr.
      (syntax-case exp (ungexp ungexp-splicing)
        ((ungexp _ ...)
         (match (assoc exp substs)
           ((_ id)
            id)
           (_
            #'(syntax-error "error: no 'ungexp' substitution"
                            #'ref))))
        (((ungexp-splicing _ ...) rest ...)
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
        ((exp0 exp ...)
         #`(cons #,(substitute-references #'exp0 substs)
                 #,(substitute-references #'(exp ...) substs)))
        (x #''x)))

    (syntax-case s (ungexp output)
      ((_ exp)
       (let* ((escapes (delete-duplicates (collect-escapes #'exp)))
              (formals (generate-temporaries escapes))
              (sexp    (substitute-references #'exp (zip escapes formals)))
              (refs    (map escape->ref escapes)))
         #`(make-gexp (map canonicalize-reference (list #,@refs))
                      (lambda #,formals
                        #,sexp)))))))


;;;
;;; Convenience procedures.
;;;

(define (default-guile)
  ;; Lazily resolve 'guile-final'.  This module must not refer to (gnu …)
  ;; modules directly, to avoid circular dependencies, hence this hack.
  (module-ref (resolve-interface '(gnu packages base))
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
                           (write
                            '(set! %load-path
                                   (cons (ungexp modules) %load-path))
                            port)
                           (write
                            '(set! %load-compiled-path
                                   (cons (ungexp compiled)
                                         %load-compiled-path))
                            port)
                           (write '(ungexp exp) port)
                           (chmod port #o555)))))))

(define (gexp->file name exp)
  "Return a derivation that builds a file NAME containing EXP."
  (gexp->derivation name
                    (gexp
                     (call-with-output-file (ungexp output)
                       (lambda (port)
                         (write '(ungexp exp) port))))))



;;;
;;; Syntactic sugar.
;;;

(eval-when (expand load eval)
  (define (read-ungexp chr port)
    "Read an 'ungexp' or 'ungexp-splicing' form from PORT."
    (define unquote-symbol
      (match (peek-char port)
        (#\@
         (read-char port)
         'ungexp-splicing)
        (_
         'ungexp)))

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
  (read-hash-extend #\$ read-ungexp))

;;; gexp.scm ends here
