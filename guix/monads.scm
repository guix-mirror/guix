;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module ((system syntax)
                #:select (syntax-local-binding))
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (;; Monads.
            define-monad
            monad?
            monad-bind
            monad-return

            ;; Syntax.
            >>=
            return
            with-monad
            mlet
            mlet*
            lift1 lift2 lift3 lift4 lift5 lift6 lift7 lift
            listm
            foldm
            mapm
            sequence
            anym

            ;; Concrete monads.
            %identity-monad

            %store-monad
            store-bind
            store-return
            store-lift
            run-with-store
            text-file
            package-file
            package->derivation
            built-derivations
            derivation-expression))

;;; Commentary:
;;;
;;; This module implements the general mechanism of monads, and provides in
;;; particular an instance of the "store" monad.  The API was inspired by that
;;; of Racket's "better-monads" module (see
;;; <http://planet.racket-lang.org/package-source/toups/functional.plt/1/1/planet-docs/better-monads-guide/index.html>).
;;; The implementation and use case were influenced by Oleg Kysielov's
;;; "Monadic Programming in Scheme" (see
;;; <http://okmij.org/ftp/Scheme/monad-in-Scheme.html>).
;;;
;;; The store monad allows us to (1) build sequences of operations in the
;;; store, and (2) make the store an implicit part of the execution context,
;;; rather than a parameter of every single function.
;;;
;;; Code:

;; Record type for monads manipulated at run time.
(define-record-type <monad>
  (make-monad bind return)
  monad?
  (bind   monad-bind)
  (return monad-return))                         ; TODO: Add 'plus' and 'zero'

(define-syntax define-monad
  (lambda (s)
    "Define the monad under NAME, with the given bind and return methods."
    (define prefix (string->symbol "% "))
    (define (make-rtd-name name)
      (datum->syntax name
                     (symbol-append prefix (syntax->datum name) '-rtd)))

    (syntax-case s (bind return)
      ((_ name (bind b) (return r))
       (with-syntax ((rtd (make-rtd-name #'name)))
         #`(begin
             (define rtd
               ;; The record type, for use at run time.
               (make-monad b r))

             (define-syntax name
               ;; An "inlined record", for use at expansion time.  The goal is
               ;; to allow 'bind' and 'return' to be resolved at expansion
               ;; time, in the common case where the monad is accessed
               ;; directly as NAME.
               (lambda (s)
                 (syntax-case s (%bind %return)
                   ((_ %bind)   #'b)
                   ((_ %return) #'r)
                   (_           #'rtd))))))))))

(define-syntax-parameter >>=
  ;; The name 'bind' is already taken, so we choose this (obscure) symbol.
  (lambda (s)
    (syntax-violation '>>= ">>= (bind) used outside of 'with-monad'" s)))

(define-syntax-parameter return
  (lambda (s)
    (syntax-violation 'return "return used outside of 'with-monad'" s)))

(define-syntax with-monad
  (lambda (s)
    "Evaluate BODY in the context of MONAD, and return its result."
    (syntax-case s ()
      ((_ monad body ...)
       (eq? 'macro (syntax-local-binding #'monad))
       ;; MONAD is a syntax transformer, so we can obtain the bind and return
       ;; methods by directly querying it.
       #'(syntax-parameterize ((>>=    (identifier-syntax (monad %bind)))
                               (return (identifier-syntax (monad %return))))
           body ...))
      ((_ monad body ...)
       ;; MONAD refers to the <monad> record that represents the monad at run
       ;; time, so use the slow method.
       #'(syntax-parameterize ((>>=    (identifier-syntax
                                        (monad-bind monad)))
                               (return (identifier-syntax
                                        (monad-return monad))))
           body ...)))))

(define-syntax mlet*
  (syntax-rules (->)
    "Bind the given monadic values MVAL to the given variables VAR.  When the
form is (VAR -> VAL), bind VAR to the non-monadic value VAL in the same way as
'let'."
    ;; Note: the '->' symbol corresponds to 'is:' in 'better-monads.rkt'.
    ((_ monad () body ...)
     (with-monad monad body ...))
    ((_ monad ((var mval) rest ...) body ...)
     (with-monad monad
       (>>= mval
            (lambda (var)
              (mlet* monad (rest ...)
                body ...)))))
    ((_ monad ((var -> val) rest ...) body ...)
     (let ((var val))
       (mlet* monad (rest ...)
         body ...)))))

(define-syntax mlet
  (lambda (s)
    (syntax-case s ()
      ((_ monad ((var mval ...) ...) body ...)
       (with-syntax (((temp ...) (generate-temporaries #'(var ...))))
         #'(mlet* monad ((temp mval ...) ...)
             (let ((var temp) ...)
               body ...)))))))

(define-syntax define-lift
  (syntax-rules ()
    ((_ liftn (args ...))
     (define (liftn proc monad)
       "Lift PROC to MONAD---i.e., return a monadic function in MONAD."
       (lambda (args ...)
         (with-monad monad
           (return (proc args ...))))))))

(define-lift lift1 (a))
(define-lift lift2 (a b))
(define-lift lift3 (a b c))
(define-lift lift4 (a b c d))
(define-lift lift5 (a b c d e))
(define-lift lift6 (a b c d e f))
(define-lift lift7 (a b c d e f g))

(define (lift nargs proc monad)
  "Lift PROC, a procedure that accepts NARGS arguments, to MONAD---i.e.,
return a monadic function in MONAD."
  (lambda args
    (with-monad monad
      (return (apply proc args)))))

(define (foldm monad mproc init lst)
  "Fold MPROC over LST, a list of monadic values in MONAD, and return a
monadic value seeded by INIT."
  (with-monad monad
    (let loop ((lst    lst)
               (result init))
      (match lst
        (()
         (return result))
        ((head tail ...)
         (mlet* monad ((item   head)
                       (result (mproc item result)))
           (loop tail result)))))))

(define (mapm monad mproc lst)
  "Map MPROC over LST, a list of monadic values in MONAD, and return a monadic
list."
  (foldm monad
         (lambda (item result)
           (mlet monad ((item (mproc item)))
             (return (cons item result))))
         '()
         (reverse lst)))

(define-inlinable (sequence monad lst)
  "Turn the list of monadic values LST into a monadic list of values, by
evaluating each item of LST in sequence."
  ;; FIXME: 'mapm' binds from right to left.
  (with-monad monad
    (mapm monad return lst)))

(define (anym monad proc lst)
  "Apply PROC to the list of monadic values LST; return the first value,
lifted in MONAD, for which PROC returns true."
  (with-monad monad
    (let loop ((lst lst))
      (match lst
        (()
         (return #f))
        ((head tail ...)
         (mlet monad ((value head))
           (or (and=> (proc value) return)
               head
               (loop tail))))))))

(define-syntax listm
  (lambda (s)
    "Return a monadic list in MONAD from the monadic values MVAL."
    (syntax-case s ()
      ((_ monad mval ...)
       (with-syntax (((val ...) (generate-temporaries #'(mval ...))))
         #'(mlet monad ((val mval) ...)
             (return (list val ...))))))))



;;;
;;; Identity monad.
;;;

(define-inlinable (identity-return value)
  value)

(define-inlinable (identity-bind mvalue mproc)
  (mproc mvalue))

(define-monad %identity-monad
  (bind   identity-bind)
  (return identity-return))


;;;
;;; Store monad.
;;;

;; return:: a -> StoreM a
(define-inlinable (store-return value)
  "Return VALUE from a monadic function."
  ;; The monadic value is just this.
  (lambda (store)
    value))

;; >>=:: StoreM a -> (a -> StoreM b) -> StoreM b
(define-inlinable (store-bind mvalue mproc)
  "Bind MVALUE in MPROC."
  (lambda (store)
    (let* ((value   (mvalue store))
           (mresult (mproc value)))
      (mresult store))))

(define-monad %store-monad
  (bind   store-bind)
  (return store-return))


(define (store-lift proc)
  "Lift PROC, a procedure whose first argument is a connection to the store,
in the store monad."
  (define result
    (lambda args
      (lambda (store)
        (apply proc store args))))

  (set-object-property! result 'documentation
                        (procedure-property proc 'documentation))
  result)

;;;
;;; Store monad operators.
;;;

(define* (text-file name text)
  "Return as a monadic value the absolute file name in the store of the file
containing TEXT."
  (lambda (store)
    (add-text-to-store store name text '())))

(define* (package-file package
                       #:optional file
                       #:key (system (%current-system)) (output "out"))
  "Return as a monadic value in the absolute file name of FILE within the
OUTPUT directory of PACKAGE.  When FILE is omitted, return the name of the
OUTPUT directory of PACKAGE."
  (lambda (store)
    (let* ((drv (package-derivation store package system))
           (out (derivation->output-path drv output)))
      (if file
          (string-append out "/" file)
          out))))

(define derivation-expression
  (store-lift build-expression->derivation))

(define package->derivation
  (store-lift package-derivation))

(define built-derivations
  (store-lift build-derivations))

(define* (run-with-store store mval
                         #:key
                         (guile-for-build (%guile-for-build))
                         (system (%current-system)))
  "Run MVAL, a monadic value in the store monad, in STORE, an open store
connection."
  (parameterize ((%guile-for-build (or guile-for-build
                                       (package-derivation store
                                                           (@ (gnu packages base)
                                                              guile-final)
                                                           system)))
                 (%current-system system))
    (mval store)))

;;; monads.scm end here
