;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2017 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module ((system syntax)
                #:select (syntax-local-binding))
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (;; Monads.
            define-monad
            monad?
            monad-bind
            monad-return

            template-directory

            ;; Syntax.
            >>=
            return
            with-monad
            mlet
            mlet*
            mbegin
            mwhen
            munless
            lift0 lift1 lift2 lift3 lift4 lift5 lift6 lift7 lift
            listm
            foldm
            mapm
            sequence
            anym

            ;; Concrete monads.
            %identity-monad

            %state-monad
            state-return
            state-bind
            current-state
            set-current-state
            state-push
            state-pop
            run-with-state))

;;; Commentary:
;;;
;;; This module implements the general mechanism of monads, and provides in
;;; particular an instance of the "state" monad.  The API was inspired by that
;;; of Racket's "better-monads" module (see
;;; <http://planet.racket-lang.org/package-source/toups/functional.plt/1/1/planet-docs/better-monads-guide/index.html>).
;;; The implementation and use case were influenced by Oleg Kysielov's
;;; "Monadic Programming in Scheme" (see
;;; <http://okmij.org/ftp/Scheme/monad-in-Scheme.html>).
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

             ;; Instantiate all the templates, specialized for this monad.
             (template-directory instantiations name)

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

;; Expansion- and run-time state of the template directory.  This needs to be
;; available at run time (and not just at expansion time) so we can
;; instantiate templates defined in other modules, or use instances defined
;; elsewhere.
(eval-when (load expand eval)
  ;; Mapping of syntax objects denoting the template to a pair containing (1)
  ;; the syntax object of the parameter over which it is templated, and (2)
  ;; the syntax of its body.
  (define-once %templates (make-hash-table))

  (define (register-template! name param body)
    (hash-set! %templates name (cons param body)))

  ;; List of template instances, where each entry is a triplet containing the
  ;; syntax of the name, the actual parameter for which the template is
  ;; specialized, and the syntax object referring to this specialization (the
  ;; procedure's identifier.)
  (define-once %template-instances '())

  (define (register-template-instance! name actual instance)
    (set! %template-instances
      (cons (list name actual instance) %template-instances))))

(define-syntax template-directory
  (lambda (s)
    "This is a \"stateful macro\" to register and lookup templates and
template instances."
    (define location
      (syntax-source s))

    (define current-info-port
      ;; Port for debugging info.
      (const (%make-void-port "w")))

    (define location-string
      (format #f "~a:~a:~a"
              (assq-ref location 'filename)
              (and=> (assq-ref location 'line) 1+)
              (assq-ref location 'column)))

    (define (matching-instance? name actual)
      (match-lambda
        ((name* instance-param proc)
         (and (free-identifier=? name name*)
              (or (equal? actual instance-param)
                  (and (identifier? actual)
                       (identifier? instance-param)
                       (free-identifier=? instance-param
                                          actual)))
              proc))))

    (define (instance-identifier name actual)
      (define stem
        (string-append
         " "
         (symbol->string (syntax->datum name))
         (if (identifier? actual)
             (string-append " " (symbol->string (syntax->datum actual)))
             "")
         " instance"))
      (datum->syntax actual (string->symbol stem)))

    (define (instance-definition name template actual)
      (match template
        ((formal . body)
         (let ((instance (instance-identifier name actual)))
           (format (current-info-port)
                   "~a: info: specializing '~a' for '~a' as '~a'~%"
                   location-string
                   (syntax->datum name) (syntax->datum actual)
                   (syntax->datum instance))

           (register-template-instance! name actual instance)

           #`(begin
               (define #,instance
                 (let-syntax ((#,formal (identifier-syntax #,actual)))
                   #,body))

               ;; Generate code to register the thing at run time.
               (register-template-instance! #'#,name #'#,actual
                                            #'#,instance))))))

    (syntax-case s (register! lookup exists? instantiations)
      ((_ register! name param body)
       ;; Register NAME as a template on PARAM with the given BODY.
       (begin
         (register-template! #'name #'param #'body)

         ;; Generate code to register the template at run time.  XXX: Because
         ;; of this, BODY must not contain ellipses.
         #'(register-template! #'name #'param #'body)))
      ((_ lookup name actual)
       ;; Search for an instance of template NAME for this ACTUAL parameter.
       ;; On success, expand to the identifier of the instance; otherwise
       ;; expand to #f.
       (any (matching-instance? #'name #'actual) %template-instances))
      ((_ exists? name actual)
       ;; Likewise, but return a Boolean.
       (let ((result (->bool
                      (any (matching-instance? #'name #'actual)
                           %template-instances))))
         (unless result
           (format (current-warning-port)
                   "~a: warning: no specialization of template '~a' for '~a'~%"
                   location-string
                   (syntax->datum #'name) (syntax->datum #'actual)))
         result))
      ((_ instantiations actual)
       ;; Expand to the definitions of all the existing templates
       ;; specialized for ACTUAL.
       #`(begin
           #,@(hash-map->list (cut instance-definition <> <> #'actual)
                              %templates))))))

(define-syntax define-template
  (lambda (s)
    "Define a template, which is a procedure that can be specialized over its
first argument.  In our case, the first argument is typically the identifier
of a monad.

Defining templates for procedures like 'mapm' allows us to make have a
specialized version of those procedures for each monad that we define, such
that calls to:

  (mapm %state-monad proc lst)

automatically expand to:

  (#{ mapm %state-monad instance}# proc lst)

Here, #{ mapm %state-monad instance}# is specialized for %state-monad, and
thus it contains inline calls to %state-bind and %state-return.  This avoids
repeated calls to 'struct-ref' to get the 'bind' and 'return' procedure of the
monad, and allows 'bind' and 'return' to be inlined, which in turn allows for
more optimizations."
    (syntax-case s ()
      ((_ (name arg0 args ...) body ...)
       (with-syntax ((generic-name (datum->syntax
                                    #'name
                                    (symbol-append '#{ %}#
                                                   (syntax->datum #'name)
                                                   '-generic)))
                     (original-name #'name))
         #`(begin
             (template-directory register! name arg0
                                 (lambda (args ...)
                                   body ...))
             (define (generic-name arg0 args ...)
               ;; The generic instance of NAME, for when no specialization was
               ;; found.
               body ...)

             (define-syntax name
               (lambda (s)
                 (syntax-case s ()
                   ((_ arg0* args ...)
                    ;; Expand to either the specialized instance or the
                    ;; generic instance of template ORIGINAL-NAME.
                    #'(if (template-directory exists? original-name arg0*)
                          ((template-directory lookup original-name arg0*)
                           args ...)
                          (generic-name arg0* args ...)))
                   (_
                    #'generic-name))))))))))

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

(define-syntax-parameter-once >>=
  ;; The name 'bind' is already taken, so we choose this (obscure) symbol.
  (lambda (s)
    (syntax-violation '>>= ">>= (bind) used outside of 'with-monad'" s)))

(define-syntax-parameter-once return
  (lambda (s)
    (syntax-violation 'return "return used outside of 'with-monad'" s)))

(define-syntax-rule (bind-syntax bind)
  "Return a macro transformer that handles the expansion of '>>=' expressions
using BIND as the binary bind operator.

This macro exists to allow the expansion of n-ary '>>=' expressions, even
though BIND is simply binary, as in:

  (with-monad %state-monad
    (>>= (return 1)
         (lift 1+ %state-monad)
         (lift 1+ %state-monad)))
"
  (lambda (stx)
    (define (expand body)
      (syntax-case body ()
        ((_ mval mproc)
         #'(bind mval mproc))
        ((x mval mproc0 mprocs (... ...))
         (expand #'(>>= (>>= mval mproc0)
                        mprocs (... ...))))))

    (expand stx)))

(define-syntax with-monad
  (lambda (s)
    "Evaluate BODY in the context of MONAD, and return its result."
    (syntax-case s ()
      ((_ monad body ...)
       (eq? 'macro (syntax-local-binding #'monad))
       ;; MONAD is a syntax transformer, so we can obtain the bind and return
       ;; methods by directly querying it.
       #'(syntax-parameterize ((>>=    (bind-syntax (monad %bind)))
                               (return (identifier-syntax (monad %return))))
           body ...))
      ((_ monad body ...)
       ;; MONAD refers to the <monad> record that represents the monad at run
       ;; time, so use the slow method.
       #'(syntax-parameterize ((>>=    (bind-syntax
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

(define-syntax mbegin
  (syntax-rules (%current-monad)
    "Bind MEXP and the following monadic expressions in sequence, returning
the result of the last expression.  Every expression in the sequence must be a
monadic expression."
    ((_ %current-monad mexp)
     mexp)
    ((_ %current-monad mexp rest ...)
     (>>= mexp
          (lambda (unused-value)
            (mbegin %current-monad rest ...))))
    ((_ monad mexp)
     (with-monad monad
       mexp))
    ((_ monad mexp rest ...)
     (with-monad monad
       (>>= mexp
            (lambda (unused-value)
              (mbegin monad rest ...)))))))

(define-syntax mwhen
  (syntax-rules ()
    "When CONDITION is true, evaluate the sequence of monadic expressions
MEXP0..MEXP* as in an 'mbegin'.  When CONDITION is false, return *unspecified*
in the current monad.  Every expression in the sequence must be a monadic
expression."
    ((_ condition mexp0 mexp* ...)
     (if condition
         (mbegin %current-monad
           mexp0 mexp* ...)
         (return *unspecified*)))))

(define-syntax munless
  (syntax-rules ()
    "When CONDITION is false, evaluate the sequence of monadic expressions
MEXP0..MEXP* as in an 'mbegin'.  When CONDITION is true, return *unspecified*
in the current monad.  Every expression in the sequence must be a monadic
expression."
    ((_ condition mexp0 mexp* ...)
     (if condition
         (return *unspecified*)
         (mbegin %current-monad
           mexp0 mexp* ...)))))

(define-syntax define-lift
  (syntax-rules ()
    ((_ liftn (args ...))
     (define-syntax liftn
       (lambda (s)
         "Lift PROC to MONAD---i.e., return a monadic function in MONAD."
         (syntax-case s ()
           ((liftn proc monad)
            ;; Inline the result of lifting PROC, such that 'return' can in
            ;; turn be open-coded.
            #'(lambda (args ...)
                (with-monad monad
                  (return (proc args ...)))))
           (id
            (identifier? #'id)
            ;; Slow path: Return a closure-returning procedure (we don't
            ;; guarantee (eq? LIFTN LIFTN), but that's fine.)
            #'(lambda (proc monad)
                (lambda (args ...)
                  (with-monad monad
                    (return (proc args ...))))))))))))

(define-lift lift0 ())
(define-lift lift1 (a))
(define-lift lift2 (a b))
(define-lift lift3 (a b c))
(define-lift lift4 (a b c d))
(define-lift lift5 (a b c d e))
(define-lift lift6 (a b c d e f))
(define-lift lift7 (a b c d e f g))

(define (lift proc monad)
  "Lift PROC, a procedure that accepts an arbitrary number of arguments, to
MONAD---i.e., return a monadic function in MONAD."
  (lambda args
    (with-monad monad
      (return (apply proc args)))))

(define-template (foldm monad mproc init lst)
  "Fold MPROC over LST and return a monadic value seeded by INIT.

  (foldm %state-monad (lift2 cons %state-monad) '() '(a b c))
  => '(c b a)  ;monadic
"
  (with-monad monad
    (let loop ((lst    lst)
               (result init))
      (match lst
        (()
         (return result))
        ((head . tail)
         (>>= (mproc head result)
              (lambda (result)
                (loop tail result))))))))

(define-template (mapm monad mproc lst)
  "Map MPROC over LST and return a monadic list.

  (mapm %state-monad (lift1 1+ %state-monad) '(0 1 2))
  => (1 2 3)  ;monadic
"
  ;; XXX: We don't use 'foldm' because template specialization wouldn't work
  ;; in this context.
  (with-monad monad
    (let mapm ((lst    lst)
               (result '()))
      (match lst
        (()
         (return (reverse result)))
        ((head . tail)
         (>>= (mproc head)
              (lambda (head)
                (mapm tail (cons head result)))))))))

(define-template (sequence monad lst)
  "Turn the list of monadic values LST into a monadic list of values, by
evaluating each item of LST in sequence."
  (with-monad monad
    (let seq ((lstx   lst)
              (result '()))
      (match lstx
        (()
         (return (reverse result)))
        ((head . tail)
         (>>= head
              (lambda (item)
                (seq tail (cons item result)))))))))

(define-template (anym monad mproc lst)
  "Apply MPROC to the list of values LST; return as a monadic value the first
value for which MPROC returns a true monadic value or #f.  For example:

  (anym %state-monad (lift1 odd? %state-monad) '(0 1 2))
  => #t   ;monadic
"
  (with-monad monad
    (let loop ((lst lst))
      (match lst
        (()
         (return #f))
        ((head . tail)
         (>>= (mproc head)
              (lambda (result)
                (if result
                    (return result)
                    (loop tail)))))))))

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
;;; State monad.
;;;

(define-inlinable (state-return value)
  (lambda (state)
    (values value state)))

(define-inlinable (state-bind mvalue mproc)
  "Bind MVALUE, a value in the state monad, and pass it to MPROC."
  (lambda (state)
    (call-with-values
        (lambda ()
          (mvalue state))
      (lambda (value state)
        ;; Note: as of Guile 2.0.11, declaring a variable to hold the result
        ;; of (mproc value) prevents a bit of unfolding/inlining.
        ((mproc value) state)))))

(define-monad %state-monad
  (bind state-bind)
  (return state-return))

(define* (run-with-state mval #:optional (state '()))
  "Run monadic value MVAL starting with STATE as the initial state.  Return
two values: the resulting value, and the resulting state."
  (mval state))

(define-inlinable (current-state)
  "Return the current state as a monadic value."
  (lambda (state)
    (values state state)))

(define-inlinable (set-current-state value)
  "Set the current state to VALUE and return the previous state as a monadic
value."
  (lambda (state)
    (values state value)))

(define (state-pop)
  "Pop a value from the current state and return it as a monadic value.  The
state is assumed to be a list."
  (lambda (state)
    (match state
      ((head . tail)
       (values head tail)))))

(define (state-push value)
  "Push VALUE to the current state, which is assumed to be a list, and return
the previous state as a monadic value."
  (lambda (state)
    (values state (cons value state))))

;;; monads.scm end here
