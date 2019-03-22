;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
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

(define-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:export (define-record-type*
            this-record

            alist->record
            object->fields
            recutils->alist
            match-record))

;;; Commentary:
;;;
;;; Utilities for dealing with Scheme records.
;;;
;;; Code:

(define-syntax record-error
  (syntax-rules ()
    "Report a syntactic error in use of CONSTRUCTOR."
    ((_ constructor form fmt args ...)
     (syntax-violation constructor
                       (format #f fmt args ...)
                       form))))

(eval-when (expand load eval)
  ;; The procedures below are needed both at run time and at expansion time.

  (define (current-abi-identifier type)
    "Return an identifier unhygienically derived from TYPE for use as its
\"current ABI\" variable."
    (let ((type-name (syntax->datum type)))
      (datum->syntax
       type
       (string->symbol
        (string-append "% " (symbol->string type-name)
                       " abi-cookie")))))

  (define (abi-check type cookie)
    "Return syntax that checks that the current \"application binary
interface\" (ABI) for TYPE is equal to COOKIE."
    (with-syntax ((current-abi (current-abi-identifier type)))
      #`(unless (eq? current-abi #,cookie)
          ;; The source file where this exception is thrown must be
          ;; recompiled.
          (throw 'record-abi-mismatch-error 'abi-check
                 "~a: record ABI mismatch; recompilation needed"
                 (list #,type) '()))))

  (define (report-invalid-field-specifier name bindings)
    "Report the first invalid binding among BINDINGS."
    (let loop ((bindings bindings))
      (syntax-case bindings ()
        (((field value) rest ...)                   ;good
         (loop #'(rest ...)))
        ((weird _ ...)                              ;weird!
         (syntax-violation name "invalid field specifier" #'weird)))))

  (define (report-duplicate-field-specifier name ctor)
    "Report the first duplicate identifier among the bindings in CTOR."
    (syntax-case ctor ()
      ((_ bindings ...)
       (let loop ((bindings #'(bindings ...))
                  (seen   '()))
         (syntax-case bindings ()
           (((field value) rest ...)
            (not (memq (syntax->datum #'field) seen))
            (loop #'(rest ...) (cons (syntax->datum #'field) seen)))
           ((duplicate rest ...)
            (syntax-violation name "duplicate field initializer"
                              #'duplicate))
           (()
            #t)))))))

(define-syntax-parameter this-record
  (lambda (s)
    "Return the record being defined.  This macro may only be used in the
context of the definition of a thunked field."
    (syntax-case s ()
      (id
       (identifier? #'id)
       (syntax-violation 'this-record
                         "cannot be used outside of a record instantiation"
                         #'id)))))

(define-syntax make-syntactic-constructor
  (syntax-rules ()
    "Make the syntactic constructor NAME for TYPE, that calls CTOR, and
expects all of EXPECTED fields to be initialized.  DEFAULTS is the list of
FIELD/DEFAULT-VALUE tuples, THUNKED is the list of identifiers of thunked
fields, and DELAYED is the list of identifiers of delayed fields.

ABI-COOKIE is the cookie (an integer) against which to check the run-time ABI
of TYPE matches the expansion-time ABI."
    ((_ type name ctor (expected ...)
        #:abi-cookie abi-cookie
        #:thunked thunked
        #:delayed delayed
        #:innate innate
        #:defaults defaults)
     (define-syntax name
       (lambda (s)
         (define (record-inheritance orig-record field+value)
           ;; Produce code that returns a record identical to ORIG-RECORD,
           ;; except that values for the FIELD+VALUE alist prevail.
           (define (field-inherited-value f)
             (and=> (find (lambda (x)
                            (eq? f (car (syntax->datum x))))
                          field+value)
                    car))

           ;; Make sure there are no unknown field names.
           (let* ((fields     (map (compose car syntax->datum) field+value))
                  (unexpected (lset-difference eq? fields '(expected ...))))
             (when (pair? unexpected)
               (record-error 'name s "extraneous field initializers ~a"
                             unexpected)))

           #`(make-struct/no-tail type
                          #,@(map (lambda (field index)
                                    (or (field-inherited-value field)
                                        (if (innate-field? field)
                                            (wrap-field-value
                                             field (field-default-value field))
                                            #`(struct-ref #,orig-record
                                                          #,index))))
                                  '(expected ...)
                                  (iota (length '(expected ...))))))

         (define (thunked-field? f)
           (memq (syntax->datum f) 'thunked))

         (define (delayed-field? f)
           (memq (syntax->datum f) 'delayed))

         (define (innate-field? f)
           (memq (syntax->datum f) 'innate))

         (define (wrap-field-value f value)
           (cond ((thunked-field? f)
                  #`(lambda (x)
                      (syntax-parameterize ((this-record
                                             (lambda (s)
                                               (syntax-case s ()
                                                 (id
                                                  (identifier? #'id)
                                                  #'x)))))
                        #,value)))
                 ((delayed-field? f)
                  #`(delay #,value))
                 (else value)))

         (define default-values
           ;; List of symbol/value tuples.
           (map (match-lambda
                  ((f v)
                   (list (syntax->datum f) v)))
                #'defaults))

         (define (field-default-value f)
           (car (assoc-ref default-values (syntax->datum f))))

         (define (field-bindings field+value)
           ;; Return field to value bindings, for use in 'let*' below.
           (map (lambda (field+value)
                  (syntax-case field+value ()
                    ((field value)
                     #`(field
                        #,(wrap-field-value #'field #'value)))))
                field+value))

         (syntax-case s (inherit expected ...)
           ((_ (inherit orig-record) (field value) (... ...))
            #`(let* #,(field-bindings #'((field value) (... ...)))
                #,(abi-check #'type abi-cookie)
                #,(record-inheritance #'orig-record
                                      #'((field value) (... ...)))))
           ((_ (field value) (... ...))
            (let ((fields (map syntax->datum #'(field (... ...)))))
              (define (field-value f)
                (or (find (lambda (x)
                            (eq? f (syntax->datum x)))
                          #'(field (... ...)))
                    (wrap-field-value f (field-default-value f))))

              ;; Pass S to make sure source location info is preserved.
              (report-duplicate-field-specifier 'name s)

              (let ((fields (append fields (map car default-values))))
                (cond ((lset= eq? fields '(expected ...))
                       #`(let* #,(field-bindings
                                  #'((field value) (... ...)))
                           #,(abi-check #'type abi-cookie)
                           (ctor #,@(map field-value '(expected ...)))))
                      ((pair? (lset-difference eq? fields
                                               '(expected ...)))
                       (record-error 'name s
                                     "extraneous field initializers ~a"
                                     (lset-difference eq? fields
                                                      '(expected ...))))
                      (else
                       (record-error 'name s
                                     "missing field initializers ~a"
                                     (lset-difference eq?
                                                      '(expected ...)
                                                      fields)))))))
           ((_ bindings (... ...))
            ;; One of BINDINGS doesn't match the (field value) pattern.
            ;; Report precisely which one is faulty, instead of letting the
            ;; "source expression failed to match any pattern" error.
            (report-invalid-field-specifier 'name
                                            #'(bindings (... ...))))))))))

(define-syntax-rule (define-field-property-predicate predicate property)
  "Define PREDICATE as a procedure that takes a syntax object and, when passed
a field specification, returns the field name if it has the given PROPERTY."
  (define (predicate s)
    (syntax-case s (property)
      ((field (property values (... ...)) _ (... ...))
       #'field)
      ((field _ properties (... ...))
       (predicate #'(field properties (... ...))))
      (_ #f))))

(define-syntax define-record-type*
  (lambda (s)
    "Define the given record type such that an additional \"syntactic
constructor\" is defined, which allows instances to be constructed with named
field initializers, à la SRFI-35, as well as default values.  An example use
may look like this:

  (define-record-type* <thing> thing make-thing
    thing?
    (name  thing-name (default \"chbouib\"))
    (port  thing-port
           (default (current-output-port)) (thunked))
    (loc   thing-location (innate) (default (current-source-location))))

This example defines a macro 'thing' that can be used to instantiate records
of this type:

  (thing
    (name \"foo\")
    (port (current-error-port)))

The value of 'name' or 'port' could as well be omitted, in which case the
default value specified in the 'define-record-type*' form is used:

  (thing)

The 'port' field is \"thunked\", meaning that calls like '(thing-port x)' will
actually compute the field's value in the current dynamic extent, which is
useful when referring to fluids in a field's value.

A field can also be marked as \"delayed\" instead of \"thunked\", in which
case its value is effectively wrapped in a (delay …) form.

It is possible to copy an object 'x' created with 'thing' like this:

  (thing (inherit x) (name \"bar\"))

This expression returns a new object equal to 'x' except for its 'name'
field and its 'loc' field---the latter is marked as \"innate\", so it is not
inherited."

    (define (field-default-value s)
      (syntax-case s (default)
        ((field (default val) _ ...)
         (list #'field #'val))
        ((field _ properties ...)
         (field-default-value #'(field properties ...)))
        (_ #f)))

    (define-field-property-predicate delayed-field? delayed)
    (define-field-property-predicate thunked-field? thunked)
    (define-field-property-predicate innate-field? innate)

    (define (wrapped-field? s)
      (or (thunked-field? s) (delayed-field? s)))

    (define (wrapped-field-accessor-name field)
      ;; Return the name (an unhygienic syntax object) of the "real"
      ;; getter for field, which is assumed to be a wrapped field.
      (syntax-case field ()
        ((field get properties ...)
         (let* ((getter      (syntax->datum #'get))
                (real-getter (symbol-append '% getter '-real)))
           (datum->syntax #'get real-getter)))))

    (define (field-spec->srfi-9 field)
      ;; Convert a field spec of our style to a SRFI-9 field spec of the
      ;; form (field get).
      (syntax-case field ()
        ((name get properties ...)
         #`(name
            #,(if (wrapped-field? field)
                  (wrapped-field-accessor-name field)
                  #'get)))))

    (define (thunked-field-accessor-definition field)
      ;; Return the real accessor for FIELD, which is assumed to be a
      ;; thunked field.
      (syntax-case field ()
        ((name get _ ...)
         (with-syntax ((real-get (wrapped-field-accessor-name field)))
           #'(define-inlinable (get x)
               ;; The real value of that field is a thunk, so call it.
               ((real-get x) x))))))

    (define (delayed-field-accessor-definition field)
      ;; Return the real accessor for FIELD, which is assumed to be a
      ;; delayed field.
      (syntax-case field ()
        ((name get _ ...)
         (with-syntax ((real-get (wrapped-field-accessor-name field)))
           #'(define-inlinable (get x)
               ;; The real value of that field is a promise, so force it.
               (force (real-get x)))))))

    (define (compute-abi-cookie field-specs)
      ;; Compute an "ABI cookie" for the given FIELD-SPECS.  We use
      ;; 'string-hash' because that's a better hash function that 'hash' on a
      ;; list of symbols.
      (syntax-case field-specs ()
        (((field get properties ...) ...)
         (string-hash (object->string
                       (syntax->datum #'((field properties ...) ...)))
                      most-positive-fixnum))))

    (syntax-case s ()
      ((_ type syntactic-ctor ctor pred
          (field get properties ...) ...)
       (let* ((field-spec #'((field get properties ...) ...))
              (thunked    (filter-map thunked-field? field-spec))
              (delayed    (filter-map delayed-field? field-spec))
              (innate     (filter-map innate-field? field-spec))
              (defaults   (filter-map field-default-value
                                      #'((field properties ...) ...)))
              (cookie     (compute-abi-cookie field-spec)))
         (with-syntax (((field-spec* ...)
                        (map field-spec->srfi-9 field-spec))
                       ((thunked-field-accessor ...)
                        (filter-map (lambda (field)
                                      (and (thunked-field? field)
                                           (thunked-field-accessor-definition
                                            field)))
                                    field-spec))
                       ((delayed-field-accessor ...)
                        (filter-map (lambda (field)
                                      (and (delayed-field? field)
                                           (delayed-field-accessor-definition
                                            field)))
                                    field-spec)))
           #`(begin
               (define-record-type type
                 (ctor field ...)
                 pred
                 field-spec* ...)
               (define #,(current-abi-identifier #'type)
                 #,cookie)
               thunked-field-accessor ...
               delayed-field-accessor ...
               (make-syntactic-constructor type syntactic-ctor ctor
                                           (field ...)
                                           #:abi-cookie #,cookie
                                           #:thunked #,thunked
                                           #:delayed #,delayed
                                           #:innate #,innate
                                           #:defaults #,defaults))))))))

(define* (alist->record alist make keys
                        #:optional (multiple-value-keys '()))
  "Apply MAKE to the values associated with KEYS in ALIST.  Items in KEYS that
are also in MULTIPLE-VALUE-KEYS are considered to occur possibly multiple
times in ALIST, and thus their value is a list."
  (let ((args (map (lambda (key)
                     (if (member key multiple-value-keys)
                         (filter-map (match-lambda
                                      ((k . v)
                                       (and (equal? k key) v)))
                                     alist)
                         (assoc-ref alist key)))
                   keys)))
    (apply make args)))

(define (object->fields object fields port)
  "Write OBJECT (typically a record) as a series of recutils-style fields to
PORT, according to FIELDS.  FIELDS must be a list of field name/getter pairs."
  (let loop ((fields fields))
    (match fields
      (()
       object)
      (((field . get) rest ...)
       (format port "~a: ~a~%" field (get object))
       (loop rest)))))

(define %recutils-field-charset
  ;; Valid characters starting a recutils field.
  ;; info "(recutils) Fields"
  (char-set-union char-set:upper-case
                  char-set:lower-case
                  (char-set #\%)))

(define (recutils->alist port)
  "Read a recutils-style record from PORT and return it as a list of key/value
pairs.  Stop upon an empty line (after consuming it) or EOF."
  (let loop ((line   (read-line port))
             (result '()))
    (cond ((eof-object? line)
           (reverse result))
          ((string-null? line)
           (if (null? result)
               (loop (read-line port) result)     ; leading space: ignore it
               (reverse result)))                 ; end-of-record marker
          (else
           ;; Now check the first character of LINE, since that's what the
           ;; recutils manual says is enough.
           (let ((first (string-ref line 0)))
             (cond
              ((char-set-contains? %recutils-field-charset first)
               (let* ((colon (string-index line #\:))
                      (field (string-take line colon))
                      (value (string-trim (string-drop line (+ 1 colon)))))
                 (loop (read-line port)
                       (alist-cons field value result))))
              ((eqv? first #\#)                   ;info "(recutils) Comments"
               (loop (read-line port) result))
              ((eqv? first #\+)                   ;info "(recutils) Fields"
               (let ((new-line (if (string-prefix? "+ " line)
                                   (string-drop line 2)
                                   (string-drop line 1))))
                (match result
                  (((field . value) rest ...)
                   (loop (read-line port)
                         `((,field . ,(string-append value "\n" new-line))
                           ,@rest))))))
              (else
               (error "unmatched line" line))))))))

(define-syntax match-record
  (syntax-rules ()
    "Bind each FIELD of a RECORD of the given TYPE to it's FIELD name.
The current implementation does not support thunked and delayed fields."
    ((_ record type (field fields ...) body ...)
     (if (eq? (struct-vtable record) type)
         ;; TODO compute indices and report wrong-field-name errors at
         ;;      expansion time
         ;; TODO support thunked and delayed fields
         (let ((field ((record-accessor type 'field) record)))
           (match-record record type (fields ...) body ...))
         (throw 'wrong-type-arg record)))
    ((_ record type () body ...)
     (begin body ...))))

;;; records.scm ends here
