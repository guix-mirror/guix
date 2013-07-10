;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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
            alist->record
            object->fields
            recutils->alist))

;;; Commentary:
;;;
;;; Utilities for dealing with Scheme records.
;;;
;;; Code:

(define-syntax define-record-type*
  (lambda (s)
    "Define the given record type such that an additional \"syntactic
constructor\" is defined, which allows instances to be constructed with named
field initializers, à la SRFI-35, as well as default values."
    (define (make-syntactic-constructor type name ctor fields thunked defaults)
      "Make the syntactic constructor NAME for TYPE, that calls CTOR, and
expects all of FIELDS to be initialized.  DEFAULTS is the list of
FIELD/DEFAULT-VALUE tuples, and THUNKED is the list of identifiers of
thunked fields."
      (with-syntax ((type     type)
                    (name     name)
                    (ctor     ctor)
                    (expected fields)
                    (defaults defaults))
        #`(define-syntax name
            (lambda (s)
              (define (record-inheritance orig-record field+value)
                ;; Produce code that returns a record identical to
                ;; ORIG-RECORD, except that values for the FIELD+VALUE alist
                ;; prevail.
                (define (field-inherited-value f)
                  (and=> (find (lambda (x)
                                 (eq? f (car (syntax->datum x))))
                               field+value)
                         car))

                #`(make-struct type 0
                               #,@(map (lambda (field index)
                                         (or (field-inherited-value field)
                                             #`(struct-ref #,orig-record
                                                           #,index)))
                                       'expected
                                       (iota (length 'expected)))))

              (define (thunked-field? f)
                (memq (syntax->datum f) '#,thunked))

              (define (field-bindings field+value)
                ;; Return field to value bindings, for use in `letrec*' below.
                (map (lambda (field+value)
                       (syntax-case field+value ()
                         ((field value)
                          #`(field
                             #,(if (thunked-field? #'field)
                                   #'(lambda () value)
                                   #'value)))))
                     field+value))

              (syntax-case s (inherit #,@fields)
                ((_ (inherit orig-record) (field value) (... ...))
                 #`(letrec* #,(field-bindings #'((field value) (... ...)))
                     #,(record-inheritance #'orig-record
                                           #'((field value) (... ...)))))
                ((_ (field value) (... ...))
                 (let ((fields (map syntax->datum #'(field (... ...))))
                       (dflt   (map (match-lambda
                                     ((f v)
                                      (list (syntax->datum f) v)))
                                    #'defaults)))

                   (define (field-value f)
                     (or (and=> (find (lambda (x)
                                        (eq? f (car (syntax->datum x))))
                                      #'((field value) (... ...)))
                                car)
                         (let ((value
                                (car (assoc-ref dflt
                                                (syntax->datum f)))))
                           (if (thunked-field? f)
                               #`(lambda () #,value)
                               value))))

                   (let-syntax ((error*
                                 (syntax-rules ()
                                   ((_ fmt args (... ...))
                                    (syntax-violation 'name
                                                      (format #f fmt args
                                                              (... ...))
                                                      s)))))
                     (let ((fields (append fields (map car dflt))))
                       (cond ((lset= eq? fields 'expected)
                              #`(letrec* #,(field-bindings
                                            #'((field value) (... ...)))
                                  (ctor #,@(map field-value 'expected))))
                             ((pair? (lset-difference eq? fields 'expected))
                              (error* "extraneous field initializers ~a"
                                      (lset-difference eq? fields 'expected)))
                             (else
                              (error* "missing field initializers ~a"
                                      (lset-difference eq? 'expected
                                                       fields)))))))))))))

    (define (field-default-value s)
      (syntax-case s (default)
        ((field (default val) _ ...)
         (list #'field #'val))
        ((field _ options ...)
         (field-default-value #'(field options ...)))
        (_ #f)))

    (define (thunked-field? s)
      ;; Return the field name if the field defined by S is thunked.
      (syntax-case s (thunked)
        ((field (thunked) _ ...)
         #'field)
        ((field _ options ...)
         (thunked-field? #'(field options ...)))
        (_ #f)))

    (define (thunked-field-accessor-name field)
      ;; Return the name (an unhygienic syntax object) of the "real"
      ;; getter for field, which is assumed to be a thunked field.
      (syntax-case field ()
        ((field get options ...)
         (let* ((getter      (syntax->datum #'get))
                (real-getter (symbol-append '% getter '-real)))
           (datum->syntax #'get real-getter)))))

    (define (field-spec->srfi-9 field)
      ;; Convert a field spec of our style to a SRFI-9 field spec of the
      ;; form (field get).
      (syntax-case field ()
        ((name get options ...)
         #`(name
            #,(if (thunked-field? field)
                  (thunked-field-accessor-name field)
                  #'get)))))

    (define (thunked-field-accessor-definition field)
      ;; Return the real accessor for FIELD, which is assumed to be a
      ;; thunked field.
      (syntax-case field ()
        ((name get _ ...)
         (with-syntax ((real-get (thunked-field-accessor-name field)))
           #'(define-inlinable (get x)
               ;; The real value of that field is a thunk, so call it.
               ((real-get x)))))))

    (syntax-case s ()
      ((_ type syntactic-ctor ctor pred
          (field get options ...) ...)
       (let* ((field-spec #'((field get options ...) ...)))
         (with-syntax (((field-spec* ...)
                        (map field-spec->srfi-9 field-spec))
                       ((thunked-field-accessor ...)
                        (filter-map (lambda (field)
                                      (and (thunked-field? field)
                                           (thunked-field-accessor-definition
                                            field)))
                                    field-spec)))
           #`(begin
               (define-record-type type
                 (ctor field ...)
                 pred
                 field-spec* ...)
               (begin thunked-field-accessor ...)
               #,(make-syntactic-constructor #'type #'syntactic-ctor #'ctor
                                             #'(field ...)
                                             (filter-map thunked-field? field-spec)
                                             (filter-map field-default-value
                                                         #'((field options ...)
                                                            ...))))))))))

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

(define %recutils-field-rx
  (make-regexp "^([[:graph:]]+): (.*)$"))

(define %recutils-comment-rx
  ;; info "(recutils) Comments"
  (make-regexp "^#"))

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
          ((regexp-exec %recutils-comment-rx line)
           (loop (read-line port) result))
          ((regexp-exec %recutils-field-rx line)
           =>
           (lambda (match)
             (loop (read-line port)
                   (alist-cons (match:substring match 1)
                               (match:substring match 2)
                               result))))
          (else
           (error "unmatched line" line)))))

;;; records.scm ends here
