;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Andrew Tropin <andrew@trop.in>
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

(define-module (gnu services configuration)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module ((guix utils) #:select (source-properties->location))
  #:use-module ((guix diagnostics) #:select (formatted-message location-file))
  #:use-module ((guix modules) #:select (file-name->module-name))
  #:use-module (guix i18n)
  #:autoload   (texinfo) (texi-fragment->stexi)
  #:autoload   (texinfo serialize) (stexi->texi)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (configuration-field
            configuration-field-name
            configuration-field-type
            configuration-missing-field
            configuration-field-error
            configuration-field-serializer
            configuration-field-getter
            configuration-field-default-value-thunk
            configuration-field-documentation

            configuration-error?

            define-configuration
            define-configuration/no-serialization
            no-serialization

            serialize-configuration
            define-maybe
            define-maybe/no-serialization
            validate-configuration
            generate-documentation
            configuration->documentation
            empty-serializer
            serialize-package

            filter-configuration-fields

            interpose
            list-of

            list-of-strings?
            alist?
            serialize-file-like
            text-config?
            serialize-text-config
            generic-serialize-alist-entry
            generic-serialize-alist))

;;; Commentary:
;;;
;;; Syntax for creating Scheme bindings to complex configuration files.
;;;
;;; Code:

(define-condition-type &configuration-error &error
  configuration-error?)

(define (configuration-error message)
  (raise (condition (&message (message message))
                    (&configuration-error))))
(define (configuration-field-error field val)
  (configuration-error
   (format #f "Invalid value for field ~a: ~s" field val)))
(define (configuration-missing-field kind field)
  (configuration-error
   (format #f "~a configuration missing required field ~a" kind field)))
(define (configuration-no-default-value kind field)
  (configuration-error
   (format #f "The field `~a' of the `~a' configuration record \
does not have a default value" field kind)))

(define-record-type* <configuration-field>
  configuration-field make-configuration-field configuration-field?
  (name configuration-field-name)
  (type configuration-field-type)
  (getter configuration-field-getter)
  (predicate configuration-field-predicate)
  (serializer configuration-field-serializer)
  (default-value-thunk configuration-field-default-value-thunk)
  (documentation configuration-field-documentation))

(define (serialize-configuration config fields)
  #~(string-append
     #$@(map (lambda (field)
               ((configuration-field-serializer field)
                (configuration-field-name field)
                ((configuration-field-getter field) config)))
             fields)))

(define (validate-configuration config fields)
  (for-each (lambda (field)
              (let ((val ((configuration-field-getter field) config)))
                (unless ((configuration-field-predicate field) val)
                  (configuration-field-error
                   (configuration-field-name field) val))))
            fields))

(define-syntax-rule (id ctx parts ...)
  "Assemble PARTS into a raw (unhygienic) identifier."
  (datum->syntax ctx (symbol-append (syntax->datum parts) ...)))

(define (define-maybe-helper serialize? prefix syn)
  (syntax-case syn ()
    ((_ stem)
     (with-syntax
         ((stem?            (id #'stem #'stem #'?))
          (maybe-stem?      (id #'stem #'maybe- #'stem #'?))
          (serialize-stem   (if prefix
                                (id #'stem prefix #'serialize- #'stem)
                                (id #'stem #'serialize- #'stem)))
          (serialize-maybe-stem (if prefix
                                    (id #'stem prefix #'serialize-maybe- #'stem)
                                    (id #'stem #'serialize-maybe- #'stem))))
       #`(begin
           (define (maybe-stem? val)
             (or (eq? val 'disabled) (stem? val)))
           #,@(if serialize?
                  (list #'(define (serialize-maybe-stem field-name val)
                            (if (stem? val)
                                (serialize-stem field-name val)
                                "")))
                  '()))))))

(define-syntax define-maybe
  (lambda (x)
    (syntax-case x (no-serialization prefix)
      ((_ stem (no-serialization))
       (define-maybe-helper #f #f #'(_ stem)))
      ((_ stem (prefix serializer-prefix))
       (define-maybe-helper #t #'serializer-prefix #'(_ stem)))
      ((_ stem)
       (define-maybe-helper #t #f #'(_ stem))))))

(define-syntax-rule (define-maybe/no-serialization stem)
  (define-maybe stem (no-serialization)))

(define (define-configuration-helper serialize? serializer-prefix syn)
  (syntax-case syn ()
    ((_ stem (field (field-type def ...) doc custom-serializer ...) ...)
     (with-syntax (((field-getter ...)
                    (map (lambda (field)
                           (id #'stem #'stem #'- field))
    			 #'(field ...)))
                   ((field-predicate ...)
                    (map (lambda (type)
                           (id #'stem type #'?))
    			 #'(field-type ...)))
                   ((field-default ...)
                    (map (match-lambda
    			   ((field-type default-value)
                            default-value)
    			   ((field-type)
                            ;; Quote `undefined' to prevent a possibly
                            ;; unbound warning.
                            (syntax 'undefined)))
    			 #'((field-type def ...) ...)))
                   ((field-serializer ...)
                    (map (lambda (type custom-serializer)
                           (and serialize?
                                (match custom-serializer
                                  ((serializer)
                                   serializer)
                                  (()
                                   (if serializer-prefix
                                       (id #'stem
                                           serializer-prefix
                                           #'serialize- type)
                                       (id #'stem #'serialize- type))))))
                         #'(field-type ...)
                         #'((custom-serializer ...) ...))))
       #`(begin
    	   (define-record-type* #,(id #'stem #'< #'stem #'>)
    	     #,(id #'stem #'% #'stem)
    	     #,(id #'stem #'make- #'stem)
    	     #,(id #'stem #'stem #'?)
    	     (%location #,(id #'stem #'stem #'-location)
    			(default (and=> (current-source-location)
    					source-properties->location))
    			(innate))
    	     #,@(map (lambda (name getter def)
    		       (if (eq? (syntax->datum def) (quote 'undefined))
    			   #`(#,name #,getter)
    			   #`(#,name #,getter (default #,def))))
    		     #'(field ...)
    		     #'(field-getter ...)
    		     #'(field-default ...)))
    	   (define #,(id #'stem #'stem #'-fields)
    	     (list (configuration-field
    		    (name 'field)
    		    (type 'field-type)
    		    (getter field-getter)
    		    (predicate field-predicate)
    		    (serializer field-serializer)
    		    (default-value-thunk
    		      (lambda ()
    			(display '#,(id #'stem #'% #'stem))
    			(if (eq? (syntax->datum field-default)
    				 'undefined)
    			    (configuration-no-default-value
    			     '#,(id #'stem #'% #'stem) 'field)
    			    field-default)))
    		    (documentation doc))
    		   ...))
    	   (define-syntax-rule (stem arg (... ...))
    	     (let ((conf (#,(id #'stem #'% #'stem) arg (... ...))))
    	       (validate-configuration conf
    				       #,(id #'stem #'stem #'-fields))
    	       conf)))))))

(define no-serialization         ;syntactic keyword for 'define-configuration'
  '(no serialization))

(define-syntax define-configuration
  (lambda (s)
    (syntax-case s (no-serialization prefix)
      ((_ stem (field (field-type def ...) doc custom-serializer ...) ...
          (no-serialization))
       (define-configuration-helper
         #f #f #'(_ stem (field (field-type def ...) doc custom-serializer ...)
                 ...)))
      ((_ stem  (field (field-type def ...) doc custom-serializer ...) ...
          (prefix serializer-prefix))
       (define-configuration-helper
         #t #'serializer-prefix #'(_ stem (field (field-type def ...)
                                                 doc custom-serializer ...)
                 ...)))
      ((_ stem (field (field-type def ...) doc custom-serializer ...) ...)
       (define-configuration-helper
         #t #f #'(_ stem (field (field-type def ...) doc custom-serializer ...)
                 ...))))))

(define-syntax-rule (define-configuration/no-serialization
                      stem (field (field-type def ...)
                                  doc custom-serializer ...) ...)
  (define-configuration stem (field (field-type def ...)
                                    doc custom-serializer ...) ...
    (no-serialization)))

(define (empty-serializer field-name val) "")
(define serialize-package empty-serializer)

;; A little helper to make it easier to document all those fields.
(define (generate-documentation documentation documentation-name)
  (define (str x) (object->string x))

  (define (package->symbol package)
    "Return the first symbol name of a package that matches PACKAGE, else #f."
    (let* ((module (file-name->module-name
                    (location-file (package-location package))))
           (symbols (filter-map
                     identity
                     (module-map (lambda (symbol var)
                                   (and (equal? package (variable-ref var))
                                        symbol))
                                 (resolve-module module)))))
      (if (null? symbols)
          #f
          (first symbols))))

  (define (generate configuration-name)
    (match (assq-ref documentation configuration-name)
      ((fields . sub-documentation)
       `((deftp (% (category "Data Type") (name ,(str configuration-name)))
           (para "Available " (code ,(str configuration-name)) " fields are:")
           (table
            (% (formatter (asis)))
            ,@(map
               (lambda (f)
                 (let ((field-name (configuration-field-name f))
                       (field-type (configuration-field-type f))
                       (field-docs (cdr (texi-fragment->stexi
                                         (configuration-field-documentation f))))
                       (default (catch #t
                                  (configuration-field-default-value-thunk f)
                                  (lambda _ '%invalid))))
                   (define (show-default? val)
                     (or (string? val) (number? val) (boolean? val)
                         (package? val)
                         (and (symbol? val) (not (eq? val '%invalid)))
                         (and (list? val) (and-map show-default? val))))

                   (define (show-default val)
                     (cond
                      ((package? val)
                       (symbol->string (package->symbol val)))
                      (else (str val))))

                   `(entry (% (heading
                               (code ,(str field-name))
                               ,@(if (show-default? default)
                                     `(" (default: "
                                       (code ,(show-default default)) ")")
                                     '())
                               " (type: " ,(str field-type) ")"))
                           (para ,@field-docs)
                           ,@(append-map
                              generate
                              (or (assq-ref sub-documentation field-name)
                                  '())))))
               fields)))))))
  (stexi->texi `(*fragment* . ,(generate documentation-name))))

(define (configuration->documentation configuration-symbol)
  "Take CONFIGURATION-SYMBOL, the symbol corresponding to the name used when
defining a configuration record with DEFINE-CONFIGURATION, and output the
Texinfo documentation of its fields."
  ;; This is helper for a simple, straight-forward application of
  ;; GENERATE-DOCUMENTATION.
  (let ((fields-getter (module-ref (current-module)
                                   (symbol-append configuration-symbol
                                                  '-fields))))
    (display (generate-documentation `((,configuration-symbol ,fields-getter))
                                     configuration-symbol))))

(define* (filter-configuration-fields configuration-fields fields
                                      #:optional negate?)
  "Retrieve the fields listed in FIELDS from CONFIGURATION-FIELDS.
If NEGATE? is @code{#t}, retrieve all fields except FIELDS."
  (filter (lambda (field)
            (let ((member? (member (configuration-field-name field) fields)))
              (if (not negate?) member? (not member?))))
          configuration-fields))


(define* (interpose ls  #:optional (delimiter "\n") (grammar 'infix))
  "Same as @code{string-join}, but without join and string, returns an
DELIMITER interposed LS.  Support 'infix and 'suffix GRAMMAR values."
  (when (not (member grammar '(infix suffix)))
    (raise
     (formatted-message
      (G_ "The GRAMMAR value must be 'infix or 'suffix, but ~a provided.")
      grammar)))
  (fold-right (lambda (e acc)
                (cons e
                      (if (and (null? acc) (eq? grammar 'infix))
                          acc
                          (cons delimiter acc))))
              '() ls))

(define (list-of pred?)
  "Return a procedure that takes a list and check if all the elements of
the list result in @code{#t} when applying PRED? on them."
    (lambda (x)
      (if (list? x)
          (every pred? x)
          #f)))


(define list-of-strings?
  (list-of string?))

(define alist? list?)

(define serialize-file-like empty-serializer)

(define (text-config? config)
  (list-of file-like?))
(define (serialize-text-config field-name val)
  #~(string-append
     #$@(interpose
         (map
          (lambda (e)
            #~(begin
                (use-modules (ice-9 rdelim))
                (with-fluids ((%default-port-encoding "UTF-8"))
                  (with-input-from-file #$e read-string))))
          val)
         "\n" 'suffix)))

(define ((generic-serialize-alist-entry serialize-field) entry)
  "Apply the SERIALIZE-FIELD procedure on the field and value of ENTRY."
  (match entry
    ((field . val) (serialize-field field val))))

(define (generic-serialize-alist combine serialize-field fields)
  "Generate a configuration from an association list FIELDS.

SERIALIZE-FIELD is a procedure that takes two arguments, it will be
applied on the fields and values of FIELDS using the
@code{generic-serialize-alist-entry} procedure.

COMBINE is a procedure that takes one or more arguments and combines
all the alist entries into one value, @code{string-append} or
@code{append} are usually good candidates for this.

See the @code{serialize-alist} procedure in `@code{(gnu home services
version-control}' for an example usage.)}"
  (apply combine
         (map (generic-serialize-alist-entry serialize-field) fields)))
