;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
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
  #:autoload   (texinfo) (texi-fragment->stexi)
  #:autoload   (texinfo serialize) (stexi->texi)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (append-map))
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
            serialize-configuration
            define-maybe
            define-configuration
            validate-configuration
            generate-documentation
            serialize-package))

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
  "Assemble PARTS into a raw (unhygienic)  identifier."
  (datum->syntax ctx (symbol-append (syntax->datum parts) ...)))

(define-syntax define-maybe
  (lambda (x)
    (syntax-case x ()
      ((_ stem)
       (with-syntax
           ((stem?                (id #'stem #'stem #'?))
            (maybe-stem?          (id #'stem #'maybe- #'stem #'?))
            (serialize-stem       (id #'stem #'serialize- #'stem))
            (serialize-maybe-stem (id #'stem #'serialize-maybe- #'stem)))
         #'(begin
             (define (maybe-stem? val)
               (or (eq? val 'disabled) (stem? val)))
             (define (serialize-maybe-stem field-name val)
               (if (stem? val) (serialize-stem field-name val) ""))))))))

(define-syntax define-configuration
  (lambda (stx)
    (syntax-case stx ()
      ((_ stem (field (field-type def) doc) ...)
       (with-syntax (((field-getter ...)
                      (map (lambda (field)
                             (id #'stem #'stem #'- field))
                           #'(field ...)))
                     ((field-predicate ...)
                      (map (lambda (type)
                             (id #'stem type #'?))
                           #'(field-type ...)))
                     ((field-serializer ...)
                      (map (lambda (type)
                             (id #'stem #'serialize- type))
                           #'(field-type ...))))
           #`(begin
               (define-record-type* #,(id #'stem #'< #'stem #'>)
                 #,(id #'stem #'% #'stem)
                 #,(id #'stem #'make- #'stem)
                 #,(id #'stem #'stem #'?)
                 (%location #,(id #'stem #'-location)
                            (default (and=> (current-source-location)
                                            source-properties->location))
                            (innate))
                 (field field-getter (default def))
                 ...)
               (define #,(id #'stem #'stem #'-fields)
                 (list (configuration-field
                        (name 'field)
                        (type 'field-type)
                        (getter field-getter)
                        (predicate field-predicate)
                        (serializer field-serializer)
                        (default-value-thunk (lambda () def))
                        (documentation doc))
                       ...))
               (define-syntax-rule (stem arg (... ...))
                 (let ((conf (#,(id #'stem #'% #'stem) arg (... ...))))
                   (validate-configuration conf
                                           #,(id #'stem #'stem #'-fields))
                   conf))))))))

(define (serialize-package field-name val)
  "")

;; A little helper to make it easier to document all those fields.
(define (generate-documentation documentation documentation-name)
  (define (str x) (object->string x))
  (define (generate configuration-name)
    (match (assq-ref documentation configuration-name)
      ((fields . sub-documentation)
       `((para "Available " (code ,(str configuration-name)) " fields are:")
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
                      (and (symbol? val) (not (eq? val '%invalid)))
                      (and (list? val) (and-map show-default? val))))
                `(deftypevr (% (category
                                (code ,(str configuration-name)) " parameter")
                               (data-type ,(str field-type))
                               (name ,(str field-name)))
                   ,@field-docs
                   ,@(if (show-default? default)
                         `((para "Defaults to " (samp ,(str default)) "."))
                         '())
                   ,@(append-map
                      generate
                      (or (assq-ref sub-documentation field-name) '())))))
            fields)))))
  (stexi->texi `(*fragment* . ,(generate documentation-name))))
