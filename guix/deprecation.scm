;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix deprecation)
  #:use-module (guix i18n)
  #:use-module (ice-9 format)
  #:export (define-deprecated
            define-deprecated/alias
            warn-about-deprecation
            deprecation-warning-port))

;;; Commentary:
;;;
;;; Provide a mechanism to mark bindings as deprecated.
;;;
;;; We don't reuse (guix ui) mostly to avoid pulling in too many things.
;;;
;;; Code:

(define deprecation-warning-port
  ;; Port where deprecation warnings go.
  (make-parameter (current-error-port)))

(define (source-properties->location-string properties)
  "Return a human-friendly, GNU-standard representation of PROPERTIES, a
source property alist."
  (let ((file   (assq-ref properties 'filename))
        (line   (assq-ref properties 'line))
        (column (assq-ref properties 'column)))
    (if (and file line column)
        (format #f "~a:~a:~a" file (+ 1 line) column)
        (G_ "<unknown location>"))))

(define* (warn-about-deprecation variable properties
                                 #:key replacement)
  (format (deprecation-warning-port)
          (G_ "~a: warning: '~a' is deprecated~@[, use '~a' instead~]~%")
          (source-properties->location-string properties)
          variable replacement))

(define-syntax define-deprecated
  (lambda (s)
    "Define a deprecated variable or procedure, along these lines:

  (define-deprecated foo bar 42)
  (define-deprecated (baz x y) qux (qux y x))

This will write a deprecation warning to DEPRECATION-WARNING-PORT."
    (syntax-case s ()
      ((_ (proc formals ...) replacement body ...)
       #'(define-deprecated proc replacement
           (lambda* (formals ...) body ...)))
      ((_ variable replacement exp)
       (identifier? #'variable)
       (with-syntax ((real (datum->syntax
                            #'variable
                            (symbol-append '%
                                           (syntax->datum #'variable)
                                           '/deprecated))))
         #`(begin
             (define real
               (begin
                 (lambda () replacement)          ;just to ensure it's bound
                 exp))

             (define-syntax variable
               (lambda (s)
                 (warn-about-deprecation 'variable (syntax-source s)
                                         #:replacement 'replacement)
                 (syntax-case s ()
                   ((_ args (... ...))
                    #'(real args (... ...)))
                   (id
                    (identifier? #'id)
                    #'real))))))))))

(define-syntax-rule (define-deprecated/alias deprecated replacement)
  "Define as an alias a deprecated variable, procedure, or macro, along
these lines:

  (define-deprecated/alias nix-server? store-connection?)

where 'nix-server?' is the deprecated name for 'store-connection?'.

This will write a deprecation warning to DEPRECATION-WARNING-PORT."
  (define-syntax deprecated
    (lambda (s)
      (warn-about-deprecation 'deprecated (syntax-source s)
                              #:replacement 'replacement)
      (syntax-case s ()
        ((_ args (... ...))
         #'(replacement args (... ...)))
        (id
         (identifier? #'id)
         #'replacement)))))
