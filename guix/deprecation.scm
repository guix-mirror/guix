;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix diagnostics)
  #:autoload   (guix utils) (source-properties->location)
  #:export (define-deprecated

            define-deprecated/public
            define-deprecated/alias
            warn-about-deprecation))

;;; Commentary:
;;;
;;; Provide a mechanism to mark bindings as deprecated.
;;;
;;; Code:

(define* (warn-about-deprecation variable properties
                                 #:key replacement)
  (let ((location (and properties (source-properties->location properties))))
    (if replacement
        (warning location (G_ "'~a' is deprecated, use '~a' instead~%")
                 variable replacement)
        (warning location (G_ "'~a' is deprecated~%")
                 variable))))

(define-syntax public (syntax-rules ()))          ;private syntactic keyword

(define-syntax define-deprecated
  (lambda (s)
    "Define a deprecated variable or procedure, along these lines:

  (define-deprecated foo bar 42)
  (define-deprecated old new)
  (define-deprecated (baz x y) qux (qux y x))

This will write a deprecation warning to GUIX-WARNING-PORT."
    (syntax-case s ()
      ((_ (proc formals ...) replacement body ...)
       #'(define-deprecated proc replacement
           (lambda* (formals ...) body ...)))
      ((_ variable replacement exp)
       #'(define-deprecated private variable replacement exp))
      ((_ visibility variable replacement exp)
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
                    #'real))))

             ;; When asking for public visibility, export both REAL and
             ;; VARIABLE.  Exporting REAL is useful when defining deprecated
             ;; packages: there must be a public variable bound to a package
             ;; so that the (guix discover) machinery finds it.
             #,(if (free-identifier=? #'visibility #'public)
                   #'(export real variable)
                   #'(begin)))))
      ((_ variable alias)
       (identifier? #'alias)
       #'(define-deprecated variable alias alias)))))

(define-syntax-rule (define-deprecated/public body ...)
  "Like 'define/deprecated', but export all the newly introduced bindings."
  (define-deprecated public body ...))

(define-syntax-rule (define-deprecated/alias deprecated replacement)
  "Define as an alias a deprecated variable, procedure, or macro, along
these lines:

  (define-deprecated/alias nix-server? store-connection?)

where 'nix-server?' is the deprecated name for 'store-connection?'.

This will write a deprecation warning to GUIX-WARNING-PORT."
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
