;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix derivations)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (guix store)
  #:export (derivation?
            derivation-outputs
            derivation-inputs
            derivation-sources
            derivation-system
            derivation-builder-arguments
            derivation-builder-environment-vars

            derivation-output?
            derivation-output-path
            derivation-output-hash-algo
            derivation-output-hash

            derivation-input?
            derivation-input-path
            derivation-input-sub-derivations

            fixed-output-derivation?
            derivation-hash

            read-derivation
            write-derivation))

;;;
;;; Nix derivations, as implemented in Nix's `derivations.cc'.
;;;

(define-record-type <derivation>
  (make-derivation outputs inputs sources system builder args env-vars)
  derivation?
  (outputs  derivation-outputs)      ; list of name/<derivation-output> pairs
  (inputs   derivation-inputs)       ; list of <derivation-input>
  (sources  derivation-sources)      ; list of store paths
  (system   derivation-system)       ; string
  (builder  derivation-builder)      ; store path
  (args     derivation-builder-arguments)         ; list of strings
  (env-vars derivation-builder-environment-vars)) ; list of name/value pairs

(define-record-type <derivation-output>
  (make-derivation-output path hash-algo hash)
  derivation-output?
  (path       derivation-output-path)             ; store path
  (hash-algo  derivation-output-hash-algo)        ; symbol | #f
  (hash       derivation-output-hash))            ; symbol | #f

(define-record-type <derivation-input>
  (make-derivation-input path sub-derivations)
  derivation-input?
  (path            derivation-input-path)             ; store path
  (sub-derivations derivation-input-sub-derivations)) ; list of strings

(define (fixed-output-derivation? drv)
  "Return #t if DRV is a fixed-output derivation, such as the result of a
download with a fixed hash (aka. `fetchurl')."
  (match drv
    (($ <derivation>
        (($ <derivation-output> _ (? symbol?) (? string?))))
     #t)
    (_ #f)))

(define (read-derivation drv-port)
  "Read the derivation from DRV-PORT and return the corresponding
<derivation> object."

  (define comma (string->symbol ","))

  (define (ununquote x)
    (match x
      (('unquote x) (ununquote x))
      ((x ...)      (map ununquote x))
      (_            x)))

  (define (outputs->alist x)
    (fold-right (lambda (output result)
                  (match output
                    ((name path "" "")
                     (alist-cons name
                                 (make-derivation-output path #f #f)
                                 result))
                    ((name path hash-algo hash)
                     ;; fixed-output
                     (let ((algo (string->symbol hash-algo)))
                       (alist-cons name
                                   (make-derivation-output path algo hash)
                                   result)))))
                '()
                x))

  (define (make-input-drvs x)
    (fold-right (lambda (input result)
                  (match input
                    ((path (sub-drvs ...))
                     (cons (make-derivation-input path sub-drvs)
                           result))))
                '()
                x))

  (let loop ((exp    (read drv-port))
             (result '()))
    (match exp
      ((? eof-object?)
       (let ((result (reverse result)))
         (match result
           (('Derive ((outputs ...) (input-drvs ...)
                      (input-srcs ...)
                      (? string? system)
                      (? string? builder)
                      ((? string? args) ...)
                      ((var value) ...)))
            (make-derivation (outputs->alist outputs)
                             (make-input-drvs input-drvs)
                             input-srcs
                             system builder args
                             (fold-right alist-cons '() var value)))
           (_
            (error "failed to parse derivation" drv-port result)))))
      ((? (cut eq? <> comma))
       (loop (read drv-port) result))
      (_
       (loop (read drv-port)
             (cons (ununquote exp) result))))))

(define (write-derivation drv port)
  "Write the ATerm-like serialization of DRV to PORT.  See Section 2.4 of
Eelco Dolstra's PhD dissertation for an overview of a previous version of
that form."
  (define (list->string lst)
    (string-append "[" (string-join lst ",") "]"))

  (define (write-list lst)
    (display (list->string lst) port))

  (match drv
    (($ <derivation> outputs inputs sources
        system builder args env-vars)
     (display "Derive(" port)
     (write-list (map (match-lambda
                       ((name . ($ <derivation-output> path hash-algo hash))
                        (format #f "(~s,~s,~s,~s)"
                                name path (or hash-algo "")
                                (or hash ""))))
                      outputs))
     (display "," port)
     (write-list (map (match-lambda
                       (($ <derivation-input> path sub-drvs)
                        (format #f "(~s,~a)" path
                                (list->string (map object->string sub-drvs)))))
                      inputs))
     (display "," port)
     (write-list sources)
     (format port ",~s,~s," system builder)
     (write-list (map object->string args))
     (display "," port)
     (write-list (map (match-lambda
                       ((name . value)
                        (format #f "(~s,~s)" name value)))
                      env-vars))
     (display ")" port))))


(define (derivation-hash drv)      ; `hashDerivationModulo' in derivations.cc
  "Return the hash of DRV, modulo its fixed-output inputs, as a bytevector."
  (match drv
    (($ <derivation> ((_ . ($ <derivation-output> path
                              (? symbol? hash-algo) (? string? hash)))))
     ;; A fixed-output derivation.
     (sha256
      (string->utf8
       (string-append "fixed:out:" hash-algo ":" hash ":" path))))
    (($ <derivation> outputs inputs sources
        system builder args env-vars)
     ;; A regular derivation: replace that path of each input with that
     ;; inputs hash; return the hash of serialization of the resulting
     ;; derivation.
     (let* ((inputs (map (match-lambda
                          (($ <derivation-input> path sub-drvs)
                           (let ((hash (call-with-input-file path
                                         (compose derivation-hash
                                                  read-derivation))))
                             (make-derivation-input hash sub-drvs))))
                         inputs))
            (drv     (make-derivation outputs inputs sources
                                      system builder args env-vars)))
       (sha256
        (string->utf8 (call-with-output-string
                       (cut write-derivation drv <>))))))))

(define (instantiate server derivation)
  #f
  )
