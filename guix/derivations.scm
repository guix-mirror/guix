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
  #:use-module (guix utils)
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
            write-derivation
            derivation-path->output-path
            derivation

            %guile-for-build
            build-expression->derivation))

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
  (hash       derivation-output-hash))            ; bytevector | #f

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
                     (let ((algo (string->symbol hash-algo))
                           (hash (base16-string->bytevector hash)))
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
                                name path
                                (or (and=> hash-algo symbol->string) "")
                                (or (and=> hash bytevector->base16-string)
                                    ""))))
                      outputs))
     (display "," port)
     (write-list (map (match-lambda
                       (($ <derivation-input> path sub-drvs)
                        (format #f "(~s,~a)" path
                                (list->string (map object->string sub-drvs)))))
                      inputs))
     (display "," port)
     (write-list (map object->string sources))
     (format port ",~s,~s," system builder)
     (write-list (map object->string args))
     (display "," port)
     (write-list (map (match-lambda
                       ((name . value)
                        (format #f "(~s,~s)" name value)))
                      env-vars))
     (display ")" port))))

(define* (derivation-path->output-path path #:optional (output "out"))
  "Read the derivation from PATH (`/nix/store/xxx.drv'), and return the store
path of its output OUTPUT."
  (let* ((drv     (call-with-input-file path read-derivation))
         (outputs (derivation-outputs drv)))
    (and=> (assoc-ref outputs output) derivation-output-path)))


;;;
;;; Derivation primitive.
;;;

(define (compressed-hash bv size)                 ; `compressHash'
  "Given the hash stored in BV, return a compressed version thereof that fits
in SIZE bytes."
  (define new (make-bytevector size 0))
  (define old-size (bytevector-length bv))
  (let loop ((i 0))
    (if (= i old-size)
        new
        (let* ((j (modulo i size))
               (o (bytevector-u8-ref new j)))
          (bytevector-u8-set! new j
                              (logxor o (bytevector-u8-ref bv i)))
          (loop (+ 1 i))))))

(define derivation-hash            ; `hashDerivationModulo' in derivations.cc
  (memoize
   (lambda (drv)
    "Return the hash of DRV, modulo its fixed-output inputs, as a bytevector."
    (match drv
      (($ <derivation> ((_ . ($ <derivation-output> path
                                (? symbol? hash-algo) (? bytevector? hash)))))
       ;; A fixed-output derivation.
       (sha256
        (string->utf8
         (string-append "fixed:out:" (symbol->string hash-algo)
                        ":" (bytevector->base16-string hash)
                        ":" path))))
      (($ <derivation> outputs inputs sources
          system builder args env-vars)
       ;; A regular derivation: replace the path of each input with that
       ;; input's hash; return the hash of serialization of the resulting
       ;; derivation.  Note: inputs are sorted as in the order of their hex
       ;; hash representation because that's what the C++ `std::map' code
       ;; does.
       (let* ((inputs (sort (map (match-lambda
                                  (($ <derivation-input> path sub-drvs)
                                   (let ((hash (call-with-input-file path
                                                 (compose bytevector->base16-string
                                                          derivation-hash
                                                          read-derivation))))
                                     (make-derivation-input hash sub-drvs))))
                                 inputs)
                            (lambda (i1 i2)
                              (string<? (derivation-input-path i1)
                                        (derivation-input-path i2)))))
              (drv    (make-derivation outputs inputs sources
                                       system builder args env-vars)))
         (sha256
          (string->utf8 (call-with-output-string
                         (cut write-derivation drv <>))))))))))

(define (store-path type hash name)               ; makeStorePath
  "Return the store path for NAME/HASH/TYPE."
  (let* ((s (string-append type ":sha256:"
                           (bytevector->base16-string hash) ":"
                           (%store-prefix) ":" name))
         (h (sha256 (string->utf8 s)))
         (c (compressed-hash h 20)))
    (string-append (%store-prefix) "/"
                   (bytevector->nix-base32-string c) "-"
                   name)))

(define (output-path output hash name)            ; makeOutputPath
  "Return an output path for OUTPUT (the name of the output as a string) of
the derivation called NAME with hash HASH."
  (store-path (string-append "output:" output) hash
              (if (string=? output "out")
                  name
                  (string-append name "-" output))))

(define* (derivation store name system builder args env-vars inputs
                     #:key (outputs '("out")) hash hash-algo hash-mode)
  "Build a derivation with the given arguments.  Return the resulting
store path and <derivation> object.  When HASH, HASH-ALGO, and HASH-MODE
are given, a fixed-output derivation is created---i.e., one whose result is
known in advance, such as a file download."
  (define (add-output-paths drv)
    ;; Return DRV with an actual store path for each of its output and the
    ;; corresponding environment variable.
    (match drv
      (($ <derivation> outputs inputs sources
          system builder args env-vars)
       (let* ((drv-hash (derivation-hash drv))
              (outputs  (map (match-lambda
                              ((output-name . ($ <derivation-output>
                                                 _ algo hash))
                               (let ((path (output-path output-name
                                                        drv-hash name)))
                                 (cons output-name
                                       (make-derivation-output path algo
                                                               hash)))))
                             outputs)))
         (make-derivation outputs inputs sources system builder args
                          (map (match-lambda
                                ((name . value)
                                 (cons name
                                       (or (and=> (assoc-ref outputs name)
                                                  derivation-output-path)
                                           value))))
                               env-vars))))))

  (define (env-vars-with-empty-outputs)
    ;; Return a variant of ENV-VARS where each OUTPUTS is associated with an
    ;; empty string, even outputs that do not appear in ENV-VARS.
    (let ((e (map (match-lambda
                   ((name . val)
                    (if (member name outputs)
                        (cons name "")
                        (cons name val))))
                  env-vars)))
      (fold-right (lambda (output-name env-vars)
                    (if (assoc output-name env-vars)
                        env-vars
                        (append env-vars `((,output-name . "")))))
                  e
                  outputs)))

  (let* ((outputs    (map (lambda (name)
                            ;; Return outputs with an empty path.
                            (cons name
                                  (make-derivation-output "" hash-algo hash)))
                          outputs))
         (inputs     (map (match-lambda
                           (((? store-path? input))
                            (make-derivation-input input '("out")))
                           (((? store-path? input) sub-drvs ...)
                            (make-derivation-input input sub-drvs))
                           ((input . _)
                            (let ((path (add-to-store store
                                                      (basename input)
                                                      (hash-algo sha256) #t #t
                                                      input)))
                              (make-derivation-input path '()))))
                          inputs))
         (env-vars   (env-vars-with-empty-outputs))
         (drv-masked (make-derivation outputs
                                      (filter (compose derivation-path?
                                                       derivation-input-path)
                                              inputs)
                                      (filter-map (lambda (i)
                                                    (let ((p (derivation-input-path i)))
                                                      (and (not (derivation-path? p))
                                                           p)))
                                                  inputs)
                                      system builder args env-vars))
         (drv        (add-output-paths drv-masked)))

    (values (add-text-to-store store (string-append name ".drv")
                               (call-with-output-string
                                (cut write-derivation drv <>))
                               (map derivation-input-path
                                    inputs))
            drv)))


;;;
;;; Guile-based builders.
;;;

(define %guile-for-build
  ;; The derivation of the Guile to be used within the build environment,
  ;; when using `build-expression->derivation'.
  (make-parameter (false-if-exception (nixpkgs-derivation "guile"))))

(define* (build-expression->derivation store name system exp inputs
                                       #:key hash hash-algo)
  "Return a derivation that executes Scheme expression EXP as a builder for
derivation NAME.  INPUTS must be a list of string/derivation-path pairs.  EXP
is evaluated in an environment where %OUTPUT is bound to the output path, and
where %BUILD-INPUTS is bound to an alist of string/output-path pairs made
from INPUTS."
  (define guile
    (string-append (derivation-path->output-path (%guile-for-build))
                   "/bin/guile"))

  (let* ((prologue `(begin
                      (define %output (getenv "out"))
                      (define %build-inputs
                        ',(map (match-lambda
                                ((name . drv)
                                 (cons name
                                       (derivation-path->output-path drv))))
                               inputs))) )
         (builder  (add-text-to-store store
                                      (string-append name "-guile-builder")
                                      (string-append (object->string prologue)
                                                     (object->string exp))
                                      (map cdr inputs))))
    (derivation store name system guile `("--no-auto-compile" ,builder)
                '(("HOME" . "/homeless"))
                `((,(%guile-for-build))
                  (,builder)))))
