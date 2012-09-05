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

(define-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix store)
  #:use-module (guix build-system)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (origin
            origin?
            origin-uri
            origin-method
            origin-sha256
            origin-file-name
            base32

            package
            package?
            package-name
            package-version
            package-full-name
            package-source
            package-build-system
            package-arguments
            package-inputs
            package-native-inputs
            package-propagated-inputs
            package-outputs
            package-search-paths
            package-description
            package-long-description
            package-license
            package-platforms
            package-maintainers
            package-properties
            package-location

            package-transitive-inputs
            package-transitive-propagated-inputs
            package-source-derivation
            package-derivation
            package-cross-derivation

            &package-error
            package-error-package
            &package-input-error
            package-error-invalid-input))

;;; Commentary:
;;;
;;; This module provides a high-level mechanism to define packages in a
;;; Guix-based distribution.
;;;
;;; Code:

;; The source of a package, such as a tarball URL and fetcher---called
;; "origin" to avoid name clash with `package-source', `source', etc.
(define-record-type* <origin>
  origin make-origin
  origin?
  (uri       origin-uri)                          ; string
  (method    origin-method)                       ; symbol
  (sha256    origin-sha256)                       ; bytevector
  (file-name origin-file-name (default #f)))      ; optional file name

(define-syntax base32
  (lambda (s)
    "Return the bytevector corresponding to the given Nix-base32
representation."
    (syntax-case s ()
      ((_ str)
       (string? (syntax->datum #'str))
       (with-syntax ((bv (nix-base32-string->bytevector
                          (syntax->datum #'str))))
         #''bv)))))

;; A package.

(define-record-type* <package>
  package make-package
  package?
  (name   package-name)                   ; string
  (version package-version)               ; string
  (source package-source)                 ; <origin> instance
  (build-system package-build-system)     ; build system
  (arguments package-arguments            ; arguments for the build method
             (default '()))

  (inputs package-inputs                  ; input packages or derivations
          (default '()))
  (propagated-inputs package-propagated-inputs    ; same, but propagated
                     (default '()))
  (native-inputs package-native-inputs    ; native input packages/derivations
                 (default '()))
  (self-native-input? package-self-native-input?  ; whether to use itself as
                                                  ; a native input when cross-
                      (default #f))               ; compiling

  (outputs package-outputs                ; list of strings
           (default '("out")))
  (search-paths package-search-paths      ; list of (ENV-VAR (DIRS ...))
                (default '()))            ; tuples; see
                                          ; `set-path-environment-variable'
                                          ; (aka. "setup-hook")

  (description package-description)       ; one-line description
  (long-description package-long-description)     ; one or two paragraphs
  (license package-license (default '()))
  (home-page package-home-page)
  (platforms package-platforms (default '()))
  (maintainers package-maintainers (default '()))

  (properties package-properties (default '()))   ; alist for anything else

  (location package-location
            (default (and=> (current-source-location)
                            source-properties->location))))

(set-record-type-printer! <package>
                          (lambda (package port)
                            (let ((loc    (package-location package))
                                  (format simple-format))
                              (format port "#<package ~a-~a ~a:~a ~a>"
                                      (package-name package)
                                      (package-version package)
                                      (location-file loc)
                                      (location-line loc)
                                      (number->string (object-address
                                                       package)
                                                      16)))))


;; Error conditions.

(define-condition-type &package-error &error
  package-error?
  (package package-error-package))

(define-condition-type &package-input-error &package-error
  package-input-error?
  (input package-error-invalid-input))


(define (package-full-name package)
  "Return the full name of PACKAGE--i.e., `NAME-VERSION'."
  (string-append (package-name package) "-" (package-version package)))

(define (package-source-derivation store source)
  "Return the derivation path for SOURCE, a package source."
  (match source
    (($ <origin> uri method sha256 name)
     (method store uri 'sha256 sha256 name))))

(define (transitive-inputs inputs)
  (let loop ((inputs  inputs)
             (result '()))
    (match inputs
      (()
       (delete-duplicates (reverse result)))      ; XXX: efficiency
      (((and i (name (? package? p) sub ...)) rest ...)
       (let ((t (map (match-lambda
                      ((dep-name derivation ...)
                       (cons (string-append name "/" dep-name)
                             derivation)))
                     (package-propagated-inputs p))))
         (loop (append t rest)
               (append t (cons i result)))))
      ((input rest ...)
       (loop rest (cons input result))))))

(define (package-transitive-inputs package)
  "Return the transitive inputs of PACKAGE---i.e., its direct inputs along
with their propagated inputs, recursively."
  (transitive-inputs (append (package-native-inputs package)
                             (package-inputs package)
                             (package-propagated-inputs package))))

(define (package-transitive-propagated-inputs package)
  "Return the propagated inputs of PACKAGE, and their propagated inputs,
recursively."
  (transitive-inputs (package-propagated-inputs package)))


;;;
;;; Package derivations.
;;;

(define %derivation-cache
  ;; Package to derivation-path mapping.
  (make-weak-key-hash-table))

(define (cache package system drv)
  "Memoize DRV as the derivation of PACKAGE on SYSTEM."
  (hash-set! %derivation-cache (cons package system) drv)
  drv)

(define (cached-derivation package system)
  "Return the cached derivation path of PACKAGE for SYSTEM, or #f."
  (hash-ref %derivation-cache (cons package system)))

(define* (package-derivation store package
                             #:optional (system (%current-system)))
  "Return the derivation of PACKAGE for SYSTEM."
  (or (cached-derivation package system)
      (match package
        (($ <package> name version source (= build-system-builder builder)
            args inputs propagated-inputs native-inputs self-native-input?
            outputs)
         ;; TODO: For `search-paths', add a builder prologue that calls
         ;; `set-path-environment-variable'.
         (let ((inputs (map (match-lambda
                             (((? string? name) (? package? package))
                              (list name (package-derivation store package)))
                             (((? string? name) (? package? package)
                               (? string? sub-drv))
                              (list name (package-derivation store package)
                                    sub-drv))
                             (((? string? name)
                               (and (? string?) (? derivation-path?) drv))
                              (list name drv))
                             (((? string? name)
                               (and (? string?) (? file-exists? file)))
                              ;; Add FILE to the store.  When FILE is in the
                              ;; sub-directory of a store path, it needs to be
                              ;; added anyway, so it can be used as a source.
                              (list name
                                    (add-to-store store (basename file)
                                                  #t #f "sha256" file)))
                             (x
                              (raise (condition (&package-input-error
                                                 (package package)
                                                 (input   x))))))
                            (package-transitive-inputs package))))

           ;; Compute the derivation and cache the result.  Caching is
           ;; important because some derivations, such as the implicit inputs
           ;; of the GNU build system, will be queried many, many times in a
           ;; row.
           (cache package system
                  (apply builder
                         store (package-full-name package)
                         (package-source-derivation store source)
                         inputs
                         #:outputs outputs #:system system
                         (if (procedure? args)
                             (args system)
                             args))))))))

(define* (package-cross-derivation store package)
  ;; TODO
  #f)
