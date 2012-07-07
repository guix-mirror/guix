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
  #:use-module (srfi srfi-9)
  #:export (location
            location?
            location-file
            location-line
            location-column

            origin
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
            package-source-derivation
            package-derivation
            package-cross-derivation))

;;; Commentary:
;;;
;;; This module provides a high-level mechanism to define packages in a
;;; Guix-based distribution.
;;;
;;; Code:

;; A source location.
(define-record-type <location>
  (make-location file line column)
  location?
  (file          location-file)                   ; file name
  (line          location-line)                   ; 1-indexed line
  (column        location-column))                ; 0-indexed column

(define location
  (memoize
   (lambda (file line column)
     "Return the <location> object for the given FILE, LINE, and COLUMN."
     (and line column file
          (make-location file line column)))))

(define (source-properties->location loc)
  "Return a location object based on the info in LOC, an alist as returned
by Guile's `source-properties', `frame-source', `current-source-location',
etc."
  (let ((file (assq-ref loc 'filename))
        (line (assq-ref loc 'line))
        (col  (assq-ref loc 'column)))
    (location file (and line (+ line 1)) col)))


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

(define (package-source-derivation store source)
  "Return the derivation path for SOURCE, a package source."
  (match source
    (($ <origin> uri method sha256 name)
     (method store uri 'sha256 sha256 name))))

(define (package-transitive-inputs package)
  "Return the transitive inputs of PACKAGE---i.e., its direct inputs along
with their propagated inputs, recursively."
  (let loop ((inputs (concatenate (list (package-native-inputs package)
                                        (package-inputs package)
                                        (package-propagated-inputs package))))
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

(define* (package-derivation store package
                             #:optional (system (%current-system)))
  "Return the derivation of PACKAGE for SYSTEM."
  (match package
    (($ <package> name version source (= build-system-builder builder)
        args inputs propagated-inputs native-inputs self-native-input?
        outputs)
     ;; TODO: For `search-paths', add a builder prologue that calls
     ;; `set-path-environment-variable'.
     (let ((inputs (map (match-lambda
                         (((? string? name) (and package ($ <package>)))
                          (list name (package-derivation store package)))
                         (((? string? name) (and package ($ <package>))
                           (? string? sub-drv))
                          (list name (package-derivation store package)
                                sub-drv))
                         (((? string? name)
                           (and (? string?) (? derivation-path?) drv))
                          (list name drv))
                         (((? string? name)
                           (and (? string?) (? (negate store-path?))
                                (? file-exists? file)))
                          (list name
                                (add-to-store store (basename file)
                                              #t #f "sha256" file))))
                        (package-transitive-inputs package))))
       (apply builder
              store (string-append name "-" version)
              (package-source-derivation store source)
              inputs
              #:outputs outputs #:system system
              (if (procedure? args)
                  (args system)
                  args))))))

(define* (package-cross-derivation store package)
  ;; TODO
  #f)
