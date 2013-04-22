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

(define-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix store)
  #:use-module (guix base32)
  #:use-module (guix derivations)
  #:use-module (guix build-system)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module ((ice-9 rdelim) #:select (read-line))
  #:use-module (ice-9 regex)
  #:re-export (%current-system)
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
            package-synopsis
            package-description
            package-license
            package-home-page
            package-platforms
            package-maintainers
            package-properties
            package-location
            package-field-location

            package-transitive-inputs
            package-transitive-propagated-inputs
            package-source-derivation
            package-derivation
            package-cross-derivation
            package-output

            &package-error
            package-error?
            package-error-package
            &package-input-error
            package-input-error?
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
       ;; A literal string: do the conversion at expansion time.
       (with-syntax ((bv (nix-base32-string->bytevector
                          (syntax->datum #'str))))
         #''bv))
      ((_ str)
       #'(nix-base32-string->bytevector str)))))

;; A package.

(define-record-type* <package>
  package make-package
  package?
  (name   package-name)                   ; string
  (version package-version)               ; string
  (source package-source)                 ; <origin> instance
  (build-system package-build-system)     ; build system
  (arguments package-arguments            ; arguments for the build method
             (default '()) (thunked))

  (inputs package-inputs                  ; input packages or derivations
          (default '()) (thunked))
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

  (synopsis package-synopsis)                    ; one-line description
  (description package-description)              ; one or two paragraphs
  (license package-license)
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

(define (package-field-location package field)
  "Return an estimate of the source code location of the definition of FIELD
for PACKAGE."
  (define field-rx
    (make-regexp (string-append "\\("
                                (regexp-quote (symbol->string field))
                                "[[:blank:]]*")))
  (define (seek-to-line port line)
    (let ((line (- line 1)))
      (let loop ()
        (when (< (port-line port) line)
          (unless (eof-object? (read-line port))
            (loop))))))

  (define (find-line port)
    (let loop ((line (read-line port)))
      (cond ((eof-object? line)
             (values #f #f))
            ((regexp-exec field-rx line)
             =>
             (lambda (match)
               ;; At this point `port-line' points to the next line, so need
               ;; need to add one.
               (values (port-line port)
                       (match:end match))))
            (else
             (loop (read-line port))))))

  (match (package-location package)
    (($ <location> file line column)
     (catch 'system
       (lambda ()
         (call-with-input-file (search-path %load-path file)
           (lambda (port)
             (seek-to-line port line)
             (let-values (((line column)
                           (find-line port)))
               (if (and line column)
                   (location file line column)
                   (package-location package))))))
       (lambda _
         (package-location package))))
    (_ #f)))


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

(define* (package-source-derivation store source
                                    #:optional (system (%current-system)))
  "Return the derivation path for SOURCE, a package source, for SYSTEM."
  (match source
    (($ <origin> uri method sha256 name)
     (method store uri 'sha256 sha256 name
             #:system system))))

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
  (make-weak-key-hash-table 100))

(define (cache package system thunk)
  "Memoize the return values of THUNK as the derivation of PACKAGE on
SYSTEM."
  (let ((vals (call-with-values thunk list)))
    ;; Use `hashq-set!' instead of `hash-set!' because `hash' returns the
    ;; same value for all structs (as of Guile 2.0.6), and because pointer
    ;; equality is sufficient in practice.
    (hashq-set! %derivation-cache package `((,system ,@vals)))
    (apply values vals)))

(define-syntax-rule (cached package system body ...)
  "Memoize the result of BODY for the arguments PACKAGE and SYSTEM.
Return the cached result when available."
  (let ((thunk (lambda () body ...)))
    (match (hashq-ref %derivation-cache package)
      ((alist (... ...))
       (match (assoc-ref alist system)
         ((vals (... ...))
          (apply values vals))
         (#f
          (cache package system thunk))))
      (#f
       (cache package system thunk)))))

(define* (package-derivation store package
                             #:optional (system (%current-system)))
  "Return the derivation path and corresponding <derivation> object of
PACKAGE for SYSTEM."
  (define (intern file)
    ;; Add FILE to the store.  Set the `recursive?' bit to #t, so that
    ;; file permissions are preserved.
    (add-to-store store (basename file) #t "sha256" file))

  (define expand-input
    ;; Expand the given input tuple such that it contains only
    ;; references to derivation paths or store paths.
    (match-lambda
     (((? string? name) (? package? package))
      (list name (package-derivation store package system)))
     (((? string? name) (? package? package)
       (? string? sub-drv))
      (list name (package-derivation store package system)
            sub-drv))
     (((? string? name)
       (and (? string?) (? derivation-path?) drv))
      (list name drv))
     (((? string? name)
       (and (? string?) (? file-exists? file)))
      ;; Add FILE to the store.  When FILE is in the sub-directory of a
      ;; store path, it needs to be added anyway, so it can be used as a
      ;; source.
      (list name (intern file)))
     (((? string? name) (? origin? source))
      (list name (package-source-derivation store source system)))
     (x
      (raise (condition (&package-input-error
                         (package package)
                         (input   x)))))))

  ;; Compute the derivation and cache the result.  Caching is important
  ;; because some derivations, such as the implicit inputs of the GNU build
  ;; system, will be queried many, many times in a row.
  (cached package system

          ;; Bind %CURRENT-SYSTEM so that thunked field values can refer
          ;; to it.
          (parameterize ((%current-system system))
            (match package
              (($ <package> name version source (= build-system-builder builder)
                  args inputs propagated-inputs native-inputs self-native-input?
                  outputs)
               ;; TODO: For `search-paths', add a builder prologue that calls
               ;; `set-path-environment-variable'.
               (let ((inputs (map expand-input
                                  (package-transitive-inputs package))))

                 (apply builder
                        store (package-full-name package)
                        (and source
                             (package-source-derivation store source system))
                        inputs
                        #:outputs outputs #:system system
                        (args))))))))

(define* (package-cross-derivation store package)
  ;; TODO
  #f)

(define* (package-output store package output
                         #:optional (system (%current-system)))
  "Return the output path of PACKAGE's OUTPUT for SYSTEM---where OUTPUT is the
symbolic output name, such as \"out\".  Note that this procedure calls
`package-derivation', which is costly."
  (let-values (((_ drv)
                (package-derivation store package system)))
    (derivation-output-path
     (assoc-ref (derivation-outputs drv) output))))
