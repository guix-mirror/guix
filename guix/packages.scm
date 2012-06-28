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
  #:use-module (srfi srfi-9)
  #:export (location
            location?
            location-file
            location-line
            location-column

            source
            package-source?
            package-source-uri
            package-source-method
            package-source-sha256
            package-source-file-name

            package
            package?
            package-name
            package-version
            package-source
            package-build-system
            package-arguments
            package-inputs
            package-native-inputs
            package-outputs
            package-search-paths
            package-description
            package-long-description
            package-license
            package-platforms
            package-maintainers
            package-location

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


;; The source of a package, such as a tarball URL and fetcher.
(define-record-type* <package-source>
  source make-package-source
  package-source?
  (uri       package-source-uri)                     ; string
  (method    package-source-method)                  ; symbol
  (sha256    package-source-sha256)                  ; bytevector
  (file-name package-source-file-name                ; optional file name
             (default #f)))

;; A package.
(define-record-type* <package>
  package make-package
  package?
  (name   package-name)                   ; string
  (version package-version)               ; string
  (source package-source)                 ; <package-source> instance
  (build-system package-build-system)     ; build system
  (arguments package-arguments            ; arguments for the build method
             (default '()))
  (inputs package-inputs                  ; input packages or derivations
          (default '()))
  (native-inputs package-native-inputs    ; native input packages/derivations
                 (default '()))
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

  (location package-location
            (default (and=> (current-source-location)
                            source-properties->location))))

(define (package-source-derivation store source)
  "Return the derivation path for SOURCE, a package source."
  (match source
    (($ <package-source> uri method sha256 name)
     (method store uri 'sha256 sha256 name))))

(define* (package-derivation store package
                             #:optional (system (%current-system)))
  "Return the derivation of PACKAGE for SYSTEM."
  (match package
    (($ <package> name version source (= build-system-builder builder)
        args inputs native-inputs outputs)
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
                          (list name drv)))
                        (append native-inputs inputs))))
       (apply builder
              store (string-append name "-" version)
              (package-source-derivation store source)
              inputs
              #:outputs outputs #:system system
              args)))))

(define* (package-cross-derivation store package)
  ;; TODO
  #f)
