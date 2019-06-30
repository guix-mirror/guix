;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
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

(define-module (guix import crate)
  #:use-module (guix base32)
  #:use-module (guix build-system cargo)
  #:use-module ((guix download) #:prefix download:)
  #:use-module (gcrypt hash)
  #:use-module (guix http-client)
  #:use-module (guix import json)
  #:use-module (guix import utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print) ; recursive
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:export (crate->guix-package
            guix-package->crate-name
            %crate-updater))

(define (crate-fetch crate-name callback)
  "Fetch the metadata for CRATE-NAME from crates.io and call the callback."

  (define (crates->inputs crates)
    (sort (map (cut assoc-ref <> "crate_id") crates) string-ci<?))

  (define (string->license string)
    (map spdx-string->license (string-split string #\/)))

  (define (crate-kind-predicate kind)
    (lambda (dep) (string=? (assoc-ref dep "kind") kind)))

  (and-let* ((crate-json (json-fetch-alist (string-append crate-url crate-name)))
             (crate (assoc-ref crate-json "crate"))
             (name (assoc-ref crate "name"))
             (version (assoc-ref crate "max_version"))
             (homepage (assoc-ref crate "homepage"))
             (repository (assoc-ref crate "repository"))
             (synopsis (assoc-ref crate "description"))
             (description (assoc-ref crate "description"))
             (license (or (and=> (assoc-ref crate "license")
                                 string->license)
                          '()))                   ;missing license info
             (path (string-append "/" version "/dependencies"))
             (deps-json (json-fetch-alist (string-append crate-url name path)))
             (deps (assoc-ref deps-json "dependencies"))
             (dep-crates (filter (crate-kind-predicate "normal") deps))
             (dev-dep-crates
              (filter (lambda (dep)
                        (not ((crate-kind-predicate "normal") dep))) deps))
             (cargo-inputs (crates->inputs dep-crates))
             (cargo-development-inputs (crates->inputs dev-dep-crates))
             (home-page (match homepage
                          (() repository)
                          (_ homepage))))
    (callback #:name name #:version version
              #:cargo-inputs cargo-inputs
              #:cargo-development-inputs cargo-development-inputs
              #:home-page home-page #:synopsis synopsis
              #:description description #:license license)))

(define (maybe-cargo-inputs package-names)
  (match (package-names->package-inputs package-names)
    (()
     '())
    ((package-inputs ...)
     `(#:cargo-inputs ,package-inputs))))

(define (maybe-cargo-development-inputs package-names)
  (match (package-names->package-inputs package-names)
    (()
     '())
    ((package-inputs ...)
     `(#:cargo-development-inputs ,package-inputs))))

(define (maybe-arguments arguments)
  (match arguments
    (()
     '())
    ((args ...)
     `((arguments (,'quasiquote ,args))))))

(define* (make-crate-sexp #:key name version cargo-inputs cargo-development-inputs
                          home-page synopsis description license
                          #:allow-other-keys)
  "Return the `package' s-expression for a rust package with the given NAME,
VERSION, CARGO-INPUTS, CARGO-DEVELOPMENT-INPUTS, HOME-PAGE, SYNOPSIS, DESCRIPTION,
and LICENSE."
  (let* ((port (http-fetch (crate-uri name version)))
         (guix-name (crate-name->package-name name))
         (cargo-inputs (map crate-name->package-name cargo-inputs))
         (cargo-development-inputs (map crate-name->package-name
                                        cargo-development-inputs))
         (pkg `(package
                   (name ,guix-name)
                   (version ,version)
                   (source (origin
                             (method url-fetch)
                             (uri (crate-uri ,name version))
                             (file-name (string-append name "-" version ".tar.gz"))
                             (sha256
                              (base32
                               ,(bytevector->nix-base32-string (port-sha256 port))))))
                   (build-system cargo-build-system)
                   ,@(maybe-arguments (append (maybe-cargo-inputs cargo-inputs)
                                              (maybe-cargo-development-inputs
                                                cargo-development-inputs)))
                   (home-page ,(match home-page
                                 (() "")
                                 (_ home-page)))
                   (synopsis ,synopsis)
                   (description ,(beautify-description description))
                   (license ,(match license
                               (() #f)
                               ((license) license)
                               (_ `(list ,@license)))))))
         (close-port port)
         pkg))

(define (crate->guix-package crate-name)
  "Fetch the metadata for CRATE-NAME from crates.io, and return the
`package' s-expression corresponding to that package, or #f on failure."
  (crate-fetch crate-name make-crate-sexp))

(define (guix-package->crate-name package)
  "Return the crate name of PACKAGE."
  (and-let* ((origin (package-source package))
             (uri (origin-uri origin))
             (crate-url? uri)
             (len (string-length crate-url))
             (path (xsubstring uri len))
             (parts (string-split path #\/)))
    (match parts
      ((name _ ...) name))))

(define (crate-name->package-name name)
  (string-append "rust-" (string-join (string-split name #\_) "-")))

;;;
;;; Updater
;;;

(define (crate-package? package)
  "Return true if PACKAGE is a Rust crate from crates.io."
  (let ((source-url (and=> (package-source package) origin-uri))
        (fetch-method (and=> (package-source package) origin-method)))
    (and (eq? fetch-method download:url-fetch)
         (match source-url
           ((? string?)
            (crate-url? source-url))
           ((source-url ...)
            (any crate-url? source-url))))))

(define (latest-release package)
  "Return an <upstream-source> for the latest release of PACKAGE."
  (let* ((crate-name (guix-package->crate-name package))
         (callback (lambda* (#:key version #:allow-other-keys) version))
         (version (crate-fetch crate-name callback))
         (url (crate-uri crate-name version)))
    (upstream-source
     (package (package-name package))
     (version version)
     (urls (list url)))))

(define %crate-updater
  (upstream-updater
   (name 'crates)
   (description "Updater for crates.io packages")
   (pred crate-package?)
   (latest latest-release)))

