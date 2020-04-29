;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Martin Becze <mjbecze@riseup.net>
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
  #:use-module (guix json)
  #:use-module (guix import json)
  #:use-module (guix import utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:export (crate->guix-package
            guix-package->crate-name
            string->license
            crate-recursive-import
            %crate-updater))


;;;
;;; Interface to https://crates.io/api/v1.
;;;

;; Crates.  A crate is essentially a "package".  It can have several
;; "versions", each of which has its own set of dependencies, license,
;; etc.--see <crate-version> below.
(define-json-mapping <crate> make-crate crate?
  json->crate
  (name          crate-name)                      ;string
  (latest-version crate-latest-version "max_version") ;string
  (home-page     crate-home-page "homepage")      ;string | #nil
  (repository    crate-repository)                ;string
  (description   crate-description)               ;string
  (keywords      crate-keywords                   ;list of strings
                 "keywords" vector->list)
  (categories    crate-categories                 ;list of strings
                 "categories" vector->list)
  (versions      crate-versions "actual_versions" ;list of <crate-version>
                 (lambda (vector)
                   (map json->crate-version
                        (vector->list vector))))
  (links         crate-links))                    ;alist

;; Crate version.
(define-json-mapping <crate-version> make-crate-version crate-version?
  json->crate-version
  (id            crate-version-id)                ;integer
  (number        crate-version-number "num")      ;string
  (download-path crate-version-download-path "dl_path") ;string
  (readme-path   crate-version-readme-path "readme_path") ;string
  (license       crate-version-license "license") ;string
  (links         crate-version-links))            ;alist

;; Crate dependency.  Each dependency (each edge in the graph) is annotated as
;; being a "normal" dependency or a development dependency.  There also
;; information about the minimum required version, such as "^0.0.41".
(define-json-mapping <crate-dependency> make-crate-dependency
  crate-dependency?
  json->crate-dependency
  (id            crate-dependency-id "crate_id")  ;string
  (kind          crate-dependency-kind "kind"     ;'normal | 'dev
                 string->symbol)
  (requirement   crate-dependency-requirement "req")) ;string

(define (lookup-crate name)
  "Look up NAME on https://crates.io and return the corresopnding <crate>
record or #f if it was not found."
  (let ((json (json-fetch (string-append (%crate-base-url) "/api/v1/crates/"
                                         name))))
    (and=> (and json (assoc-ref json "crate"))
           (lambda (alist)
             ;; The "versions" field of ALIST is simply a list of version IDs
             ;; (integers).  Here, we squeeze in the actual version
             ;; dictionaries that are not part of ALIST but are just more
             ;; convenient handled this way.
             (let ((versions (or (assoc-ref json "versions") '#())))
               (json->crate `(,@alist
                              ("actual_versions" . ,versions))))))))

(define (crate-version-dependencies version)
  "Return the list of <crate-dependency> records of VERSION, a
<crate-version>."
  (let* ((path (assoc-ref (crate-version-links version) "dependencies"))
         (url  (string-append (%crate-base-url) path)))
    (match (assoc-ref (or (json-fetch url) '()) "dependencies")
      ((? vector? vector)
       (delete-duplicates (map json->crate-dependency (vector->list vector))))
      (_
       '()))))


;;;
;;; Converting crates to Guix packages.
;;;

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

(define (string->license string)
  (filter-map (lambda (license)
                (and (not (string-null? license))
                     (not (any (lambda (elem) (string=? elem license))
                               '("AND" "OR" "WITH")))
                     (or (spdx-string->license license)
                         'unknown-license!)))
              (string-split string (string->char-set " /"))))

(define* (crate->guix-package crate-name #:optional version)
  "Fetch the metadata for CRATE-NAME from crates.io, and return the
`package' s-expression corresponding to that package, or #f on failure.
When VERSION is specified, attempt to fetch that version; otherwise fetch the
latest version of CRATE-NAME."

  (define (normal-dependency? dependency)
    (eq? (crate-dependency-kind dependency) 'normal))

  (define crate
    (lookup-crate crate-name))

  (define version-number
    (and crate
         (or version
             (crate-latest-version crate))))

  (define version*
    (and crate
         (find (lambda (version)
                 (string=? (crate-version-number version)
                           version-number))
               (crate-versions crate))))

  (and crate version*
       (let* ((dependencies   (crate-version-dependencies version*))
              (dep-crates     (filter normal-dependency? dependencies))
              (dev-dep-crates (remove normal-dependency? dependencies))
              (cargo-inputs   (sort (map crate-dependency-id dep-crates)
                                    string-ci<?))
              (cargo-development-inputs
               (sort (map crate-dependency-id dev-dep-crates)
                     string-ci<?)))
         (values
          (make-crate-sexp #:name crate-name
                           #:version (crate-version-number version*)
                           #:cargo-inputs cargo-inputs
                           #:cargo-development-inputs cargo-development-inputs
                           #:home-page (or (crate-home-page crate)
                                           (crate-repository crate))
                           #:synopsis (crate-description crate)
                           #:description (crate-description crate)
                           #:license (and=> (crate-version-license version*)
                                            string->license))
          (append cargo-inputs cargo-development-inputs)))))

(define* (crate-recursive-import crate-name #:optional version)
  (recursive-import crate-name #f
                    #:repo->guix-package
                    (lambda (name repo)
                      (let ((version (and (string=? name crate-name)
                                          version)))
                        (crate->guix-package name version)))
                    #:guix-name crate-name->package-name))

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
         (crate      (lookup-crate crate-name))
         (version    (crate-latest-version crate))
         (url        (crate-uri crate-name version)))
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

