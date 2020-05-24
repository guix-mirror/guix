;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2020, 2021 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (guix import hexpm)
  #:use-module (guix base32)
  #:use-module ((guix download) #:prefix download:)
  #:use-module (guix hexpm-download)
  #:use-module (gcrypt hash)
  #:use-module (guix http-client)
  #:use-module (json)
  #:use-module (guix import utils)
  #:use-module ((guix import json) #:select (json-fetch))
  #:use-module ((guix build utils)
                #:select ((package-name->name+version
                           . hyphen-package-name->name+version)
                          dump-port))
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:export (hexpm->guix-package
            guix-package->hexpm-name
            strings->licenses
            hexpm-recursive-import
            %hexpm-updater))


;;;
;;; Interface to https://hex.pm/api, version 2.
;;; https://github.com/hexpm/specifications/blob/master/apiary.apib
;;; https://github.com/hexpm/specifications/blob/master/endpoints.md
;;;

(define %hexpm-api-url
  (make-parameter "https://hex.pm/api"))

(define (package-url name)
  (string-append (%hexpm-api-url) "/packages/" name))

;; Hexpm Package. /api/packages/${name}
;; It can have several "releases", each of which has its own set of
;; requirements, buildtool, etc. - see <hexpm-release> below.
(define-json-mapping <hexpm-pkgdef> make-hexpm-pkgdef hexpm-pkgdef?
  json->hexpm
  (name          hexpm-name)                      ;string
  (html-url      hexpm-html-url      "html_url")      ;string
  (docs-html-url hexpm-docs-html-url "docs_html_url") ;string | #nil
  (meta          hexpm-meta "meta" json->hexpm-meta)
  (versions      hexpm-versions "releases" ;list of <hexpm-version>
                 (lambda (vector)
                   (map json->hexpm-version
                        (vector->list vector)))))

;; Hexpm meta.
(define-json-mapping <hexpm-meta> make-hexpm-meta hexpm-meta?
  json->hexpm-meta
  (description hexpm-meta-description)        ;string
  (licenses    hexpm-meta-licenses "licenses" ;list of strings
               (lambda (vector)
                 (or (and vector (vector->list vector))
                     #f))))

;; Hexpm version.
(define-json-mapping <hexpm-version> make-hexpm-version hexpm-version?
  json->hexpm-version
  (number  hexpm-version-number "version")   ;string
  (url     hexpm-version-url))               ;string


(define (lookup-hexpm name)
  "Look up NAME on https://hex.pm and return the corresopnding <hexpm>
record or #f if it was not found."
  (let ((json (json-fetch (package-url name))))
    (and json
         (json->hexpm json))))

;; Hexpm release. /api/packages/${name}/releases/${version}
(define-json-mapping <hexpm-release> make-hexpm-release hexpm-release?
  json->hexpm-release
  (number  hexpm-release-number "version")   ;string
  (url     hexpm-release-url)               ;string
  (requirements hexpm-requirements "requirements")) ;list of <hexpm-dependency>
;; meta:build_tools -> alist

;; Hexpm dependency.  Each dependency (each edge in the graph) is annotated as
;; being a "normal" dependency or a development dependency.  There also
;; information about the minimum required version, such as "^0.0.41".
(define-json-mapping <hexpm-dependency> make-hexpm-dependency
  hexpm-dependency?
  json->hexpm-dependency
  (app           hexpm-dependency-app "app")  ;string
  (optional      hexpm-dependency-optional)  ;bool
  (requirement   hexpm-dependency-requirement)) ;string

(define (hexpm-release-dependencies release)
  "Return the list of dependency names of RELEASE, a <hexpm-release>."
  (let ((reqs (or (hexpm-requirements release) '#())))
    (map first reqs)))  ;; TODO: also return required version


(define (lookup-hexpm-release version*)
  "Look up RELEASE on hexpm-version-url and return the corresopnding
<hexpm-release> record or #f if it was not found."
  (let* ((url (hexpm-version-url version*))
         (json (json-fetch url)))
    (json->hexpm-release json)))


;;;
;;; Converting hex.pm packages to Guix packages.
;;;

(define* (make-hexpm-sexp #:key name version tarball-url
                          home-page synopsis description license
                          #:allow-other-keys)
  "Return the `package' s-expression for a rust package with the given NAME,
VERSION, tarball-url, HOME-PAGE, SYNOPSIS, DESCRIPTION, and LICENSE."
  (call-with-temporary-directory
   (lambda (directory)
     (let ((port (http-fetch tarball-url))
           (tar (open-pipe* OPEN_WRITE "tar" "-C" directory
                            "-xf" "-" "contents.tar.gz")))
       (dump-port port tar)
       (close-port port)

       (let ((status (close-pipe tar)))
         (unless (zero? status)
           (error "tar extraction failure" status))))

     (let ((guix-name (hexpm-name->package-name name))
           (sha256 (bytevector->nix-base32-string
                    (call-with-input-file
                        (string-append directory "/contents.tar.gz")
                      port-sha256))))

       `(package
         (name ,guix-name)
         (version ,version)
         (source (origin
                   (method hexpm-fetch)
                   (uri (hexpm-uri ,name version))
                   (sha256 (base32 ,sha256))))
         (build-system ,'rebar3-build-system)
         (home-page ,(match home-page
                            (() "")
                            (_ home-page)))
         (synopsis ,synopsis)
         (description ,(beautify-description description))
         (license ,(match license
                          (() #f)
                          ((license) license)
                          (_ `(list ,@license)))))))))

(define (strings->licenses strings)
  (filter-map (lambda (license)
                (and (not (string-null? license))
                     (not (any (lambda (elem) (string=? elem license))
                               '("AND" "OR" "WITH")))
                     (or (spdx-string->license license)
                         license)))
              strings))

(define (hexpm-latest-version package)
  (let ((versions (map hexpm-version-number (hexpm-versions package))))
    (fold (lambda (a b)
            (if (version>? a b) a b)) (car versions) versions)))

(define* (hexpm->guix-package package-name #:key repo version)
  "Fetch the metadata for PACKAGE-NAME from hexpms.io, and return the
`package' s-expression corresponding to that package, or #f on failure.
When VERSION is specified, attempt to fetch that version; otherwise fetch the
latest version of PACKAGE-NAME."

  (define package
    (lookup-hexpm package-name))

  (define version-number
    (and package
         (or version
             (hexpm-latest-version package))))

  (define version*
    (and package
         (find (lambda (version)
                 (string=? (hexpm-version-number version)
                           version-number))
               (hexpm-versions package))))

  (define release
    (and package version*
         (lookup-hexpm-release version*)))

  (and package version*
       (let ((dependencies  (hexpm-release-dependencies release))
             (pkg-meta      (hexpm-meta package)))
         (values
          (make-hexpm-sexp
           #:name package-name
           #:version version-number
           #:home-page (or (hexpm-docs-html-url package)
                           ;; TODO: Homepage?
                           (hexpm-html-url package))
           #:synopsis (hexpm-meta-description pkg-meta)
           #:description (hexpm-meta-description pkg-meta)
           #:license (or (and=> (hexpm-meta-licenses pkg-meta)
                                strings->licenses))
           #:tarball-url (hexpm-uri package-name version-number))
          dependencies))))

(define* (hexpm-recursive-import pkg-name #:optional version)
  (recursive-import pkg-name
                    #:version version
                    #:repo->guix-package hexpm->guix-package
                    #:guix-name hexpm-name->package-name))

(define (guix-package->hexpm-name package)
  "Return the hex.pm name of PACKAGE."
  (define (url->hexpm-name url)
    (hyphen-package-name->name+version
     (basename (file-sans-extension url))))

  (match (and=> (package-source package) origin-uri)
    ((? string? url)
     (url->hexpm-name url))
    ((lst ...)
     (any url->hexpm-name lst))
    (#f #f)))

(define (hexpm-name->package-name name)
  (string-append "erlang-" (string-join (string-split name #\_) "-")))


;;;
;;; Updater
;;;

(define (hexpm-package? package)
  "Return true if PACKAGE is a package from hex.pm."
  (let ((source-url (and=> (package-source package) origin-uri))
        (fetch-method (and=> (package-source package) origin-method)))
    (and (eq? fetch-method hexpm-fetch)
         (match source-url
           ((? string?)
            (hexpm-url? source-url))
           ((source-url ...)
            (any hexpm-url? source-url))))))

(define (latest-release package)
  "Return an <upstream-source> for the latest release of PACKAGE."
  (let* ((hexpm-name (guix-package->hexpm-name package))
         (hexpm      (lookup-hexpm hexpm-name))
         (version    (hexpm-latest-version hexpm))
         (url        (hexpm-uri hexpm-name version)))
    (upstream-source
     (package (package-name package))
     (version version)
     (urls (list url)))))

(define %hexpm-updater
  (upstream-updater
   (name 'hexpm)
   (description "Updater for hex.pm packages")
   (pred hexpm-package?)
   (latest latest-release)))
