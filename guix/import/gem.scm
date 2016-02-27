;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copryight © 2016 Ben Woodcroft <donttrustben@gmail.com>
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

(define-module (guix import gem)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:use-module (json)
  #:use-module (web uri)
  #:use-module ((guix download) #:prefix download:)
  #:use-module (guix import utils)
  #:use-module (guix import json)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module (guix licenses)
  #:use-module (guix base32)
  #:use-module (guix build-system ruby)
  #:use-module (gnu packages)
  #:export (gem->guix-package
            %gem-updater))

(define (rubygems-fetch name)
  "Return an alist representation of the RubyGems metadata for the package NAME,
or #f on failure."
  ;; XXX: We want to silence the download progress report, which is especially
  ;; annoying for 'guix refresh', but we have to use a file port.
  (call-with-output-file "/dev/null"
    (lambda (null)
      (with-error-to-port null
        (lambda ()
          (json-fetch
           (string-append "https://rubygems.org/api/v1/gems/" name ".json")))))))

(define (ruby-package-name name)
  "Given the NAME of a package on RubyGems, return a Guix-compliant name for
the package."
  (if (string-prefix? "ruby-" name)
      (snake-case name)
      (string-append "ruby-" (snake-case name))))

(define (hex-string->bytevector str)
  "Convert the hexadecimal encoded string STR to a bytevector."
  (define hex-char->int
    (match-lambda
     (#\0 0)
     (#\1 1)
     (#\2 2)
     (#\3 3)
     (#\4 4)
     (#\5 5)
     (#\6 6)
     (#\7 7)
     (#\8 8)
     (#\9 9)
     (#\a 10)
     (#\b 11)
     (#\c 12)
     (#\d 13)
     (#\e 14)
     (#\f 15)))

  (define (read-byte i)
    (let ((j (* 2 i)))
      (+ (hex-char->int (string-ref str (1+ j)))
         (* (hex-char->int (string-ref str j)) 16))))

  (let* ((len (/ (string-length str) 2))
         (bv  (make-bytevector len)))
    (let loop ((i 0))
      (if (= i len)
          bv
          (begin
            (bytevector-u8-set! bv i (read-byte i))
            (loop (1+ i)))))))

(define (make-gem-sexp name version hash home-page synopsis description
                       dependencies licenses)
  "Return the `package' s-expression for a Ruby package with the given NAME,
VERSION, HASH, HOME-PAGE, DESCRIPTION, DEPENDENCIES, and LICENSES."
  `(package
     (name ,(ruby-package-name name))
     (version ,version)
     (source (origin
               (method url-fetch)
               (uri (rubygems-uri ,name version))
               (sha256
                (base32
                 ,(bytevector->nix-base32-string
                   (hex-string->bytevector hash))))))
     (build-system ruby-build-system)
     ,@(if (null? dependencies)
           '()
           `((propagated-inputs
              (,'quasiquote
               ,(map (lambda (name)
                       `(,name
                         (,'unquote
                          ,(string->symbol name))))
                     dependencies)))))
     (synopsis ,synopsis)
     (description ,description)
     (home-page ,home-page)
     (license ,(match licenses
                 (() #f)
                 ((license) (license->symbol license))
                 (_ `(list ,@(map license->symbol licenses)))))))

(define* (gem->guix-package package-name #:optional version)
  "Fetch the metadata for PACKAGE-NAME from rubygems.org, and return the
`package' s-expression corresponding to that package, or #f on failure."
  (let ((package (rubygems-fetch package-name)))
    (and package
         (let ((name         (assoc-ref package "name"))
               (version      (assoc-ref package "version"))
               (hash         (assoc-ref package "sha"))
               (synopsis     (assoc-ref package "info")) ; nothing better to use
               (description  (beautify-description
                              (assoc-ref package "info")))
               (home-page    (assoc-ref package "homepage_uri"))
               (dependencies (map (lambda (dep)
                                    (let ((name (assoc-ref dep "name")))
                                      (if (string=? name "bundler")
                                          "bundler" ; special case, no prefix
                                          (ruby-package-name name))))
                                  (assoc-ref* package "dependencies"
                                              "runtime")))
               (licenses     (map string->license
                                  (assoc-ref package "licenses"))))
           (make-gem-sexp name version hash home-page synopsis
                          description dependencies licenses)))))

(define (guix-package->gem-name package)
  "Given a PACKAGE built from rubygems.org, return the name of the
package on RubyGems."
  (let ((source-url (and=> (package-source package) origin-uri)))
    ;; The URL has the form:
    ;; 'https://rubygems.org/downloads/' +
    ;; package name + '-' + version + '.gem'
    ;; e.g. "https://rubygems.org/downloads/hashery-2.1.1.gem"
    (substring source-url 31 (string-rindex source-url #\-))))

(define (gem-package? package)
  "Return true if PACKAGE is a gem package from RubyGems."

  (define (rubygems-url? url)
    (string-prefix? "https://rubygems.org/downloads/" url))

  (let ((source-url (and=> (package-source package) origin-uri))
        (fetch-method (and=> (package-source package) origin-method)))
    (and (eq? fetch-method download:url-fetch)
         (match source-url
           ((? string?)
            (rubygems-url? source-url))
           ((source-url ...)
            (any rubygems-url? source-url))))))

(define (latest-release guix-package)
  "Return an <upstream-source> for the latest release of GUIX-PACKAGE."
  (let* ((gem-name (guix-package->gem-name
                    (specification->package guix-package)))
         (metadata (rubygems-fetch gem-name))
         (version (assoc-ref metadata "version"))
         (url (rubygems-uri gem-name version)))
    (upstream-source
     (package guix-package)
     (version version)
     (urls (list url)))))

(define %gem-updater
  (upstream-updater
   (name 'gem)
   (description "Updater for RubyGem packages")
   (pred gem-package?)
   (latest latest-release)))
