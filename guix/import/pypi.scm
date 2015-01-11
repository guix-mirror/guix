;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
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

(define-module (guix import pypi)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:use-module (json)
  #:use-module (web uri)
  #:use-module (guix utils)
  #:use-module (guix import utils)
  #:use-module (guix import json)
  #:use-module (guix base32)
  #:use-module (guix hash)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python)
  #:export (pypi->guix-package))

(define (join lst delimiter)
  "Return a list that contains the elements of LST, each separated by
DELIMETER."
  (match lst
    (() '())
    ((elem)
     (list elem))
    ((elem . rest)
     (cons* elem delimiter (join rest delimiter)))))

(define string->license
  (match-lambda
   ("GNU LGPL" lgpl2.0)
   ("GPL" gpl3)
   ((or "BSD" "BSD License") bsd-3)
   ((or "MIT" "MIT license" "Expat license") expat)
   ("Public domain" public-domain)
   ("Apache License, Version 2.0" asl2.0)
   (_ #f)))

(define (pypi-fetch name)
  "Return an alist representation of the PyPI metadata for the package NAME,
or #f on failure."
  (json-fetch (string-append "https://pypi.python.org/pypi/" name "/json")))

(define (latest-source-release pypi-package)
  "Return the latest source release for PYPI-PACKAGE."
  (let ((releases (assoc-ref* pypi-package "releases"
                              (assoc-ref* pypi-package "info" "version"))))
    (or (find (lambda (release)
                (string=? "sdist" (assoc-ref release "packagetype")))
              releases)
        (error "No source release found for pypi package: "
               (assoc-ref* pypi-package "info" "name")
               (assoc-ref* pypi-package "info" "version")))))

(define (snake-case str)
  "Return a downcased version of the string STR where underscores are replaced
with dashes."
  (string-join (string-split (string-downcase str) #\_) "-"))

(define (guix-hash-url url)
  "Download the resource at URL and return the hash in nix-base32 format."
  (call-with-temporary-output-file
   (lambda (temp port)
     (and (url-fetch url temp)
          (bytevector->nix-base32-string
           (call-with-input-file temp port-sha256))))))

(define (make-pypi-sexp name version source-url home-page synopsis
                        description license)
  "Return the `package' s-expression for a python package with the given NAME,
VERSION, SOURCE-URL, HOME-PAGE, SYNOPSIS, DESCRIPTION, and LICENSE."
  `(package
     (name ,(if (string-prefix? "python-" name)
                (snake-case name)
                (string-append "python-" (snake-case name))))
     (version ,version)
     (source (origin
               (method url-fetch)
               (uri (string-append ,@(factorize-uri source-url version)))
               (sha256
                (base32
                 ,(guix-hash-url source-url)))))
     (build-system python-build-system)
     (inputs
      `(("python-setuptools" ,python-setuptools)))
     (home-page ,home-page)
     (synopsis ,synopsis)
     (description ,description)
     (license ,(assoc-ref `((,lgpl2.0 . lgpl2.0)
                            (,gpl3 . gpl3)
                            (,bsd-3 . bsd-3)
                            (,expat . expat)
                            (,public-domain . public-domain)
                            (,asl2.0 . asl2.0))
                          license))))

(define (pypi->guix-package package-name)
  "Fetch the metadata for PACKAGE-NAME from pypi.python.org, and return the
`package' s-expression corresponding to that package, or #f on failure."
  (let ((package (pypi-fetch package-name)))
    (and package
         (let ((name (assoc-ref* package "info" "name"))
               (version (assoc-ref* package "info" "version"))
               (release (assoc-ref (latest-source-release package) "url"))
               (synopsis (assoc-ref* package "info" "summary"))
               (description (assoc-ref* package "info" "summary"))
               (home-page (assoc-ref* package "info" "home_page"))
               (license (string->license (assoc-ref* package "info" "license"))))
           (make-pypi-sexp name version release home-page synopsis
                           description license)))))
