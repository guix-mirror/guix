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
  #:use-module ((ice-9 rdelim) #:select (read-line))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs bytevectors)
  #:use-module (json)
  #:use-module (web uri)
  #:use-module (guix ui)
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

(define (guix-hash-url filename)
  "Return the hash of FILENAME in nix-base32 format."
  (bytevector->nix-base32-string  (file-sha256 filename)))

(define (python->package-name name)
  "Given the NAME of a package on PyPI, return a Guix-compliant name for the
package."
  (if (string-prefix? "python-" name)
      (snake-case name)
      (string-append "python-" (snake-case name))))

(define (maybe-inputs package-inputs)
  "Given a list of PACKAGE-INPUTS, tries to generate the 'inputs' field of a
package definition."
  (match package-inputs
    (()
     '())
    ((package-inputs ...)
     `((inputs (,'quasiquote ,package-inputs))))))

(define (guess-requirements source-url tarball)
  "Given SOURCE-URL and a TARBALL of the package, return a list of the required
packages specified in the requirements.txt file. TARBALL will be extracted in
the current directory, and will be deleted."

  (define (tarball-directory url)
    ;; Given the URL of the package's tarball, return the name of the directory
    ;; that will be created upon decompressing it. If the filetype is not
    ;; supported, return #f.
    ;; TODO: Support more archive formats.
    (let ((basename (substring url (+ 1 (string-rindex url #\/)))))
      (cond
       ((string-suffix? ".tar.gz" basename)
        (string-drop-right basename 7))
       ((string-suffix? ".tar.bz2" basename)
        (string-drop-right basename 8))
       (else
        (begin
          (warning (_ "Unsupported archive format: \
cannot determine package dependencies"))
          #f)))))

  (define (clean-requirement s)
    ;; Given a requirement LINE, as can be found in a Python requirements.txt
    ;; file, remove everything other than the actual name of the required
    ;; package, and return it.
    (string-take s
     (or (string-index s #\space)
         (string-length s))))

  (define (comment? line)
    ;; Return #t if the given LINE is a comment, #f otherwise.
    (eq? (string-ref (string-trim line) 0) #\#))

  (define (read-requirements requirements-file)
    ;; Given REQUIREMENTS-FILE, a Python requirements.txt file, return a list
    ;; of name/variable pairs describing the requirements.
    (call-with-input-file requirements-file
      (lambda (port)
        (let loop ((result '()))
          (let ((line (read-line port)))
            (if (eof-object? line)
                result
                (cond
                 ((or (string-null? line) (comment? line))
                  (loop result))
                 (else
                  (loop (cons (python->package-name (clean-requirement line))
                              result))))))))))

  (let ((dirname (tarball-directory source-url)))
    (if (string? dirname)
        (let* ((req-file (string-append dirname "/requirements.txt"))
               (exit-code (system* "tar" "xf" tarball req-file)))
          ;; TODO: support more formats.
          (if (zero? exit-code)
              (dynamic-wind
                (const #t)
                (lambda ()
                  (read-requirements req-file))
                (lambda ()
                  (delete-file req-file)
                  (rmdir dirname)))
              (begin
                (warning (_ "'tar xf' failed with exit code ~a\n")
                         exit-code)
                '())))
        '())))

(define (compute-inputs source-url tarball)
  "Given the SOURCE-URL of an already downloaded TARBALL, return a list of
name/variable pairs describing the required inputs of this package."
  (sort
    (map (lambda (input)
           (list input (list 'unquote (string->symbol input))))
         (append '("python-setuptools")
                 ;; Argparse has been part of Python since 2.7.
                 (remove (cut string=? "python-argparse" <>)
                         (guess-requirements source-url tarball))))
    (lambda args
      (match args
        (((a _ ...) (b _ ...))
         (string-ci<? a b))))))

(define (make-pypi-sexp name version source-url home-page synopsis
                        description license)
  "Return the `package' s-expression for a python package with the given NAME,
VERSION, SOURCE-URL, HOME-PAGE, SYNOPSIS, DESCRIPTION, and LICENSE."
  (call-with-temporary-output-file
   (lambda (temp port)
     (and (url-fetch source-url temp)
          `(package
             (name ,(python->package-name name))
             (version ,version)
             (source (origin
                       (method url-fetch)
                       (uri (string-append ,@(factorize-uri source-url version)))
                       (sha256
                        (base32
                         ,(guix-hash-url temp)))))
             (build-system python-build-system)
             ,@(maybe-inputs (compute-inputs source-url temp))
             (home-page ,home-page)
             (synopsis ,synopsis)
             (description ,description)
             (license ,(assoc-ref `((,lgpl2.0 . lgpl2.0)
                                    (,gpl3 . gpl3)
                                    (,bsd-3 . bsd-3)
                                    (,expat . expat)
                                    (,public-domain . public-domain)
                                    (,asl2.0 . asl2.0))
                                  license)))))))

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
