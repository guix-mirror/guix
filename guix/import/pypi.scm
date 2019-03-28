;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 receive)
  #:use-module ((ice-9 rdelim) #:select (read-line))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module ((guix build utils)
                #:select ((package-name->name+version
                           . hyphen-package-name->name+version)
                          find-files
                          invoke))
  #:use-module (guix import utils)
  #:use-module ((guix download) #:prefix download:)
  #:use-module (guix import json)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system python)
  #:export (parse-requires.txt
            parse-wheel-metadata
            specification->requirement-name
            guix-package->pypi-name
            pypi-recursive-import
            pypi->guix-package
            %pypi-updater))

(define (pypi-fetch name)
  "Return an alist representation of the PyPI metadata for the package NAME,
or #f on failure."
  (json-fetch-alist (string-append "https://pypi.org/pypi/" name "/json")))

;; For packages found on PyPI that lack a source distribution.
(define-condition-type &missing-source-error &error
  missing-source-error?
  (package  missing-source-error-package))

(define (latest-source-release pypi-package)
  "Return the latest source release for PYPI-PACKAGE."
  (let ((releases (assoc-ref* pypi-package "releases"
                              (assoc-ref* pypi-package "info" "version"))))
    (or (find (lambda (release)
                (string=? "sdist" (assoc-ref release "packagetype")))
              releases)
        (raise (condition (&missing-source-error
                           (package pypi-package)))))))

(define (latest-wheel-release pypi-package)
  "Return the url of the wheel for the latest release of pypi-package,
or #f if there isn't any."
  (let ((releases (assoc-ref* pypi-package "releases"
                              (assoc-ref* pypi-package "info" "version"))))
    (or (find (lambda (release)
                (string=? "bdist_wheel" (assoc-ref release "packagetype")))
              releases)
        #f)))

(define (python->package-name name)
  "Given the NAME of a package on PyPI, return a Guix-compliant name for the
package."
  (if (string-prefix? "python-" name)
      (snake-case name)
      (string-append "python-" (snake-case name))))

(define (guix-package->pypi-name package)
  "Given a Python PACKAGE built from pypi.org, return the name of the
package on PyPI."
  (define (url->pypi-name url)
    (hyphen-package-name->name+version
     (basename (file-sans-extension url))))

  (match (and=> (package-source package) origin-uri)
    ((? string? url)
     (url->pypi-name url))
    ((lst ...)
     (any url->pypi-name lst))
    (#f #f)))

(define (wheel-url->extracted-directory wheel-url)
  (match (string-split (basename wheel-url) #\-)
    ((name version _ ...)
     (string-append name "-" version ".dist-info"))))

(define (maybe-inputs package-inputs)
  "Given a list of PACKAGE-INPUTS, tries to generate the 'inputs' field of a
package definition."
  (match package-inputs
    (()
     '())
    ((package-inputs ...)
     `((propagated-inputs (,'quasiquote ,package-inputs))))))

(define %requirement-name-regexp
  ;; Regexp to match the requirement name in a requirement specification.

  ;; Some grammar, taken from PEP-0508 (see:
  ;; https://www.python.org/dev/peps/pep-0508/).

  ;; Using this grammar makes the PEP-0508 regexp easier to understand for
  ;; humans.  The use of a regexp is preferred to more primitive string
  ;; manipulations because we can more directly match what upstream uses
  ;; (again, per PEP-0508).  The regexp approach is also easier to extend,
  ;; should we want to implement more completely the grammar of PEP-0508.

  ;; The unified rule can be expressed as:
  ;; specification = wsp* ( url_req | name_req ) wsp*

  ;; where url_req is:
  ;; url_req = name wsp* extras? wsp* urlspec wsp+ quoted_marker?

  ;; and where name_req is:
  ;; name_req = name wsp* extras? wsp* versionspec? wsp* quoted_marker?

  ;; Thus, we need only matching NAME, which is expressed as:
  ;; identifer_end = letterOrDigit | (('-' | '_' | '.' )* letterOrDigit)
  ;; identifier    = letterOrDigit identifier_end*
  ;; name          = identifier
  (let* ((letter-or-digit "[A-Za-z0-9]")
         (identifier-end (string-append "(" letter-or-digit "|"
                                        "[-_.]*" letter-or-digit ")"))
         (identifier (string-append "^" letter-or-digit identifier-end "*"))
         (name identifier))
    (make-regexp name)))

(define (specification->requirement-name spec)
  "Given a specification SPEC, return the requirement name."
  (match:substring
   (or (regexp-exec %requirement-name-regexp spec)
       (error (G_ "Could not extract requirement name in spec:") spec))))

(define (parse-requires.txt requires.txt)
  "Given REQUIRES.TXT, a Setuptools requires.txt file, return a list of
requirement names."

  (define (comment? line)
    ;; Return #t if the given LINE is a comment, #f otherwise.
    (string-prefix? "#" (string-trim line)))

  (define (section-header? line)
    ;; Return #t if the given LINE is a section header, #f otherwise.
    (string-prefix? "[" (string-trim line)))

  (call-with-input-file requires.txt
    (lambda (port)
      (let loop ((result '()))
        (let ((line (read-line port)))
          ;; Stop when a section is encountered, as sections contain optional
          ;; (extra) requirements.  Non-optional requirements must appear
          ;; before any section is defined.
          (cond
           ((or (eof-object? line) (section-header? line))
            ;; Duplicates can occur, since the same requirement can be
            ;; listed multiple times with different conditional markers, e.g.
            ;; pytest >= 3 ; python_version >= "3.3"
            ;; pytest < 3 ; python_version < "3.3"
            (reverse (delete-duplicates result)))
           ((or (string-null? line) (comment? line))
            (loop result))
           (else
            (loop (cons (specification->requirement-name line)
                        result)))))))))

(define (parse-wheel-metadata metadata)
  "Given METADATA, a Wheel metadata file, return a list of requirement names."
  ;; METADATA is a RFC-2822-like, header based file.

  (define (requires-dist-header? line)
    ;; Return #t if the given LINE is a Requires-Dist header.
    (string-match "^Requires-Dist: " line))

  (define (requires-dist-value line)
    (string-drop line (string-length "Requires-Dist: ")))

  (define (extra? line)
    ;; Return #t if the given LINE is an "extra" requirement.
    (string-match "extra == '(.*)'" line))

  (call-with-input-file metadata
    (lambda (port)
      (let loop ((requirements '()))
        (let ((line (read-line port)))
          ;; Stop at the first 'Provides-Extra' section: the non-optional
          ;; requirements appear before the optional ones.
          (cond
           ((eof-object? line)
            (reverse (delete-duplicates requirements)))
           ((and (requires-dist-header? line) (not (extra? line)))
            (loop (cons (specification->requirement-name
                         (requires-dist-value line))
                        requirements)))
           (else
            (loop requirements))))))))

(define (guess-requirements source-url wheel-url archive)
  "Given SOURCE-URL, WHEEL-URL and a ARCHIVE of the package, return a list
of the required packages specified in the requirements.txt file.  ARCHIVE will
be extracted in a temporary directory."

  (define (read-wheel-metadata wheel-archive)
    ;; Given WHEEL-ARCHIVE, a ZIP Python wheel archive, return the package's
    ;; requirements, or #f if the metadata file contained therein couldn't be
    ;; extracted.
    (let* ((dirname (wheel-url->extracted-directory wheel-url))
           (metadata (string-append dirname "/METADATA")))
      (call-with-temporary-directory
       (lambda (dir)
         (if (zero? (system* "unzip" "-q" wheel-archive "-d" dir metadata))
             (parse-wheel-metadata (string-append dir "/" metadata))
             (begin
               (warning
                (G_ "Failed to extract file: ~a from wheel.~%") metadata)
               #f))))))

  (define (guess-requirements-from-wheel)
    ;; Return the package's requirements using the wheel, or #f if an error
    ;; occurs.
    (call-with-temporary-output-file
     (lambda (temp port)
       (if wheel-url
           (and (url-fetch wheel-url temp)
                (read-wheel-metadata temp))
           #f))))

  (define (guess-requirements-from-source)
    ;; Return the package's requirements by guessing them from the source.
    (if (compressed-file? source-url)
        (call-with-temporary-directory
         (lambda (dir)
           (parameterize ((current-error-port (%make-void-port "rw+"))
                          (current-output-port (%make-void-port "rw+")))
             (if (string=? "zip" (file-extension source-url))
                 (invoke "unzip" archive "-d" dir)
                 (invoke "tar" "xf" archive "-C" dir)))
           (let ((requires.txt-files
                  (find-files dir (lambda (abs-file-name _)
		                    (string-match "\\.egg-info/requires.txt$"
                                                  abs-file-name)))))
             (match requires.txt-files
               (()
                (warning (G_ "Cannot guess requirements from source archive:\
 no requires.txt file found.~%"))
                '())
               (else (parse-requires.txt (first requires.txt-files)))))))
        (begin
          (warning (G_ "Unsupported archive format; \
cannot determine package dependencies from source archive: ~a~%")
                   (basename source-url))
          '())))

  ;; First, try to compute the requirements using the wheel, else, fallback to
  ;; reading the "requires.txt" from the egg-info directory from the source
  ;; tarball.
  (or (guess-requirements-from-wheel)
      (guess-requirements-from-source)))

(define (compute-inputs source-url wheel-url archive)
  "Given the SOURCE-URL of an already downloaded ARCHIVE, return a list of
name/variable pairs describing the required inputs of this package.  Also
return the unaltered list of upstream dependency names."
  (let ((dependencies
         (remove (cut string=? "argparse" <>)
                 (guess-requirements source-url wheel-url archive))))
    (values (sort
             (map (lambda (input)
                    (let ((guix-name (python->package-name input)))
                      (list guix-name (list 'unquote (string->symbol guix-name)))))
                  dependencies)
             (lambda args
               (match args
                 (((a _ ...) (b _ ...))
                  (string-ci<? a b)))))
            dependencies)))

(define (make-pypi-sexp name version source-url wheel-url home-page synopsis
                        description license)
  "Return the `package' s-expression for a python package with the given NAME,
VERSION, SOURCE-URL, HOME-PAGE, SYNOPSIS, DESCRIPTION, and LICENSE."
  (call-with-temporary-output-file
   (lambda (temp port)
     (and (url-fetch source-url temp)
          (receive (input-package-names upstream-dependency-names)
              (compute-inputs source-url wheel-url temp)
            (values
             `(package
                (name ,(python->package-name name))
                (version ,version)
                (source (origin
                          (method url-fetch)

                          ;; Sometimes 'pypi-uri' doesn't quite work due to mixed
                          ;; cases in NAME, for instance, as is the case with
                          ;; "uwsgi".  In that case, fall back to a full URL.
                          (uri (pypi-uri ,(string-downcase name) version))
                          (sha256
                           (base32
                            ,(guix-hash-url temp)))))
                (build-system python-build-system)
                ,@(maybe-inputs input-package-names)
                (home-page ,home-page)
                (synopsis ,synopsis)
                (description ,description)
                (license ,(license->symbol license)))
             upstream-dependency-names))))))

(define pypi->guix-package
  (memoize
   (lambda* (package-name)
     "Fetch the metadata for PACKAGE-NAME from pypi.org, and return the
`package' s-expression corresponding to that package, or #f on failure."
     (let ((package (pypi-fetch package-name)))
       (and package
            (guard (c ((missing-source-error? c)
                       (let ((package (missing-source-error-package c)))
                         (leave (G_ "no source release for pypi package ~a ~a~%")
                                (assoc-ref* package "info" "name")
                                (assoc-ref* package "info" "version")))))
              (let ((name (assoc-ref* package "info" "name"))
                    (version (assoc-ref* package "info" "version"))
                    (release (assoc-ref (latest-source-release package) "url"))
                    (wheel (assoc-ref (latest-wheel-release package) "url"))
                    (synopsis (assoc-ref* package "info" "summary"))
                    (description (assoc-ref* package "info" "summary"))
                    (home-page (assoc-ref* package "info" "home_page"))
                    (license (string->license (assoc-ref* package "info" "license"))))
                (make-pypi-sexp name version release wheel home-page synopsis
                                description license))))))))

(define (pypi-recursive-import package-name)
  (recursive-import package-name #f
                    #:repo->guix-package (lambda (name repo)
                                           (pypi->guix-package name))
                    #:guix-name python->package-name))

(define (string->license str)
  "Convert the string STR into a license object."
  (match str
    ("GNU LGPL" license:lgpl2.0)
    ("GPL" license:gpl3)
    ((or "BSD" "BSD License") license:bsd-3)
    ((or "MIT" "MIT license" "Expat license") license:expat)
    ("Public domain" license:public-domain)
    ((or "Apache License, Version 2.0" "Apache 2.0") license:asl2.0)
    (_ #f)))

(define (pypi-package? package)
  "Return true if PACKAGE is a Python package from PyPI."

  (define (pypi-url? url)
    (or (string-prefix? "https://pypi.org/" url)
        (string-prefix? "https://pypi.python.org/" url)
        (string-prefix? "https://pypi.org/packages" url)))

  (let ((source-url (and=> (package-source package) origin-uri))
        (fetch-method (and=> (package-source package) origin-method)))
    (and (eq? fetch-method download:url-fetch)
         (match source-url
           ((? string?)
            (pypi-url? source-url))
           ((source-url ...)
            (any pypi-url? source-url))))))

(define (latest-release package)
  "Return an <upstream-source> for the latest release of PACKAGE."
  (let* ((pypi-name    (guix-package->pypi-name package))
         (pypi-package (pypi-fetch pypi-name)))
    (and pypi-package
         (guard (c ((missing-source-error? c) #f))
           (let* ((metadata pypi-package)
                  (version (assoc-ref* metadata "info" "version"))
                  (url (assoc-ref (latest-source-release metadata) "url")))
             (upstream-source
              (package (package-name package))
              (version version)
              (urls (list url))))))))

(define %pypi-updater
  (upstream-updater
   (name 'pypi)
   (description "Updater for PyPI packages")
   (pred pypi-package?)
   (latest latest-release)))
