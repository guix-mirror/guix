;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2015-2017, 2019-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Lars-Dominik Braun <ldb@leibniz-psychology.org>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2022 Vivien Kraus <vivien@planete-kraus.eu>
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
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (guix utils)
  #:use-module (guix memoization)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module ((guix ui) #:select (display-hint))
  #:use-module ((guix build utils)
                #:select ((package-name->name+version
                           . hyphen-package-name->name+version)
                          find-files
                          invoke))
  #:use-module (guix import utils)
  #:use-module ((guix download) #:prefix download:)
  #:use-module (guix import json)
  #:use-module (json)
  #:use-module (guix packages)
  #:use-module (guix upstream)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system python)
  #:export (parse-requires.txt
            parse-wheel-metadata
            specification->requirement-name
            guix-package->pypi-name
            pypi-recursive-import
            find-project-url
            pypi->guix-package
            %pypi-updater))

;; The PyPI API (notice the rhyme) is "documented" at:
;; <https://warehouse.readthedocs.io/api-reference/json/>.

(define non-empty-string-or-false
  (match-lambda
    ("" #f)
    ((? string? str) str)
    ((or 'null #f) #f)))

;; PyPI project.
(define-json-mapping <pypi-project> make-pypi-project pypi-project?
  json->pypi-project
  (info          pypi-project-info "info" json->project-info) ;<project-info>
  (last-serial   pypi-project-last-serial "last_serial")      ;integer
  (releases      pypi-project-releases "releases" ;string/<distribution>* pairs
                 (match-lambda
                   (((versions . dictionaries) ...)
                    (map (lambda (version vector)
                           (cons version
                                 (map json->distribution
                                      (vector->list vector))))
                         versions dictionaries))))
  (distributions pypi-project-distributions "urls" ;<distribution>*
                 (lambda (vector)
                   (map json->distribution (vector->list vector)))))

;; Project metadata.
(define-json-mapping <project-info> make-project-info project-info?
  json->project-info
  (name         project-info-name)                ;string
  (author       project-info-author)              ;string
  (maintainer   project-info-maintainer)          ;string
  (classifiers  project-info-classifiers          ;list of strings
                "classifiers" vector->list)
  (description  project-info-description)         ;string
  (summary      project-info-summary)             ;string
  (keywords     project-info-keywords)            ;string
  (license      project-info-license)             ;string
  (download-url project-info-download-url         ;string | #f
                "download_url" non-empty-string-or-false)
  (home-page    project-info-home-page            ;string
                "home_page")
  (url          project-info-url "project_url")   ;string
  (release-url  project-info-release-url "release_url") ;string
  (version      project-info-version))            ;string

;; Distribution: a URL along with cryptographic hashes and metadata.
(define-json-mapping <distribution> make-distribution distribution?
  json->distribution
  (url          distribution-url)                  ;string
  (digests      distribution-digests)              ;list of string pairs
  (file-name    distribution-file-name "filename") ;string
  (has-signature? distribution-has-signature? "has_sig") ;Boolean
  (package-type distribution-package-type "packagetype") ;"bdist_wheel" | ...
  (python-version distribution-package-python-version
                  "python_version"))

(define (pypi-fetch name)
  "Return a <pypi-project> record for package NAME, or #f on failure."
  (and=> (json-fetch (string-append "https://pypi.org/pypi/" name "/json"))
         json->pypi-project))

;; For packages found on PyPI that lack a source distribution.
(define-condition-type &missing-source-error &error
  missing-source-error?
  (package  missing-source-error-package))

(define (latest-version project)
  "Return the latest version of PROJECT, a <pypi-project> record."
  (project-info-version (pypi-project-info project)))

(define* (source-release pypi-package
                         #:optional (version (latest-version pypi-package)))
  "Return the source release of VERSION for PYPI-PACKAGE, a <pypi-project>
record, by default the latest version."
  (let ((releases (or (assoc-ref (pypi-project-releases pypi-package) version)
                      '())))
    (or (find (lambda (release)
                (string=? "sdist" (distribution-package-type release)))
              releases)
        (raise (condition (&missing-source-error
                           (package pypi-package)))))))

(define* (wheel-release pypi-package
                        #:optional (version (latest-version pypi-package)))
  "Return the url of the wheel for the latest release of pypi-package,
or #f if there isn't any."
  (let ((releases (assoc-ref (pypi-project-releases pypi-package) version)))
    (find (lambda (release)
            (string=? "bdist_wheel" (distribution-package-type release)))
          releases)))

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

  (or (assoc-ref (package-properties package) 'upstream-name)
      (match (and=> (package-source package) origin-uri)
        ((? string? url)
         (url->pypi-name url))
        ((lst ...)
         (any url->pypi-name lst))
        (#f #f))))

(define (wheel-url->extracted-directory wheel-url)
  (match (string-split (basename wheel-url) #\-)
    ((name version _ ...)
     (string-append name "-" version ".dist-info"))))

(define (maybe-inputs package-inputs input-type)
  "Given a list of PACKAGE-INPUTS, tries to generate the 'inputs' field of a
package definition.  INPUT-TYPE, a symbol, is used to populate the name of
the input field."
  (match package-inputs
    (()
     '())
    ((package-inputs ...)
     `((,input-type (list ,@package-inputs))))))

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

(define (test-section? name)
  "Return #t if the section name contains 'test' or 'dev'."
  (any (cut string-contains-ci name <>)
       '("test" "dev")))

(define (parse-requires.txt requires.txt)
  "Given REQUIRES.TXT, a path to a Setuptools requires.txt file, return a list
of lists of requirements.

The first list contains the required dependencies while the second the
optional test dependencies.  Note that currently, optional, non-test
dependencies are omitted since these can be difficult or expensive to
satisfy."

  (define (comment? line)
    ;; Return #t if the given LINE is a comment, #f otherwise.
    (string-prefix? "#" (string-trim line)))

  (define (section-header? line)
    ;; Return #t if the given LINE is a section header, #f otherwise.
    (string-prefix? "[" (string-trim line)))

  (call-with-input-file requires.txt
    (lambda (port)
      (let loop ((required-deps '())
                 (test-deps '())
                 (inside-test-section? #f)
                 (optional? #f))
        (let ((line (read-line port)))
          (cond
           ((eof-object? line)
            ;; Duplicates can occur, since the same requirement can be
            ;; listed multiple times with different conditional markers, e.g.
            ;; pytest >= 3 ; python_version >= "3.3"
            ;; pytest < 3 ; python_version < "3.3"
            (map (compose reverse delete-duplicates)
                 (list required-deps test-deps)))
           ((or (string-null? line) (comment? line))
            (loop required-deps test-deps inside-test-section? optional?))
           ((section-header? line)
            ;; Encountering a section means that all the requirements
            ;; listed below are optional. Since we want to pick only the
            ;; test dependencies from the optional dependencies, we must
            ;; track those separately.
            (loop required-deps test-deps (test-section? line) #t))
           (inside-test-section?
            (loop required-deps
                  (cons (specification->requirement-name line)
                        test-deps)
                  inside-test-section? optional?))
           ((not optional?)
            (loop (cons (specification->requirement-name line)
                        required-deps)
                  test-deps inside-test-section? optional?))
           (optional?
            ;; Skip optional items.
            (loop required-deps test-deps inside-test-section? optional?))
           (else
            (warning (G_ "parse-requires.txt reached an unexpected \
condition on line ~a~%") line))))))))

(define (parse-wheel-metadata metadata)
  "Given METADATA, a Wheel metadata file, return a list of lists of
requirements.

Refer to the documentation of PARSE-REQUIRES.TXT for a description of the
returned value."
  ;; METADATA is a RFC-2822-like, header based file.

  (define (requires-dist-header? line)
    ;; Return #t if the given LINE is a Requires-Dist header.
    (string-match "^Requires-Dist: " line))

  (define (requires-dist-value line)
    (string-drop line (string-length "Requires-Dist: ")))

  (define (extra? line)
    ;; Return #t if the given LINE is an "extra" requirement.
    (string-match "extra == '(.*)'" line))

  (define (test-requirement? line)
    (and=> (match:substring (extra? line) 1) test-section?))

  (call-with-input-file metadata
    (lambda (port)
      (let loop ((required-deps '())
                 (test-deps '()))
        (let ((line (read-line port)))
          (cond
           ((eof-object? line)
            (map (compose reverse delete-duplicates)
                 (list required-deps test-deps)))
           ((and (requires-dist-header? line) (not (extra? line)))
            (loop (cons (specification->requirement-name
                         (requires-dist-value line))
                        required-deps)
                  test-deps))
           ((and (requires-dist-header? line) (test-requirement? line))
            (loop required-deps
                  (cons (specification->requirement-name (requires-dist-value line))
                        test-deps)))
           (else
            (loop required-deps test-deps)))))))) ;skip line

(define (guess-requirements source-url wheel-url archive)
  "Given SOURCE-URL, WHEEL-URL and an ARCHIVE of the package, return a list
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
         (if (zero?
              (parameterize ((current-error-port (%make-void-port "rw+"))
                             (current-output-port (%make-void-port "rw+")))
                (system* "unzip" wheel-archive "-d" dir metadata)))
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
                (list '() '()))
               (else (parse-requires.txt (first requires.txt-files)))))))
        (begin
          (warning (G_ "Unsupported archive format; \
cannot determine package dependencies from source archive: ~a~%")
                   (basename source-url))
          (list '() '()))))

  ;; First, try to compute the requirements using the wheel, else, fallback to
  ;; reading the "requires.txt" from the egg-info directory from the source
  ;; archive.
  (or (guess-requirements-from-wheel)
      (guess-requirements-from-source)))

(define (compute-inputs source-url wheel-url archive)
  "Given the SOURCE-URL and WHEEL-URL of an already downloaded ARCHIVE, return
a pair of lists, each consisting of a list of name/variable pairs, for the
propagated inputs and the native inputs, respectively.  Also
return the unaltered list of upstream dependency names."

  (define (strip-argparse deps)
    (remove (cut string=? "argparse" <>) deps))

  (define (requirement->package-name/sort deps)
    (map string->symbol
         (sort (map python->package-name deps) string-ci<?)))

  (define process-requirements
    (compose requirement->package-name/sort strip-argparse))

  (let ((dependencies (guess-requirements source-url wheel-url archive)))
    (values (map process-requirements dependencies)
            (concatenate dependencies))))

(define (find-project-url name pypi-url)
  "Try different project name substitution until the result is found in
pypi-uri.  Downcase is required for \"uWSGI\", and
underscores are required for flake8-array-spacing."
  (or (find (cut string-contains pypi-url <>)
            (list name
                  (string-downcase name)
                  (string-replace-substring name "-" "_")))
      (begin
        (warning
         (G_ "project name ~a does not appear verbatim in the PyPI URI~%")
         name)
        (display-hint
         (format #f (G_ "The PyPI URI is: @url{~a}.  You should review the
pypi-uri declaration in the generated package. You may need to replace ~s with
a substring of the PyPI URI that identifies the package.")  pypi-url name))
name)))

(define (make-pypi-sexp name version source-url wheel-url home-page synopsis
                        description license)
  "Return the `package' s-expression for a python package with the given NAME,
VERSION, SOURCE-URL, HOME-PAGE, SYNOPSIS, DESCRIPTION, and LICENSE."
  (define (maybe-upstream-name name)
    (if (string-match ".*\\-[0-9]+" name)
        `((properties ,`'(("upstream-name" . ,name))))
        '()))
  
  (call-with-temporary-output-file
   (lambda (temp port)
     (and (url-fetch source-url temp)
          (receive (guix-dependencies upstream-dependencies)
              (compute-inputs source-url wheel-url temp)
            (match guix-dependencies
              ((required-inputs native-inputs)
               (when (string-suffix? ".zip" source-url)
                 (set! native-inputs (cons
                                     '("unzip" ,unzip)
                                     native-inputs)))
               (values
                `(package
                   (name ,(python->package-name name))
                   (version ,version)
                   (source
                    (origin
                      (method url-fetch)
                      (uri (pypi-uri
                             ,(find-project-url name source-url)
                             version
                             ;; Some packages have been released as `.zip`
                             ;; instead of the more common `.tar.gz`. For
                             ;; example, see "path-and-address".
                             ,@(if (string-suffix? ".zip" source-url)
                                   '(".zip")
                                   '())))
                      (sha256
                       (base32
                        ,(guix-hash-url temp)))))
                   ,@(maybe-upstream-name name)
                   (build-system python-build-system)
                   ,@(maybe-inputs required-inputs 'propagated-inputs)
                   ,@(maybe-inputs native-inputs 'native-inputs)
                   (home-page ,home-page)
                   (synopsis ,synopsis)
                   (description ,(beautify-description description))
                   (license ,(license->symbol license)))
                upstream-dependencies))))))))

(define pypi->guix-package
  (memoize
   (lambda* (package-name #:key repo version)
     "Fetch the metadata for PACKAGE-NAME from pypi.org, and return the
`package' s-expression corresponding to that package, or #f on failure."
     (let* ((project (pypi-fetch package-name))
            (info    (and=> project pypi-project-info))
            (version (or version (and=> project latest-version))))
       (and project
            (guard (c ((missing-source-error? c)
                       (let ((package (missing-source-error-package c)))
                         (raise
                          (make-compound-condition
                           (formatted-message
                            (G_ "no source release for pypi package ~a ~a~%")
                            (project-info-name info) version)
                           (condition
                            (&fix-hint
                             (hint (format #f (G_ "This indicates that the
package is available on PyPI, but only as a \"wheel\" containing binaries, not
source.  To build it from source, refer to the upstream repository at
@uref{~a}.")
                                           (or (project-info-home-page info)
                                               (project-info-url info)
                                               "?"))))))))))
              (make-pypi-sexp (project-info-name info) version
                              (and=> (source-release project version)
                                     distribution-url)
                              (and=> (wheel-release project version)
                                     distribution-url)
                              (project-info-home-page info)
                              (project-info-summary info)
                              (project-info-summary info)
                              (string->license
                               (project-info-license info)))))))))

(define* (pypi-recursive-import package-name #:optional version)
  (recursive-import package-name
                    #:version version
                    #:repo->guix-package pypi->guix-package
                    #:guix-name python->package-name))

(define (string->license str)
  "Convert the string STR into a license object."
  (match str
    ("GNU LGPL" license:lgpl2.0)
    ("GPL" license:gpl3)
    ((or "BSD" "BSD-3" "BSD License") license:bsd-3)
    ("BSD-2-Clause" license:bsd-2)
    ((or "MIT" "MIT license" "MIT License" "Expat license") license:expat)
    ("Public domain" license:public-domain)
    ((or "Apache License, Version 2.0" "Apache 2.0") license:asl2.0)
    ("MPL 2.0" license:mpl2.0)
    (_ #f)))

(define pypi-package?
  (url-predicate
   (lambda (url)
     (or (string-prefix? "https://pypi.org/" url)
         (string-prefix? "https://pypi.python.org/" url)
         (string-prefix? "https://pypi.org/packages" url)
         (string-prefix? "https://files.pythonhosted.org/packages" url)))))

(define (latest-release package)
  "Return an <upstream-source> for the latest release of PACKAGE."
  (let* ((pypi-name    (guix-package->pypi-name package))
         (pypi-package (pypi-fetch pypi-name)))
    (and pypi-package
         (guard (c ((missing-source-error? c) #f))
           (let* ((info    (pypi-project-info pypi-package))
                  (version (project-info-version info))
                  (dist    (source-release pypi-package))
                  (url     (distribution-url dist)))
             (upstream-source
              (urls (list url))
              (signature-urls
               (if (distribution-has-signature? dist)
                   (list (string-append url ".asc"))
                   #f))
              (input-changes
               (changed-inputs package
                               (pypi->guix-package pypi-name)))
              (package (package-name package))
              (version version)))))))

(define %pypi-updater
  (upstream-updater
   (name 'pypi)
   (description "Updater for PyPI packages")
   (pred pypi-package?)
   (latest latest-release)))
