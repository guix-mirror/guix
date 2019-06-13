;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (guix import cran)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module ((ice-9 rdelim) #:select (read-string read-line))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (ice-9 receive)
  #:use-module (web uri)
  #:use-module (guix memoization)
  #:use-module (guix http-client)
  #:use-module (gcrypt hash)
  #:use-module (guix store)
  #:use-module (guix base32)
  #:use-module ((guix download) #:select (download-to-store))
  #:use-module (guix import utils)
  #:use-module ((guix build utils) #:select (find-files))
  #:use-module (guix utils)
  #:use-module ((guix build-system r) #:select (cran-uri bioconductor-uri))
  #:use-module (guix upstream)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:export (cran->guix-package
            bioconductor->guix-package
            cran-recursive-import
            %cran-updater
            %bioconductor-updater

            cran-package?
            bioconductor-package?
            bioconductor-data-package?
            bioconductor-experiment-package?))

;;; Commentary:
;;;
;;; Generate a package declaration template for the latest version of an R
;;; package on CRAN, using the DESCRIPTION file downloaded from
;;; cran.r-project.org.
;;;
;;; Code:

(define string->license
  (match-lambda
   ("AGPL-3" 'agpl3+)
   ("Artistic-2.0" 'artistic2.0)
   ("Apache License 2.0" 'asl2.0)
   ("BSD_2_clause" 'bsd-2)
   ("BSD_2_clause + file LICENSE" 'bsd-2)
   ("BSD_3_clause" 'bsd-3)
   ("BSD_3_clause + file LICENSE" 'bsd-3)
   ("GPL" '(list gpl2+ gpl3+))
   ("GPL (>= 2)" 'gpl2+)
   ("GPL (>= 3)" 'gpl3+)
   ("GPL-2" 'gpl2)
   ("GPL-3" 'gpl3)
   ("LGPL-2" 'lgpl2.0)
   ("LGPL-2.1" 'lgpl2.1)
   ("LGPL-3" 'lgpl3)
   ("LGPL (>= 2)" 'lgpl2.0+)
   ("LGPL (>= 3)" 'lgpl3+)
   ("MIT" 'expat)
   ("MIT + file LICENSE" 'expat)
   ((x) (string->license x))
   ((lst ...) `(list ,@(map string->license lst)))
   (_ #f)))


(define (description->alist description)
  "Convert a DESCRIPTION string into an alist."
  (let ((lines (string-split description #\newline))
        (parse (lambda (line acc)
                 (if (string-null? line) acc
                     ;; Keys usually start with a capital letter and end with
                     ;; ":".  There are some exceptions, unfortunately (such
                     ;; as "biocViews").  There are no blanks in a key.
                     (if (string-match "^[A-Za-z][^ :]+:( |\n|$)" line)
                         ;; New key/value pair
                         (let* ((pos   (string-index line #\:))
                                (key   (string-take line pos))
                                (value (string-drop line (+ 1 pos))))
                           (cons (cons key
                                       (string-trim-both value))
                                 acc))
                         ;; This is a continuation of the previous pair
                         (match-let ((((key . value) . rest) acc))
                           (cons (cons key (string-join
                                            (list value
                                                  (string-trim-both line))))
                                 rest)))))))
    (fold parse '() lines)))

(define (format-inputs names)
  "Generate a sorted list of package inputs from a list of package NAMES."
  (map (lambda (name)
         (list name (list 'unquote (string->symbol name))))
       (sort names string-ci<?)))

(define* (maybe-inputs package-inputs #:optional (type 'inputs))
  "Given a list of PACKAGE-INPUTS, tries to generate the TYPE field of a
package definition."
  (match package-inputs
    (()
     '())
    ((package-inputs ...)
     `((,type (,'quasiquote ,(format-inputs package-inputs)))))))

(define %cran-url "https://cran.r-project.org/web/packages/")
(define %bioconductor-url "https://bioconductor.org/packages/")

;; The latest Bioconductor release is 3.9.  Bioconductor packages should be
;; updated together.
(define %bioconductor-version "3.9")

(define %bioconductor-packages-list-url
  (string-append "https://bioconductor.org/packages/"
                 %bioconductor-version "/bioc/src/contrib/PACKAGES"))

(define (bioconductor-packages-list)
  "Return the latest version of package NAME for the current bioconductor
release."
  (let ((url (string->uri %bioconductor-packages-list-url)))
    (guard (c ((http-get-error? c)
               (format (current-error-port)
                       "error: failed to retrieve list of packages from ~s: ~a (~s)~%"
                       (uri->string (http-get-error-uri c))
                       (http-get-error-code c)
                       (http-get-error-reason c))
               #f))
      ;; Split the big list on empty lines, then turn each chunk into an
      ;; alist of attributes.
      (map (lambda (chunk)
             (description->alist (string-join chunk "\n")))
           (chunk-lines (read-lines (http-fetch/cached url)))))))

(define (latest-bioconductor-package-version name)
  "Return the version string corresponding to the latest release of the
bioconductor package NAME, or #F if the package is unknown."
  (and=> (find (lambda (meta)
                 (string=? (assoc-ref meta "Package") name))
               (bioconductor-packages-list))
         (cut assoc-ref <> "Version")))

;; Little helper to download URLs only once.
(define download
  (memoize
   (lambda (url)
     (with-store store (download-to-store store url)))))

(define (fetch-description repository name)
  "Return an alist of the contents of the DESCRIPTION file for the R package
NAME in the given REPOSITORY, or #f in case of failure.  NAME is
case-sensitive."
  (case repository
    ((cran)
     (let ((url (string-append %cran-url name "/DESCRIPTION")))
       (guard (c ((http-get-error? c)
                  (format (current-error-port)
                          "error: failed to retrieve package information \
from ~s: ~a (~s)~%"
                          (uri->string (http-get-error-uri c))
                          (http-get-error-code c)
                          (http-get-error-reason c))
                  #f))
         (description->alist (read-string (http-fetch url))))))
    ((bioconductor)
     ;; Currently, the bioconductor project does not offer a way to access a
     ;; package's DESCRIPTION file over HTTP, so we determine the version,
     ;; download the source tarball, and then extract the DESCRIPTION file.
     (and-let* ((version (latest-bioconductor-package-version name))
                (url     (car (bioconductor-uri name version)))
                (tarball (download url)))
       (call-with-temporary-directory
        (lambda (dir)
          (parameterize ((current-error-port (%make-void-port "rw+"))
                         (current-output-port (%make-void-port "rw+")))
            (and (zero? (system* "tar" "--wildcards" "-x"
                                 "--strip-components=1"
                                 "-C" dir
                                 "-f" tarball "*/DESCRIPTION"))
                 (description->alist (with-input-from-file
                                         (string-append dir "/DESCRIPTION") read-string))))))))))

(define (listify meta field)
  "Look up FIELD in the alist META.  If FIELD contains a comma-separated
string, turn it into a list and strip off parenthetic expressions.  Return the
empty list when the FIELD cannot be found."
  (let ((value (assoc-ref meta field)))
    (if (not value)
        '()
        ;; Strip off parentheses
        (let ((items (string-split (regexp-substitute/global
                                    #f "( *\\([^\\)]+\\)) *"
                                    value 'pre 'post)
                                   #\,)))
          (remove (lambda (item)
                    (or (string-null? item)
                        ;; When there is whitespace inside of items it is
                        ;; probably because this was not an actual list to
                        ;; begin with.
                        (string-any char-set:whitespace item)))
                  (map string-trim-both items))))))

(define default-r-packages
  (list "base"
        "compiler"
        "grDevices"
        "graphics"
        "grid"
        "methods"
        "parallel"
        "splines"
        "stats"
        "stats4"
        "tcltk"
        "tools"
        "translations"
        "utils"))

;; The field for system dependencies is often abused to specify non-package
;; dependencies (such as c++11).  This list is used to ignore them.
(define invalid-packages
  (list "c++11"))

(define cran-guix-name (cut guix-name "r-" <>))

(define (needs-fortran? tarball)
  "Check if the TARBALL contains Fortran source files."
  (define (check pattern)
    (parameterize ((current-error-port (%make-void-port "rw+"))
                   (current-output-port (%make-void-port "rw+")))
      (zero? (system* "tar" "--wildcards" "--list" pattern "-f" tarball))))
  (or (check "*.f90")
      (check "*.f95")
      (check "*.f")))

(define (tarball-files-match-pattern? tarball regexp . file-patterns)
  "Return #T if any of the files represented by FILE-PATTERNS in the TARBALL
match the given REGEXP."
  (call-with-temporary-directory
   (lambda (dir)
     (let ((pattern (make-regexp regexp)))
       (parameterize ((current-error-port (%make-void-port "rw+")))
         (apply system* "tar"
                "xf" tarball "-C" dir
                `("--wildcards" ,@file-patterns)))
       (any (lambda (file)
              (call-with-input-file file
                (lambda (port)
                  (let loop ()
                    (let ((line (read-line port)))
                      (cond
                       ((eof-object? line) #f)
                       ((regexp-exec pattern line) #t)
                       (else (loop))))))))
            (find-files dir))))))

(define (needs-zlib? tarball)
  "Return #T if any of the Makevars files in the src directory of the TARBALL
contain a zlib linker flag."
  (tarball-files-match-pattern?
   tarball "-lz"
   "*/src/Makevars*" "*/src/configure*" "*/configure*"))

(define (needs-pkg-config? tarball)
  "Return #T if any of the Makevars files in the src directory of the TARBALL
reference the pkg-config tool."
  (tarball-files-match-pattern?
   tarball "pkg-config"
   "*/src/Makevars*" "*/src/configure*" "*/configure*"))

(define (description->package repository meta)
  "Return the `package' s-expression for an R package published on REPOSITORY
from the alist META, which was derived from the R package's DESCRIPTION file."
  (let* ((base-url   (case repository
                       ((cran)         %cran-url)
                       ((bioconductor) %bioconductor-url)))
         (uri-helper (case repository
                       ((cran)         cran-uri)
                       ((bioconductor) bioconductor-uri)))
         (name       (assoc-ref meta "Package"))
         (synopsis   (assoc-ref meta "Title"))
         (version    (assoc-ref meta "Version"))
         (license    (string->license (assoc-ref meta "License")))
         ;; Some packages have multiple home pages.  Some have none.
         (home-page  (match (listify meta "URL")
                       ((url rest ...) url)
                       (_ (string-append base-url name))))
         (source-url (match (uri-helper name version)
                       ((url rest ...) url)
                       ((? string? url) url)
                       (_ #f)))
         (tarball    (download source-url))
         (sysdepends (append
                      (if (needs-zlib? tarball) '("zlib") '())
                      (filter (lambda (name)
                                (not (member name invalid-packages)))
                              (map string-downcase (listify meta "SystemRequirements")))))
         (propagate  (filter (lambda (name)
                               (not (member name (append default-r-packages
                                                         invalid-packages))))
                             (lset-union equal?
                                         (listify meta "Imports")
                                         (listify meta "LinkingTo")
                                         (delete "R"
                                                 (listify meta "Depends"))))))
    (values
     `(package
        (name ,(cran-guix-name name))
        (version ,version)
        (source (origin
                  (method url-fetch)
                  (uri (,(procedure-name uri-helper) ,name version))
                  (sha256
                   (base32
                    ,(bytevector->nix-base32-string (file-sha256 tarball))))))
        ,@(if (not (equal? (string-append "r-" name)
                           (cran-guix-name name)))
              `((properties ,`(,'quasiquote ((,'upstream-name . ,name)))))
              '())
        (build-system r-build-system)
        ,@(maybe-inputs sysdepends)
        ,@(maybe-inputs (map cran-guix-name propagate) 'propagated-inputs)
        ,@(maybe-inputs
           `(,@(if (needs-fortran? tarball)
                   '("gfortran") '())
             ,@(if (needs-pkg-config? tarball)
                   '("pkg-config") '()))
           'native-inputs)
        (home-page ,(if (string-null? home-page)
                        (string-append base-url name)
                        home-page))
        (synopsis ,synopsis)
        (description ,(beautify-description (or (assoc-ref meta "Description")
                                                "")))
        (license ,license))
     propagate)))

(define cran->guix-package
  (memoize
   (lambda* (package-name #:optional (repo 'cran))
     "Fetch the metadata for PACKAGE-NAME from REPO and return the `package'
s-expression corresponding to that package, or #f on failure."
     (let ((description (fetch-description repo package-name)))
       (if (and (not description)
                (eq? repo 'bioconductor))
           ;; Retry import from CRAN
           (cran->guix-package package-name 'cran)
           (and description
                (description->package repo description)))))))

(define* (cran-recursive-import package-name #:optional (repo 'cran))
  (recursive-import package-name repo
                    #:repo->guix-package cran->guix-package
                    #:guix-name cran-guix-name))


;;;
;;; Updater.
;;;

(define (package->upstream-name package)
  "Return the upstream name of the PACKAGE."
  (let* ((properties (package-properties package))
         (upstream-name (and=> properties
                               (cut assoc-ref <> 'upstream-name))))
    (if upstream-name
        upstream-name
        (match (package-source package)
          ((? origin? origin)
           (match (origin-uri origin)
             ((or (? string? url) (url _ ...))
              (let ((end   (string-rindex url #\_))
                    (start (string-rindex url #\/)))
                ;; The URL ends on
                ;; (string-append "/" name "_" version ".tar.gz")
                (and start end (substring url (+ start 1) end))))
             (_ #f)))
          (_ #f)))))

(define (latest-cran-release pkg)
  "Return an <upstream-source> for the latest release of the package PKG."

  (define upstream-name
    (package->upstream-name pkg))

  (define meta
    (fetch-description 'cran upstream-name))

  (and meta
       (let ((version (assoc-ref meta "Version")))
         ;; CRAN does not provide signatures.
         (upstream-source
          (package (package-name pkg))
          (version version)
          (urls (cran-uri upstream-name version))
          (input-changes
           (changed-inputs pkg
                           (description->package 'cran meta)))))))

(define (latest-bioconductor-release pkg)
  "Return an <upstream-source> for the latest release of the package PKG."

  (define upstream-name
    (package->upstream-name pkg))

  (define version
    (latest-bioconductor-package-version upstream-name))

  (and version
       ;; Bioconductor does not provide signatures.
       (upstream-source
        (package (package-name pkg))
        (version version)
        (urls (bioconductor-uri upstream-name version))
        (input-changes
         (changed-inputs
          pkg
          (cran->guix-package upstream-name 'bioconductor))))))

(define (cran-package? package)
  "Return true if PACKAGE is an R package from CRAN."
  (and (string-prefix? "r-" (package-name package))
       ;; Check if the upstream name can be extracted from package uri.
       (package->upstream-name package)
       ;; Check if package uri(s) are prefixed by "mirror://cran".
       (match (and=> (package-source package) origin-uri)
         ((? string? uri)
          (string-prefix? "mirror://cran" uri))
         ((? list? uris)
          (any (cut string-prefix? "mirror://cran" <>) uris))
         (_ #f))))

(define (bioconductor-package? package)
  "Return true if PACKAGE is an R package from Bioconductor."
  (let ((predicate (lambda (uri)
                     (and (string-prefix? "https://bioconductor.org" uri)
                          ;; Data packages are neither listed in SVN nor on
                          ;; the Github mirror, so we have to exclude them
                          ;; from the set of bioconductor packages that can be
                          ;; updated automatically.
                          (not (string-contains uri "/data/annotation/"))
                          ;; Experiment packages are in a separate repository.
                          (not (string-contains uri "/data/experiment/"))))))
    (and (string-prefix? "r-" (package-name package))
         (match (and=> (package-source package) origin-uri)
           ((? string? uri)
            (predicate uri))
           ((? list? uris)
            (any predicate uris))
           (_ #f)))))

(define (bioconductor-data-package? package)
  "Return true if PACKAGE is an R data package from Bioconductor."
  (let ((predicate (lambda (uri)
                     (and (string-prefix? "https://bioconductor.org" uri)
                          (string-contains uri "/data/annotation/")))))
    (and (string-prefix? "r-" (package-name package))
         (match (and=> (package-source package) origin-uri)
           ((? string? uri)
            (predicate uri))
           ((? list? uris)
            (any predicate uris))
           (_ #f)))))

(define (bioconductor-experiment-package? package)
  "Return true if PACKAGE is an R experiment package from Bioconductor."
  (let ((predicate (lambda (uri)
                     (and (string-prefix? "https://bioconductor.org" uri)
                          (string-contains uri "/data/experiment/")))))
    (and (string-prefix? "r-" (package-name package))
         (match (and=> (package-source package) origin-uri)
           ((? string? uri)
            (predicate uri))
           ((? list? uris)
            (any predicate uris))
           (_ #f)))))

(define %cran-updater
  (upstream-updater
   (name 'cran)
   (description "Updater for CRAN packages")
   (pred cran-package?)
   (latest latest-cran-release)))

(define %bioconductor-updater
  (upstream-updater
   (name 'bioconductor)
   (description "Updater for Bioconductor packages")
   (pred bioconductor-package?)
   (latest latest-bioconductor-release)))

;;; cran.scm ends here
