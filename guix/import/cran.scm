;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module ((ice-9 rdelim) #:select (read-string))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (guix http-client)
  #:use-module (guix hash)
  #:use-module (guix store)
  #:use-module (guix base32)
  #:use-module ((guix download) #:select (download-to-store))
  #:use-module (guix import utils)
  #:use-module ((guix build-system r) #:select (cran-uri))
  #:use-module (guix upstream)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:export (cran->guix-package
            %cran-updater))

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
   ("BSD_3_clause" 'bsd-3)
   ("GPL" (list 'gpl2+ 'gpl3+))
   ("GPL (>= 2)" 'gpl2+)
   ("GPL (>= 3)" 'gpl3+)
   ("GPL-2" 'gpl2+)
   ("GPL-3" 'gpl3+)
   ("LGPL-2" 'lgpl2.0+)
   ("LGPL-2.1" 'lgpl2.1+)
   ("LGPL-3" 'lgpl3+)
   ("LGPL (>= 2)" 'lgpl2.0+)
   ("LGPL (>= 3)" 'lgpl3+)
   ("MIT" 'x11)
   ("MIT + file LICENSE" 'x11)
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

(define %cran-url "http://cran.r-project.org/web/packages/")

(define (cran-fetch name)
  "Return an alist of the contents of the DESCRIPTION file for the R package
NAME, or #f on failure.  NAME is case-sensitive."
  ;; This API always returns the latest release of the module.
  (let ((url (string-append %cran-url name "/DESCRIPTION")))
    (description->alist (read-string (http-fetch url)))))

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

(define (beautify-description description)
  "Improve the package DESCRIPTION by turning a beginning sentence fragment
into a proper sentence and by using two spaces between sentences."
  (let ((cleaned (if (string-prefix? "A " description)
                     (string-append "This package provides a"
                                    (substring description 1))
                     description)))
    ;; Use double spacing between sentences
    (regexp-substitute/global #f "\\. \\b"
                              cleaned 'pre ".  " 'post)))

(define (description->package meta)
  "Return the `package' s-expression for a CRAN package from the alist META,
which was derived from the R package's DESCRIPTION file."
  (define (guix-name name)
    (if (string-prefix? "r-" name)
        (string-downcase name)
        (string-append "r-" (string-downcase name))))

  (let* ((name       (assoc-ref meta "Package"))
         (synopsis   (assoc-ref meta "Title"))
         (version    (assoc-ref meta "Version"))
         (license    (string->license (assoc-ref meta "License")))
         ;; Some packages have multiple home pages.  Some have none.
         (home-page  (match (listify meta "URL")
                       ((url rest ...) url)
                       (_ (string-append %cran-url name))))
         (source-url (match (cran-uri name version)
                       ((url rest ...) url)
                       (_ #f)))
         (tarball    (with-store store (download-to-store store source-url)))
         (sysdepends (map string-downcase (listify meta "SystemRequirements")))
         (propagate  (map guix-name (lset-union equal?
                                                (listify meta "Imports")
                                                (listify meta "LinkingTo")
                                                (delete "R"
                                                        (listify meta "Depends"))))))
    `(package
       (name ,(guix-name name))
       (version ,version)
       (source (origin
                 (method url-fetch)
                 (uri (cran-uri ,name version))
                 (sha256
                  (base32
                   ,(bytevector->nix-base32-string (file-sha256 tarball))))))
       (properties ,`(,'quasiquote ((,'upstream-name . ,name))))
       (build-system r-build-system)
       ,@(maybe-inputs sysdepends)
       ,@(maybe-inputs propagate 'propagated-inputs)
       (home-page ,(if (string-null? home-page)
                       (string-append %cran-url name)
                       home-page))
       (synopsis ,synopsis)
       (description ,(beautify-description (assoc-ref meta "Description")))
       (license ,license))))

(define (cran->guix-package package-name)
  "Fetch the metadata for PACKAGE-NAME from cran.r-project.org, and return the
`package' s-expression corresponding to that package, or #f on failure."
  (let ((module-meta (cran-fetch package-name)))
    (and=> module-meta description->package)))


;;;
;;; Updater.
;;;

(define (latest-release package)
  "Return an <upstream-source> for the latest release of PACKAGE."

  (define (package->cran-name package)
    (match (package-source package)
      ((? origin? origin)
       (match (origin-uri origin)
         ((url rest ...)
          (let ((end   (string-rindex url #\_))
                (start (string-rindex url #\/)))
            ;; The URL ends on
            ;; (string-append "/" name "_" version ".tar.gz")
            (substring url start end)))
         (_ #f)))
    (_ #f)))

  (define cran-name
    (package->cran-name (specification->package package)))

  (define meta
    (cran-fetch cran-name))

  (and meta
       (let ((version (assoc-ref meta "Version")))
         ;; CRAN does not provide signatures.
         (upstream-source
          (package package)
          (version version)
          (urls (cran-uri cran-name version))))))

(define (cran-package? package)
  "Return true if PACKAGE is an R package from CRAN."
  ;; Assume all R packages are available on CRAN.
  (string-prefix? "r-" (package-name package)))

(define %cran-updater
  (upstream-updater
   (name 'cran)
   (description "Updater for CRAN packages")
   (pred cran-package?)
   (latest latest-release)))

;;; cran.scm ends here
