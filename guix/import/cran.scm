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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (sxml simple)
  #:use-module (sxml match)
  #:use-module (sxml xpath)
  #:use-module (guix http-client)
  #:use-module (guix hash)
  #:use-module (guix store)
  #:use-module (guix base32)
  #:use-module ((guix download) #:select (download-to-store))
  #:use-module (guix import utils)
  #:use-module (guix upstream)
  #:use-module (guix packages)
  #:export (cran->guix-package
            %cran-updater))

;;; Commentary:
;;;
;;; Generate a package declaration template for the latest version of an R
;;; package on CRAN, using the HTML description downloaded from
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
   ("GPL-2" 'gpl2+)
   ("GPL-3" 'gpl3+)
   ("LGPL-2" 'lgpl2.0+)
   ("LGPL-2.1" 'lgpl2.1+)
   ("LGPL-3" 'lgpl3+)
   ("MIT" 'x11)
   ((x) (string->license x))
   ((lst ...) `(list ,@(map string->license lst)))
   (_ #f)))

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

(define (table-datum tree label)
  "Extract the datum node following a LABEL in the sxml table TREE.  Only the
first cell of a table row is considered a label cell."
  ((node-pos 1)
   ((sxpath `(xhtml:tr
              (xhtml:td 1)        ; only first cell can contain label
              (equal? ,label)
              ,(node-parent tree) ; go up to label cell
              ,(node-parent tree) ; go up to matching row
              (xhtml:td 2)))      ; select second cell
    tree)))

(define %cran-url "http://cran.r-project.org/web/packages/")

(define (cran-fetch name)
  "Return an sxml representation of the CRAN page for the R package NAME,
or #f on failure.  NAME is case-sensitive."
  ;; This API always returns the latest release of the module.
  (let ((cran-url (string-append %cran-url name "/")))
    (false-if-exception
     (xml->sxml (http-fetch cran-url)
                #:trim-whitespace? #t
                #:namespaces '((xhtml . "http://www.w3.org/1999/xhtml"))
                #:default-entity-handler
                (lambda (port name)
                  (case name
                    ((nbsp) " ")
                    ((ge) ">=")
                    ((gt) ">")
                    ((lt) "<")
                    (else
                     (format (current-warning-port)
                             "~a:~a:~a: undefined entitity: ~a\n"
                             cran-url (port-line port) (port-column port)
                             name)
                     (symbol->string name))))))))

(define (downloads->url downloads)
  "Extract from DOWNLOADS, the downloads item of the CRAN sxml tree, the
download URL."
  (string-append "mirror://cran/"
                 ;; Remove double dots, because we want an
                 ;; absolute path.
                 (regexp-substitute/global
                  #f "\\.\\./"
                  (string-join ((sxpath '((xhtml:a 1) @ href *text*))
                                (table-datum downloads " Package source: ")))
                  'pre 'post)))

(define (nodes->text nodeset)
  "Return the concatenation of the text nodes among NODESET."
  (string-join ((sxpath '(// *text*)) nodeset) " "))

(define (cran-sxml->sexp sxml)
  "Return the `package' s-expression for a CRAN package from the SXML
representation of the package page."
  (define (guix-name name)
    (if (string-prefix? "r-" name)
        (string-downcase name)
        (string-append "r-" (string-downcase name))))

  (sxml-match-let*
   (((*TOP* (xhtml:html
             ,head
             (xhtml:body
              (xhtml:h2 ,name-and-synopsis)
              (xhtml:p ,description)
              ,summary
              (xhtml:h4 "Downloads:") ,downloads
              . ,rest)))
     sxml))
   (let* ((name       (match:prefix (string-match ": " name-and-synopsis)))
          (synopsis   (match:suffix (string-match ": " name-and-synopsis)))
          (version    (nodes->text (table-datum summary "Version:")))
          (license    ((compose string->license nodes->text)
                       (table-datum summary "License:")))
          (home-page  (nodes->text ((sxpath '((xhtml:a 1)))
                                    (table-datum summary "URL:"))))
          (source-url (downloads->url downloads))
          (tarball    (with-store store (download-to-store store source-url)))
          (sysdepends (map match:substring
                           (list-matches
                            "[^ ]+"
                            ;; Strip off comma and parenthetical
                            ;; expressions.
                            (regexp-substitute/global
                             #f "(,|\\([^\\)]+\\))"
                             (nodes->text (table-datum summary
                                                       "SystemRequirements:"))
                             'pre 'post))))
          (imports    (map guix-name
                           ((sxpath '(// xhtml:a *text*))
                            (table-datum summary "Imports:")))))
     `(package
        (name ,(guix-name name))
        (version ,version)
        (source (origin
                  (method url-fetch)
                  (uri (cran-uri ,name version))
                  (sha256
                   (base32
                    ,(bytevector->nix-base32-string (file-sha256 tarball))))))
        (build-system r-build-system)
        ,@(maybe-inputs sysdepends)
        ,@(maybe-inputs imports 'propagated-inputs)
        (home-page ,(if (string-null? home-page)
                        (string-append %cran-url name)
                        home-page))
        (synopsis ,synopsis)
        ;; Use double spacing
        (description ,(regexp-substitute/global #f "\\. \\b" description
                                                'pre ".  " 'post))
        (license ,license)))))

(define (cran->guix-package package-name)
  "Fetch the metadata for PACKAGE-NAME from cran.r-project.org, and return the
`package' s-expression corresponding to that package, or #f on failure."
  (let ((module-meta (cran-fetch package-name)))
    (and=> module-meta cran-sxml->sexp)))


;;;
;;; Updater.
;;;

(define (latest-release package)
  "Return an <upstream-source> for the latest release of PACKAGE."
  (define name
    (if (string-prefix? "r-" package)
        (string-drop package 2)
        package))

  (define sxml
    (cran-fetch name))

  (and sxml
       (sxml-match-let*
        (((*TOP* (xhtml:html
                  ,head
                  (xhtml:body
                   (xhtml:h2 ,name-and-synopsis)
                   (xhtml:p ,description)
                   ,summary
                   (xhtml:h4 "Downloads:") ,downloads
                   . ,rest)))
          sxml))
        (let ((version (nodes->text (table-datum summary "Version:")))
              (url     (downloads->url downloads)))
          ;; CRAN does not provide signatures.
          (upstream-source
           (package package)
           (version version)
           (urls (list url)))))))

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
