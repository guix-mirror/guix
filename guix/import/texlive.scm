;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix import texlive)
  #:use-module (ice-9 match)
  #:use-module (sxml simple)
  #:use-module (sxml xpath)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (web uri)
  #:use-module (guix http-client)
  #:use-module (gcrypt hash)
  #:use-module (guix memoization)
  #:use-module (guix store)
  #:use-module (guix base32)
  #:use-module (guix serialization)
  #:use-module (guix svn-download)
  #:use-module (guix import utils)
  #:use-module (guix utils)
  #:use-module (guix upstream)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix build-system texlive)
  #:export (texlive->guix-package))

;;; Commentary:
;;;
;;; Generate a package declaration template for the latest version of a
;;; package on CTAN, using the XML output produced by the XML API to the CTAN
;;; database at http://www.ctan.org/xml/1.2/
;;;
;;; Instead of taking the packages from CTAN, however, we fetch the sources
;;; from the SVN repository of the Texlive project.  We do this because CTAN
;;; only keeps a single version of each package whereas we can access any
;;; version via SVN.  Unfortunately, this means that the importer is really
;;; just a Texlive importer, not a generic CTAN importer.
;;;
;;; Code:

(define string->license
  (match-lambda
    ("artistic2" 'gpl3+)
    ("gpl" 'gpl3+)
    ("gpl1" 'gpl1)
    ("gpl1+" 'gpl1+)
    ("gpl2" 'gpl2)
    ("gpl2+" 'gpl2+)
    ("gpl3" 'gpl3)
    ("gpl3+" 'gpl3+)
    ("lgpl2.1" 'lgpl2.1)
    ("lgpl3" 'lgpl3)
    ("knuth" 'knuth)
    ("pd" 'public-domain)
    ("bsd2" 'bsd-2)
    ("bsd3" 'bsd-3)
    ("bsd4" 'bsd-4)
    ("opl" 'opl1.0+)
    ("ofl" 'silofl1.1)
    ("lppl" 'lppl)
    ("lppl1" 'lppl1.0+) ; usually means "or later"
    ("lppl1.2" 'lppl1.2+) ; usually means "or later"
    ("lppl1.3" 'lppl1.3+) ; usually means "or later"
    ("lppl1.3a" 'lppl1.3a)
    ("lppl1.3b" 'lppl1.3b)
    ("lppl1.3c" 'lppl1.3c)
    ("cc-by-2" 'cc-by-2.0)
    ("cc-by-3" 'cc-by-3.0)
    ("cc-by-sa-2" 'cc-by-sa2.0)
    ("cc-by-sa-3" 'cc-by-sa3.0)
    ("mit" 'expat)
    ("fdl" 'fdl1.3+)
    ("gfl" 'gfl1.0)

    ;; These are known non-free licenses
    ("noinfo" 'unknown)
    ("nosell" 'non-free)
    ("shareware" 'non-free)
    ("nosource" 'non-free)
    ("nocommercial" 'non-free)
    ("cc-by-nc-nd-1" 'non-free)
    ("cc-by-nc-nd-2" 'non-free)
    ("cc-by-nc-nd-2.5" 'non-free)
    ("cc-by-nc-nd-3" 'non-free)
    ("cc-by-nc-nd-4" 'non-free)
    ((x) (string->license x))
    ((lst ...) `(list ,@(map string->license lst)))
    (_ #f)))

(define (fetch-sxml name)
  "Return an sxml representation of the package information contained in the
XML description of the CTAN package or #f in case of failure."
  ;; This API always returns the latest release of the module.
  (let ((url (string-append "http://www.ctan.org/xml/1.2/pkg/" name)))
    (guard (c ((http-get-error? c)
               (format (current-error-port)
                       "error: failed to retrieve package information \
from ~s: ~a (~s)~%"
                       (uri->string (http-get-error-uri c))
                       (http-get-error-code c)
                       (http-get-error-reason c))
               #f))
      (xml->sxml (http-fetch url)
                 #:trim-whitespace? #t))))

(define (guix-name component name)
  "Return a Guix package name for a given Texlive package NAME."
  (string-append "texlive-" component "-"
                 (string-map (match-lambda
                               (#\_ #\-)
                               (#\. #\-)
                               (chr (char-downcase chr)))
                             name)))

(define* (sxml->package sxml #:optional (component "latex"))
  "Return the `package' s-expression for a Texlive package from the SXML
expression describing it."
  (define (sxml-value path)
    (match ((sxpath path) sxml)
      (() #f)
      ((val) val)))
  (with-store store
    (let* ((id         (sxml-value '(entry @ id *text*)))
           (synopsis   (sxml-value '(entry caption *text*)))
           (version    (or (sxml-value '(entry version @ number *text*))
                           (sxml-value '(entry version @ date *text*))))
           (license    (string->license (sxml-value '(entry license @ type *text*))))
           (home-page  (string-append "http://www.ctan.org/pkg/" id))
           (ref        (texlive-ref component id))
           (checkout   (download-svn-to-store store ref)))
      `(package
         (name ,(guix-name component id))
         (version ,version)
         (source (origin
                   (method svn-fetch)
                   (uri (texlive-ref ,component ,id))
                   (sha256
                    (base32
                     ,(bytevector->nix-base32-string
                       (let-values (((port get-hash) (open-sha256-port)))
                         (write-file checkout port)
                         (force-output port)
                         (get-hash)))))))
         (build-system texlive-build-system)
         (arguments ,`(,'quote (#:tex-directory ,(string-join (list component id) "/"))))
         (home-page ,home-page)
         (synopsis ,synopsis)
         (description ,(string-trim-both
                        (string-join
                         (map string-trim-both
                              (string-split
                               (beautify-description
                                (sxml->string (or (sxml-value '(entry description))
                                                  '())))
                               #\newline)))))
         (license ,license)))))

(define texlive->guix-package
  (memoize
   (lambda* (package-name #:optional (component "latex"))
     "Fetch the metadata for PACKAGE-NAME from REPO and return the `package'
s-expression corresponding to that package, or #f on failure."
     (and=> (fetch-sxml package-name)
            (cut sxml->package <> component)))))

;;; ctan.scm ends here
