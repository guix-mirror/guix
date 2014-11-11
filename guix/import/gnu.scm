;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix import gnu)
  #:use-module (guix gnu-maintenance)
  #:use-module (guix utils)
  #:use-module (guix store)
  #:use-module (guix hash)
  #:use-module (guix base32)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (gnu->guix-package))

;;; Commentary:
;;;
;;; Generate a package declaration template for the latest version of a GNU
;;; package, using meta-data available upstream for the package.
;;;
;;; Code:

(define (file-sha256 file)
  "Return the SHA256 hash of FILE as a bytevector."
  (call-with-input-file file port-sha256))

(define (qualified-url url)
  "Return a fully-qualified URL based on URL."
  (if (string-prefix? "/" url)
      (string-append "http://www.gnu.org" url)
      url))

(define (preferred-archive-type release)
  "Return the preferred type of archive for downloading RELEASE."
  (find (cute member <> (gnu-release-archive-types release))
        '("xz" "lz" "bz2" "tbz2" "gz" "tgz" "Z")))

(define* (gnu-package->sexp package release
                            #:key (key-download 'interactive))
  "Return the 'package' sexp for the RELEASE (a <gnu-release>) of PACKAGE (a
<gnu-package>).  Use KEY-DOWNLOAD as the OpenPGP key download policy (see
'download-tarball' for details.)"
  (define name
    (gnu-package-name package))

  (define url-base
    ;; XXX: We assume that RELEASE's directory starts with "/gnu".
    (string-append "mirror:/" (gnu-release-directory release)
                   "/" name "-"))

  (define archive-type
    (preferred-archive-type release))

  (let ((tarball (with-store store
                   (download-tarball store name
                                     (gnu-release-directory release)
                                     (gnu-release-version release)
                                     #:archive-type archive-type
                                     #:key-download key-download))))
    `(package
       (name ,name)
       (version ,(gnu-release-version release))
       (source (origin
                 (method url-fetch)
                 (uri (string-append ,url-base version
                                     ,(string-append ".tar." archive-type)))
                 (sha256
                  (base32
                   ,(bytevector->base32-string (file-sha256 tarball))))))
       (build-system gnu-build-system)
       (synopsis ,(gnu-package-doc-summary package))
       (description ,(gnu-package-doc-description package))
       (home-page ,(match (gnu-package-doc-urls package)
                     ((head . tail) (qualified-url head))))
       (license find-by-yourself!))))

(define* (gnu->guix-package name
                            #:key (key-download 'interactive))
  "Return the package declaration for NAME as an s-expression.  Use
KEY-DOWNLOAD as the OpenPGP key download policy (see 'download-tarball' for
details.)"
  (match (latest-release name)
    ((? gnu-release? release)
     (let ((version (gnu-release-version release)))
       (match (find-packages (regexp-quote name))
         ((info . _)
          (gnu-package->sexp info release))
         (()
          (raise (condition
                  (&message
                   (message "couldn't find meta-data for GNU package"))))))))
    (_
     (raise (condition
             (&message
              (message
               "failed to determine latest release of GNU package")))))))

;;; gnu.scm ends here
