;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module ((guix diagnostics) #:select (formatted-message))
  #:use-module (guix gnu-maintenance)
  #:use-module (guix import utils)
  #:use-module (guix i18n)
  #:use-module (guix utils)
  #:use-module (guix store)
  #:use-module (gcrypt hash)
  #:use-module (guix base32)
  #:use-module (guix upstream)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (web uri)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (gnu->guix-package))

;;; Commentary:
;;;
;;; Generate a package declaration template for the latest version of a GNU
;;; package, using meta-data available upstream for the package.
;;;
;;; Code:

(define (qualified-url url)
  "Return a fully-qualified URL based on URL."
  (if (string-prefix? "/" url)
      (string-append "http://www.gnu.org" url)
      url))

(define (preferred-archive-type release)
  "Return the preferred type of archive for downloading RELEASE."
  (find (cute member <> (upstream-source-archive-types release))
        '("xz" "lz" "bz2" "tbz2" "gz" "tgz" "Z")))

(define* (gnu-package->sexp package release
                            #:key (key-download 'interactive))
  "Return the 'package' sexp for the RELEASE (a <gnu-release>) of PACKAGE (a
<gnu-package>), or #f upon failure.  Use KEY-DOWNLOAD as the OpenPGP key
download policy (see 'download-tarball' for details.)"
  (define name
    (gnu-package-name package))

  (define url-base
    ;; XXX: We assume that RELEASE's directory starts with "/gnu".
    (string-append "mirror:/"
                   (match (upstream-source-urls release)
                     ((url rest ...)
                      (dirname (uri-path (string->uri url)))))
                   "/" name "-"))

  (define archive-type
    (preferred-archive-type release))

  (define url
    (find (cut string-suffix? archive-type <>)
          (upstream-source-urls release)))

  (define sig-url
    (find (cute string-suffix? (string-append archive-type ".sig") <>)
          (upstream-source-signature-urls release)))

  (with-store store
    (match (download-tarball store url sig-url
                             #:key-download key-download)
      ((? string? tarball)
       `(package
          (name ,name)
          (version ,(upstream-source-version release))
          (source (origin
                    (method url-fetch)
                    (uri (string-append ,url-base version
                                        ,(string-append ".tar." archive-type)))
                    (sha256
                     (base32
                      ,(bytevector->nix-base32-string
                        (file-sha256 tarball))))))
          (build-system gnu-build-system)
          (synopsis ,(gnu-package-doc-summary package))
          (description ,(beautify-description
                         (gnu-package-doc-description package)))
          (home-page ,(match (gnu-package-doc-urls package)
                        ((head . tail) (qualified-url head))))
          (license find-by-yourself!)))
      (#f                     ;failure to download or authenticate the tarball
       #f))))

(define* (gnu->guix-package name
                            #:key (key-download 'interactive))
  "Return the package declaration for NAME as an s-expression.  Use
KEY-DOWNLOAD as the OpenPGP key download policy (see 'download-tarball' for
details.)"
  (let ((package (find-package name)))
    (unless package
      (raise (formatted-message (G_ "no GNU package found for ~a") name)))

    (match (latest-release name)
      ((? upstream-source? release)
       (let ((version (upstream-source-version release)))
         (gnu-package->sexp package release #:key-download key-download)))
      (_
       (raise (formatted-message
               (G_ "failed to determine latest release of GNU ~a")
               name))))))

;;; gnu.scm ends here
