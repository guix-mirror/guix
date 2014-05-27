;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages backup)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages rsync)
  #:use-module (srfi srfi-1))

(define-public duplicity
  (package
    (name "duplicity")
    (version "0.6.24")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://code.launchpad.net/duplicity/"
                          (string-join (take (string-split version #\.) 2) ".")
                          "-series/" version "/+download/duplicity-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0l14nrhbgkyjgvh339bbhnm6hrdwrjadphq1jmpi0mcgcdbdfh8x"))))
    (build-system python-build-system)
    (native-inputs
     `(("python2-setuptools" ,python2-setuptools)))
    (inputs
     `(("python" ,python-2)
       ("librsync" ,librsync)
       ("mock" ,python2-mock)           ;for testing
       ("lockfile" ,python2-lockfile)
       ("gnupg" ,gnupg-1)))             ;gpg executable needed
    (arguments
     `(#:python ,python-2               ;setup assumes Python 2
       #:test-target "test"
       #:phases (alist-cons-before
                 'check 'patch-tests
                 (lambda _
                   (substitute* "testing/functional/__init__.py"
                     (("/bin/sh") (which "sh"))))
                 %standard-phases)))
    (home-page "http://duplicity.nongnu.org/index.html")
    (synopsis "Encrypted backup using rsync algorithm")
    (description
     "Duplicity backs up directories by producing encrypted tar-format volumes
and uploading them to a remote or local file server.  Because duplicity uses
librsync, the incremental archives are space efficient and only record the
parts of files that have changed since the last backup.  Because duplicity
uses GnuPG to encrypt and/or sign these archives, they will be safe from
spying and/or modification by the server.")
    (license gpl2+)))
