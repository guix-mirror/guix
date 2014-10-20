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

(define-module (gnu packages ccache)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages compression))

(define-public ccache
  (package
    (name "ccache")
    (version "3.1.10")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.samba.org/ftp/ccache/ccache-"
                          version ".tar.xz"))
      (sha256
       (base32
        "0mr8n1nbykxw4rs55ad8wd6xmfhihn09mxpxb91sn9mlsd1ryhw8"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)))   ;for test.sh
    (inputs `(("zlib" ,zlib)))
    (arguments
     '(#:phases (alist-cons-before
                 'check 'patch-test-shebangs
                 (lambda _
                   (substitute* '("test/test_hashutil.c" "test.sh")
                     (("#!/bin/sh") (string-append "#!" (which "sh")))))
                 %standard-phases)))
    (home-page "https://ccache.samba.org/")
    (synopsis "Compiler cache")
    (description
     "Ccache is a compiler cache.  It speeds up recompilation by caching
previous compilations and detecting when the same compilation is being done
again.  Supported languages are C, C++, Objective-C and Objective-C++.")
    (license gpl3+)))
