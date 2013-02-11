;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages popt)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses))

(define-public popt
  (package
    (name "popt")
    (version "1.16")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://rpm5.org/files/popt/popt-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1j2c61nn2n351nhj4d25mnf3vpiddcykq005w2h6kw79dwlysa77"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-before
                 'configure 'patch-test
                 (lambda _
                   (substitute* "test-poptrc.in"
                     (("/bin/echo") (which "echo")))
                   (substitute* "testit.sh"   ; don't expect old libtool names
                     (("lt-test1") "test1")))
                 %standard-phases)))
    (home-page #f)
    (synopsis "Command line option parsing library")
    (description
     "This is the popt(3) command line option parsing library.  While it is
similar to getopt(3), it contains a number of enhancements, including:

  - popt is fully reentrant;

  - popt can parse arbitrary argv[] style arrays while getopt(3) makes this
    quite difficult;

  - popt allows users to alias command line arguments;

  - popt provides convience functions for parsing strings into argv[] style
    arrays.")
    (license x11)))
