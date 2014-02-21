;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages search)
  #:use-module ((guix licenses)
                #:select (gpl2+ bsd-3 x11))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:export (xapian))

(define-public xapian
  (package
    (name "xapian")
    (version "1.2.17")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://oligarchy.co.uk/xapian/" version
                                  "/xapian-core-" version ".tar.xz"))
              (sha256
               (base32 "1pn65h06c23imck2pb42zhrrngch3clk39wl2bjwyqhfyfq4b7g7"))))
    (build-system gnu-build-system)
    (inputs `(("zlib" ,zlib)
              ("util-linux" ,util-linux)))
    (arguments
     `(#:phases (alist-cons-after
                 'unpack 'patch-remotetcp-harness
                 (lambda _
                   (substitute* "tests/harness/backendmanager_remotetcp.cc"
                     (("/bin/sh") (which "bash"))))
                 %standard-phases)))
    (synopsis "Search Engine Library")
    (description
     "Xapian is a highly adaptable toolkit which allows developers to easily
add advanced indexing and search facilities to their own applications.  It
supports the Probabilistic Information Retrieval model and also supports a
rich set of boolean query operators.")
    (home-page "http://xapian.org/")
    (license (list gpl2+ bsd-3 x11))))

;;; search.scm ends here
