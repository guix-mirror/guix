;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages musl)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages))

(define-public musl
  (package
    (name "musl")
    (version "1.1.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.musl-libc.org/releases/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0i2z52zgc86af1n1gjiz43hgd85mxjgvgn345zsybja9dxpvchn7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; musl has no tests
       #:configure-flags
       (list "--disable-gcc-wrapper")))
    (synopsis "Small C standard library")
    (description "musl is a simple and lightweight C standard library.  It
strives to be correct in the sense of standards-conformance and safety.")
    (home-page "https://www.musl-libc.org")
    ;; Musl as a whole is released under the Expat license.  Parts of it are
    ;; derived from various third-party projects that are released under
    ;; non-copyleft licenses.  See the COPYRIGHT file for details.
    (license license:expat)))
