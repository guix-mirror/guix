;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages logging)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages))

(define-public log4cpp
  (package
    (name "log4cpp")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/log4cpp/log4cpp-"
                                  (version-major+minor version) ".x%20%28new%29"
                                  "/log4cpp-" (version-major+minor version)
                                  "/log4cpp-" version ".tar.gz"))
              (sha256
               (base32
                "1l5yz5rfzzv6g3ynrj14mxfsk08cp5h1ssr7d74hjs0accrg7arm"))))
    (build-system gnu-build-system)
    (synopsis "Log library for C++")
    (description
     "Log4cpp is library of C++ classes for flexible logging to files, syslog,
IDSA and other destinations.  It is modeled after the Log4j Java library,
staying as close to their API as is reasonable.")
    (home-page "http://log4cpp.sourceforge.net/")
    (license license:lgpl2.1+)))
