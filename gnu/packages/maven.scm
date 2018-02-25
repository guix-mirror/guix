;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
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

(define-module (gnu packages maven)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages)
  #:use-module (gnu packages java))

(define-public maven-resolver-api
  (package
    (name "maven-resolver-api")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/apache/maven-resolver/"
                                  "archive/maven-resolver-" version ".tar.gz"))
              (sha256
               (base32
                "0rpvdg3qr1j88gw0ankf0wnwfyq6238mdlm7s39vf5jrcvhdgwcl"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-resolver-api.jar"
       #:source-dir "maven-resolver-api/src/main/java"
       #:test-dir "maven-resolver-api/src/test"))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "https://github.com/apache/maven-resolver")
    (synopsis "Maven repository system API")
    (description "This package contains the API for the maven repository system.")
    (license license:asl2.0)))
