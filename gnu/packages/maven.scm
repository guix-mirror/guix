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

(define-public maven-resolver-spi
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-spi")
    (arguments
     `(#:jar-name "maven-resolver-spi.jar"
       #:source-dir "maven-resolver-spi/src/main/java"
       #:test-dir "maven-resolver-spi/src/test"
       #:jdk ,icedtea-8))
    (inputs
     `(("maven-resolver-api" ,maven-resolver-api)))
    (synopsis "Maven repository system SPI")
    (description "This package contains the service provider interface (SPI)
for repository system implementations and repository connectors.")))

(define-public maven-resolver-test-util
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-test-util")
    (arguments
     `(#:jar-name "maven-resolver-test-util.jar"
       #:source-dir "maven-resolver-test-util/src/main/java"
       #:test-dir "maven-resolver-test-util/src/test"
       #:jdk ,icedtea-8))
    (inputs
     `(("maven-resolver-api" ,maven-resolver-api)
       ("maven-resolver-spi" ,maven-resolver-spi)))
    (synopsis "Utility classes for testing the maven repository system")
    (description "This package contains a collection of utility classes to
ease testing of the repository system.")))

(define-public maven-resolver-util
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-util")
    (arguments
     `(#:jar-name "maven-resolver-util.jar"
       #:source-dir "maven-resolver-util/src/main/java"
       #:test-dir "maven-resolver-util/src/test"
       #:jdk ,icedtea-8))
    (inputs
     `(("maven-resolver-api" ,maven-resolver-api)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("maven-resolver-test-util" ,maven-resolver-test-util)))
    (synopsis "Utility classes for the maven repository system")
    (description "This package contains a collection of utility classes to
ease usage of the repository system.")))

(define-public maven-resolver-connector-basic
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-connector-basic")
    (arguments
     `(#:jar-name "maven-resolver-connector-basic.jar"
       #:source-dir "maven-resolver-connector-basic/src/main/java"
       #:test-dir "maven-resolver-connector-basic/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda _
                 (display "org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory\n"))))))))
    (inputs
     `(("maven-resolver-api" ,maven-resolver-api)
       ("maven-resolver-spi" ,maven-resolver-spi)
       ("maven-resolver-util" ,maven-resolver-util)
       ("java-javax-inject" ,java-javax-inject)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("maven-resolver-test-util" ,maven-resolver-test-util)))
    (synopsis "Maven repository connector implementation")
    (description "This package contains a repository connector implementation
for repositories using URI-based layouts.")))
