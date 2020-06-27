;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
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

(define-module (gnu packages maven-parent-pom)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages java))

(define (make-apache-parent-pom version hash)
  (hidden-package
    (package
      (name "apache-parent-pom")
      (version version)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/apache/maven-apache-parent")
                       (commit (string-append "apache-" version))))
                (file-name (git-file-name name version))
                (sha256 (base32 hash))))
      (build-system ant-build-system)
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (install-pom-file "pom.xml")))))
      (home-page "https://apache.org/")
      (synopsis "Apache parent pom")
      (description "This package contains the Apache parent POM.")
      (license license:asl2.0))))

(define-public apache-parent-pom-6
  (make-apache-parent-pom
    "6" "1bq0ma2ya2cnp2icd4l20sv6y7zxqr9sa35wzv1s49nqsrm38kw3"))

(define-public apache-parent-pom-11
  (make-apache-parent-pom
    "11" "0m1a4db8s6y8f4vvm9bx7zx7lixcvaah064560nbja7na3xz6lls"))

(define-public apache-parent-pom-13
  (make-apache-parent-pom
    "13" "1cfxaz1jy8fbn06sb648qpiq23swpbj3kb5ya7f9g9jmya5fy09z"))

(define-public apache-parent-pom-16
  (make-apache-parent-pom
    "16" "1y5b0dlc72ijcqfffdbh0k75qwaddy5qw725v9pzhrzqkpaa51xb"))

(define-public apache-parent-pom-17
  (make-apache-parent-pom
    "17" "06hj5d6rdkmwl24k2rvzj8plq8x1ncsbjck4w3awz1hp9gngg4y5"))

(define-public apache-parent-pom-18
  (make-apache-parent-pom
    "18" "1il97vpdmv5k2gnyinj45q00f7f4w9hcb588digwfid5bskddnyy"))

(define-public apache-parent-pom-19
  (make-apache-parent-pom
    "19" "02drnwv2qqk1dmxbmmrk0bi1iil5cal9l47w53ascpbjg6242mp1"))

(define-public apache-parent-pom-21
  (make-apache-parent-pom
    "21" "0clcbrq1b2b8sbvlqddyw2dg5niq25dhdma9sk4b0i30hqaipx96"))

(define (make-apache-commons-parent-pom version hash parent)
  (hidden-package
    (package
      (name "apache-commons-parent-pom")
      (version version)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/apache/commons-parent")
                       (commit (string-append "commons-parent-" version))))
                (file-name (git-file-name name version))
                (sha256 (base32 hash))))
      (build-system ant-build-system)
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (install-pom-file "pom.xml")))))
      (propagated-inputs
        (if parent
            `(("parent" ,parent))
            '()))
      (home-page "https://maven.apache.org/")
      (synopsis "Apache Commons parent pom")
      (description "This package contains the Apache Commons parent POM.")
      (license license:asl2.0))))

(define-public apache-commons-parent-pom-39
  (make-apache-commons-parent-pom
    "39" "0mjx48a55ik1h4hsxhifkli1flvkp6d05ab14p4al0fc6rhdxi46"
    apache-parent-pom-16))
