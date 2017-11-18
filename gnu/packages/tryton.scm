;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Adriano Peluso <catonano@gmail.com>
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

(define-module (gnu packages tryton)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages time)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python))

(define-public trytond
  (package
    (name "trytond")
    (version "4.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://downloads.tryton.org/4.4/trytond-"
             version ".tar.gz"))
       (sha256
        (base32
         "15gm34qwj5fpnkqvrxzndl8653zbczhsa76dm1gi4cqj1r29bbpr"))))
    (build-system python-build-system)
    (inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-genshi" ,python-genshi)
       ("python-polib" ,python-polib)
       ;; there's no python-mysql in Guix right now
       ;; so python-psycopg2 (postgresql) only for now
       ("python-psycopg2" ,python-psycopg2)
       ("python-relatorio" ,python-relatorio)
       ("python-lxml" ,python-lxml)
       ("python-sql" ,python-sql)
       ("python-werkzeug" ,python-werkzeug)
       ("python-wrapt" ,python-wrapt)))
    (native-inputs
     `(("python-mock" ,python-mock)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'preparations
           (lambda _
             (setenv "DB_NAME" ":memory:"))))))
    (home-page "https://www.tryton.org/")
    (synopsis "Server component of Tryton")
    (description "Tryton is a three-tier high-level general purpose
application platform using PostgreSQL as its main database engine.  It is the
core base of a complete business solution providing modularity, scalability
and security.")
    (license license:gpl3+)))

(define-public tryton
  (package
    (name "tryton")
    (version "4.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://downloads.tryton.org/4.4/tryton-"
             version ".tar.gz"))
       (sha256
        (base32
         "1lklcz5fs6rkrd7z2m2f5gz4fdwzkgnhg2hyvzp20kdsvi33bq2j"))))
    (build-system python-build-system)
    (inputs
     `(("python2-chardet" ,python2-chardet)
       ("python2-dateutil" ,python2-dateutil)
       ("python2-pygtk" ,python2-pygtk)))
    (arguments
     `(#:python ,python-2))
    (home-page "https://www.tryton.org/")
    (synopsis "Client component of Tryton")
    (description "This package is the client component of Tryton.")
    (license license:gpl3+)))
