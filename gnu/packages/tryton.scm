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
  #:use-module (gnu packages finance)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python))

(define-public python-trytond
  (package
    (name "python-trytond")
    (version "4.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond" version))
       (sha256
        (base32
         "0asc3pd37h8ky8j66iqxr0fv0k6mpjcwxwm0xgm5hrdi32l5cdda"))))
    (build-system python-build-system)
    (inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-genshi" ,python-genshi)
       ("python-polib" ,python-polib)
       ("python-magic" ,python-magic)
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
             (setenv "DB_NAME" ":memory:")
             #t)))))
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
    (version "4.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tryton" version))
       (sha256
        (base32
         "0bamr040np02gfjk8c734rw3mbgg75irfgpdcl2npgkdzyw1ksf9"))))
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

(define-public python-trytond-country
  (package
  (name "python-trytond-country")
  (version "4.6.0")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "trytond_country" version))
      (sha256
        (base32
          "11c9mw2scbjn7c6yhlwh5ml266f0s31lh4jwj6gh7vl1shs3isr3"))))
  (build-system python-build-system)
  (arguments
   `(#:phases
     (modify-phases %standard-phases
       (add-before 'check 'preparations
         (lambda _
           (setenv "DB_NAME" ":memory:")
           #t)))))
  (propagated-inputs
   `(("python-trytond" ,python-trytond)
     ("python-wrapt" ,python-wrapt)
     ("python-werkzeug" ,python-werkzeug)
     ("python-sql" ,python-sql)
     ("python-polib" ,python-polib)
     ("python-dateutil" ,python-dateutil)
     ("python-genshi" ,python-genshi)
     ("python-relatorio" ,python-relatorio)
     ("python-magic" ,python-magic)))
  (home-page "http://www.tryton.org/")
  (synopsis "Tryton module with countries")
  (description "This package provides a Tryton module with countries.")
  (license license:gpl3+)))

(define-public python-trytond-party
  (package
    (name "python-trytond-party")
    (version "4.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_party" version))
       (sha256
        (base32
         "0fbf4kxywiglcdsx9ppjg7nxw87915mb6bpn1jn652gk949rdww5"))))
    (build-system python-build-system)
    ;; XXX The tests seem to require Proteus. But Proteus tests seem to
    ;; require trytond-party.
    (arguments
     '(#:tests? #f))
    (propagated-inputs
     `(("python-trytond" ,python-trytond)
       ("python-trytond-country" ,python-trytond-country)
       ("python-stdnum" ,python-stdnum)
       ("python-sql" ,python-sql)
       ("python-wrapt" ,python-wrapt)
       ("python-werkzeug" ,python-werkzeug)
       ("python-polib" ,python-polib)
       ("python-dateutil" ,python-dateutil)
       ("python-genshi" ,python-genshi)
       ("python-relatorio" ,python-relatorio)
       ("python-magic" ,python-magic)
       ("python-phonenumbers" ,python-phonenumbers)))
    (home-page "http://www.tryton.org/")
    (synopsis
     "Tryton module for parties and addresses")
    (description
     "This package provides a Tryton module for (counter)parties and
addresses.")
    (license license:gpl3+)))

(define-public python-proteus
  (package
    (name "python-proteus")
    (version "4.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "proteus" version))
       (sha256
        (base32
         "0flkf1vxbhz51b7bq31dn7q9mlkli3pmpbzfhsxfqpf6laghbkqg"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-trytond-party" ,python-trytond-party)
       ("python-trytond-country" ,python-trytond-country)
       ("python-trytond" ,python-trytond)
       ("python-stdnum" ,python-stdnum)
       ("python-sql" ,python-sql)
       ("python-wrapt" ,python-wrapt)
       ("python-werkzeug" ,python-werkzeug)
       ("python-polib" ,python-polib)
       ("python-genshi" ,python-genshi)
       ("python-relatorio" ,python-relatorio)
       ("python-magic" ,python-magic)))
    (home-page "http://www.tryton.org/")
    (synopsis
     "Library to access a Tryton server as a client")
    (description
     "This package provides a library to access Tryton server as a client.")
    (license license:lgpl3+)))
