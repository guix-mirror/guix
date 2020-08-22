;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
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
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system python))

(define-public python-trytond
  (package
    (name "python-trytond")
    (version "5.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond" version))
       (sha256
        (base32 "1n76ccv2x5csz80p42dav8rhzg2m14wdi3bj1pizhw8x2hxxfwx3"))))
    (build-system python-build-system)
    (inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-genshi" ,python-genshi)
       ("python-lxml" ,python-lxml)
       ("python-magic" ,python-magic)
       ("python-passlib" ,python-passlib)
       ("python-polib" ,python-polib)
       ("python-psycopg2" ,python-psycopg2)
       ("python-relatorio" ,python-relatorio)
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
             (setenv "HOME" "/tmp")
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
    (version "5.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tryton" version))
       (sha256
        (base32 "1dghr6x5wga3sizjvj261xndpl38si5hwiz3llm2bhmg33nplfh7"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'change-home
           (lambda _
             ;; Change from /homeless-shelter to /tmp for write permission.
             (setenv "HOME" "/tmp")))
         (add-after 'install 'wrap-gi-python
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH")))
               (wrap-program (string-append out "/bin/tryton")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
             #t)))))
    (native-inputs
     `(("glib-compile-schemas" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ("python-dateutil" ,python-dateutil)
       ("python-pycairo" ,python-pycairo)
       ("python-pygobject" ,python-pygobject)))
    (home-page "https://www.tryton.org/")
    (synopsis "Client component of Tryton")
    (description
     "This package is the client component of Tryton.")
    (license license:gpl3+)))

(define-public python-trytond-country
  (package
    (name "python-trytond-country")
    (version "5.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_country" version))
       (sha256
        (base32 "0k1xw5r2pfd5mvvg3pn3vavwjwpgmm5i6nsc8x421znk4gvvns78"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (let ((runtest
                  (string-append
                   (assoc-ref %build-inputs "python-trytond")
                   "/lib/python"
                   ,(version-major+minor (package-version python))
                   "/site-packages/trytond/tests/run-tests.py")))
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (add-installed-pythonpath inputs outputs)
               ;; Doctest contains one test that requires internet access.
               (invoke "python" runtest "-m" "country" "--no-doctest")))))))
    (native-inputs
     `(("python" ,python)
       ("python-dateutil" ,python-dateutil)
       ("python-genshi" ,python-genshi)
       ("python-lxml" ,python-lxml)
       ("python-magic" ,python-magic)
       ("python-passlib" ,python-passlib)
       ("python-polib" ,python-polib)
       ("python-proteus" ,python-proteus)
       ("python-relatorio" ,python-relatorio)
       ("python-sql" ,python-sql)
       ("python-werkzeug" ,python-werkzeug)
       ("python-wrapt" ,python-wrapt)))
    (propagated-inputs
     `(("python-pycountry" ,python-pycountry)
       ("python-trytond" ,python-trytond)))
    (home-page "http://www.tryton.org/")
    (synopsis "Tryton module with countries")
    (description
     "This package provides a Tryton module with countries.")
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
    (version "5.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "proteus" version))
       (sha256
        (base32 "0kxac5pkps243wf0xbmbd1g5bml96xl94j88y6yyzm093vyli150"))))
    (build-system python-build-system)
    ;; Tests require python-trytond-party which requires python-proteus.
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)))
    (home-page "http://www.tryton.org/")
    (synopsis "Library to access a Tryton server as a client")
    (description
     "This package provides a library to access Tryton server as a client.")
    (license license:lgpl3+)))
