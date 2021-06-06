;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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

(define-public trytond
  (package
    (name "trytond")
    (version "5.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond" version))
       (sha256
        (base32 "1h1x0cmmmxvjclbglvvxkv634jw6av5ilymbix1lln5lq0gd39yy"))))
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
    (synopsis "Tryton Server")
    (description "Tryton is a three-tier high-level general purpose
application platform using PostgreSQL as its main database engine.  It is the
core base of a complete business solution providing modularity, scalability
and security.")
    (license license:gpl3+)))

(define-public python-trytond
  (deprecated-package "python-trytond" trytond))

(define-public tryton
  (package
    (name "tryton")
    (version "5.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tryton" version))
       (sha256
        (base32 "0kr5ngmmldgb9a9d5ylkmppy5p8vlf9d8iwv9lnci2fyxg2705wh"))))
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
    (synopsis "Tryton Client")
    (description
     "This package provides the Tryton GTK client.")
    (license license:gpl3+)))

(define-public python-proteus
  (package
    (name "python-proteus")
    (version "5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "proteus" version))
       (sha256
        (base32 "03z5ssvjcvxv1p10y7c1y0jah0k3yyc9hlyi7xax98sfqyk13bnw"))))
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

(define (tryton-phases module . extra-arguments)
  "Return the phases for building and testing a Tryton module named MODULE.
If present, pass EXTRA-ARGUMENTS to runtest as well."
  `(modify-phases %standard-phases
     (replace 'check
       (lambda* (#:key inputs outputs tests? #:allow-other-keys)
         (let ((runtest
                (string-append
                 (assoc-ref inputs "trytond")
                 "/lib/python"
                 ,(version-major+minor (package-version python))
                 "/site-packages/trytond/tests/run-tests.py")))
           (when tests?
             (add-installed-pythonpath inputs outputs)
             (invoke "python" runtest "-m" ,module ,@extra-arguments)))))))

(define (tryton-arguments module . extra-arguments)
  "Like ’tryton-phases’, but directly return all arguments for
the build system."
  `(#:phases ,(apply tryton-phases module extra-arguments)))

;;;
;;;  Tryton modules - please sort alphabetically
;;;

(define-public python-trytond-account
  (package
    (name "python-trytond-account")
    (version "5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account" version))
       (sha256
        (base32 "16ny67vcnxk9ngcxd56cfixm441vs9jxv3apmb16xsi47yk2xd7w"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account"))
    (native-inputs
     `(("python-genshi" ,python-genshi)
       ("python-lxml" ,python-lxml)
       ("python-magic" ,python-magic)
       ("python-passlib" ,python-passlib)
       ("python-polib" ,python-polib)
       ("python-proteus" ,python-proteus)
       ("python-relatorio" ,python-relatorio)
       ("python-werkzeug" ,python-werkzeug)
       ("python-wrapt" ,python-wrapt)))
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-simpleeval" ,python-simpleeval)
       ("python-sql" ,python-sql)
       ("python-trytond-company"
        ,python-trytond-company)
       ("python-trytond-currency"
        ,python-trytond-currency)
       ("python-trytond-party" ,python-trytond-party)
       ("trytond" ,trytond)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for accounting")
    (description
     "This package provides a Tryton module that defines the fundamentals for
most of accounting needs.")
    (license license:gpl3+)))

(define-public python-trytond-account-invoice
  (package
    (name "python-trytond-account-invoice")
    (version "5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_invoice" version))
       (sha256
        (base32 "0drccambg6855p7ai8654c7f9v85jzwicwpxmagyrr09qz6qzgcz"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_invoice"))
    (native-inputs
     `(("python-genshi" ,python-genshi)
       ("python-lxml" ,python-lxml)
       ("python-magic" ,python-magic)
       ("python-passlib" ,python-passlib)
       ("python-polib" ,python-polib)
       ("python-proteus" ,python-proteus)
       ("python-relatorio" ,python-relatorio)
       ("python-werkzeug" ,python-werkzeug)
       ("python-wrapt" ,python-wrapt)))
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-sql" ,python-sql)
       ("python-trytond-account"
        ,python-trytond-account)
       ("python-trytond-account-product"
        ,python-trytond-account-product)
       ("python-trytond-company"
        ,python-trytond-company)
       ("python-trytond-currency"
        ,python-trytond-currency)
       ("python-trytond-party" ,python-trytond-party)
       ("python-trytond-product"
        ,python-trytond-product)
       ("trytond" ,trytond)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for invoicing")
    (description
     "This package provides a Tryton module that adds the invoice, payment
term.")
    (license license:gpl3+)))

(define-public python-trytond-account-invoice-stock
  (package
    (name "python-trytond-account-invoice-stock")
    (version "5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri
             "trytond_account_invoice_stock"
             version))
       (sha256
        (base32 "02m6ikcc38ac41ddzg5xp5l9jz0k6j7j1g2xa62ki4v093yn4z5v"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_invoice_stock"))
    (native-inputs
     `(("python-dateutil" ,python-dateutil)
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
     `(("python-trytond-account-invoice"
        ,python-trytond-account-invoice)
       ("python-trytond-product"
        ,python-trytond-product)
       ("python-trytond-stock" ,python-trytond-stock)
       ("trytond" ,trytond)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module to link stock and invoice")
    (description
     "This package provides a Tryton module that adds link between invoice
lines and stock moves.  The unit price of the stock move is updated with the
average price of the posted invoice lines that are linked to it.")
    (license license:gpl3+)))

(define-public python-trytond-account-product
  (package
    (name "python-trytond-account-product")
    (version "5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_product" version))
       (sha256
        (base32 "10bpbkkmllbh9lm5ajydmc5nvqm9bbdn9rmm03jqgik23s5kyx2z"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_product"))
    (native-inputs
     `(("python-dateutil" ,python-dateutil)
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
     `(("python-trytond-account"
        ,python-trytond-account)
       ("python-trytond-analytic-account"
        ,python-trytond-analytic-account)
       ("python-trytond-company"
        ,python-trytond-company)
       ("python-trytond-product"
        ,python-trytond-product)
       ("trytond" ,trytond)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module to add accounting on product")
    (description
     "This package provides a Tryton module that adds accounting on product
and category.")
    (license license:gpl3+)))

(define-public python-trytond-analytic-account
  (package
    (name "python-trytond-analytic-account")
    (version "5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_analytic_account" version))
       (sha256
        (base32 "10rn2rf1ji7d1gxmgca368yvabql1ahklqg7p8sh5bl79vn5qx5x"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "analytic_account"))
    (native-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-genshi" ,python-genshi)
       ("python-lxml" ,python-lxml)
       ("python-magic" ,python-magic)
       ("python-passlib" ,python-passlib)
       ("python-polib" ,python-polib)
       ("python-proteus" ,python-proteus)
       ("python-relatorio" ,python-relatorio)
       ("python-werkzeug" ,python-werkzeug)
       ("python-wrapt" ,python-wrapt)))
    (propagated-inputs
     `(("python-sql" ,python-sql)
       ("python-trytond-account"
        ,python-trytond-account)
       ("python-trytond-company"
        ,python-trytond-company)
       ("python-trytond-currency"
        ,python-trytond-currency)
       ("python-trytond-party" ,python-trytond-party)
       ("trytond" ,trytond)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for analytic accounting")
    (description
     "This package provides a Tryton module that adds the fundamentals
required to analyse accounting using multiple different axes.")
    (license license:gpl3+)))

(define-public python-trytond-company
  (package
    (name "python-trytond-company")
    (version "5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_company" version))
       (sha256
        (base32 "1bwy2rkgfw32cwhq5fh3rpy7bx425h44ap10i9kjx5ak86bfnpz9"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "company"))
    (native-inputs
     `(("python-dateutil" ,python-dateutil)
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
     `(("python-trytond-currency"
        ,python-trytond-currency)
       ("python-trytond-party" ,python-trytond-party)
       ("trytond" ,trytond)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module with companies and employees")
    (description
     "This package provides a Tryton module that defines the concepts of
company and employee and extend the user model.")
    (license license:gpl3+)))

(define-public python-trytond-country
  (package
    (name "python-trytond-country")
    (version "5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_country" version))
       (sha256
        (base32 "1lkspk5w5pb0gg2h27zb7vwcj993gkm1f84qdxmqlpkc8raqvicj"))))
    (build-system python-build-system)
    ;; Doctest contains one test that requires internet access.
    (arguments (tryton-arguments "country" "--no-doctest"))
    (native-inputs
     `(("python-dateutil" ,python-dateutil)
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
       ("trytond" ,trytond)))
    (home-page "http://www.tryton.org/")
    (synopsis "Tryton module with countries")
    (description
     "This package provides a Tryton module with countries.")
    (license license:gpl3+)))

(define-public python-trytond-currency
  (package
    (name "python-trytond-currency")
    (version "5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_currency" version))
       (sha256
        (base32 "0b5p7ibil7nlsv7f31j69rka4xj5za798262algx7xa88a6h7mmx"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "currency"))
    (native-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-genshi" ,python-genshi)
       ("python-forex-python" ,python-forex-python)
       ("python-lxml" ,python-lxml)
       ("python-magic" ,python-magic)
       ("python-passlib" ,python-passlib)
       ("python-polib" ,python-polib)
       ("python-proteus" ,python-proteus)
       ("python-pycountry" ,python-pycountry)
       ("python-relatorio" ,python-relatorio)
       ("python-werkzeug" ,python-werkzeug)
       ("python-wrapt" ,python-wrapt)))
    (propagated-inputs
     `(("python-sql" ,python-sql)
       ("trytond" ,trytond)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module with currencies")
    (description
     "This package provides a Tryton module that defines the concepts of
currency and rate.")
    (license license:gpl3+)))

(define-public python-trytond-party
  (package
    (name "python-trytond-party")
    (version "5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_party" version))
       (sha256
        (base32 "1hapfq7ip99s4qp9xra1m40q4n379p9pmfnz2x4ggd79ss76bghc"))))
    (build-system python-build-system)
    ;; Doctest 'scenario_party_phone_number.rst' fails.
    (arguments (tryton-arguments "party" "--no-doctest"))
    (native-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-genshi" ,python-genshi)
       ("python-lxml" ,python-lxml)
       ("python-magic" ,python-magic)
       ("python-passlib" ,python-passlib)
       ("python-polib" ,python-polib)
       ("python-proteus" ,python-proteus)
       ("python-relatorio" ,python-relatorio)
       ("python-werkzeug" ,python-werkzeug)
       ("python-wrapt" ,python-wrapt)))
    (propagated-inputs
     `(("python-sql" ,python-sql)
       ("python-stnum" ,python-stdnum)
       ("python-trytond-country" ,python-trytond-country)
       ("trytond" ,trytond)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for parties and addresses")
    (description
     "This package provides a Tryton module for (counter)parties and
addresses.")
    (license license:gpl3+)))

(define-public python-trytond-product
  (package
    (name "python-trytond-product")
    (version "5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_product" version))
       (sha256
        (base32 "0x18ngpjyrdwjwg17bz98jph4jv5gcv0qc0p2kxpam4lqsy34ic2"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "product"))
    (native-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-genshi" ,python-genshi)
       ("python-lxml" ,python-lxml)
       ("python-magic" ,python-magic)
       ("python-passlib" ,python-passlib)
       ("python-polib" ,python-polib)
       ("python-proteus" ,python-proteus)
       ("python-relatorio" ,python-relatorio)
       ("python-werkzeug" ,python-werkzeug)
       ("python-wrapt" ,python-wrapt)))
    (propagated-inputs
     `(("python-sql" ,python-sql)
       ("python-stdnum" ,python-stdnum)
       ("python-trytond-company"
        ,python-trytond-company)
       ("trytond" ,trytond)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module with products")
    (description
     "This package provides a Tryton module that defines two concepts: Product
Template and Product.")
    (license license:gpl3+)))

(define-public python-trytond-purchase
  (package
    (name "python-trytond-purchase")
    (version "5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_purchase" version))
       (sha256
        (base32 "0na74zijj46b12gypy9si3las02a96rh5ygl503c7razha61g1b0"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "purchase"))
    (native-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-genshi" ,python-genshi)
       ("python-lxml" ,python-lxml)
       ("python-magic" ,python-magic)
       ("python-passlib" ,python-passlib)
       ("python-polib" ,python-polib)
       ("python-proteus" ,python-proteus)
       ("python-relatorio" ,python-relatorio)
       ("python-werkzeug" ,python-werkzeug)
       ("python-wrapt" ,python-wrapt)))
    (propagated-inputs
     `(("python-sql" ,python-sql)
       ("python-trytond-account"
        ,python-trytond-account)
       ("python-trytond-account-invoice"
        ,python-trytond-account-invoice)
       ("python-trytond-account-invoice-stock"
        ,python-trytond-account-invoice-stock)
       ("python-trytond-account-product"
        ,python-trytond-account-product)
       ("python-trytond-company"
        ,python-trytond-company)
       ("python-trytond-currency"
        ,python-trytond-currency)
       ("python-trytond-party" ,python-trytond-party)
       ("python-trytond-product"
        ,python-trytond-product)
       ("python-trytond-stock" ,python-trytond-stock)
       ("trytond" ,trytond)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for purchase")
    (description
     "This package provides a Tryton module that defines the Purchase model.")
    (license license:gpl3+)))

(define-public python-trytond-purchase-request
  (package
    (name "python-trytond-purchase-request")
    (version "5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_purchase_request" version))
       (sha256
        (base32 "1m92snnvgisnv083nml6cz5qgnfdg539rd5bwg3lqrknm7343w16"))))
    (build-system python-build-system)
    ;; Doctest 'scenario_purchase_request.rst' fails.
    (arguments (tryton-arguments "purchase_request" "--no-doctest"))
    (native-inputs
     `(("python-dateutil" ,python-dateutil)
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
     `(("python-trytond-product"
        ,python-trytond-product)
       ("python-trytond-purchase"
        ,python-trytond-purchase)
       ("trytond" ,trytond)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for purchase requests")
    (description
     "This package provides a Tryton module that introduces the concept of
Purchase Requests which are central points to collect purchase requests
generated by other process from Tryton.")
    (license license:gpl3+)))

(define-public python-trytond-stock
  (package
    (name "python-trytond-stock")
    (version "5.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock" version))
       (sha256
        (base32 "0yb8kd3alwqkivrlpx0ni4jxv3x14i37lmwism9yi81xwchyrcjk"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "stock"))
    (native-inputs
     `(("python-genshi" ,python-genshi)
       ("python-lxml" ,python-lxml)
       ("python-magic" ,python-magic)
       ("python-passlib" ,python-passlib)
       ("python-polib" ,python-polib)
       ("python-proteus" ,python-proteus)
       ("python-relatorio" ,python-relatorio)
       ("python-werkzeug" ,python-werkzeug)
       ("python-wrapt" ,python-wrapt)))
    (propagated-inputs
     `(("python-simpleeval" ,python-simpleeval)
       ("python-sql" ,python-sql)
       ("python-trytond-company"
        ,python-trytond-company)
       ("python-trytond-currency"
        ,python-trytond-currency)
       ("python-trytond-party" ,python-trytond-party)
       ("python-trytond-product"
        ,python-trytond-product)
       ("trytond" ,trytond)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for stock and inventory")
    (description
     "This package provides a Tryton module that defines the fundamentals for
all stock management situations: Locations where products are stored, moves
between these locations, shipments for product arrivals and departures and
inventory to control and update stock levels.")
    (license license:gpl3+)))

(define-public python-trytond-stock-lot
  (package
    (name "python-trytond-stock-lot")
    (version "5.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_lot" version))
       (sha256
        (base32 "0w2f62cfzm7j8wnw8igmjslpxc1a8s82dkdizyvim5qhjg6mrsym"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "stock_lot"))
    (native-inputs
     `(("python-dateutil" ,python-dateutil)
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
     `(("python-trytond-product"
        ,python-trytond-product)
       ("python-trytond-stock" ,python-trytond-stock)
       ("trytond" ,trytond)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for lot of products")
    (description
     "This package provides a Tryton module that defines lot of products.")
    (license license:gpl3+)))

(define-public python-trytond-stock-supply
  (package
    (name "python-trytond-stock-supply")
    (version "5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_supply" version))
       (sha256
        (base32 "01cgpxlznldrba79a3xmj4d0csyfc3ccgs66c490j8v8rdnqpbww"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "stock_supply"))
    (native-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-genshi" ,python-genshi)
       ("python-lxml" ,python-lxml)
       ("python-magic" ,python-magic)
       ("python-passlib" ,python-passlib)
       ("python-polib" ,python-polib)
       ("python-proteus" ,python-proteus)
       ("python-relatorio" ,python-relatorio)
       ("python-werkzeug" ,python-werkzeug)
       ("python-wrapt" ,python-wrapt)))
    (propagated-inputs
     `(("python-sql" ,python-sql)
       ("python-trytond-account"
        ,python-trytond-account)
       ("python-trytond-party" ,python-trytond-party)
       ("python-trytond-product"
        ,python-trytond-product)
       ("python-trytond-purchase"
        ,python-trytond-purchase)
       ("python-trytond-purchase-request"
        ,python-trytond-purchase-request)
       ("python-trytond-stock" ,python-trytond-stock)
       ("trytond" ,trytond)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for stock supply")
    (description
     "This package provides a Tryton module that adds automatic supply
mechanisms and introduces the concepts of order point.")
    (license license:gpl3+)))
