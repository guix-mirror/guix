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
    (version "5.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_party" version))
       (sha256
        (base32 "0wh7g1g67g4vwxm797ra6fkfvmd3w77vl7nxj76y856cy217gbzp"))))
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
               ;; Doctest 'scenario_party_phone_number.rst' fails.
               (invoke "python" runtest "-m" "party" "--no-doctest")))))))
    (native-inputs
     `(("python" ,python-minimal-wrapper)
       ("python-dateutil" ,python-dateutil)
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
       ("python-trytond" ,python-trytond)
       ("python-trytond-country" ,python-trytond-country)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for parties and addresses")
    (description
     "This package provides a Tryton module for (counter)parties and
addresses.")
    (license license:gpl3+)))

(define-public python-trytond-currency
  (package
    (name "python-trytond-currency")
    (version "5.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_currency" version))
       (sha256
        (base32 "1x6ynxpbafjpky5vfir9favijj6v5gl62szshladlx14ng6qgm68"))))
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
               (invoke "python" runtest "-m" "currency")))))))
    (native-inputs
     `(("python" ,python-minimal-wrapper)
       ("python-dateutil" ,python-dateutil)
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
       ("python-trytond" ,python-trytond)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module with currencies")
    (description
     "This package provides a Tryton module that defines the concepts of
currency and rate.")
    (license license:gpl3+)))

(define-public python-trytond-company
  (package
    (name "python-trytond-company")
    (version "5.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_company" version))
       (sha256
        (base32 "0fa2yswfal1fbmm0ml845lm6bwcm65fln6s1xq1wqi17xqbbx44x"))))
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
               (invoke "python" runtest "-m" "company")))))))
    (native-inputs
     `(("python" ,python-minimal-wrapper)
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
     `(("python-trytond" ,python-trytond)
       ("python-trytond-currency"
        ,python-trytond-currency)
       ("python-trytond-party" ,python-trytond-party)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module with companies and employees")
    (description
     "This package provides a Tryton module that defines the concepts of
company and employee and extend the user model.")
    (license license:gpl3+)))

(define-public python-trytond-product
  (package
    (name "python-trytond-product")
    (version "5.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_product" version))
       (sha256
        (base32 "0k1sw1jfgsm9qhyhv4lzama31db6ccjx5f2a7xw96ypflfl9f1xz"))))
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
               (invoke "python" runtest "-m" "product")))))))
    (native-inputs
     `(("python" ,python-minimal-wrapper)
       ("python-dateutil" ,python-dateutil)
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
       ("python-trytond" ,python-trytond)
       ("python-trytond-company"
        ,python-trytond-company)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module with products")
    (description
     "This package provides a Tryton module that defines two concepts: Product
Template and Product.")
    (license license:gpl3+)))

(define-public python-trytond-account
  (package
    (name "python-trytond-account")
    (version "5.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account" version))
       (sha256
        (base32 "17q0cxp3vkymbv62ir4c6kg9y8lx8xvz30p0asrbsnbgwl6sjm30"))))
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
               (invoke "python" runtest "-m" "account")))))))
    (native-inputs
     `(("python" ,python-minimal-wrapper)
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
     `(("python-dateutil" ,python-dateutil)
       ("python-simpleeval" ,python-simpleeval)
       ("python-sql" ,python-sql)
       ("python-trytond" ,python-trytond)
       ("python-trytond-company"
        ,python-trytond-company)
       ("python-trytond-currency"
        ,python-trytond-currency)
       ("python-trytond-party" ,python-trytond-party)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for accounting")
    (description
     "This package provides a Tryton module that defines the fundamentals for
most of accounting needs.")
    (license license:gpl3+)))

(define-public python-trytond-analytic-account
  (package
    (name "python-trytond-analytic-account")
    (version "5.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_analytic_account" version))
       (sha256
        (base32 "02hv3mmhadz248vy4azrw8rs2mwgsixd4cnzsm82z15gjmfxl34q"))))
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
               (invoke "python" runtest "-m" "analytic_account")))))))
    (native-inputs
     `(("python" ,python-minimal-wrapper)
       ("python-dateutil" ,python-dateutil)
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
       ("python-trytond" ,python-trytond)
       ("python-trytond-account"
        ,python-trytond-account)
       ("python-trytond-company"
        ,python-trytond-company)
       ("python-trytond-currency"
        ,python-trytond-currency)
       ("python-trytond-party" ,python-trytond-party)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for analytic accounting")
    (description
     "This package provides a Tryton module that adds the fundamentals
required to analyse accounting using multiple different axes.")
    (license license:gpl3+)))

(define-public python-trytond-account-product
  (package
    (name "python-trytond-account-product")
    (version "5.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_product" version))
       (sha256
        (base32 "0iyk07adixf37jlvn7n27ac12hk23zx09w4s78dr9sfp0lsf8vv8"))))
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
               (invoke "python" runtest "-m" "account_product")))))))
    (native-inputs
     `(("python" ,python-minimal-wrapper)
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
     `(("python-trytond" ,python-trytond)
       ("python-trytond-account"
        ,python-trytond-account)
       ("python-trytond-analytic-account"
        ,python-trytond-analytic-account)
       ("python-trytond-company"
        ,python-trytond-company)
       ("python-trytond-product"
        ,python-trytond-product)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module to add accounting on product")
    (description
     "This package provides a Tryton module that adds accounting on product
and category.")
    (license license:gpl3+)))

(define-public python-trytond-account-invoice
  (package
    (name "python-trytond-account-invoice")
    (version "5.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_invoice" version))
       (sha256
        (base32 "0ic5zghg50m6jwdr88y4vgzy58g1h17b63sbhh61brys1cp2dxhc"))))
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
               (invoke "python" runtest "-m" "account_invoice")))))))
    (native-inputs
     `(("python" ,python-minimal-wrapper)
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
     `(("python-dateutil" ,python-dateutil)
       ("python-sql" ,python-sql)
       ("python-trytond" ,python-trytond)
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
        ,python-trytond-product)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for invoicing")
    (description
     "This package provides a Tryton module that adds the invoice, payment
term.")
    (license license:gpl3+)))

(define-public python-trytond-stock
  (package
    (name "python-trytond-stock")
    (version "5.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock" version))
       (sha256
        (base32 "13m57absscks2advpjz0mmmp6c0c4ksf9aln1brqyd0js38arw76"))))
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
               (invoke "python" runtest "-m" "stock")))))))
    (native-inputs
     `(("python" ,python-minimal-wrapper)
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
     `(("python-simpleeval" ,python-simpleeval)
       ("python-sql" ,python-sql)
       ("python-trytond" ,python-trytond)
       ("python-trytond-company"
        ,python-trytond-company)
       ("python-trytond-currency"
        ,python-trytond-currency)
       ("python-trytond-party" ,python-trytond-party)
       ("python-trytond-product"
        ,python-trytond-product)))
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
    (version "5.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_lot" version))
       (sha256
        (base32 "0a0kc8dyx2zw244dhq6lm2jdp1kzpllhdhrmx7kkr9cjhp58rqip"))))
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
               (invoke "python" runtest "-m" "stock_lot")))))))
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
     `(("python-trytond" ,python-trytond)
       ("python-trytond-product"
        ,python-trytond-product)
       ("python-trytond-stock" ,python-trytond-stock)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for lot of products")
    (description
     "This package provides a Tryton module that defines lot of products.")
    (license license:gpl3+)))

(define-public python-trytond-account-invoice-stock
  (package
    (name "python-trytond-account-invoice-stock")
    (version "5.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri
             "trytond_account_invoice_stock"
             version))
       (sha256
        (base32 "1ky39ab8ax7hla5d9qgmp0p4ra4q6n29xc3wba5qdw3ij8wkn96f"))))
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
               (invoke "python" runtest "-m" "account_invoice_stock")))))))
    (native-inputs
     `(("python" ,python-minimal-wrapper)
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
     `(("python-trytond" ,python-trytond)
       ("python-trytond-account-invoice"
        ,python-trytond-account-invoice)
       ("python-trytond-product"
        ,python-trytond-product)
       ("python-trytond-stock" ,python-trytond-stock)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module to link stock and invoice")
    (description
     "This package provides a Tryton module that adds link between invoice
lines and stock moves.  The unit price of the stock move is updated with the
average price of the posted invoice lines that are linked to it.")
    (license license:gpl3+)))

(define-public python-trytond-stock-supply
  (package
    (name "python-trytond-stock-supply")
    (version "5.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_supply" version))
       (sha256
        (base32 "0lw7qrg58k5pjpp3nz73rmlm8v086l49ri31ci75calm552zpfay"))))
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
               (invoke "python" runtest "-m" "stock_supply")))))))
    (native-inputs
     `(("python" ,python-minimal-wrapper)
       ("python-dateutil" ,python-dateutil)
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
       ("python-trytond" ,python-trytond)
       ("python-trytond-account"
        ,python-trytond-account)
       ("python-trytond-party" ,python-trytond-party)
       ("python-trytond-product"
        ,python-trytond-product)
       ("python-trytond-purchase"
        ,python-trytond-purchase)
       ("python-trytond-purchase-request"
        ,python-trytond-purchase-request)
       ("python-trytond-stock" ,python-trytond-stock)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for stock supply")
    (description
     "This package provides a Tryton module that adds automatic supply
mechanisms and introduces the concepts of order point.")
    (license license:gpl3+)))

(define-public python-trytond-purchase
  (package
    (name "python-trytond-purchase")
    (version "5.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_purchase" version))
       (sha256
        (base32 "0znjxhhwqlz4sq96ah9g28mvw4w83rhp022h6vihnz64z2ad3p3z"))))
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
               (invoke "python" runtest "-m" "purchase")))))))
    (native-inputs
     `(("python" ,python-minimal-wrapper)
       ("python-dateutil" ,python-dateutil)
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
       ("python-trytond" ,python-trytond)
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
       ("python-trytond-stock" ,python-trytond-stock)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for purchase")
    (description
     "This package provides a Tryton module that defines the Purchase model.")
    (license license:gpl3+)))

(define-public python-trytond-purchase-request
  (package
    (name "python-trytond-purchase-request")
    (version "5.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_purchase_request" version))
       (sha256
        (base32 "15r1x3jzr83rna1vfpans277785dpmgjr7989icav602nkyhhx0x"))))
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
               ;; Doctest 'scenario_purchase_request.rst' fails.
               (invoke "python" runtest
                       "-m" "purchase_request" "--no-doctest")))))))
    (native-inputs
     `(("python" ,python-minimal-wrapper)
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
     `(("python-trytond" ,python-trytond)
       ("python-trytond-product"
        ,python-trytond-product)
       ("python-trytond-purchase"
        ,python-trytond-purchase)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for purchase requests")
    (description
     "This package provides a Tryton module that introduces the concept of
Purchase Requests which are central points to collect purchase requests
generated by other process from Tryton.")
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
