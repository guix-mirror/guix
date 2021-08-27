;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Hartmut Goebel <h.goebel@crazy-compilers.com>
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
    (version "6.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond" version))
       (sha256
        (base32 "1jp5cadqpwkcnml8r1hj6aak5kc8an2d5ai62p96x77nn0dp3ny4"))))
    (build-system python-build-system)
    (propagated-inputs
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
     `(("python-mock" ,python-mock)
       ("python-pillow" ,python-pillow)))
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
    (version "6.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tryton" version))
       (sha256
        (base32 "15cbp2r25pkr7lp4yliqgb6d0n779z70d4gckv56bx5aw4z27f66"))))
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
    (propagated-inputs
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
    (version "6.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "proteus" version))
       (sha256
        (base32 "0qr7rir7ysxvy2kyfzp2d2kcw9qzq4vdkddbwswzgddxjpycksdh"))))
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

(define %standard-trytond-native-inputs
  ;; native-inputs required by most of the tryton module for running the test
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

(define-public trytond-account
  (package
    (name "trytond-account")
    (version "6.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account" version))
       (sha256
        (base32 "0j1mn8sd5n8rkwgfvcy9kf8s7s3qxvnilnc72i83ac573zj922xc"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("python-simpleeval" ,python-simpleeval)
       ("trytond" ,trytond)
       ("trytond-company" ,trytond-company)
       ("trytond-currency" ,trytond-currency)
       ("trytond-party" ,trytond-party)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for accounting")
    (description
     "This package provides a Tryton module that defines the fundamentals for
most of accounting needs.")
    (license license:gpl3+)))

(define-public python-trytond-account
  (deprecated-package "python-trytond-account" trytond-account))

(define-public trytond-account-asset
  (package
    (name "trytond-account-asset")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_asset" version))
       (sha256
        (base32 "12qf6f4hpxi6c3mx18d07ljbzzr58h0lg8yz95nby3g3mpx2mlaz"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_asset"))
    (native-inputs
     `(,@%standard-trytond-native-inputs
       ("trytond-purchase" ,trytond-purchase)))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-account-product" ,trytond-account-product)
       ("trytond-product" ,trytond-product)))
    (home-page "https://docs.tryton.org/projects/modules-account-asset")
    (synopsis "Tryton module for assets management")
    (description "The @emph{Account Asset} Tryton module adds the depreciation
of fixed assets.")
    (license license:gpl3+)))

(define-public trytond-account-be
  (package
    (name "trytond-account-be")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_be" version))
       (sha256
        (base32 "1l4zxsh9f3ndsgj3224xv23nr7gbg5kwrydwgv34nlkyxp5557dk"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_be"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-account-eu" ,trytond-account-eu)))
    (home-page "https://docs.tryton.org/projects/modules-account-be")
    (synopsis "Tryton module with Belgian chart of accounts")
    (description "The @emph{Account BE} Tryton module defines the standard
chart of account for Belgium.")
    (license license:gpl3+)))

(define-public trytond-account-cash-rounding
  (package
    (name "trytond-account-cash-rounding")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_cash_rounding" version))
       (sha256
        (base32 "15xl36929zgfw9rlwaqdqs5l4ijspfx8i060z6884p2nizhh69s5"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_cash_rounding"))
    (native-inputs
     `(,@%standard-trytond-native-inputs
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-purchase" ,trytond-purchase)
       ("trytond-sale" ,trytond-sale)))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-currency" ,trytond-currency)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-cash-rounding")
    (synopsis "Tryton module to round cash amount")
    (description "The @emph{Account Cash Rounding} Tryton module allows cash
amounts to be rounded using the cash rounding factor of the currency.")
    (license license:gpl3+)))

(define-public trytond-account-credit-limit
  (package
    (name "trytond-account-credit-limit")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_credit_limit" version))
       (sha256
        (base32 "1ni4cfydypxrkdj7fc0j4js9ygfkxa22ii5db2p89z2izbg1hxd8"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_credit_limit"))
    (native-inputs
     `(,@%standard-trytond-native-inputs
       ("trytond-account-dunning" ,trytond-account-dunning)))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-company" ,trytond-company)
       ("trytond-party" ,trytond-party)))
    (home-page "https://docs.tryton.org/projects/modules-account-credit-limit")
    (synopsis "Tryton module for account credit limit")
    (description "The @emph{Account Credit Limit} Tryton module for manages
credit limit of parties.")
    (license license:gpl3+)))

(define-public trytond-account-de-skr03
  (package
    (name "trytond-account-de-skr03")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_de_skr03" version))
       (sha256
        (base32 "1dhgspabr2bm0y6qkzh5kz6badhf23arzkw7lra1zsn52r23j9dl"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_de_skr03"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)))
    (home-page "https://docs.tryton.org/projects/modules-account-de-skr03")
    (synopsis "Tryton module with German chart of accounts SKR03")
    (description "This package provides the German SKR03 chart of accounts for
Tryton.")
    (license license:gpl3+)))

(define-public trytond-account-deposit
  (package
    (name "trytond-account-deposit")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_deposit" version))
       (sha256
        (base32 "005yw868wxv8fhp7dlqd2z19hhjlmk4cgqa36axdfjmbwxvh1r6r"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_deposit"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-company" ,trytond-company)
       ("trytond-party" ,trytond-party)))
    (home-page "https://docs.tryton.org/projects/modules-account-deposit")
    (synopsis "Tryton module for accounting deposit")
    (description "The @emph{Account Deposit} Tryton module adds support for
deposit accounting.

A deposit is an amount paid by the customer prior to the company providing it
with services or goods.  A wizard on invoice allows to recall prior deposit of
the party.")
    (license license:gpl3+)))

(define-public trytond-account-dunning
  (package
    (name "trytond-account-dunning")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_dunning" version))
       (sha256
        (base32 "0d5wabn5g1gzmllihkdhzqix934184v303pp20927qxpzb2pm3qw"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_dunning"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-company" ,trytond-company)
       ("trytond-party" ,trytond-party)))
    (home-page "https://docs.tryton.org/projects/modules-account-dunning")
    (synopsis "Tryton module for account dunning")
    (description "The @emph{Account Dunning} Tryton module adds dunning for
receivable move lines.")
    (license license:gpl3+)))

(define-public trytond-account-dunning-email
  (package
    (name "trytond-account-dunning-email")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_dunning_email" version))
       (sha256
        (base32 "0sbh2rnziw5fhlbaq4n8q9rwqcgz35rik77dbvhflyyjdxh51vfq"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_dunning_email"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account-dunning" ,trytond-account-dunning)
       ("trytond-account-dunning-letter" ,trytond-account-dunning-letter)
       ("trytond-party" ,trytond-party)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-dunning-email")
    (synopsis "Tryton module for account dunning email")
    (description "This package provides a Tryton module for sending dunning
emails.")
    (license license:gpl3+)))

(define-public trytond-account-dunning-fee
  (package
    (name "trytond-account-dunning-fee")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_dunning_fee" version))
       (sha256
        (base32 "1h8qlrdvpyyf36cdss5lv3qp4h4xs6kp3ybh9ci14mhwy0jyni75"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_dunning_fee"))
    (native-inputs
     `(,@%standard-trytond-native-inputs
       ("trytond-account-dunning-letter" ,trytond-account-dunning-letter)))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account-dunning" ,trytond-account-dunning)
       ("trytond-account-product" ,trytond-account-product)))
    (home-page "https://docs.tryton.org/projects/modules-account-dunning-fee")
    (synopsis "Tryton module for account dunning fee")
    (description "This package provides a Tryton module for generating
accounting moves as fees when processing dunning.")
    (license license:gpl3+)))

(define-public trytond-account-dunning-letter
  (package
    (name "trytond-account-dunning-letter")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_dunning_letter" version))
       (sha256
        (base32 "0xb0mpf7n8jjgm8sb52q5sb2fjs1ap1gc0p33csdgz2lyh66lh3b"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_dunning_letter"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-account-dunning" ,trytond-account-dunning)
       ("trytond-company" ,trytond-company)
       ("trytond-party" ,trytond-party)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-dunning-letter")
    (synopsis "Tryton module for account dunning letter")
    (description "This package provides a Tryton module for generating dunning
letters.")
    (license license:gpl3+)))

(define-public trytond-account-es
  (package
    (name "trytond-account-es")
    (version "6.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_es" version))
       (sha256
        (base32 "19qflzfwbbwz7jxxbah2l8z89m1rwsgvm80w2qsjw93chzabz802"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_es"))
    (native-inputs
     `(,@%standard-trytond-native-inputs
       ("trytond-account-asset" ,trytond-account-asset)
       ("trytond-account-payment-sepa" ,trytond-account-payment-sepa)))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-account-eu" ,trytond-account-eu)
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-party" ,trytond-party)))
    (home-page "https://docs.tryton.org/projects/modules-account-es")
    (synopsis "Tryton with Spanish chart of accounts")
    (description "This package provides the following Spanish charts of
accounts for Tryton:
@itemize
@item Plan General Contable Español 2008
@item Plan Contable para PYMES 2008
@end itemize

A wizard allows to generate the following AEAT files:

@itemize
@item Modelo 111
@item Modelo 115
@item Modelo 303
@end itemize")
    (license license:gpl3+)))

(define-public trytond-account-eu
  (package
    (name "trytond-account-eu")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_eu" version))
       (sha256
        (base32 "1x3w7iijkckv20q8lpqb5fnfrvddm130f51mcpnh4hlyx14q1c5i"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_eu"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-company" ,trytond-company)
       ("trytond-currency" ,trytond-currency)
       ("trytond-party" ,trytond-party)))
    (home-page "https://docs.tryton.org/projects/modules-account-eu")
    (synopsis "Tryton module for european accounting")
    (description "This package provides a Tryton module implementing common
accounting requirements in Europe.  It includes:

@itemize
@item EC Sales List (ESL)
@end itemize")
    (license license:gpl3+)))

(define-public trytond-account-fr
  (package
    (name "trytond-account-fr")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_fr" version))
       (sha256
        (base32 "1zpzdnd68gsjrz4as019amygdh8yad8wvwrm22dbccwmbbnyvqpg"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_fr"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-party-siret" ,trytond-party-siret)))
    (home-page "https://docs.tryton.org/projects/modules-account-fr")
    (synopsis "Tryton module with French chart of accounts")
    (description "This package provides the French standard chart of account
for Tryton.")
    (license license:gpl3+)))

(define-public trytond-account-fr-chorus
  (package
    (name "trytond-account-fr-chorus")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_fr_chorus" version))
       (sha256
        (base32 "13z30390zinv6ps0zr3k7mdmxrw2nhr49k248yjk0c0qh9rwifll"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_fr_chorus"))
    (native-inputs
     `(,@%standard-trytond-native-inputs
       ("trytond-edocument-uncefact" ,trytond-edocument-uncefact)))
    (propagated-inputs
     `(("python-requests" ,python-requests)
       ("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-company" ,trytond-company)
       ("trytond-party" ,trytond-party)
       ("trytond-party-siret" ,trytond-party-siret)))
    (home-page "https://docs.tryton.org/projects/modules-account-fr-chorus")
    (synopsis "Tryton module to communicate with the French Chorus Pro
portal")
    (description "This package provides a Tryton module to send invoices
through the French Chorus Pro portal.

If the party is checked for Chorus Pro, all posted customer invoices are
queued to be sent.  A cron job will send them every 15 minutes by default,
using the credential from the accounting configuration.")
    (license license:gpl3+)))

(define-public trytond-account-invoice
  (package
    (name "trytond-account-invoice")
    (version "6.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_invoice" version))
       (sha256
        (base32 "0r8zigb4qmv40kf835x8jd7049nnhk5g7g0aibvfd0y9p28lspnz"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_invoice"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-account-product" ,trytond-account-product)
       ("trytond-company" ,trytond-company)
       ("trytond-currency" ,trytond-currency)
       ("trytond-party" ,trytond-party)
       ("trytond-product" ,trytond-product)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for invoicing")
    (description
     "This package provides a Tryton module that adds the invoice, payment
term.")
    (license license:gpl3+)))

(define-public python-trytond-account-invoice
  (deprecated-package "python-trytond-account-invoice" trytond-account-invoice))

(define-public trytond-account-invoice-correction
  (package
    (name "trytond-account-invoice-correction")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_invoice_correction" version))
       (sha256
        (base32 "152jxsl6v2wclk1wjhykbyvianh47cp2yg575hkx18dfynyp7nmw"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_invoice_correction"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account-invoice" ,trytond-account-invoice)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-invoice-correction")
    (synopsis "Tryton module to correct invoice")
    (description "The @emph{Account Invoice Correction} Tryton module adds a
wizard on invoice which allows select lines for which the unit price must be
corrected.  A new invoice is created with those lines in double: once with the
original quantity, once with the inverted quantity.")
    (license license:gpl3+)))

(define-public trytond-account-invoice-defer
  (package
    (name "trytond-account-invoice-defer")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_invoice_defer" version))
       (sha256
        (base32 "1ay9cpf6z8j3gamwy52z88qg31s09wkp6k5flv20f9c00hvly452"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_invoice_defer"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-company" ,trytond-company)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-invoice-defer")
    (synopsis "Tryton module to defer expense and revenue")
    (description "The @emph{Account Invoice Defer} Tryton module allows to
defer the expense or the revenue of an invoice line over many periods.")
    (license license:gpl3+)))

(define-public trytond-account-invoice-history
  (package
    (name "trytond-account-invoice-history")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_invoice_history" version))
       (sha256
        (base32 "0pq1raj6v76vqsb2mk1bfv1vg1ngfk9iiai30rlzj2zcl53phcvj"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_invoice_history"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-party" ,trytond-party)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-invoice-history")
    (synopsis "Tryton module to historize invoices")
    (description "The @emph{Account Invoice History} Tryton module activates
the historization of the invoice and its related fields.")
    (license license:gpl3+)))

(define-public trytond-account-invoice-line-standalone
  (package
    (name "trytond-account-invoice-line-standalone")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_invoice_line_standalone" version))
       (sha256
        (base32 "1affxhinyzz1lqfq621f76fixnx523fi7qrxwsqa4f1b6g31651a"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_invoice_line_standalone"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account-invoice" ,trytond-account-invoice)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-invoice-line-standalone")
    (synopsis "Tryton module to have standalone invoice lines")
    (description "The @emph{Account Invoice Line Standalone} Tryton module
allows to create invoice line not linked to an invoice.")
    (license license:gpl3+)))

(define-public trytond-account-invoice-secondary-unit
  (package
    (name "trytond-account-invoice-secondary-unit")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_invoice_secondary_unit" version))
       (sha256
        (base32 "0wam7v92ldajpx3529x2cfvczgwv8ayr6hi6bwj8fi736p9x2kbp"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_invoice_secondary_unit"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-product" ,trytond-product)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-invoice-secondary-unit")
    (synopsis "Tryton module to add a secondary unit on invoice line")
    (description "The @emph{Account Invoice Secondary Unit} Tryton module adds
a secondary unit of measure on invoice line.")
    (license license:gpl3+)))

(define-public trytond-account-invoice-stock
  (package
    (name "trytond-account-invoice-stock")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_invoice_stock" version))
       (sha256
        (base32 "1228n6vsx0rdjsy3idvpyssa3n21nhvz9gqaacwa46c0hp2251bp"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_invoice_stock"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-product" ,trytond-product)
       ("trytond-stock" ,trytond-stock)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module to link stock and invoice")
    (description
     "This package provides a Tryton module that adds link between invoice
lines and stock moves.  The unit price of the stock move is updated with the
average price of the posted invoice lines that are linked to it.")
    (license license:gpl3+)))

(define-public python-trytond-account-invoice-stock
  (deprecated-package
   "python-trytond-account-invoice-stock" trytond-account-invoice-stock))

(define-public trytond-account-payment
  (package
    (name "trytond-account-payment")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_payment" version))
       (sha256
        (base32 "006d78kcml65mxikqp80igln118vkxfs9ah03lq5j9bvnfr6bb2m"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_payment"))
    (native-inputs
     `(,@%standard-trytond-native-inputs
       ("trytond-account-invoice" ,trytond-account-invoice)))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-company" ,trytond-company)
       ("trytond-currency" ,trytond-currency)
       ("trytond-party" ,trytond-party)))
    (home-page "https://docs.tryton.org/projects/modules-account-payment")
    (synopsis "Tryton module for payment")
    (description "This package provides a Tryton module for generating grouped
payments for receivable or payable Account Move Lines.")
    (license license:gpl3+)))

(define-public trytond-account-payment-braintree
  (package
    (name "trytond-account-payment-braintree")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_payment_braintree" version))
       (sha256
        (base32 "0dgw47q4m5l13bhfl1kdpajh0q94pazdrq9sqzf1vg9mggai2gvi"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_payment_braintree"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("python-braintree" ,python-braintree)
       ("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-account-payment" ,trytond-account-payment)
       ("trytond-party" ,trytond-party)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-payment-braintree")
    (synopsis "Tryton module for Braintree payment")
    (description "The @emph{Account Payment Braintree} Tryton module allows
receipt of payments using Braintree.  It uses the Drop-in UI in a checkout
form to handle the payment method nonce for card and other supported payment
methods.")
    (license license:gpl3+)))

(define-public trytond-account-payment-clearing
  (package
    (name "trytond-account-payment-clearing")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_payment_clearing" version))
       (sha256
        (base32 "0dvjfgp0lrqn838wchkmhbbg4990xx2jv337rivnis164nw84dn0"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_payment_clearing"))
    (native-inputs
     `(,@%standard-trytond-native-inputs
       ("trytond-account-statement" ,trytond-account-statement)
       ("trytond-account-statement-rule" ,trytond-account-statement-rule)))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account-payment" ,trytond-account-payment)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-payment-clearing")
    (synopsis "Tryton module for payment clearing")
    (description "The @emph{Account Payment Clearing} Tryton module allows to
generate account move when a payment is succeeded between the
receivable/payable account to a clearing account defined on the payment
journal.")
    (license license:gpl3+)))

(define-public trytond-account-payment-sepa
  (package
    (name "trytond-account-payment-sepa")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_payment_sepa" version))
       (sha256
        (base32 "0clnl2lql5q0gh48lad3vw47xajgd3cdj4kbmvdi72jh775p174w"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_payment_sepa"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("python-stdnum" ,python-stdnum)
       ("trytond" ,trytond)
       ("trytond-account-payment" ,trytond-account-payment)
       ("trytond-bank" ,trytond-bank)
       ("trytond-company" ,trytond-company)
       ("trytond-party" ,trytond-party)))
    (home-page "https://docs.tryton.org/projects/modules-account-payment-sepa")
    (synopsis "Tryton module for SEPA payment")
    (description "The @emph{Account Payment SEPA} Tryton module allows to
generate SEPA files for a Payment Group.")
    (license license:gpl3+)))

(define-public trytond-account-payment-sepa-cfonb
  (package
    (name "trytond-account-payment-sepa-cfonb")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_payment_sepa_cfonb" version))
       (sha256
        (base32 "0ccn5s360w78wqnq86qfyci8wii4n1n3pd9wvwsnbrpmabj1byv1"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_payment_sepa_cfonb"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account-payment" ,trytond-account-payment)
       ("trytond-account-payment-sepa" ,trytond-account-payment-sepa)
       ("trytond-bank" ,trytond-bank)
       ("trytond-company" ,trytond-company)
       ("trytond-party" ,trytond-party)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-payment-sepa-cfonb")
    (synopsis "Tryton module for CFONB SEPA payment")
    (description "The @emph{account payment sepa cfonb} Tryton module adds
CFONB flavors to SEPA messages.")
    (license license:gpl3+)))

(define-public trytond-account-payment-stripe
  (package
    (name "trytond-account-payment-stripe")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_payment_stripe" version))
       (sha256
        (base32 "18hr2lrrx9asb0d3cjcpska4bv825yjln9cbqjzg0xbl36z6w3s2"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_payment_stripe"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("python-stripe" ,python-stripe)
       ("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-account-payment" ,trytond-account-payment)
       ("trytond-party" ,trytond-party)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-payment-stripe")
    (synopsis "Tryton module for Stripe payment")
    (description "The @emph{Account Payment Stripe} Tryton module for
receiving payments from Stripe.  It uses Stripe.js and Stripe Elements in a
checkout form to handle Setup Intent and Payment Intent by card.")
    (license license:gpl3+)))

(define-public trytond-account-product
  (package
    (name "trytond-account-product")
    (version "6.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_product" version))
       (sha256
        (base32 "1z0dn1p22smzb4a9v451224wrpxcw94inl7jxkarc0q088gasn7d"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_product"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-analytic-account" ,trytond-analytic-account)
       ("trytond-company" ,trytond-company)
       ("trytond-product" ,trytond-product)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module to add accounting on product")
    (description
     "This package provides a Tryton module that adds accounting on product
and category.")
    (license license:gpl3+)))

(define-public python-trytond-account-product
  (deprecated-package "python-trytond-account-product" trytond-account-product))

(define-public trytond-account-statement
  (package
    (name "trytond-account-statement")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_statement" version))
       (sha256
        (base32 "0nlak4kv2ampb5v2zbsvabnirvdi53h6vr35kp2zmrv4alpjsla0"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_statement"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-bank" ,trytond-bank)
       ("trytond-company" ,trytond-company)
       ("trytond-currency" ,trytond-currency)
       ("trytond-party" ,trytond-party)))
    (home-page "https://docs.tryton.org/projects/modules-account-statement")
    (synopsis "Tryton module with account statements")
    (description "The @emph{Account Statement} Tryton module allows to book
statements.  Statement can be used for bank statement, cash daybook etc.")
    (license license:gpl3+)))

(define-public trytond-account-statement-aeb43
  (package
    (name "trytond-account-statement-aeb43")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_statement_aeb43" version))
       (sha256
        (base32 "09mkxb9m9167lpca8pb8k4rvnwhsng6b6pmhw9c21w2r4q0hppxv"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_statement_aeb43"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("python-csb43" ,python-csb43)
       ("python-stdnum" ,python-stdnum)
       ("trytond" ,trytond)
       ("trytond-account-statement" ,trytond-account-statement)
       ("trytond-bank" ,trytond-bank)))
    (home-page
     "https://docs.tryton.org/projects/trytond-account-statement-aeb43")
    (synopsis "Tryton module to import AEB43 statements")
    (description "The @emph{Account Statement AEB43} Tryton module implements
the import of @emph{Norm 43} files as statement.  @emph{Norm 43} is a standard
defined by the Spanish banking association.")
    (license license:gpl3+)))

(define-public trytond-account-statement-coda
  (package
    (name "trytond-account-statement-coda")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_statement_coda" version))
       (sha256
        (base32 "11gryxh2b2py0h6f89nj2y42cwb8rrxjn0r5jbhrcsfhb2kh1x3w"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_statement_coda"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("python-febelfin-coda" ,python-febelfin-coda)
       ("trytond" ,trytond)
       ("trytond-account-statement" ,trytond-account-statement)
       ("trytond-bank" ,trytond-bank)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-statement-coda")
    (synopsis "Tryton module to import CODA statements")
    (description "The @emph{Account Statement CODA} Tryton module implements
the import of @emph{CODA} files as statement.  @emph{CODA} is a standard
defined by Belgian \"febelfin\".")
    (license license:gpl3+)))

(define-public trytond-account-statement-ofx
  (package
    (name "trytond-account-statement-ofx")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_statement_ofx" version))
       (sha256
        (base32 "1n24lwp1lfw59xdd7mqaz6ncr5b0bldr2qniqdnkvyaav0h8h5px"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_statement_ofx"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("python-ofxparse" ,python-ofxparse)
       ("trytond" ,trytond)
       ("trytond-account-statement" ,trytond-account-statement)
       ("trytond-bank" ,trytond-bank)
       ("trytond-party" ,trytond-party)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-statement-ofx")
    (synopsis "Tryton module to import OFX statements")
    (description "The @emph{Account Statement OFX} Tryton module implements
the import of the @emph{OFX} files as statement.")
    (license license:gpl3+)))

(define-public trytond-account-statement-rule
  (package
    (name "trytond-account-statement-rule")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_statement_rule" version))
       (sha256
        (base32 "0kg6lf2wa5scwxggr8p7r7j6jd3a34qv2dcs9w18ra1qvg6p4kmp"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_statement_rule"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-account-statement" ,trytond-account-statement)
       ("trytond-company" ,trytond-company)
       ("trytond-party" ,trytond-party)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-statement-rule")
    (synopsis "Tryton module to automate statement import with rules")
    (description "The @emph{Account Statement Rule} Tryton module allows rules
to be defined to complete statement lines from imported files.

When the @emph{Apply Rule} button is clicked on a statement, each rule is
tested in order against each origin that does not have any lines until one is
found that matches.  Then the rule found is used to create the statement lines
linked to the origin.")
    (license license:gpl3+)))

(define-public trytond-account-stock-anglo-saxon
  (package
    (name "trytond-account-stock-anglo-saxon")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_stock_anglo_saxon" version))
       (sha256
        (base32 "090a4ykrlk2xpqxqhmpa2jrbjkrvhibm60ilp1n28wisbgv9d3di"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_stock_anglo_saxon"))
    (native-inputs
     `(,@%standard-trytond-native-inputs
       ("trytond-purchase" ,trytond-purchase)
       ("trytond-sale" ,trytond-sale)
       ("trytond-sale-supply-drop-shipment"
        ,trytond-sale-supply-drop-shipment)))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-account-invoice-stock" ,trytond-account-invoice-stock)
       ("trytond-account-product" ,trytond-account-product)
       ("trytond-account-stock-continental"
        ,trytond-account-stock-continental)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-stock-anglo-saxon")
    (synopsis "Tryton module for anglo-saxon real-time stock valuation")
    (description "The @emph{Account Stock Anglo Saxon} Tryton module adds the
anglo-saxon accounting model for stock valuation.")
    (license license:gpl3+)))

(define-public trytond-account-stock-continental
  (package
    (name "trytond-account-stock-continental")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_stock_continental" version))
       (sha256
        (base32 "1i7cyxqs2vxxvg5iyplhai4g8pi0nmh90i5kxqhbnhich6myfr4n"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_stock_continental"))
    (native-inputs
     `(,@%standard-trytond-native-inputs
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-purchase" ,trytond-purchase)
       ("trytond-sale" ,trytond-sale)
       ("trytond-sale-supply-drop-shipment"
        ,trytond-sale-supply-drop-shipment)))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-account-product" ,trytond-account-product)
       ("trytond-stock" ,trytond-stock)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-stock-continental")
    (synopsis "Tryton module for continental real-time stock valuation")
    (description "The @emph{Account Stock Continental} Tryton module adds the
continental accounting model for stock valuation.")
    (license license:gpl3+)))

(define-public trytond-account-stock-landed-cost
  (package
    (name "trytond-account-stock-landed-cost")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_stock_landed_cost" version))
       (sha256
        (base32 "1p5xgd76585i55zcwvsi2gqhl0br9gbw398ap7m0cvadxfa6nxch"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_stock_landed_cost"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-product" ,trytond-product)
       ("trytond-stock" ,trytond-stock)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-stock-landed-cost")
    (synopsis "Tryton module for landed cost")
    (description "The @emph{Account Stock Landed Cost} Tryton module allows to
allocate landed cost on Supplier Shipments after their reception.")
    (license license:gpl3+)))

(define-public trytond-account-stock-landed-cost-weight
  (package
    (name "trytond-account-stock-landed-cost-weight")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_stock_landed_cost_weight" version))
       (sha256
        (base32 "10i0ww3k2cgdg227lv6h8ag2j6rm07maylbh3n0grwxfy8dbq34m"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_stock_landed_cost_weight"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account-stock-landed-cost" ,trytond-account-stock-landed-cost)
       ("trytond-product" ,trytond-product)
       ("trytond-product-measurements" ,trytond-product-measurements)
       ("trytond-stock-shipment-measurements"
        ,trytond-stock-shipment-measurements)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-stock-landed-cost-weight")
    (synopsis "Tryton module for landed cost per weight")
    (description "The @emph{Account Stock Landed Cost Weight} Tryton module
adds an allocation method based on weight of each line.  The Weight is taken
from the Product Measurements")
    (license license:gpl3+)))

(define-public trytond-account-tax-cash
  (package
    (name "trytond-account-tax-cash")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_tax_cash" version))
       (sha256
        (base32 "1di8brrj4jpx99i0553whyh2fddayvwq06dwdshb3iibgv4357cr"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_tax_cash"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-party" ,trytond-party)))
    (home-page "https://docs.tryton.org/projects/modules-account-tax-cash")
    (synopsis "Tryton module to support tax report on cash basis")
    (description "The @emph{Account Tax Cash} Tryton module allows to make tax
report on cash basis.")
    (license license:gpl3+)))

(define-public trytond-account-tax-rule-country
  (package
    (name "trytond-account-tax-rule-country")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_account_tax_rule_country" version))
       (sha256
        (base32 "0gy2zp023d2wqcivaiw5db5d8f4wrpqdnd6jywjhpkvqk4ay6i27"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "account_tax_rule_country"))
    (native-inputs
     `(,@%standard-trytond-native-inputs
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-purchase" ,trytond-purchase)
       ("trytond-sale" ,trytond-sale)
       ("trytond-stock" ,trytond-stock)))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-country" ,trytond-country)))
    (home-page
     "https://docs.tryton.org/projects/modules-account-tax-rule-country")
    (synopsis "Tryton module to add countries on tax rules")
    (description "The @emph{Account Tax Rule Country} Tryton module extends
the tax rule to add origin and destination countries and subdivisions as
criteria.")
    (license license:gpl3+)))

(define-public trytond-analytic-account
  (package
    (name "trytond-analytic-account")
    (version "6.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_analytic_account" version))
       (sha256
        (base32 "09j9xz41n5hk3j7w63xbw1asd3p00prqvl652qcm9x1nrlmqiw3r"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "analytic_account"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-company" ,trytond-company)
       ("trytond-currency" ,trytond-currency)
       ("trytond-party" ,trytond-party)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for analytic accounting")
    (description
     "This package provides a Tryton module that adds the fundamentals
required to analyse accounting using multiple different axes.")
    (license license:gpl3+)))

(define-public python-trytond-analytic-account
  (deprecated-package
   "python-trytond-analytic-account" trytond-analytic-account))

(define-public trytond-analytic-invoice
  (package
    (name "trytond-analytic-invoice")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_analytic_invoice" version))
       (sha256
        (base32 "151vwcn5sgpqma9kjxbznx4v4wlhfc97dcyb432brxnswf5glcir"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "analytic_invoice"))
    (native-inputs
     `(,@%standard-trytond-native-inputs
       ("trytond-account-asset" ,trytond-account-asset)))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-analytic-account" ,trytond-analytic-account)))
    (home-page "https://docs.tryton.org/projects/modules-analytic-invoice")
    (synopsis "Tryton module to add analytic accounting on invoice")
    (description "The @emph{Analytic Invoice} Tryton module allows to set
analytic accounts on invoice line.")
    (license license:gpl3+)))

(define-public trytond-analytic-purchase
  (package
    (name "trytond-analytic-purchase")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_analytic_purchase" version))
       (sha256
        (base32 "1yx3w3p98y11qw99jy02kal9393y6jpxazsfcznsffgigw2vl3y6"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "analytic_purchase"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-analytic-account" ,trytond-analytic-account)
       ("trytond-analytic-invoice" ,trytond-analytic-invoice)
       ("trytond-purchase" ,trytond-purchase)))
    (home-page "https://docs.tryton.org/projects/modules-analytic-purchase")
    (synopsis "Tryton module to add analytic accounting on purchase")
    (description "The @emph{Analytic Purchase} Tryton module allows to set
analytic accounts on purchase line.")
    (license license:gpl3+)))

(define-public trytond-analytic-sale
  (package
    (name "trytond-analytic-sale")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_analytic_sale" version))
       (sha256
        (base32 "07l97jmg67468pihymfcjagqbbfdcmry3654f24zbmnljxy2qm1x"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "analytic_sale"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-analytic-account" ,trytond-analytic-account)
       ("trytond-analytic-invoice" ,trytond-analytic-invoice)
       ("trytond-sale" ,trytond-sale)))
    (home-page "https://docs.tryton.org/projects/modules-analytic-sale")
    (synopsis "Tryton module to add analytic accounting on sale")
    (description "The @emph{Analytic Sale} Tryton module allows to set
analytic accounts on sale line.")
    (license license:gpl3+)))

(define-public trytond-attendance
  (package
    (name "trytond-attendance")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_attendance" version))
       (sha256
        (base32 "1kwbxblpwjw9sv9axfp29vqgdkwkzf0c0vw8qx1fx8mfwv1hba5c"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "attendance"))
    (native-inputs
     `(,@%standard-trytond-native-inputs
       ("trytond-timesheet" ,trytond-timesheet)))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-company" ,trytond-company)))
    (home-page "https://docs.tryton.org/projects/modules-attendance")
    (synopsis "Tryton module for recording employee attendance")
    (description "The @emph{Attendance} Tryton module allows you to track the
entry and exit time of employees.  The module also comes with a sheet that
shows for each employee the total duration per day in the company and the
detail of the time of entrance and exit")
    (license license:gpl3+)))

(define-public trytond-authentication-sms
  (package
    (name "trytond-authentication-sms")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_authentication_sms" version))
       (sha256
        (base32 "1g46mimgjkz2lvh90p2ffmkfgwl7w03iqnvqlcghwpxk5vyxw3sj"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "authentication_sms"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)))
    (home-page "https://docs.tryton.org/projects/modules-authentication-sms")
    (synopsis "Tryton module to authenticate users via SMS")
    (description "The @emph{Authentication SMS} Tryton module allows users to
authenticate via SMS.  It adds a new authentication method sms, which can be
used in the list of authentications in the session section of the
configuration file.")
    (license license:gpl3+)))

(define-public trytond-bank
  (package
    (name "trytond-bank")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_bank" version))
       (sha256
        (base32 "0qwgp2s88n4hcqqxg1g34lmmvq5spdlkm978gzn6s96kmmzang0c"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "bank"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("python-stdnum" ,python-stdnum)
       ("trytond" ,trytond)
       ("trytond-currency" ,trytond-currency)
       ("trytond-party" ,trytond-party)))
    (home-page "https://docs.tryton.org/projects/modules-bank")
    (synopsis "Tryton module with banks")
    (description "The @emph{Bank} Tryton module defines the concept of bank
and account.")
    (license license:gpl3+)))

(define-public trytond-carrier
  (package
    (name "trytond-carrier")
    (version "6.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_carrier" version))
       (sha256
        (base32 "0kp08jb5f86bzfyn99qs4k1047svdrkhz3jxv3jw46vrpc2s0c1y"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "carrier"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-country" ,trytond-country)
       ("trytond-party" ,trytond-party)
       ("trytond-product" ,trytond-product)))
    (home-page "https://docs.tryton.org/projects/modules-carrier")
    (synopsis "Tryton module with carriers")
    (description "The @emph{Carrier} Tryton module defines the concept
of carrier.")
    (license license:gpl3+)))

(define-public trytond-carrier-percentage
  (package
    (name "trytond-carrier-percentage")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_carrier_percentage" version))
       (sha256
        (base32 "0glgjf733qliqxbr1sykxn2rlphagnpixhkg84my7bv6cxfmvkh6"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "carrier_percentage"))
    (native-inputs
     `(,@%standard-trytond-native-inputs
       ("trytond-purchase-shipment-cost" ,trytond-purchase-shipment-cost)
       ("trytond-sale-shipment-cost" ,trytond-sale-shipment-cost)))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-carrier" ,trytond-carrier)
       ("trytond-currency" ,trytond-currency)))
    (home-page "https://docs.tryton.org/projects/modules-carrier-percentage")
    (synopsis "Tryton module to add cost method based on percentage")
    (description "The @emph{Carrier Percentage} Tryton module adds a cost
method \"on percentage\" on carrier.")
    (license license:gpl3+)))

(define-public trytond-carrier-subdivision
  (package
    (name "trytond-carrier-subdivision")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_carrier_subdivision" version))
       (sha256
        (base32 "1mmbz541yywdgm16ij2xfvm50xad24plmqdb739ssg6jj0skmji4"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "carrier_subdivision"))
    (native-inputs
     `(,@%standard-trytond-native-inputs
       ("trytond-sale-shipment-cost" ,trytond-sale-shipment-cost)))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-carrier" ,trytond-carrier)))
    (home-page "https://docs.tryton.org/projects/modules-carrier-subdivision")
    (synopsis "Tryton module that allows carriers selection to be restricted
by subdivision")
    (description "The @emph{Carrier Subdivision} Tryton module extends the
carrier selection pattern with

@itemize
@item the warehouse Subdivision,
@item the customer Subdivision,
@item a regular expression to match against warehouse postal code and
@item A regular expression to match against customer postal code.
@end itemize

These can be used to restrict the usage of a carrier to a specific subdivision
or a specific postal code.")
    (license license:gpl3+)))

(define-public trytond-carrier-weight
  (package
    (name "trytond-carrier-weight")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_carrier_weight" version))
       (sha256
        (base32 "0l2gg4syym7jk0vjqnw7invh0gngrfmg9zmmvc1k6110aryr77yi"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "carrier_weight"))
    (native-inputs
     `(,@%standard-trytond-native-inputs
       ("trytond-purchase-shipment-cost" ,trytond-purchase-shipment-cost)
       ("trytond-sale-shipment-cost" ,trytond-sale-shipment-cost)))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-carrier" ,trytond-carrier)
       ("trytond-company" ,trytond-company)
       ("trytond-currency" ,trytond-currency)
       ("trytond-product" ,trytond-product)
       ("trytond-product-measurements" ,trytond-product-measurements)))
    (home-page "https://docs.tryton.org/projects/modules-carrier-weight")
    (synopsis "Tryton module to add cost method based on weight")
    (description "The @emph{Carrier Weight} Tryton module adds a cost method
\"on weight\" on carrier.  The price is computed by finding the line for which
the weight is greater or equal but smaller than the next line.")
    (license license:gpl3+)))

(define-public trytond-commission
  (package
    (name "trytond-commission")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_commission" version))
       (sha256
        (base32 "0a5grn6gvvwn0prr5fpgyj4kz283w08a8svmv3b4a4c5pchik3hv"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "commission"))
    (native-inputs
     `(,@%standard-trytond-native-inputs
       ("trytond-sale" ,trytond-sale)))
    (propagated-inputs
     `(("python-simpleeval" ,python-simpleeval)
       ("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-account-product" ,trytond-account-product)
       ("trytond-party" ,trytond-party)
       ("trytond-product" ,trytond-product)))
    (home-page "https://docs.tryton.org/projects/modules-commission")
    (synopsis "Tryton module for commission")
    (description "The @emph{Commission} Tryton module allows to manageq
commission for sale's agents.  A commission move is created when posting the
invoice, following the agent's commission plan.")
    (license license:gpl3+)))

(define-public trytond-company
  (package
    (name "trytond-company")
    (version "6.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_company" version))
       (sha256
        (base32 "1q4qdyg32dn00pn3pj2yjl3jhxaqpv7a1cv5s5c95cpy5p46p02n"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "company"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-currency" ,trytond-currency)
       ("trytond-party" ,trytond-party)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module with companies and employees")
    (description
     "This package provides a Tryton module that defines the concepts of
company and employee and extend the user model.")
    (license license:gpl3+)))

(define-public python-trytond-company
  (deprecated-package "python-trytond-company" trytond-company))

(define-public trytond-country
  (package
    (name "trytond-country")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_country" version))
       (sha256
        (base32 "1ksinysac7p0k8avsz8xqzfkmm21s6i93qyrsma5h4y5477cwmw7"))))
    (build-system python-build-system)
    ;; Doctest contains one test that requires internet access.
    (arguments (tryton-arguments "country" "--no-doctest"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("python-pycountry" ,python-pycountry)
       ("trytond" ,trytond)))
    (home-page "http://www.tryton.org/")
    (synopsis "Tryton module with countries")
    (description
     "This package provides a Tryton module with countries.")
    (license license:gpl3+)))

(define-public python-trytond-country
  (deprecated-package "python-trytond-country" trytond-country))

(define-public trytond-currency
  (package
    (name "trytond-currency")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_currency" version))
       (sha256
        (base32 "0fs2wvhgvc0l4yzs5m9l8z4lbzazr42hgz0859malhnlp1sya2kq"))))
    (build-system python-build-system)
    ;; Doctest 'scenario_currency_rate_update.rst' fails.
    (arguments (tryton-arguments "currency" "--no-doctest"))
    (native-inputs
     `(,@%standard-trytond-native-inputs
       ("python-forex-python" ,python-forex-python)
       ("python-pycountry" ,python-pycountry)))
    (propagated-inputs
     `(("python-sql" ,python-sql)
       ("trytond" ,trytond)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module with currencies")
    (description
     "This package provides a Tryton module that defines the concepts of
currency and rate.")
    (license license:gpl3+)))

(define-public python-trytond-currency
  (deprecated-package "python-trytond-currency" trytond-currency))

(define-public trytond-party
  (package
    (name "trytond-party")
    (version "6.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_party" version))
       (sha256
        (base32 "0aikzpr0ambc98v76dl6xqa42b08dy3b011y33lvxjp5mcha3f7y"))))
    (build-system python-build-system)
    ;; Doctest 'scenario_party_phone_number.rst' fails.
    (arguments (tryton-arguments "party" "--no-doctest"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("python-stdnum" ,python-stdnum)
       ("trytond" ,trytond)
       ("trytond-country" ,trytond-country)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for parties and addresses")
    (description
     "This package provides a Tryton module for (counter)parties and
addresses.")
    (license license:gpl3+)))

(define-public python-trytond-party
  (deprecated-package "python-trytond-party" trytond-party))

(define-public trytond-product
  (package
    (name "trytond-product")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_product" version))
       (sha256
        (base32 "1xvvqxkvzyqy6fn2sj5h3zj0g17igzwx6s18sxkdz72vqz6kpv0l"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "product"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("python-stdnum" ,python-stdnum)
       ("trytond" ,trytond)
       ("trytond-company" ,trytond-company)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module with products")
    (description
     "This package provides a Tryton module that defines two concepts: Product
Template and Product.")
    (license license:gpl3+)))

(define-public python-trytond-product
  (deprecated-package "python-trytond-product" trytond-product))

(define-public trytond-purchase
  (package
    (name "trytond-purchase")
    (version "6.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_purchase" version))
       (sha256
        (base32 "12drjw30ik3alckn6xrny4814vzi3ysh17wgiawiy9319yahsvay"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "purchase"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-account-invoice" ,trytond-account-invoice)
       ("trytond-account-invoice-stock" ,trytond-account-invoice-stock)
       ("trytond-account-product" ,trytond-account-product)
       ("trytond-company" ,trytond-company)
       ("trytond-currency" ,trytond-currency)
       ("trytond-party" ,trytond-party)
       ("trytond-product" ,trytond-product)
       ("trytond-stock" ,trytond-stock)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for purchase")
    (description
     "This package provides a Tryton module that defines the Purchase model.")
    (license license:gpl3+)))

(define-public python-trytond-purchase
  (deprecated-package "python-trytond-purchase" trytond-purchase))

(define-public trytond-purchase-request
  (package
    (name "trytond-purchase-request")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_purchase_request" version))
       (sha256
        (base32 "0yhf3lh5b24qpk80r5pbmmswf5757bxa0s7ckl40vf6lkjkccv5i"))))
    (build-system python-build-system)
    ;; Doctest 'scenario_purchase_request.rst' fails.
    (arguments (tryton-arguments "purchase_request" "--no-doctest"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-product" ,trytond-product)
       ("trytond-purchase" ,trytond-purchase)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for purchase requests")
    (description
     "This package provides a Tryton module that introduces the concept of
Purchase Requests which are central points to collect purchase requests
generated by other process from Tryton.")
    (license license:gpl3+)))

(define-public python-trytond-purchase-request
  (deprecated-package
   "python-trytond-purchase-request" trytond-purchase-request))

(define-public trytond-stock
  (package
    (name "trytond-stock")
    (version "6.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock" version))
       (sha256
        (base32 "1v6pvkwj6vhjqbz2zn0609kb7kx4g0dsn1xhvax4z2dqigh7ywpx"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "stock"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("python-simpleeval" ,python-simpleeval)
       ("trytond" ,trytond)
       ("trytond-company" ,trytond-company)
       ("trytond-currency" ,trytond-currency)
       ("trytond-party" ,trytond-party)
       ("trytond-product" ,trytond-product)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for stock and inventory")
    (description
     "This package provides a Tryton module that defines the fundamentals for
all stock management situations: Locations where products are stored, moves
between these locations, shipments for product arrivals and departures and
inventory to control and update stock levels.")
    (license license:gpl3+)))

(define-public python-trytond-stock
  (deprecated-package "python-trytond-stock" trytond-stock))

(define-public trytond-stock-lot
  (package
    (name "trytond-stock-lot")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_lot" version))
       (sha256
        (base32 "18cwrvnrzjk1wb765gr6hp3plpdpwz1a7cwimjhxi47iw7w5c84g"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "stock_lot"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-product" ,trytond-product)
       ("trytond-stock" ,trytond-stock)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for lot of products")
    (description
     "This package provides a Tryton module that defines lot of products.")
    (license license:gpl3+)))

(define-public python-trytond-stock-lot
  (deprecated-package "python-trytond-stock-lot" trytond-stock-lot))

(define-public trytond-stock-supply
  (package
    (name "trytond-stock-supply")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trytond_stock_supply" version))
       (sha256
        (base32 "1p5l3yjjy6l25kk9xnhbl691l3v8gfg9fhc87jc6qszhxlqxk730"))))
    (build-system python-build-system)
    (arguments (tryton-arguments "stock_supply"))
    (native-inputs `(,@%standard-trytond-native-inputs))
    (propagated-inputs
     `(("trytond" ,trytond)
       ("trytond-account" ,trytond-account)
       ("trytond-party" ,trytond-party)
       ("trytond-product" ,trytond-product)
       ("trytond-purchase" ,trytond-purchase)
       ("trytond-purchase-request" ,trytond-purchase-request)
       ("trytond-stock" ,trytond-stock)))
    (home-page "https://www.tryton.org/")
    (synopsis "Tryton module for stock supply")
    (description
     "This package provides a Tryton module that adds automatic supply
mechanisms and introduces the concepts of order point.")
    (license license:gpl3+)))

(define-public python-trytond-stock-supply
  (deprecated-package "python-trytond-stock-supply" trytond-stock-supply))
