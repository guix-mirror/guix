;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages gnucash)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xml))

(define-public gnucash
  (package
    (name "gnucash")
    (version "2.6.18")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/gnucash/gnucash%20%28stable%29/"
                          version "/gnucash-" version "-1.tar.bz2"))
      (sha256
       (base32
        "1794qi7lkn1kbnhzk08wawacfcphbln3ngdl3q0qax5drv7hnwv8"))
      (patches (search-patches "gnucash-price-quotes-perl.patch"))))
    (build-system glib-or-gtk-build-system)
    (inputs
     `(("guile" ,guile-2.0)
       ("icu4c" ,icu4c)
       ("glib" ,glib)
       ("gtk" ,gtk+-2)
       ("goffice" ,goffice-0.8)
       ("libgnomecanvas" ,libgnomecanvas)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("webkitgtk" ,webkitgtk/gtk+-2)
       ("aqbanking" ,aqbanking)
       ("perl-date-manip" ,perl-date-manip)
       ("perl-finance-quote" ,perl-finance-quote)))
    (native-inputs
     `(("glib" ,glib "bin") ; glib-compile-schemas, etc.
       ("intltool" ,intltool)
       ("gnucash-docs" ,gnucash-docs)
       ("pkg-config" ,pkg-config)))
    (outputs '("out" "doc"))
    (arguments
     `(#:tests? #f ;FIXME: failing at /qof/gnc-date/qof print date dmy buff
       #:configure-flags '("--disable-dbi"
                           "--enable-aqbanking")
       #:phases
       (modify-phases %standard-phases
         ;; There are about 100 megabytes of documentation.
         (add-after
          'install 'install-docs
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((docs (assoc-ref inputs "gnucash-docs"))
                  (doc-output (assoc-ref outputs "doc")))
              (symlink (string-append docs "/share/gnome")
                       (string-append doc-output "/share/gnome")))))
         (add-after
          'install-docs 'wrap-programs
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (for-each (lambda (prog)
                        (wrap-program (string-append (assoc-ref outputs "out")
                                                     "/bin/" prog)
                          `("PERL5LIB" ":" prefix
                            ,(map (lambda (o)
                                    (string-append o "/lib/perl5/site_perl/"
                                                   ,(package-version perl)))
                                  (if (string=? prog "gnc-fq-helper")
                                      (list
                                       ,@(transitive-input-references
                                          'inputs
                                          (map (lambda (l)
                                                 (assoc l (inputs)))
                                               '("perl-finance-quote"
                                                 "perl-date-manip"))))
                                      (list
                                       ,@(transitive-input-references
                                          'inputs
                                          (map (lambda (l)
                                                 (assoc l (inputs)))
                                               '("perl-finance-quote")))))))))
                      '("gnucash"
                        "gnc-fq-check"
                        "gnc-fq-helper"
                        "gnc-fq-dump")))))))
    (home-page "https://www.gnucash.org/")
    (synopsis "Personal and small business financial accounting software")
    (description
     "GnuCash is personal and professional financial-accounting software.
It can be used to track bank accounts, stocks, income and expenses, based on
the double-entry accounting practice.  It includes support for QIF/OFX/HBCI
import and transaction matching.  It also automates several tasks, such as
financial calculations or scheduled transactions.")
    (license license:gpl3+)))

;; This package is not public, since we use it to build the "doc" output of
;; the gnucash package (see above).  It would be confusing if it were public.
(define gnucash-docs
  (package
    (name "gnucash-docs")
    (version (package-version gnucash))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/gnucash/gnucash-docs/"
                           version "/gnucash-docs-" version ".tar.gz"))
       (sha256
        (base32
         "0bzc7mvdba2sc8m61yxa1fp2liqs00b64dvfxhv854vdrl1z5vmb"))))
    (build-system gnu-build-system)
    ;; These are native-inputs because they are only required for building the
    ;; documentation.
    (native-inputs
     `(("libxml2" ,libxml2)
       ;; The "check" target needs the docbook xml packages for validating the
       ;; DocBook XML during the tests.
       ("docbook-xml-4.4" ,docbook-xml-4.4)
       ("docbook-xml-4.2" ,docbook-xml-4.2)
       ("docbook-xml-4.1.2" ,docbook-xml-4.1.2)
       ("libxslt" ,libxslt)
       ("docbook-xsl" ,docbook-xsl)
       ("scrollkeeper" ,scrollkeeper)))
    (home-page "https://www.gnucash.org/")
    (synopsis "Documentation for GnuCash")
    (description
     "User guide and other documentation for GnuCash in various languages.
This package exists because the GnuCash project maintains its documentation in
an entirely separate package from the actual GnuCash program.  It is intended
to be read using the GNOME Yelp program.")
    (license (list license:fdl1.1+ license:gpl3+))))

(define-public gwenhywfar
  (package
    (name "gwenhywfar")
    (version "4.15.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.aquamaniac.de/sites/download/download.php?"
                           "package=01&release=201&file=01"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0fp67s932x66xfljb26zbrn8ambbc5y5c3hllr6l284nr63qf3ka"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-network-checks"
             ;; Both GTK+2 and QT4 are supported.
             "--with-guis=gtk2"
             (string-append "--with-openssl-includes="
                            (assoc-ref %build-inputs "openssl") "/include")
             (string-append "--with-openssl-libs="
                            (assoc-ref %build-inputs "openssl") "/lib"))))
    (inputs
     `(("libgcrypt" ,libgcrypt)
       ("gnutls" ,gnutls)
       ("openssl" ,openssl)
       ("gtk+" ,gtk+-2)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.aquamaniac.de/sites/aqbanking/index.php")
    (synopsis "Utility library for networking and security applications")
    (description
     "This package provides a helper library for networking and security
applications and libraries.  It is used by AqBanking.")
    ;; The license includes an explicit additional permission to compile and
    ;; distribute this library with the OpenSSL Toolkit.
    (license license:lgpl2.1+)))

(define-public aqbanking
  (package
    (name "aqbanking")
    (version "5.6.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.aquamaniac.de/sites/download/download.php?"
                           "package=03&release=208&file=01"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "08jbwmiv6f3v8iqdr44x4szna496fqcjfi6mlx04cnbx91m70lh6"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Parallel building fails because aqhbci is required before it's
       ;; built.
       #:parallel-build? #f
       #:configure-flags
       (list (string-append "--with-gwen-dir="
                            (assoc-ref %build-inputs "gwenhywfar")))))
    (propagated-inputs
     `(("gwenhywfar" ,gwenhywfar)))
    (inputs
     `(("gmp" ,gmp)
       ("xmlsec" ,xmlsec)
       ("gnutls" ,gnutls)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("libltdl" ,libltdl)))
    (home-page "http://www.aquamaniac.de/sites/aqbanking/index.php")
    (synopsis "Interface for online banking tasks")
    (description
     "AqBanking is a modular and generic interface to online banking tasks,
financial file formats (import/export) and bank/country/currency information.
AqBanking uses backend plugins to actually perform the online tasks.  HBCI,
OFX DirectConnect, YellowNet, GeldKarte, and DTAUS discs are currently
supported.  AqBanking is used by GnuCash, KMyMoney, and QBankManager.")
    ;; AqBanking is licensed under the GPLv2 or GPLv3
    (license (list license:gpl2 license:gpl3))))
