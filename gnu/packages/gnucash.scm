;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (guix build-system cmake)
  #:use-module ((guix records) #:select (this-record))
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
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
  ;; TODO: Unbundle libraries such as guile-json found under the "borrowed/"
  ;; directory.
  (package
    (name "gnucash")
    (version "3.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/gnucash/gnucash%20%28stable%29/"
                           version "/gnucash-" version ".tar.bz2"))
       (sha256
        (base32
         "1ms2wg4sh5gq3rpjmmnp85rh5nc9ahca1imxkvhz4d3yiwy8hm52"))
       (patches (search-patches "gnucash-fix-test-transaction-failure.patch"))))
    (build-system cmake-build-system)
    (inputs
     `(("guile" ,guile-2.2)
       ("boost" ,boost)
       ("icu4c" ,icu4c)
       ("glib" ,glib)
       ("gtk" ,gtk+)
       ("libgnomecanvas" ,libgnomecanvas)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("webkitgtk" ,webkitgtk)
       ("aqbanking" ,aqbanking)
       ("perl-date-manip" ,perl-date-manip)
       ("perl-finance-quote" ,perl-finance-quote)
       ("tzdata" ,tzdata-for-tests)))
    (native-inputs
     `(("glib" ,glib "bin")             ; glib-compile-schemas, etc.
       ("intltool" ,intltool)
       ("googlemock" ,(package-source googletest))
       ("googletest" ,googletest)
       ("gnucash-docs" ,gnucash-docs)
       ("pkg-config" ,pkg-config)))
    (outputs '("out" "doc" "debug"))
    (arguments
     `(#:test-target "check"
       #:configure-flags
       (list "-DWITH_OFX=OFF"           ; libofx is not available yet
             "-DWITH_SQL=OFF")          ; without dbi.h
       #:make-flags '("GUILE_AUTO_COMPILE=0")
       #:modules ((guix build cmake-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build cmake-build-system)
                           (guix build glib-or-gtk-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-gmock
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir "gmock")
             (copy-recursively (assoc-ref inputs "googlemock") "gmock")
             (setenv "GMOCK_ROOT" (string-append (getcwd) "/gmock/googlemock"))
             #t))
         (add-after 'unpack 'set-env-vars
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((tzdata (assoc-ref inputs "tzdata")))
               ;; At least one test is time-related and requires this
               ;; environment variable.
               (setenv "TZDIR"
                       (string-append tzdata
                                      "/share/zoneinfo"))
               (substitute* "CMakeLists.txt"
                 (("set\\(SHELL /bin/bash\\)")
                  (string-append "set(SHELL " (which "bash") ")")))
               #t)))
         ;; After wrapping gnc-fq-check and gnc-fq-helper we can no longer
         ;; execute them with perl, so execute them directly instead.
         (add-after 'unpack 'fix-finance-quote-check
           (lambda _
             (substitute* "libgnucash/scm/price-quotes.scm"
               (("\"perl\" \"-w\" ") ""))
             #t))
         ;; The test-stress-options unit test is known to fail, so we disable
         ;; it (see: https://bugs.gnucash.org/show_bug.cgi?id=796877).
         (add-after 'unpack 'disable-stress-options-test
           (lambda _
             (substitute* "gnucash/report/standard-reports/test/CMakeLists.txt"
               (("test-stress-options.scm") ""))
             #t))
         ;; The qof test requires the en_US, en_GB, and fr_FR locales.
         (add-before 'check 'install-locales
           (lambda _
             (setenv "LOCPATH" (getcwd))
             (invoke "localedef" "-i" "en_US" "-f" "UTF-8" "./en_US.UTF-8")
             (invoke "localedef" "-i" "en_GB" "-f" "UTF-8" "./en_GB.UTF-8")
             (invoke "localedef" "-i" "fr_FR" "-f" "UTF-8" "./fr_FR.UTF-8")
             #t))
         ;; There are about 100 megabytes of documentation.
         (add-after 'install 'install-docs
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((docs (assoc-ref inputs "gnucash-docs"))
                   (doc-output (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc-output "/share"))
               (symlink (string-append docs "/share/gnome")
                        (string-append doc-output "/share/gnome"))
               #t)))
         (add-after 'install-docs 'wrap-programs
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
                                                  (assoc l (package-inputs this-record)))
                                                '("perl-finance-quote"
                                                  "perl-date-manip"))))
                                       (list
                                        ,@(transitive-input-references
                                           'inputs
                                           (map (lambda (l)
                                                  (assoc l (package-inputs this-record)))
                                                '("perl-finance-quote")))))))))
                       '("gnucash"
                         "gnc-fq-check"
                         "gnc-fq-helper"
                         "gnc-fq-dump"))))
         (add-after 'install 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'install 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (home-page "https://www.gnucash.org/")
    (synopsis "Personal and small business financial accounting software")
    (description
     "GnuCash is personal and professional financial-accounting software.
It can be used to track bank accounts, stocks, income and expenses, based on
the double-entry accounting practice.  It includes support for QIF/OFX/HBCI
import and transaction matching.  It also automates several tasks, such as
financial calculations or scheduled transactions.

To make the GnuCash documentation available, its doc output must be
installed as well as Yelp, the Gnome help browser.")
    (license license:gpl3+)))

;; This package is not public, since we use it to build the "doc" output of
;; the gnucash package (see above).  It would be confusing if it were public.
(define gnucash-docs
  (let ((revision "a"))              ;set to the empty string when no revision
    (package
      (name "gnucash-docs")
      (version (package-version gnucash))
      (source
       (origin
         (method url-fetch)
         (uri (string-append "mirror://sourceforge/gnucash/gnucash%20%28stable%29/"
                             version "/gnucash-docs-" version revision ".tar.gz"))
         (sha256
          (base32
           "0bgjxpxgk7hy8ihn1kvd8p6vv191q5md2hz6jb9mqc4aykpvdlq7"))))
      (build-system gnu-build-system)
      ;; These are native-inputs because they are only required for building the
      ;; documentation.
      (native-inputs
       `(("libxml2" ,libxml2)
         ;; The "check" target needs the docbook xml package for validating the
         ;; DocBook XML during the tests.
         ("docbook-xml" ,docbook-xml)
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
      (license (list license:fdl1.1+ license:gpl3+)))))

(define-public gwenhywfar
  (package
    (name "gwenhywfar")
    (version "4.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.aquamaniac.de/sites/download/"
                           "download.php?package=01&release=208&file=02&"
                           "dummy=gwenhywfar-" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1c0g3f8jk6j693774ifslx2ds4ksabgbbalhhm9gk20kpamxm22s"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-network-checks"
             ;; GTK+3, GTK+2 and QT4 are supported.
             "--with-guis=gtk3"
             (string-append "--with-openssl-includes="
                            (assoc-ref %build-inputs "openssl") "/include")
             (string-append "--with-openssl-libs="
                            (assoc-ref %build-inputs "openssl") "/lib"))))
    (inputs
     `(("libgcrypt" ,libgcrypt)
       ("gnutls" ,gnutls)
       ("openssl" ,openssl)
       ("gtk+" ,gtk+)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.aquamaniac.de/sites/aqbanking/index.php")
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
    (version "5.7.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.aquamaniac.de/sites/download/"
                           "download.php?package=03&release=217&file=02&"
                           "dummy=aqbanking-" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0s67mysskbiw1h1p0np4ph4351r7wq3nc873vylam7lsqi66xy0n"))))
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
    (home-page "https://www.aquamaniac.de/sites/aqbanking/index.php")
    (synopsis "Interface for online banking tasks")
    (description
     "AqBanking is a modular and generic interface to online banking tasks,
financial file formats (import/export) and bank/country/currency information.
AqBanking uses backend plugins to actually perform the online tasks.  HBCI,
OFX DirectConnect, YellowNet, GeldKarte, and DTAUS discs are currently
supported.  AqBanking is used by GnuCash, KMyMoney, and QBankManager.")
    ;; AqBanking is licensed under the GPLv2 or GPLv3
    (license (list license:gpl2 license:gpl3))))
