;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xml))

(define-public gnucash
  (package
    (name "gnucash")
    (version "2.6.9")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/gnucash/gnucash-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "0iw25l1kv60cg6fd2vg11mcvzmjqnc5p9lp3rjy06ghkjfrn3and"))
      (patches (list (search-patch "gnucash-price-quotes-perl.patch")))))
    (build-system gnu-build-system)
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
       ("perl-date-manip" ,perl-date-manip)
       ("perl-finance-quote" ,perl-finance-quote)))
    (native-inputs
     `(("glib" ,glib "bin") ; glib-compile-schemas, etc.
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (arguments
     `(#:tests? #f ;FIXME: failing at /qof/gnc-date/qof print date dmy buff
       #:configure-flags '("--disable-dbi")
       #:phases
       (modify-phases %standard-phases
         (add-after
          'install 'wrap-programs
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
    (home-page "https://gnu.org/software/gnucash")
    (synopsis "Personal and small business financial accounting software")
    (description
     "GnuCash is personal and professional financial-accounting software.
It can be used to track bank accounts, stocks, income and expenses, based on
the double-entry accounting practice.  It includes support for QIF/OFX/HBCI
import and transaction matching.  It also automates several tasks, such as
financial calculations or scheduled transactions.")
    (license gpl3+)))
