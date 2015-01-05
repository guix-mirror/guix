;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages cups)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages fonts) ;font-dejavu
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config))

(define-public cups-filters
  (package
    (name "cups-filters")
    (version "1.0.61")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "http://openprinting.org/download/cups-filters/"
                              "cups-filters-" version ".tar.xz"))
              (sha256
               (base32
                "1bq48nnrarlbf6qc93bz1n5wlh6j420gppbck3r45sinwhz5wa7m"))
              (modules '((guix build utils)))
              (snippet
               ;; install backends, banners and filters to cups-filters output
               ;; directory, not the cups server directory
               '(substitute* "Makefile.in"
                  (("CUPS_DATADIR = @CUPS_DATADIR@")
                   "CUPS_DATADIR = $(PREFIX)/share/cups")
                  (("pkgcupsserverrootdir = \\$\\(CUPS_SERVERROOT\\)")
                   "pkgcupsserverrootdir = $(PREFIX)")
                  (("pkgbackenddir = \\$\\(CUPS_SERVERBIN\\)/backend")
                   "pkgbackenddir = $(PREFIX)/backend")
                  (("pkgfilterdir = \\$\\(CUPS_SERVERBIN\\)/filter")
                   "pkgfilterdir = $(PREFIX)/filter")))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" %output))
       #:configure-flags
       `(,(string-append "--with-test-font-path="
                         (assoc-ref %build-inputs "font-dejavu")
                         "/share/fonts/truetype/DejaVuSans.ttf")
         ,(string-append "--with-rcdir="
                         (assoc-ref %outputs "out") "/etc/rc.d"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("fontconfig"   ,fontconfig)
       ("freetype"     ,freetype)
       ("font-dejavu"  ,font-dejavu) ;needed by test suite
       ("ghostscript"  ,ghostscript)
       ("ijs"          ,ijs)
       ("dbus"         ,dbus)
       ("lcms"         ,lcms)
       ("libjpeg-8"    ,libjpeg-8)
       ("libpng"       ,libpng)
       ("libtiff"      ,libtiff)
       ("glib"         ,glib)
       ("qpdf"         ,qpdf)
       ("poppler"      ,poppler)
       ("cups-minimal" ,cups-minimal)))
    (home-page "http://www.linuxfoundation.org/collaborate/workgroups/openprinting/cups-filters")
    (synopsis "OpenPrinting CUPS filters and backends")
    (description
     "Contains backends, filters, and other software that was once part of the
core CUPS distribution but is no longer maintained by Apple Inc.  In addition
it contains additional filters developed independently of Apple, especially
filters for the PDF-centric printing workflow introduced by OpenPrinting.")
    ;; Different filters and backends have different licenses; see COPYING for
    ;; details
    (license (list license:gpl2
                   license:gpl2+
                   license:gpl3
                   license:gpl3+
                   license:lgpl2.0+
                   license:expat))))

;; CUPS on non-MacOS systems requires cups-filters.  Since cups-filters also
;; depends on CUPS libraries and binaries, cups-minimal has been added to
;; satisfy this dependency.
(define-public cups-minimal
  (package
    (name "cups-minimal")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.cups.org/software/"
                                  version "/cups-" version "-source.tar.gz"))
              (sha256
               (base32
                "1kbc85kwhm1vyzahblrg3qih9yypggs91d13gdrbnaac8q7jd9jr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       '("--disable-launchd"
         "--disable-systemd"
         "--disable-avahi"
         "--disable-dnssd")
       ;; Seven tests fail, mostly because of files that are provided by the
       ;; cups-filters package.
       #:tests? #f
       #:phases
       (alist-cons-before
        'configure
        'patch-makedefs
        (lambda _
          (substitute* "Makedefs.in"
            (("INITDIR.*=.*@INITDIR@") "INITDIR = @prefix@/@INITDIR@")
            (("/bin/sh") (which "sh"))))
        (alist-cons-before
         'build
         'patch-tests
         (lambda _
           (substitute* "test/ippserver.c"
             (("#  else /\\* HAVE_AVAHI \\*/") "#elif defined(HAVE_AVAHI)")))
         %standard-phases))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("zlib"  ,zlib)
       ("gnutls" ,gnutls)))
    (home-page "http://www.cups.org")
    (synopsis "The Common Unix Printing System")
    (description
     "CUPS is a printing system that uses the Internet Printing
Protocol (IPP).  It provides System V and BSD command-line interfaces, as well
as a Web interface and a C programming interface to manage printers and print
jobs.  It supports printing to both local (parallel, serial, USB) and
networked printers, and printers can be shared from one computer to another.
Internally, CUPS uses PostScript Printer Description (PPD) files to describe
printer capabilities and features and a wide variety of generic and
device-specific programs to convert and print many types of files.")
    (license license:gpl2)))

(define-public cups
  (package (inherit cups-minimal)
    (name "cups")
    (arguments
     `(;; Three tests fail:
       ;; * two tests in ipp-1.1.test related to "RFC 2911 section 3.2.6:
       ;;   Get-Jobs Operation"
       ;; * test of number of error/warning messages, probably related to a
       ;;   missing font.
       #:tests? #f
       #:configure-flags
       '("--disable-launchd"
         "--disable-systemd")
       #:phases
       (alist-cons-before
        'configure
        'patch-makedefs
        (lambda _
          (substitute* "Makedefs.in"
            (("INITDIR.*=.*@INITDIR@") "INITDIR = @prefix@/@INITDIR@")
            (("/bin/sh") (which "sh"))))
        (alist-cons-before
         'check
         'patch-tests
         (lambda _
           (let ((filters (assoc-ref %build-inputs "cups-filters"))
                 (catpath (string-append
                           (assoc-ref %build-inputs "coreutils") "/bin/"))
                 (testdir (string-append (getcwd) "/tmp/")))
             (mkdir testdir)
             (substitute* "test/run-stp-tests.sh"
               ((" *BASE=/tmp/") (string-append "BASE=" testdir))

               ;; allow installation of filters from output dir and from
               ;; cups-filters
               (("for dir in /usr/libexec/cups/filter /usr/lib/cups/filter")
                (string-append
                 "for dir in "
                 (assoc-ref %outputs "out") "/lib/cups/filter "
                 filters "/filter"))

               ;; check for charsets in cups-filters output
               (("/usr/share/cups/charsets")
                (string-append filters "/share/cups/charsets"))

               ;; install additional required filters
               (("instfilter texttopdf texttopdf pdf")
                (string-append
                 "instfilter texttopdf texttopdf pdf;"
                 "instfilter imagetoraster imagetoraster raster;"
                 "instfilter gstoraster gstoraster raster;"
                 "instfilter urftopdf urftopdf pdf;"
                 "instfilter rastertopdf rastertopdf pdf;"
                 "instfilter pstopdf pstopdf pdf"))

               ;; specify location of lpstat binary
               (("description=\"`lpstat -l")
                "description=\"`../systemv/lpstat -l")

               ;; patch shebangs of embedded scripts
               (("#!/bin/sh") (string-append "#!" (which "sh")))

               ;; also link mime definitions from cups-filters
               ;; to enable the additional filters for the test suite
               (("ln -s \\$root/conf/mime\\.types")
                (string-append
                 "ln -s " filters
                 "/share/cups/mime/cupsfilters.types $BASE/share/mime; "
                 "ln -s $root/conf/mime.types"))
               (("ln -s \\$root/conf/mime\\.convs")
                (string-append
                 "ln -s " filters
                 "/share/cups/mime/cupsfilters.convs $BASE/share/mime; "
                 "ln -s $root/conf/mime.convs")))

             ;; fix search path for "cat"
             (substitute* "cups/testfile.c"
               (("cupsFileFind\\(\"cat\", \"/bin\"")
                (string-append "cupsFileFind(\"cat\", \"" catpath "\""))
               (("cupsFileFind\\(\"cat\", \"/bin:/usr/bin\"")
                (string-append "cupsFileFind(\"cat\", \"" catpath "\"")))))
         (alist-cons-after
          'install
          'install-cups-filters-symlinks
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (cups-filters (assoc-ref inputs "cups-filters")))
              ;; charsets
              (symlink
               (string-append cups-filters "/share/cups/charsets")
               (string-append out "/share/charsets"))

              ;; mime types, driver file, ppds
              (for-each
               (lambda (f)
                 (symlink (string-append cups-filters f)
                          (string-append out f)))
               '("/share/cups/mime/cupsfilters.types"
                 "/share/cups/mime/cupsfilters.convs"
                 "/share/cups/drv/cupsfilters.drv"
                 "/share/ppd"))

              ;; filters
              (for-each
               (lambda (f)
                 (symlink f
                          (string-append out "/lib/cups/filter" (basename f))))
               (find-files (string-append cups-filters "/filter") ".*"))

              ;; backends
              (for-each
               (lambda (f)
                 (symlink (string-append cups-filters f)
                          (string-append out "/lib/cups" f)))
               '("/backend/parallel"
                 "/backend/serial"))

              ;; banners
              (let ((banners "/share/cups/banners"))
                (delete-file-recursively (string-append out banners))
                (symlink (string-append cups-filters banners)
                         (string-append out banners)))

              ;; assorted data
              (let ((data "/share/cups/data"))
                (delete-file-recursively (string-append out data))
                (symlink (string-append cups-filters data)
                         (string-append out data)))))
          %standard-phases)))))
    (inputs
     `(("avahi" ,avahi)
       ("gnutls" ,gnutls)
       ("cups-filters" ,cups-filters)
       ("zlib"  ,zlib)))))
