;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fonts)     ; font-dejavu
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public cups-filters
  (package
    (name "cups-filters")
    (version "1.22.0")
    (source(origin
              (method url-fetch)
              (uri
               (string-append "https://openprinting.org/download/cups-filters/"
                              "cups-filters-" version ".tar.xz"))
              (sha256
               (base32
                "0gdv33g7dr1i7756n07zwgsv9b1i15rp7n1z1xr3n8f59br4fds4"))
              (modules '((guix build utils)))
              (snippet
               ;; install backends, banners and filters to cups-filters output
               ;; directory, not the cups server directory
               '(begin
                  (substitute* "Makefile.in"
                    (("CUPS_DATADIR = @CUPS_DATADIR@")
                     "CUPS_DATADIR = $(PREFIX)/share/cups")
                    (("pkgcupsserverrootdir = \\$\\(CUPS_SERVERROOT\\)")
                     "pkgcupsserverrootdir = $(PREFIX)")
                    ;; Choose standard directories notably so that binaries are
                    ;; stripped.
                    (("pkgbackenddir = \\$\\(CUPS_SERVERBIN\\)/backend")
                     "pkgbackenddir = $(PREFIX)/lib/cups/backend")
                    (("pkgfilterdir = \\$\\(CUPS_SERVERBIN\\)/filter")
                     "pkgfilterdir = $(PREFIX)/lib/cups/filter"))
                  ;; Find bannertopdf data such as the print test page in our
                  ;; output directory, not CUPS's prefix.
                  (substitute* "configure"
                    (("\\{CUPS_DATADIR\\}/data")
                     "{prefix}/share/cups/data"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" %output))
       #:configure-flags
       `("--disable-driverless" ; TODO: enable this
         "--disable-mutool"     ; depends on yet another PDF library (mupdf)

         ;; Look for the "domain socket of CUPS" in /var/run/cups.
         "--localstatedir=/var"

         ;; Free software for the win.
         "--with-acroread-path=evince"

         ,(string-append "--with-test-font-path="
                         (assoc-ref %build-inputs "font-dejavu")
                         "/share/fonts/truetype/DejaVuSans.ttf")
         ,(string-append "--with-gs-path="
                         (assoc-ref %build-inputs "ghostscript")
                         "/bin/gsc")
         ,(string-append "--with-shell="
                         (assoc-ref %build-inputs "bash")
                         "/bin/bash")
         ,(string-append "--with-rcdir="
                         (assoc-ref %outputs "out") "/etc/rc.d"))

       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-foomatic-hardcoded-file-names
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; Foomatic has hardcoded file names we need to fix.
                      (let ((out (assoc-ref outputs "out"))
                            (gs  (assoc-ref inputs "ghostscript")))
                        (substitute* "filter/foomatic-rip/foomaticrip.c"
                          (("/usr/local/lib/cups/filter")
                           (string-append out "/lib/cups/filter")))
                        #t)))
                  (add-after 'install 'wrap-filters
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; Some filters expect to find 'gs' in $PATH.  We cannot
                      ;; just hard-code its absolute file name in the source
                      ;; because foomatic-rip, for example, has tests like
                      ;; 'startswith(cmd, "gs")'.
                      (let ((out         (assoc-ref outputs "out"))
                            (ghostscript (assoc-ref inputs "ghostscript")))
                        (for-each (lambda (file)
                                    (wrap-program file
                                      `("PATH" ":" prefix
                                        (,(string-append ghostscript
                                                         "/bin")))))
                                  (find-files (string-append
                                               out "/lib/cups/filter")))
                        #t))))))
    (native-inputs
     `(("glib" ,glib "bin") ; for gdbus-codegen
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("avahi"        ,avahi)
       ("fontconfig"   ,fontconfig)
       ("freetype"     ,freetype)
       ("font-dejavu"  ,font-dejavu) ; also needed by test suite
       ("ghostscript"  ,ghostscript/cups)
       ("ijs"          ,ijs)
       ("dbus"         ,dbus)
       ("lcms"         ,lcms)
       ("libjpeg"      ,libjpeg)
       ("libpng"       ,libpng)
       ("libtiff"      ,libtiff)
       ("glib"         ,glib)
       ("qpdf"         ,qpdf)
       ("poppler"      ,poppler)
       ("cups-minimal" ,cups-minimal)))
    (home-page "https://wiki.linuxfoundation.org/openprinting/cups-filters")
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
    (version "2.2.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/apple/cups/releases/download/v"
                           version "/cups-" version "-source.tar.gz"))
       (sha256
        (base32
         "1fq52aw1mini3ld2czv5gg37wbbvh4n7yc7wzzxvbs3zpfrv5j3p"))))
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
       (modify-phases %standard-phases
         (add-before 'configure 'patch-makedefs
           (lambda _
             (substitute* "Makedefs.in"
               (("INITDIR.*=.*@INITDIR@") "INITDIR = @prefix@/@INITDIR@")
               (("/bin/sh") (which "sh")))
             #t))
         ;; Make the compressed manpages writable so that the
         ;; reset-gzip-timestamps phase does not error out.
         (add-before 'reset-gzip-timestamps 'make-manpages-writable
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man")))
               (for-each (lambda (file) (chmod file #o644))
                         (find-files man "\\.gz"))
               #t)))
         (add-before 'build 'patch-tests
           (lambda _
             (substitute* "test/ippserver.c"
               (("#  else /\\* HAVE_AVAHI \\*/")
                "#elif defined(HAVE_AVAHI)"))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("zlib"  ,zlib)
       ("gnutls" ,gnutls)))
    (home-page "https://www.cups.org")
    (synopsis "The Common Unix Printing System")
    (description
     "CUPS is a printing system that uses the Internet Printing Protocol
(@dfn{IPP}).  It provides System V and BSD command-line interfaces, as well
as a Web interface and a C programming interface to manage printers and print
jobs.  It supports printing to both local (parallel, serial, USB) and
networked printers, and printers can be shared from one computer to another.
Internally, CUPS uses PostScript Printer Description (@dfn{PPD}) files to
describe printer capabilities and features, and a wide variety of generic and
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
       (modify-phases %standard-phases
         (add-before 'configure 'patch-makedefs
           (lambda _
             (substitute* "Makedefs.in"
               (("INITDIR.*=.*@INITDIR@") "INITDIR = @prefix@/@INITDIR@")
               (("/bin/sh") (which "sh")))
             #t))
         (add-before 'check 'patch-tests
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
                   filters "/lib/cups/filter"))

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
                  (string-append "cupsFileFind(\"cat\", \"" catpath "\"")))
               #t)))
         ;; Make the compressed manpages writable so that the
         ;; reset-gzip-timestamps phase does not error out.
         (add-before 'reset-gzip-timestamps 'make-manpages-writable
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man")))
               (for-each (lambda (file) (chmod file #o644))
                         (find-files man "\\.gz"))
               #t)))
         (add-after 'install 'install-cups-filters-symlinks
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
                (find-files (string-append cups-filters "/lib/cups/filter")))

               ;; backends
               (for-each
                (lambda (f)
                  (symlink (string-append cups-filters f)
                           (string-append out "/lib/cups/backend/"
                                          (basename f))))
                '("/lib/cups/backend/parallel"
                  "/lib/cups/backend/serial"))

               ;; banners
               (let ((banners "/share/cups/banners"))
                 (delete-file-recursively (string-append out banners))
                 (symlink (string-append cups-filters banners)
                          (string-append out banners)))

               ;; assorted data
               (let ((data "/share/cups/data"))
                 (delete-file-recursively (string-append out data))
                 (symlink (string-append cups-filters data)
                          (string-append out data)))

               #t))))))
    (inputs
     `(("avahi" ,avahi)
       ("gnutls" ,gnutls)
       ("cups-filters" ,cups-filters)
       ("zlib"  ,zlib)))))

(define-public cups-pk-helper
  (package
    (name "cups-pk-helper")
    (version "0.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://freedesktop.org/software/"
                                  name "/releases/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0a52jw6rm7lr5nbyksiia0rn7sasyb5cjqcb95z1wxm2yprgi6lm"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("glib" ,glib)
       ("polkit" ,polkit)
       ("cups" ,cups)))
    (home-page "https://www.freedesktop.org/wiki/Software/cups-pk-helper/")
    (synopsis "PolicyKit helper to configure CUPS with fine-grained privileges")
    (description
     "This package provides the org.opensuse.CupsPkHelper.Mechanism DBus
system service which uses @file{cups-pk-helper-mechanism}.  This package
should only be used as part of the Guix cups-pk-helper service.")
    (license license:gpl2+)))

(define-public hplip
  (package
    (name "hplip")
    (version "3.18.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/hplip/hplip/" version
                                  "/hplip-" version ".tar.gz"))
              (sha256
               (base32
                "0g3q5mm2crjyc1z4z6gv4lam6sc5d3diz704djrnpqadk4q3h290"))
              (modules '((guix build utils)))
              (patches (search-patches "hplip-remove-imageprocessor.patch"))
              (snippet
               '(begin
                  ;; Delete non-free blobs
                  (for-each delete-file (find-files "." "\\.so$"))
                  (delete-file "prnt/hpcups/ImageProcessor.h")
                  ;; Fix type mismatch.
                  (substitute* "prnt/hpcups/genPCLm.cpp"
                    (("boolean") "bool"))
                  #t))))
    (build-system gnu-build-system)
    (home-page "https://developers.hp.com/hp-linux-imaging-and-printing")
    (synopsis "HP printer drivers")
    (description
     "Hewlett-Packard printer drivers and PostScript Printer Descriptions
(@dfn{PPD}s).")

    ;; The 'COPYING' file lists directories where each of these 3 licenses
    ;; applies.
    (license (list license:gpl2+ license:bsd-3 license:expat))

    ;; TODO install apparmor profile files eventually.
    (arguments
     `(#:configure-flags
       `("--disable-network-build"
         ,(string-append "--prefix=" (assoc-ref %outputs "out"))
         ,(string-append "--sysconfdir=" (assoc-ref %outputs "out") "/etc")
         ,(string-append "LDFLAGS=-Wl,-rpath="
                         (assoc-ref %outputs "out") "/lib")
         ;; Disable until mime.types merging works (FIXME).
         "--disable-fax-build"
         "--enable-hpcups-install"
         "--enable-new-hpcups"
         "--enable-cups_ppd_install"
         "--enable-cups_drv_install"
         ;; TODO add foomatic drv install eventually.
         ;; TODO --enable-policykit eventually.
         ,(string-append "--with-cupsfilterdir="
                         (assoc-ref %outputs "out") "/lib/cups/filter")
         ,(string-append "--with-cupsbackenddir="
                         (assoc-ref %outputs "out") "/lib/cups/backend")
         ,(string-append "--with-icondir="
                         (assoc-ref %outputs "out") "/share/applications")
         ,(string-append "--with-systraydir="
                         (assoc-ref %outputs "out") "/etc/xdg")
         "--enable-qt5" "--disable-qt4")

       #:imported-modules ((guix build python-build-system)
                           ,@%gnu-build-system-modules)
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  ((guix build python-build-system) #:prefix python:))

       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-hard-coded-file-names
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out"))
                            ;; FIXME: use merged ppds (I think actually only
                            ;; drvs need to be merged).
                            (cupsdir (assoc-ref inputs "cups-minimal")))
                        (substitute* "base/g.py"
                          (("'/usr/share;[^']*'")
                           (string-append "'" cupsdir "/share'"))
                          (("'/etc/hp/hplip.conf'")
                           (string-append "'" out
                                          "/etc/hp/hplip.conf" "'")))

                        (substitute* "Makefile.in"
                          (("[[:blank:]]check-plugin\\.py[[:blank:]]") " ")
                          ;; FIXME Use beginning-of-word in regexp.
                          (("[[:blank:]]plugin\\.py[[:blank:]]") " ")
                          (("/usr/include/libusb-1.0")
                           (string-append (assoc-ref inputs "libusb")
                                          "/include/libusb-1.0"))
                          (("hplip_statedir =.*$")
                           ;; Don't bail out while trying to create
                           ;; /var/lib/hplip.  We can safely change its value
                           ;; here because it's hard-coded in the code anyway.
                           "hplip_statedir = $(prefix)\n")
                          (("hplip_confdir = /etc/hp")
                           ;; This is only used for installing the default config.
                           (string-append "hplip_confdir = " out
                                          "/etc/hp"))
                          (("halpredir = /usr/share/hal/fdi/preprobe/10osvendor")
                           ;; We don't use hal.
                           (string-append "halpredir = " out
                                          "/share/hal/fdi/preprobe/10osvendor"))
                          (("rulesdir = /etc/udev/rules.d")
                           ;; udev rules will be merged by base service.
                           (string-append "rulesdir = " out
                                          "/lib/udev/rules.d"))
                          (("rulessystemdir = /usr/lib/systemd/system")
                           ;; We don't use systemd.
                           (string-append "rulessystemdir = " out
                                          "/lib/systemd/system"))
                          (("/etc/sane.d")
                           (string-append out "/etc/sane.d"))))))

                  ;; Wrap bin/* so that the Python libraries are found.
                  (add-after 'install 'wrap-binaries
                    (assoc-ref python:%standard-phases 'wrap)))))

    ;; Note that the error messages printed by the tools in the case of
    ;; missing dependencies are often downright misleading.
    ;; TODO: hp-toolbox still fails to start with:
    ;;   from dbus.mainloop.pyqt5 import DBusQtMainLoop
    ;;   ModuleNotFoundError: No module named 'dbus.mainloop.pyqt5'
    (inputs
     `(("cups-minimal" ,cups-minimal)
       ("dbus" ,dbus)
       ("libjpeg" ,libjpeg)
       ("libusb" ,libusb)
       ("python" ,python)
       ("python-dbus" ,python-dbus)
       ("python-pygobject" ,python-pygobject)
       ("python-pyqt" ,python-pyqt)
       ("python-wrapper" ,python-wrapper)
       ("sane-backends" ,sane-backends-minimal)
       ("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))))

(define-public hplip-minimal
  (package
    (inherit hplip)
    (name "hplip-minimal")
    (arguments
      (substitute-keyword-arguments (package-arguments hplip)
        ((#:configure-flags cf)
         `(delete "--enable-qt5" ,cf))))
    (inputs
     (fold alist-delete (package-inputs hplip)
           '("python-pygobject" "python-pyqt")))
    (synopsis "GUI-less version of hplip")))

(define-public foomatic-filters
  (package
    (name "foomatic-filters")
    (version "4.0.17")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.openprinting.org/download/foomatic/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "1qrkgbm5jay2r7sh9qbyf0aiyrsl1mdc844hxf7fhw95a0zfbqm2"))
              (patches
               (search-patches "foomatic-filters-CVE-2015-8327.patch"
                               "foomatic-filters-CVE-2015-8560.patch"))))
    (build-system gnu-build-system)
    (home-page
     "https://wiki.linuxfoundation.org/openprinting/database/foomatic")
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("dbus" ,dbus)
       ("a2ps" ,a2ps)))
    (arguments
     '( ;; Specify the installation directories.
       #:configure-flags (list (string-append "ac_cv_path_CUPS_BACKENDS="
                                              (assoc-ref %outputs "out")
                                              "/lib/cups/backend")
                               (string-append "ac_cv_path_CUPS_FILTERS="
                                              (assoc-ref %outputs "out")
                                              "/lib/cups/filter")
                               (string-append "ac_cv_path_PPR_INTERFACES="
                                              (assoc-ref %outputs "out")
                                              "/lib/ppr/interfaces")
                               (string-append "ac_cv_path_PPR_LIB="
                                              (assoc-ref %outputs "out")
                                              "/lib/ppr/lib")

                               ;; For some reason these are misdiagnosed.
                               "ac_cv_func_malloc_0_nonnull=yes"
                               "ac_cv_func_realloc_0_nonnull=yes")
       #:test-target "tests"))
    (synopsis "Convert PostScript to the printer's native format")
    (description
     "This package contains filter scripts used by the printer spoolers to
convert the incoming PostScript data into the printer's native format using a
printer/driver specific, but spooler-independent PPD file.")
    (license license:gpl2+)))

(define-public foo2zjs
  (package
    (name "foo2zjs")
    (version "20180910")
    (source (origin
              (method url-fetch)
              ;; XXX: This is an unversioned URL!
              (uri "http://foo2zjs.rkkda.com/foo2zjs.tar.gz")
              (sha256
               (base32
                "1clddqy4y4vvws0lllv1v90dfiihihdc23bn93c544yb3fvmwmr6"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* (find-files "." "^Makefile$")
                        ;; Set the installation directory.
                        (("^PREFIX[[:blank:]]*=.*$")
                         (string-append "PREFIX = "
                                        (assoc-ref outputs "out")
                                        "\n"))
                        (("^UDEVBIN[[:blank:]]*=.*$")
                         "UDEVBIN = $(PREFIX)/bin\n")
                        ;; Don't try to chown/chgrp the installed files.
                        (("-oroot")
                         "")
                        (("-glp")
                         "")
                        ;; Placate the dependency checks.
                        (("/usr/include/stdio.h")
                         "/etc/passwd")
                        (("/usr/")
                         "$(PREFIX)/")
                        ;; Ensure fixed timestamps in man pages.
                        (("^MODTIME[[:blank:]]*=.*$")
                         "MODTIME = echo Thu Jan 01 01:00:00 1970\n"))
                      #t))
                  (add-after 'install 'remove-pdf
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Remove 'manual.pdf' which is (1) useless (it's a
                      ;; concatenation of man pages), and (2) not
                      ;; bit-reproducible due to <https://bugs.gnu.org/27593>.
                      (let ((out (assoc-ref outputs "out")))
                        (for-each delete-file
                                  (find-files out "^manual\\.pdf$"))
                        #t))))
       #:parallel-build? #f                       ;broken makefile
       #:tests? #f                                ;no tests
       #:make-flags '("CC=gcc")))
    (inputs
     `(("ghostscript" ,ghostscript)
       ("foomatic-filters" ,foomatic-filters)))   ;for 'foomatic-rip'
    (native-inputs
     `(("bc" ,bc)
       ("groff" ,groff)))
    (home-page "http://foo2zjs.rkkda.com/")
    (synopsis "Printer driver for ZjStream-based printers")
    (description
     "foo2zjs is a printer driver for printers that use the Zenographics
ZjStream wire protocol for their print data, often erroneously referred to as
winprinters or GDI printers.

It supports Minolta/QMS@tie{}Magicolor, Minolta@tie{}Color@tie{}PageWorks/Pro,
HP@tie{}LaserJet, and possibly other printers.  See @file{README} for details.")
    (license (list license:expat        ; icc2ps/*.[ch]
                   license:gpl2+))))    ; everything else

(define-public escpr
  (package
    (name "escpr")
    (version "1.6.30")
    ;; XXX: This currently works.  But it will break as soon as a newer
    ;; version is available since the URLs for older versions are not
    ;; preserved.  An alternative source will be added as soon as
    ;; available.
    (source (origin
              (method url-fetch)
              ;; The uri has to be chopped up in order to satisfy guix lint.
              (uri (string-append "https://download3.ebz.epson.net/dsc/f/03/00/08/18/20/"
                                  "e94de600e28e510c1cfa158929d8b2c0aadc8aa0/"
                                  "epson-inkjet-printer-escpr-1.6.30-1lsb3.2.tar.gz"))
              (sha256
               (base32
                "0m8pyfkixisp0vclwxj340isn15zzisal0v2xvv66kxfd68dzf12"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       `(,(string-append "--prefix="
                         (assoc-ref %outputs "out"))
         ,(string-append "--with-cupsfilterdir="
                         (assoc-ref %outputs "out") "/lib/cups/filter")
         ,(string-append "--with-cupsppddir="
                         (assoc-ref %outputs "out") "/share/ppd"))))
    (inputs `(("cups" ,cups-minimal)))
    (synopsis "ESC/P-R printer driver")
    (description
     "This package provides a filter for the Common UNIX Printing
System (CUPS).  It offers high-quality printing with Seiko Epson color ink jet
printers.  It can only be used with printers that support the Epson ESC/P-R
language.")
    (home-page "http://download.ebz.epson.net/dsc/search/01/search")
    (license license:gpl2+)))

(define-public python-pycups
  (package
    (name "python-pycups")
    (version "1.9.74")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pycups" version ".tar.bz2"))
       (sha256
        (base32
         "1ffp7sswhdsfpy88zg0cc8kl04wygkjs01rlm9f0spbwk8jhy2c6"))))
    (build-system python-build-system)
    (arguments
     '(;; Tests require CUPS to be running
       #:tests? #f))
    (inputs
     `(("cups" ,cups)))
    (home-page "https://github.com/zdohnal/pycups")
    (synopsis "Python bindings for libcups")
    (description
     "This package provides Python bindings for libcups, wrapping the CUPS
API.")
    (license license:gpl2+)))
