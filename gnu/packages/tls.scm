;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Leo Famulari <leo@famulari.name>
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

(define-module (gnu packages tls)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages base))

(define-public libtasn1
  (package
    (name "libtasn1")
    (version "4.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/libtasn1/libtasn1-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1nhvnznhg2aqfrfjxc8v008hjlzkh5831jsfahqk89qrw7fbbcw9"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)

                     ;; XXX: For some reason, libtasn1.info wants to be
                     ;; rebuilt, so we must provide 'makeinfo'.
                     ("texinfo" ,texinfo)))
    (home-page "http://www.gnu.org/software/libtasn1/")
    (synopsis "ASN.1 library")
    (description
     "GNU libtasn1 is a library implementing the ASN.1 notation.  It is used
for transmitting machine-neutral encodings of data objects in computer
networking, allowing for formal validation of data according to some
specifications.")
    (license license:lgpl2.0+)))

(define-public p11-kit
  (package
    (name "p11-kit")
    (version "0.23.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://p11-glue.freedesktop.org/releases/p11-kit-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1i3a1wdpagm0p3y1bwaz5x5rjhcpqbcrnhkcp10p259vkxk72wz5"))
      (modules '((guix build utils))) ; for substitute*
      (snippet
        '(begin
           ;; Drop one test that fails, also when trying to compile manually.
           ;; Reported upstream at
           ;; https://bugs.freedesktop.org/show_bug.cgi?id=89027
           (substitute* "Makefile.in"
             (("test-module\\$\\(EXEEXT\\) ") ""))))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libffi" ,libffi)
       ("libtasn1" ,libtasn1)))
    (arguments
     `(#:configure-flags '("--without-trust-paths")))
    (home-page "http://p11-glue.freedesktop.org/p11-kit.html")
    (synopsis "PKCS#11 library")
    (description
     "p11-kit provides a way to load and enumerate PKCS#11 modules.  It
provides a standard configuration setup for installing PKCS#11 modules
in such a way that they are discoverable.  It also solves problems with
coordinating the use of PKCS#11 by different components or libraries
living in the same process.")
    (license license:bsd-3)))

(define-public gnutls
  (package
    (name "gnutls")
    (version "3.4.5")
    (source (origin
             (method url-fetch)
             (uri
              ;; Note: Releases are no longer on ftp.gnu.org since the
              ;; schism (after version 3.1.5).
              (string-append "mirror://gnupg/gnutls/v"
                             (version-major+minor version)
                             "/gnutls-" version ".tar.xz"))
             (sha256
              (base32
               "1bks1zpmhmnkz2v32dd9b44pz6x0a5w4yi9zzwsd0a078vhbi25g"))
             (patches (list (search-patch "gnutls-doc-fix.patch")))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--with-guile-site-dir="
                            (assoc-ref %outputs "out")
                            "/share/guile/site/2.0")
             ;; GnuTLS doesn't consult any environment variables to specify
             ;; the location of the system-wide trust store.  Instead it has a
             ;; configure-time option.  Unless specified, its configure script
             ;; attempts to auto-detect the location by looking for common
             ;; places in the filesystem, none of which are present in our
             ;; chroot build environment.  If not found, then no default trust
             ;; store is used, so each program has to provide its own
             ;; fallback, and users have to configure each program
             ;; independently.  This seems suboptimal.
             "--with-default-trust-store-dir=/etc/ssl/certs"

             ;; FIXME: Temporarily disable p11-kit support since it is not
             ;; working on mips64el.
             "--without-p11-kit")

       #:phases (modify-phases %standard-phases
                  (add-after
                   'unpack 'delete-prebuilt-unfixed-info-file
                   (lambda _
                     ;; XXX Delete the prebuilt info file, so that it will be
                     ;; rebuilt with the fixes in gnutls-doc-fix.patch.
                     (delete-file "doc/gnutls.info")
                     #t))
                  (add-after
                   'install 'move-doc
                   (lambda* (#:key outputs #:allow-other-keys)
                     ;; Copy the 4.1 MiB of section 3 man pages to "doc".
                     (let* ((out    (assoc-ref outputs "out"))
                            (doc    (assoc-ref outputs "doc"))
                            (mandir (string-append doc "/share/man/man3"))
                            (oldman (string-append out "/share/man/man3")))
                       (mkdir-p mandir)
                       (copy-recursively oldman mandir)
                       (delete-file-recursively oldman)
                       #t))))))
    (outputs '("out"                              ;4.4 MiB
               "debug"
               "doc"))                            ;4.1 MiB of man pages
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo) ; XXX needed only to replace prebuilt, unfixed docs.
       ("which" ,which)))
    (inputs
     `(("guile" ,guile-2.0)
       ("perl" ,perl)))
    (propagated-inputs
     ;; These are all in the 'Requires.private' field of gnutls.pc.
     `(("libtasn1" ,libtasn1)
       ("libidn" ,libidn)
       ("nettle" ,nettle)
       ("zlib" ,zlib)))
    (home-page "http://www.gnu.org/software/gnutls/")
    (synopsis "Transport layer security library")
    (description
     "GnuTLS is a secure communications library implementing the SSL, TLS
and DTLS protocols.  It is provided in the form of a C library to support the
protocols, as well as to parse and write X.5009, PKCS 12, OpenPGP and other
required structures.")
    (license license:lgpl2.1+)))

(define-public openssl
  (package
   (name "openssl")
   (version "1.0.2e")
   (source (origin
            (method url-fetch)
            (uri (string-append "ftp://ftp.openssl.org/source/openssl-" version
                                ".tar.gz"))
            (sha256
             (base32
              "1zqb1rff1wikc62a7vj5qxd1k191m8qif5d05mwdxz2wnzywlg72"))
            (patches (map search-patch
                          '("openssl-runpath.patch"
                            "openssl-c-rehash.patch")))))
   (build-system gnu-build-system)
   (native-inputs `(("perl" ,perl)))
   (arguments
    `(#:parallel-build? #f
      #:parallel-tests? #f
      #:test-target "test"
      #:phases
      (modify-phases %standard-phases
        (add-before
         'configure 'patch-Makefile.org
         (lambda* (#:key outputs #:allow-other-keys)
           ;; The default MANDIR is some unusual place.  Fix that.
           (let ((out (assoc-ref outputs "out")))
             (patch-makefile-SHELL "Makefile.org")
             (substitute* "Makefile.org"
               (("^MANDIR[[:blank:]]*=.*$")
                (string-append "MANDIR = " out "/share/man\n")))
             #t)))
        (replace
         'configure
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
             (zero?
              (system* "./config"
                       "shared"                   ;build shared libraries
                       "--libdir=lib"

                       ;; The default for this catch-all directory is
                       ;; PREFIX/ssl.  Change that to something more
                       ;; conventional.
                       (string-append "--openssldir=" out
                                      "/share/openssl-" ,version)

                       (string-append "--prefix=" out)

                       ;; XXX FIXME: Work around a code generation bug in GCC
                       ;; 4.9.3 on ARM when compiled with -mfpu=neon.  See:
                       ;; <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=66917>
                       ,@(if (and (not (%current-target-system))
                                  (string-prefix? "armhf" (%current-system)))
                             '("-mfpu=vfpv3")
                             '()))))))
        (add-after
         'install 'make-libraries-writable
         (lambda* (#:key outputs #:allow-other-keys)
           ;; Make libraries writable so that 'strip' does its job.
           (let ((out (assoc-ref outputs "out")))
             (for-each (lambda (file)
                         (chmod file #o644))
                       (find-files (string-append out "/lib")
                                   "\\.so"))
             #t)))
        (add-after
         'unpack 'fix-broken-symlinks
         (lambda _
           ;; Repair the broken symlinks in the openssl-1.0.2e tarball.
           (let* ((link-prefix "openssl-1.0.2e/")
                  (link-prefix-length (string-length link-prefix))
                  (broken-links
                   (find-files "." (lambda (file stat)
                                     (and (eq? 'symlink (stat:type stat))
                                          (string-prefix? link-prefix
                                                          (readlink file)))))))
             (when (null? broken-links)
               (error "The 'fix-broken-symlinks' phase is obsolete; remove it"))
             (for-each (lambda (file)
                         (let* ((old-target (readlink file))
                                (new-target (string-drop old-target
                                                         link-prefix-length)))
                           (delete-file file)
                           (symlink new-target file)))
                       broken-links)
             #t)))
        (add-before
         'patch-source-shebangs 'patch-tests
         (lambda* (#:key inputs native-inputs #:allow-other-keys)
           (let ((bash (assoc-ref (or native-inputs inputs) "bash")))
             (substitute* (find-files "test" ".*")
               (("/bin/sh")
                (string-append bash "/bin/bash"))
               (("/bin/rm")
                "rm"))
             #t)))
        (add-after
         'install 'remove-miscellany
         (lambda* (#:key outputs #:allow-other-keys)
           ;; The 'misc' directory contains random undocumented shell and Perl
           ;; scripts.  Remove them to avoid retaining a reference on Perl.
           (let ((out (assoc-ref outputs "out")))
             (delete-file-recursively (string-append out "/share/openssl-"
                                                     ,version "/misc"))
             #t))))))
   (native-search-paths
    ;; FIXME: These two variables must designate a single file or directory
    ;; and are not actually "search paths."  In practice it works OK in user
    ;; profiles because there's always just one item that matches the
    ;; specification.
    (list (search-path-specification
           (variable "SSL_CERT_DIR")
           (files '("etc/ssl/certs")))
          (search-path-specification
           (variable "SSL_CERT_FILE")
           (files '("etc/ssl/certs/ca-certificates.crt")))))
   (synopsis "SSL/TLS implementation")
   (description
    "OpenSSL is an implementation of SSL/TLS.")
   (license license:openssl)
   (home-page "http://www.openssl.org/")))

(define-public libressl
  (package
    (name "libressl")
    (version "2.2.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
             "http://ftp.openbsd.org/pub/OpenBSD/LibreSSL/libressl-"
             version ".tar.gz"))
      (sha256 (base32
               "0h1haqb4y39p1zihwvnr1ib0zfq5bcqfnbj5jm9l4j2xibrxi44n"))))
    (build-system gnu-build-system)
    (native-search-paths
      ;; FIXME: These two variables must designate a single file or directory
      ;; and are not actually "search paths."  In practice it works OK in
      ;; user profiles because there's always just one item that matches the
      ;; specification.
     (list (search-path-specification
            (variable "SSL_CERT_DIR")
            (files '("etc/ssl/certs")))
           (search-path-specification
            (variable "SSL_CERT_FILE")
            (files '("etc/ssl/certs/ca-certificates.crt")))))
    (home-page "http://www.libressl.org/")
    (synopsis "SSL/TLS implementation")
    (description "LibreSSL is a version of the TLS/crypto stack forked
from OpenSSL in 2014, with the goals of modernizing the codebase, improving
security, and applying best practice development processes.")
    ;; Files taken from OpenSSL keep their license, others are under various
    ;; non-copyleft licenses.
    (license (list license:openssl
                   (license:non-copyleft
                     "file://COPYING"
                     "See COPYING in the distribution.")))))

(define-public acme
  (package
    (name "acme")
    (version "0.1.0")
    (source (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/a/acme/acme-"
                          version ".tar.gz"))
      (sha256
        (base32
          "0fj0m04zzdxx23vazl00ilqyl3jxqq9c9p4x61pfz1zps7nbzsy3"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    ;; TODO: Add optional inputs for testing and building documentation.
    (native-inputs
     `(("python2-mock" ,python2-mock)
       ("python2-setuptools" ,python2-setuptools)))
    (propagated-inputs
     `(("python2-ndg-httpsclient" ,python2-ndg-httpsclient)
       ("python2-werkzeug" ,python2-werkzeug)
       ("python2-six" ,python2-six)
       ("python2-requests" ,python2-requests)
       ("python2-pytz" ,python2-pytz)
       ("python2-pyrfc3339" ,python2-pyrfc3339)
       ("python2-pyasn1" ,python2-pyasn1)
       ("python2-cryptography" ,python2-cryptography)
       ("python2-pyopenssl" ,python2-pyopenssl)))
    (home-page "https://github.com/letsencrypt/letsencrypt")
    (synopsis "ACME protocol implementation in Python")
    (description "ACME protocol implementation in Python")
    (license license:asl2.0)))

(define-public letsencrypt
  (package
    (name "letsencrypt")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pypi.python.org/packages/source/l/"
                                  "letsencrypt/letsencrypt-" version ".tar.gz"))
              (sha256
               (base32
                "1zb96xz32k6ai41h5m1l22qi47y71dq69dcmbz7vfm6jfrhjgxl1"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    ;; TODO: Add optional inputs for testing building documentation.
    (native-inputs
     `(("python2-nose" ,python2-nose)
       ("python2-mock" ,python2-mock)))
    (propagated-inputs
     `(("acme" ,acme)
       ("python2-zope-interface" ,python2-zope-interface)
       ("python2-pythondialog" ,python2-pythondialog)
       ("python2-pyrfc3339" ,python2-pyrfc3339)
       ("python2-pyopenssl" ,python2-pyopenssl)
       ("python2-configobj" ,python2-configobj)
       ("python2-configargparse" ,python2-configargparse)
       ("python2-zope-component" ,python2-zope-component)
       ("python2-parsedatetime" ,python2-parsedatetime)
       ("python2-six" ,python2-six)
       ("python2-psutil" ,python2-psutil)
       ("python2-requests" ,python2-requests)
       ("python2-pytz" ,python2-pytz)))
    (synopsis "Let's Encrypt client")
    (description "Tool to automatically receive and install X.509 certificates
to enable TLS on servers.  The client will interoperate with the Let’s Encrypt CA which
will be issuing browser-trusted certificates for free.")
    (home-page "https://letsencrypt.org/")
    (license license:asl2.0)))

(define-public perl-net-ssleay
  (package
    (name "perl-net-ssleay")
    (version "1.68")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MI/MIKEM/"
                                  "Net-SSLeay-" version ".tar.gz"))
              (sha256
               (base32
                "1m2wwzhjwsg0drlhp9w12fl6bsgj69v8gdz72jqrqll3qr7f408p"))))
    (build-system perl-build-system)
    (native-inputs
     `(("patch" ,patch)
       ("patch/disable-ede-test"
        ,(search-patch "perl-net-ssleay-disable-ede-test.patch"))))
    (inputs `(("openssl" ,openssl)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'apply-patch
          (lambda* (#:key inputs #:allow-other-keys)
            ;; XXX We apply this patch here instead of in the 'origin' because
            ;; this package's build system fails badly when the source file
            ;; times are zeroed.
            ;; XXX Try removing this patch for perl-net-ssleay > 1.68
            (zero? (system* "patch" "--force" "-p1" "-i"
                            (assoc-ref inputs "patch/disable-ede-test")))))
         (add-before
          'configure 'set-ssl-prefix
          (lambda* (#:key inputs #:allow-other-keys)
            (setenv "OPENSSL_PREFIX" (assoc-ref inputs "openssl"))
            #t)))))
    (synopsis "Perl extension for using OpenSSL")
    (description
     "This module offers some high level convenience functions for accessing
web pages on SSL servers (for symmetry, the same API is offered for accessing
http servers, too), an sslcat() function for writing your own clients, and
finally access to the SSL api of the SSLeay/OpenSSL package so you can write
servers or clients for more complicated applications.")
    (license (package-license perl))
    (home-page "http://search.cpan.org/~mikem/Net-SSLeay-1.66/")))
