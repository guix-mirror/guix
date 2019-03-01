;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2018 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018, 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages base)
  #:use-module (srfi srfi-1))

(define-public libtasn1
  (package
    (name "libtasn1")
    (version "4.13")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/libtasn1/libtasn1-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1jlc1iahj8k3haz28j55nzg7sgni5h41vqy461i1bpbx6668wlky"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")))
    (native-inputs `(("perl" ,perl)))
    (home-page "https://www.gnu.org/software/libtasn1/")
    (synopsis "ASN.1 library")
    (description
     "GNU libtasn1 is a library implementing the ASN.1 notation.  It is used
for transmitting machine-neutral encodings of data objects in computer
networking, allowing for formal validation of data according to some
specifications.")
    (license license:lgpl2.0+)))

(define-public asn1c
  (package
    (name "asn1c")
    (version "0.9.28")
    (source (origin
      (method url-fetch)
      (uri (string-append "https://lionet.info/soft/asn1c-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1fc64g45ykmv73kdndr4zdm4wxhimhrir4rxnygxvwkych5l81w0"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)))
    (home-page "https://lionet.info/asn1c")
    (synopsis "ASN.1 to C compiler")
    (description "The ASN.1 to C compiler takes ASN.1 module
files and generates C++ compatible C source code.  That code can be
used to serialize the native C structures into compact and unambiguous
BER/XER/PER-based data files, and deserialize the files back.

Various ASN.1 based formats are widely used in the industry, such as to encode
the X.509 certificates employed in the HTTPS handshake, to exchange control
data between mobile phones and cellular networks, to car-to-car communication
in intelligent transportation networks.")
    (license license:bsd-2)))

(define-public p11-kit
  (package
    (name "p11-kit")
    (version "0.23.15")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/p11-glue/p11-kit/releases/"
                          "download/" version "/p11-kit-" version ".tar.gz"))
      (patches (search-patches "p11-kit-jks-timestamps.patch"))
      (sha256
       (base32
        "166pwj00cffv4qq4dvx0k53zka0b0r1fa0whc49007vsqyh3khgp"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libffi" ,libffi)
       ("libtasn1" ,libtasn1)))
    (arguments
     `(#:configure-flags '("--without-trust-paths")
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'prepare-tests
                    (lambda _
                      ;; "test-runtime" expects XDG_RUNTIME_DIR to be set up
                      ;; and looks for .cache and other directories (only).
                      ;; For simplicity just drop it since it is irrelevant
                      ;; in the build container.
                      (substitute* "Makefile"
                        (("test-runtime\\$\\(EXEEXT\\)") ""))
                      #t)))))
    (home-page "https://p11-glue.freedesktop.org/p11-kit.html")
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
    (version "3.6.6")
    (source (origin
             (method url-fetch)
             (uri
              ;; Note: Releases are no longer on ftp.gnu.org since the
              ;; schism (after version 3.1.5).
              (string-append "mirror://gnupg/gnutls/v"
                             (version-major+minor version)
                             "/gnutls-" version ".tar.xz"))
             (patches (search-patches "gnutls-skip-trust-store-test.patch"))
             (sha256
              (base32
               "19rcfgsfxb01cyz8jxmmgkjqc7y5s97amajzyknk1i1amywcm6mv"))))
    (build-system gnu-build-system)
    (arguments
     `(; Ensure we don't keep a reference to this buggy software.
       #:disallowed-references (,net-tools)
       #:configure-flags
       (list
             ;; GnuTLS doesn't consult any environment variables to specify
             ;; the location of the system-wide trust store.  Instead it has a
             ;; configure-time option.  Unless specified, its configure script
             ;; attempts to auto-detect the location by looking for common
             ;; places in the file system, none of which are present in our
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
     `(("net-tools" ,net-tools)
       ("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("guile" ,guile-2.2)))
    (propagated-inputs
     ;; These are all in the 'Requires.private' field of gnutls.pc.
     `(("libtasn1" ,libtasn1)
       ("libidn2" ,libidn2)
       ("nettle" ,nettle)
       ("zlib" ,zlib)))
    (home-page "https://www.gnu.org/software/gnutls/")
    (synopsis "Transport layer security library")
    (description
     "GnuTLS is a secure communications library implementing the SSL, TLS
and DTLS protocols.  It is provided in the form of a C library to support the
protocols, as well as to parse and write X.5009, PKCS 12, OpenPGP and other
required structures.")
    (license license:lgpl2.1+)
    (properties '((ftp-server . "ftp.gnutls.org")
                  (ftp-directory . "/gcrypt/gnutls")))))

(define-public gnutls/guile-2.2
  (deprecated-package "guile2.2-gnutls" gnutls))

(define-public gnutls/guile-2.0
  ;; GnuTLS for Guile 2.0.
  (package
    (inherit gnutls)
    (name "guile2.0-gnutls")
    (inputs `(("guile" ,guile-2.0)
              ,@(alist-delete "guile" (package-inputs gnutls))))))

(define-public gnutls/dane
  ;; GnuTLS with build libgnutls-dane, implementing DNS-based
  ;; Authentication of Named Entities.  This is required for GNS functionality
  ;; by GNUnet and gnURL.  This is done in an extra package definition
  ;; to have the choice between GnuTLS with Dane and without Dane.
  (package
    (inherit gnutls)
    (name "gnutls-dane")
    (inputs `(("unbound" ,unbound)
              ,@(package-inputs gnutls)))))

(define-public openssl
  (package
   (name "openssl")
   (version "1.1.1b")
   (source (origin
             (method url-fetch)
             (uri (list (string-append "https://www.openssl.org/source/openssl-"
                                       version ".tar.gz")
                        (string-append "ftp://ftp.openssl.org/source/"
                                       "openssl-" version ".tar.gz")
                        (string-append "ftp://ftp.openssl.org/source/old/"
                                       (string-trim-right version char-set:letter)
                                       "/openssl-" version ".tar.gz")))
             (sha256
              (base32
               "0jza8cmznnyiia43056dij1jdmz62dx17wsn0zxksh9h6817nmaw"))
             (patches (search-patches "openssl-1.1-c-rehash-in.patch"))))
   (build-system gnu-build-system)
   (outputs '("out"
              "doc"         ;6.8 MiB of man3 pages and full HTML documentation
              "static"))    ;6.4 MiB of .a files
   (native-inputs `(("perl" ,perl)))
   (arguments
    `(#:disallowed-references (,perl)
      #:parallel-build? #f
      #:parallel-tests? #f
      #:test-target "test"

      ;; Changes to OpenSSL sometimes cause Perl to "sneak in" to the closure,
      ;; so we explicitly disallow it here.
      #:disallowed-references ,(list (canonical-package perl))
      #:phases
      (modify-phases %standard-phases
        (replace 'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (lib (string-append out "/lib")))
              ;; It's not a shebang so patch-source-shebangs misses it.
              (substitute* "config"
                (("/usr/bin/env")
                 (string-append (assoc-ref %build-inputs "coreutils")
                                "/bin/env")))
              (invoke "./config"
                      "shared"       ;build shared libraries
                      "--libdir=lib"

                      ;; The default for this catch-all directory is
                      ;; PREFIX/ssl.  Change that to something more
                      ;; conventional.
                      (string-append "--openssldir=" out
                                     "/share/openssl-" ,version)

                      (string-append "--prefix=" out)
                      (string-append "-Wl,-rpath," lib)))))
        (add-after 'install 'move-static-libraries
          (lambda* (#:key outputs #:allow-other-keys)
            ;; Move static libraries to the "static" output.
            (let* ((out    (assoc-ref outputs "out"))
                   (lib    (string-append out "/lib"))
                   (static (assoc-ref outputs "static"))
                   (slib   (string-append static "/lib")))
              (for-each (lambda (file)
                          (install-file file slib)
                          (delete-file file))
                        (find-files lib "\\.a$"))
              #t)))
        (add-after 'install 'move-extra-documentation
          (lambda* (#:key outputs #:allow-other-keys)
               ;; Move man3 pages and full HTML documentation to "doc".
               (let* ((out    (assoc-ref outputs "out"))
                      (man3   (string-append out "/share/man/man3"))
                      (html (string-append out "/share/doc/openssl"))
                      (doc    (assoc-ref outputs "doc"))
                      (man-target (string-append doc "/share/man/man3"))
                      (html-target (string-append doc "/share/doc/openssl")))
                 (copy-recursively man3 man-target)
                 (delete-file-recursively man3)
                 (copy-recursively html html-target)
                 (delete-file-recursively html)
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
    (list (search-path-specification
           (variable "SSL_CERT_DIR")
           (separator #f)                        ;single entry
           (files '("etc/ssl/certs")))
          (search-path-specification
           (variable "SSL_CERT_FILE")
           (file-type 'regular)
           (separator #f)                        ;single entry
           (files '("etc/ssl/certs/ca-certificates.crt")))))
   (synopsis "SSL/TLS implementation")
   (description
    "OpenSSL is an implementation of SSL/TLS.")
   (license license:openssl)
   (home-page "https://www.openssl.org/")))

(define-public openssl-1.0
  (package
    (inherit openssl)
    (name "openssl")
    (version "1.0.2r")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "https://www.openssl.org/source/openssl-"
                                        version ".tar.gz")
                         (string-append "ftp://ftp.openssl.org/source/"
                                        "openssl-" version ".tar.gz")
                         (string-append "ftp://ftp.openssl.org/source/old/"
                                        (string-trim-right version char-set:letter)
                                        "/openssl-" version ".tar.gz")))
              (sha256
               (base32
                "1mnh27zf6r1bhm5d9fxqq9slv2gz0d9z2ij9i679b0wapa5x0ldf"))
              (patches (search-patches "openssl-runpath.patch"
                                       "openssl-c-rehash-in.patch"))))
    (outputs '("out"
               "doc"                    ;1.5MiB of man3 pages
               "static"))               ;6MiB of .a files
    (arguments
     (substitute-keyword-arguments (package-arguments openssl)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'patch-source-shebangs 'patch-tests
             (lambda* (#:key inputs native-inputs #:allow-other-keys)
               (let ((bash (assoc-ref (or native-inputs inputs) "bash")))
                 (substitute* (find-files "test" ".*")
                   (("/bin/sh")
                    (string-append bash "/bin/sh"))
                   (("/bin/rm")
                    "rm"))
                 #t)))
           (add-before 'configure 'patch-Makefile.org
             (lambda* (#:key outputs #:allow-other-keys)
               ;; The default MANDIR is some unusual place.  Fix that.
               (let ((out (assoc-ref outputs "out")))
                 (patch-makefile-SHELL "Makefile.org")
                 (substitute* "Makefile.org"
                   (("^MANDIR[[:blank:]]*=.*$")
                    (string-append "MANDIR = " out "/share/man\n")))
                 #t)))
        (replace 'configure
          ;; Override this phase because OpenSSL 1.0 does not understand -rpath.
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (invoke "./config"
                      "shared"                 ;build shared libraries
                      "--libdir=lib"

                      ;; The default for this catch-all directory is
                      ;; PREFIX/ssl.  Change that to something more
                      ;; conventional.
                      (string-append "--openssldir=" out
                                     "/share/openssl-" ,version)

                      (string-append "--prefix=" out)))))
        (delete 'move-extra-documentation)
        (add-after 'install 'move-man3-pages
          (lambda* (#:key outputs #:allow-other-keys)
            ;; Move section 3 man pages to "doc".
            (let* ((out    (assoc-ref outputs "out"))
                   (man3   (string-append out "/share/man/man3"))
                   (doc    (assoc-ref outputs "doc"))
                   (target (string-append doc "/share/man/man3")))
              (mkdir-p target)
              (for-each (lambda (file)
                          (rename-file file
                                       (string-append target "/"
                                                      (basename file))))
                        (find-files man3))
              (delete-file-recursively man3)
              #t)))
           ;; XXX: Duplicate this phase to make sure 'version' evaluates
           ;; in the current scope and not the inherited one.
           (replace 'remove-miscellany
             (lambda* (#:key outputs #:allow-other-keys)
               ;; The 'misc' directory contains random undocumented shell and Perl
               ;; scripts.  Remove them to avoid retaining a reference on Perl.
               (let ((out (assoc-ref outputs "out")))
                 (delete-file-recursively (string-append out "/share/openssl-"
                                                         ,version "/misc"))
                 #t)))))))))

(define-public libressl
  (package
    (name "libressl")
    (version "2.7.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://openbsd/LibreSSL/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "19kxa5i97q7p6rrps9qm0nd8zqhdjvzx02j72400c73cl2nryfhy"))))
    (build-system gnu-build-system)
    (arguments
     ;; Do as if 'getentropy' was missing since older Linux kernels lack it
     ;; and libc would return ENOSYS, which is not properly handled.
     ;; See <https://lists.gnu.org/archive/html/guix-devel/2017-04/msg00235.html>.
     '(#:configure-flags '("ac_cv_func_getentropy=no"
                           ;; Provide a TLS-enabled netcat.
                           "--enable-nc")))
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
    (home-page "https://www.libressl.org/")
    (synopsis "SSL/TLS implementation")
    (description "LibreSSL is a version of the TLS/crypto stack, forked from
OpenSSL in 2014 with the goals of modernizing the codebase, improving security,
and applying best practice development processes.  This package also includes a
netcat implementation that supports TLS.")
    ;; Files taken from OpenSSL keep their license, others are under various
    ;; non-copyleft licenses.
    (license (list license:openssl
                   (license:non-copyleft
                     "file://COPYING"
                     "See COPYING in the distribution.")))))

(define-public python-acme
  (package
    (name "python-acme")
    ;; Remember to update the hash of certbot when updating python-acme.
    (version "0.31.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "acme" version))
              (sha256
               (base32
                "1gxjv09c695lj8swspa390nch117i60qkrgy135383vfk00jsp3y"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-documentation
           (lambda _
             (invoke "make" "-C" "docs" "man" "info")))
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man/man1"))
                    (info (string-append out "/info")))
               (install-file "docs/_build/texinfo/acme-python.info" info)
               (install-file "docs/_build/man/acme-python.1" man)
               #t))))))
    ;; TODO: Add optional inputs for testing.
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ;; For documentation
       ("python-sphinx" ,python-sphinx)
       ("python-sphinxcontrib-programoutput" ,python-sphinxcontrib-programoutput)
       ("python-sphinx-rtd-theme" ,python-sphinx-rtd-theme)
       ("texinfo" ,texinfo)))
    (propagated-inputs
     `(("python-josepy" ,python-josepy)
       ("python-six" ,python-six)
       ("python-requests" ,python-requests)
       ("python-requests-toolbelt" ,python-requests-toolbelt)
       ("python-pytz" ,python-pytz)
       ("python-pyrfc3339" ,python-pyrfc3339)
       ("python-pyasn1" ,python-pyasn1)
       ("python-cryptography" ,python-cryptography)
       ("python-pyopenssl" ,python-pyopenssl)))
    (home-page "https://github.com/certbot/certbot")
    (synopsis "ACME protocol implementation in Python")
    (description "ACME protocol implementation in Python")
    (license license:asl2.0)))

(define-public certbot
  (package
    (name "certbot")
    ;; Certbot and python-acme are developed in the same repository, and their
    ;; versions should remain synchronized.
    (version (package-version python-acme))
    (source (origin
              (method url-fetch)
              (uri (pypi-uri name version))
              (sha256
               (base32
                "0wq4jgyzli684h154w26xplp0fzyks2vlrnmhafhyb0h1bw9cc8c"))))
    (build-system python-build-system)
    (arguments
     `(,@(substitute-keyword-arguments (package-arguments python-acme)
           ((#:phases phases)
            `(modify-phases ,phases
              (replace 'install-documentation
                (lambda* (#:key outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (man1 (string-append out "/share/man/man1"))
                         (man7 (string-append out "/share/man/man7"))
                         (info (string-append out "/info")))
                    (install-file "docs/_build/texinfo/Certbot.info" info)
                    (install-file "docs/_build/man/certbot.1" man1)
                    (install-file "docs/_build/man/certbot.7" man7)
                    #t))))))))
    ;; TODO: Add optional inputs for testing.
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-mock" ,python-mock)
       ;; For documentation
       ("python-sphinx" ,python-sphinx)
       ("python-sphinx-rtd-theme" ,python-sphinx-rtd-theme)
       ("python-sphinx-repoze-autointerface" ,python-sphinx-repoze-autointerface)
       ("python-sphinxcontrib-programoutput" ,python-sphinxcontrib-programoutput)
       ("texinfo" ,texinfo)))
    (propagated-inputs
     `(("python-acme" ,python-acme)
       ("python-zope-interface" ,python-zope-interface)
       ("python-pyrfc3339" ,python-pyrfc3339)
       ("python-pyopenssl" ,python-pyopenssl)
       ("python-configobj" ,python-configobj)
       ("python-configargparse" ,python-configargparse)
       ("python-zope-component" ,python-zope-component)
       ("python-parsedatetime" ,python-parsedatetime)
       ("python-six" ,python-six)
       ("python-psutil" ,python-psutil)
       ("python-requests" ,python-requests)
       ("python-pytz" ,python-pytz)))
    (synopsis "Let's Encrypt client by the Electronic Frontier Foundation")
    (description "Certbot automatically receives and installs X.509 certificates
to enable Transport Layer Security (TLS) on servers.  It interoperates with the
Let’s Encrypt certificate authority (CA), which issues browser-trusted
certificates for free.")
    (home-page "https://certbot.eff.org/")
    (license license:asl2.0)))

(define-public letsencrypt
  (package (inherit certbot)
    (name "letsencrypt")
    (properties `((superseded . ,certbot)))))

(define-public perl-net-ssleay
  (package
    (name "perl-net-ssleay")
    (version "1.85")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/M/MI/MIKEM/"
                                  "Net-SSLeay-" version ".tar.gz"))
              (sha256
               (base32
                "1j5h4ycm8538397l204d2d5fkm9595aj174pj7bkpbhwzfwqi0cx"))))
    (build-system perl-build-system)
    (inputs `(("openssl" ,openssl)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
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
    (license license:perl-license)
    (home-page "https://metacpan.org/release/Net-SSLeay")))

(define-public perl-crypt-openssl-rsa
 (package
  (name "perl-crypt-openssl-rsa")
  (version "0.31")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/T/TO/TODDR/Crypt-OpenSSL-RSA-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "0djl5i6kibl7862b6ih29q8dhg5zpwzq77q9j8hp6xngshx40ws1"))))
  (build-system perl-build-system)
  (native-inputs
   `(("perl-crypt-openssl-guess" ,perl-crypt-openssl-guess)))
  (inputs
    `(("perl-crypt-openssl-bignum" ,perl-crypt-openssl-bignum)
      ("perl-crypt-openssl-random" ,perl-crypt-openssl-random)
      ("openssl" ,openssl)))
  (arguments perl-crypt-arguments)
  (home-page
    "https://metacpan.org/release/Crypt-OpenSSL-RSA")
  (synopsis
    "RSA encoding and decoding, using the openSSL libraries")
  (description "Crypt::OpenSSL::RSA does RSA encoding and decoding (using the
OpenSSL libraries).")
  (license license:perl-license)))

(define perl-crypt-arguments
   `(#:phases (modify-phases %standard-phases
      (add-before 'configure 'patch-Makefile.PL
        (lambda* (#:key inputs #:allow-other-keys)
          (substitute* "Makefile.PL"
            (("'LIBS'.*=>.*") (string-append "'LIBS' => ['-L"
                                             (assoc-ref inputs "openssl")
                                             "/lib -lcrypto'],")))
          #t)))))

(define-public perl-crypt-openssl-bignum
 (package
  (name "perl-crypt-openssl-bignum")
  (version "0.09")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/K/KM/KMX/Crypt-OpenSSL-Bignum-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "1p22znbajq91lbk2k3yg12ig7hy5b4vy8igxwqkmbm4nhgxp4ki3"))))
  (build-system perl-build-system)
  (inputs `(("openssl" ,openssl)))
  (arguments perl-crypt-arguments)
  (home-page
    "https://metacpan.org/release/Crypt-OpenSSL-Bignum")
  (synopsis
    "OpenSSL's multiprecision integer arithmetic in Perl")
  (description "Crypt::OpenSSL::Bignum provides multiprecision integer
arithmetic in Perl.")
  ;; At your option either gpl1+ or the Artistic License
  (license license:perl-license)))

(define-public perl-crypt-openssl-guess
  (package
    (name "perl-crypt-openssl-guess")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/A/AK/AKIYM/Crypt-OpenSSL-Guess-"
             version ".tar.gz"))
       (sha256
        (base32
         "0rvi9l4ljcbhwwvspq019nfq2h2v746dk355h2nwnlmqikiihsxa"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Crypt-OpenSSL-Guess")
    (synopsis "Guess the OpenSSL include path")
    (description
     "The Crypt::OpenSSL::Guess Perl module provides helpers to guess the
correct OpenSSL include path.  It is intended for use in your
@file{Makefile.PL}.")
    (license license:perl-license)))

(define-public perl-crypt-openssl-random
 (package
  (name "perl-crypt-openssl-random")
  (version "0.13")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/R/RU/RURBAN/Crypt-OpenSSL-Random-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "0vmvrb3shrzjzri3qn524dzdasbq8zhhbpc1vmq8sx68n4jhizb0"))))
  (build-system perl-build-system)
  (native-inputs
   `(("perl-crypt-openssl-guess" ,perl-crypt-openssl-guess)))
  (inputs
   `(("openssl" ,openssl)))
  (arguments perl-crypt-arguments)
  (home-page
    "https://metacpan.org/release/Crypt-OpenSSL-Random")
  (synopsis
    "OpenSSL/LibreSSL pseudo-random number generator access")
  (description "Crypt::OpenSSL::Random is a OpenSSL/LibreSSL pseudo-random
number generator")
  (license license:perl-license)))

(define-public acme-client
  (package
    (name "acme-client")
    (version "0.1.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://kristaps.bsd.lv/" name "/"
                                  "snapshots/" name "-portable-"
                                  version ".tgz"))
              (sha256
               (base32
                "00q05b3b1dfnfp7sr1nbd212n0mqrycl3cr9lbs51m7ncaihbrz9"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no test suite
       #:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((pem (string-append (assoc-ref inputs "libressl")
                                       "/etc/ssl/cert.pem")))
               (substitute* "http.c"
                 (("/etc/ssl/cert.pem") pem))
               #t)))
         (delete 'configure)))) ; no './configure' script
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libbsd" ,libbsd)
       ("libressl" ,libressl)))
    (synopsis "Let's Encrypt client by the OpenBSD project")
    (description "acme-client is a Let's Encrypt client implemented in C.  It
uses a modular design, and attempts to secure itself by dropping privileges and
operating in a chroot where possible.  acme-client is developed on OpenBSD and
then ported to the GNU / Linux environment.")
    (home-page "https://kristaps.bsd.lv/acme-client/")
    ;; acme-client is distributed under the ISC license, but the files 'jsmn.h'
    ;; and 'jsmn.c' are distributed under the Expat license.
    (license (list license:isc license:expat))))

;; The "-apache" variant is the upstreamed prefered variant. A "-gpl"
;; variant exists in addition to the "-apache" one.
(define-public mbedtls-apache
  (package
    (name "mbedtls-apache")
    (version "2.16.0")
    (source
     (origin
       (method url-fetch)
       ;; XXX: The download links on the website are script redirection links
       ;; which effectively lead to the format listed in the uri here.
       (uri (string-append "https://tls.mbed.org/download/mbedtls-"
                           version "-apache.tgz"))
       (sha256
        (base32
         "1qlscr0m97favkqmrlj90rlgw40h8lcypxz0snvr1iwkj1pbbnp3"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DUSE_SHARED_MBEDTLS_LIBRARY=ON")))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python)))
    (synopsis "Small TLS library")
    (description
     "@code{mbed TLS}, formerly known as PolarSSL, makes it trivially easy
for developers to include cryptographic and SSL/TLS capabilities in their
(embedded) products, facilitating this functionality with a minimal
coding footprint.")
    (home-page "https://tls.mbed.org")
    (license license:asl2.0)))

;; The Hiawatha Web server requires some specific features to be enabled.
(define-public mbedtls-for-hiawatha
  (hidden-package
   (package
     (inherit mbedtls-apache)
     (arguments
      (substitute-keyword-arguments
          `(#:phases
            (modify-phases %standard-phases
              (add-after 'configure 'configure-extra-features
                (lambda _
                  (for-each (lambda (feature)
                              (invoke "scripts/config.pl" "set" feature))
                            (list "MBEDTLS_THREADING_C"
                                  "MBEDTLS_THREADING_PTHREAD"))
                  #t)))
            ,@(package-arguments mbedtls-apache)))))))

(define-public ghc-tls
  (package
    (name "ghc-tls")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "tls/tls-" version ".tar.gz"))
              (sha256
               (base32
                "1y083724mym28n6xfaz7pcc7zqxdhjpaxpbvzxfbs25qq2px3smv"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-cereal" ,ghc-cereal)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-memory" ,ghc-memory)
       ("ghc-cryptonite" ,ghc-cryptonite)
       ("ghc-asn1-types" ,ghc-asn1-types)
       ("ghc-asn1-encoding" ,ghc-asn1-encoding)
       ("ghc-x509" ,ghc-x509)
       ("ghc-x509-store" ,ghc-x509-store)
       ("ghc-x509-validation" ,ghc-x509-validation)
       ("ghc-async" ,ghc-async)
       ("ghc-network" ,ghc-network)
       ("ghc-hourglass" ,ghc-hourglass)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/vincenthz/hs-tls")
    (synopsis
     "TLS/SSL protocol native implementation (Server and Client)")
    (description
     "Native Haskell TLS and SSL protocol implementation for server and client.
This provides a high-level implementation of a sensitive security protocol,
eliminating a common set of security issues through the use of the advanced
type system, high level constructions and common Haskell features.  Currently
implement the SSL3.0, TLS1.0, TLS1.1 and TLS1.2 protocol, and support RSA and
Ephemeral (Elliptic curve and regular) Diffie Hellman key exchanges, and many
extensions.")
    (license license:bsd-3)))

(define-public dehydrated
  (package
    (name "dehydrated")
    (version "0.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/lukas2511/dehydrated/releases/download/"
                    "v" version "/dehydrated-" version ".tar.gz"))
              (sha256
               (base32
                "03p80yj6bnzjc6dkp5hb9wpplmlrla8n5src71cnzw4rj53q8cqn"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source (assoc-ref %build-inputs "source"))
                (tar (assoc-ref %build-inputs "tar"))
                (gz  (assoc-ref %build-inputs "gzip"))
                (out (assoc-ref %outputs "out"))
                (bin (string-append out "/bin"))
                (doc (string-append out "/share/doc/"))
                (bash (in-vicinity (assoc-ref %build-inputs "bash") "bin")))

           (setenv "PATH" (string-append gz "/bin"))
           (invoke (string-append tar "/bin/tar") "xvf" source)
           (chdir (string-append ,name "-" ,version))

           (install-file "dehydrated" bin)
           (install-file "LICENSE" (string-append doc ,name "-" ,version))
           (with-directory-excursion bin
             (patch-shebang "dehydrated" (list bash))

             ;; Do not try to write in the store.
             (substitute* "dehydrated"
               (("SCRIPTDIR=\"\\$.*\"") "SCRIPTDIR=~/.dehydrated"))

             (setenv "PATH" bash)
             (wrap-program "dehydrated"
               `("PATH" ":" prefix
                 ,(map (lambda (dir)
                         (string-append dir "/bin"))
                       (map (lambda (input)
                              (assoc-ref %build-inputs input))
                            '("coreutils"
                              "curl"
                              "diffutils"
                              "gawk"
                              "grep"
                              "openssl"
                              "sed"))))))
           #t))))
    (inputs
     `(("bash" ,bash)
       ("coreutils" ,coreutils)
       ("curl" ,curl)
       ("diffutils" ,diffutils)
       ("gawk" ,gawk)
       ("grep" ,grep)
       ("openssl" ,openssl)
       ("sed" ,sed)))
    (native-inputs
     `(("gzip" ,gzip)
       ("tar" ,tar)))
    (home-page "https://dehydrated.io/")
    (synopsis "Let's Encrypt/ACME client implemented as a shell script")
    (description "Dehydrated is a client for signing certificates with an
ACME-server (currently only provided by Let's Encrypt) implemented as a
relatively simple Bash script.")
    (license license:expat)))
