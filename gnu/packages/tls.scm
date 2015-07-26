;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
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
  #:use-module (gnu packages compression)
  #:use-module (gnu packages)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
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
    (version "3.4.1")
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
               "0bmih0zyiplr4v8798w0v9g3215zmganq18n8935cizkxj5zbdg9"))))
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
                   'install 'move-doc
                   (lambda* (#:key outputs #:allow-other-keys)
                     ;; Copy the 4.1 MiB of section 3 man pages to "doc".
                     (let* ((out    (assoc-ref outputs "out"))
                            (doc    (assoc-ref outputs "doc"))
                            (mandir (string-append doc "/share/man"))
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
   (version "1.0.2d")
   (source (origin
            (method url-fetch)
            (uri (string-append "ftp://ftp.openssl.org/source/openssl-" version
                                ".tar.gz"))
            (sha256
             (base32
              "1j58r7rdj9fz2lanir8ajbx4bspb5jnm5ikl6dq8lql5fx43c737"))
            (patches (list (search-patch "openssl-runpath.patch")))))
   (build-system gnu-build-system)
   (native-inputs `(("perl" ,perl)))
   (arguments
    `(#:parallel-build? #f
      #:parallel-tests? #f
      #:test-target "test"
      #:phases
      (alist-replace
       'configure
       (lambda* (#:key outputs #:allow-other-keys)
         (let ((out (assoc-ref outputs "out")))
           (zero?
            (system* "./config"
                     "shared"                   ; build shared libraries
                     "--libdir=lib"
                     (string-append "--prefix=" out)
                     ;; XXX FIXME: Work around a code generation bug in GCC
                     ;; 4.9.3 on ARM when compiled with -mfpu=neon.  See:
                     ;; <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=66917>
                     ,@(if (and (not (%current-target-system))
                                (string-prefix? "armhf" (%current-system)))
                           '("-mfpu=vfpv3")
                           '())))))
       (alist-cons-before
        'patch-source-shebangs 'patch-tests
        (lambda* (#:key inputs native-inputs #:allow-other-keys)
          (let ((bash (assoc-ref (or native-inputs inputs) "bash")))
            (substitute* (find-files "test" ".*")
              (("/bin/sh")
               (string-append bash "/bin/bash"))
              (("/bin/rm")
               "rm"))))
        %standard-phases))))
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
