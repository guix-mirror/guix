;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015, 2018 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014, 2015, 2016, 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2016, 2017, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2016 Mike Gerwitz <mtg@gnu.org>
;;; Copyright © 2016 Troy Sankey <sankeytms@gmail.com>
;;; Copyright © 2017, 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Fredrik Salomonsson <plattfot@posteo.net>
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

(define-module (gnu packages gnupg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pth)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tor)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (srfi srfi-1))

(define-public libgpg-error
  (package
    (name "libgpg-error")
    (version "1.37")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnupg/libgpg-error/libgpg-error-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "0qwpx8mbc2l421a22l0l1hpzkip9jng06bbzgxwpkkvk5bvnybdk"))))
    (build-system gnu-build-system)
    (arguments
     (if (%current-target-system)
         `(#:modules ((ice-9 match)
                      (guix build gnu-build-system)
                      (guix build utils))
           #:phases
           (modify-phases %standard-phases
             ;; When cross-compiling, some platform specific properties cannot
             ;; be detected. Create a symlink to the appropriate platform
             ;; file. See Cross-Compiling section at:
             ;; https://github.com/gpg/libgpg-error/blob/master/README
             (add-after 'unpack 'cross-symlinks
               (lambda* (#:key target inputs #:allow-other-keys)
                 (let ((triplet
                        (match (string-take target
                                            (string-index target #\-))
                          ("armhf" "arm-unknown-linux-gnueabi")
                          ("mips64el" "mips-unknown-linux-gnu")
                          (x
                           (string-append x "-unknown-linux-gnu")))))
                   (symlink
                    (string-append "lock-obj-pub." triplet ".h")
                    "src/syscfg/lock-obj-pub.linux-gnu.h"))
                 #t))))
         '()))
    (native-inputs `(("gettext" ,gettext-minimal)))
    (home-page "https://gnupg.org")
    (synopsis "Library of error values for GnuPG components")
    (description
     "Libgpg-error is a small library that defines common error values
for all GnuPG components.  Among these are GPG, GPGSM, GPGME,
GPG-Agent, libgcrypt, Libksba, DirMngr, Pinentry, SmartCard
Daemon and possibly more in the future.")
    (license license:lgpl2.0+)
    (properties '((ftp-server . "ftp.gnupg.org")
                  (ftp-directory . "/gcrypt/libgpg-error")))))

(define-public libgcrypt
  (package
    (name "libgcrypt")
    (version "1.8.5")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnupg/libgcrypt/libgcrypt-"
                                 version ".tar.bz2"))
             (sha256
              (base32
                "1hvsazms1bfd769q0ngl0r9g5i4m9mpz9jmvvrdzyzk3rfa2ljiv"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libgpg-error-host" ,libgpg-error)))
    (native-inputs
     ;; Needed here for the 'gpg-error' program.
     `(("libgpg-error-native" ,libgpg-error)))
    (arguments
     ;; The '--with-gpg-error-prefix' argument is needed because otherwise
     ;; 'configure' uses 'gpg-error-config' to determine the '-L' flag, and
     ;; the 'gpg-error-config' it runs is the native one---i.e., the wrong one.
     `(#:configure-flags
       (list (string-append "--with-gpg-error-prefix="
                            (assoc-ref %build-inputs "libgpg-error-host"))
             ;; When cross-compiling, _gcry_mpih_lshift etc are undefined
             ,@(if (%current-target-system) '("--disable-asm")
                   '()))))
    (outputs '("out" "debug"))
    (home-page "https://gnupg.org/")
    (synopsis "Cryptographic function library")
    (description
     "Libgcrypt is a general-purpose cryptographic library.  It provides the
standard cryptographic building blocks such as symmetric ciphers, hash
algorithms, public key algorithms, large integer functions and random number
generation.")
    (license license:lgpl2.0+)
    (properties '((ftp-server . "ftp.gnupg.org")
                  (ftp-directory . "/gcrypt/libgcrypt")))))

(define-public libassuan
  (package
    (name "libassuan")
    (version "2.5.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnupg/libassuan/libassuan-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "1w7vnnycq4z7gf4bk38pi4hrb8qrrzgfpz3cd7frwldxnfbfx060"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libgpg-error" ,libgpg-error)
       ("pth" ,pth)))
    (home-page "https://gnupg.org")
    (synopsis
     "IPC library used by GnuPG and related software")
    (description
     "Libassuan is a small library implementing the so-called Assuan
protocol.  This protocol is used for IPC between most newer
GnuPG components.  Both, server and client side functions are
provided.")
    (license license:lgpl2.0+)
    (properties '((ftp-server . "ftp.gnupg.org")
                  (ftp-directory . "/gcrypt/libassuan")))))

(define-public libksba
  (package
    (name "libksba")
    (version "1.5.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://gnupg/libksba/libksba-"
            version ".tar.bz2"))
      (sha256
       (base32
        "1fm0mf3wq9fmyi1rmc1vk2fafn6liiw2mgxml3g7ybbb44lz2jmf"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libgpg-error" ,libgpg-error)))
    (native-inputs
     `(("libgpg-error" ,libgpg-error)))
    (arguments
     `(#:configure-flags
       (list ,@(if (%current-target-system)
                   '("CC_FOR_BUILD=gcc")
                   '())
             (string-append "--with-gpg-error-prefix="
                            (assoc-ref %build-inputs "libgpg-error")))))
    (home-page "https://www.gnupg.org")
    (synopsis "CMS and X.509 access library")
    (description
     "KSBA (pronounced Kasbah) is a library to make X.509 certificates
as well as the CMS easily accessible by other applications.  Both
specifications are building blocks of S/MIME and TLS.")
    (license license:gpl3+)
    (properties '((ftp-server . "ftp.gnupg.org")
                  (ftp-directory . "/gcrypt/libksba")))))

(define-public npth
  (package
    (name "npth")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnupg/npth/npth-" version ".tar.bz2"))
       (sha256
        (base32 "1lg2lkdd3z1s3rpyf88786l243adrzyk9p4q8z9n41ygmpcsp4qk"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnupg.org")
    (synopsis "Non-preemptive thread library")
    (description
     "Npth is a library to provide the GNU Pth API and thus a non-preemptive
threads implementation.

In contrast to GNU Pth is is based on the system's standard threads
implementation.  This allows the use of libraries which are not
compatible to GNU Pth.")
    (license (list license:lgpl3+ license:gpl2+)) ; dual license
    (properties '((ftp-server . "ftp.gnupg.org")
                  (ftp-directory . "/gcrypt/npth")))))

(define-public gnupg
  (package
    (name "gnupg")
    (version "2.2.27")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnupg/gnupg/gnupg-" version
                                  ".tar.bz2"))
              (patches (search-patches "gnupg-default-pinentry.patch"))
              (sha256
               (base32
                "1693s2rp9sjwvdslj94n03wnb6rxysjy0dli0q1698af044h1ril"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("gnutls" ,gnutls)
       ("libassuan" ,libassuan)
       ("libgcrypt" ,libgcrypt)
       ("libgpg-error" ,libgpg-error)
       ("libksba" ,libksba)
       ("npth" ,npth)
       ("openldap" ,openldap)
       ("pcsc-lite" ,pcsc-lite)
       ("readline" ,readline)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
   (arguments
    `(#:configure-flags '(;; Otherwise, the test suite looks for the `gpg`
                          ;; executable in its installation directory in
                          ;; /gnu/store before it has been installed.
                          "--enable-gnupg-builddir-envvar"
                          "--enable-all-tests")
      #:phases
      (modify-phases %standard-phases
        (add-before 'configure 'patch-paths
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "scd/scdaemon.c"
              (("\"(libpcsclite\\.so[^\"]*)\"" _ name)
               (string-append "\"" (assoc-ref inputs "pcsc-lite")
                              "/lib/" name "\"")))
            #t))
        (add-after 'build 'patch-scheme-tests
          (lambda _
            (substitute* (find-files "tests" ".\\.scm$")
              (("/usr/bin/env gpgscm")
               (string-append (getcwd) "/tests/gpgscm/gpgscm")))
            #t))
        (add-before 'build 'patch-test-paths
          (lambda _
            (substitute* '("tests/inittests"
                           "tests/pkits/inittests"
                           "tests/Makefile"
                           "tests/pkits/common.sh"
                           "tests/pkits/Makefile")
             (("/bin/pwd") (which "pwd")))
            (substitute* "common/t-exectool.c"
              (("/bin/cat") (which "cat"))
              (("/bin/true") (which "true"))
              (("/bin/false") (which "false")))
            #t)))))
    (home-page "https://gnupg.org/")
    (synopsis "GNU Privacy Guard")
    (description
     "The GNU Privacy Guard is a complete implementation of the OpenPGP
standard.  It is used to encrypt and sign data and communication.  It
features powerful key management and the ability to access public key
servers.  It includes several libraries: libassuan (IPC between GnuPG
components), libgpg-error (centralized GnuPG error values), and
libskba (working with X.509 certificates and CMS data).")
    (license license:gpl3+)
    (properties '((ftp-server . "ftp.gnupg.org")
                  (ftp-directory . "/gcrypt/gnupg")))))

(define-public gnupg-1
  (package (inherit gnupg)
    (version "1.4.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnupg/gnupg/gnupg-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "1fkq4sqldvf6a25mm2qz95swv1qjg464736091w51djiwqbjyin9"))))
    (native-inputs '())
    (inputs
     `(("zlib" ,zlib)
       ("bzip2" ,bzip2)
       ("curl" ,curl)
       ("readline" ,readline)
       ("libgpg-error" ,libgpg-error)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-check-sh
           (lambda _
             (substitute* "checks/Makefile.in"
               (("/bin/sh") (which "sh")))
             #t)))))))

(define-public gpgme
  (package
    (name "gpgme")
    (version "1.15.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnupg/gpgme/gpgme-" version ".tar.bz2"))
      (sha256
       (base32 "1bg13l5s8x9p1v0jyv29n84bay27pflindpzjsc9gj7i4wdkrg7f"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gnupg" ,gnupg)))
    (propagated-inputs
     ;; Needs to be propagated because gpgme.h includes gpg-error.h.
     `(("libgpg-error" ,libgpg-error)))
    (inputs
     `(("libassuan" ,libassuan)))
    (home-page "https://www.gnupg.org/related_software/gpgme/")
    (synopsis "Library providing simplified access to GnuPG functionality")
    (description
     "GnuPG Made Easy (GPGME) is a library designed to make access to GnuPG
easier for applications.  It provides a High-Level Crypto API for encryption,
decryption, signing, signature verification and key management.  Currently
it uses GnuPG as its backend but the API isn't restricted to this engine.

Because the direct use of GnuPG from an application can be a complicated
programming task, it is suggested that all software should try to use GPGME
instead.  This way bug fixes or improvements can be done at a central place
and every application benefits from this.")
    (license license:lgpl2.1+)
    (properties '((ftp-server . "ftp.gnupg.org")
                  (ftp-directory . "/gcrypt/gpgme")))))

(define-public qgpgme
  (package
    (inherit gpgme)
    (name "qgpgme")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'chdir-and-symlink
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gpgme (assoc-ref inputs "gpgme")))
               (symlink (string-append gpgme "/lib/libgpgmepp.la")
                        "lang/cpp/src/libgpgmepp.la")
               (symlink (string-append gpgme "/lib/libgpgme.la")
                        "src/libgpgme.la"))
             (chdir "lang/qt")
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ,@(package-native-inputs gpgme)))
    (inputs
     `(("gpgme" ,gpgme)
       ("qtbase" ,qtbase)
       ,@(package-inputs gpgme)))
    (synopsis "Qt API bindings for gpgme")
    (description "QGpgme provides a very high level Qt API around GpgMEpp.

QGpgME was originally developed as part of libkleo and incorporated into
gpgpme starting with version 1.7.")
    (license license:gpl2+))) ;; Note: this differs from gpgme

(define-public guile-gcrypt
  (package
    (name "guile-gcrypt")
    (version "0.3.0")
    (home-page "https://notabug.org/cwebber/guile-gcrypt")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append home-page ".git"))
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0m29fg4pdfifnqqsa437zc5c1bhbfh62mc69ba25ak4x2cla41ll"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     ;; Work around <https://bugs.gnu.org/20272> to achieve reproducible
     ;; builds.
     '(#:parallel-build? #f

       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'add-libgrypt-config
           (lambda* (#:key inputs target #:allow-other-keys)
             (when target
               ;; When cross-compiling, the bash script 'libgcrypt-config'
               ;; must be accessible during the configure phase.
               (setenv "PATH"
                       (string-append (assoc-ref inputs "libgcrypt")
                                      "/bin:" (getenv "PATH")))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("texinfo" ,texinfo)
       ("guile" ,guile-3.0)))
    (inputs
     `(("guile" ,guile-3.0)
       ("libgcrypt" ,libgcrypt)))
    (synopsis "Cryptography library for Guile using Libgcrypt")
    (description
     "Guile-Gcrypt provides a Guile interface to a subset of the
GNU Libgcrypt crytographic library.  It provides modules for cryptographic
hash functions, message authentication codes (MAC), public-key cryptography,
strong randomness, and more.  It is implemented using the foreign function
interface (FFI) of Guile.")
    (license license:gpl3+)))

(define-public guile2.0-gcrypt
  (package (inherit guile-gcrypt)
    (name "guile2.0-gcrypt")
    (native-inputs
     `(("guile" ,guile-2.0)
       ,@(alist-delete "guile" (package-native-inputs guile-gcrypt))))
    (inputs
     `(("guile" ,guile-2.0)
       ,@(alist-delete "guile" (package-inputs guile-gcrypt))))))

(define-public guile2.2-gcrypt
  (package
    (inherit guile-gcrypt)
    (name "guile2.2-gcrypt")
    (native-inputs
     `(("guile" ,guile-2.2)
       ,@(alist-delete "guile" (package-native-inputs guile-gcrypt))))
    (inputs
     `(("guile" ,guile-2.2)
       ,@(alist-delete "guile" (package-inputs guile-gcrypt))))))

(define-public guile3.0-gcrypt
  (deprecated-package "guile3.0-gcrypt" guile-gcrypt))

(define-public python-gpg
  (package
    (name "python-gpg")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "gpg" version))
              (sha256
               (base32
                "1ji3ynhp36m1ccx7bmaq75dhij9frpn19v9mpi4aajn8csl194il"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-environment
           (lambda _
             (substitute* "setup.py"
               (("cc") (which "gcc")))
             #t)))
       #:tests? #f)) ; No test suite.
    (inputs
     `(("gpgme" ,gpgme)))
    (native-inputs
     `(("swig" ,swig)))
    (home-page (package-home-page gpgme))
    (synopsis "Python bindings for GPGME GnuPG cryptography library")
    (description "This package provides Python bindings to the GPGME GnuPG
cryptographic library.  It is developed in the GPGME source code, and then
distributed separately.")
    (license license:lgpl2.1+)))

(define-public python2-gpg
  (package-with-python2 python-gpg))

(define-public python-pygpgme
  (package
    (name "python-pygpgme")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pygpgme" version))
       (sha256
        (base32
         "1q82p3gs6lwq8j8dxk4pvrwk3jpww1zqcjrzznl9clh10z28gn2z"))
       ;; Unfortunately, we have to disable some tests due to some gpg-agent
       ;; goofiness... see:
       ;;   https://bugs.launchpad.net/pygpgme/+bug/999949
       (patches (search-patches "pygpgme-disable-problematic-tests.patch"
                                "python-pygpgme-fix-pinentry-tests.patch"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'make-build
           (lambda _ (invoke "make" "build")))
         (replace 'check
           (lambda _ (invoke "make" "check"))))))
    (build-system python-build-system)
    (native-inputs
     `(("gnupg" ,gnupg-1)))
    (inputs
     `(("gpgme" ,gpgme)))
    (home-page "https://launchpad.net/pygpgme")
    (synopsis "Python module for working with OpenPGP messages")
    (description
     "PyGPGME is a Python module that lets you sign, verify, encrypt and
decrypt messages using the OpenPGP format by making use of GPGME.")
    (license license:lgpl2.1+)))

(define-public python2-pygpgme
  (package-with-python2 python-pygpgme))

(define-public python-gnupg
  (package
    (name "python-gnupg")
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-gnupg" version))
       (sha256
        (base32
         "03pvjyp6q9pr8qa22i38az06ddzhvzy5kj192hxa3gbhnchg1nj5"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (substitute* "test_gnupg.py"
               ;; Unsure why this test fails.
               (("'test_search_keys'") "True")
               (("def test_search_keys") "def disabled__search_keys"))
             (setenv "USERNAME" "guixbuilder")
             ;; The doctests are extremely slow and sometimes time out,
             ;; so we disable them.
             (invoke "python"
                     "test_gnupg.py" "--no-doctests"))))))
    (native-inputs
     `(("gnupg" ,gnupg-1)))
    (home-page "https://packages.python.org/python-gnupg/index.html")
    (synopsis "Wrapper for the GNU Privacy Guard")
    (description
      "This module allows easy access to GnuPG’s key management, encryption
and signature functionality from Python programs.")
    (license license:bsd-3)))

(define-public python2-gnupg
  (package-with-python2 python-gnupg))

(define-public perl-gnupg-interface
  (package
    (name "perl-gnupg-interface")
    (version "0.52")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/A/AL/ALEXMV/"
                                  "GnuPG-Interface-" version ".tar.gz"))
              (sha256
               (base32
                "0dgx8yhdsmhkazcrz14n4flrk1afv7azgl003hl4arxvi1d9yyi4"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; FIXME: This test fails for unknown reasons
         (add-after 'unpack 'delete-broken-test
           (lambda _
             (delete-file "t/encrypt_symmetrically.t")
             #t)))))
    (inputs
     `(("gnupg" ,gnupg-1)))
    (propagated-inputs
     `(("perl-moo" ,perl-moo)
       ("perl-moox-handlesvia" ,perl-moox-handlesvia)
       ("perl-moox-late" ,perl-moox-late)))
    (native-inputs
     `(("which" ,which)
       ("perl-module-install" ,perl-module-install)))
    (home-page "https://metacpan.org/release/GnuPG-Interface")
    (synopsis "Perl interface to GnuPG")
    (description "@code{GnuPG::Interface} and its associated modules are
designed to provide an object-oriented method for interacting with GnuPG,
being able to perform functions such as but not limited to encrypting,
signing, decryption, verification, and key-listing parsing.")
    (license license:perl-license)))

(define-public pius
  (package
   (name "pius")
   (version "2.2.7")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/jaymzh/pius/releases/download/v"
                  version "/pius-" version ".tar.bz2"))
            (sha256
             (base32
              "1nsl7czicv95j0gfz4s82ys3g3h2mwr6cq3ilid8bpz3iy7z4ipy"))))
   (build-system python-build-system)
   (inputs `(("perl" ,perl)             ; for 'pius-party-worksheet'
             ("gpg" ,gnupg)
             ("python-six" ,python2-six)))
   (arguments
    `(#:tests? #f
      #:python ,python-2                ; uses the Python 2 'print' syntax
      #:phases
      (modify-phases %standard-phases
        (add-before
         'build 'set-gpg-file-name
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let* ((gpg (string-append (assoc-ref inputs "gpg")
                                      "/bin/gpg")))
             (substitute* "libpius/constants.py"
               (("/usr/bin/gpg2") gpg))
             #t))))))
   (synopsis "Programs to simplify GnuPG key signing")
   (description
    "Pius (PGP Individual UID Signer) helps attendees of PGP keysigning
parties.  It is the main utility and makes it possible to quickly and easily
sign each UID on a set of PGP keys.  It is designed to take the pain out of
the sign-all-the-keys part of PGP Keysigning Party while adding security
to the process.

pius-keyring-mgr and pius-party-worksheet help organisers of
PGP keysigning parties.")
   (license license:gpl2)
   (home-page "https://www.phildev.net/pius/index.shtml")))

(define-public signing-party
  (package
    (name "signing-party")
    (version "2.11")
    (home-page "https://salsa.debian.org/signing-party-team/signing-party")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1aig5ssabzbk4mih7xd04vgr931bw0flbi8dz902wlr610gyv5s5"))))
    (build-system gnu-build-system)
    (native-inputs
     ;; autoconf-wrapper is required due to the non-standard
     ;; 'configure phase.
     `(("autoconf" ,autoconf-wrapper)
       ("automake" ,automake)))
    (inputs `(("perl" ,perl)
              ("perl-text-template" ,perl-text-template)
              ("perl-mime-tools" ,perl-mime-tools)
              ("perl-gnupg-interface" ,perl-gnupg-interface)
              ("perl-net-idn-encode" ,perl-net-idn-encode)
              ("libmd" ,libmd)))
    (arguments
     `(#:tests? #f ; no test suite
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "keyanalyze/Makefile"
                 (("LDLIBS") (string-append "CC=" (which "gcc") "\nLDLIBS")))
               (substitute* "keyanalyze/Makefile"
                 (("\\./configure") (string-append "./configure --prefix=" out)))
               (substitute* "gpgwrap/Makefile"
                 (("\\} clean")
                  (string-append "} clean\ninstall:\n\tinstall -D bin/gpgwrap "
                                 out "/bin/gpgwrap\n")))
               (substitute* '("gpgsigs/Makefile" "keyanalyze/Makefile"
                              "keylookup/Makefile" "sig2dot/Makefile"
                              "springgraph/Makefile")
                 (("/usr") out))
               (setenv "CONFIG_SHELL" (which "sh")))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys #:rest args)
             (let ((out (assoc-ref outputs "out"))
                   (install (assoc-ref %standard-phases 'install)))
               (apply install args)
               (for-each
                (lambda (dir file)
                  (copy-file (string-append dir "/" file)
                             (string-append out "/bin/" file)))
                '("caff" "caff" "caff" "gpgdir" "gpg-key2ps"
                  "gpglist" "gpg-mailkeys" "gpgparticipants")
                '("caff" "pgp-clean" "pgp-fixkey" "gpgdir" "gpg-key2ps"
                  "gpglist" "gpg-mailkeys" "gpgparticipants"))
               (for-each
                (lambda (dir file)
                  (copy-file (string-append dir "/" file)
                             (string-append out "/share/man/man1/" file)))
                '("caff" "caff" "caff" "gpgdir"
                  "gpg-key2ps" "gpglist" "gpg-mailkeys"
                  "gpgparticipants" "gpgsigs" "gpgwrap/doc"
                  "keyanalyze" "keyanalyze/pgpring" "keyanalyze")
                '("caff.1" "pgp-clean.1" "pgp-fixkey.1" "gpgdir.1"
                  "gpg-key2ps.1" "gpglist.1" "gpg-mailkeys.1"
                  "gpgparticipants.1" "gpgsigs.1" "gpgwrap.1"
                  "process_keys.1" "pgpring.1" "keyanalyze.1")))
             #t))
         (add-after 'install 'wrap-programs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (wrap-program
                   (string-append out "/bin/caff")
                 `("PERL5LIB" ":" prefix (,(getenv "PERL5LIB")))))
             #t)))))
    (synopsis "Collection of scripts for simplifying gnupg key signing")
    (description
     "Signing-party is a collection for all kinds of PGP/GnuPG related things,
including tools for signing keys, keyring analysis, and party preparation.
@enumerate
@item caff: CA - Fire and Forget signs and mails a key
@item pgp-clean: removes all non-self signatures from key
@item pgp-fixkey: removes broken packets from keys
@item gpg-mailkeys: simply mail out a signed key to its owner
@item gpg-key2ps: generate PostScript file with fingerprint paper strips
@item gpgdir: recursive directory encryption tool
@item gpglist: show who signed which of your UIDs
@item gpgsigs: annotates list of GnuPG keys with already done signatures
@item gpgparticipants: create list of party participants for the organiser
@item gpgwrap: a passphrase wrapper
@item keyanalyze: minimum signing distance (MSD) analysis on keyrings
@item keylookup: ncurses wrapper around gpg --search
@item sig2dot: converts a list of GnuPG signatures to a .dot file
@item springgraph: creates a graph from a .dot file
@end enumerate")
    ;; gpl2+ for almost all programs, except for keyanalyze: gpl2
    ;; and caff and gpgsigs: bsd-3, see
    ;; http://packages.debian.org/changelogs/pool/main/s/signing-party/current/copyright
    (license license:gpl2)))

(define-public pinentry-tty
  (package
    (name "pinentry-tty")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnupg/pinentry/pinentry-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0zx5vg6wws2sp2yxwi01b8i1pnsqkydncpj7x0p8xl9y05ja04nd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-pinentry-tty")))
    (inputs
     `(("ncurses" ,ncurses)
       ("libassuan" ,libassuan)
       ("libsecret" ,libsecret "out")))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://gnupg.org/aegypten2/")
    (synopsis "GnuPG's interface to passphrase input")
    (description
     "Pinentry provides a console that allows users to enter a passphrase when
@code{gpg} is run and needs it.")
    (license license:gpl2+)
    (properties '((ftp-server . "ftp.gnupg.org")
                  (ftp-directory . "/gcrypt/pinentry")
                  (upstream-name . "pinentry")))))

(define-public pinentry-emacs
  (package
    (inherit pinentry-tty)
    (name "pinentry-emacs")
    (arguments
     `(#:configure-flags '("--enable-pinentry-emacs")))
    (description
     "Pinentry provides a console and an Emacs interface that allows users to
enter a passphrase when required by @code{gpg} or other software.")))

(define-public pinentry-gtk2
  (package
    (inherit pinentry-tty)
    (name "pinentry-gtk2")
    (arguments
     `(#:configure-flags '("--enable-fallback-curses")))
    (inputs
     `(("gtk+" ,gtk+-2)
       ("glib" ,glib)
       ,@(package-inputs pinentry-tty)))
    (description
     "Pinentry provides a console and a GTK+ GUI that allows users to enter a
passphrase when @code{gpg} is run and needs it.")))

(define-public pinentry-gnome3
  (package
    (inherit pinentry-tty)
    (name "pinentry-gnome3")
    (inputs
     `(("gtk+" ,gtk+-2)
       ("gcr" ,gcr)
       ("glib" ,glib)
       ,@(package-inputs pinentry-tty)))
    (arguments
     `(#:configure-flags '("--enable-pinentry-gnome3"
                           "--enable-fallback-curses")))
    (description
     "Pinentry provides a console and a GUI designed for use with GNOME@tie{}3
that allows users to enter a passphrase when required by @code{gpg} or other
software.")))

(define-public pinentry-qt
  (package
    (inherit pinentry-tty)
    (name "pinentry-qt")
    (arguments
     `(#:configure-flags '("--enable-fallback-curses")))
    (inputs
     `(("qtbase" ,qtbase)
       ,@(package-inputs pinentry-tty)))
  (description
   "Pinentry provides a console and a Qt GUI that allows users to enter a
passphrase when @code{gpg} is run and needs it.")))

(define-public pinentry-efl
  (package
    (inherit pinentry-tty)
    (name "pinentry-efl")
    (arguments
     '(#:configure-flags '("--enable-pinentry-efl"
                           "--enable-fallback-curses")
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _
             (invoke "sh" "autogen.sh"))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ,@(package-native-inputs pinentry-tty)))
    (inputs
     `(("efl" ,efl)
       ,@(package-inputs pinentry-tty)))
    (description
   "Pinentry provides a console and a graphical interface for @acronym{EFL,
the Enlightenment Foundation Libraries} that allows users to enter a
passphrase when @code{gpg} is run and needs it.")))

(define-public pinentry-rofi
  (package
    (name "pinentry-rofi")
    (version "2.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/plattfot/pinentry-rofi/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0kjzvgni9srl8h5c52pqrvgdxs6avv0nhgk19apd97sx10qdwdhk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((ice-9 match)
        (ice-9 ftw)
        ,@%gnu-build-system-modules)
       #:phases
       (modify-phases
           %standard-phases
         (add-after 'install 'hall-wrap-binaries
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/"))
                    (site (string-append out "/share/guile/site"))
                    (rofi-bin (string-append (assoc-ref inputs "rofi") "/bin")))
               (match (scandir site)
                 (("." ".." version)
                  (wrap-program
                      (string-append bin "pinentry-rofi")
                    (list "PATH" ":" 'prefix `(,rofi-bin)))
                  #t)))))
         (add-after 'compress-documentation 'installcheck
           (lambda* rest
             (invoke "make" "installcheck"))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("autoconf-archive" ,autoconf-archive)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)))
    (inputs `(("guile" ,guile-3.0)
              ("rofi" ,rofi)))
    (synopsis "Rofi GUI for GnuPG's passphrase input")
    (description "Pinentry-rofi is a simple graphical user interface for
passphrase or PIN when required by @code{gpg} or other software.  It is using
the Rofi application launcher as the user interface.  Which makes it combined
with @code{rofi-pass} a good front end for @code{password-store}.")
    (home-page "https://github.com/plattfot/pinentry-rofi/")
    (license license:gpl3+)))

(define-public pinentry
  (package (inherit pinentry-gtk2)
    (name "pinentry")))

(define-public paperkey
  (package
    (name "paperkey")
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.jabberwocky.com/"
                                  "software/paperkey/paperkey-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1xq5gni6gksjkd5avg0zpd73vsr97appksfx0gx2m38s4w9zsid2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'patch-check-scripts
           (lambda _
             (substitute* '("checks/roundtrip.sh"
                            "checks/roundtrip-raw.sh")
               (("/bin/echo") "echo"))
             #t)))))
    (home-page "https://www.jabberwocky.com/software/paperkey/")
    (synopsis "Backup OpenPGP keys to paper")
    (description
     "Paperkey extracts the secret bytes from an OpenPGP (GnuPG, PGP, etc) key
for printing with paper and ink, which have amazingly long retention
qualities.  To reconstruct a secret key, you re-enter those
bytes (whether by hand, OCR, QR code, or the like) and paperkey can use
them to transform your existing public key into a secret key.")
    (license license:gpl2+)))

(define-public pgpdump
  (package
    (name "pgpdump")
    (version "0.33")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.mew.org/~kazu/proj/pgpdump/pgpdump-"
                           version ".tar.gz"))
       (sha256
        (base32 "1j001jra2m89n6cys3n0hs574bipjdzfxhzpnd4jfyv95mqwl7n4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no make check
       #:configure-flags (list "--prefix=/")
       #:make-flags (list "CC=gcc"
                          (string-append "DESTDIR=" (assoc-ref %outputs "out")))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "https://www.mew.org/~kazu/proj/pgpdump/en/")
    (synopsis "PGP packet visualizer")
    (description "pgpdump displays the sequence of OpenPGP or PGP version 2
packets from a file.

The output of this command is similar to GnuPG's list packets command,
however, pgpdump produces more detailed and easier to understand output.")
    (license license:bsd-3)))

(define-public gpa
  (package
    (name "gpa")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnupg/gpa/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1cbpc45f8qbdkd62p12s3q2rdq6fa5xdzwmcwd3xrj55bzkspnwm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (gnupg (assoc-ref inputs "gnupg")))
               (wrap-program (string-append out "/bin/gpa")
                 `("PATH" ":" prefix (,(string-append gnupg "/bin"))))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("gnupg" ,gnupg)
       ("gpgme" ,gpgme)
       ("libassuan" ,libassuan)
       ("libgpg-error" ,libgpg-error)
       ("gtk+-2" ,gtk+-2)))
    (home-page "https://gnupg.org/software/gpa/")
    (synopsis "Graphical user interface for GnuPG")
    (description
     "GPA, the GNU Privacy Assistant, is a graphical user interface for
@uref{https://gnupg.org, GnuPG}.  It can be used to encrypt, decrypt, and sign
files, to verify signatures, and to manage the private and public keys.")
    (license license:gpl3+)
    (properties '((ftp-server . "ftp.gnupg.org")
                  (ftp-directory . "/gcrypt/gpa")))))

(define-public parcimonie
  (package
    (name "parcimonie")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gaffer.boum.org/intrigeri/files/"
                                  "parcimonie/App-Parcimonie-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "14pvapvzrxh1yh8zgcj1llmc2dd8g1fgzskxlja21gmw8c88aqdk"))))
    (build-system perl-build-system)
    (inputs
     `(("gnupg" ,gnupg)
       ("perl-config-general" ,perl-config-general)
       ("perl-clone" ,perl-clone)
       ("perl-data" ,perl-data)
       ("perl-exporter-tiny" ,perl-exporter-tiny)
       ("perl-file-homedir" ,perl-file-homedir)
       ("perl-file-sharedir" ,perl-file-sharedir)
       ("perl-file-which" ,perl-file-which)
       ("perl-getopt-long-descriptive" ,perl-getopt-long-descriptive)
       ("perl-gnupg-interface" ,perl-gnupg-interface)
       ("perl-ipc-system-simple" ,perl-ipc-system-simple)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-libintl-perl" ,perl-libintl-perl) ; Locale::TextDomain
       ("perl-lwp-online" ,perl-lwp-online)
       ("perl-module-build" ,perl-module-build)
       ("perl-module-pluggable-object" ,perl-module-pluggable)
       ("perl-moo" ,perl-moo)
       ("perl-moox-handlesvia" ,perl-moox-handlesvia)
       ("perl-moox-late" ,perl-moox-late)
       ("perl-moox-options" ,perl-moox-options)
       ("perl-moox-strictconstructor" ,perl-moox-strictconstructor)
       ("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-net-dbus" ,perl-net-dbus)
       ("perl-net-dbus-glib" ,perl-net-dbus-glib)
       ("perl-path-tiny" ,perl-path-tiny)
       ("perl-strictures" ,perl-strictures-2)
       ("perl-test-most" ,perl-test-most)
       ("perl-test-trap" ,perl-test-trap)
       ("perl-time-duration" ,perl-time-duration)
       ("perl-time-duration-parse" ,perl-time-duration-parse)
       ("perl-try-tiny" ,perl-try-tiny)
       ("perl-type-tiny" ,perl-type-tiny)
       ("perl-types-path-tiny" ,perl-types-path-tiny)
       ("perl-unicode-linebreak" ,perl-unicode-linebreak)
       ("perl-xml-parser" ,perl-xml-parser)
       ("perl-xml-twig" ,perl-xml-twig)
       ("torsocks" ,torsocks)))
    (native-inputs
     `(("xorg-server" ,xorg-server-for-tests)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Needed for using gpg-connect-agent during tests.
         (add-before 'check 'prepare-for-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xorg-server (assoc-ref inputs "xorg-server")))
               (system (string-append xorg-server "/bin/Xvfb :1 &"))
               (setenv "DISPLAY" ":1")
               (setenv "HOME" "/tmp")
               ;; These tests are known to fail
               (delete-file "t/32-keyserver_defined_on_command_line.t")
               (delete-file "t/33-checkGpgHasDefinedKeyserver.t")
               ;; The applet is deprecated upstream.
               (delete-file "t/00-load_all.t")
               #t)))
         (add-before 'install 'fix-references
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "lib/App/Parcimonie/GnuPG/Interface.pm"
               ;; Skip check whether dependencies are in the PATH
               (("defined which.*") ""))
             #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (perllib (string-append out "/lib/perl5/site_perl/"
                                            ,(package-version perl))))
               (wrap-program (string-append out "/bin/parcimonie")
                 `("PERL5LIB" ":"
                   prefix (,(string-append perllib ":" (getenv "PERL5LIB")))))
               #t))))))
    (home-page "https://gaffer.boum.org/intrigeri/code/parcimonie/")
    (synopsis "Incrementally refreshes a GnuPG keyring")
    (description "Parcimonie incrementaly refreshes a GnuPG keyring in a way
that makes it hard to correlate the keyring content to an individual, and
makes it hard to locate an individual based on an identifying subset of her
keyring content.  Parcimonie is a daemon that fetches one key at a time using
the Tor network, waits a bit, changes the Tor circuit being used, and starts
over.")
    (license license:gpl1+)))

(define-public jetring
  (package
    (name "jetring")
    (version "0.30")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://salsa.debian.org/debian/jetring")
               (commit "535380166eb1b222ba34864af07f3e36f4fb52c9")))
        (file-name (git-file-name name version))
        (sha256
         (base32 "19m7rj446pr4nql44khwq0cfxfrm8cslj5v9jll08p7nk6glq5px"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure script
         (add-before 'install 'hardlink-gnupg
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gpg (string-append (assoc-ref inputs "gnupg")
                                       "/bin/gpg")))
               (substitute* (find-files "." "jetring-[[:alpha:]]+$")
                 (("gpg -") (string-append gpg " -"))
                 (("\\\"gpg\\\"") (string-append "\"" gpg "\"")))
               #t)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man")))
               (for-each (lambda (file)
                           (install-file file (string-append out "/bin/")))
                         (find-files "." "jetring-[[:alpha:]]+$"))
               (for-each (lambda (file)
                           (install-file file (string-append man "/man1/")))
                         (find-files "." ".*\\.1$"))
               (install-file "jetring.7" (string-append man "/man7/"))
               #t))))
       #:tests? #f)) ; no test phase
    (inputs
     `(("gnupg" ,gnupg)
       ("perl" ,perl)))
    (home-page "https://joeyh.name/code/jetring/")
    (synopsis "GnuPG keyring maintenance using changesets")
    (description
     "Jetring is a collection of tools that allow for gpg keyrings to be
maintained using changesets.  It was developed with the Debian keyring in mind,
and aims to solve the problem that a gpg keyring is a binary blob that's hard
for multiple people to collaboratively edit.

With jetring, changesets can be submitted, reviewed to see exactly what they
will do, applied, and used to build a keyring.  The origin of every change made
to the keyring is available for auditing, and gpg signatures can be used for
integrity guarantees.")
    (license license:gpl2+)))
