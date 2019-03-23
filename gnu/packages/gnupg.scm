;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015, 2018 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2016, 2017, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2016, 2017 ng0 <ng0@n0.is>
;;; Copyright © 2016 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2016 Mike Gerwitz <mtg@gnu.org>
;;; Copyright © 2016 Troy Sankey <sankeytms@gmail.com>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
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
    (version "1.35")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnupg/libgpg-error/libgpg-error-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "0rz00xyrk7qzg7mmcjhvrnm34hddkvyg9zsqq548vj58m1ifxmfb"))))
    (build-system gnu-build-system)
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
    (version "1.8.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnupg/libgcrypt/libgcrypt-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "09r27ywj9zplq6n9qw3mn7zmvf6y2jdmwx5d1kg8yqkj0qx18f7n"))))
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
                            (assoc-ref %build-inputs "libgpg-error-host")))))
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
    (version "2.5.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnupg/libassuan/libassuan-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "00p7cpvzf0q3qwcgg51r9d0vbab4qga2xi8wpk2fgd36710b1g4i"))))
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
    (version "1.3.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://gnupg/libksba/libksba-"
            version ".tar.bz2"))
      (sha256
       (base32
        "0h53q4sns1jz1pkmhcz5wp9qrfn9f5g9i3vjv6dafwzzlvblyi21"))))
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
    (version "1.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnupg/npth/npth-" version ".tar.bz2"))
      (sha256
       (base32
        "1hmkkp6vzyrh8v01c2ynzf9vwikyagp7p1lxhbnr4ysk3w66jji9"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnupg.org")
    (synopsis "Non-preemptive thread library")
    (description
     "Npth is a library to provide the GNU Pth API and thus a non-preemptive
threads implementation.

In contrast to GNU Pth is is based on the system's standard threads
implementation.  This allows the use of libraries which are not
compatible to GNU Pth.")
    (license (list license:lgpl3+ license:gpl2+)))) ; dual license

(define-public gnupg
  (package
    (name "gnupg")
    (version "2.2.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnupg/gnupg/gnupg-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "0yzqrg24j9fc4f8ss5pclyvg70a9z53sv89vl77xii8yvi3fvy8v"))))
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

(define-public gnupg-2.0
  (package (inherit gnupg)
    (version "2.0.30")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnupg/gnupg/gnupg-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "0wax4cy14hh0h7kg9hj0hjn9424b71z8lrrc5kbsasrn9xd7hag3"))))
    (native-inputs '())
    (inputs
     `(("adns" ,adns)
       ("bzip2" ,bzip2)
       ("curl" ,curl)
       ("libassuan" ,libassuan)
       ("libgcrypt" ,libgcrypt)
       ("libgpg-error" ,libgpg-error)
       ("libksba" ,libksba)
       ("pth" ,pth)
       ("openldap" ,openldap)
       ("zlib" ,zlib)
       ("readline" ,readline)))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-before 'configure 'patch-config-files
          (lambda _
            (substitute* "tests/openpgp/Makefile.in"
              (("/bin/sh") (which "sh")))
            #t))
        (add-after 'install 'rename-v2-commands
          (lambda* (#:key outputs #:allow-other-keys)
            ;; Upstream suggests removing the trailing '2' from command names:
            ;; <http://debbugs.gnu.org/cgi/bugreport.cgi?bug=22883#58>.
            (let ((out (assoc-ref outputs "out")))
              (with-directory-excursion (string-append out "/bin")
                (rename-file "gpgv2" "gpgv")
                (rename-file "gpg2" "gpg")

                ;; Keep the old name around to ease transition.
                (symlink "gpgv" "gpgv2")
                (symlink "gpg" "gpg2")
                #t)))))))
   (properties `((superseded . ,gnupg)))))

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
    (version "1.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnupg/gpgme/gpgme-" version
                          ".tar.bz2"))
      (sha256
       (base32
        "1n4c1q2ls7sqx1vpr3p5n8vbjkw6kqp8jxqa28p0x9j36wf9bp5l"))
      ;; One test fails because the conflict keys have expired.  See
      ;; https://dev.gnupg.org/T3815
      (patches (list (origin
                       (method url-fetch)
                       (uri "https://dev.gnupg.org/rM66376f3e206a1aa791d712fb8577bb3490268f60?diff=1")
                       (file-name "gpgme-fix-conflict-test-keys.patch")
                       (sha256
                        (base32
                         "0j718iy5a9fhkrfs4gzrnm4ggi163prqf1i8lfmqczswvz88zfly")))))))
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
    (license license:lgpl2.1+)))

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
    (version "0.1.0")
    (home-page "https://notabug.org/cwebber/guile-gcrypt")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append home-page ".git"))
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1lhgh3105yi0ggrjsjibv4wp1ipz8s17pa820hk2wln3rc04wpvf"))
              (file-name (string-append name "-" version "-checkout"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("texinfo" ,texinfo)))
    (inputs
     `(("guile" ,guile-2.2)
       ("libgcrypt" ,libgcrypt)))
    (synopsis "Cryptography library for Guile using Libgcrypt")
    (description
     "Guile-Gcrypt provides a Guile 2.x interface to a subset of the
GNU Libgcrypt crytographic library.  It provides modules for cryptographic
hash functions, message authentication codes (MAC), public-key cryptography,
strong randomness, and more.  It is implemented using the foreign function
interface (FFI) of Guile.")
    (license license:gpl3+)))

(define-public guile2.0-gcrypt
  (package (inherit guile-gcrypt)
    (name "guile2.0-gcrypt")
    (inputs
     `(("guile" ,guile-2.0)
       ,@(alist-delete "guile" (package-inputs guile-gcrypt))))))

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
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-gnupg" version))
       (sha256
        (base32
         "03dc8whhvk7ccspbk8vzfhkxli8cd9zfbss5p597g4jldgy8s59d"))))
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
    (version "2.7")
    (home-page "https://salsa.debian.org/debian/signing-party")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "release-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gx9017wag4bgc0h7kca9n3jwwdm7z77yv3viayhg62flbwkvbgb"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf-wrapper)
       ("automake" ,automake)))
    (inputs `(("perl" ,perl)
              ("perl-text-template" ,perl-text-template)
              ("perl-mime-tools" ,perl-mime-tools)
              ("perl-gnupg-interface" ,perl-gnupg-interface)
              ("perl-net-idn-encode" ,perl-net-idn-encode)
              ("libmd" ,libmd)))
    (arguments
     `(#:tests? #f
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
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnupg/pinentry/pinentry-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0w35ypl960pczg5kp6km3dyr000m1hf0vpwwlh72jjkjza36c1v8"))))
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
     `(#:configure-flags '("--enable-pinentry-gnome3")))
    (description
     "Pinentry provides a console and a GUI designed for use with GNOME@tie{}3
that allows users to enter a passphrase when required by @code{gpg} or other
software.")))

(define-public pinentry-qt
  (package
    (inherit pinentry-tty)
    (name "pinentry-qt")
    (inputs
     `(("qtbase" ,qtbase)
       ,@(package-inputs pinentry-tty)))
    (arguments
     `(#:configure-flags '("CXXFLAGS=-std=gnu++11")))
  (description
   "Pinentry provides a console and a Qt GUI that allows users to enter a
passphrase when @code{gpg} is run and needs it.")))

(define-public pinentry-efl
  (package
    (inherit pinentry-tty)
    (name "pinentry-efl")
    (source
      (origin
        (inherit (package-source pinentry-tty))
        (patches (search-patches "pinentry-efl.patch"))))
    (arguments
     '(#:configure-flags '("--enable-pinentry-efl")
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
   "Pinentry provides a console and a graphical interface for the
@dfn{Enlightenment Foundation Libraries} (EFL) that allows users to enter a
passphrase when @code{gpg} is run and needs it.")))

(define-public pinentry
  (package (inherit pinentry-gtk2)
    (name "pinentry")))

(define-public paperkey
  (package
    (name "paperkey")
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.jabberwocky.com/"
                                  "software/paperkey/paperkey-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1prd2jaf4zjad3xhv160hmi5n408ssljfg7iz90jxs9w111pjwy4"))))
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
    (home-page "http://www.jabberwocky.com/software/paperkey/")
    (synopsis "Backup OpenPGP keys to paper")
    (description
     "Paperkey extracts the secret bytes from an OpenPGP (GnuPG, PGP, etc) key
for printing with paper and ink, which have amazingly long retention
qualities.  To reconstruct a secret key, you re-enter those
bytes (whether by hand, OCR, QR code, or the like) and paperkey can use
them to transform your existing public key into a secret key.")
    (license license:gpl2+)))

(define-public gpa
  (package
    (name "gpa")
    (version "0.9.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnupg/gpa/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "09xphbi2456qynwqq5n0yh0zdmdi2ggrj3wk4hsyh5lrzlvcrff3"))))
    (build-system gnu-build-system)
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
    (license license:gpl3+)))

(define-public parcimonie
  (package
    (name "parcimonie")
    (version "0.10.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gaffer.ptitcanardnoir.org/"
                                  "intrigeri/files/parcimonie/App-Parcimonie-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1kf891117s1f3k6lxvbjdb21va9gxh29vlp9bd664ssgw266rcyb"))))
    (build-system perl-build-system)
    (inputs
     `(("gnupg" ,gnupg-1)    ; This is the version used by perl-gnupg-interface
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
       ("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-net-dbus" ,perl-net-dbus)
       ("perl-net-dbus-glib" ,perl-net-dbus-glib)
       ("perl-path-tiny" ,perl-path-tiny)
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
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Needed for using gpg-connect-agent during tests.
         (add-before 'check 'set-HOME
           (lambda _ (setenv "HOME" "/tmp") #t))
         (add-before 'install 'fix-references
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "lib/App/Parcimonie/GnuPG/Interface.pm"
               (("gpg2") "gpg")
               ;; Skip check whether dependencies are in the PATH
               (("defined which.*") "")
               (("call\\('parcimonie-torified-gpg'\\)")
                (string-append "call('" (assoc-ref outputs "out")
                               "/bin/parcimonie-torified-gpg')")))
             (substitute* "bin/parcimonie-torified-gpg"
               (("torsocks") (string-append (assoc-ref inputs "torsocks")
                                            "/bin/torsocks")))
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
    (home-page "https://gaffer.ptitcanardnoir.org/intrigeri/code/parcimonie/")
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
    (version "0.27")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://debian/pool/main/j/" name "/"
                            name "_" version ".tar.xz"))
        (sha256
         (base32
          "0jy0x5zj7v87xgyldlsx1knzp0mv10wzamblrw1b61i2m1ii4pxz"))))
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
