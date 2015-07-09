;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
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
  #:use-module (gnu packages curl)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pth)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages tls)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public libgpg-error
  (package
    (name "libgpg-error")
    (version "1.18")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnupg/libgpg-error/libgpg-error-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "0408v19h3h0q6w61g51hgbdg6cyw81nyzkh70qfprvsc3pkddwcz"))))
    (build-system gnu-build-system)
    (home-page "http://gnupg.org")
    (synopsis "Library of error values for GnuPG components")
    (description
     "Libgpg-error is a small library that defines common error values
for all GnuPG components.  Among these are GPG, GPGSM, GPGME,
GPG-Agent, libgcrypt, Libksba, DirMngr, Pinentry, SmartCard
Daemon and possibly more in the future.")
    (license license:lgpl2.0+)))

(define-public libgcrypt
  (package
    (name "libgcrypt")
    (version "1.6.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnupg/libgcrypt/libgcrypt-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0pq2nwfqgggrsh8rk84659d80vfnlkbphwqjwahccd5fjdxr3d21"))))
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
    (home-page "http://gnupg.org/")
    (synopsis "Cryptographic function library")
    (description
     "Libgcrypt is a general-purpose cryptographic library.  It provides the
standard cryptographic building blocks such as symmetric ciphers, hash
algorithms, public key algorithms, large integer functions and random number
generation.")
    (license license:lgpl2.0+)))

(define-public libgcrypt-1.5
  (package (inherit libgcrypt)
    (version "1.5.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnupg/libgcrypt/libgcrypt-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "0czvqxkzd5y872ipy6s010ifwdwv29sqbnqc4pf56sd486gqvy6m"))))))

(define-public libassuan
  (package
    (name "libassuan")
    (version "2.2.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnupg/libassuan/libassuan-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "1pp2kl5gc2vja41g3wk03h1hgh7gxy6pj354fb5n4lrlg6xqb4ll"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libgpg-error" ,libgpg-error) ("pth" ,pth)))
    (home-page "http://gnupg.org")
    (synopsis
     "IPC library used by GnuPG and related software")
    (description
     "Libassuan is a small library implementing the so-called Assuan
protocol.  This protocol is used for IPC between most newer
GnuPG components.  Both, server and client side functions are
provided.")
    (license license:lgpl2.0+)))

(define-public libksba
  (package
    (name "libksba")
    (version "1.3.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://gnupg/libksba/libksba-"
            version ".tar.bz2"))
      (sha256
       (base32
        "11kp3h9l3b8ikydkcdkwgx45r662zi30m26ra5llyhfh6kz5yzqc"))))
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
    (home-page "http://www.gnupg.org")
    (synopsis "CMS and X.509 access library")
    (description
     "KSBA (pronounced Kasbah) is a library to make X.509 certificates
as well as the CMS easily accessible by other applications.  Both
specifications are building blocks of S/MIME and TLS.")
    (license license:gpl3+)))

(define-public npth
  (package
    (name "npth")
    (version "1.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://gnupg/npth/npth-"
            version ".tar.bz2"))
      (sha256
       (base32
        "12n0nvhw4fzwp0k7gjv3rc6pdml0qiinbbfiz4ilg6pl5kdxvnvd"))))
    (build-system gnu-build-system)
    (home-page "http://www.gnupg.org")
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
    (version "2.1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnupg/gnupg/gnupg-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "1zcj5vsrc64zyq7spnx2xlxlq6wxaf5bilpf6gbkp7qr8barlnay"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("bzip2" ,bzip2)
       ("curl" ,curl)
       ("gnutls" ,gnutls)
       ("libassuan" ,libassuan)
       ("libgcrypt" ,libgcrypt)
       ("libgpg-error" ,libgpg-error)
       ("libksba" ,libksba)
       ("npth" ,npth)
       ("openldap" ,openldap)
       ("zlib" ,zlib)
       ("readline" ,readline)))
   (arguments
    `(#:phases
       (alist-cons-before
        'configure 'patch-config-files
        (lambda _
          (substitute* "tests/openpgp/defs.inc"
            (("/bin/pwd") (which "pwd"))))
       %standard-phases)))
    (home-page "http://gnupg.org/")
    (synopsis "GNU Privacy Guard")
    (description
     "The GNU Privacy Guard is a complete implementation of the OpenPGP
standard.  It is used to encrypt and sign data and communication.  It
features powerful key management and the ability to access public key
servers.  It includes several libraries: libassuan (IPC between GnuPG
components), libgpg-error (centralized GnuPG error values), and
libskba (working with X.509 certificates and CMS data).")
    (license license:gpl3+)))

(define-public gnupg-2.0
  (package (inherit gnupg)
    (version "2.0.28")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnupg/gnupg/gnupg-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "0k2k399fnhfhhr4dvm8d6vs4ihq6gg06191lzfwikzaqmgj2w2ff"))))
    (native-inputs '())
    (inputs
     `(("bzip2" ,bzip2)
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
       (alist-cons-before
        'configure 'patch-config-files
        (lambda _
          (substitute* "tests/openpgp/Makefile.in"
            (("/bin/sh") (which "bash"))))
       %standard-phases)))))

(define-public gnupg-1
  (package (inherit gnupg)
    (version "1.4.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnupg/gnupg/gnupg-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "11pxx26sfilh0vswylh9mhiifw5yffw7nn733zknw3sb0jfk22bz"))))
    (native-inputs '())
    (inputs
     `(("zlib" ,zlib)
       ("bzip2" ,bzip2)
       ("curl" ,curl)
       ("readline" ,readline)
       ("libgpg-error" ,libgpg-error)))
    (arguments
     `(#:phases (alist-cons-after
                 'unpack 'patch-check-sh
                 (lambda _
                   (substitute* "checks/Makefile.in"
                     (("/bin/sh") (which "bash"))))
                 %standard-phases)))))

(define-public gpgme
  (package
    (name "gpgme")
    (version "1.5.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnupg/gpgme/gpgme-" version
                          ".tar.bz2"))
      (sha256
       (base32
        "01y28fkq52wwf4p470wscaxd2vgzl615irmafx3mj3380x8ksg8b"))))
    (build-system gnu-build-system)
    (propagated-inputs
     ;; Needs to be propagated because gpgme.h includes gpg-error.h.
     `(("libgpg-error" ,libgpg-error)))
    (inputs
     `(("gnupg" ,gnupg-2.0)
       ("libassuan" ,libassuan)))
    (arguments '(#:make-flags '("GPG=gpg2")))
    (home-page "http://www.gnupg.org/related_software/gpgme/")
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

(define-public pius
  (package
   (name "pius")
   (version "2.1.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/pgpius/pius/"
                                version "/pius-"
                                version ".tar.bz2"))
            (sha256 (base32
                     "0ry3kc3x1qjmvb581ja2z2v32r1rl1g8rhfj7iqvs8nzq4ca512i"))))
   (build-system gnu-build-system)
   (inputs `(("perl" ,perl)
             ("python" ,python-2)          ;uses the Python 2 'print' syntax
             ("gpg" ,gnupg-2.0)))          ;2.1 fails to talk to gpg-agent 2.0
   (arguments
    `(#:tests? #f
      #:phases
       (alist-delete
        'configure
       (alist-delete
        'build
       (alist-replace
        'install
        (lambda* (#:key inputs outputs #:allow-other-keys)
          (let* ((out (assoc-ref outputs "out"))
                 (gpg (string-append (assoc-ref inputs "gpg")
                                     "/bin/gpg2")))
            (mkdir out)
            (mkdir (string-append out "/bin"))
            (for-each (lambda (file)
                        (substitute* file
                          (("/usr/bin/gpg") gpg))
                        (copy-file file (string-append out "/bin/" file)))
                      '("pius" "pius-keyring-mgr" "pius-party-worksheet"))))
       %standard-phases)))))
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
   (home-page "http://www.phildev.net/pius/index.shtml")))

(define-public signing-party
  (package
   (name "signing-party")
   (version "1.1.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://ftp.debian.org/debian/pool/main/s/signing-party/signing-party_"
                                version ".orig.tar.gz"))
            (sha256 (base32
                     "188gp0prbh8qs29lq3pbf0qibfd6jq4fk7i0pfrybl8aahvm84rx"))))
   (build-system gnu-build-system)
   (inputs `(("perl" ,perl)))
   (arguments
    `(#:tests? #f
      #:phases
      (alist-cons-after
       'unpack 'remove-spurious-links
       (lambda _ (delete-file "keyanalyze/pgpring/depcomp"))
      (alist-replace
       'configure
       (lambda* (#:key outputs #:allow-other-keys)
         (let ((out (assoc-ref outputs "out")))
           (substitute* "keyanalyze/Makefile"
             (("LDLIBS") (string-append "CC=" (which "gcc") "\nLDLIBS")))
           (substitute* "keyanalyze/Makefile"
             (("./configure") (string-append "./configure --prefix=" out)))
           (substitute* "keyanalyze/pgpring/configure"
             (("/bin/sh") (which "bash")))
           (substitute* "gpgwrap/Makefile"
             (("\\} clean") (string-append "} clean\ninstall:\n\tinstall -D bin/gpgwrap "
                                      out "/bin/gpgwrap\n")))
           (substitute* '("gpgsigs/Makefile" "keyanalyze/Makefile"
                          "keylookup/Makefile" "sig2dot/Makefile"
                          "springgraph/Makefile")
             (("/usr") out))))
       (alist-replace
        'install
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
                "process_keys.1" "pgpring.1" "keyanalyze.1"))))
      %standard-phases)))))
   (synopsis "Collection of scripts for simplifying gnupg key signing")
   (description
    "Signing-party is a collection for all kinds of PGP/GnuPG related things,
including tools for signing keys, keyring analysis, and party preparation.

 * caff: CA - Fire and Forget signs and mails a key

 * pgp-clean: removes all non-self signatures from key

 * pgp-fixkey: removes broken packets from keys

 * gpg-mailkeys: simply mail out a signed key to its owner

 * gpg-key2ps: generate PostScript file with fingerprint paper strips

 * gpgdir: recursive directory encryption tool

 * gpglist: show who signed which of your UIDs

 * gpgsigs: annotates list of GnuPG keys with already done signatures

 * gpgparticipants: create list of party participants for the organiser

 * gpgwrap: a passphrase wrapper

 * keyanalyze: minimum signing distance (MSD) analysis on keyrings

 * keylookup: ncurses wrapper around gpg --search

 * sig2dot: converts a list of GnuPG signatures to a .dot file

 * springgraph: creates a graph from a .dot file")
   ;; gpl2+ for almost all programs, except for keyanalyze: gpl2
   ;; and caff and gpgsigs: bsd-3, see
   ;; http://packages.debian.org/changelogs/pool/main/s/signing-party/current/copyright
   (license license:gpl2)
   (home-page "http://pgp-tools.alioth.debian.org/")))

(define-public pinentry
  (package
    (name "pinentry")
    (version "0.9.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnupg/pinentry/pinentry-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1338hj1h3sh34897120y30x12b64wyj3xjzzk5asm2hdzhxgsmva"))))
    (build-system gnu-build-system)
    (inputs
     `(("ncurses" ,ncurses)
       ("libassuan" ,libassuan)
       ("gtk+" ,gtk+-2)
       ("glib" ,glib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://gnupg.org/aegypten2/")
    (synopsis "GnuPG's interface to passphrase input")
    (description
     "Pinentry provides a console and a GTK+ GUI that allows users to
enter a passphrase when `gpg' or `gpg2' is run and needs it.")
    (license license:gpl2+)))

(define-public paperkey
  (package
    (name "paperkey")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.jabberwocky.com/"
                                  "software/paperkey/paperkey-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1yybj8bj68v4lxwpn596b6ismh2fyixw5vlqqg26byrn4d9dfmsv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (alist-cons-before
        'check 'patch-check-scripts
        (lambda _
          (substitute* '("checks/roundtrip.sh"
                         "checks/roundtrip-raw.sh")
            (("/bin/echo") "echo")))
        %standard-phases)))
    (home-page "http://www.jabberwocky.com/software/paperkey/")
    (synopsis "Backup OpenPGP keys to paper")
    (description
     "Paperkey extracts the secret bytes from an OpenPGP (GnuPG, PGP, etc) key
for printing with paper and ink, which have amazingly long retention
qualities.  To reconstruct a secret key, you re-enter those
bytes (whether by hand, OCR, QR code, or the like) and paperkey can use
them to transform your existing public key into a secret key.")
    (license license:gpl2+)))
