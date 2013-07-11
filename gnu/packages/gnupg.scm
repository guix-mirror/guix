;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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
  #:use-module (guix licenses)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pth)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module ((gnu packages compression)
                #:renamer (symbol-prefix-proc 'guix:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public libgpg-error
  (package
    (name "libgpg-error")
    (version "1.12")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnupg/libgpg-error/libgpg-error-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "0pz58vr12qihq2f0bypjxsb6cf6ajq5258fmfm8s6lvwm3b9xz6a"))))
    (build-system gnu-build-system)
    (home-page "http://gnupg.org")
    (synopsis
     "Libgpg-error, a small library that defines common error values for all GnuPG components")
    (description
     "Libgpg-error is a small library that defines common error values
for all GnuPG components.  Among these are GPG, GPGSM, GPGME,
GPG-Agent, libgcrypt, Libksba, DirMngr, Pinentry, SmartCard
Daemon and possibly more in the future.")
    (license lgpl2.0+)))

(define-public libgcrypt
  (package
    (name "libgcrypt")
    (version "1.5.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnupg/libgcrypt/libgcrypt-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0gwnzqd64cpwdmk93nll54nidsr74jpimxzj4p4z7502ylwl66p4"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libgpg-error" ,libgpg-error)))
    (home-page "http://gnupg.org/")
    (synopsis "Cryptographic function library")
    (description
     "GNU Libgcrypt is a general purpose cryptographic library based on
the code from GnuPG.  It provides functions for all
cryptographic building blocks: symmetric ciphers, hash
algorithms, MACs, public key algorithms, large integer
functions, random numbers and a lot of supporting functions.")
    (license lgpl2.0+)))

(define-public libassuan
  (package
    (name "libassuan")
    (version "2.1.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnupg/libassuan/libassuan-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "1783nb0b6nr9yjhb5wfh8ykc5w89f4anppz1kz9913mqg5vxdqi3"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libgpg-error" ,libgpg-error) ("pth" ,pth)))
    (home-page "http://gnupg.org")
    (synopsis
     "Libassuan, the IPC library used by GnuPG and related software")
    (description
     "Libassuan is a small library implementing the so-called Assuan
protocol.  This protocol is used for IPC between most newer
GnuPG components.  Both, server and client side functions are
provided.")
    (license lgpl2.0+)))

(define-public libksba
  (package
    (name "libksba")
    (version "1.2.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://gnupg/libksba/libksba-"
            version ".tar.bz2"))
      (sha256
       (base32
        "0jwk7hm3x3g4hd7l12z3d79dy7359x7lc88dq6z7q0ixn1jwxbq9"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libgpg-error" ,libgpg-error)))
    (home-page "http://www.gnupg.org")
    (synopsis
     "Libksba is a CMS and X.509 access library under development")
    (description
     "KSBA (pronounced Kasbah) is a library to make X.509 certificates
as well as the CMS easily accessible by other applications.  Both
specifications are building blocks of S/MIME and TLS.")
    (license gpl3+)))

(define-public gnupg
  (package
    (name "gnupg")
    (version "2.0.20")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnupg/gnupg/gnupg-" version
                          ".tar.bz2"))
      (sha256
       (base32
        "16mp0j5inrcqcb3fxbn0b3aamascy3n923wiy0y8marc0rzrp53f"))))
    (build-system gnu-build-system)
    (inputs
     `(;; TODO: Add missing optional dep libusb.
;;        ("libusb" ,libusb)
       ("bzip2" ,guix:bzip2)
       ("curl" ,curl)
       ("libassuan" ,libassuan)
       ("libgcrypt" ,libgcrypt)
       ("libgpg-error" ,libgpg-error)
       ("libksba" ,libksba)
       ("pth" ,pth)
       ("openldap" ,openldap)
       ("zlib" ,guix:zlib)
       ("readline" ,readline)))
   (arguments
    `(#:phases
       (alist-replace
        'configure
        (lambda* (#:key #:allow-other-keys #:rest args)
         (let ((configure (assoc-ref %standard-phases 'configure)))
           (substitute* "tests/openpgp/Makefile.in"
             (("/bin/sh") (which "bash")))
           (apply configure args)))
       %standard-phases)))
    (home-page "http://gnupg.org/")
    (synopsis "GNU Privacy Guard")
    (description
     "GnuPG is the GNU project's complete and free implementation of
the OpenPGP standard as defined by RFC4880.  GnuPG allows to
encrypt and sign your data and communication, features a
versatile key managment system as well as access modules for all
kind of public key directories.  GnuPG, also known as GPG, is a
command line tool with features for easy integration with other
applications.  A wealth of frontend applications and libraries
are available.  Version 2 of GnuPG also provides support for
S/MIME.")
    (license gpl3+)))

(define-public pius
  (package
   (name "pius")
   (version "2.0.9")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/pgpius/pius/"
                                version "/pius-"
                                version ".tar.bz2"))
            (sha256 (base32
                     "1g1jly3wl4ks6h8ydkygyl2c4i7v3z91rg42005m6vm70y1d8b3d"))))
   (build-system gnu-build-system)
   (inputs `(("perl" ,perl)
             ("python" ,python)))
   (arguments
    `(#:tests? #f
      #:phases
       (alist-replace
        'configure
        (lambda* (#:key #:allow-other-keys) #t)
       (alist-replace
        'build
        (lambda* (#:key #:allow-other-keys) #t)
       (alist-replace
        'install
        (lambda* (#:key outputs #:allow-other-keys)
          (let ((out (assoc-ref outputs "out")))
            (mkdir out)
            (mkdir (string-append out "/bin"))
            (for-each
              (lambda (filename)
                (copy-file filename (string-append out "/bin/" filename)))
              '("pius" "pius-keyring-mgr" "pius-party-worksheet"))))
       %standard-phases)))))
   (synopsis "programs to simplify gnupg key signing")
   (description
    "Pius (PGP Individual UID Signer) helps attendees of PGP keysigning
parties. It is the main utility and makes it possible to quickly and easily
sign each UID on a set of PGP keys. It is designed to take the pain out of
the sign-all-the-keys part of PGP Keysigning Party while adding security
to the process.

pius-keyring-mgr and pius-party-worksheet help organisers of
PGP keysigning parties.")
   (license gpl2)
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
      (alist-replace
       'unpack
       (lambda* (#:key #:allow-other-keys #:rest args)
         (let ((unpack (assoc-ref %standard-phases 'unpack)))
           (apply unpack args)
           ;; remove spurious symlink
           (delete-file "keyanalyze/pgpring/depcomp")))
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
   (synopsis "collection of scripts for simplifying gnupg key signing")
   (description
    "signing-party is a collection for all kinds of PGP/GnuPG related things,
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
   (license gpl2)
   (home-page "http://pgp-tools.alioth.debian.org/")))
