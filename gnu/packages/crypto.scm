;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox>
;;; Copyright © 2016 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2017 ng0 <ng0@infotropique.org>
;;; Copyright © 2016, 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017 Pierre Langlois <pierre.langlois@gmx.com>
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

(define-module (gnu packages crypto)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages search)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl))

(define-public libsodium
  (package
    (name "libsodium")
    (version "1.0.16")
    (source (origin
            (method url-fetch)
            (uri (list (string-append
                        "https://download.libsodium.org/libsodium/"
                        "releases/libsodium-" version ".tar.gz")
                       (string-append
                        "https://download.libsodium.org/libsodium/"
                        "releases/old/libsodium-" version ".tar.gz")))
            (sha256
             (base32
              "0cq5pn7qcib7q70mm1lgjwj75xdxix27v0xl1xl0kvxww7hwgbgf"))))
    (build-system gnu-build-system)
    (synopsis "Portable NaCl-based crypto library")
    (description
     "Sodium is a new easy-to-use high-speed software library for network
communication, encryption, decryption, signatures, etc.")
    (license license:isc)
    (home-page "https://libsodium.org")))

(define-public libmd
  (package
    (name "libmd")
    (version "0.0.0")
    (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://archive.hadrons.org/software/libmd/libmd-"
                  version
                  ".tar.xz"))
            (sha256
             (base32
              "121s73pgbqsnmy6xblbrkj9y44c5zzzpf2hcmh6zvcvg4dk26gzx"))))
    (build-system gnu-build-system)
    (synopsis "Message Digest functions from BSD systems")
    (description
     "The currently provided message digest algorithms are:
@itemize
@item MD2
@item MD4
@item MD5
@item RIPEMD-160
@item SHA-1
@item SHA-2 (SHA-256, SHA-384 and SHA-512)
@end itemize")
    (license (list license:bsd-3
                   license:bsd-2
                   license:isc
                   license:public-domain))
    (home-page "https://www.hadrons.org/software/libmd/")))

(define-public signify
  (package
    (name "signify")
    (version "23")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/aperezdc/signify/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c70mzawgahsvmsv4xdrass4pgyynd67ipd9lij0fgi8wkq0ns8w"))))
    (build-system gnu-build-system)
    ;; TODO Build with libwaive (described in README.md), to implement something
    ;; like OpenBSD's pledge().
    (arguments
     `(#:tests? #f ; no test suite
       #:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libbsd" ,libbsd)))
    (synopsis "Create and verify cryptographic signatures")
    (description "The signify utility creates and verifies cryptographic
signatures using the elliptic curve Ed25519.  This is a Linux port of the
OpenBSD tool of the same name.")
    (home-page "https://github.com/aperezdc/signify")
    ;; This package includes third-party code that was originally released under
    ;; various non-copyleft licenses. See the source files for clarification.
    (license (list license:bsd-3 license:bsd-4 license:expat license:isc
                   license:public-domain (license:non-copyleft
                                          "file://base64.c"
                                          "See base64.c in the distribution for
                                           the license from IBM.")))))


(define-public opendht
  (package
    (name "opendht")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/savoirfairelinux/" name
         "/archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "src/argon2")
           (substitute* "src/Makefile.am"
             (("./argon2/libargon2.la") "")
             (("SUBDIRS = argon2") ""))
           (substitute* "src/crypto.cpp"
             (("argon2/argon2.h") "argon2.h"))
           (substitute* "configure.ac"
             (("src/argon2/Makefile") ""))))
       (sha256
        (base32
         "09yvkmbqbym3b5md4n96qc1s9sf2n8ji404hagih45rmsj49599x"))))
    (build-system gnu-build-system)
    (inputs
     `(("gnutls" ,gnutls)
       ("nettle" ,nettle)
       ("readline" ,readline)
       ("argon2" ,argon2)))
    (propagated-inputs
     `(("msgpack" ,msgpack)))           ;included in several installed headers
    (native-inputs
     `(("autoconf" ,autoconf)
       ("pkg-config" ,pkg-config)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (arguments
     `(#:configure-flags '("--disable-tools" "--disable-python")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'autoconf
                    (lambda _
                      (zero? (system* "autoreconf" "-vfi")))))))
    (home-page "https://github.com/savoirfairelinux/opendht/")
    (synopsis "Distributed Hash Table (DHT) library")
    (description "OpenDHT is a Distributed Hash Table (DHT) library.  It may
be used to manage peer-to-peer network connections as needed for real time
communication.")
    (license license:gpl3)))

(define-public encfs
  (package
    (name "encfs")
    (version "1.9.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/vgough/encfs/releases/download/v"
                       version "/encfs-" version ".tar.gz"))
       (sha256
        (base32
         "1906254dg5hwljh0h4gyrw09ms3b57dlhjfzhfzffv50yzpkl837"))
       (modules '((guix build utils)))
       ;; Remove bundled dependencies in favour of proper inputs.
       (snippet '(for-each delete-file-recursively
                           (find-files "internal" "^tinyxml2-[0-9]"
                                       #:directories? #t)))))
    (build-system cmake-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)

       ;; Test dependencies.
       ("expect" ,expect)
       ("perl" ,perl)))
    (inputs
     `(("attr" ,attr)
       ("fuse" ,fuse)
       ("openssl" ,openssl)
       ("tinyxml2" ,tinyxml2)))
    (arguments
     `(#:configure-flags (list "-DUSE_INTERNAL_TINYXML=OFF")))
    (home-page "https://vgough.github.io/encfs")
    (synopsis "Encrypted virtual file system")
    (description
     "EncFS creates a virtual encrypted file system in user-space.  Each file
created under an EncFS mount point is stored as a separate encrypted file on
the underlying file system.  Like most encrypted file systems, EncFS is meant
to provide security against off-line attacks, such as a drive falling into
the wrong hands.")
    (license (list license:expat                  ; internal/easylogging++.h
                   license:lgpl3+                 ; encfs library
                   license:gpl3+))))              ; command-line tools

(define-public keyutils
  (package
    (name "keyutils")
    (version "1.5.9")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://people.redhat.com/dhowells/keyutils/keyutils-"
                       version ".tar.bz2"))
       (sha256
        (base32
         "1bl3w03ygxhc0hz69klfdlwqn33jvzxl1zfl2jmnb2v85iawb8jd"))
       (modules '((guix build utils)))
       ;; Create relative symbolic links instead of absolute ones to /lib/*
       (snippet '(substitute* "Makefile" (("\\$\\(LNS\\) \\$\\(LIBDIR\\)/")
                                          "$(LNS) ")))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure))          ; no configure script
       #:make-flags (list "CC=gcc"
                          "RPATH=-Wl,-rpath,$(DESTDIR)$(LIBDIR)"
                          (string-append "DESTDIR="
                                         (assoc-ref %outputs "out"))
                          "INCLUDEDIR=/include"
                          "LIBDIR=/lib"
                          "MANDIR=/share/man"
                          "SHAREDIR=/share/keyutils")
       #:test-target "test"))
    (home-page "https://people.redhat.com/dhowells/keyutils/")
    (synopsis "Linux key management utilities")
    (description
     "Keyutils is a set of utilities for managing the key retention facility in
the Linux kernel, which can be used by file systems, block devices, and more to
gain and retain the authorization and encryption keys required to perform
secure operations. ")
    (license (list license:lgpl2.1+             ; the files keyutils.*
                   license:gpl2+))))            ; the rest

;; There is no release candidate but commits point out a version number,
;; furthermore no tarball exists.
(define-public eschalot
  (let ((commit "0bf31d88a11898c19b1ed25ddd2aff7b35dbac44")
        (revision "1"))
    (package
      (name "eschalot")
      (version (string-append "1.2.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/schnabear/eschalot")
               (commit commit)))
         (file-name (string-append name "-" version))
         (sha256
          (base32
           "0lj38ldh8vzi11wp4ghw4k0fkwp0s04zv8k8d473p1snmbh7mx98"))))
      (inputs
       `(("openssl" ,openssl))) ; It needs: openssl/{bn,pem,rsa,sha}.h
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list "CC=gcc"
                            (string-append "PREFIX=" (assoc-ref %outputs "out"))
                            (string-append "INSTALL=" "install"))
         ;; XXX: make test would run a !VERY! long hashing of names with the use
         ;; of a wordlist, the amount of computing time this would waste on build
         ;; servers is in no relation to the size or importance of this small
         ;; application, therefore we run our own tests on eschalot and worgen.
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'check
             (lambda _
               (and
                 (zero? (system* "./worgen" "8-12" "top1000.txt" "3-10" "top400nouns.txt"
                                 "3-6" "top150adjectives.txt" "3-6"))
                 (zero? (system* "./eschalot" "-r" "^guix|^guixsd"))
                 (zero? (system* "./eschalot" "-r" "^gnu|^free"))
                 (zero? (system* "./eschalot" "-r" "^cyber|^hack"))
                 (zero? (system* "./eschalot" "-r" "^troll")))))
           ;; Make install can not create the bin dir, create it.
           (add-before 'install 'create-bin-dir
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (mkdir-p bin)
                 #t))))))
      (home-page "https://github.com/schnabear/eschalot")
      (synopsis "Tor hidden service name generator")
      (description
       "Eschalot is a tor hidden service name generator, it allows one to
produce customized vanity .onion addresses using a brute-force method.  Searches
for valid names can be run with regular expressions and wordlists.  For the
generation of wordlists the included tool @code{worgen} can be used.  There is
no man page, refer to the home page for usage details.")
      (license (list license:isc license:expat)))))

(define-public tomb
  (package
    (name "tomb")
    (version "2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.dyne.org/tomb/"
                                  "Tomb-" version ".tar.gz"))
              (sha256
               (base32
                "1hv1w79as7swqj0n137vz8n8mwvcgwlvd91sdyssz41jarg7f1vr"))))
    (build-system gnu-build-system)
    (native-inputs `(("sudo" ,sudo)))   ;presence needed for 'check' phase
    (inputs
     `(("zsh" ,zsh)
       ("gnupg" ,gnupg)
       ("cryptsetup" ,cryptsetup)
       ("e2fsprogs" ,e2fsprogs)         ;for mkfs.ext4
       ("gettext" ,gettext-minimal)     ;used at runtime
       ("mlocate" ,mlocate)
       ("pinentry" ,pinentry)
       ("qrencode" ,qrencode)
       ("steghide" ,steghide)))
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       ;; TODO: Build and install gtk and qt trays
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)   ;no configuration to be done
         (add-after 'install 'i18n
           (lambda* (#:key make-flags #:allow-other-keys)
             (zero? (apply system*
                           "make" "-C" "extras/translations"
                           "install" make-flags))))
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/tomb")
                 `("PATH" ":" prefix
                   (,(string-append (assoc-ref inputs "mlocate") "/bin")
                    ,@(map (lambda (program)
                             (or (and=> (which program) dirname)
                                 (error "program not found:" program)))
                           '("seq" "mkfs.ext4" "pinentry" "sudo"
                             "gpg" "cryptsetup" "gettext"
                             "qrencode" "steghide")))))
               #t)))
         (delete 'check)
         (add-after 'wrap 'check
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Running the full tests requires sudo/root access for
             ;; cryptsetup, which is not available in the build environment.
             ;; But we can run `tomb dig` without root, so make sure that
             ;; works.  TODO: It Would Be Nice to check the expected "index",
             ;; "search", "bury", and "exhume" features are available by
             ;; querying `tomb -h`.
             (let ((tomb (string-append (assoc-ref outputs "out")
                                        "/bin/tomb")))
               (zero? (system* tomb "dig" "-s" "10" "secrets.tomb"))))))))
    (home-page "http://www.dyne.org/software/tomb")
    (synopsis "File encryption for secret data")
    (description
     "Tomb is an application to manage the creation and access of encrypted
storage files: it can be operated from commandline and it can integrate with a
user's graphical desktop.")
    (license license:gpl3+)))

(define-public scrypt
  (package
    (name "scrypt")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://www.tarsnap.com/scrypt/scrypt-"
                            version ".tgz"))
        (sha256
         (base32
          "0xy5yhrwwv13skv9im9vm76rybh9f29j2dh4hlh2x01gvbkza8a6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
        (add-after 'unpack 'patch-command-invocations
          (lambda _
            (substitute* "Makefile.in"
              (("command -p") ""))
            #t))
        (add-after 'install 'install-docs
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref %outputs "out"))
                   (misc (string-append out "/share/doc/scrypt")))
              (install-file "FORMAT" misc)
              #t))))))
    (inputs
     `(("openssl" ,openssl)))
    (home-page "https://www.tarsnap.com/scrypt.html")
    (synopsis "Memory-hard encryption tool based on scrypt")
    (description "This packages provides a simple password-based encryption
utility as a demonstration of the @code{scrypt} key derivation function.
@code{Scrypt} is designed to be far more resistant against hardware brute-force
attacks than alternative functions such as @code{PBKDF2} or @code{bcrypt}.")
    (license license:bsd-2)))

(define-public perl-math-random-isaac-xs
  (package
    (name "perl-math-random-isaac-xs")
    (version "1.004")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/J/JA/JAWNSY/"
                           "Math-Random-ISAAC-XS-" version ".tar.gz"))
       (sha256
        (base32
         "0yxqqcqvj51fn7b7j5xqhz65v74arzgainn66c6k7inijbmr1xws"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-nowarnings" ,perl-test-nowarnings)))
    (home-page "http://search.cpan.org/dist/Math-Random-ISAAC-XS")
    (synopsis "C implementation of the ISAAC PRNG algorithm")
    (description "ISAAC (Indirection, Shift, Accumulate, Add, and Count) is a
fast pseudo-random number generator.  It is suitable for applications where a
significant amount of random data needs to be produced quickly, such as
solving using the Monte Carlo method or for games.  The results are uniformly
distributed, unbiased, and unpredictable unless you know the seed.

This package implements the same interface as @code{Math::Random::ISAAC}.")
    (license license:public-domain)))

(define-public perl-math-random-isaac
  (package
    (name "perl-math-random-isaac")
    (version "1.004")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/J/JA/JAWNSY/"
                           "Math-Random-ISAAC-" version ".tar.gz"))
       (sha256
        (base32
         "0z1b3xbb3xz71h25fg6jgsccra7migq7s0vawx2rfzi0pwpz0wr7"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-nowarnings" ,perl-test-nowarnings)))
    (propagated-inputs
     `(("perl-math-random-isaac-xs" ,perl-math-random-isaac-xs)))
    (home-page "http://search.cpan.org/dist/Math-Random-ISAAC")
    (synopsis "Perl interface to the ISAAC PRNG algorithm")
    (description "ISAAC (Indirection, Shift, Accumulate, Add, and Count) is a
fast pseudo-random number generator.  It is suitable for applications where a
significant amount of random data needs to be produced quickly, such as
solving using the Monte Carlo method or for games.  The results are uniformly
distributed, unbiased, and unpredictable unless you know the seed.

This package provides a Perl interface to the ISAAC pseudo random number
generator.")
    (license license:public-domain)))

(define-public perl-crypt-random-source
  (package
    (name "perl-crypt-random-source")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Crypt-Random-Source-" version ".tar.gz"))
       (sha256
        (base32
         "00mw5m52sbz9nqp3f6axyrgcrihqxn7k8gv0vi1kvm1j1nc9g29h"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build-tiny" ,perl-module-build-tiny)
       ("perl-test-exception" ,perl-test-exception)))
    (propagated-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)
       ("perl-module-find" ,perl-module-find)
       ("perl-module-runtime" ,perl-module-runtime)
       ("perl-moo" ,perl-moo)
       ("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-sub-exporter" ,perl-sub-exporter)
       ("perl-type-tiny" ,perl-type-tiny)))
    (home-page "http://search.cpan.org/dist/Crypt-Random-Source")
    (synopsis "Get weak or strong random data from pluggable sources")
    (description "This module provides implementations for a number of
byte-oriented sources of random data.")
    (license license:perl-license)))

(define-public perl-math-random-secure
  (package
    (name "perl-math-random-secure")
    (version "0.080001")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/F/FR/FREW/"
                           "Math-Random-Secure-" version ".tar.gz"))
       (sha256
        (base32
         "0dgbf4ncll4kmgkyb9fsaxn0vf2smc9dmwqzgh3259zc2zla995z"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-test-leaktrace" ,perl-test-leaktrace)
       ("perl-test-sharedfork" ,perl-test-sharedfork)
       ("perl-test-warn" ,perl-test-warn)))
    (inputs
     `(("perl-crypt-random-source" ,perl-crypt-random-source)
       ("perl-math-random-isaac" ,perl-math-random-isaac)
       ("perl-math-random-isaac-xs" ,perl-math-random-isaac-xs)
       ("perl-moo" ,perl-moo)))
    (home-page "http://search.cpan.org/dist/Math-Random-Secure")
    (synopsis "Cryptographically secure replacement for rand()")
    (description "This module is intended to provide a
cryptographically-secure replacement for Perl's built-in @code{rand} function.
\"Crytographically secure\", in this case, means:

@enumerate
@item No matter how many numbers you see generated by the random number
generator, you cannot guess the future numbers, and you cannot guess the seed.
@item There are so many possible seeds that it would take decades, centuries,
or millenia for an attacker to try them all.
@item The seed comes from a source that generates relatively strong random
data on your platform, so the seed itself will be as random as possible.
@end enumerate\n")
    (license license:artistic2.0)))

(define-public crypto++
  (package
    (name "crypto++")
    (version "5.6.5")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://cryptopp.com/cryptopp"
                                  (string-join (string-split version #\.) "")
                                  ".zip"))
              (sha256
               (base32
                "0d1cqdz369ivi082k59025wvxzywvkizw7i0pf5h0a1izs3g8pm7"))
              (patches
               (search-patches "crypto++-fix-dos-in-asn.1-decoders.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://cryptopp.com/")
    (synopsis "C++ class library of cryptographic schemes")
    (description "Crypto++ is a C++ class library of cryptographic schemes.")
    ;; The compilation is distributed under the Boost license; the individual
    ;; files in the compilation are in the public domain.
    (license (list license:boost1.0 license:public-domain))))

