;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018, 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox>
;;; Copyright © 2016, 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2017 ng0 <ng0@n0.is>
;;; Copyright © 2016, 2017, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2018 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
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
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages search)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public libsodium
  (package
    (name "libsodium")
    (version "1.0.17")
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
              "1cf2d9v1gylz1qcy2zappbf526qfmph6gd6fnn3w2b347vixmhqc"))))
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
    (version "1.0.1")
    (source (origin
            (method url-fetch)
            (uri
             (list
              (string-append "https://archive.hadrons.org/software/libmd/libmd-"
                             version ".tar.xz")
              (string-append "https://libbsd.freedesktop.org/releases/libmd-"
                             version ".tar.xz")))
            (sha256
             (base32
              "0waclg2d5qin3r26gy5jvy4584ik60njc8pqbzwk0lzq3j9ynkp1"))))
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
    (version "24")
    (home-page "https://github.com/aperezdc/signify")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0grdlrpxcflzmzzc30r8rvsmamvbsgqyni59flzzk4w5hpjh464w"))))
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
    ;; This package includes third-party code that was originally released under
    ;; various non-copyleft licenses. See the source files for clarification.
    (license (list license:bsd-3 license:bsd-4 license:expat license:isc
                   license:public-domain (license:non-copyleft
                                          "file://base64.c"
                                          "See base64.c in the distribution for
                                           the license from IBM.")))))

(define-public encfs
  (package
    (name "encfs")
    (version "1.9.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/vgough/encfs/releases/download/v"
                       version "/encfs-" version ".tar.gz"))
       (sha256
        (base32
         "0qzxavvv20577bxvly8s7d3y7bqasqclc2mllp0ddfncjm9z02a7"))
       (modules '((guix build utils)))
       ;; Remove bundled dependencies in favour of proper inputs.
       (snippet '(begin
                   (for-each delete-file-recursively
                             '("vendor/github.com/leethomason/tinyxml2"
                               "vendor/github.com/google/googletest"))
                   #t))))
    (build-system cmake-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)

       ;; Test dependencies.
       ("expect" ,expect)
       ("googletest-source" ,(package-source googletest))
       ("perl" ,perl)))
    (inputs
     `(("attr" ,attr)
       ("fuse" ,fuse)
       ("openssl" ,openssl)
       ("tinyxml2" ,tinyxml2)))
    (arguments
     `(#:configure-flags (list "-DUSE_INTERNAL_TINYXML=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-googletest
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "vendor/github.com/google/googletest")
             (copy-recursively (assoc-ref inputs "googletest-source")
                               "vendor/github.com/google/googletest")
             #t))
         (add-before 'check 'make-unittests
           (lambda _
             (invoke "make" "unittests"))))))
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
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://people.redhat.com/dhowells/keyutils/keyutils-"
                       version ".tar.bz2"))
       (sha256
        (base32
         "05bi5ja6f3h3kdi7p9dihlqlfrsmi1wh1r2bdgxc0180xh6g5bnk"))
       (modules '((guix build utils)))
       ;; Create relative symbolic links instead of absolute ones to /lib/*.
       (snippet '(begin
                   (substitute* "Makefile" (("\\$\\(LNS\\) \\$\\(LIBDIR\\)/")
                                            "$(LNS) "))
                   #t))))
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
    (inputs
     `(("mit-krb5" ,mit-krb5)))
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
               (invoke "./worgen" "8-12" "top1000.txt" "3-10" "top400nouns.txt"
                       "3-6" "top150adjectives.txt" "3-6")
               (invoke "./eschalot" "-r" "^guix|^guixsd")
               (invoke "./eschalot" "-r" "^gnu|^free")
               (invoke "./eschalot" "-r" "^cyber|^hack")
               (invoke "./eschalot" "-r" "^troll")))
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
    (version "2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.dyne.org/tomb/"
                                  "Tomb-" version ".tar.gz"))
              (sha256
               (base32
                "12c6qldngaw520gvb02inzkhnxbl4k0dwmddrgnaf7xashy6j0wc"))))
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
       ("steghide" ,steghide)
       ("util-linux" ,util-linux)))
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       ;; TODO: Build and install gtk and qt trays
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)   ;no configuration to be done
         (add-after 'install 'i18n
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "-C" "extras/translations"
                    "install" make-flags)
             #t))
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
                             "qrencode" "steghide" "findmnt")))))
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
               (invoke tomb "dig" "-s" "10" "secrets.tomb")
               #t))))))
    (home-page "https://www.dyne.org/software/tomb")
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

(define-public libscrypt
  (package
    (name "libscrypt")
    (version "1.21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/technion/libscrypt.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1d76ys6cp7fi4ng1w3mz2l0p9dbr7ljbk33dcywyimzjz8bahdng"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" %output)
                          "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://lolware.net/libscrypt.html")
    (synopsis "Password hashing library")
    (description "@code{libscrypt} implements @code{scrypt} key derivation
function.  It is designed to be far more secure against hardware brute-force
attacks than alternative functions such as @code{PBKDF2} or @code{bcrypt}.")
    (license license:bsd-3)))

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
    (home-page "https://metacpan.org/release/Math-Random-ISAAC-XS")
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
    (home-page "https://metacpan.org/release/Math-Random-ISAAC")
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
    (home-page "https://metacpan.org/release/Crypt-Random-Source")
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
    (home-page "https://metacpan.org/release/Math-Random-Secure")
    (synopsis "Cryptographically secure replacement for rand()")
    (description "This module is intended to provide a
cryptographically-secure replacement for Perl's built-in @code{rand} function.
\"Crytographically secure\", in this case, means:

@enumerate
@item No matter how many numbers you see generated by the random number
generator, you cannot guess the future numbers, and you cannot guess the seed.
@item There are so many possible seeds that it would take decades, centuries,
or millennia for an attacker to try them all.
@item The seed comes from a source that generates relatively strong random
data on your platform, so the seed itself will be as random as possible.
@end enumerate\n")
    (license license:artistic2.0)))

(define-public crypto++
  (package
    (name "crypto++")
    (version "8.0.0")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://cryptopp.com/cryptopp"
                                  (string-join (string-split version #\.) "")
                                  ".zip"))
              (sha256
               (base32
                "0b5qrsm4jhy4nzxgrm13nixhvbswr242plx1jw6r4sw492rqkzdv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             ;; Override "/sbin/ldconfig" with simply "echo" since
             ;; we don't need ldconfig(8).
             "LDCONF=echo")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-native-optimisation
           ;; This package installs more than just headers.  Ensure that the
           ;; cryptest.exe binary & static library aren't CPU model specific.
           (lambda _
             (substitute* "GNUmakefile"
               ((" -march=native") ""))
             #t))
         (delete 'configure)
         (add-after 'build 'build-shared
           (lambda _
             ;; By default, only the static library is built.
             (invoke "make" "shared")))
         (add-after 'install 'install-pkg-config
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (pkg-dir (string-append out "/lib/pkgconfig")))
               (mkdir-p pkg-dir)
               (with-output-to-file (string-append pkg-dir "/libcrypto++.pc")
                 (lambda _
                   (display
                    (string-append
                     "prefix=" out "\n"
                     "libdir=" out "/lib\n"
                     "includedir=" out "/include\n\n"
                     "Name: libcrypto++-" ,version "\n"
                     "Description: Class library of cryptographic schemes"
                     "Version: " ,version "\n"
                     "Libs: -L${libdir} -lcryptopp\n"
                     "Cflags: -I${includedir}\n"))))))))))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://cryptopp.com/")
    (synopsis "C++ class library of cryptographic schemes")
    (description "Crypto++ is a C++ class library of cryptographic schemes.")
    ;; The compilation is distributed under the Boost license; the individual
    ;; files in the compilation are in the public domain.
    (license (list license:boost1.0 license:public-domain))))

(define-public libb2
  (package
    (name "libb2")
    (version "0.98.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/BLAKE2/libb2/releases/download/v"
                    version "/libb2-" version ".tar.gz"))
              (sha256
               (base32
                "0bn7yrzdixdvzm46shbhpkqbr6zyqyxiqn7a7x54ag3mrvfnyqjk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        ,@(if (any (cute string-prefix? <> (or (%current-system)
                                               (%current-target-system)))
                   '("x86_64" "i686"))
              ;; fat only checks for Intel optimisations
              '("--enable-fat")
              '())
        "--disable-native")))           ;don't optimise at build time
    (home-page "https://blake2.net/")
    (synopsis "Library implementing the BLAKE2 family of hash functions")
    (description
     "libb2 is a portable implementation of the BLAKE2 family of cryptographic
hash functions.  It includes optimised implementations for IA-32 and AMD64
processors, and an interface layer that automatically selects the best
implementation for the processor it is run on.

@dfn{BLAKE2} (RFC 7693) is a family of high-speed cryptographic hash functions
that are faster than MD5, SHA-1, SHA-2, and SHA-3, yet are at least as secure
as the latest standard, SHA-3.  It is an improved version of the SHA-3 finalist
BLAKE.")
    (license license:public-domain)))

(define-public rhash
  (package
    (name "rhash")
    (version "1.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/rhash/RHash/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "14ngzfgmd1lfp7m78sn49x8ymf2s37nrr67c6p5vas85nrrgjkcn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       ;; The binaries in /bin need some help finding librhash.so.0.
       (list (string-append "LDFLAGS=-Wl,-rpath=" %output "/lib"))
       #:test-target "test"             ; ‘make check’ just checks the sources
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; ./configure is not GNU autotools' and doesn't gracefully handle
           ;; unrecognized options, so we must call it manually.
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "./configure"
                     (string-append "--prefix=" (assoc-ref outputs "out")))))
         (add-after 'install 'install-library-extras
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke
                    "make" "-C" "librhash"
                    "install-headers" "install-so-link"
                    make-flags))))))
    (home-page "https://sourceforge.net/projects/rhash/")
    (synopsis "Utility for computing hash sums")
    (description "RHash is a console utility for calculation and verification
of magnet links and a wide range of hash sums like CRC32, MD4, MD5, SHA1,
SHA256, SHA512, SHA3, AICH, ED2K, Tiger, DC++ TTH, BitTorrent BTIH, GOST R
34.11-94, RIPEMD-160, HAS-160, EDON-R, Whirlpool and Snefru.")
    (license (license:non-copyleft "file://COPYING"))))

(define-public botan
  (package
    (name "botan")
    (version "2.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://botan.randombit.net/releases/"
                                  "Botan-" version ".tgz"))
              (sha256
               (base32
                "142aqabwc266jxn8wrp0f1ffrmcvdxwvyh8frb38hx9iaqazjbg4"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref %outputs "out"))
                    (lib (string-append out "/lib")))
               (invoke "python" "./configure.py"
                       (string-append "--prefix=" out)
                       ;; Otherwise, the `botan` executable cannot find
                       ;; libbotan.
                       (string-append "--ldflags=-Wl,-rpath=" lib)
                       "--with-rst2man"
                       ;; Recommended by upstream
                       "--with-zlib" "--with-bzip2" "--with-sqlite3"))))
         (replace 'check
           (lambda _ (invoke "./botan-test"))))))
    (native-inputs
     `(("python" ,python-wrapper)
       ("python-docutils" ,python-docutils)))
    (inputs
     `(("sqlite" ,sqlite)
       ("bzip2" ,bzip2)
       ("zlib" ,zlib)))
    (synopsis "Cryptographic library in C++11")
    (description "Botan is a cryptography library, written in C++11, offering
the tools necessary to implement a range of practical systems, such as TLS/DTLS,
PKIX certificate handling, PKCS#11 and TPM hardware support, password hashing,
and post-quantum crypto schemes.  In addition to the C++, botan has a C89 API
specifically designed to be easy to call from other languages.  A Python binding
using ctypes is included, and several other language bindings are available.")
    (home-page "https://botan.randombit.net")
    (license license:bsd-2)))

(define-public ccrypt
  (package
    (name "ccrypt")
    (version "1.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/ccrypt/"
                                  version "/ccrypt-" version ".tar.gz"))
              (sha256
               (base32
                "0kx4a5mhmp73ljknl2lcccmw9z3f5y8lqw0ghaymzvln1984g75i"))))
    (build-system gnu-build-system)
    (home-page "http://ccrypt.sourceforge.net")
    (synopsis "Command-line utility for encrypting and decrypting files and streams")
    (description "@command{ccrypt} is a utility for encrypting and decrypting
files and streams.  It was designed as a replacement for the standard unix
@command{crypt} utility, which is notorious for using a very weak encryption
algorithm.  @command{ccrypt} is based on the Rijndael block cipher, a version of
which is also used in the Advanced Encryption Standard (AES, see
@url{http://www.nist.gov/aes}).  This cipher is believed to provide very strong
security.")
    (license license:gpl2)))

(define-public asignify
  (let ((commit "f58e7977a599f040797975d649ed318e25cbd2d5")
        (revision "0"))
    (package
      (name "asignify")
      (version (git-version "1.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/vstakhov/asignify.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1zl68qq6js6fdahxzyhvhrpyrwlv8c2zhdplycnfxyr1ckkhq8dw"))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags
         (list "--enable-openssl"
               (string-append "--with-openssl="
                              (assoc-ref %build-inputs "openssl")))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)))
      (inputs
       `(("openssl" ,openssl-next)))
      (home-page "https://github.com/vstakhov/asignify")
      (synopsis "Cryptographic authentication and encryption tool and library")
      (description "Asignify offers public cryptographic signatures and
encryption with a library or a command-line tool.  The tool is heavily inspired
by signify as used in OpenBSD.  The main goal of this project is to define a
high level API for signing files, validating signatures and encrypting using
public-key cryptography.  Asignify is designed to be portable and self-contained
with zero external dependencies.  Asignify can verify OpenBSD signatures, but it
cannot sign messages in OpenBSD format yet.")
      (license license:bsd-2))))

(define-public enchive
  (package
    (name "enchive")
    (version "3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/skeeto/" name "/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "17hrxpp4cpn10bk48sfvfjc8hghky34agsnypam1v9f36kbalqfk"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no check target         '
       #:make-flags (list "CC=gcc" "PREFIX=$(out)")
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'install 'post-install
                    (lambda _
                      (let* ((out (assoc-ref %outputs "out"))
                             (lisp (string-append out "/share/emacs/site-lisp")))
                        (install-file "enchive-mode.el" lisp)
                        #t))))))
    (synopsis "Encrypted personal archives")
    (description
     "Enchive is a tool to encrypt files to yourself for long-term
archival.  It's a focused, simple alternative to more complex solutions such as
GnuPG or encrypted filesystems.  Enchive has no external dependencies and is
trivial to build for local use.  Portability is emphasized over performance.")
    (home-page "https://github.com/skeeto/enchive")
    (license license:unlicense)))

(define-public libsecp256k1
  (let ((commit "e34ceb333b1c0e6f4115ecbb80c632ac1042fa49"))
    (package
      (name "libsecp256k1")
      (version (git-version "20181126" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/bitcoin-core/secp256k1")
                      (commit commit)))
                (sha256
                 (base32
                  "0as78s179hcr3ysk3fw98k5wzabgnwri7vkkc17wg31lyz6ids6c"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)))
      ;; WARNING: This package might need additional configure flags to run properly.
      ;; See https://git.archlinux.org/svntogit/community.git/tree/trunk/PKGBUILD?h=packages/libsecp256k1.
      (synopsis "C library for EC operations on curve secp256k1")
      (description
       "Optimized C library for EC operations on curve secp256k1.

This library is a work in progress and is being used to research best
practices.  Use at your own risk.

Features:

@itemize
@item secp256k1 ECDSA signing/verification and key generation.
@item Adding/multiplying private/public keys.
@item Serialization/parsing of private keys, public keys, signatures.
@item Constant time, constant memory access signing and pubkey generation.
@item Derandomized DSA (via RFC6979 or with a caller provided function.)
@item Very efficient implementation.
@end itemize\n")
      (home-page "https://github.com/bitcoin-core/secp256k1")
      (license license:unlicense))))

(define-public stoken
  (package
    (name "stoken")
    (version "0.92")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/stoken/"
                                  "stoken-" version ".tar.gz"))
              (sha256
               (base32
                "0npgr6y85gzwksy8jkwa4yzvqwjprwnplx3yiw3ayk4f0ldlhaxa"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("nettle" ,nettle)
       ("libxml2" ,libxml2)))
    (home-page "http://stoken.sf.net")
    (synopsis "Software Token for cryptographic authentication")
    (description
     "@code{stoken} is a token code generator compatible with RSA SecurID
128-bit (AES) tokens.  This package contains a standalone command-line program
that allows for importing token seeds, generating token codes, and various
utility/testing functions.")
    (license license:lgpl2.1+)))

(define-public hpenc
  (package
    (name "hpenc")
    (version "3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/vstakhov/hpenc")
                     (commit version)))
              (sha256
               (base32
                "1fb5yi3d2k8kd4zm7liiqagpz610y168xrr1cvn7cbq314jm2my1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No test suite
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             ;; Build the program and the docs.
             "SUBDIRS=src doc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; No ./configure script
         (add-after 'unpack 'patch-path
           (lambda _
             (substitute* '("src/Makefile" "doc/Makefile")
               (("/usr/bin/install")
                "install"))))
         (add-before 'install 'make-output-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man1 (string-append out "/share/man/man1")))
               (mkdir-p bin)
               (mkdir-p man1)
               #t))))))
    (inputs
     `(("libsodium" ,libsodium)
       ("openssl" ,openssl)))
    (synopsis "High-performance command-line tool for stream encryption")
    (description "Hpenc is a command-line tool for performing authenticated
encryption (AES-GCM and ChaCha20-Poly1305) of streaming data.  It does not
perform an asymmetric key exchange, instead requiring the user to distribute
pre-shared keys out of band.  It is designed to handle large amounts of data
quickly by using all your CPU cores and hardware acceleration.")
    (home-page "https://github.com/vstakhov/hpenc")
    (license license:bsd-3)))
