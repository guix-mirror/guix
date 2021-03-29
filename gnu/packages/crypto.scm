;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018, 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox>
;;; Copyright © 2016–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2017, 2019, 2020 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2017 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2018, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018, 2020 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2018 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Hendur Saga <hendursaga@yahoo.com>
;;; Copyright © 2020 pukkamustard <pukkamustard@posteo.net>
;;; Copyright © 2021 Ellis Kenyő <me@elken.dev>
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
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages man)
  #:use-module (gnu packages multiprecision)
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
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system perl)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public libdecaf
  (package
    (name "libdecaf")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.code.sf.net/p/ed448goldilocks/code")
                    (commit
                     (string-append "v" version))))
              (file-name
               (git-file-name name version))
              (sha256
               (base32 "1ajgmyvc6a4m1h2hg1g4wz7ibx10x1xys9m6ancnmmf1f2srlfly"))))
    (build-system cmake-build-system)
    (outputs '("out" "python" "doc"))
    (arguments
     `(#:configure-flags '("-DENABLE_STATIC=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-python-binding
           (lambda _
             (substitute* "python/setup.py"
               (("gmake")
                "make")
               (("'\\.\\.', 'build', 'lib', 'libdecaf\\.so'")
                "'..', '..', 'build', 'src', 'libdecaf.so'"))))
         (add-after 'install 'install-python-binding
           (lambda* (#:key outputs #:allow-other-keys)
             (with-directory-excursion "../source/python"
               (invoke "python" "setup.py" "install"
                       (string-append "--prefix=" (assoc-ref outputs "python"))
                       "--root=/"))))
         (add-after 'install-python-binding 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "make" "doc")
             (let* ((doc (assoc-ref outputs "doc"))
                    (dest (string-append doc "/share/doc")))
               (copy-recursively "doc" dest)))))))
    (native-inputs
     `(("dot" ,graphviz)
       ("doxygen" ,doxygen)
       ("python" ,python-wrapper)))
    (synopsis "Decaf Elliptic Curve Library")
    (description "The libdecaf library is an implementation of elliptic curve
cryptography using the Montgomery and Edwards curves Curve25519, Ed25519,
Ed448-Goldilocks and Curve448, using the Decaf encoding.")
    (home-page "http://ed448goldilocks.sourceforge.net/")
    (license (list license:expat        ;library
                   license:bsd-2))))    ;python bindings

(define-public libsodium
  (package
    (name "libsodium")
    (version "1.0.18")
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
              "1h9ncvj23qbbni958knzsli8dvybcswcjbx0qjjgi922nf848l3g"))))
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
    (version "1.0.3")
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
              "0jmga8y94h857ilra3qjaiax3wd5pd6mx1h120zhl9fcjmzhj0js"))))
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
    (version "30")
    (home-page "https://github.com/aperezdc/signify")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/aperezdc/signify/releases"
                                  "/download/v" version "/signify-" version ".tar.xz"))
              (sha256
               (base32
                "11l67j04gyxnlw6zrzsygqs5cgsc1sww1rh0apl05yay131hd17n"))))
    (build-system gnu-build-system)
    ;; TODO Build with libwaive (described in README.md), to implement something
    ;; like OpenBSD's pledge().
    (arguments
     `(#:make-flags
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

(define-public rust-minisign
  (package
    (name "rust-minisign")
    (version "0.5.20")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "minisign" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0xmcvh2snravghaar8igc6b9r3s1snnmf9qam9l3zyhm4987767y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-getrandom" ,rust-getrandom-0.1)
        ("rust-rpassword" ,rust-rpassword-4)
        ("rust-scrypt" ,rust-scrypt-0.3))))
    (home-page "https://github.com/jedisct1/rust-minisign")
    (synopsis "Crate to sign files and verify signatures")
    (description
     "This package provides a crate to sign files and verify signatures.")
    (license license:expat)))

(define-public go-minisign
  (package
    (name "go-minisign")
    (version "0.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jedisct1/go-minisign")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0wc0rk5m60yz52f0cncmbgq67yvb1rcx91gvzjg6jpc4mpw2db27"))
        (modules '((guix build utils)))
        (snippet
         '(begin (delete-file-recursively "vendor") #t))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jedisct1/go-minisign"))
    (propagated-inputs
     `(("go-golang-org-x-crypto" ,go-golang-org-x-crypto)))
    (home-page "https://github.com/jedisct1/go-minisign")
    (synopsis "Minisign verification library for Golang")
    (description "A Golang library to verify Minisign signatures.")
    (license license:expat)))

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
         (add-before 'configure 'patch-CMakeLists.txt
           (lambda _
             ;; Prevent CMake from adding libc on the system include path.
             ;; Otherwise it will interfere with the libc used by GCC and
             ;; ultimately cause #include_next errors.
             (substitute* "CMakeLists.txt"
               (("include_directories \\(SYSTEM \\$\\{Intl_INCLUDE_DIRS\\}\\)")
                ""))
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
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://people.redhat.com/dhowells/keyutils/keyutils-"
                       version ".tar.bz2"))
       (sha256
        (base32 "1kk4pmyflgplkgxn2bzpc069ph9c9jdd9ikcsyd5pnaimqi5gcf8"))
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
       `(("openssl" ,openssl-1.0)))     ; for openssl/{bn,pem,rsa,sha}.h
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list (string-append "CC=" ,(cc-for-target))
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

(define-public ssss
  (package
    (name "ssss")
    (version "0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://point-at-infinity.org/ssss/ssss-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "15grn2fp1x8p92kxkwbmsx8rz16g93y9grl3hfqbh1jn21ama5jx"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No test suite
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configuration to be done
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((outdir (assoc-ref outputs "out"))
                    (bindir (string-append outdir "/bin"))
                    (docdir (string-append outdir
                                           "/share/doc/ssss-"
                                           ,version)))
               (install-file "ssss-combine" bindir)
               (install-file "ssss-split" bindir)
               (install-file "ssss.1" docdir)
               (install-file "ssss.1.html" docdir)
               #t))))))
    (inputs
     `(("gmp" ,gmp)))
    (native-inputs
     `(("xmltoman" ,xmltoman)))
    (home-page "http://point-at-infinity.org/ssss/")
    (synopsis "Shamir's secret sharing scheme implementation")
    (description "@command{ssss-split} and @command{ssss-combine} are utilities that split
and combine secrets securely using Shamir's secret sharing scheme.  This implementation
allows for a threshold scheme where the minimum number of shares can be less than the
total number of shares generated.")
    (license license:gpl2+)))

(define-public tomb
  (package
    (name "tomb")
    (version "2.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.dyne.org/tomb/releases/"
                                  "Tomb-" version ".tar.gz"))
              (sha256
               (base32
                "136nfnpaz29hngwwnzrmc858gpnvnb977gf4ldbpapw1h1k3r8mk"))))
    (build-system gnu-build-system)
    (native-inputs `(("sudo" ,sudo)))   ;presence needed for 'check' phase
    (inputs
     `(("zsh" ,zsh)
       ("gnupg" ,gnupg)
       ("cryptsetup" ,cryptsetup)
       ("e2fsprogs" ,e2fsprogs)         ;for mkfs.ext4
       ("gettext" ,gettext-minimal)     ;used at runtime
       ("lsof" ,lsof)
       ("mlocate" ,mlocate)
       ("pinentry" ,pinentry)
       ("qrencode" ,qrencode)
       ("steghide" ,steghide)
       ("util-linux" ,util-linux)))
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       ;; The "sudo" input is needed only to satisfy dependency checks in the
       ;; 'check' phase.  The "sudo" used at runtime should come from the
       ;; system's setuid-programs, so ensure no reference is kept.
       #:disallowed-references (,sudo)
       ;; TODO: Build and install gtk and qt trays
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ;no configuration to be done
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
                           '("seq" "mkfs.ext4" "pinentry"
                             "gpg" "cryptsetup" "gettext" "lsof"
                             "qrencode" "steghide" "findmnt" "getent")))))
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
    (version "1.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://www.tarsnap.com/scrypt/scrypt-"
                            version ".tgz"))
        (sha256
         (base32
          "1hnl0r6pmyxiy4dmafmqk1db7wpc0x9rqpzqcwr9d2cmghcj6byz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:license-file-regexp "COPYRIGHT"
       #:phases (modify-phases %standard-phases
        (add-after 'unpack 'patch-$PATH-assumptions
          (lambda _
            (substitute* "configure"
              (("\\{POSIX_PATH\\}")
               "{PATH}"))
            (substitute* "Makefile.in"
              (("command -p") ""))
            #t))
        (add-after 'install 'install-docs
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref %outputs "out"))
                   (doc (string-append out "/share/doc/" ,name "-" ,version)))
              (install-file "FORMAT" doc)
              #t))))))
    (inputs
     `(("openssl" ,openssl)))
    (home-page "https://www.tarsnap.com/scrypt.html")
    (synopsis "Memory-hard encryption tool based on scrypt")
    (description "This package provides a simple password-based encryption
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
             (url "https://github.com/technion/libscrypt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1d76ys6cp7fi4ng1w3mz2l0p9dbr7ljbk33dcywyimzjz8bahdng"))))
    (build-system gnu-build-system)
    (outputs (list "out" "static"))
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" %output)
                          "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'install 'install:static
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (install-file "libscrypt.a" lib)
               #t))))))
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
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETHER/"
                           "Crypt-Random-Source-" version ".tar.gz"))
       (sha256
        (base32 "1rpdds3sy5l1fhngnkrsgwsmwd54wpicx3i9ds69blcskwkcwkpc"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build-tiny" ,perl-module-build-tiny)
       ("perl-test-fatal" ,perl-test-fatal)))
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
    (version "8.5.0")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/weidai11/cryptopp")
                (commit
                 (string-append "CRYPTOPP_"
                                (string-replace-substring version "." "_")))))
              (sha256
               (base32
                "0in7rlazq91vfi519g9wr7bh87hii47cimxv7fmj0f88vhjaidq3"))))
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
         (replace 'build
           ;; By default, only the static library is built.
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             (apply invoke "make" "shared"
                    "-j" (number->string (parallel-job-count))
                    make-flags)))
         (add-after 'install 'install-shared-library-links
           ;; By default, only .so and .so.x.y.z are installed.
           ;; Create all the ‘intermediates’ expected by dependent packages.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (prefix "libcryptopp.so.")
                    (target (string-append prefix ,version)))
               (with-directory-excursion lib
                 (symlink target
                          (string-append prefix ,(version-major+minor version)))
                 (symlink target
                          (string-append prefix ,(version-major version)))
                 #t))))
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
                     "Description: Class library of cryptographic schemes\n"
                     "Version: " ,version "\n"
                     "Libs: -L${libdir} -lcryptopp\n"
                     "Cflags: -I${includedir}\n"))
                   #t))))))))
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
    (version "1.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/rhash/rhash/" version
                           "/rhash-" version "-src.tar.gz"))
       (file-name (string-append "rhash-" version ".tar.gz"))
       (sha256
        (base32
         "1xn9fqa6rlnhsbgami45g82dlw9i1skg2sri3ydiinwak5ph1ca2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--prefix=" (assoc-ref %outputs "out"))
             ,@(let ((target (%current-target-system)))
                 (if target
                     `((string-append "--target=" ,target)
                       (string-append "--cc="
                                      (assoc-ref %build-inputs "cross-gcc")
                                      "/bin/" ,target "-gcc"))
                     '())))
       #:make-flags
       ;; The binaries in /bin need some help finding librhash.so.0.
       (list (string-append "LDFLAGS=-Wl,-rpath=" %output "/lib"))
       #:test-target "test"             ; ‘make check’ just checks the sources
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; ./configure is not GNU autotools' and doesn't gracefully handle
           ;; unrecognized options, so we must call it manually.
           (lambda* (#:key configure-flags #:allow-other-keys)
             (apply invoke "./configure" configure-flags)))
         (add-before 'check 'patch-/bin/sh
           (lambda _
             (substitute* "Makefile"
               (("/bin/sh") (which "sh")))
             #t))
         (add-after 'install 'install-library-extras
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke
                    "make" "-C" "librhash"
                    "install-lib-headers" "install-so-link"
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
    (version "2.17.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://botan.randombit.net/releases/"
                                  "Botan-" version ".tar.xz"))
              (sha256
               (base32
                "121vn1aryk36cpks70kk4c4cfic5g0qs82bf92xap9258ijkn4kr"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref %outputs "out"))
                    (lib (string-append out "/lib")))
               ;; Upstream tests and benchmarks with -O3.
               (setenv "CXXFLAGS" "-O3")
               (invoke "python" "./configure.py"
                       (string-append "--prefix=" out)
                       ;; Otherwise, the `botan` executable cannot find
                       ;; libbotan.
                       (string-append "--ldflags=-Wl,-rpath=" lib)

                       "--with-os-feature=getentropy"
                       "--with-rst2man"

                       ;; Recommended by upstream
                       "--with-zlib" "--with-bzip2" "--with-sqlite3"))))
         (add-before 'check 'library-path-for-tests
           (lambda _ (setenv "LD_LIBRARY_PATH" (getcwd))))
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
                       (url "https://github.com/vstakhov/asignify")
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
       `(("openssl" ,openssl)))
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
    (version "3.5")
    (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/skeeto/enchive")
                      (commit version)))
                (sha256
                 (base32
                  "0fdrfc5l42lj2bvmv9dmkmhmm7qiszwk7cmdvnqad3fs7652g0qa"))
                (file-name (git-file-name name version))))
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
  (let ((commit "dbd41db16a0e91b2566820898a3ab2d7dad4fe00"))
    (package
      (name "libsecp256k1")
      (version (git-version "20200615" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/bitcoin-core/secp256k1")
                      (commit commit)))
                (sha256
                 (base32
                  "1fcpnksq5cqwqzshn5f0lq94b73p3frwbp04hgmmbnrndpqg6mpy"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       '(#:configure-flags '("--enable-module-recovery"
                             "--enable-experimental"
                             "--enable-module-ecdh"
                             "--enable-shared")))
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

(define-public libsecp256k1-bitcoin-cash
  (package
    (name "libsecp256k1-bitcoin-cash")
    (version "0.22.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Bitcoin-ABC/secp256k1")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rnif3iny6pz1r3g69bagzr342mm3x0v66b60csnmm1rg44bd5v1"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (arguments
     '(#:configure-flags '("--enable-module-recovery"
                           "--enable-experimental"
                           "--enable-module-ecdh"
                           "--disable-jni"
                           "--with-bignum=no"
                           "--enable-module-schnorr"
                           "--disable-static"
                           "--enable-shared")))
    (synopsis "Optimized C library for EC operations on curve secp256k1")
    (description
     "Optimized C library for cryptographic operations on curve secp256k1.

This library is used for consensus critical cryptographic operations on the
Bitcoin Cash network.

Features:

@itemize
@item secp256k1 ECDSA signing/verification and key generation.
@item secp256k1 Schnorr signing/verification (Bitcoin Cash Schnorr variant).
@item Additive and multiplicative tweaking of secret/public keys.
@item Serialization/parsing of secret keys, public keys, signatures.
@item Constant time, constant memory access signing and pubkey generation.
@item Derandomized ECDSA (via RFC6979 or with a caller provided function).
@item Very efficient implementation.
@item Suitable for embedded systems.
@item Optional module for public key recovery.
@item Optional module for ECDH key exchange (experimental).
@item Optional module for multiset hash (experimental).
@end itemize\n")
    (home-page "https://github.com/Bitcoin-ABC/secp256k1")
    (license license:expat)))

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
              (file-name (git-file-name name version))
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

(define-public minisign
  (package
    (name "minisign")
    (version "0.9")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/jedisct1/minisign/releases/download/"
                       version "/minisign-" version ".tar.gz"))
       (sha256
        (base32 "1h9cfvvm6lqq33b2wdar1x3w4k7zyrscavllyb0l5dmcdabq60r2"))))
    (build-system cmake-build-system)
    (arguments
     ; No test suite
     `(#:tests? #f))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libsodium" ,libsodium)))
    (home-page "https://jedisct1.github.io/minisign")
    (synopsis "Tool to sign files and verify signatures")
    (description
     "Minisign is a dead simple tool to sign files and verify signatures.  It is
portable, lightweight, and uses the highly secure Ed25519 public-key signature
system.  Signature written by minisign can be verified using OpenBSD's
signify tool: public key files and signature files are compatible.  However,
minisign uses a slightly different format to store secret keys.  Minisign
signatures include trusted comments in addition to untrusted comments.
Trusted comments are signed, thus verified, before being displayed.")
    (license license:isc)))

(define-public libolm
  (package
    (name "libolm")
    (version "3.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.matrix.org/git/olm")
                    (commit version)))
              (sha256
               (base32
                "0qji25wiwmkxyfpraxj96c54hyayqmjkvwh0gsy5gb5pz5bp4mcy"))
              (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (with-directory-excursion "tests"
               (invoke "ctest" ".")))))))
    (synopsis "Implementation of the olm and megolm cryptographic ratchets")
    (description "The libolm library implements the Double Ratchet
cryptographic ratchet.  It is written in C and C++11, and exposed as a C
API.")
    (home-page "https://matrix.org/docs/projects/other/olm/")
    (license license:asl2.0)))

(define-public hash-extender
  (let ((commit "cb8aaee49f93e9c0d2f03eb3cafb429c9eed723d")
        (revision "2"))
    (package
      (name "hash-extender")
      (version (git-version "0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/iagox86/hash_extender")
                      (commit commit)))
                (sha256
                 (base32
                  "1fj118566hr1wv03az2w0iqknazsqqkak0mvlcvwpgr6midjqi9b"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'check
             (lambda _
               (invoke "./hash_extender_test")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((outdir (assoc-ref outputs "out"))
                      (bindir (string-append outdir "/bin"))
                      (docdir (string-append outdir
                                             "/share/doc/hash-extender-"
                                             ,version)))
                 (install-file "hash_extender" bindir)
                 (install-file "README.md" docdir)
                 #t))))))
      (inputs
       `(("openssl" ,openssl)))
      (synopsis "Tool for hash length extension attacks")
      (description "@command{hash_extender} is a utility for performing hash
length extension attacks supporting MD4, MD5, RIPEMD-160, SHA-0, SHA-1,
SHA-256, SHA-512, and WHIRLPOOL hashes.")
      (home-page "https://github.com/iagox86/hash_extender")
      (license license:bsd-3))))

(define-public mkp224o
  (package
    (name "mkp224o")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cathugger/mkp224o")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0b2cn96wg4l8jkkqqp8l2295xlmm2jc8nrw6rdqb5g0zkpfmrxbb"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no test suite
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((outdir (assoc-ref outputs "out"))
                    (bindir (string-append outdir "/bin")))
               (install-file "mkp224o" bindir)
               #t))))))
    (native-inputs
     `(("autoconf" ,autoconf)))
    (inputs
     `(("libsodium" ,libsodium)))
    (synopsis "Tor hidden service v3 name generator")
    (description "@code{mkp224o} generates valid ed25519 (hidden service
version 3) onion addresses.  It allows one to produce customized vanity .onion
addresses using a brute-force method.")
    (home-page "https://github.com/cathugger/mkp224o")
    (license license:cc0)))

(define-public transcrypt
  (package
    (name "transcrypt")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elasticdog/transcrypt")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0bpz1hazbhfb6pqi68x55kq6a31bgh6vwij836slmi4jqiwvnh5a"))
       (file-name (git-file-name name version))))
    (inputs
     `(("git" ,git)
       ("openssl" ,openssl)))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("transcrypt" "bin/transcrypt")
         ("man/transcrypt.1" "share/man/man1/transcrypt.1")
         ("contrib/bash/transcrypt"
          "share/bash-completion/completions/transcrypt")
         ("contrib/zsh/_transcrypt"
          "share/zsh/site-functions/_transcrypt"))))
    (home-page "https://github.com/elasticdog/transcrypt")
    (synopsis "Transparently encrypt files within a git repository")
    (description
     "Transcrypt is a script to configure transparent encryption of sensitive
files stored in a Git repository.  Files that you choose will be automatically
encrypted when you commit them, and automatically decrypted when you check
them out.  The process will degrade gracefully, so even people without your
encryption password can safely commit changes to the repository's
non-encrypted files.")
    (license license:expat)))
