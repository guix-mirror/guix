;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox>
;;; Copyright © 2016 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016 ng0 <ng0@we.make.ritual.n0.is>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages perl)
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
  #:use-module (guix build-system gnu))

(define-public libsodium
  (package
    (name "libsodium")
    (version "1.0.10")
    (source (origin
            (method url-fetch)
            (uri (list (string-append
                        "http://download.libsodium.org/libsodium/"
                        "releases/libsodium-" version ".tar.gz")
                       (string-append
                        "https://download.libsodium.org/libsodium/"
                        "releases/old/libsodium-" version ".tar.gz")))
            (sha256
             (base32
              "1gn45g956lyz8l6iq187yc6l627vyivyp8qc5dkr6dnhdnlqddvi"))))
    (build-system gnu-build-system)
    (synopsis "Portable NaCl-based crypto library")
    (description
     "Sodium is a new easy-to-use high-speed software library for network
communication, encryption, decryption, signatures, etc.")
    (license license:isc)
    (home-page "http://libsodium.org")))

(define-public signify
  (package
    (name "signify")
    (version "19")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/aperezdc/signify/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0d2wrss1xl9wm3yzl571cv6h7zdp170v7a45f953bgsy64hkqavh"))))
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
       ("msgpack" ,msgpack)
       ("readline" ,readline)
       ("argon2" ,argon2)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("pkg-config" ,pkg-config)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (arguments
     `(#:configure-flags '("--disable-tools" "--disable-python")
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'autoconf
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
    (version "1.9")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/vgough/encfs/releases/download/v"
                       version "/encfs-" version ".tar.gz"))
       (sha256
        (base32
         "1gzlhq2dlwan3hll414wqinj92lb05wd4j370j190dgcalaxplih"))
       (modules '((guix build utils)))
       ;; Remove bundled dependencies in favour of proper inputs.
       (snippet '(for-each delete-file-recursively
                           (find-files "internal" "^tinyxml2-[0-9]"
                                       #:directories? #t)))))
    (build-system cmake-build-system)
    (native-inputs
     `(("gettext" ,gnu-gettext)

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
    (version "2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.dyne.org/tomb/"
                                  "tomb-" version ".tar.gz"))
              (sha256
               (base32
                "11msj38fdmymiqcmwq1883kjqi5zr01ybdjj58rfjjrw4zw2w5y0"))))
    (build-system gnu-build-system)
    (inputs
     `(("zsh" ,zsh)
       ("sudo" ,sudo)
       ("gnupg" ,gnupg)
       ("cryptsetup" ,cryptsetup)
       ("e2fsprogs" ,e2fsprogs)         ;for mkfs.ext4
       ("gettext" ,gnu-gettext)         ;used at runtime
       ("mlocate" ,mlocate)
       ("pinentry" ,pinentry)
       ("qrencode" ,qrencode)
       ("steghide" ,steghide)
       ("swish-e" ,swish-e)))
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
                             "qrencode" "steghide" "swish-e")))))
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
