;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2015, 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Alex Vong <alexvong1995@gmail.com>
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

(define-module (gnu packages backup)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages ftp)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mcrypt)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml))

(define-public duplicity
  (package
    (name "duplicity")
    (version "0.7.18.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://code.launchpad.net/duplicity/"
                          (version-major+minor version)
                          "-series/" version "/+download/duplicity-"
                          version ".tar.gz"))
      (sha256
       (base32
        "17c0203y5qz9w8iyhs26l44qf6a1vp26b5ykz1ypdr2kv6g02df9"))))
    (build-system python-build-system)
    (native-inputs
     `(("util-linux" ,util-linux)       ; setsid command, for the tests
       ("par2cmdline" ,par2cmdline)
       ("python-pexpect" ,python2-pexpect)
       ("python-fasteners" ,python2-fasteners)
       ("mock" ,python2-mock)))
    (propagated-inputs
     `(("lockfile" ,python2-lockfile)
       ("urllib3" ,python2-urllib3)))
    (inputs
     `(("librsync" ,librsync-0.9)
       ("lftp" ,lftp)
       ("gnupg" ,gnupg)                 ; gpg executable needed
       ("util-linux" ,util-linux)       ; for setsid
       ("tzdata" ,tzdata)))
    (arguments
     `(#:python ,python-2               ; setup assumes Python 2
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'use-store-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "duplicity/gpginterface.py"
               (("self.call = 'gpg'")
                (string-append "self.call = '" (assoc-ref inputs "gnupg") "/bin/gpg'")))

             (substitute* '("testing/functional/__init__.py"
                            "testing/overrides/bin/lftp")
               (("/bin/sh") (which "sh")))
             #t))
         (add-before 'check 'check-setup
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "HOME" (getcwd))   ; gpg needs to write to $HOME
             (setenv "TZDIR"            ; some timestamp checks need TZDIR
                     (string-append (assoc-ref inputs "tzdata")
                                    "/share/zoneinfo"))
             #t)))))
    (home-page "http://duplicity.nongnu.org/index.html")
    (synopsis "Encrypted backup using rsync algorithm")
    (description
     "Duplicity backs up directories by producing encrypted tar-format volumes
and uploading them to a remote or local file server.  Because duplicity uses
librsync, the incremental archives are space efficient and only record the
parts of files that have changed since the last backup.  Because duplicity
uses GnuPG to encrypt and/or sign these archives, they will be safe from
spying and/or modification by the server.")
    (license license:gpl2+)))

(define-public par2cmdline
  (package
    (name "par2cmdline")
    (version "0.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Parchive/par2cmdline.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0f1jsd5sw2wynjzi7yjqjaf13yhyjfdid91p8yh0jn32y03kjyrz"))))
    (native-inputs
     `(("automake" ,automake)
       ("autoconf" ,autoconf)))
    (build-system gnu-build-system)
    (synopsis "File verification and repair tools")
    (description "Par2cmdline uses Reed-Solomon error-correcting codes to
generate and verify PAR2 recovery files.  These files can be distributed
alongside the source files or stored together with back-ups to protect against
transmission errors or @dfn{bit rot}, the degradation of storage media over
time.
Unlike a simple checksum, PAR2 doesn't merely detect errors: as long as the
damage isn't too extensive (and smaller than the size of the recovery file), it
can even repair them.")
    (home-page "https://github.com/Parchive/par2cmdline")
    (license license:gpl3+)))

(define-public hdup
  (package
    (name "hdup")
    (version "2.0.14")
    (source
     (origin
      (method url-fetch)
      (uri "https://fossies.org/linux/privat/old/hdup-2.0.14.tar.bz2")
      (sha256
       (base32
        "02bnczg01cyhajmm4rhbnc0ja0dd9ikv9fwv28asxh1rlx9yr0b7"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("tar" ,tar)
       ("lzop" ,lzop)
       ("mcrypt" ,mcrypt)
       ("openssh" ,openssh)
       ("gnupg" ,gnupg-1)))
    (arguments
     `(#:configure-flags
       `(,(string-append "--sbindir=" (assoc-ref %outputs "out") "/bin"))
       #:tests? #f))
    (home-page "http://archive.miek.nl/projects/hdup/index.html")
    (synopsis "Simple incremental backup tool")
    (description
     "Hdup2 is a backup utility, its aim is to make backup really simple.  The
backup scheduling is done by means of a cron job.  It supports an
include/exclude mechanism, remote backups, encrypted backups and split
backups (called chunks) to allow easy burning to CD/DVD.")
    (license license:gpl2)))

(define-public libarchive
  (package
    (name "libarchive")
    (version "3.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://libarchive.org/downloads/libarchive-"
                           version ".tar.gz"))
       (patches (search-patches "libarchive-CVE-2018-1000877.patch"
                                "libarchive-CVE-2018-1000878.patch"
                                "libarchive-CVE-2018-1000880.patch"))
       (sha256
        (base32
         "0bhfncid058p7n1n8v29l6wxm3mhdqfassscihbsxfwz3iwb2zms"))))
    (build-system gnu-build-system)
    (inputs
     `(("zlib" ,zlib)
       ("nettle" ,nettle)
       ("lzo" ,lzo)
       ("bzip2" ,bzip2)
       ("libxml2" ,libxml2)
       ("xz" ,xz)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-pwd
           (lambda _
             (substitute* "Makefile"
               (("/bin/pwd") (which "pwd")))
             #t))
         (replace 'check
           (lambda _
             ;; XXX: The test_owner_parse, test_read_disk, and
             ;; test_write_disk_lookup tests expect user 'root' to exist, but
             ;; the chroot's /etc/passwd doesn't have it.  Turn off those tests.
             ;;
             ;; The tests allow one to disable tests matching a globbing pattern.
             (invoke "make" "libarchive_test" "bsdcpio_test" "bsdtar_test")
             ;; XXX: This glob disables too much.
             (invoke "./libarchive_test" "^test_*_disk*")
             (invoke "./bsdcpio_test" "^test_owner_parse")
             (invoke "./bsdtar_test")))
         (add-after 'install 'add--L-in-libarchive-pc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (lib     (string-append out "/lib"))
                    (nettle  (assoc-ref inputs "nettle"))
                    (libxml2 (assoc-ref inputs "libxml2"))
                    (xz      (assoc-ref inputs "xz"))
                    (zlib    (assoc-ref inputs "zlib"))
                    (bzip2   (assoc-ref inputs "bzip2")))
               (substitute* (string-append lib "/pkgconfig/libarchive.pc")
                 (("-lnettle")
                  (string-append "-L" nettle "/lib -lnettle"))
                 (("-lxml2")
                  (string-append "-L" libxml2 "/lib -lxml2"))
                 (("-llzma")
                  (string-append "-L" xz "/lib -llzma"))
                 (("-lz")
                  (string-append "-L" zlib "/lib -lz"))
                 (("-lbz2")
                  (string-append "-L" bzip2 "/lib -lbz2")))
               #t))))

       ;; libarchive/test/test_write_format_gnutar_filenames.c needs to be
       ;; compiled with C99 or C11 or a gnu variant.
       #:configure-flags '("CFLAGS=-O2 -g -std=c99")))
    (home-page "https://libarchive.org/")
    (synopsis "Multi-format archive and compression library")
    (description
     "Libarchive provides a flexible interface for reading and writing
archives in various formats such as tar and cpio.  Libarchive also supports
reading and writing archives compressed using various compression filters such
as gzip and bzip2.  The library is inherently stream-oriented; readers
serially iterate through the archive, writers serially add things to the
archive.  In particular, note that there is currently no built-in support for
random access nor for in-place modification.")
    (license license:bsd-2)))

(define-public rdup
  (package
    (name "rdup")
    (version "1.1.15")
    (source
     (origin
       (method url-fetch)
       (file-name (string-append name "-" version ".tar.gz"))
       (uri (string-append "https://github.com/miekg/rdup/archive/"
                           version ".tar.gz"))
       (sha256
        (base32
         "1jr91hgcf0rrpanqlwws72ql9db6d6grs2i122ki1s4bx0vqqyvq"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)

       ;; For tests.
       ("dejagnu" ,dejagnu)))
    (inputs
     `(("glib" ,glib)
       ("pcre" ,pcre)
       ("libarchive" ,libarchive)
       ("mcrypt" ,mcrypt)
       ("nettle" ,nettle)))
    (arguments
     `(#:parallel-build? #f             ;race conditions
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'qualify-inputs
           (lambda* (#:key inputs #:allow-other-keys)
             ;; This script is full of pitfalls.  Fix some that particularly
             ;; affect Guix users & leave the rest as reader excercises.
             (substitute* "rdup-simple"
               ;; Use the input ‘mcrypt’, not whatever's in $PATH at run time.
               (("([' ])mcrypt " all delimiter)
                (string-append delimiter (which "mcrypt") " "))
               ;; Avoid frivolous dependency on ‘which’ with a shell builtin.
               (("which") "command -v"))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             (setenv "HOME" (getcwd))
             (substitute* "testsuite/rdup/rdup.rdup-up-t-with-file.exp"
               (("/bin/cat") (which "cat")))
             #t)))))
    (home-page "https://github.com/miekg/rdup")
    (synopsis "Provide a list of files to backup")
    (description
     "Rdup is a utility inspired by rsync and the plan9 way of doing backups.
Rdup itself does not backup anything, it only print a list of absolute
file names to standard output.  Auxiliary scripts are needed that act on this
list and implement the backup strategy.")
    (license license:gpl3+)))

(define-public btar
  (package
    (name "btar")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://vicerveza.homeunix.net/~viric/soft/btar/"
                           "btar-" version ".tar.gz"))
       (sha256
        (base32
         "0miklk4bqblpyzh1bni4x6lqn88fa8fjn15x1k1n8bxkx60nlymd"))))
    (build-system gnu-build-system)
    (inputs
     `(("librsync" ,librsync-0.9)))
    (arguments
     `(#:make-flags `(,(string-append "PREFIX=" (assoc-ref %outputs "out"))
                      "CC=gcc")
       #:tests? #f                      ;test input not distributed
       #:phases
       ;; no configure phase
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "http://viric.name/cgi-bin/btar/doc/trunk/doc/home.wiki")
    (synopsis "Tar-compatible archiver")
    (description
     "Btar is a tar-compatible archiver which allows arbitrary compression and
ciphering, redundancy, differential backup, indexed extraction, multicore
compression, input and output serialisation, and tolerance to partial archive
errors.")
    (license license:gpl3+)))

(define-public rdiff-backup
  (package
    (name "rdiff-backup")
    (version "1.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/rdiff-backup/rdiff-backup-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1nwmmh816f96h0ff1jxk95ad38ilbhbdl5dgibx1d4cl81dsi48d"))))
    (build-system python-build-system)
    (inputs
     `(("python" ,python-2)
       ("librsync" ,librsync-0.9)))
    (arguments
     `(#:python ,python-2
       #:tests? #f))
    (home-page "https://www.nongnu.org/rdiff-backup/")
    (synopsis "Local/remote mirroring+incremental backup")
    (description
     "Rdiff-backup backs up one directory to another, possibly over a network.
The target directory ends up a copy of the source directory, but extra reverse
diffs are stored in a special subdirectory of that target directory, so you
can still recover files lost some time ago.  The idea is to combine the best
features of a mirror and an incremental backup.  Rdiff-backup also preserves
subdirectories, hard links, dev files, permissions, uid/gid ownership,
modification times, extended attributes, acls, and resource forks.  Also,
rdiff-backup can operate in a bandwidth efficient manner over a pipe, like
rsync.  Thus you can use rdiff-backup and ssh to securely back a hard drive up
to a remote location, and only the differences will be transmitted.  Finally,
rdiff-backup is easy to use and settings have sensible defaults.")
    (license license:gpl2+)))

(define-public rsnapshot
  (package
    (name "rsnapshot")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/rsnapshot/rsnapshot/releases/download/"
             version "/rsnapshot-" version ".tar.gz"))
       (sha256
        (base32
         "05jfy99a0xs6lvsjfp3wz21z0myqhmwl2grn3jr9clijbg282ah4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (substitute* '("t/cmd-post_pre-exec/conf/pre-true-post-true.conf"
                            "t/backup_exec/conf/backup_exec_fail.conf"
                            "t/backup_exec/conf/backup_exec.conf")
               (("/bin/true") (which "true"))
               (("/bin/false") (which "false")))
             (invoke "make" "test"))))))
    (inputs
     `(("perl" ,perl)
       ("rsync" ,rsync)))
    (home-page "http://rsnapshot.org")
    (synopsis "Deduplicating snapshot backup utility based on rsync")
    (description "rsnapshot is a file system snapshot utility based on rsync.
rsnapshot makes it easy to make periodic snapshots of local machines, and
remote machines over SSH.  To reduce the disk space required for each backup,
rsnapshot uses hard links to deduplicate identical files.")
    (license license:gpl2+)))

(define-public libchop
  (package
    (name "libchop")
    (version "0.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/libchop/libchop-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0fpdyxww41ba52d98blvnf543xvirq1v9xz1i3x1gm9lzlzpmc2g"))
              (patches (search-patches "diffutils-gets-undeclared.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'adjust-configure-script
                    (lambda _
                      ;; Mimic upstream commit
                      ;; 25750ab5ef82fd3cfce5205d5f1ef07b47098091.
                      (substitute* "configure"
                        (("GUILE=(.*)--variable bindir`" _ middle)
                         (string-append "GUILE=" middle
                                        "--variable bindir`/guile")))
                      #t))
                  (add-before 'check 'skip-test
                    (lambda _
                      ;; XXX: This test fails (1) because current GnuTLS no
                      ;; longer supports OpenPGP authentication, and (2) for
                      ;; some obscure reason.  Better skip it.
                      (setenv "XFAIL_TESTS" "utils/block-server")
                      #t)))))
    (native-inputs
     `(("guile" ,guile-2.0)
       ("gperf" ,gperf-3.0)                  ;see <https://bugs.gnu.org/32382>
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-2.0)
       ("util-linux" ,util-linux)
       ("gnutls" ,gnutls)
       ("tdb" ,tdb)
       ("bdb" ,bdb)
       ("gdbm" ,gdbm)
       ("libgcrypt" ,libgcrypt)
       ("lzo" ,lzo)
       ("bzip2" ,bzip2)
       ("zlib" ,zlib)))
    (home-page "https://nongnu.org/libchop/")
    (synopsis "Tools & library for data backup and distributed storage")
    (description
     "Libchop is a set of utilities and library for data backup and
distributed storage.  Its main application is @command{chop-backup}, an
encrypted backup program that supports data integrity checks, versioning,
distribution among several sites, selective sharing of stored data, adaptive
compression, and more.  The library itself implements storage techniques such
as content-addressable storage, content hash keys, Merkle trees, similarity
detection, and lossless compression.")
    (license license:gpl3+)))

(define-public borg
  (package
    (name "borg")
    (version "1.1.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "borgbackup" version))
       (sha256
        (base32
         "0x95nhv4h34m8cxycbwc4xdz350saaxlgh727b23bgn4ci7gh3vx"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete files generated by Cython.  We used to have a regex
           ;; that created the list of generated files but Borg has
           ;; added new non-generated C files that cause the regex to
           ;; generate the wrong list.
           (for-each delete-file
                     '("src/borg/algorithms/checksums.c"
                       "src/borg/chunker.c"
                       "src/borg/compress.c"
                       "src/borg/crypto/low_level.c"
                       "src/borg/hashindex.c"
                       "src/borg/item.c"
                       "src/borg/platform/darwin.c"
                       "src/borg/platform/freebsd.c"
                       "src/borg/platform/linux.c"
                       "src/borg/platform/posix.c"))
           ;; Remove bundled shared libraries.
           (with-directory-excursion "src/borg/algorithms"
             (for-each delete-file-recursively
                       (list "blake2" "lz4" "zstd")))
           #t))))
    (build-system python-build-system)
    (arguments
     `(#:modules ((srfi srfi-26) ; for cut
                  (guix build utils)
                  (guix build python-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((openssl (assoc-ref inputs "openssl"))
                   (libb2 (assoc-ref inputs "libb2"))
                   (lz4 (assoc-ref inputs "lz4"))
                   (zstd (assoc-ref inputs "zstd")))
               (setenv "BORG_OPENSSL_PREFIX" openssl)
               (setenv "BORG_LIBB2_PREFIX" libb2)
               (setenv "BORG_LIBLZ4_PREFIX" lz4)
               (setenv "BORG_LIBZSTD_PREFIX" zstd)
               (setenv "PYTHON_EGG_CACHE" "/tmp")
               ;; The test 'test_return_codes[python]' fails when
               ;; HOME=/homeless-shelter.
               (setenv "HOME" "/tmp")
               #t)))
         ;; The tests need to be run after Borg is installed.
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make the installed package available for the test suite.
             (add-installed-pythonpath inputs outputs)
             ;; The tests should be run in an empty directory.
             (mkdir-p "tests")
             (with-directory-excursion "tests"
               (invoke "py.test" "-v" "--pyargs" "borg.testsuite" "-k"
                       (string-append
                        ;; These tests need to write to '/var'.
                        "not test_get_cache_dir "
                        "and not test_get_config_dir "
                        "and not test_get_keys_dir "
                        "and not test_get_security_dir "
                        ;; These tests assume there is a root user in
                        ;; '/etc/passwd'.
                        "and not test_access_acl "
                        "and not test_default_acl "
                        "and not test_non_ascii_acl "
                        ;; This test needs the unpackaged pytest-benchmark.
                        "and not benchmark "
                        ;; These tests assume the kernel supports FUSE.
                        "and not test_fuse "
                        "and not test_fuse_allow_damaged_files "
                        "and not test_mount_hardlinks")))))
         (add-after 'install 'install-doc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man/man1"))
                    (misc (string-append out "/share/borg/misc")))
               (for-each (cut install-file <> misc)
                         '("docs/misc/create_chunker-params.txt"
                           "docs/misc/internals-picture.txt"
                           "docs/misc/prune-example.txt"))
               (copy-recursively "docs/man" man)
               #t))))))
    (native-inputs
     `(("python-cython" ,python-cython)
       ("python-setuptools-scm" ,python-setuptools-scm)
       ("python-pytest" ,python-pytest)))
    (inputs
     `(("acl" ,acl)
       ("libb2" ,libb2)
       ("lz4" ,lz4)
       ("openssl" ,openssl)
       ("python-llfuse" ,python-llfuse)
       ;; The Python msgpack library changed its name so Borg requires this
       ;; transitional package for now:
       ;; <https://bugs.gnu.org/30662>
       ("python-msgpack" ,python-msgpack-transitional)
       ("zstd" ,zstd)))
    (synopsis "Deduplicated, encrypted, authenticated and compressed backups")
    (description "Borg is a deduplicating backup program.  Optionally, it
supports compression and authenticated encryption.  The main goal of Borg is to
provide an efficient and secure way to backup data.  The data deduplication
technique used makes Borg suitable for daily backups since only changes are
stored.  The authenticated encryption technique makes it suitable for backups
to not fully trusted targets.  Borg is a fork of Attic.")
    (home-page "https://www.borgbackup.org/")
    (license license:bsd-3)))

(define-public attic
  (package
    (name "attic")
    (version "0.16")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Attic" version))
              (sha256
               (base32
                "0b5skd36r4c0915lwpkqg5hxm49gls9pprs1b7hc40910wlcsl36"))))
    (build-system python-build-system)
    (arguments
     `(;; The tests assume they are run as root:
       ;; https://github.com/jborg/attic/issues/7
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before
          'build 'set-openssl-prefix
          (lambda* (#:key inputs #:allow-other-keys)
            (setenv "ATTIC_OPENSSL_PREFIX" (assoc-ref inputs "openssl"))
            #t)))))
    (inputs
     `(("acl" ,acl)
       ("openssl" ,openssl)
       ("python-msgpack" ,python-msgpack)

       ;; Attic is probably incompatible with llfuse > 0.41.
       ;; These links are to discussions of llfuse compatibility from
       ;; the borg project. Borg is a recent fork of attic, and attic
       ;; has not been updated since the fork, so it's likely that
       ;; llfuse compatibility requirements are still the same.
       ;; https://github.com/borgbackup/borg/issues/642
       ;; https://github.com/borgbackup/borg/issues/643
       ("python-llfuse" ,python-llfuse-0.41)))
    (synopsis "Deduplicating backup program")
    (description "Attic is a deduplicating backup program.  The main goal of
Attic is to provide an efficient and secure way to backup data.  The data
deduplication technique used makes Attic suitable for daily backups since only
changes are stored.")
    (home-page "https://attic-backup.org/")
    (license license:bsd-3)
    (properties `((superseded . ,borg)))))

(define-public wimlib
  (package
    (name "wimlib")
    (version "1.13.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://wimlib.net/downloads/"
                                  "wimlib-" version ".tar.gz"))
              (sha256
               (base32
                "02wpsxjlw9vysj6x6q7kmvbcdkpvdzw201mmj5x0q670mapjrnai"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("fuse" ,fuse)
       ("libxml2" ,libxml2)
       ("ntfs-3g" ,ntfs-3g)
       ("openssl" ,openssl)))
    (arguments
     `(#:configure-flags (list "--enable-test-support")))
    (home-page "https://wimlib.net/")
    (synopsis "WIM file manipulation library and utilities")
    (description "wimlib is a C library and set of command-line utilities for
creating, modifying, extracting, and mounting archives in the Windows Imaging
Format (@dfn{WIM files}).  It can capture and apply WIMs directly from and to
NTFS volumes using @code{ntfs-3g}, preserving NTFS-specific attributes.")
    ;; wimlib is dual-licenced under version 3 or later of either the GPL or
    ;; LGPL, except those files explicitly marked as being released into the
    ;; public domain (CC0) in their headers.
    (license (list license:gpl3+
                   license:lgpl3+
                   license:cc0))))

(define-public obnam
  (package
    (name "obnam")
    (version "1.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://code.liw.fi/debian/pool/main/o/obnam/obnam_"
             version ".orig.tar.xz"))
       (sha256
        (base32
         "0qlipsq50hca71zc0dp1mg9zs12qm0sbblw7qfzl0hj6mk2rv1by"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'check
                  (lambda _
                    (substitute* "obnamlib/vfs_local_tests.py"
                      ;; Check for the nobody user instead of root.
                      (("self.fs.get_username\\(0\\), 'root'")
                       "self.fs.get_username(65534), 'nobody'")
                      ;; Disable tests checking for root group.
                      (("self.fs.get_groupname\\(0\\)") "'root'"))
                    (substitute* "obnamlib/vfs_local.py"
                      ;; Don't cover get_groupname function.
                      (("def get_groupname\\(self, gid\\):")
                       "def get_groupname(self, gid):  # pragma: no cover"))
                    ;; Can't run network tests.
                    (invoke "./check" "--unit-tests"))))))
    (inputs
     `(("python2-cliapp" ,python2-cliapp)
       ("python2-larch" ,python2-larch)
       ("python2-paramiko" ,python2-paramiko)
       ("python2-pyaml" ,python2-pyaml)
       ("python2-tracing" ,python2-tracing)
       ("python2-ttystatus" ,python2-ttystatus)))
    (native-inputs
     `(("gnupg" ,gnupg)
       ("python2-coverage" ,python2-coverage)
       ("python2-coverage-test-runner" ,python2-coverage-test-runner)
       ("python2-pep8" ,python2-pep8)
       ("python2-pylint" ,python2-pylint)))
    (home-page "https://obnam.org/")
    (synopsis "Retired backup program")
    (description
     "Warning: @uref{https://blog.liw.fi/posts/2017/08/13/retiring_obnam/,
the Obnam project is retired}.  You should use another backup solution instead.

Obnam was an easy, secure backup program.  Features included snapshot backups,
data de-duplication and encrypted backups using GnuPG.  Backups can be stored on
local hard disks, or online via the SSH SFTP protocol.  The backup server, if
used, does not require any special software, on top of SSH.")
    (license license:gpl3+)))

(define-public dirvish
  (package
    (name "dirvish")
    (version "1.2.1")
    (build-system gnu-build-system)
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://dirvish.org/dirvish-" version ".tgz"))
              (sha256
               (base32
                "1kbxa1irszp2zw8hd5qzqnrrzb4vxfivs1vn64yxnj0lak1jjzvb"))))
    (arguments
     `(#:modules ((ice-9 match) (ice-9 rdelim)
                  ,@%gnu-build-system-modules)
       #:phases
       ;; This mostly mirrors the steps taken in the install.sh that ships
       ;; with dirvish, but simplified because we aren't prompting interactively
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; These are mostly the same steps the install.sh that comes with
             ;; dirvish does
             (let* (;; Files we'll be copying
                    (executables
                     '("dirvish" "dirvish-runall"
                       "dirvish-expire" "dirvish-locate"))
                    (man-pages
                     '(("dirvish" "8") ("dirvish-runall" "8")
                       ("dirvish-expire" "8") ("dirvish-locate" "8")
                       ("dirvish.conf" "5")))

                    (output-dir
                     (assoc-ref outputs "out"))

                    ;; Just a default... not so useful on guixsd though
                    ;; You probably want to a service with file(s) to point to.
                    (confdir "/etc/dirvish")

                    (perl (string-append (assoc-ref %build-inputs "perl")
                                         "/bin/perl"))
                    (loadconfig.pl (call-with-input-file "loadconfig.pl"
                                     read-string)))


               (define (write-pl filename)
                 (define pl-header
                   (string-append "#!" perl "\n\n"
                                  "$CONFDIR = \"" confdir "\";\n\n"))
                 (define input-file-location
                   (string-append filename ".pl"))
                 (define target-file-location
                   (string-append output-dir "/bin/" filename ".pl"))
                 (define text-to-write
                   (string-append pl-header
                                  (call-with-input-file input-file-location
                                    read-string)
                                  "\n" loadconfig.pl))
                 (with-output-to-file target-file-location
                   (lambda ()
                     (display text-to-write)))
                 (chmod target-file-location #o755)
                 (wrap-program target-file-location
                   `("PERL5LIB" ":" prefix
                     ,(map (lambda (l) (string-append (assoc-ref %build-inputs l)
                                                      "/lib/perl5/site_perl"))
                           '("perl-libtime-period"
                             "perl-libtime-parsedate")))))

               (define write-man
                 (match-lambda
                   ((file-base man-num)
                    (let* ((filename
                            (string-append file-base "." man-num))
                           (output-path
                            (string-append output-dir
                                           "/share/man/man" man-num
                                           "/" filename)))
                      (copy-file filename output-path)))))

               ;; Make directories
               (mkdir-p (string-append output-dir "/bin/"))
               (mkdir-p (string-append output-dir "/share/man/man8/"))
               (mkdir-p (string-append output-dir "/share/man/man5/"))

               ;; Write out executables
               (for-each write-pl executables)
               ;; Write out man pages
               (for-each write-man man-pages)
               #t))))))
    (inputs
     `(("perl" ,perl)
       ("rsync" ,rsync)
       ("perl-libtime-period" ,perl-libtime-period)
       ("perl-libtime-parsedate" ,perl-libtime-parsedate)))
    (home-page "http://dirvish.org/")
    (synopsis "Fast, disk based, rotating network backup system")
    (description
     "With dirvish you can maintain a set of complete images of your
file systems with unattended creation and expiration.  A dirvish backup vault
is like a time machine for your data. ")
    (license (license:fsf-free "file://COPYING"
                               "Open Software License 2.0"))))

(define-public restic
  (package
    (name "restic")
    (version "0.9.4")
    ;; TODO Try packaging the bundled / vendored dependencies in the 'vendor/'
    ;; directory.
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/restic/restic/releases/download/"
                    "v" version "/restic-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "13ksprq1ia86px8x4lqrmx0l6y9rb1ppg8pnp7lcx0zxnq7skp67"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/restic/restic"
       #:unpack-path "github.com/restic"
      ;; We don't need to install the source code for end-user applications.
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion (string-append
                                        "src/github.com/restic/restic-"
                                        ,version)
               ;; Disable 'restic self-update'.  It makes little sense in Guix.
               (substitute* "build.go" (("selfupdate") ""))
               (setenv "HOME" (getcwd)) ; for $HOME/.cache/go-build
               (invoke "go" "run" "build.go"))))

         (replace 'check
           (lambda _
             (with-directory-excursion (string-append
                                        "src/github.com/restic/restic-"
                                        ,version)
               ;; Disable FUSE tests.
               (setenv "RESTIC_TEST_FUSE" "0")
               (invoke "go" "run" "build.go" "--test"))))

         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (src (string-append "src/github.com/restic/restic-"
                                       ,version)))
               (install-file (string-append src "/restic")
                             (string-append out "/bin"))
               #t)))

         (add-after 'install 'install-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man "/share/man")
                    (man-section (string-append man "/man"))
                    (src (string-append "src/github.com/restic/restic-"
                                        ,version "/doc/man/")))
               ;; Install all the man pages to "out".
               (for-each
                 (lambda (file)
                   (install-file file
                                 (string-append out man-section
                                                (string-take-right file 1))))
                 (find-files src "\\.[1-9]"))
               #t)))

         (add-after 'install-docs 'install-shell-completion
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (etc (string-append out "/etc"))
                    (share (string-append out "/share")))
               (for-each
                (lambda (shell)
                  (let* ((shell-name (symbol->string shell))
                         (dir (string-append "etc/completion/" shell-name)))
                    (mkdir-p dir)
                    (invoke (string-append bin "/restic") "generate"
                            (string-append "--" shell-name "-completion")
                            (string-append dir "/"
                                           (case shell
                                             ((bash) "restic")
                                             ((zsh) "_restic"))))))
                '(bash zsh))
               (with-directory-excursion "etc/completion"
                 (install-file "bash/restic"
                               (string-append etc "/bash_completion.d"))
                 (install-file "zsh/_restic"
                               (string-append share "/zsh/site-functions")))
               #t))))))
    (home-page "https://restic.net/")
    (synopsis "Backup program with multiple revisions, encryption and more")
    (description "Restic is a program that does backups right and was designed
with the following principles in mind:

@itemize
@item Easy: Doing backups should be a frictionless process, otherwise you
might be tempted to skip it.  Restic should be easy to configure and use, so
that, in the event of a data loss, you can just restore it.  Likewise,
restoring data should not be complicated.

@item Fast: Backing up your data with restic should only be limited by your
network or hard disk bandwidth so that you can backup your files every day.
Nobody does backups if it takes too much time.  Restoring backups should only
transfer data that is needed for the files that are to be restored, so that
this process is also fast.

@item Verifiable: Much more important than backup is restore, so restic
enables you to easily verify that all data can be restored.  @item Secure:
Restic uses cryptography to guarantee confidentiality and integrity of your
data.  The location the backup data is stored is assumed not to be a trusted
environment (e.g.  a shared space where others like system administrators are
able to access your backups).  Restic is built to secure your data against
such attackers.

@item Efficient: With the growth of data, additional snapshots should only
take the storage of the actual increment.  Even more, duplicate data should be
de-duplicated before it is actually written to the storage back end to save
precious backup space.
@end itemize")
    (license license:bsd-2)))

(define-public burp
  (package
    (name "burp")
    (version "2.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/burp/burp-" version
                                  "/burp-" version ".tar.bz2"))
              (sha256
               (base32
                "0in49c0ir7lb7jli0fcphdq1nh5rclhans4ngm7z7hzyxa4jrgri"))))
    (build-system gnu-build-system)
    (inputs
     `(("librsync" ,librsync)
       ("openssl" ,openssl)
       ("uthash" ,uthash)
       ("zlib" ,zlib)))
    (native-inputs
     `(("check" ,check)
       ("pkg-config" ,pkg-config)))
    (home-page "https://burp.grke.org")
    (synopsis "Differential backup and restore")
    (description "Burp is a network backup and restore program.  It attempts
to reduce network traffic and the amount of space that is used by each
backup.")
    (license license:agpl3)))
