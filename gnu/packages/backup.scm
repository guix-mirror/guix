;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2015, 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
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
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages ftp)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
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
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml))

(define-public duplicity
  (package
    (name "duplicity")
    (version "0.7.12")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://code.launchpad.net/duplicity/"
                          (version-major+minor version)
                          "-series/" version "/+download/duplicity-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1rhgrz2lm9vbfdp2raykrih1c6n2lw5jd572z4dsz488m52avjqi"))))
    (build-system python-build-system)
    (native-inputs
     `(("util-linux" ,util-linux)     ;setsid command, for the tests
       ("par2cmdline" ,par2cmdline)
       ("python-pexpect" ,python2-pexpect)
       ("mock" ,python2-mock)))
    (propagated-inputs
     `(("lockfile" ,python2-lockfile)
       ("urllib3" ,python2-urllib3)))
    (inputs
     `(("librsync" ,librsync)
       ("lftp" ,lftp)
       ("gnupg" ,gnupg)                 ;gpg executable needed
       ("util-linux" ,util-linux)       ;for setsid
       ("tzdata" ,tzdata)))
    (arguments
     `(#:python ,python-2               ;setup assumes Python 2
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-source
           (lambda* (#:key inputs #:allow-other-keys)
             ;; embed gpg store name
             (substitute* "duplicity/gpginterface.py"
               (("self.call = 'gpg'")
                (string-append "self.call = '" (assoc-ref inputs "gnupg") "/bin/gpg'")))
             (substitute* '("testing/functional/__init__.py"
                            "testing/overrides/bin/lftp")
               (("/bin/sh") (which "sh")))
             #t))
         (add-before 'check 'check-setup
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "HOME" (getcwd)) ;gpg needs to write to $HOME
             (setenv "TZDIR"          ;some timestamp checks need TZDIR
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
              (method url-fetch)
              (uri (string-append "https://github.com/Parchive/par2cmdline/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jpshmmcr81mxly0md2rr231qz9c8c680bbvcmhh100dg9i4a6s6"))))
    (native-inputs
     `(("automake" ,automake)
       ("autoconf" ,autoconf)))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autoreconf
           (lambda _ (zero? (system* "autoreconf" "-vfi")))))))
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
    (replacement libarchive-3.3.2)
    (version "3.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://libarchive.org/downloads/libarchive-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1rr40hxlm9vy5z2zb5w7pyfkgd1a4s061qapm83s19accb8mpji9"))))
    (build-system gnu-build-system)
    ;; TODO: Add -L/path/to/nettle in libarchive.pc.
    (inputs
     `(("zlib" ,zlib)
       ("nettle" ,nettle)
       ("lzo" ,lzo)
       ("bzip2" ,bzip2)
       ("libxml2" ,libxml2)
       ("xz" ,xz)))
    (arguments
     `(#:phases
       (alist-cons-before
        'build 'patch-pwd
        (lambda _
          (substitute* "Makefile"
            (("/bin/pwd") (which "pwd"))))
        (alist-replace
         'check
         (lambda _
           ;; XXX: The test_owner_parse, test_read_disk, and
           ;; test_write_disk_lookup tests expect user 'root' to exist, but
           ;; the chroot's /etc/passwd doesn't have it.  Turn off those tests.
           ;;
           ;; The tests allow one to disable tests matching a globbing pattern.
           (and (zero? (system* "make"
                                "libarchive_test" "bsdcpio_test" "bsdtar_test"))
                ;; XXX: This glob disables too much.
                (zero? (system* "./libarchive_test" "^test_*_disk*"))
                (zero? (system* "./bsdcpio_test" "^test_owner_parse"))
                (zero? (system* "./bsdtar_test"))))
         %standard-phases))
       ;; libarchive/test/test_write_format_gnutar_filenames.c needs to be
       ;; compiled with C99 or C11 or a gnu variant.
       #:configure-flags '("CFLAGS=-O2 -g -std=c99")))
    (home-page "http://libarchive.org/")
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

(define libarchive-3.3.2
  (package
    (inherit libarchive)
    (version "3.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://libarchive.org/downloads/libarchive-"
                           version ".tar.gz"))
       (patches (search-patches "libarchive-CVE-2017-14166.patch"
                                "libarchive-CVE-2017-14502.patch"))
       (sha256
        (base32
         "1km0mzfl6in7l5vz9kl09a88ajx562rw93ng9h2jqavrailvsbgd"))))))

(define-public rdup
  (package
    (name "rdup")
    (version "1.1.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://archive.miek.nl/projects/rdup/rdup-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "0aklwd9v7ix0m4ayl762sil685f42cwljzx3jz5skrnjaq32npmj"))
       (modules '((guix build utils)))
       (snippet
        ;; Some test scripts are missing shebangs, which cause "could not
        ;; execute" errors.  Add shebangs.
        '(for-each
          (lambda (testscript)
            (with-atomic-file-replacement
                (string-append "testsuite/rdup/" testscript)
              (lambda (in out)
                (begin
                  (format out "#!/bin/sh\n" )
                  (dump-port in out)))))
          '("rdup.hardlink.helper"
            "rdup.hardlink-strip.helper"
            "rdup.hardlink-strip2.helper"
            "rdup.pipeline.helper")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("dejagnu" ,dejagnu)))
    (inputs
     `(("glib" ,glib)
       ("pcre" ,pcre)
       ("libarchive" ,libarchive)
       ("nettle" ,nettle)))
    (arguments
     `(#:parallel-build? #f             ;race conditions
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-Werror
           ;; rdup uses a deprecated function from libarchive
           (lambda _
             (substitute* "GNUmakefile"
               (("^(CFLAGS=.*)-Werror" _ front) front))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             (setenv "HOME" (getcwd))
             (substitute* "testsuite/rdup/rdup.rdup-up-t-with-file.exp"
               (("/bin/cat") (which "cat")))
             #t)))))
    (home-page "http://archive.miek.nl/projects/rdup/index.html")
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
     `(("librsync" ,librsync)))
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
       ("librsync" ,librsync)))
    (arguments
     `(#:python ,python-2
       #:tests? #f))
    (home-page "http://www.nongnu.org/rdiff-backup/")
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
             (zero? (system* "make" "test")))))))
    (inputs
     `(("perl" ,perl)
       ("rsync" ,rsync)))
    (home-page "http://rsnapshot.org")
    (synopsis "Deduplicating snapshot backup utility based on rsync")
    (description "rsnapshot is a filesystem snapshot utility based on rsync.
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
    (native-inputs
     `(("guile" ,guile-2.0)
       ("gperf" ,gperf)
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
    (home-page "http://nongnu.org/libchop/")
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
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "borgbackup" version))
              (patches (search-patches "borg-fix-archive-corruption-bug.patch"))
              (sha256
               (base32
                "1rvn8b6clzd1r317r9jkvk34r31risi0dxfjc7jffhnwasck4anc"))
              (modules '((guix build utils)))
              (snippet
               '(for-each
                  delete-file (find-files "borg" "^(c|h|p).*\\.c$")))))
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
                   (lz4 (assoc-ref inputs "lz4")))
               (setenv "BORG_OPENSSL_PREFIX" openssl)
               (setenv "BORG_LZ4_PREFIX" lz4)
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
               (zero?
                 (system* "py.test" "-v" "--pyargs" "borg.testsuite" "-k"
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
                            "and not test_mount_hardlinks"))))))
         (add-after 'install 'install-doc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man/man1"))
                    (misc (string-append out "/share/borg/misc")))
               (for-each (cut install-file <> misc)
                         '("docs/misc/create_chunker-params.txt"
                           "docs/misc/internals-picture.txt"
                           "docs/misc/prune-example.txt"))
               (add-installed-pythonpath inputs outputs)
               (and
                 (zero? (system* "python3" "setup.py" "build_man"))
                 (begin
                   (copy-recursively "docs/man" man)
                   #t))))))))
    (native-inputs
     `(("python-cython" ,python-cython)
       ("python-setuptools-scm" ,python-setuptools-scm)
       ;; Borg 1.0.8's test suite uses 'tmpdir_factory', which was introduced in
       ;; pytest 2.8.
       ("python-pytest" ,python-pytest-3.0)
       ;; For generating the documentation.
       ("python-sphinx" ,python-sphinx)
       ("python-guzzle-sphinx-theme" ,python-guzzle-sphinx-theme)))
    (inputs
     `(("acl" ,acl)
       ("lz4" ,lz4)
       ("openssl" ,openssl)
       ("python-llfuse" ,python-llfuse)
       ("python-msgpack" ,python-msgpack)))
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
              (uri (string-append
                    "https://pypi.python.org/packages/source/A/Attic/Attic-"
                    version ".tar.gz"))
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
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://wimlib.net/downloads/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ks6hq7vwq13ljkzxp3a490bf8dnracgl2azf57rg49ad2fzab45"))))
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
                      ;; Check for the nobody user instead of root
                      (("self.fs.get_username\\(0\\), 'root'")
                       "self.fs.get_username(65534), 'nobody'")
                      ;; Disable tests checking for root group
                      (("self.fs.get_groupname\\(0\\)") "'root'"))
                    (substitute* "obnamlib/vfs_local.py"
                      ;; Don't cover get_groupname function
                      (("def get_groupname\\(self, gid\\):")
                       "def get_groupname(self, gid):  # pragma: no cover"))
                    ;; Can't run network tests
                    (zero? (system* "./check" "--unit-tests")))))))
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
    (synopsis "Easy and secure backup program")
    (description "Obnam is an easy, secure backup program.  Features
include snapshot backups, data de-duplication and encrypted backups
using GnuPG.  Backups can be stored on local hard disks, or online via
the SSH SFTP protocol.  The backup server, if used, does not require
any special software, on top of SSH.")
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
filesystems with unattended creation and expiration.  A dirvish backup vault
is like a time machine for your data. ")
    (license (license:fsf-free "file://COPYING"
                               "Open Software License 2.0"))))
