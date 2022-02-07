;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2020 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2015, 2016, 2017, 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Marcin Karpezo <sirmacik@wioo.waw.pl>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2021 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages ftp)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mcrypt)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages xml))

(define-public duplicity
  (package
    (name "duplicity")
    (version "0.8.21")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://code.launchpad.net/duplicity/"
                          (version-major+minor version)
                          "-series/" version "/+download/duplicity-"
                          version ".tar.gz"))
      (sha256
       (base32 "0ld4bhsi6iv4bvy99pblbr7vlwy9jbgfd6flyvb8qwbl8rvadzjp"))))
    (build-system python-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)     ; for msgfmt
       ("util-linux" ,util-linux)       ; setsid command, for the tests
       ("par2cmdline" ,par2cmdline)
       ("python-fasteners" ,python-fasteners)
       ("python-future" ,python-future) ; for tests
       ("python-paramiko" ,python-paramiko)
       ("python-pexpect" ,python-pexpect)
       ("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)
       ("python-setuptools-scm" ,python-setuptools-scm)
       ("tzdata" ,tzdata-for-tests)
       ("mock" ,python-mock)))
    (propagated-inputs
     `(("lockfile" ,python-lockfile)
       ("pygobject" ,python-pygobject)
       ("urllib3" ,python-urllib3)))
    (inputs
     (list dbus ; dbus-launch (Gio backend)
           librsync
           lftp
           gnupg ; gpg executable needed
           util-linux))     ; for setsid
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'use-store-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "duplicity/gpginterface.py"
               (("self.call = u'gpg'")
                (string-append "self.call = '" (assoc-ref inputs "gnupg") "/bin/gpg'")))
             (substitute* "duplicity/backends/giobackend.py"
               (("subprocess.Popen\\(\\[u'dbus-launch'\\]")
                (string-append "subprocess.Popen([u'"
                               (assoc-ref inputs "dbus")
                               "/bin/dbus-launch']")))
             (substitute* '("testing/functional/__init__.py"
                            "testing/overrides/bin/lftp")
               (("/bin/sh") (which "sh")))))
         (add-before 'check 'set-up-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "HOME" (getcwd))   ; gpg needs to write to $HOME
             (setenv "TZDIR"            ; some timestamp checks need TZDIR
                     (search-input-directory inputs "share/zoneinfo"))
             ;; Some things respect TMPDIR, others hard-code /tmp, and the
             ;; defaults don't match up, breaking test_restart.  Fix it.
             (setenv "TMPDIR" "/tmp"))))))
    (home-page "https://duplicity.gitlab.io/duplicity-web/")
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
    (version "0.8.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Parchive/par2cmdline")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11mx8q29cr0sryd11awab7y4mhqgbamb1ss77rffjj6in8pb4hdk"))))
    (native-inputs
     (list automake autoconf))
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
    (native-inputs (list pkg-config))
    (inputs
     (list glib
           tar
           lzop
           mcrypt
           openssh
           gnupg-1))
    (arguments
     `(#:configure-flags
       `(,(string-append "--sbindir=" (assoc-ref %outputs "out") "/bin"))
       #:tests? #f))
    (home-page (string-append "http://web.archive.org/web/20150925223424/"
                              "http://archive.miek.nl/projects/hdup/index.html"))
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
    (version "3.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "https://libarchive.org/downloads/libarchive-"
                                 version ".tar.xz")
                  (string-append "https://github.com/libarchive/libarchive"
                                 "/releases/download/v" version "/libarchive-"
                                 version ".tar.xz")))
       (sha256
        (base32
         "16r95rlmikll1k8vbhh06vq6x3srkc10hzxjjf3021mjs2ld65qf"))))
    (build-system gnu-build-system)
    (inputs
     (list bzip2
           libxml2
           lzo
           nettle
           xz
           zlib
           `(,zstd "lib")))
    (arguments
     `(#:configure-flags '("--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-pwd
           (lambda _
             (substitute* "Makefile"
               (("/bin/pwd") (which "pwd")))
             #t))
         (replace 'check
           (lambda* (#:key (tests? #t) #:allow-other-keys)
             (if tests?
		 ;; XXX: The test_owner_parse, test_read_disk, and
		 ;; test_write_disk_lookup tests expect user 'root' to
		 ;; exist, but the chroot's /etc/passwd doesn't have
		 ;; it.  Turn off those tests.
		 ;;
		 ;; XXX: Adjust test that fails with zstd 1.4.1
		 ;; because the default options compresses two bytes
		 ;; better than this test expects.
		 ;; https://github.com/libarchive/libarchive/issues/1226
                 (begin
                   (substitute* "libarchive/test/test_write_filter_zstd.c"
		     (("compression-level\", \"6\"")
		      "compression-level\", \"7\""))

		   ;; The tests allow one to disable tests matching a globbing pattern.
		   (invoke "make"
			   "libarchive_test"
			   "bsdcpio_test"
			   "bsdtar_test")

		   ;; XXX: This glob disables too much.
		   (invoke "./libarchive_test" "^test_*_disk*")
		   (invoke "./bsdcpio_test" "^test_owner_parse")
		   (invoke "./bsdtar_test"))
                 ;; Tests may be disabled if cross-compiling.
                 (format #t "Test suite not run.~%"))))
         (add-after 'install 'add--L-in-libarchive-pc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (lib     (string-append out "/lib"))
                    (nettle  (assoc-ref inputs "nettle"))
                    (libxml2 (assoc-ref inputs "libxml2"))
                    (xz      (assoc-ref inputs "xz"))
                    (zlib    (assoc-ref inputs "zlib"))
                    (zstd    (assoc-ref inputs "zstd"))
                    (bzip2   (assoc-ref inputs "bzip2")))
               ;; Embed absolute references to these inputs to avoid propagation.
               (substitute* (list (string-append lib "/pkgconfig/libarchive.pc")
                                  (string-append lib "/libarchive.la"))
                 (("-lnettle")
                  (string-append "-L" nettle "/lib -lnettle"))
                 (("-lxml2")
                  (string-append "-L" libxml2 "/lib -lxml2"))
                 (("-llzma")
                  (string-append "-L" xz "/lib -llzma"))
                 (("-lz")
                  (string-append "-L" zlib "/lib -lz"))
                 (("-lzstd")
                  (string-append "-L" zstd "/lib -lzstd"))
                 (("-lbz2")
                  (string-append "-L" bzip2 "/lib -lbz2")))
               #t))))))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/miekg/rdup")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bzyv6qmnivxnv9nw7lnfn46k0m1dlxcjj53zcva6v8y8084l1iw"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config
           ;; For tests.
           dejagnu))
    (inputs
     ;; XXX Compiling with nettle (encryption) support requires patching out
     ;; -Werror from GNUmakefile.in.  Then, rdup-tr-{en,de}crypt tests fail:
     ;; free(): invalid pointer
     ;; ** rdup-tr: SIGPIPE received, exiting
     (list glib pcre libarchive mcrypt))
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
         (add-before 'check 'disable-encryption-tests
           (lambda _
             (for-each delete-file
                       (list "testsuite/rdup/rdup.rdup-tr-crypt.exp"
                             "testsuite/rdup/rdup.rdup-tr-decrypt.exp"
                             "testsuite/rdup/rdup.rdup-tr-encrypt.exp"))
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

(define-public snapraid
  (package
    (name "snapraid")
    (version "12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/amadvance/snapraid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k8pynafkx8bhnqnjhc3jsds5p40sflz4drm88i6dg6ifv35mhh9"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           ;; XXX --enable-valgrind fails with ‘A must-be-redirected function
           ;; whose name matches the pattern: strlen in an object with soname
           ;; matching: ld-linux-x86-64.so.2 was not found […]’; used to work.
           #~(list "--with-blkid")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'bootstrap 'set-version
                 (lambda _
                   (setenv "VERSION" #$version)
                   (patch-shebang "autover.sh"))))))
    (native-inputs
     (list automake autoconf))
    (inputs
     (list `(,util-linux "lib"))) ; libblkid
    (home-page "https://www.snapraid.it/")
    (synopsis "Efficient backups using parity snapshots across disk arrays")
    (description
     "SnapRAID backs up files stored across multiple storage devices, such as
disk arrays, in an efficient way reminiscent of its namesake @acronym{RAID,
Redundant Array of Independent Disks} level 4.

Instead of creating a complete copy of the data like classic backups do, it
saves space by calculating one or more sets of parity information that's a
fraction of the size.  Each parity set is stored on an additional device the
size of the largest single storage volume, and protects against the loss of any
one device, up to a total of six.  If more devices fail than there are parity
sets, (only) the files they contained are lost, not the entire array.  Data
corruption by unreliable devices can also be detected and repaired.

SnapRAID is distinct from actual RAID in that it operates on files and creates
distinct snapshots only when run.  It mainly targets large collections of big
files that rarely change, like home media centers.  One disadvantage is that
@emph{all} data not in the latest snapshot may be lost if one device fails.  An
advantage is that accidentally deleted files can be recovered, which is not the
case with RAID.

It's also more flexible than true RAID: devices can have different sizes and
more can be added without disturbing others.  Devices that are not in use can
remain fully idle, saving power and producing less noise.")
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
     (list librsync-0.9))
    (arguments
     `(#:make-flags `(,(string-append "PREFIX=" (assoc-ref %outputs "out"))
                      ,(string-append "CC=" ,(cc-for-target)))
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
    (version "2.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/rdiff-backup/rdiff-backup/releases/"
                           "download/v" version "/rdiff-backup-" version ".tar.gz"))
       (sha256
        (base32 "11rvjcp77zwgkphz1kyf5yqgr3rlss7dm9xzmvpvc4lp99xq7drb"))))
    (build-system python-build-system)
    (native-inputs
     (list python-setuptools-scm))
    (inputs
     (list python librsync))
    (arguments
     `(#:tests? #f))                    ; Tests require root/sudo
    (home-page "https://rdiff-backup.net/")
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
    (version "1.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/rsnapshot/rsnapshot/releases/download/"
             version "/rsnapshot-" version ".tar.gz"))
       (sha256
        (base32 "0yc5k2fhm54ypxgm1fsaf8vrg5b7qbvbsqk371n6baf592vprjy1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "make" "test"))))))
    (inputs
     (list perl rsync))
    (home-page "https://rsnapshot.org")
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
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Include all the libtirpc headers necessary to get the
                  ;; definitions of 'u_int', etc.
                  (substitute* '("src/block-server.c"
                                 "include/chop/block-server.h"
                                 "utils/chop-block-server.c")
                    (("#include <rpc/(.*)\\.h>" _ header)
                     (string-append "#include <rpc/types.h>\n"
                                    "#include <rpc/rpc.h>\n"
                                    "#include <rpc/" header ".h>\n")))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(;; Link against libtirpc.
       #:configure-flags '("LDFLAGS=-ltirpc -Wl,--as-needed")

       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'adjust-configure-script
                    (lambda _
                      ;; Mimic upstream commit
                      ;; 25750ab5ef82fd3cfce5205d5f1ef07b47098091.
                      (substitute* "configure"
                        (("GUILE=(.*)--variable bindir`" _ middle)
                         (string-append "GUILE=" middle
                                        "--variable bindir`/guile")))))
                  (add-before 'build 'set-libtirpc-include-path
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; Allow <rpc/rpc.h> & co. to be found.
                      (let ((tirpc (string-append (assoc-ref inputs "libtirpc")
                                                  "/include/tirpc")))
                        (if (getenv "CPATH")
                          (setenv "CPATH"
                                  (string-append (getenv "CPATH")
                                                 ":" tirpc))
                          (setenv "CPATH" tirpc)))))
                  (add-before 'check 'skip-test
                    (lambda _
                      ;; XXX: This test fails (1) because current GnuTLS no
                      ;; longer supports OpenPGP authentication, and (2) for
                      ;; some obscure reason.  Better skip it.
                      (setenv "XFAIL_TESTS" "utils/block-server"))))))
    (native-inputs
     (list guile-2.0 gperf-3.0 ;see <https://bugs.gnu.org/32382>
           pkg-config rpcsvc-proto))           ;for 'rpcgen'
    (inputs
     (list guile-2.0
           util-linux
           libtirpc
           gnutls
           tdb
           bdb
           gdbm
           libgcrypt
           lzo
           bzip2
           zlib))
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
    (version "1.1.17")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "borgbackup" version))
       (sha256
        (base32 "0x0ncy0b0bmf586hbdgrif3gjmkdw760vfnfxndr493v07y29fbs"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete files generated by Cython.  We used to have a regex
           ;; that created the list of generated files but Borg has
           ;; added new non-generated C files that cause the regex to
           ;; generate the wrong list.
           (for-each delete-file
                     '("src/borg/algorithms/checksums.c"
                       "src/borg/algorithms/msgpack/_packer.cpp"
                       "src/borg/algorithms/msgpack/_unpacker.cpp"
                       "src/borg/chunker.c"
                       "src/borg/compress.c"
                       "src/borg/crypto/low_level.c"
                       "src/borg/hashindex.c"
                       "src/borg/item.c"
                       "src/borg/platform/darwin.c"
                       "src/borg/platform/freebsd.c"
                       "src/borg/platform/linux.c"
                       "src/borg/platform/posix.c"
                       "src/borg/platform/syncfilerange.c"))
           ;; Remove bundled shared libraries.
           (with-directory-excursion "src/borg/algorithms"
             (for-each delete-file-recursively
                       (list "blake2" "lz4" "zstd")))
           #t))))
    (build-system python-build-system)
    (arguments
     `(#:modules ((srfi srfi-26)        ; for cut
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
                        "and not test_create_stdin "
                        ;; This test needs the unpackaged pytest-benchmark.
                        "and not benchmark "
                        ;; These tests assume the kernel supports FUSE.
                        "and not test_fuse "
                        "and not test_fuse_allow_damaged_files "
                        "and not test_mount_hardlinks "
                        "and not test_readonly_mount ")))))
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
               #t)))
         (add-after 'install-docs 'install-shell-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (etc (string-append out "/etc"))
                    (share (string-append out "/share")))
               (with-directory-excursion "scripts/shell_completions"
                 (install-file "bash/borg"
                               (string-append etc "/bash_completion.d"))
                 (install-file "zsh/_borg"
                               (string-append share "/zsh/site-functions"))
                 (install-file "fish/borg.fish"
                               (string-append share "/fish/vendor_completions.d")))
               #t))))))
    (native-inputs
     (list python-cython python-setuptools-scm python-pytest))
    (inputs
     (list acl
           libb2
           lz4
           openssl
           python-llfuse
           `(,zstd "lib")))
    (synopsis "Deduplicated, encrypted, authenticated and compressed backups")
    (description "Borg is a deduplicating backup program.  Optionally, it
supports compression and authenticated encryption.  The main goal of Borg is to
provide an efficient and secure way to backup data.  The data deduplication
technique used makes Borg suitable for daily backups since only changes are
stored.  The authenticated encryption technique makes it suitable for
storing backups on untrusted computers.")
    (home-page "https://www.borgbackup.org/")
    (license license:bsd-3)))

(define-public wimlib
  (package
    (name "wimlib")
    (version "1.13.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://wimlib.net/downloads/"
                                  "wimlib-" version ".tar.gz"))
              (sha256
               (base32
                "08z3xxm5hq1n4wmyhgz14p1cv0w2lx610vn8nhfwpds4n7lwkz1j"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list fuse libxml2 ntfs-3g openssl))
    (arguments
     `(#:configure-flags
       (list "--disable-static"
             "--enable-test-support")))
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

                    ;; Just a default... not so useful on Guix Systems though.
                    ;; You probably want a service with file(s) to point to.
                    (confdir "/etc/dirvish")

                    (perl (search-input-file inputs "/bin/perl"))
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
     (list perl rsync perl-libtime-period perl-libtime-parsedate))
    (home-page "http://dirvish.org/")
    (synopsis "Fast, disk based, rotating network backup system")
    (description
     "With dirvish you can maintain a set of complete images of your
file systems with unattended creation and expiration.  A dirvish backup vault
is like a time machine for your data.")
    (license (license:fsf-free "file://COPYING"
                               "Open Software License 2.0"))))

(define-public restic
  (package
    (name "restic")
    (version "0.9.6")
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
                "1zmh42aah32ah8w5n6ilz9bci0y2xrf8p7qshy3yf1lzm5gnbj0w"))
              (patches
               (search-patches "restic-0.9.6-fix-tests-for-go1.15.patch"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/restic/restic"
      ;; We don't need to install the source code for end-user applications.
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "src/github.com/restic/restic"
               ;; Disable 'restic self-update'.  It makes little sense in Guix.
               (substitute* "build.go" (("selfupdate") ""))
               (setenv "HOME" (getcwd)) ; for $HOME/.cache/go-build
               (invoke "go" "run" "build.go"))))

         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "src/github.com/restic/restic"
                 ;; Disable FUSE tests.
                 (setenv "RESTIC_TEST_FUSE" "0")
                 (invoke "go" "run" "build.go" "--test")))))

         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (src "src/github.com/restic/restic"))
               (install-file (string-append src "/restic")
                             (string-append out "/bin"))
               #t)))

         (add-after 'install 'install-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man "/share/man")
                    (man-section (string-append man "/man"))
                    (src "src/github.com/restic/restic/doc/man/"))
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

(define-public zbackup
  (package
    (name "zbackup")
    (version "1.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zbackup/zbackup")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14l1kyxg7pccpax3d6qcpmdycb70kn3fxp1a59w64hqy2493hngl"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ;no test
    (inputs
     (list lzo libressl protobuf xz zlib))
    (home-page "http://zbackup.org")
    (synopsis "Versatile deduplicating backup tool")
    (description
     "ZBackup is a globally-deduplicating backup tool, based on the
ideas found in Rsync.  Feed a large @file{.tar} into it, and it will
store duplicate regions of it only once, then compress and optionally
encrypt the result.  Feed another @file{.tar} file, and it will also
re-use any data found in any previous backups.  This way only new
changes are stored, and as long as the files are not very different,
the amount of storage required is very low.  Any of the backup files
stored previously can be read back in full at any time.  The program
is format-agnostic, so you can feed virtually any files to it.")
    (license license:gpl2+)))

(define-public dump
  (package
    (name "dump")
    (version "0.4b47")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/dump/dump/"
                           version "/dump-" version ".tar.gz"))
       (sha256
        (base32
         "1l2gzzxyqhinx1yqvj4yn9k8vx3iyqi1965dxf9kvvdv9zgaq8fh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       `("--sysconfdir=/etc"
         "--disable-readline"
         "--disable-rmt")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl zlib
           `(,util-linux "lib") e2fsprogs))
    (home-page "https://dump.sourceforge.io/")
    (synopsis "Ext2/3/4 file system dump/restore utilities")
    (description "Dump examines files in a file system, determines which ones
need to be backed up, and copies those files to a specified disk, tape or
other storage medium.  Subsequent incremental backups can then be layered on
top of the full backup.  The restore command performs the inverse function of
dump; it can restore a full backup of a file system.  Single files and
directory subtrees may also be restored from full or partial backups in
interactive mode.")
    (license license:bsd-3)))

(define-public btrbk
  (package
    (name "btrbk")
    (version "0.31.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://digint.ch/download/btrbk/releases/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1lx7vnf386nsik8mxrrfyx1h7mkqk5zs26sy0s0lynfxcm4lkxb2"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list (string-append "PREFIX=" #$output))
      #:phases #~(modify-phases %standard-phases
                   (replace 'configure
                     (lambda _
                       (substitute* "Makefile"
                         (("= /etc")
                          (string-append "= " #$output "/etc")))))
                   (delete 'check)
                   (add-after 'install 'wrap-scripts
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (define btrbk (search-input-file outputs "bin/btrbk"))
                       ;; From a comment in btrbk, "Calling btrbk via 'lsbtr'
                       ;; symlink acts as an alias for 'btrbk ls', while also
                       ;; changing the semantics of the command line options."
                       (substitute* btrbk
                         (("program_name = \\$0")
                          (string-append "program_name = "
                                         "$ENV{'BTRBK_PROGRAM_NAME'}")))
                       ;; Wrap the script, so that it works with SSH URI and
                       ;; finds mbuffer out of the box.
                       (wrap-program btrbk
                         #:sh (search-input-file inputs "bin/bash")
                         '("BTRBK_PROGRAM_NAME" = ("$0"))
                         `("PATH" prefix
                           ,(list (string-append #$btrfs-progs "/bin")
                                  (string-append #$coreutils "/bin")
                                  (string-append #$mbuffer "/bin")
                                  (string-append #$openssh "/bin")))))))))
    (native-inputs (list ruby-asciidoctor))
    (inputs (list bash-minimal
                  btrfs-progs
                  coreutils
                  mbuffer
                  openssh
                  perl))
    (home-page "https://digint.ch/btrbk/")
    (synopsis "Backup tool for Btrfs subvolumes")
    (description "Btrbk is a backup tool for Btrfs subvolumes, taking
advantage of Btrfs specific capabilities to create atomic snapshots and
transfer them incrementally to your backup locations.  The source and target
locations are specified in a config file, which allows easily configuring
simple scenarios like e.g. a @i{laptop with locally attached backup disks}, as
well as more complex ones, e.g. a @i{server receiving backups from several
hosts via SSH, with different retention policy}.  It has features such as:
@itemize
@item atomic snapshots
@item incremental backups
@item flexible retention policy
@item backups to multiple destinations
@item transfer via SSH
@item resume backups (for removable and mobile devices)
@item archive to offline storage
@item encrypted backups to non-btrfs storage
@item wildcard subvolumes (useful for Docker and LXC containers)
@item transaction log
@item comprehensive list and statistics output
@item resolve and trace Btrfs parent-child and received-from relationships
@item list file changes between backups
@item calculate accurate disk space usage based on block regions.
@end itemize
Btrbk is designed to run as a cron job for triggering periodic snapshots and
backups, as well as from the command line (e.g. for instantly creating
additional snapshots).")
    (license license:gpl3+)))

(define-public burp
  (package
    (name "burp")
    (version "2.3.38")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grke/burp")
             (commit version)))
       (sha256
        (base32 "0m0s6rrgxn3l6bad45vyhks6iz6bwvd0f3rzdsc7l28gar79wsj6"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'extend-test-time-outs
           ;; The defaults are far too low for busy boxes & spinning storage.
           (lambda _
             (substitute* (find-files "utest" "\\.c$")
               (("(tcase_set_timeout\\(tc_core,)[ 0-9]*(\\);.*)$" _ prefix suffix)
                (string-append prefix " 3600" suffix "\n")))
             #t)))))
    (inputs
     (list acl
           librsync
           ncurses ; for the live status monitor
           openssl
           uthash
           zlib))
    (native-inputs
     (list autoconf automake check-0.14 pkg-config))
    (home-page "https://burp.grke.org")
    (synopsis "Differential backup and restore")
    (description "Burp is a network backup and restore program.  It attempts
to reduce network traffic and the amount of space that is used by each
backup.")
    (license license:agpl3)))

(define-public disarchive
  (package
    (name "disarchive")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.ngyro.com/disarchive/"
                                  "disarchive-" version ".tar.gz"))
              (sha256
               (base32
                "1pql8cspsxyx8cpw3xyhirnisv6rb4vj5mxr1d7w9la72q740n8s"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           pkg-config
           guile-3.0 ;for cross-compilation
           guile-gcrypt
           guile-lzma
           guile-quickcheck))
    (inputs
     (list guile-3.0 zlib))
    (propagated-inputs
     (list guile-gcrypt guile-lzma))
    (home-page "https://ngyro.com/software/disarchive.html")
    (synopsis "Software archive disassembler")
    (description "Disarchive can disassemble software archives into data
and metadata.  The goal is to create a small amount of metadata that
can be used to recreate a software archive bit-for-bit from the
original files.  For example, a software archive made using tar and
Gzip will need to describe the order of files in the tarball and the
compression parameters used by Gzip.")
    (license license:gpl3+)))

(define-public borgmatic
  (package
    (name "borgmatic")
    (version "1.5.22")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "borgmatic" version))
       (sha256
        (base32 "0pvqlj17vp81i7saxqh5hsaxqz29ldrjd7bcssh4g1h0ikmnaf2r"))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'configure
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Set absolute store path to borg.
                   (substitute* "borgmatic/commands/borgmatic.py"
                     (("location\\.get\\('local_path', 'borg'\\)")
                      (string-append "location.get('local_path', '"
                                     (search-input-file inputs "bin/borg")
                                     "')")))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     ;; Tests require the installed executable.
                     (setenv "PATH" (string-append #$output "/bin"
                                                   ":" (getenv "PATH")))
                     (invoke "pytest")))))))
    (inputs
     (list borg python-colorama python-jsonschema python-requests
           python-ruamel.yaml))
    (native-inputs
     (list python-flexmock python-pytest python-pytest-cov))
    (home-page "https://torsion.org/borgmatic/")
    (synopsis "Simple, configuration-driven backup software")
    (description
     "borgmatic is simple, configuration-driven backup software for servers
and workstations.  Protect your files with client-side encryption.  Backup
your databases too.  Monitor it all with integrated third-party services.
borgmatic is powered by borg.")
    (license license:gpl3+)))

(define-public vorta
  (package
    (name "vorta")
    (version "0.8.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "vorta" version))
              (sha256
               (base32
                "0674crxsmf1gwiacpi9ixypgz685fyzr5y3f9sd768b0bmh7ilsn"))))
    (build-system python-build-system)
    (arguments
     (list
      #:imported-modules `((guix build qt-utils)
                           (guix build cmake-build-system)
                           (guix build qt-build-system)
                           ,@%python-build-system-modules)
      #:modules '((guix build utils)
                  (guix build python-build-system)
                  ((guix build qt-build-system) #:prefix qt:))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-borg-path
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/vorta/borg/borg_job.py"
                (("which\\('borg'\\)")
                 (string-append "which('" #$(this-package-input "borg")
                                "/bin/borg')")))))
          ;; XXX This phase tries to write to $HOME
          (add-before 'sanity-check 'set-HOME
            (lambda _
              (setenv "HOME" "/tmp")))
          ;; Otherwise, the user interface's icons will be missing.
          (add-after 'wrap 'qt-wrap
            (assoc-ref qt:%standard-phases 'qt-wrap)))))
    (native-inputs
     (list python-pytest-mock
           python-pytest-qt
           python-pytest-runner
           python-setuptools-git))
    (inputs
     (list borg
           python-appdirs
           python-dateutil
           python-keyring
           python-paramiko
           python-peewee
           python-psutil
           python-pyqt-without-qtwebkit
           python-secretstorage
           ;; This is included so that the qt-wrap phase picks it up.
           qtsvg))
    (home-page "https://github.com/borgbase/vorta")
    (synopsis "Graphical backup client based on BorgBackup")
    (description "Vorta is a graphical backup client based on the Borg backup
tool.  It supports the use of remote backup repositories.  It can perform
scheduled backups, and has a graphical tool for browsing and extracting the Borg
archives.")
    (license license:gpl3+)))
