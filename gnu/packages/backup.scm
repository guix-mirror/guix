;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix download)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages mcrypt)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages python)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-1))

(define-public duplicity
  (package
    (name "duplicity")
    (version "0.6.24")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://code.launchpad.net/duplicity/"
                          (string-join (take (string-split version #\.) 2) ".")
                          "-series/" version "/+download/duplicity-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0l14nrhbgkyjgvh339bbhnm6hrdwrjadphq1jmpi0mcgcdbdfh8x"))))
    (build-system python-build-system)
    (native-inputs
     `(("python2-setuptools" ,python2-setuptools)
       ("util-linux" ,util-linux)))     ;setsid command, for the tests
    (inputs
     `(("python" ,python-2)
       ("librsync" ,librsync)
       ("mock" ,python2-mock)           ;for testing
       ("lockfile" ,python2-lockfile)
       ("gnupg" ,gnupg-1)))             ;gpg executable needed
    (arguments
     `(#:python ,python-2               ;setup assumes Python 2
       #:test-target "test"
       #:phases (alist-cons-before
                 'check 'patch-tests
                 (lambda _
                   (substitute* "testing/functional/__init__.py"
                     (("/bin/sh") (which "sh"))))
                 %standard-phases)))
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

(define-public hdup
  (package
    (name "hdup")
    (version "2.0.14")
    (source
     (origin
      (method url-fetch)
      ;; Source tarballs are not versioned
      (uri "http://archive.miek.nl/projects/hdup2/hdup.tar.bz2")
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
     "Hdup2 is a backup utilty, its aim is to make backup really simple.  The
backup scheduling is done by means of a cron job.  It supports an
include/exclude mechanism, remote backups, encrypted backups and split
backups (called chunks) to allow easy burning to CD/DVD.")
    (license license:gpl2)))

(define-public libarchive
  (package
    (name "libarchive")
    (version "3.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://libarchive.org/downloads/libarchive-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0pixqnrcf35dnqgv0lp7qlcw7k13620qkhgxr288v7p4iz6ym1zb"))))
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
         %standard-phases))))
    (home-page "http://libarchive.org/")
    (synopsis "Multi-format archive and compression library")
    (description
     "Libarchive provides a flexible interface for reading and writing
archives in various formats such as tar and cpio.  Libarchive also supports
reading and writing archives compressed using various compression filters such
as gzip and bzip2.  The library is inherently stream-oriented; readers
serially iterate through the archive, writers serially add things to the
archive. In particular, note that there is currently no built-in support for
random access nor for in-place modification.")
    (license license:bsd-2)))

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
       #:phases (alist-cons-before
                 'build 'remove-Werror
                 ;; rdup uses a deprecated function from libarchive
                 (lambda _
                   (substitute* "GNUmakefile"
                     (("^(CFLAGS=.*)-Werror" _ front) front)))
                 (alist-cons-before
                  'check 'pre-check
                  (lambda _
                    (setenv "HOME" (getcwd))
                    (substitute* "testsuite/rdup/rdup.rdup-up-t-with-file.exp"
                      (("/bin/cat") (which "cat"))))
                  
                  %standard-phases))))
    (home-page "http://archive.miek.nl/projects/rdup/index.html")
    (synopsis "Provide a list of files to backup")
    (description
     "Rdup is a utility inspired by rsync and the plan9 way of doing backups.
Rdup itself does not backup anything, it only print a list of absolute
filenames to standard output.  Auxiliary scripts are needed that act on this
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
       (alist-delete
        'configure                      ;no configure phase
        %standard-phases)))
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
    (native-inputs
     `(("python2-setuptools" ,python2-setuptools)))
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
rdiff-backup is easy to use and settings have sensical defaults.")
    (license license:gpl2+)))
