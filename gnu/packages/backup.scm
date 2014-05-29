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
  #:use-module (guix licenses)
  #:use-module (guix download)
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
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ssh)
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
     `(("python2-setuptools" ,python2-setuptools)))
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
    (license gpl2+)))

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
    (license gpl2)))

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
         "0aklwd9v7ix0m4ayl762sil685f42cwljzx3jz5skrnjaq32npmj"))))
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
                 %standard-phases)))
    (home-page "http://archive.miek.nl/projects/rdup/index.html")
    (synopsis "Provide a list of files to backup")
    (description
     "Rdup is a utility inspired by rsync and the plan9 way of doing backups.
Rdup itself does not backup anything, it only print a list of absolute
filenames to standard output.  Auxiliary scripts are needed that act on this
list and implement the backup strategy.")
    (license gpl3+)))
