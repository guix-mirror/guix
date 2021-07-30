;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2016, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2017, 2018, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2021 Leo Le Bouter <lle-bout@zaclys.net>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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

(define-module (gnu packages dbm)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (ice-9 match))

;;; Commentary:
;;;
;;; This module has been separated from (gnu packages databases) to reduce the
;;; number of module references for core packages.

(define-public bdb-4.8
  (package
    (name "bdb")
    (version "4.8.30")
    (license (license:non-copyleft "file://LICENSE"
                                   "See LICENSE in the distribution."))
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.oracle.com/berkeley-db/db-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0ampbl2f0hb1nix195kz1syrqqxpmvnvnfvphambj7xjrl3iljg0"))
             (patches (search-patches "bdb-5.3-atomics-on-gcc-9.patch"))))
    (build-system gnu-build-system)
    (outputs '("out"                             ; programs, libraries, headers
               "doc"))                           ; 94 MiB of HTML docs
    (arguments
     `(#:tests? #f                            ; no check target available
       #:disallowed-references ("doc")
       #:phases
       (modify-phases %standard-phases
         ;; The configure script is too old to recognise aarch64 and
         ;; powerpc64le as valid architectures.  The trick below works
         ;; for "--build", but not for "--host", so update config.sub.
         ,@(if (and (%current-target-system)
                    (or (target-ppc64le? (%current-target-system))
                        (target-aarch64? (%current-target-system))))
               `((add-after 'unpack 'update-config.sub
                   (lambda* (#:key native-inputs #:allow-other-keys)
                     (delete-file "dist/config.sub")
                     (symlink
                      (search-input-file native-inputs "/bin/config.sub")
                      "dist/config.sub"))))
               '())
         (replace 'configure
           (lambda* (#:key target outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (doc (assoc-ref outputs "doc")))
               ;; '--docdir' is not honored, so we need to patch.
               (substitute* "dist/Makefile.in"
                 (("docdir[[:blank:]]*=.*")
                  (string-append "docdir = " doc "/share/doc/bdb")))

               (chdir "build_unix")
               (invoke "../dist/configure"
                       (string-append "--prefix=" out)
                       (string-append "CONFIG_SHELL=" (which "bash"))
                       (string-append "SHELL=" (which "bash"))

                       ;; Bdb's config script doesn't recognize very many
                       ;; architectures, and is a dependant on the 'config'
                       ;; package, so we manually define the build target.
                       ,@(match (%current-system)
                           ("aarch64-linux"
                            '("--build=aarch64-unknown-linux-gnu"))
                           ("powerpc64le-linux"
                            '("--build=powerpc64le-unknown-linux-gnu"))
                           ("riscv64-linux"
                            '("--build=riscv64-unknown-linux-gnu"))
                           (_ '()))

                       ,@(if (%current-target-system)         ; cross building
                             '((string-append "--host=" target))
                             '())

                       ;; Remove 7 MiB of .a files.
                       "--disable-static"

                       ;; The compatibility mode is needed by some packages,
                       ;; notably iproute2.
                       "--enable-compat185"

                       ;; The following flag is needed so that the inclusion
                       ;; of db_cxx.h into C++ files works; it leads to
                       ;; HAVE_CXX_STDHEADERS being defined in db_cxx.h.
                       "--enable-cxx")))))))
    (native-inputs
     (if (and (%current-target-system)
              (or (target-ppc64le? (%current-target-system))
                  (target-aarch64? (%current-target-system))))
         `(("config" ,config)) ; for config.sub
         '()))
    (synopsis "Berkeley database")
    (description
     "Berkeley DB is an embeddable database allowing developers the choice of
SQL, Key/Value, XML/XQuery or Java Object storage for their data model.")
    ;; Starting with version 6, BDB is distributed under AGPL3. Many individual
    ;; files are covered by the 3-clause BSD license.
    (home-page
     "http://www.oracle.com/us/products/database/berkeley-db/overview/index.html")))

(define-public bdb-5.3
  (package (inherit bdb-4.8)
    (name "bdb")
    (version "5.3.28")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.oracle.com/berkeley-db/db-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0a1n5hbl7027fbz5lm0vp0zzfp1hmxnz14wx3zl9563h83br5ag0"))
              (patch-flags '("-p0"))
              (patches (search-patches "bdb-5.3-atomics-on-gcc-9.patch"))))))

(define-public bdb-6
  (package (inherit bdb-4.8)
    (name "bdb")
    (version "6.2.32")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.oracle.com/berkeley-db/db-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1yx8wzhch5wwh016nh0kfxvknjkafv6ybkqh6nh7lxx50jqf5id9"))))
    ;; Starting with version 6, BDB is distributed under AGPL3. Many individual
    ;; files are covered by the 3-clause BSD license.
    (license (list license:agpl3+ license:bsd-3))))

(define-public bdb bdb-6)

(define-public gdbm
  (package
    (name "gdbm")
    (version "1.20")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gdbm/gdbm-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "14m22j0zndd42yc0ps0bcnnjj2iq7agnp66sl882lj5k91bc1sis"))))
    (arguments `(#:configure-flags '("--enable-libgdbm-compat"
                                     "--disable-static")))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org.ua/software/gdbm")
    (synopsis
     "Hash library of database functions compatible with traditional dbm")
    (description
     "GDBM is a library for manipulating hashed databases.  It is used to
store key/value pairs in a file in a manner similar to the Unix dbm library
and provides interfaces to the traditional file format.")
    (license license:gpl3+)))
