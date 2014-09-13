;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2012 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages databases)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages check)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages python)
  #:use-module ((guix licenses)
                #:select (gpl2 gpl3+ lgpl3+ x11-style bsd-style
                          public-domain))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define-public bdb
  (package
    (name "bdb")
    (version "5.3.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.oracle.com/berkeley-db/db-" version
                                  ".tar.gz"))
              (sha256 (base32
                       "1f2g2612lf8djbwbwhxsvmffmf9d7693kh2l20195pqp0f9jmnfx"))))
    (build-system gnu-build-system)
    (outputs '("out"                             ; programs, libraries, headers
               "doc"))                           ; 94 MiB of HTML docs
    (arguments
     '(#:tests? #f                            ; no check target available
                #:phases
                (alist-replace
                 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out"))
                         (doc (assoc-ref outputs "doc")))
                     ;; '--docdir' is not honored, so we need to patch.
                     (substitute* "dist/Makefile.in"
                       (("docdir[[:blank:]]*=.*")
                        (string-append "docdir = " doc "/share/doc/bdb")))

                     (zero?
                      (system* "./dist/configure"
                               (string-append "--prefix=" out)
                               (string-append "CONFIG_SHELL=" (which "bash"))
                               (string-append "SHELL=" (which "bash"))

                               ;; The compatibility mode is needed by some packages,
                               ;; notably iproute2.
                               "--enable-compat185"))))
                 %standard-phases)))
    (synopsis "db, the Berkeley database")
    (description
     "Berkeley DB is an embeddable database allowing developers the choice of
SQL, Key/Value, XML/XQuery or Java Object storage for their data model.")
    (license (bsd-style "file://LICENSE"
                        "See LICENSE in the distribution."))
    (home-page
     "http://www.oracle.com/us/products/database/berkeley-db/overview/index.html")))

(define-public mysql
  (package
    (name "mysql")
    (version "5.1.73")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://dev.mysql.com/get/Downloads/MySQL-5.1/mysql-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1dfwi4ck0vq6sdci6gz0031s7zz5lc3pddqlgm0292s00l9y5sq5"))))
    (build-system gnu-build-system)
    (inputs
     `(("procps" ,procps)
       ("openssl" ,openssl)
       ("perl" ,perl)
       ("zlib" ,zlib)
       ("ncurses" ,ncurses)))
    (arguments
     '(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 ftw))                    ; for "rm -rf"
       #:phases (alist-cons-after
                 'install 'clean-up
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Remove the 112 MiB of tests that get installed.
                   (let ((out (assoc-ref outputs "out")))
                     (define (rm-rf dir)
                       (file-system-fold (const #t) ; enter?
                                         (lambda (file stat result) ; leaf
                                           (delete-file file))
                                         (const #t)               ; down
                                         (lambda (dir stat result) ; up
                                           (rmdir dir))
                                         (const #t)
                                         (lambda (file stat errno result)
                                           (format (current-error-port)
                                                   "error: ~a: ~a~%"
                                                   file (strerror errno)))
                                         #t
                                         (string-append out "/" dir)))
                     (rm-rf "mysql-test")
                     (rm-rf "sql-bench")

                     ;; Compress the 14 MiB Info file.
                     (zero?
                      (system* "gzip" "--best"
                               (string-append out "/share/info/mysql.info")))))
                 %standard-phases)))
    (home-page "http://www.mysql.com/")
    (synopsis "Fast, easy to use, and popular database")
    (description
     "MySQL is a fast, reliable, and easy to use relational database
management system that supports the standardized Structured Query
Language.")
    (license gpl2)))

(define-public postgresql
  (package
    (name "postgresql")
    (version "9.3.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.postgresql.org/pub/source/v"
                                  version "/postgresql-" version ".tar.gz"))
              (sha256
               (base32
                "08kga00izykgvnx7hn995wc4zjqslspapaa8z63045p1ya14mr4g"))))
    (build-system gnu-build-system)
    (inputs
     `(("readline" ,readline)
       ("zlib" ,zlib)))
    (home-page "http://www.postgresql.org/")
    (synopsis "Powerful object-relational database system")
    (description
     "PostgreSQL is a powerful object-relational database system.  It is fully
ACID compliant, has full support for foreign keys, joins, views, triggers, and
stored procedures (in multiple languages).  It includes most SQL:2008 data
types, including INTEGER, NUMERIC, BOOLEAN, CHAR, VARCHAR, DATE, INTERVAL, and
TIMESTAMP.  It also supports storage of binary large objects, including
pictures, sounds, or video.")
    (license (x11-style "file://COPYRIGHT"))))

(define-public recutils
  (package
    (name "recutils")
    (version "1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/recutils/recutils-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0cdwa4094x3yx7vn98xykvnlp9rngvd58d19vs3vh5hrvggccg93"))))
    (build-system gnu-build-system)

    ;; Running tests in parallel leads to test failures and crashes in
    ;; torture/utils.
    (arguments '(#:parallel-tests? #f))

    (native-inputs `(("emacs" ,emacs)
                     ("bc" ,bc)))

    ;; TODO: Add more optional inputs.
    ;; FIXME: Our Bash doesn't have development headers (need for the 'readrec'
    ;; built-in command), but it's not clear how to get them installed.
    ;; See <https://lists.gnu.org/archive/html/bug-bash/2014-03/msg00125.html>.
    (inputs `(("curl" ,curl)
              ("libgcrypt" ,libgcrypt)
              ("check" ,check)))
    (synopsis "Manipulate plain text files as databases")
    (description
     "GNU Recutils is a set of tools and libraries for creating and
manipulating text-based, human-editable databases.  Despite being text-based,
databases created with Recutils carry all of the expected features such as
unique fields, primary keys, time stamps and more.  Many different field
types are supported, as is encryption.")
    (license gpl3+)
    (home-page "http://www.gnu.org/software/recutils/")))

(define-public sqlite
  (package
   (name "sqlite")
   (version "3.8.4.3")
   (source (origin
            (method url-fetch)
            ;; TODO: Download from sqlite.org once this bug :
            ;; http://lists.gnu.org/archive/html/bug-guile/2013-01/msg00027.html
            ;; has been fixed.
            (uri (let ((numeric-version
                        (match (string-split version #\.)
                          ((first-digit other-digits ...)
                           (string-append first-digit
                                          (string-pad-right
                                           (string-concatenate
                                            (map (cut string-pad <> 2 #\0)
                                                 other-digits))
                                           6 #\0))))))
                   (string-append
                    "mirror://sourceforge/sqlite.mirror/SQLite%20" version
                    "/sqlite-autoconf-" numeric-version ".tar.gz")))
            (sha256
             (base32
              "0rcdsk5sz34w8vy0g5yhfms4saiq81i872jxx5m5sjij7bi9bsg0"))
            (patches
             (list (search-patch "sqlite-large-page-size-fix.patch")))))
   (build-system gnu-build-system)
   (home-page "http://www.sqlite.org/")
   (synopsis "The SQLite database management system")
   (description
    "SQLite is a software library that implements a self-contained, serverless,
zero-configuration, transactional SQL database engine. SQLite is the most
widely deployed SQL database engine in the world. The source code for SQLite is
in the public domain.")
   (license public-domain)))

(define-public tdb
  (package
    (name "tdb")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://samba.org/ftp/tdb/tdb-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "085sd2kii72fr0c4pdc7c7m0xk34nc66wnjp21c83dss826y9gh4"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-replace
                 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     ;; The 'configure' script is a wrapper for Waf and
                     ;; doesn't recognize things like '--enable-fast-install'.
                     (zero? (system* "./configure"
                                     (string-append "--prefix=" out)))))
                 %standard-phases)))
    (native-inputs
     `(;; TODO: Build the documentation.
       ;; ("docbook-xsl" ,docbook-xsl)
       ;; ("libxml2" ,libxml2)
       ;; ("libxslt" ,libxslt)
       ("python" ,python-2)))                     ;for the Waf build system
    (home-page "http://tdb.samba.org/")
    (synopsis "TDB, the trivial database")
    (description
     "TDB is a Trivial Database.  In concept, it is very much like GDBM,
and BSD's DB except that it allows multiple simultaneous writers and uses
locking internally to keep writers from trampling on each other.  TDB is also
extremely small.")
    (license lgpl3+)))
