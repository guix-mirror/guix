;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2012, 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2017 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Leo Famulari <leo@famulari.name>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2015, 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 ng0 <contact.ng0@cryptolab.net>
;;; Copyright © 2016, 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2016 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016, 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2017 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2015, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Kristofer Buffington <kristoferbuffington@gmail.com>
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
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages time)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system r)
  #:use-module (guix build-system scons)
  #:use-module ((guix build utils) #:hide (which))
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define-public 4store
  (package
    (name "4store")
    (version "1.1.6")
    (source (origin
      (method url-fetch)
      (uri (string-append "https://github.com/garlik/4store/archive/v"
                          version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "004fmcf1w75zhc1x3zc6kc97j4jqn2v5nhk6yb3z3cpfrhzi9j50"))
      (patches (list (search-patch "4store-fix-buildsystem.patch")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("libtool" ,libtool)
       ("pcre" ,pcre "bin")                       ;for 'pcre-config'
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("rasqal" ,rasqal)
       ("libxml2" ,libxml2)
       ("raptor2" ,raptor2)
       ("readline" ,readline)
       ("avahi" ,avahi)
       ("cyrus-sasl" ,cyrus-sasl)
       ("openssl" ,openssl)
       ("util-linux" ,util-linux)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'generate-configure
           (lambda _
             (zero? (system* "sh" "autogen.sh")))))))
    ;; http://www.4store.org has been down for a while now.
    (home-page "https://github.com/garlik/4store")
    (synopsis "Clustered RDF storage and query engine")
    (description "4store is a RDF/SPARQL store written in C, supporting
either single machines or networked clusters.")
    (license license:gpl3+)))

(define-public gdbm
  (package
    (name "gdbm")
    (version "1.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gdbm/gdbm-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0lx201q20dvc70f8a3c9s7s18z15inlxvbffph97ngvrgnyjq9cx"))))
    (arguments `(#:configure-flags '("--enable-libgdbm-compat")))
    (build-system gnu-build-system)
    (home-page "http://www.gnu.org.ua/software/gdbm")
    (synopsis
     "Hash library of database functions compatible with traditional dbm")
    (description
     "GDBM is a library for manipulating hashed databases.  It is used to
store key/value pairs in a file in a manner similar to the Unix dbm library
and provides interfaces to the traditional file format.")
    (license license:gpl3+)))

(define-public bdb
  (package
    (name "bdb")
    (version "6.2.32")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.oracle.com/berkeley-db/db-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1yx8wzhch5wwh016nh0kfxvknjkafv6ybkqh6nh7lxx50jqf5id9"))))
    (build-system gnu-build-system)
    (outputs '("out"                             ; programs, libraries, headers
               "doc"))                           ; 94 MiB of HTML docs
    (arguments
     '(#:tests? #f                            ; no check target available
       #:disallowed-references ("doc")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
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

                         ;; Remove 7 MiB of .a files.
                         "--disable-static"

                         ;; The compatibility mode is needed by some packages,
                         ;; notably iproute2.
                         "--enable-compat185"

                         ;; The following flag is needed so that the inclusion
                         ;; of db_cxx.h into C++ files works; it leads to
                         ;; HAVE_CXX_STDHEADERS being defined in db_cxx.h.
                         "--enable-cxx"))))))))
    (synopsis "Berkeley database")
    (description
     "Berkeley DB is an embeddable database allowing developers the choice of
SQL, Key/Value, XML/XQuery or Java Object storage for their data model.")
    ;; Starting with version 6, BDB is distributed under AGPL3. Many individual
    ;; files are covered by the 3-clause BSD license.
    (license (list license:agpl3+ license:bsd-3))
    (home-page
     "http://www.oracle.com/us/products/database/berkeley-db/overview/index.html")))

(define-public bdb-5.3
  (package (inherit bdb)
    (name "bdb")
    (version "5.3.28")
    (license (license:non-copyleft "file://LICENSE"
                                   "See LICENSE in the distribution."))
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.oracle.com/berkeley-db/db-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0a1n5hbl7027fbz5lm0vp0zzfp1hmxnz14wx3zl9563h83br5ag0"))))
    (arguments
     `(#:tests? #f                            ; no check target available
       #:disallowed-references ("doc")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
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

                         ;; Bdb doesn't recognize aarch64 as an architecture.
                         ,@(if (string=? "aarch64-linux" (%current-system))
                               '("--build=aarch64-unknown-linux-gnu")
                               '())

                         ;; Remove 7 MiB of .a files.
                         "--disable-static"

                         ;; The compatibility mode is needed by some packages,
                         ;; notably iproute2.
                         "--enable-compat185"

                         ;; The following flag is needed so that the inclusion
                         ;; of db_cxx.h into C++ files works; it leads to
                         ;; HAVE_CXX_STDHEADERS being defined in db_cxx.h.
                         "--enable-cxx"))))))))))

(define-public es-dump-restore
  (package
    (name "es-dump-restore")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "es_dump_restore" version))
       (sha256
        (base32
         "020yk7f1hw48clmf5501z3xv9shsdchyymcv0y2cci2c1xvr1mim"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f ;; No testsuite.
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-bin-es_dump_restore
           (lambda* (#:key outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/es_dump_restore")
               `("GEM_PATH" ":" prefix (,(string-append
                                          (getenv "GEM_PATH")
                                          ":"
                                          (getenv "GEM_HOME")))))
             #t)))))
    (propagated-inputs
     `(("ruby-httpclient" ,ruby-httpclient)
       ("ruby-multi-json" ,ruby-multi-json)
       ("ruby-progress_bar" ,ruby-progress_bar)
       ("ruby-rubyzip" ,ruby-rubyzip)
       ("ruby-thor" ,ruby-thor)))
    (synopsis "Utility for dumping and restoring ElasticSearch indexes")
    (description
     "This package provides a utility for dumping the contents of an
ElasticSearch index to a compressed file and restoring the dumpfile back to an
ElasticSearch server")
    (home-page "https://github.com/patientslikeme/es_dump_restore")
    (license license:expat)))

(define-public leveldb
  (package
    (name "leveldb")
    (version "1.20")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/leveldb"
                                  "/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0r36bcrj6b2afsp4aw1gjai3jbs1c7734pxpc1jz7hh9nasyiazm"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           ;; There is no install target, so we do it here.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (include (string-append out "/include")))
               (for-each (lambda (file)
                           (install-file file lib))
                         (find-files "out-shared" "^libleveldb\\.so.*$"))
               (copy-recursively "include" include)
               #t))))))
    (inputs
     `(("snappy" ,snappy)))
    (home-page "http://leveldb.org/")
    (synopsis "Fast key-value storage library")
    (description
     "LevelDB is a fast key-value storage library that provides an ordered
mapping from string keys to string values.")
    (license license:bsd-3)))

(define-public memcached
  (package
    (name "memcached")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://memcached.org/files/memcached-" version ".tar.gz"))
       (sha256
        (base32 "0chwc0g7wfvcad36z8pf2jbgygdnm9nm1l6pwjsn3d2b089gh0f0"))))
    (build-system gnu-build-system)
    (inputs
     `(("libevent" ,libevent)
       ("cyrus-sasl" ,cyrus-sasl)))
    (home-page "https://memcached.org/")
    (synopsis "In memory caching service")
    (description "Memcached is a in memory key value store.  It has a small
and generic API, and was originally intended for use with dynamic web
applications.")
    (license license:bsd-3)))

(define-public mongodb
  (package
    (name "mongodb")
    (version "3.4.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mongodb/mongo/archive/r"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "0676lvkljj7a5hdhv78dbykqnqrj9lbn9799mi84b8vbnzsq961r"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each (lambda (dir)
                              (delete-file-recursively
                                (string-append "src/third_party/" dir)))
                            '("pcre-8.41" "scons-2.5.0" "snappy-1.1.3"
                              "valgrind-3.11.0" "wiredtiger"
                              "yaml-cpp-0.5.3" "zlib-1.2.8"))))
              (patches
               (list
                (search-patch "mongodb-support-unknown-linux-distributions.patch")))))
    (build-system scons-build-system)
    (inputs
     `(("openssl" ,openssl)
       ("pcre" ,pcre)
        ,@(match (%current-system)
            ((or "x86_64-linux" "aarch64-linux" "mips64el-linux")
             `(("wiredtiger" ,wiredtiger)))
            (_ `()))
       ("yaml-cpp" ,yaml-cpp)
       ("zlib" ,zlib)
       ("snappy" ,snappy)))
    (native-inputs
     `(("valgrind" ,valgrind)
       ("perl" ,perl)
       ("python" ,python2-minimal)
       ("python2-pymongo" ,python2-pymongo)
       ("python2-pyyaml" ,python2-pyyaml)
       ("tzdata" ,tzdata)))
    (arguments
     `(#:scons ,scons-python2
       #:phases
       (let ((common-options
              `(;; "--use-system-tcmalloc" TODO: Missing gperftools
                "--use-system-pcre"
                ;; wiredtiger is 64-bit only
                ,,(if (any (cute string-prefix? <> (or (%current-target-system)
                                                       (%current-system)))
                           '("i686-linux" "armhf-linux"))
                    ``"--wiredtiger=off"
                    ``"--use-system-wiredtiger")
                ;; TODO
                ;; build/opt/mongo/db/fts/unicode/string.o failed: Error 1
                ;; --use-system-boost
                "--use-system-snappy"
                "--use-system-zlib"
                "--use-system-valgrind"
                ;; "--use-system-stemmer" TODO: Missing relevant package
                "--use-system-yaml"
                "--disable-warnings-as-errors"
                ,(format #f "--jobs=~a" (parallel-job-count))
                "--ssl")))
         (modify-phases %standard-phases
           (add-after 'unpack 'scons-propagate-environment
             (lambda _
               ;; Modify the SConstruct file to arrange for
               ;; environment variables to be propagated.
               (substitute* "SConstruct"
                 (("^env = Environment\\(")
                  "env = Environment(ENV=os.environ, "))
               #t))
           (add-after 'unpack 'create-version-file
             (lambda _
               (call-with-output-file "version.json"
                 (lambda (port)
                   (display ,(simple-format #f "{
    \"version\": \"~A\"
}" version) port)))
               #t))
           (replace 'build
             (lambda _
               (zero? (apply system*
                             `("scons"
                               ,@common-options
                               "mongod" "mongo" "mongos")))))
           (replace 'check
             (lambda* (#:key tests? inputs #:allow-other-keys)
               (setenv "TZDIR"
                       (string-append (assoc-ref inputs "tzdata")
                                      "/share/zoneinfo"))
               (or (not tests?)
                   ;; Note that with the tests, especially the unittests, the
                   ;; build can take up to ~45GB of space, as many tests are
                   ;; individual executable files, with some being hundreds of
                   ;; megabytes in size.
                   (begin
		     (apply
                      invoke `("scons" ,@common-options "dbtest" "unittests"))
                     (substitute* "build/unittests.txt"
                       ;; TODO: Don't run the async_stream_test, as it hangs
                       (("^build\\/opt\\/mongo\\/executor\\/async\\_stream\\_test\n$")
                        "")
                       ;; TODO: This test fails
                       ;; Expected 0UL != disks.size() (0 != 0) @src/mongo/util/procparser_test.cpp:476
                       (("^build\\/opt\\/mongo\\/util\\/procparser\\_test\n$")
                        ""))
		     (invoke "python" "buildscripts/resmoke.py"
			     "--suites=dbtest,unittests"
                             (format #f  "--jobs=~a" (parallel-job-count)))))))
           (replace 'install
             (lambda _
               (let ((bin  (string-append (assoc-ref %outputs "out") "/bin")))
                 (install-file "mongod" bin)
                 (install-file "mongos" bin)
                 (install-file "mongo" bin))
               #t))))))
    (home-page "https://www.mongodb.org/")
    (synopsis "High performance and high availability document database")
    (description
     "Mongo is a high-performance, high availability, schema-free
document-oriented database.  A key goal of MongoDB is to bridge the gap
between key/value stores (which are fast and highly scalable) and traditional
RDBMS systems (which are deep in functionality).")
    (license (list license:agpl3
                   ;; Some parts are licensed under the Apache License
                   license:asl2.0))))

(define-public mysql
  (package
    (name "mysql")
    (version "5.7.20")
    (source (origin
             (method url-fetch)
             (uri (list (string-append
                          "http://dev.mysql.com/get/Downloads/MySQL-"
                          (version-major+minor version) "/"
                          name "-" version ".tar.gz")
                        (string-append
                          "http://downloads.mysql.com/archives/get/file/"
                          name "-" version ".tar.gz")))
             (sha256
              (base32
               "11v4g3igigv3zvknv67qml8in6fjrbs2vnr3q6bg6f62nydm95sk"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       '("-DBUILD_CONFIG=mysql_release"
         "-DWITH_SSL=system"
         "-DWITH_ZLIB=system"
         "-DDEFAULT_CHARSET=utf8"
         "-DDEFAULT_COLLATION=utf8_general_ci"
         "-DMYSQL_DATADIR=/var/lib/mysql"
         "-DMYSQL_UNIX_ADDR=/run/mysqld/mysqld.sock"
         "-DINSTALL_INFODIR=share/mysql/docs"
         "-DINSTALL_MANDIR=share/man"
         "-DINSTALL_PLUGINDIR=lib/mysql/plugin"
         "-DINSTALL_SCRIPTDIR=bin"
         "-DINSTALL_INCLUDEDIR=include/mysql"
         "-DINSTALL_DOCREADMEDIR=share/mysql/docs"
         "-DINSTALL_SUPPORTFILESDIR=share/mysql"
         "-DINSTALL_MYSQLSHAREDIR=share/mysql"
         "-DINSTALL_DOCDIR=share/mysql/docs"
         "-DINSTALL_SHAREDIR=share/mysql"
         ;; Get rid of test data.
         "-DINSTALL_MYSQLTESTDIR="
         "-DINSTALL_SQLBENCHDIR=")
       #:phases (modify-phases %standard-phases
                  (add-after
                   'unpack 'patch-boost-version
                   (lambda _
                     ;; Mysql wants boost-1.59.0 specifically
                     (substitute* "cmake/boost.cmake"
                       (("59")
                        ,(match (string-split (package-version boost) #\.)
                           ((_ minor . _) minor))))))
                  (add-after
                   'install 'remove-extra-binaries
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let ((out (assoc-ref outputs "out")))
                       ;; Remove the 3 *_embedded files, which weigh in at
                       ;; 14 MiB each.
                       (for-each delete-file
                                 (find-files (string-append out "/bin")
                                             "_embedded$"))
                       #t))))))
    (native-inputs
     `(("bison" ,bison)
       ("perl" ,perl)))
    (inputs
     `(("boost" ,boost)
       ("libaio" ,libaio)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (home-page "http://www.mysql.com/")
    (synopsis "Fast, easy to use, and popular database")
    (description
     "MySQL is a fast, reliable, and easy to use relational database
management system that supports the standardized Structured Query
Language.")
    (license license:gpl2)))

(define-public mariadb
  (package
    (name "mariadb")
    (version "10.1.29")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://downloads.mariadb.org/f/"
                                  name "-" version "/source/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1m3ya6c3snnsyscd0waklayqfv0vhws52iizv2j5masj5xhdbfvk"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       '("-DBUILD_CONFIG=mysql_release"
         ;; Linking with libarchive fails, like this:

         ;; ld: /gnu/store/...-libarchive-3.2.2/lib/libarchive.a(archive_entry.o):
         ;; relocation R_X86_64_32 against `.bss' can not be used when
         ;; making a shared object; recompile with -fPIC

         ;; For now, disable the features that that use libarchive (xtrabackup).
         "-DWITH_LIBARCHIVE=OFF"

         "-DDEFAULT_CHARSET=utf8"
         "-DDEFAULT_COLLATION=utf8_general_ci"
         "-DMYSQL_DATADIR=/var/lib/mysql"
         "-DMYSQL_UNIX_ADDR=/run/mysqld/mysqld.sock"
         "-DINSTALL_INFODIR=share/mysql/docs"
         "-DINSTALL_MANDIR=share/man"
         "-DINSTALL_PLUGINDIR=lib/mysql/plugin"
         "-DINSTALL_SCRIPTDIR=bin"
         "-DINSTALL_INCLUDEDIR=include/mysql"
         "-DINSTALL_DOCREADMEDIR=share/mysql/docs"
         "-DINSTALL_SUPPORTFILESDIR=share/mysql/support-files"
         "-DINSTALL_MYSQLSHAREDIR=share/mysql"
         "-DINSTALL_DOCDIR=share/mysql/docs"
         "-DINSTALL_SHAREDIR=share/mysql")
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'pre-configure
          (lambda _
            (setenv "CONFIG_SHELL" (which "sh"))
            #t))
         (add-after
          'install 'post-install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out     (assoc-ref outputs "out"))
                   (test    (assoc-ref outputs "test")))
              (substitute* (string-append out "/bin/mysql_install_db")
                (("basedir=\"\"")
                 (string-append "basedir=\"" out "\"")))
              ;; Remove unneeded files for testing.
              (with-directory-excursion out
                (for-each delete-file-recursively
                          '("data" "mysql-test" "sql-bench"
                            "share/man/man1/mysql-test-run.pl.1")))))))))
    (native-inputs
     `(("bison" ,bison)
       ("perl" ,perl)))
    (inputs
     `(("jemalloc" ,jemalloc)
       ("libaio" ,libaio)
       ("libxml2" ,libxml2)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("pcre" ,pcre)
       ("zlib" ,zlib)))
    (home-page "https://mariadb.org/")
    (synopsis "SQL database server")
    (description
     "MariaDB is a multi-user and multi-threaded SQL database server, designed
as a drop-in replacement of MySQL.")
    (license license:gpl2)))

(define-public postgresql
  (package
    (name "postgresql")
    (version "10.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.postgresql.org/pub/source/v"
                                  version "/postgresql-" version ".tar.bz2"))
              (sha256
               (base32
                "04z7lm4h94625vbncwv98svycqr942n3q47ailqaczkszqjlxjrw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--with-uuid=e2fs")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-/bin/sh
                     (lambda _
                       ;; Refer to the actual shell.
                       (substitute* '("src/bin/pg_ctl/pg_ctl.c"
                                      "src/bin/psql/command.c")
                         (("/bin/sh") (which "sh")))
                       #t))
         (add-after 'build 'build-contrib
           (lambda _
             (zero? (system* "make" "-C" "contrib"))))
         (add-after 'install 'install-contrib
           (lambda _
             (zero? (system* "make" "-C" "contrib" "install")))))))
    (inputs
     `(("readline" ,readline)
       ("libuuid" ,util-linux)
       ("zlib" ,zlib)))
    (home-page "https://www.postgresql.org/")
    (synopsis "Powerful object-relational database system")
    (description
     "PostgreSQL is a powerful object-relational database system.  It is fully
ACID compliant, has full support for foreign keys, joins, views, triggers, and
stored procedures (in multiple languages).  It includes most SQL:2008 data
types, including INTEGER, NUMERIC, BOOLEAN, CHAR, VARCHAR, DATE, INTERVAL, and
TIMESTAMP.  It also supports storage of binary large objects, including
pictures, sounds, or video.")
    (license (license:x11-style "file://COPYRIGHT"))))

(define-public postgresql-9.6
  (package
    (inherit postgresql)
    (name "postgresql")
    (version "9.6.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.postgresql.org/pub/source/v"
                                  version "/postgresql-" version ".tar.bz2"))
              (sha256
               (base32
                "0m417h30s18rwa7yzkqqcdb22ifpcda2fpg2cyx8bxvjp3ydz71r"))))))

(define-public qdbm
  (package
    (name "qdbm")
    (version "1.8.78")
    (source
      (origin
       (method url-fetch)
       (uri (string-append "http://fallabs.com/" name "/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "0gmpvhn02pkq280ffmn4da1g4mdr1xxz7l80b7y4n7km1mrzwrml"))))
    (build-system gnu-build-system)
    (arguments
     `( #:configure-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                               (assoc-ref %outputs "out")
                                               "/lib"))))
    (home-page "http://fallabs.com/qdbm")
    (synopsis "Key-value database")
    (description "QDBM is a library of routines for managing a
database.  The database is a simple data file containing key-value
pairs.  Every key and value is serial bytes with variable length.
Binary data as well as character strings can be used as a key or a
value.  There is no concept of data tables or data types.  Records are
organized in a hash table or B+ tree.")
    (license license:lgpl2.1+)))

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
    (arguments '(#:parallel-tests? #f
                 #:configure-flags
                 (list (string-append "--with-bash-headers="
                                      (assoc-ref %build-inputs "bash:include")
                                      "/include/bash"))

                 #:phases (modify-phases %standard-phases
                            (add-before 'build 'set-bash4.4-header-location
                              (lambda _
                                (substitute* "bash/Makefile.in"
                                  ;; Adjust the header search path for Bash
                                  ;; 4.4 in accordance with 'bash.pc'.
                                  (("AM_CPPFLAGS = (.*)$" _ rest)
                                   (string-append "AM_CPPFLAGS = "
                                                  "-I$(BASH_HEADERS)/include "
                                                  rest))

                                  ;; Install to PREFIX/lib/bash to match Bash
                                  ;; 4.4's search path.
                                  (("^libdir = .*$")
                                   "libdir = @libdir@/bash\n"))
                                #t)))))

    (native-inputs `(("emacs" ,emacs-minimal)
                     ("bc" ,bc)
                     ("bash:include" ,bash "include")
                     ("libuuid" ,util-linux)))

    ;; TODO: Add more optional inputs.
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
    (license license:gpl3+)
    (home-page "https://www.gnu.org/software/recutils/")))

(define-public rocksdb
  (package
    (name "rocksdb")
    (version "5.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/facebook/rocksdb"
                                  "/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1v2q05bl56sfp51m09z7g6489hkfq4vf6b4qgfg3d96ylgmay9yb"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; TODO: unbundle gtest.
                  (delete-file "build_tools/gnu_parallel")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CC=gcc"
                          (string-append "INSTALL_PATH="
                                         (assoc-ref %outputs "out")))
       ;; Many tests fail on 32-bit platforms. There are multiple reports about
       ;; this upstream, but it's not going to be supported any time soon.
       #:tests? (let ((system ,(or (%current-target-system)
                                   (%current-system))))
                  (or (string-prefix? "x86_64-linux" system)
                      (string-prefix? "aarch64-linux" system)))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-Makefile
           (lambda _
             (substitute* "Makefile"
               (("build_tools/gnu_parallel") "parallel")
               ;; Don't depend on the static library when installing.
               (("install: install-static")
                "install: install-shared")
               (("#!/bin/sh") (string-append "#!" (which "sh"))))
             #t))
         (delete 'configure)
         ;; The default target is only needed for tests and built on demand.
         (delete 'build)
         (add-before 'check 'disable-optimizations
           (lambda _
             ;; Prevent the build from passing '-march=native' to the compiler.
             (setenv "PORTABLE" "1")
             #t))
         (add-before 'check 'disable-failing-tests
           (lambda _
             (substitute* "Makefile"
               ;; This test fails with GCC-5 and is unmaintained.
               ;; https://github.com/facebook/rocksdb/issues/2148
               (("^[[:blank:]]+spatial_db_test[[:blank:]]+\\\\") "\\")
               ;; These tests reliably fail due to "Too many open files".
               (("^[[:blank:]]+env_test[[:blank:]]+\\\\") "\\")
               (("^[[:blank:]]+persistent_cache_test[[:blank:]]+\\\\") "\\"))
             #t))
         (add-after 'check 'build-release-libraries
           ;; The default build target is a debug build for tests. The
           ;; install target depends on "shared_lib" and "static_lib"
           ;; targets for release builds so we build them here for clarity.
           ;; TODO: Add debug output.
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             (zero? (apply system* "make" "shared_lib" make-flags)))))))
    (native-inputs
     `(("parallel" ,parallel)
       ("perl" ,perl)
       ("procps" ,procps)
       ("python" ,python-2)
       ("which" ,which)))
    (inputs
     `(("bzip2" ,bzip2)
       ("gflags" ,gflags)
       ("jemalloc" ,jemalloc)
       ("lz4" ,lz4)
       ("snappy" ,snappy)
       ("zlib" ,zlib)))
    (home-page "http://rocksdb.org/")
    (synopsis "Persistent key-value store for fast storage")
    (description
     "RocksDB is a library that forms the core building block for a fast
key-value server, especially suited for storing data on flash drives.  It
has a @dfn{Log-Structured-Merge-Database} (LSM) design with flexible tradeoffs
between @dfn{Write-Amplification-Factor} (WAF), @dfn{Read-Amplification-Factor}
(RAF) and @dfn{Space-Amplification-Factor} (SAF).  It has multi-threaded
compactions, making it specially suitable for storing multiple terabytes of
data in a single database.  RocksDB is partially based on @code{LevelDB}.")
    ;; RocksDB is BSD-3 and the JNI adapter is Apache 2.0.
    (license (list license:bsd-3 license:asl2.0))))

(define-public sparql-query
  (package
    (name "sparql-query")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/tialaramex/"
                                  name "/archive/" version ".tar.gz"))
              (sha256
               (base32 "0yq3k20472rv8npcc420q9ab6idy584g5y0q501d360k5q0ggr8w"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (inputs
     `(("readline" ,readline)
       ("ncurses" ,ncurses)
       ("glib" ,glib)
       ("libxml2" ,libxml2)
       ("curl" ,curl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:make-flags '("CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;; The Makefile uses git to obtain versioning information. This phase
         ;; substitutes the git invocation with the package version.
         (add-after 'unpack 'remove-git-dependency
           (lambda _
             (substitute* "Makefile"
               (("^gitrev :=.*$")
                (string-append "gitrev = \"v" ,version "\"")))))
         ;; The install phase of the Makefile assumes $PREFIX/usr/local/bin.
         ;; This replacement does the same thing, except for using $PREFIX/bin
         ;; instead.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "sparql-query" bin)
               (system* "ln" "--symbolic"
                        (string-append bin "/sparql-query")
                        (string-append bin "/sparql-update")))))
         (replace 'check
           (lambda* (#:key make-flags #:allow-other-keys)
             (and
              (zero? (apply system* "make" `(,@make-flags "scan-test")))
              (zero? (system "./scan-test"))))))))
    (home-page "https://github.com/tialaramex/sparql-query/")
    (synopsis "Command-line tool for accessing SPARQL endpoints over HTTP")
    (description "Sparql-query is a command-line tool for accessing SPARQL
endpoints over HTTP.  It has been intentionally designed to 'feel' similar to
tools for interrogating SQL databases.  For example, you can enter a query over
several lines, using a semi-colon at the end of a line to indicate the end of
your query.  It also supports readline so that you can more easily recall and
edit previous queries, even across sessions.  It can be used non-interactively,
for example from a shell script.")
    ;; Some files (like scan-sparql.c) contain a GPLv3+ license header, while
    ;; others (like sparql-query.c) contain a GPLv2+ license header.
    (license (list license:gpl3+))))

(define-public sqlite
  (package
   (name "sqlite")
   (version "3.19.3")
   (source (origin
            (method url-fetch)
            (uri (let ((numeric-version
                        (match (string-split version #\.)
                          ((first-digit other-digits ...)
                           (string-append first-digit
                                          (string-pad-right
                                           (string-concatenate
                                            (map (cut string-pad <> 2 #\0)
                                                 other-digits))
                                           6 #\0))))))
                   (string-append "https://sqlite.org/2017/sqlite-autoconf-"
                                  numeric-version ".tar.gz")))
            (sha256
             (base32
              "00b3l2qglpl1inx21fckiwxnfq5xf6441flc79rqg7zdvh1rq4h6"))))
   (build-system gnu-build-system)
   (inputs `(("readline" ,readline)))
   (arguments
    `(#:configure-flags
      ;; Add -DSQLITE_SECURE_DELETE, -DSQLITE_ENABLE_UNLOCK_NOTIFY and
      ;; -DSQLITE_ENABLE_DBSTAT_VTAB to CFLAGS.  GNU Icecat will refuse
      ;; to use the system SQLite unless these options are enabled.
      (list (string-append "CFLAGS=-O2 -DSQLITE_SECURE_DELETE "
                           "-DSQLITE_ENABLE_UNLOCK_NOTIFY "
                           "-DSQLITE_ENABLE_DBSTAT_VTAB"))))
   (home-page "https://www.sqlite.org/")
   (synopsis "The SQLite database management system")
   (description
    "SQLite is a software library that implements a self-contained, serverless,
zero-configuration, transactional SQL database engine.  SQLite is the most
widely deployed SQL database engine in the world.  The source code for SQLite
is in the public domain.")
   (license license:public-domain)))

;; This is used by Clementine.
(define-public sqlite-with-fts3
  (package (inherit sqlite)
    (name "sqlite-with-fts3")
    (arguments
     (substitute-keyword-arguments (package-arguments sqlite)
       ((#:configure-flags flags)
        `(list (string-append "CFLAGS=-O2 -DSQLITE_SECURE_DELETE "
                              "-DSQLITE_ENABLE_UNLOCK_NOTIFY "
                              "-DSQLITE_ENABLE_DBSTAT_VTAB "
                              "-DSQLITE_ENABLE_FTS3 "
                              "-DSQLITE_ENABLE_FTS3_PARENTHESIS "
                              "-DSQLITE_ENABLE_FTS3_TOKENIZER")))))))

(define-public tdb
  (package
    (name "tdb")
    (version "1.3.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.samba.org/ftp/tdb/tdb-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0a37jhpij8wr4f4pjqdlwnffy2l6a2vkqdpz1bqxj6v06cwbz8dl"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; The 'configure' script is a wrapper for Waf and
               ;; doesn't recognize things like '--enable-fast-install'.
               (zero? (system* "./configure"
                               (string-append "--prefix=" out)))))))))
    (native-inputs
     `(;; TODO: Build the documentation.
       ;; ("docbook-xsl" ,docbook-xsl)
       ;; ("libxml2" ,libxml2)
       ;; ("libxslt" ,libxslt)
       ("python" ,python-2)))                     ;for the Waf build system
    (home-page "https://tdb.samba.org/")
    (synopsis "Trivial database")
    (description
     "TDB is a Trivial Database.  In concept, it is very much like GDBM,
and BSD's DB except that it allows multiple simultaneous writers and uses
locking internally to keep writers from trampling on each other.  TDB is also
extremely small.")
    (license license:lgpl3+)))

(define-public perl-dbi
  (package
    (name "perl-dbi")
    (version "1.637")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/T/TI/TIMB/DBI-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1ikbsb6sb0bd2m1dqknl4vx7ikmnd41y0xq8np1l40g8jcjp2mr5"))))
    (build-system perl-build-system)
    (synopsis "Database independent interface for Perl")
    (description "This package provides an database interface for Perl.")
    (home-page "http://search.cpan.org/dist/DBI")
    (license license:perl-license)))

(define-public perl-dbix-class
  (package
    (name "perl-dbix-class")
    (version "0.082840")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RI/RIBASUSHI/"
                           "DBIx-Class-" version ".tar.gz"))
       (sha256
        (base32
         "1vw1f756g8m5hq11nqf5dk2cw2y4mqq91ca5p75fn5g3fp8syja0"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-dbd-sqlite" ,perl-dbd-sqlite)
       ("perl-file-temp" ,perl-file-temp)
       ("perl-module-install" ,perl-module-install)
       ("perl-package-stash" ,perl-package-stash)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-test-exception" ,perl-test-exception)
       ("perl-test-warn" ,perl-test-warn)))
    (propagated-inputs
     `(("perl-class-accessor-grouped" ,perl-class-accessor-grouped)
       ("perl-class-c3-componentised" ,perl-class-c3-componentised)
       ("perl-class-inspector" ,perl-class-inspector)
       ("perl-config-any" ,perl-config-any)
       ("perl-context-preserve" ,perl-context-preserve)
       ("perl-data-dumper-concise" ,perl-data-dumper-concise)
       ("perl-data-page" ,perl-data-page)
       ("perl-dbi" ,perl-dbi)
       ("perl-devel-globaldestruction" ,perl-devel-globaldestruction)
       ("perl-hash-merge" ,perl-hash-merge)
       ("perl-module-find" ,perl-module-find)
       ("perl-moo" ,perl-moo)
       ("perl-mro-compat" ,perl-mro-compat)
       ("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-path-class" ,perl-path-class)
       ("perl-scalar-list-utils" ,perl-scalar-list-utils)
       ("perl-scope-guard" ,perl-scope-guard)
       ("perl-sql-abstract" ,perl-sql-abstract)
       ("perl-sub-name" ,perl-sub-name)
       ("perl-text-balanced" ,perl-text-balanced)
       ("perl-try-tiny" ,perl-try-tiny)))
    (home-page "http://search.cpan.org/dist/DBIx-Class")
    (synopsis "Extensible and flexible object <-> relational mapper")
    (description "An SQL to OO mapper with an object API inspired by
Class::DBI (with a compatibility layer as a springboard for porting) and a
resultset API that allows abstract encapsulation of database operations.  It
aims to make representing queries in your code as perl-ish as possible while
still providing access to as many of the capabilities of the database as
possible, including retrieving related records from multiple tables in a
single query, \"JOIN\", \"LEFT JOIN\", \"COUNT\", \"DISTINCT\", \"GROUP BY\",
\"ORDER BY\" and \"HAVING\" support.")
    (license license:perl-license)))

(define-public perl-dbix-class-cursor-cached
  (package
    (name "perl-dbix-class-cursor-cached")
    (version "1.001004")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AR/ARCANEZ/"
                           "DBIx-Class-Cursor-Cached-" version ".tar.gz"))
       (sha256
        (base32
         "09b2jahn2x12qm4f7qm1jzsxbz7qn1czp6a3fnl5l2i3l4r5421p"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-cache-cache" ,perl-cache-cache)
       ("perl-dbd-sqlite" ,perl-dbd-sqlite)
       ("perl-module-install" ,perl-module-install)))
    (propagated-inputs
     `(("perl-carp-clan" ,perl-carp-clan)
       ("perl-dbix-class" ,perl-dbix-class)))
    (home-page "http://search.cpan.org/dist/DBIx-Class-Cursor-Cached")
    (synopsis "Cursor with built-in caching support")
    (description "DBIx::Class::Cursor::Cached provides a cursor class with
built-in caching support.")
    (license license:perl-license)))

(define-public perl-dbix-class-introspectablem2m
  (package
    (name "perl-dbix-class-introspectablem2m")
    (version "0.001002")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IL/ILMARI/"
                           "DBIx-Class-IntrospectableM2M-" version ".tar.gz"))
       (sha256
        (base32
         "1w47rh2241iy5x3a9bqsyd5kdp9sk43dksr99frzv4qn4jsazfn6"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)))
    (propagated-inputs
     `(("perl-dbix-class" ,perl-dbix-class)))
    (home-page "http://search.cpan.org/dist/DBIx-Class-IntrospectableM2M")
    (synopsis "Introspect many-to-many relationships")
    (description "Because the many-to-many relationships are not real
relationships, they can not be introspected with DBIx::Class.  Many-to-many
relationships are actually just a collection of convenience methods installed
to bridge two relationships.  This DBIx::Class component can be used to store
all relevant information about these non-relationships so they can later be
introspected and examined.")
    (license license:perl-license)))

(define-public perl-dbix-class-schema-loader
  (package
    (name "perl-dbix-class-schema-loader")
    (version "0.07047")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IL/ILMARI/"
                           "DBIx-Class-Schema-Loader-" version ".tar.gz"))
       (sha256
        (base32
         "06s2q6xj95600sdlfph57spjk2z1gjs4zwq5b7mz7d5izcxgnwb6"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-config-any" ,perl-config-any)
       ("perl-config-general" ,perl-config-general)
       ("perl-dbd-sqlite" ,perl-dbd-sqlite)
       ("perl-dbix-class-introspectablem2m" ,perl-dbix-class-introspectablem2m)
       ("perl-module-install" ,perl-module-install)
       ("perl-moose" ,perl-moose)
       ("perl-moosex-markasmethods" ,perl-moosex-markasmethods)
       ("perl-moosex-nonmoose" ,perl-moosex-nonmoose)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-test-differences" ,perl-test-differences)
       ("perl-test-exception" ,perl-test-exception)
       ("perl-test-pod" ,perl-test-pod)
       ("perl-test-warn" ,perl-test-warn)))
    (propagated-inputs
     `(("perl-class-unload" ,perl-class-unload)
       ("perl-class-inspector" ,perl-class-inspector)
       ("perl-class-accessor-grouped" ,perl-class-accessor-grouped)
       ("perl-class-c3-componentised" ,perl-class-c3-componentised)
       ("perl-carp-clan" ,perl-carp-clan)
       ("perl-data-dump" ,perl-data-dump)
       ("perl-dbix-class" ,perl-dbix-class)
       ("perl-hash-merge" ,perl-hash-merge)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-lingua-en-inflect-phrase" ,perl-lingua-en-inflect-phrase)
       ("perl-lingua-en-inflect-number" ,perl-lingua-en-inflect-number)
       ("perl-lingua-en-tagger" ,perl-lingua-en-tagger)
       ("perl-namespace-clean" ,perl-namespace-clean)
       ("perl-mro-compat" ,perl-mro-compat)
       ("perl-scope-guard" ,perl-scope-guard)
       ("perl-string-camelcase" ,perl-string-camelcase)
       ("perl-string-toidentifier-en" ,perl-string-toidentifier-en)
       ("perl-sub-name" ,perl-sub-name)
       ("perl-try-tiny" ,perl-try-tiny)))
    (arguments `(#:tests? #f))          ;TODO: t/20invocations.t fails
    (home-page "http://search.cpan.org/dist/DBIx-Class-Schema-Loader")
    (synopsis "Create a DBIx::Class::Schema based on a database")
    (description "DBIx::Class::Schema::Loader automates the definition of a
DBIx::Class::Schema by scanning database table definitions and setting up the
columns, primary keys, unique constraints and relationships.")
    (license license:perl-license)))

(define-public perl-dbd-pg
  (package
    (name "perl-dbd-pg")
    (version "3.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TU/TURNSTEP/"
                           "DBD-Pg-" version ".tar.gz"))
       (sha256
        (base32
         "0nb4wmkhq1q9f4g42sxy1m3d0xjqd3plqkxpmzni43ygr5ch8vp3"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-dbi" ,perl-dbi)))
    (propagated-inputs
     `(("perl-dbi" ,perl-dbi)
       ("postgresql" ,postgresql)))
    (home-page "http://search.cpan.org/dist/DBD-Pg")
    (synopsis "DBI PostgreSQL interface")
    (description "This package provides a PostgreSQL driver for the Perl5
@dfn{Database Interface} (DBI).")
    (license license:perl-license)))

(define-public perl-dbd-mysql
  (package
    (name "perl-dbd-mysql")
    (version "4.043")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MICHIELB/"
                           "DBD-mysql-" version ".tar.gz"))
       (sha256
        (base32
         "16bg7l28n65ngi1abjxvwk906a80i2vd5vzjn812dx8phdg8d7v2"))
       (patches (search-patches "perl-dbd-mysql-CVE-2017-10788.patch"))))
    (build-system perl-build-system)
    ;; Tests require running MySQL server
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("perl-dbi" ,perl-dbi)
       ("mysql" ,mysql)))
    (home-page "http://search.cpan.org/dist/DBD-mysql")
    (synopsis "DBI MySQL interface")
    (description "This package provides a MySQL driver for the Perl5
@dfn{Database Interface} (DBI).")
    (license license:perl-license)))

(define-public perl-dbd-sqlite
  (package
    (name "perl-dbd-sqlite")
    (version "1.54")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/I/IS/ISHIGAKI/DBD-SQLite-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0sbj9lx9syzpknvjv8cz9jndg32qz775vy2prgq305npv3dsca9r"))))
    (build-system perl-build-system)
    (inputs `(("sqlite" ,sqlite)))
    (propagated-inputs `(("perl-dbi" ,perl-dbi)))
    (synopsis "SQlite interface for Perl")
    (description "DBD::SQLite is a Perl DBI driver for SQLite, that includes
the entire thing in the distribution.  So in order to get a fast transaction
capable RDBMS working for your Perl project you simply have to install this
module, and nothing else.")
    (license license:perl-license)
    (home-page "http://search.cpan.org/~ishigaki/DBD-SQLite/lib/DBD/SQLite.pm")))

(define-public perl-sql-abstract
  (package
    (name "perl-sql-abstract")
    (version "1.84")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IL/ILMARI/"
                           "SQL-Abstract-" version ".tar.gz"))
       (sha256
        (base32
         "0xayvgv6nic61jm3nhg41rzwgm8h83wfyazvpaks0z7asjillpv5"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-install" ,perl-module-install)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-test-exception" ,perl-test-exception)
       ("perl-test-warn" ,perl-test-warn)))
    (propagated-inputs
     `(("perl-hash-merge" ,perl-hash-merge)
       ("perl-moo" ,perl-moo)
       ("perl-mro-compat" ,perl-mro-compat)
       ("perl-text-balanced" ,perl-text-balanced)))
    (home-page "http://search.cpan.org/dist/SQL-Abstract")
    (synopsis "Generate SQL from Perl data structures")
    (description "This module was inspired by the excellent DBIx::Abstract.
While based on the concepts used by DBIx::Abstract, the concepts used have
been modified to make the SQL easier to generate from Perl data structures.
The underlying idea is for this module to do what you mean, based on the data
structures you provide it, so that you don't have to modify your code every
time your data changes.")
    (license license:perl-license)))

(define-public perl-sql-splitstatement
  (package
    (name "perl-sql-splitstatement")
    (version "1.00020")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/EM/EMAZEP/"
                           "SQL-SplitStatement-" version ".tar.gz"))
       (sha256
        (base32
         "0bqg45k4c9qkb2ypynlwhpvzsl4ssfagmsalys18s5c79ps30z7p"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-exception" ,perl-test-exception)))
    (propagated-inputs
     `(("perl-class-accessor" ,perl-class-accessor)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-regexp-common" ,perl-regexp-common)
       ("perl-sql-tokenizer" ,perl-sql-tokenizer)))
    (home-page "http://search.cpan.org/dist/SQL-SplitStatement")
    (synopsis "Split SQL code into atomic statements")
    (description "This module tries to split any SQL code, even including
non-standard extensions, into the atomic statements it is composed of.")
    (license license:perl-license)))

(define-public perl-sql-tokenizer
  (package
    (name "perl-sql-tokenizer")
    (version "0.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IZ/IZUT/"
                           "SQL-Tokenizer-" version ".tar.gz"))
       (sha256
        (base32
         "1qa2dfbzdlr5qqdam9yn78z5w3al5r8577x06qan8wv58ay6ka7s"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/SQL-Tokenizer")
    (synopsis "SQL tokenizer")
    (description "SQL::Tokenizer is a tokenizer for SQL queries.  It does not
claim to be a parser or query verifier.  It just creates sane tokens from a
valid SQL query.")
    (license license:perl-license)))

(define-public unixodbc
  (package
   (name "unixodbc")
   (version "2.3.4")
   (source (origin
            (method url-fetch)
            (uri
             (string-append
              "ftp://ftp.unixodbc.org/pub/unixODBC/unixODBC-"
              version ".tar.gz"))
            (sha256
             (base32 "0f8y88rcc2akjvjv5y66yx7k0ms9h1s0vbcfy25j93didflhj59f"))))
   (build-system gnu-build-system)
   (synopsis "Data source abstraction library")
   (description "Unixodbc is a library providing an API with which to access
data sources.  Data sources include SQL Servers and any software with an ODBC
Driver.")
   (license license:lgpl2.1+)
   ;; COPYING contains copy of lgpl2.1 - but copyright notices just say "LGPL"
   (home-page "http://www.unixodbc.org")))

(define-public unqlite
  (package
    (name "unqlite")
    (version "1.1.6")
    (source (origin
              (method url-fetch)
              ;; Contains bug fixes against the official release, and has an
              ;; autotooled build system.
              (uri (string-append "https://github.com/aidin36/tocc/releases/"
                                  "download/v1.0.0/"
                                  "unqlite-unofficial-" version ".tar.gz"))
              (sha256
               (base32
                "1sbpvhg15gadq0mpcy16q7k3rkg4b4dicpnn5xifpkpn02sqik3s"))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f))          ;No check target
    (home-page "http://www.unqlite.org")
    (synopsis "In-memory key/value and document store")
    (description
     "UnQLite is an in-process software library which implements a
self-contained, serverless, zero-configuration, transactional NoSQL
database engine.  UnQLite is a document store database similar to
MongoDB, Redis, CouchDB, etc. as well as a standard Key/Value store
similar to BerkeleyDB, LevelDB, etc.")
    (license license:bsd-2)))

(define-public redis
  (package
    (name "redis")
    (version "4.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.redis.io/releases/redis-"
                                  version".tar.gz"))
              (sha256
               (base32
                "04s8cgvwjj1979s3hg8zkwc9pyn3jkjpz5zidp87kfcipifr385i"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; tests related to master/slave and replication fail
       #:phases (modify-phases %standard-phases
                  (delete 'configure))
       #:make-flags `("CC=gcc"
                      "MALLOC=libc"
                      "LDFLAGS=-ldl"
                      ,(string-append "PREFIX="
                                      (assoc-ref %outputs "out")))))
    (synopsis "Key-value cache and store")
    (description "Redis is an advanced key-value cache and store.  Redis
supports many data structures including strings, hashes, lists, sets, sorted
sets, bitmaps and hyperloglogs.")
    (home-page "http://redis.io/")
    (license license:bsd-3)))

(define-public kyotocabinet
  (package
    (name "kyotocabinet")
    (version "1.2.76")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://fallabs.com/kyotocabinet/pkg/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0g6js20x7vnpq4p8ghbw3mh9wpqksya9vwhzdx6dnlf354zjsal1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--disable-opt" ;"-march=native". XXX this also turns off -O0.
        (string-append "LDFLAGS=-Wl,-rpath="
                       (assoc-ref %outputs "out") "/lib"))))
    (inputs `(("zlib" ,zlib)))
    (home-page "http://fallabs.com/kyotocabinet/")
    (synopsis
     "Kyoto Cabinet is a modern implementation of the DBM database")
    (description
     "Kyoto Cabinet is a standalone file-based database that supports Hash
and B+ Tree data storage models.  It is a fast key-value lightweight
database and supports many programming languages.  It is a NoSQL database.")
    (license license:gpl3+)))

(define-public tokyocabinet
  (package
    (name "tokyocabinet")
    (version "1.4.48")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://fallabs.com/tokyocabinet/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "140zvr0n8kvsl0fbn2qn3f2kh3yynfwnizn4dgbj47m975yg80x0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-pthread" "--enable-off64" "--enable-fastest"
        (string-append "LDFLAGS=-Wl,-rpath="
                       (assoc-ref %outputs "out") "/lib"))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "http://fallabs.com/tokyocabinet/")
    (synopsis "Tokyo Cabinet is a modern implementation of the DBM database")
    (description
     "Tokyo Cabinet is a library of routines for managing a database.
The database is a simple data file containing records, each is a pair of a
key and a value.  Every key and value is serial bytes with variable length.
Both binary data and character string can be used as a key and a value.
There is neither concept of data tables nor data types.  Records are
organized in hash table, B+ tree, or fixed-length array.")
    (license license:lgpl2.1+)))

(define-public wiredtiger
  (package
    (name "wiredtiger")
    (version "2.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://source.wiredtiger.com/releases/wiredtiger-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0krwnb2zfbhvjaskwl875qzd3y626s84zcciq2mxr5c5riw3yh6s"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-lz4" "--with-builtins=snappy,zlib")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'disable-test/fops
           (lambda _
             ;; XXX: timed out after 3600 seconds of silence
             (substitute* "Makefile"
               (("test/fops") ""))
             #t)))))
    (inputs
     `(("lz4" ,lz4)
       ("zlib" ,zlib)
       ("snappy" ,snappy)))
    (home-page "http://source.wiredtiger.com/")
    (synopsis "NoSQL data engine")
    (description
     "WiredTiger is an extensible platform for data management.  It supports
row-oriented storage (where all columns of a row are stored together),
column-oriented storage (where columns are stored in groups, allowing for
more efficient access and storage of column subsets) and log-structured merge
trees (LSM), for sustained throughput under random insert workloads.")
    (license license:gpl3) ; or GPL-2
    ;; configure.ac: WiredTiger requires a 64-bit build.
    (supported-systems '("x86_64-linux" "mips64el-linux" "aarch64-linux"))))

(define-public guile-wiredtiger
  (package
    (name "guile-wiredtiger")
    (version "20171113.6cbc51da")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://framagit.org/a-guile-mind/guile-wiredtiger.git")
                    (commit "6cbc51dab95d28fe31ae025fbdd88f3ecbf2111b")))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0x3qwpgch5pg0k21kc792h4y6b36a8xd1zkfq8ar2l2mqmpzkzyd"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:configure-flags
       (list (string-append "--with-libwiredtiger-prefix="
                            (assoc-ref %build-inputs "wiredtiger")))
       #:make-flags '("GUILE_AUTO_COMPILE=0")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'bootstrap
           (lambda _
             (zero? (system* "sh" "bootstrap"))))
         (add-before 'bootstrap 'remove-bundled-dependencies
           (lambda _
             ;; TODO: Remove microkanren.scm when we have a separate package
             ;; for it.
             (delete-file "htmlprag.scm")
             (substitute* "Makefile.am"
               (("htmlprag\\.scm") ""))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("wiredtiger" ,wiredtiger)
       ("guile" ,guile-2.2)))
    (propagated-inputs
     `(("guile-lib" ,guile-lib)))                 ;for (htmlprag)
    (synopsis "Wired Tiger bindings for GNU Guile")
    (description
     "This package provides Guile bindings to the WiredTiger ``NoSQL''
database.")
    (home-page "https://framagit.org/a-guile-mind/guile-wiredtiger")
    (license license:gpl3+)))

(define-public perl-db-file
 (package
  (name "perl-db-file")
  (version "1.840")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/P/PM/PMQS/DB_File-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "1i5jz85z4hpx15lw6ix27pyvrf0ziyh4z33lii4d3wnhz83lg1mp"))))
  (build-system perl-build-system)
  (inputs `(("bdb" ,bdb)))
  (native-inputs `(("perl-test-pod" ,perl-test-pod)))
  (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before
                   'configure 'modify-config.in
                   (lambda* (#:key inputs #:allow-other-keys)
                     (substitute* "config.in"
                       (("/usr/local/BerkeleyDB") (assoc-ref inputs "bdb")))
                     #t)))))
  (home-page "http://search.cpan.org/dist/DB_File")
  (synopsis
    "Perl5 access to Berkeley DB version 1.x")
  (description
    "The DB::File module provides Perl bindings to the Berkeley DB version 1.x.")
  (license license:perl-license)))

(define-public lmdb
  (package
    (name "lmdb")
    (version "0.9.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/LMDB/lmdb/archive/"
                                  "LMDB_" version ".tar.gz"))
              (sha256
               (base32
                "0ndmj07hkm2ic60z1f4rdscxs7pq45hk9fibjyv5nhfclhsvd1qi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (chdir (string-append
               (getenv "PWD") "/lmdb-LMDB_" ,version "/libraries/liblmdb"))
             (substitute* "Makefile"
               (("/usr/local") (assoc-ref outputs "out")))
            #t)))))
    (home-page "https://symas.com/products/lightning-memory-mapped-database")
    (synopsis "Lightning Memory-Mapped Database library")
    (description
     "The @dfn{Lightning Memory-Mapped Database} (LMDB) is a high-performance
transactional database.  Unlike more complex relational databases, LMDB handles
only key-value pairs (stored as arbitrary byte arrays) and relies on the
underlying operating system for caching and locking, keeping the code small and
simple.
The use of ‘zero-copy’ memory-mapped files combines the persistence of classic
disk-based databases with high read performance that scales linearly over
multiple cores.  The size of each database is limited only by the size of the
virtual address space — not physical RAM.")
    (license license:openldap2.8)))

(define-public libpqxx
  (package
    (name "libpqxx")
    (version "4.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://pqxx.org/download/software/libpqxx/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "0f6wxspp6rx12fkasanb0z2g2gc8dhcfwnxagx8wwqbpg6ifsz09"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python-2)))
    (inputs `(("postgresql" ,postgresql)))
    (arguments
     `(#:tests? #f   ; # FAIL:  1
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-sed-command
           (lambda _
             ;; Newer sed versions error out if double brackets are not used.
             (substitute* "configure"
               (("\\[:space:\\]") "[[:space:]]"))
             #t)))))
    (synopsis "C++ connector for PostgreSQL")
    (description
     "Libpqxx is a C++ library to enable user programs to communicate with the
PostgreSQL database back-end.  The database back-end can be local or it may be
on another machine, accessed via TCP/IP.")
    (home-page "http://pqxx.org/")
    (license license:bsd-3)))

(define-public python-peewee
  (package
    (name "python-peewee")
    (version "2.10.2")
      (source
        (origin
        (method url-fetch)
        (uri (pypi-uri "peewee" version))
        (sha256
         (base32
          "10f2mrd5hw6rjklrzaix2lsxlgc8vx3xak54arcy6yd791zhchi3"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; Fails to import test data
    (native-inputs
     `(("python-cython" ,python-cython)))
    (home-page "https://github.com/coleifer/peewee/")
    (synopsis "Small object-relational mapping utility")
    (description
     "Peewee is a simple and small ORM (object-relation mapping) tool.  Peewee
handles converting between pythonic values and those used by databases, so you
can use Python types in your code without having to worry.  It has built-in
support for sqlite, mysql and postgresql.  If you already have a database, you
can autogenerate peewee models using @code{pwiz}, a model generator.")
    (license license:expat)))

(define-public python2-peewee
  (package-with-python2 python-peewee))

(define-public sqlcipher
  (package
    (name "sqlcipher")
    (version "3.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/sqlcipher/" name
                           "/archive/v" version ".tar.gz"))
       (sha256
        (base32 "1gv58dlbpzrmznly52yqbxgvii0ib88zr3aszla1bsypwjr6flff"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (inputs
     `(("libcrypto" ,openssl)
       ("libtcl8.6" ,tcl))) ; required for running the tests
    (native-inputs
     `(("tcl" ,tcl)))
    (arguments
     '(#:configure-flags
       '("--enable-tempstore=yes"
         "CFLAGS=-DSQLITE_HAS_CODEC -DSQLITE_ENABLE_FTS3"
         "LDFLAGS=-lcrypto -ltcl8.6"
         "--disable-tcl")
       ;; tests cannot be run from the Makefile
       ;; see: <https://github.com/sqlcipher/sqlcipher/issues/172>
       #:test-target "testfixture"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'build-test-runner
           (assoc-ref %standard-phases 'check))
         (replace 'check
           (lambda _
             (zero?
              (system* "./testfixture" "test/crypto.test")))))))
    (home-page "https://www.zetetic.net/sqlcipher/")
    (synopsis
     "Library providing transparent encryption of SQLite database files")
    (description "SQLCipher is an implementation of SQLite, extended to
provide transparent 256-bit AES encryption of database files.  Pages are
encrypted before being written to disk and are decrypted when read back.  It’s
well suited for protecting embedded application databases and for mobile
development.")
    ;; The source files
    ;; src/{crypto.c,crypto_impl.c,crypto.h,crypto_cc.c,crypto_libtomcrypt.c},
    ;; src/{crypto_openssl.c,sqlcipher.h}, tool/crypto-speedtest.tcl,
    ;; test/crypto.test are licensed under a 3-clause BSD license. All other
    ;; source files are in the public domain.
    (license (list license:public-domain license:bsd-3))))

(define-public python-pyodbc-c
  (package
    (name "python-pyodbc-c")
    (version "3.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gitlab.com/daym/pyodbc-c/repository/"
                           "archive.tar.gz?ref=v" version))
       (sha256
        (base32
         "05aq2297k779xidmxcwkrrxjvj1bh2q7d9a1rcjv6zr15y764ga9"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system python-build-system)
    (inputs
     `(("unixodbc" ,unixodbc)))
    (arguments
     `(;; No unit tests exist.
       #:tests? #f))
    (home-page "https://github.com/mkleehammer/pyodbc")
    (synopsis "Python ODBC Library")
    (description "@code{python-pyodbc-c} provides a Python DB-API driver
for ODBC.")
    (license (license:x11-style "file://LICENSE.TXT"))))

(define-public python2-pyodbc-c
  (package-with-python2 python-pyodbc-c))

(define-public python-pyodbc
  (package
    (name "python-pyodbc")
    (version "4.0.21")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyodbc" version))
       (sha256
        (base32
         "0a83zwz3h1agshnsc6r7al6q83222w8601gpzzzjvjz5m56ghmcn"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system python-build-system)
    (inputs
     `(("unixodbc" ,unixodbc)))
    (arguments
     `(;; No unit tests exist.
       #:tests? #f))
    (home-page "https://github.com/mkleehammer/pyodbc")
    (synopsis "Python ODBC Library")
    (description "@code{python-pyodbc} provides a Python DB-API driver
for ODBC.")
    (license (license:x11-style "file:///LICENSE.TXT"))))

(define-public python2-pyodbc
  (package-with-python2 python-pyodbc))

(define-public mdbtools
  (package
    (name "mdbtools")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/brianb/mdbtools/archive/"
                           version ".tar.gz"))
       (sha256
        (base32
         "05hbmxcq173kzb899gdi3bz2qcc1vi3n1qbbkwpsvrq7ggf11wyw"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (inputs
     `(("glib" ,glib)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("txt2man" ,txt2man)
       ("which" ,which)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autoreconf
           (lambda _
             (zero? (system* "autoreconf" "-vfi")))))))
    (home-page "http://mdbtools.sourceforge.net/")
    (synopsis "Read Microsoft Access databases")
    (description "MDB Tools is a set of tools and applications to read the
proprietary MDB file format used in Microsoft's Access database package.  This
includes programs to export schema and data from Microsoft's Access database
file format to other databases such as MySQL, Oracle, Sybase, PostgreSQL,
etc., and an SQL engine for performing simple SQL queries.")
    (license (list license:lgpl2.0
                   license:gpl2+))))

(define-public python-lmdb
  (package
    (name "python-lmdb")
    (version "0.93")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "lmdb" version))
              (sha256
               (base32
                "0xdpb298fyl68acadbwv5801wcwfpnhc7sm4bnrq1x4bd5dhhsql"))
              (modules '((guix build utils)))
              (snippet
               ;; Delete bundled lmdb source files.
               '(for-each delete-file (list "lib/lmdb.h"
                                            "lib/mdb.c"
                                            "lib/midl.c"
                                            "lib/midl.h")))))
    (build-system python-build-system)
    (inputs
     `(("lmdb" ,lmdb)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'use-system-lmdb
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((lmdb (assoc-ref inputs "lmdb")))
               (setenv "LMDB_FORCE_SYSTEM" "set")
               (setenv "LMDB_INCLUDEDIR" (string-append lmdb "/include"))
               (setenv "LMDB_LIBDIR" (string-append lmdb "/lib"))
               #t))))
       ;; Tests fail with: ‘lmdb.tool: Please specify environment (--env)’.
       #:tests? #f))
    (home-page "https://github.com/dw/py-lmdb")
    (synopsis "Python binding for the ‘Lightning’ database (LMDB)")
    (description
     "python-lmdb or py-lmdb is a Python binding for the @dfn{Lightning
Memory-Mapped Database} (LMDB), a high-performance key-value store.")
    (license
     (list license:openldap2.8
           ;; ‘lib/win32/inttypes.h’ and ‘lib/win32-stdint/stdint.h’ are BSD-3,
           ;; but not actually needed on platforms currently supported by Guix.
           license:bsd-3))))

(define-public python2-lmdb
  (package-with-python2 python-lmdb))

(define-public python-orator
  (package
    (name "python-orator")
    (version "0.9.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "orator" version))
              (sha256
               (base32
                "14r58z64fdp76ixnvmi4lni762b405ynmsx6chr1qihs3yl9zn6c"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'loosen-dependencies
           ;; Tests are not actually run since they are not included with the
           ;; distributed package, but dependencies are checked.
           (lambda _
             (substitute* "setup.py"
               ((",<.*'") "'")
               (("flexmock==0.9.7") "flexmock")
               ;; The pytest-mock package is out of date, so we remove minimum
               ;; version requirement.
               (("pytest-mock.*'") "pytest-mock'"))
             #t)))))
    (native-inputs
     `(("python-pytest-mock" ,python-pytest-mock)
       ("python-pytest" ,python-pytest-3.0)
       ("python-flexmock" ,python-flexmock)))
    (propagated-inputs
     `(("python-backpack" ,python-backpack)
       ("python-blinker" ,python-blinker)
       ("python-cleo" ,python-cleo)
       ("python-faker" ,python-faker)
       ("python-inflection" ,python-inflection)
       ("python-lazy-object-proxy" ,python-lazy-object-proxy)
       ("python-pendulum" ,python-pendulum)
       ("python-pyaml" ,python-pyaml)
       ("python-pygments" ,python-pygments)
       ("python-simplejson" ,python-simplejson)
       ("python-six" ,python-six)
       ("python-wrapt" ,python-wrapt)))
    (home-page "https://orator-orm.com/")
    (synopsis "ActiveRecord ORM for Python")
    (description
     "Orator provides a simple ActiveRecord-like Object Relational Mapping
implementation for Python.")
    (license license:expat)
    (properties `((python2-variant . ,(delay python2-orator))))))

(define-public python2-orator
  (package-with-python2 (strip-python2-variant python-orator)))

(define-public virtuoso-ose
  (package
    (name "virtuoso-ose")
    (version "7.2.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/openlink/virtuoso-opensource/releases/"
             "download/v" version "/virtuoso-opensource-" version ".tar.gz"))
       (sha256
        (base32 "12dqam1gc1v93l0bj0vlpvjqppki6y1hqrlznywxnw0rrz9pb002"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ; Tests require a network connection.
    (inputs
     `(("openssl" ,openssl)
       ("net-tools" ,net-tools)))
    (home-page "http://vos.openlinksw.com/owiki/wiki/VOS/")
    (synopsis "Multi-model database system")
    (description "Virtuoso is a scalable cross-platform server that combines
relational, graph, and document data management with web application server
and web services platform functionality.")
    ;; configure: error: ... can only be build on 64bit platforms
    (supported-systems '("x86_64-linux" "mips64el-linux" "aarch64-linux"))
    (license license:gpl2)))

(define-public r-rmysql
  (package
    (name "r-rmysql")
    (version "0.10.13")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RMySQL" version))
       (sha256
        (base32
         "1j0vr2l4s02cg2hzgr3pla96pjj4h85sxw28lidy58rg5awnsf82"))))
    (properties `((upstream-name . "RMySQL")))
    (build-system r-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("mariadb" ,mariadb)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("r-dbi" ,r-dbi)))
    (home-page "https://github.com/r-dbi/RMySQL")
    (synopsis "Database interface and MySQL driver for R")
    (description
     "This package provides a DBI interface to MySQL / MariaDB.  The RMySQL
package contains an old implementation based on legacy code from S-PLUS which
is being phased out.  A modern MySQL client based on Rcpp is available from
the RMariaDB package.")
    (license license:gpl2)))

(define-public python-ccm
  (package
    (name "python-ccm")
    (version "2.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ccm" version))
       (sha256
        (base32
         "177dfxsmk3k4cih6fh6v8d91bh4nqx7ns6pc07w7m7i3cvdx3c8n"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pyyaml" ,python-pyyaml)
       ;; Not listed in setup.py, but used in ccmlib/node.py for full
       ;; functionality
       ("python-psutil" ,python-psutil)
       ("python-six" ,python-six)))
    (home-page "https://github.com/pcmanus/ccm")
    (synopsis "Cassandra Cluster Manager for Apache Cassandra clusters on
localhost")
    (description "Cassandra Cluster Manager is a development tool for testing
local Cassandra clusters. It creates, launches and removes Cassandra clusters
on localhost.")
    (license license:asl2.0)))

(define-public python2-ccm
  (package-with-python2 python-ccm))

(define-public python2-pysqlite
  (package
    (name "python2-pysqlite")
    (version "2.8.3")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "pysqlite" version))
      (sha256
       (base32
        "1424gwq9sil2ffmnizk60q36vydkv8rxs6m7xs987kz8cdc37lqp"))))
    (build-system python-build-system)
    (inputs
     `(("sqlite" ,sqlite)))
    (arguments
     `(#:python ,python-2 ; incompatible with Python 3
       #:tests? #f)) ; no test target
    (home-page "https://github.com/ghaering/pysqlite")
    (synopsis "SQLite bindings for Python")
    (description
     "Pysqlite provides SQLite bindings for Python that comply to the
Database API 2.0T.")
    (license license:zlib)))

(define-public python-sqlalchemy
  (package
    (name "python-sqlalchemy")
    (version "1.0.12")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://pypi.python.org/packages/source/S/"
                          "SQLAlchemy/SQLAlchemy-" version ".tar.gz"))
      (sha256
       (base32
        "1l8qclhd0s90w3pvwhi5mjxdwr5j7gw7cjka2fx6f2vqmq7f4yb6"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-cython" ,python-cython) ;for c extensions
       ("python-pytest" ,python-pytest)
       ("python-mock"   ,python-mock))) ;for tests
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (zero? (system* "py.test")))))))
    (home-page "http://www.sqlalchemy.org")
    (synopsis "Database abstraction library")
    (description
     "SQLAlchemy is the Python SQL toolkit and Object Relational Mapper that
gives application developers the full power and flexibility of SQL.  It
provides a full suite of well known enterprise-level persistence patterns,
designed for efficient and high-performing database access, adapted into a
simple and Pythonic domain language.")
    (license license:x11)))

(define-public python2-sqlalchemy
  (package-with-python2 python-sqlalchemy))

(define-public python-sqlalchemy-utils
  (package
    (name "python-sqlalchemy-utils")
    (version "0.32.21")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "SQLAlchemy-Utils" version))
        (sha256
         (base32
          "1myn71dn8j74xglyh46f12sh8ywb7j0j732rzwq70kvwwnq32m73"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; FIXME: Many tests require a running database server.
       ;; #:phases
       ;; (modify-phases %standard-phases
       ;;   (replace 'check
       ;;     (lambda _
       ;;       (zero? (system* "py.test" "sqlalchemy_utils" "tests")))))
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-sqlalchemy" ,python-sqlalchemy)))
    (native-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-flexmock" ,python-flexmock)
       ("python-psycopg2" ,python-psycopg2)
       ("python-pytest" ,python-pytest)
       ("python-pytz" ,python-pytz)))
    (home-page "https://github.com/kvesteri/sqlalchemy-utils")
    (synopsis "Various utility functions for SQLAlchemy")
    (description
     "SQLAlchemy-utils provides various utility functions and custom data types
for SQLAlchemy.  SQLAlchemy is an SQL database abstraction library for Python.

You might also want to install the following optional dependencies:
@enumerate
@item @code{python-passlib}
@item @code{python-babel}
@item @code{python-cryptography}
@item @code{python-pytz}
@item @code{python-psycopg2}
@item @code{python-furl}
@item @code{python-flask-babel}
@end enumerate
")
    (license license:bsd-3)))

(define-public python2-sqlalchemy-utils
  (package-with-python2 python-sqlalchemy-utils))

(define-public python-alembic
  (package
    (name "python-alembic")
    (version "0.9.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "alembic" version))
       (sha256
        (base32
         "0cm73vabrqj92v7a0wwvldj8j7bc7dwv358kvkk7p87gx7mm2a04"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest-cov" ,python-pytest-cov)))
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-sqlalchemy" ,python-sqlalchemy)
       ("python-mako" ,python-mako)
       ("python-editor" ,python-editor)))
    (home-page "https://bitbucket.org/zzzeek/alembic")
    (synopsis
     "Database migration tool for SQLAlchemy")
    (description
     "Alembic is a lightweight database migration tool for usage with the
SQLAlchemy Database Toolkit for Python.")
    (license license:expat)))

(define-public python2-alembic
  (package-with-python2 python-alembic))

(define-public python-pickleshare
  (package
    (name "python-pickleshare")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pypi.python.org/packages/source/p/"
                           "pickleshare/pickleshare-" version ".tar.gz"))
       (sha256
        (base32 "11ljr90j3p6qswdrbl7p4cjb2i93f6vn0vx9anzpshsx0d2mggn0"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pathpy" ,python-pathpy)))
    (home-page "https://github.com/vivainio/pickleshare")
    (synopsis "Tiny key value database with concurrency support")
    (description
     "PickleShare is a small ‘shelve’-like datastore with concurrency support.
Like shelve, a PickleShareDB object acts like a normal dictionary.  Unlike
shelve, many processes can access the database simultaneously.  Changing a
value in database is immediately visible to other processes accessing the same
database.  Concurrency is possible because the values are stored in separate
files.  Hence the “database” is a directory where all files are governed by
PickleShare.")
    (license license:expat)))

(define-public python2-pickleshare
  (package-with-python2 python-pickleshare))

(define-public python-apsw
  (package
    (name "python-apsw")
    (version "3.9.2-r1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "apsw" version))
        (sha256
          (base32
           "0w4jb0wpx785qw42r3h4fh7gl5w2968q48i7gygybsfxck8nzffs"))))
    (build-system python-build-system)
    (inputs
      `(("sqlite" ,sqlite)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (zero?
              (system* "python" "setup.py" "build" "--enable-all-extensions"))))
         (add-after 'build 'build-test-helper
           (lambda _
             (zero?
              (system
               (string-append "gcc -fPIC -shared -o ./testextension.sqlext "
                              "-I. -Isqlite3 src/testextension.c") ))))
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (zero? (system* "python" "setup.py" "test")))))))
    (home-page "https://github.com/rogerbinns/apsw/")
    (synopsis "Another Python SQLite Wrapper")
    (description "APSW is a Python wrapper for the SQLite
embedded relational database engine.  In contrast to other wrappers such as
pysqlite it focuses on being a minimal layer over SQLite attempting just to
translate the complete SQLite API into Python.")
    (license license:zlib)))

(define-public python2-apsw
  (package-with-python2 python-apsw))

(define-public python2-neo4j-driver
  (package
    (name "python2-neo4j-driver")
    ;; NOTE: When upgrading to 1.5.0, please add a python3 variant.
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "neo4j-driver" version))
              (sha256
               (base32
                "011r1vh182p8mm83d8dz9rfnc3l7rf7fd00cyrbyfzi71jmc4g98"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (home-page "https://neo4j.com/developer/python/")
    (synopsis "Neo4j driver code written in Python")
    (description "This package provides the Neo4j Python driver that connects
to the database using Neo4j's binary protocol.  It aims to be minimal, while
being idiomatic to Python.")
    (license license:asl2.0)))

(define-public python2-py2neo
  (package
    (name "python2-py2neo")
    (version "3.1.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "py2neo" version))
              (sha256
               (base32
                "1f1q95vqcvlc3nsc33p841swnjdcjazddlq2dzi3qfnjqjrajxw1"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (home-page "http://py2neo.org")
    (synopsis "Library and toolkit for working with Neo4j in Python")
    (description "This package provides a client library and toolkit for
working with Neo4j from within Python applications and from the command
line.  The core library has no external dependencies and has been carefully
designed to be easy and intuitive to use.")
    (license license:asl2.0)))

(define-public python-psycopg2
  (package
    (name "python-psycopg2")
    (version "2.7.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "psycopg2" version))
       (sha256
        (base32
         "0rda1j02ds6s28752fhmpwg761sh6jsxi1gpczqkrd28cki1cywv"))))
    (build-system python-build-system)
    (arguments
     ;; Tests would require a postgresql database "psycopg2_test"
     ;; and a running postgresql database management service.
     `(#:tests? #f)) ; TODO re-enable after providing a test-db.
    (inputs
     `(("postgresql" ,postgresql))) ; libpq
    (home-page "http://initd.org/psycopg/")
    (synopsis "Python PostgreSQL adapter")
    (description
     "psycopg2 is a thread-safe PostgreSQL adapter that implements DB-API 2.0. ")
    (license license:lgpl3+)))

(define-public python2-psycopg2
  (package-with-python2 python-psycopg2))

(define-public python-sadisplay
  (package
    (name "python-sadisplay")
    (version "0.4.8")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "sadisplay" version))
      (sha256
        (base32
          "01d9lxhmgpb68gy8rd6zj6fcwp84n2qq210n1qsk3qbsir79bzh4"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-sqlalchemy" ,python-sqlalchemy)))
    (native-inputs
     ;; For tests.
      `(("python-nose" ,python-nose)))
    (home-page "https://bitbucket.org/estin/sadisplay")
    (synopsis "SQLAlchemy schema displayer")
    (description "This package provides a program to build Entity
Relationship diagrams from a SQLAlchemy model (or directly from the
database).")
    (license license:bsd-3)))

(define-public python2-sadisplay
  (package-with-python2 python-sadisplay))

(define-public python-mysqlclient
  (package
    (name "python-mysqlclient")
    (version "1.3.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mysqlclient" version))
       (sha256
        (base32
         "0qkj570x4rbsblji6frvsvp2v1ap32dqzj1lq62zp9515ffsyaj5"))))
    (build-system python-build-system)
    (native-inputs
     `(("mariadb" ,mariadb)
       ("nose" ,python-nose)
       ("mock" ,python-mock)
       ("py.test" ,python-pytest)))
    (inputs
     `(("mysql" ,mysql)
       ("libz" ,zlib)
       ("openssl" ,openssl)))
    (home-page "https://github.com/PyMySQL/mysqlclient-python")
    (synopsis "MySQLdb is an interface to the popular MySQL database server for Python")
    (description "MySQLdb is an interface to the popular MySQL database server
for Python.  The design goals are:
@enumerate
@item Compliance with Python database API version 2.0 [PEP-0249],
@item Thread-safety,
@item Thread-friendliness (threads will not block each other).
@end enumerate")
    (license license:gpl2)))

(define-public python2-mysqlclient
  (package-with-python2 python-mysqlclient))

(define-public python-hiredis
  (package
    (name "python-hiredis")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hiredis" version))
       (sha256
        (base32
         "1dfm2k9l9zar9nw9fwmm74zrgraxdxs04vx9li56fjcf289qx5fa"))))
    (build-system python-build-system)
    (arguments
     ;; no tests
     `(#:tests? #f))
    (home-page "https://github.com/redis/hiredis-py")
    (synopsis "Python extension that wraps protocol parsing code in hiredis")
    (description "Python-hiredis is a python extension that wraps protocol
parsing code in hiredis.  It primarily speeds up parsing of multi bulk replies.")
    (license license:bsd-3)))

(define-public python2-hiredis
  (package-with-python2 python-hiredis))

(define-public python-fakeredis
  (package
    (name "python-fakeredis")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fakeredis" version))
       (sha256
        (base32
         "0zncahj3byyasyfx9i7k991ph0n0lq8v3a21pqri5qxn9564bk9r"))))
    (build-system python-build-system)
    (arguments
     ;; no tests
     `(#:tests? #f))
    (home-page "https://github.com/jamesls/fakeredis")
    (synopsis "Fake implementation of redis API for testing purposes")
    (description "Fakeredis is a pure python implementation of the redis-py
python client that simulates talking to a redis server.  This was created for a
single purpose: to write unittests.  Setting up redis is not hard, but many time
 you want to write unittests that do not talk to an external server (such as
redis).  This module now allows tests to simply use this module as a reasonable
substitute for redis.")
    (license license:bsd-3)))

(define-public python2-fakeredis
  (package-with-python2 python-fakeredis))

(define-public python-redis
  (package
    (name "python-redis")
    (version "2.10.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "redis" version))
       (sha256
        (base32 "03vcgklykny0g0wpvqmy8p6azi2s078317wgb2xjv5m2rs9sjb52"))))
    (build-system python-build-system)
    ;; Tests require a running Redis server
    (arguments '(#:tests? #f))
    ;; As long as we are not running test, we do not need this input :-)
    ;;(native-inputs
    ;; `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/andymccurdy/redis-py")
    (synopsis "Redis Python client")
    (description
     "This package provides a Python interface to the Redis key-value store.")
    (license license:expat)))

(define-public python2-redis
  (package-with-python2 python-redis))

(define-public python-rq
  (package
    (name "python-rq")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "rq" version))
       (sha256
        (base32 "0gaq5pnh0zy46r8jvygi0ifbvz3pq6i7xla78ijcgjw0x77qzsdh"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-click" ,python-click)
       ("python-redis" ,python-redis)))
    (home-page "http://python-rq.org/")
    (synopsis "Simple job queues for Python")
    (description
     "RQ (Redis Queue) is a simple Python library for queueing jobs and
processing them in the background with workers.  It is backed by Redis and it
is designed to have a low barrier to entry.")
    (license license:bsd-2)))

(define-public python2-rq
  (package-with-python2 python-rq))

(define-public python-trollius-redis
  (package
    (name "python-trollius-redis")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "trollius_redis" version))
        (sha256
         (base32
          "0k3vypszmgmaipgw9xscvgm79h2zd6p6ci8gdp5sxl6g5kbqr9fy"))))
    (build-system python-build-system)
    ;; TODO: Tests require packaging 'hiredis'.
    (arguments '(#:tests? #f))
    (home-page "https://github.com/benjolitz/trollius-redis")
    (synopsis "Port of asyncio-redis to trollius")
    (description "@code{trollius-redis} is a Redis client for Python
  trollius.  It is an asynchronious IO (PEP 3156) implementation of the
  Redis protocol.")
    (license license:bsd-2)))

(define-public python2-trollius-redis
  (package-with-python2 python-trollius-redis))

(define-public python-sqlparse
  (package
    (name "python-sqlparse")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sqlparse" version))
              (sha256
               (base32
                "1v3xh0bkfhb262dbndgzhivpnhdwavdzz8jjhx9vx0xbrx2880nf"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (zero? (system* "py.test")))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/andialbrecht/sqlparse")
    (synopsis "Non-validating SQL parser")
    (description "Sqlparse is a non-validating SQL parser for Python.  It
provides support for parsing, splitting and formatting SQL statements.")
    (license license:bsd-3)))

(define-public python2-sqlparse
  (package-with-python2 python-sqlparse))

(define-public python-sql
  (package
    (name "python-sql")
    (version "0.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-sql" version))
       (sha256
        (base32
         "0p6kaqj02vz0habmdx37zjk6hjxdfm8aw737zs059vvpr70ird87"))))
    (build-system python-build-system)
    (home-page "https://python-sql.tryton.org/")
    (synopsis "Library to write SQL queries in a pythonic way")
    (description "@code{python-sql} is a library to write SQL queries, that
transforms idiomatic python function calls to well-formed SQL queries.")
    (license license:bsd-3)))

(define-public python2-sql
  (package-with-python2 python-sql))
