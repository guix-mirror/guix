;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2012, 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2017 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Leo Famulari <leo@famulari.name>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2016, 2017, 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2016 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016, 2017, 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2017 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017, 2018 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017, 2018 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2015, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Kristofer Buffington <kristoferbuffington@gmail.com>
;;; Copyright © 2018 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
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
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages time)
  #:use-module (gnu packages golang)
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
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system cmake)
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
      (uri (string-append "https://github.com/4store/4store/archive/v"
                          version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32 "004fmcf1w75zhc1x3zc6kc97j4jqn2v5nhk6yb3z3cpfrhzi9j50"))
      (patches (search-patches "4store-unset-preprocessor-directive.patch"
                               "4store-fix-buildsystem.patch"))))
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
             (invoke "sh" "autogen.sh"))))))
    ;; http://www.4store.org has been down for a while now.
    (home-page "https://github.com/4store/4store")
    (synopsis "Clustered RDF storage and query engine")
    (description "4store is a RDF/SPARQL store written in C, supporting
either single machines or networked clusters.")
    (license license:gpl3+)))

(define-public go-gopkg.in-mgo.v2
  (package
    (name "go-gopkg.in-mgo.v2")
    (version "2016.08.01")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-mgo/mgo")
                    (commit (string-append "r" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rwbi1z63w43b0z9srm8m7iz1fdwx7bq7n2mz862d6liiaqa59jd"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "gopkg.in/mgo.v2"
       ;; TODO: The tests fail as MongoDB fails to start
       ;; Error parsing command line: unrecognised option '--chunkSize'
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'reset-gzip-timestamps)
         (add-before 'check 'start-mongodb
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "src/gopkg.in/mgo.v2"
                 (invoke "make" "startdb")))
             #t))
         (add-after 'check 'stop'mongodb
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "src/gopkg.in/mgo.v2"
                 (invoke "make" "stopdb")))
             #t)))))
    (native-inputs
     `(("go-gopkg.in-check.v1" ,go-gopkg.in-check.v1)
       ("mongodb" ,mongodb)
       ("daemontools" ,daemontools)))
    (synopsis "@code{mgo} offers a rich MongoDB driver for Go.")
    (description
     "@code{mgo} (pronounced as mango) is a MongoDB driver for the Go language.
It implements a rich selection of features under a simple API following
standard Go idioms.")
    (home-page "http://labix.org/mgo")
    (license license:bsd-2)))

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
     '(#:tests? #f)) ;; No testsuite.
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
    (version "1.5.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://memcached.org/files/memcached-" version ".tar.gz"))
       (sha256
        (base32 "0aav15f0lh8k4i62aza2bdv4s8vv65j38pz2zc4v45snd3arfby0"))))
    (build-system gnu-build-system)
    (inputs
     `(("libevent" ,libevent)
       ("cyrus-sasl" ,cyrus-sasl)))
    (home-page "https://memcached.org/")
    (synopsis "In-memory caching service")
    (description "Memcached is an in-memory key-value store.  It has a small
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
                              "yaml-cpp-0.5.3" "zlib-1.2.8"))
                  #t))
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
       ("python" ,python-2)
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
               (apply invoke `("scons"
                               ,@common-options
                               "mongod" "mongo" "mongos"))))
           (replace 'check
             (lambda* (#:key tests? inputs #:allow-other-keys)
               (setenv "TZDIR"
                       (string-append (assoc-ref inputs "tzdata")
                                      "/share/zoneinfo"))
               (when tests?
                 ;; Note that with the tests, especially the unittests, the
                 ;; build can take up to ~45GB of space, as many tests are
                 ;; individual executable files, with some being hundreds of
                 ;; megabytes in size.
                 (apply invoke `("scons" ,@common-options "dbtest" "unittests"))
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
                         (format #f  "--jobs=~a" (parallel-job-count))))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
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

;; XXX When updating, check whether boost-for-mysql is still needed.
;; It might suffice to patch ‘cmake/boost.cmake’ as done in the past.
(define-public mysql
  (package
    (name "mysql")
    (version "5.7.23")
    (source (origin
             (method url-fetch)
             (uri (list (string-append
                          "https://dev.mysql.com/get/Downloads/MySQL-"
                          (version-major+minor version) "/"
                          name "-" version ".tar.gz")
                        (string-append
                          "https://downloads.mysql.com/archives/get/file/"
                          name "-" version ".tar.gz")))
             (sha256
              (base32
               "0rbc3xsc11lq2dm0ip6gxa16c06hi74scb97x5cw7yhbabaz4c07"))))
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
     `(("boost" ,boost-for-mysql)
       ("libaio" ,libaio)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (home-page "https://www.mysql.com/")
    (synopsis "Fast, easy to use, and popular database")
    (description
     "MySQL is a fast, reliable, and easy to use relational database
management system that supports the standardized Structured Query
Language.")
    (license license:gpl2)))

(define-public mariadb
  (package
    (name "mariadb")
    (version "10.1.37")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://downloads.mariadb.org/f/"
                                  name "-" version "/source/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ijdmdn9mcciwv361zfmja6b1h6qpbdqgrnnq6kkdapplyq1dmcc"))
              (patches (search-patches "mariadb-client-test-32bit.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled snappy and xz.
                  (delete-file-recursively "storage/tokudb/PerconaFT/third_party")
                  (substitute* "storage/tokudb/PerconaFT/CMakeLists.txt"
                    ;; This file checks that the bundled sources are present and
                    ;; declares build procedures for them.
                    (("^include\\(TokuThirdParty\\)") ""))
                  (substitute* "storage/tokudb/PerconaFT/ft/CMakeLists.txt"
                    ;; Don't attempt to use the procedures we just removed.
                    ((" build_lzma build_snappy") ""))

                  ;; Preserve CMakeLists.txt for these.
                  (for-each (lambda (file)
                              (unless (string-suffix? "CMakeLists.txt" file)
                                (delete-file file)))
                            (append (find-files "extra/yassl")
                                    (find-files "pcre") (find-files "zlib")))
                  #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       '("-DBUILD_CONFIG=mysql_release"
         ;; Linking with libarchive fails, like this:

         ;; ld: /gnu/store/...-libarchive-3.2.2/lib/libarchive.a(archive_entry.o):
         ;; relocation R_X86_64_32 against `.bss' can not be used when
         ;; making a shared object; recompile with -fPIC

         ;; For now, disable the features that that use libarchive (xtrabackup).
         "-DWITH_LIBARCHIVE=OFF"

         ;; Ensure the system libraries are used.
         "-DWITH_JEMALLOC=yes"
         "-DWITH_PCRE=system"
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
         "-DINSTALL_SUPPORTFILESDIR=share/mysql/support-files"
         "-DINSTALL_MYSQLSHAREDIR=share/mysql"
         "-DINSTALL_DOCDIR=share/mysql/docs"
         "-DINSTALL_SHAREDIR=share")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-pcre-detection
           (lambda _
             ;; The bundled PCRE in MariaDB has a patch that was upstreamed
             ;; in version 8.34.  Unfortunately the upstream patch behaves
             ;; slightly differently and the build system fails to detect it.
             ;; See <https://bugs.exim.org/show_bug.cgi?id=2173>.
             ;; XXX: Consider patching PCRE instead.
             (substitute* "cmake/pcre.cmake"
               ((" OR NOT PCRE_STACK_SIZE_OK") ""))
             #t))
         (add-after 'unpack 'adjust-tests
           (lambda _
             (let ((disabled-tests
                    '(;; These fail because root@hostname == root@localhost in
                      ;; the build environment, causing a user count mismatch.
                      ;; See <https://jira.mariadb.org/browse/MDEV-7761>.
                      "main.join_cache"
                      "main.explain_non_select"
                      "main.stat_tables_innodb"
                      "roles.acl_statistics"

                      ;; This file contains a time bomb which makes it fail after
                      ;; 2019-01-01.  See <https://bugs.gnu.org/34351> for details.
                      "main.mysqldump"

                      ;; XXX: Fails sporadically.
                      "innodb_fts.crash_recovery"

                      ;; FIXME: This test fails on i686:
                      ;; -myisampack: Can't create/write to file (Errcode: 17 "File exists")
                      ;; +myisampack: Can't create/write to file (Errcode: 17 "File exists)
                      ;; When running "myisampack --join=foo/t3 foo/t1 foo/t2"
                      ;; (all three tables must exist and be identical)
                      ;; in a loop it produces the same error around 1/240 times.
                      ;; montywi on #maria suggested removing the real_end check in
                      ;; "strings/my_vsnprintf.c" on line 503, yet it still does not
                      ;; reach the ending quote occasionally.  Disable it for now.
                      "main.myisampack"
                      ;; FIXME: This test fails on armhf-linux:
                      "mroonga/storage.index_read_multiple_double"))

                   ;; This file contains a list of known-flaky tests for this
                   ;; release.  Append our own items.
                   (unstable-tests (open-file "mysql-test/unstable-tests" "a")))
               (for-each (lambda (test)
                           (format unstable-tests "~a : ~a\n"
                                   test "Disabled in Guix"))
                         disabled-tests)
               (close-port unstable-tests)

               (substitute* "mysql-test/mysql-test-run.pl"
                 (("/bin/ls") (which "ls"))
                 (("/bin/sh") (which "sh")))
               #t)))
         (add-before 'configure 'disable-plugins
           (lambda _
             (let ((disable-plugin (lambda (name)
                                     (call-with-output-file
                                         (string-append "plugin/" name
                                                        "/CMakeLists.txt")
                                       (lambda (port)
                                         (format port "\n")))))
                   (disabled-plugins '(;; XXX: Causes a test failure.
                                       "disks")))
               (for-each disable-plugin disabled-plugins)
               #t)))
         (replace 'check
           (lambda* (#:key (tests? #t) #:allow-other-keys)
             (if tests?
                 (with-directory-excursion "mysql-test"
                   (invoke "./mtr" "--verbose"
                           "--retry=3"
                           "--testcase-timeout=40"
                           "--suite-timeout=600"
                           "--parallel" (number->string (parallel-job-count))
                           "--skip-test-list=unstable-tests"))
                 (format #t "test suite not run~%"))
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
                            "share/man/man1/mysql-test-run.pl.1"))
                ;; Delete huge and unnecessary executables.
                (for-each delete-file (find-files "bin" "(test|embedded)"))
                ;; And static libraries.
                (for-each delete-file (find-files "lib" "\\.a$")))
              #t))))))
    (native-inputs
     `(("bison" ,bison)
       ("perl" ,perl)))
    (inputs
     `(("jemalloc" ,jemalloc)
       ("libaio" ,libaio)
       ("libxml2" ,libxml2)
       ("ncurses" ,ncurses)
       ("pcre" ,pcre)
       ("snappy" ,snappy)
       ("xz" ,xz)
       ("zlib" ,zlib)))
    (propagated-inputs
     ;; mariadb.pc says -lssl -lcrypto, so propagate it.
     `(("openssl" ,openssl)))
    ;; The test suite is very resource intensive and can take more than three
    ;; hours on a x86_64 system.  Give slow and busy machines some leeway.
    (properties '((timeout . 64800)))        ;18 hours
    (home-page "https://mariadb.org/")
    (synopsis "SQL database server")
    (description
     "MariaDB is a multi-user and multi-threaded SQL database server, designed
as a drop-in replacement of MySQL.")
    (license license:gpl2)))

;; Don't forget to update the other postgresql packages when upgrading this one.
(define-public postgresql
  (package
    (name "postgresql")
    (version "10.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.postgresql.org/pub/source/v"
                                  version "/postgresql-" version ".tar.bz2"))
              (sha256
               (base32
                "1piyfcrcqscjhnnwn91kdvr764s7d0qz4lgygf9bl6qc71ji1vdz"))
              (patches (search-patches "postgresql-disable-resolve_symlinks.patch"))))
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
             (invoke "make" "-C" "contrib")))
         (add-after 'install 'install-contrib
           (lambda _
             (invoke "make" "-C" "contrib" "install"))))))
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
    (version "9.6.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.postgresql.org/pub/source/v"
                                  version "/postgresql-" version ".tar.bz2"))
              (sha256
               (base32
                "114xay230xia2fagisxahs5fc2mza8hmmkr6ibd7nxllp938931f"))))))

(define-public python-pymysql
  (package
    (name "python-pymysql")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyMySQL" version))
       (sha256
        (base32 "1ry8lxgdc1p3k7gbw20r405jqi5lvhi5wk83kxdbiv8xv3f5kh6q"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-unittest2" ,python-unittest2)))
    (inputs
     `(("python-cryptography" ,python-cryptography)))
    (arguments
     `(#:tests? #f))                    ; tests expect a running MySQL
    (home-page "https://github.com/PyMySQL/PyMySQL/")
    (synopsis "Pure-Python MySQL driver")
    (description
     "PyMySQL is a pure-Python MySQL client library, based on PEP 249.
Most public APIs are compatible with @command{mysqlclient} and MySQLdb.")
    (license license:expat)))

(define-public python2-pymysql
  (package-with-python2 python-pymysql))

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
    (version "1.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/recutils/recutils-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "14xiln4immfsw8isnvwvq0h23f6z0wilpgsc4qzabnrzb5lsx3nz"))))
    (build-system gnu-build-system)

    (arguments '(#:configure-flags
                 (list (string-append "--with-bash-headers="
                                      (assoc-ref %build-inputs "bash:include")
                                      "/include/bash"))))

    (native-inputs `(("emacs" ,emacs-minimal)
                     ("bc" ,bc)
                     ("bash:include" ,bash "include")
                     ("check" ,check)
                     ("libuuid" ,util-linux)
                     ("pkg-config" ,pkg-config)))

    ;; TODO: Add more optional inputs.
    (inputs `(("curl" ,curl)
              ("libgcrypt" ,libgcrypt)))
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
    (version "5.18.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/facebook/rocksdb")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1v2slmmr1dsgf8z0qcfg1y9x1al96859rg48b66p9nsawczd5zv9"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; TODO: unbundle gtest.
                  (delete-file "build_tools/gnu_parallel")
                  (substitute* "Makefile"
                    (("build_tools/gnu_parallel") "parallel"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CC=gcc" "V=1"
                          ;; Ceph requires that RTTI is enabled.
                          "USE_RTTI=1"
                          (string-append "INSTALL_PATH="
                                         (assoc-ref %outputs "out"))

                          ;; Running the full test suite takes hours and require
                          ;; a lot of disk space.  Instead we only run a subset
                          ;; (see .travis.yml and Makefile).
                          "ROCKSDBTESTS_END=db_tailing_iter_test")
       #:test-target "check_some"
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
               ;; These tests reliably fail due to "Too many open files".
               (("^[[:blank:]]+env_test[[:blank:]]+\\\\") "\\")
               (("^[[:blank:]]+persistent_cache_test[[:blank:]]+\\\\") "\\"))
             #t))
         (add-after 'check 'build
           ;; The default build target is a debug build for tests. The
           ;; install target depends on the "shared_lib" release target
           ;; so we build it here for clarity.
           (lambda* (#:key (make-flags '()) parallel-build? #:allow-other-keys)
               (apply invoke "make" "shared_lib"
                      `(,@(if parallel-build?
                              `("-j" ,(number->string (parallel-job-count)))
                              '())
                        ,@make-flags)))))))
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
    ;; RocksDB is dual licensed under GPL2 and ASL 2.0.  Some header
    ;; files carry the 3-clause BSD license.
    (license (list license:gpl2 license:asl2.0 license:bsd-3))))

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
                (string-append "gitrev = \"v" ,version "\"")))
             #t))
         ;; The install phase of the Makefile assumes $PREFIX/usr/local/bin.
         ;; This replacement does the same thing, except for using $PREFIX/bin
         ;; instead.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "sparql-query" bin)
               (symlink (string-append bin "/sparql-query")
                        (string-append bin "/sparql-update")))
             #t))
         (replace 'check
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" `(,@make-flags "scan-test"))
             (invoke "./scan-test"))))))
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

(define-public sqlcrush
  ;; Unfortunately, there is no proper upstream release and may never be.
  (let ((commit "b5f6868f189566a26eecc78d0f0659813c1aa98a")
        (revision "1"))
    (package
      (name "sqlcrush")
      (version (git-version "0.1.5" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/coffeeandscripts/sqlcrush.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0x3wy40r93p0jv3nbwj9a77wa4ff697d13r0wffmm7q9h3mzsww8"))))
      (build-system python-build-system)
      (inputs
       `(("python-cryptography" ,python-cryptography)
         ("python-psycopg2" ,python-psycopg2)
         ("python-pymysql" ,python-pymysql)
         ("python-sqlalchemy" ,python-sqlalchemy)))
      (home-page "https://github.com/coffeeandscripts/sqlcrush")
      (synopsis "Text console-based database viewer and editor")
      (description
       "SQLcrush lets you view and edit a database directly from the text
console through an ncurses interface.  You can explore each table's structure,
browse and edit the contents, add and delete entries, all while tracking your
changes.")
      (license license:gpl3+)))) ; no headers, see README.md

(define-public tdb
  (package
    (name "tdb")
    (version "1.3.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.samba.org/ftp/tdb/tdb-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1drnsdh1w0px35r0y7l7g59yvyr67mvcsdrli4wab0mwi07b8mn1"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; The 'configure' script is a wrapper for Waf and
               ;; doesn't recognize things like '--enable-fast-install'.
               (invoke "./configure"
                       (string-append "--prefix=" out))))))))
    (native-inputs
     `(;; TODO: Build the documentation.
       ;; ("docbook-xsl" ,docbook-xsl)
       ;; ("libxml2" ,libxml2)
       ;; ("libxslt" ,libxslt)
       ("python" ,python)                         ;for the Waf build system
       ("which" ,which)))
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
    (version "1.642")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/T/TI/TIMB/DBI-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0pbzqazrx7pnw4nbyaf27in4b6yddkirbd2ws7mnqa2n7812a81z"))))
    (build-system perl-build-system)
    (synopsis "Database independent interface for Perl")
    (description "This package provides an database interface for Perl.")
    (home-page "https://metacpan.org/release/DBI")
    (license license:perl-license)))

(define-public perl-dbix-class
  (package
    (name "perl-dbix-class")
    (version "0.082841")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RI/RIBASUSHI/"
                           "DBIx-Class-" version ".tar.gz"))
       (sha256
        (base32
         "1gf3hgv8f9rnr8bl4ljgsqk4aliphmvljhsk4282kvdc4mcgh1fp"))))
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
    (home-page "https://metacpan.org/release/DBIx-Class")
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
    (home-page "https://metacpan.org/release/DBIx-Class-Cursor-Cached")
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
    (home-page "https://metacpan.org/release/DBIx-Class-IntrospectableM2M")
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
    (version "0.07049")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IL/ILMARI/"
                           "DBIx-Class-Schema-Loader-" version ".tar.gz"))
       (sha256
        (base32
         "0r57fv71ypxafb85cpxph1hdqii7ipjwvc19yb6fpkvq2ggcssg8"))))
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
    (home-page "https://metacpan.org/release/DBIx-Class-Schema-Loader")
    (synopsis "Create a DBIx::Class::Schema based on a database")
    (description "DBIx::Class::Schema::Loader automates the definition of a
DBIx::Class::Schema by scanning database table definitions and setting up the
columns, primary keys, unique constraints and relationships.")
    (license license:perl-license)))

(define-public perl-dbd-pg
  (package
    (name "perl-dbd-pg")
    (version "3.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TU/TURNSTEP/"
                           "DBD-Pg-" version ".tar.gz"))
       (sha256
        (base32
         "0gkqlvbmzbdm0g4k328nlkjdg3wrjm5i2n9jxj1i8sqxkm79rylz"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-dbi" ,perl-dbi)))
    (propagated-inputs
     `(("perl-dbi" ,perl-dbi)
       ("postgresql" ,postgresql)))
    (home-page "https://metacpan.org/release/DBD-Pg")
    (synopsis "DBI PostgreSQL interface")
    (description "This package provides a PostgreSQL driver for the Perl5
@dfn{Database Interface} (DBI).")
    (license license:perl-license)))

(define-public perl-dbd-mysql
  (package
    (name "perl-dbd-mysql")
    (version "4.050")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/D/DV/DVEEDEN/"
                           "DBD-mysql-" version ".tar.gz"))
       (sha256
        (base32 "0y4djb048i09dk19av7mzfb3khr72vw11p3ayw2p82jsy4gm8j2g"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'skip-library-detection
           ;; Avoid depencies on perl-devel-checklib, openssl, and zlib.  They
           ;; are really only needed for the test suite; their absence does not
           ;; affect the build or the end result.
           (lambda _
             (substitute* "Makefile.PL"
               (("use Devel::CheckLib;" match)
                (string-append "# " match))
               (("assert_lib")
                "print"))
             #t)))
       ;; Tests require running MySQL server.
       #:tests? #f))
    (propagated-inputs
     `(("perl-dbi" ,perl-dbi)
       ("mysql" ,mysql)))
    (home-page "https://metacpan.org/release/DBD-mysql")
    (synopsis "DBI MySQL interface")
    (description "This package provides a MySQL driver for the Perl5
@dfn{Database Interface} (DBI).")
    (license license:perl-license)))

(define-public perl-dbd-sqlite
  (package
    (name "perl-dbd-sqlite")
    (version "1.62")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/I/IS/ISHIGAKI/DBD-SQLite-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0p78ri1q6xpc1i98i6mlriv8n66iz8r5r11dlsknjm4y58rfz0mx"))))
    (build-system perl-build-system)
    (inputs `(("sqlite" ,sqlite)))
    (propagated-inputs `(("perl-dbi" ,perl-dbi)))
    (synopsis "SQlite interface for Perl")
    (description "DBD::SQLite is a Perl DBI driver for SQLite, that includes
the entire thing in the distribution.  So in order to get a fast transaction
capable RDBMS working for your Perl project you simply have to install this
module, and nothing else.")
    (license license:perl-license)
    (home-page "https://metacpan.org/release/DBD-SQLite")))

(define-public perl-sql-abstract
  (package
    (name "perl-sql-abstract")
    (version "1.86")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IL/ILMARI/"
                           "SQL-Abstract-" version ".tar.gz"))
       (sha256
        (base32 "1pwcm8hwxcgidyyrak37lx69d85q728jxsb0b14jz93gbvdgg9z7"))))
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
    (home-page "https://metacpan.org/release/SQL-Abstract")
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
    (home-page "https://metacpan.org/release/SQL-SplitStatement")
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
    (home-page "https://metacpan.org/release/SQL-Tokenizer")
    (synopsis "SQL tokenizer")
    (description "SQL::Tokenizer is a tokenizer for SQL queries.  It does not
claim to be a parser or query verifier.  It just creates sane tokens from a
valid SQL query.")
    (license license:perl-license)))

(define-public unixodbc
  (package
   (name "unixodbc")
   (version "2.3.7")
   (source (origin
            (method url-fetch)
            (uri
             (string-append
              "ftp://ftp.unixodbc.org/pub/unixODBC/unixODBC-"
              version ".tar.gz"))
            (sha256
             (base32 "0xry3sg497wly8f7715a7gwkn2k36bcap0mvzjw74jj53yx6kwa5"))))
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
    (version "4.0.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.redis.io/releases/redis-"
                                  version".tar.gz"))
              (sha256
               (base32
                "194cydhv3hf4v95ifvjvsqrs4jn3ffrkg5lvxj5d3y04lwsp9dhx"))))
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
    (version "1.2.77")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://fallabs.com/kyotocabinet/pkg/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rlx4307adbzd842b4npq6cwlw8h010ingxaz3qz1ijc70lr72an"))))
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

(define-public wiredtiger-3
  (package
    (inherit wiredtiger)
    (name "wiredtiger")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://source.wiredtiger.com/releases/wiredtiger-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "014awypv579ascg4jbx4pndj2wld337m79yyzrzyr7hxrff139jx"))))))

(define-public guile-wiredtiger
  (package
    (name "guile-wiredtiger")
    (version "0.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://framagit.org/a-guile-mind/guile-wiredtiger.git")
                    (commit "340ad4bc2ff4dcc6216a2f5c6f9172ca320ac66b")))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "15j36bvxxzil7qpwlmh1rffqpva3ynvrcpqhhqbj2c9208ayz595"))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-tests? #f  ;; tests can't be run in parallel, yet.
       #:configure-flags
       (list (string-append "--with-libwiredtiger-prefix="
                            (assoc-ref %build-inputs "wiredtiger")))
       #:make-flags '("GUILE_AUTO_COMPILE=0")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("wiredtiger" ,wiredtiger-3)
       ("guile" ,guile-2.2)))
    (propagated-inputs
     `(("guile-bytestructures" ,guile-bytestructures)))
    (synopsis "WiredTiger bindings for GNU Guile")
    (description
     "This package provides Guile bindings to the WiredTiger ``NoSQL''
database.")
    (home-page "https://framagit.org/a-guile-mind/guile-wiredtiger")
    (license license:gpl3+)))

(define-public perl-db-file
 (package
  (name "perl-db-file")
  (version "1.843")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/P/PM/PMQS/DB_File-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "0sildz1i3fmh949w1scpjsyjx0cbmfw0yna3y70mc6vbwp8y696y"))))
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
  (home-page "https://metacpan.org/release/DB_File")
  (synopsis
    "Perl5 access to Berkeley DB version 1.x")
  (description
    "The DB::File module provides Perl bindings to the Berkeley DB version 1.x.")
  (license license:perl-license)))

(define-public lmdb
  (package
    (name "lmdb")
    (version "0.9.23")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/LMDB/lmdb.git")
             (commit (string-append "LMDB_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ag7l5180ajvm73y59m7sn3p52xm8m972d08cshxhpwgwa4v35k6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (chdir "libraries/liblmdb")
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
    (version "3.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/sqlcipher/" name
                           "/archive/v" version ".tar.gz"))
       (sha256
        (base32 "1nxarwbci8jx99f1d0y1ivxcv25s78l1p7q6qy28lkpkcx8pm2b9"))
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
             (invoke "./testfixture" "test/crypto.test"))))))
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
    (version "4.0.24")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyodbc" version))
       (sha256
        (base32
         "1m311vi7vpay1j7rkq71fpsk0gb7454k4lldk5b63hyy6yvsn9j3"))
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
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/brianb/mdbtools.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0gwcpp9y09xhs21g7my2fs8ncb8i6ahlyixcx8jd3q97jbzj441l"))))
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
    (version "0.94")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "lmdb" version))
              (sha256
               (base32
                "1zh38gvkqw1jm5105if6rr7ccbgyxr7k2rm5ygb9ab3bq82pyaww"))
              (modules '((guix build utils)))
              (snippet
               ;; Delete bundled lmdb source files.
               '(begin
                  (for-each delete-file (list "lib/lmdb.h"
                                              "lib/mdb.c"
                                              "lib/midl.c"
                                              "lib/midl.h"))
                  #t))))
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
       ("python-pytest" ,python-pytest)
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
    (version "7.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/openlink/virtuoso-opensource/releases/"
             "download/v" version "/virtuoso-opensource-" version ".tar.gz"))
       (sha256
        (base32 "0r1xakclkfi69pzh8z2k16z3x0m49pxp764icj0ad4w4bb97fr42"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; Tests require a network connection.
       ;; TODO: Removing the libsrc/zlib source directory breaks the build.
       ;; This indicates that the internal zlib code may still be used.
       #:configure-flags '("--without-internal-zlib"
                           "--with-readline")))
    (inputs
     `(("openssl" ,openssl)
       ("net-tools" ,net-tools)
       ("readline" ,readline)
       ("zlib" ,zlib)))
    (home-page "http://vos.openlinksw.com/owiki/wiki/VOS/")
    (synopsis "Multi-model database system")
    (description "Virtuoso is a scalable cross-platform server that combines
relational, graph, and document data management with web application server
and web services platform functionality.")
    ;; configure: error: ... can only be build on 64bit platforms
    (supported-systems '("x86_64-linux" "mips64el-linux" "aarch64-linux"))
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
    (version "1.2.11")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "SQLAlchemy" version))
      (sha256
       (base32
        "094mmbs4igrxplfyqd59j90jb83ixpbbzqc0w49yw81m82nnjrgg"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-cython" ,python-cython) ;for c extensions
       ("python-pytest" ,python-pytest)
       ("python-mock"   ,python-mock))) ;for tests
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (invoke "py.test"))))))
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
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "alembic" version))
       (sha256
        (base32
         "0asqz9mwc4w8bsar1icv3ik9jslxrj3gv3yxgmhc6nc6r9qbkg04"))))
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
    (version "0.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pickleshare" version))
       (sha256
        (base32 "1jmghg3c53yp1i8cm6pcrm280ayi8621rwyav9fac7awjr3kss47"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "pytest"))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
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
    (properties `((python2-variant . ,(delay python2-pickleshare))))
    (license license:expat)))

(define-public python2-pickleshare
  (let ((pickleshare (package-with-python2
                      (strip-python2-variant python-pickleshare))))
    (package (inherit pickleshare)
      (propagated-inputs `(("python2-pathlib2" ,python2-pathlib2)
                           ,@(package-propagated-inputs pickleshare))))))

(define-public python-apsw
  (package
    (name "python-apsw")
    (version "3.20.1-r1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/rogerbinns/apsw/archive/"
                            version ".tar.gz"))
        (file-name (string-append "apsw-" version ".tar.gz"))
        (sha256
          (base32
           "00ai7m2pqi26qaflhz314d8k5i3syw7xzr145fhfl0crhyh6adz2"))))
    (build-system python-build-system)
    (inputs
     `(("sqlite" ,sqlite)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (invoke "python" "setup.py" "build" "--enable-all-extensions")
             #t))
         (add-after 'build 'build-test-helper
           (lambda _
             (invoke "gcc" "-fPIC" "-shared" "-o" "./testextension.sqlext"
                     "-I." "-Isqlite3" "src/testextension.c")
             #t))
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "python" "setup.py" "test")
             #t)))))
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
    (version "2.7.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "psycopg2" version))
       (sha256
        (base32 "0zjbabb4qjx9dm07imhf8y5a9rpa06d5zah80myiimgdi83nslpl"))))
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
     "psycopg2 is a thread-safe PostgreSQL adapter that implements DB-API
2.0.")
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
    (version "1.3.13")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mysqlclient" version))
       (sha256
        (base32
         "0kv4a1icwdav8jpl7qvnr931lw5h3v22ids6lwq6qpi1hjzf33pz"))))
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
    (description
     "Fakeredis is a pure-Python implementation of the redis-py Python client
that simulates talking to a redis server.  It was created for a single purpose:
to write unit tests.

Setting up redis is not hard, but one often wants to write unit tests that don't
talk to an external server such as redis.  This module can be used as a
reasonable substitute.")
    (license license:bsd-3)))

(define-public python2-fakeredis
  (package-with-python2 python-fakeredis))

(define-public python-redis
  (package
    (name "python-redis")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "redis" version))
       (sha256
        (base32 "0m1b88wg1w6xdwg0siky5k86x8sh6smhbr42ixz41ra81lv34jbj"))))
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
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "rq" version))
       (sha256
        (base32 "0xvapd2bxnyq480i48bdkddzlqmv2axbsq85rlfy8k3al8zxxxrf"))))
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
           (lambda _ (invoke "py.test"))))))
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

(define-public mongo-tools
  (package
    (name "mongo-tools")
    (version "3.4.0")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/mongodb/mongo-tools")
                   (commit (string-append "r" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1bcsz5cvj39a7nsxsfqmz9igrw33j6yli9kffigqyscs52amw7x1"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/mongodb/mongo-tools"
       #:modules ((srfi srfi-1)
                  (guix build go-build-system)
                  (guix build utils))
       #:phases
       (let ((all-tools
              '("bsondump" "mongodump" "mongoexport" "mongofiles"
                "mongoimport" "mongooplog" "mongorestore"
                "mongostat" "mongotop")))
         (modify-phases %standard-phases
           (add-after 'unpack 'delete-bundled-source-code
             (lambda _
               (delete-file-recursively
                "src/github.com/mongodb/mongo-tools/vendor")
               #t))
           (add-after 'delete-bundled-source-code 'patch-source
             (lambda _
               ;; Remove a redundant argument that causes compilation to fail.
               (substitute*
                   "src/github.com/mongodb/mongo-tools/mongorestore/filepath.go"
                 (("skipping restore of system.profile collection\", db)")
                  "skipping restore of system.profile collection\")"))
               #t))
           ;; We don't need to install the source code for end-user applications
           (delete 'install-source)
           (replace 'build
             (lambda _
               (for-each (lambda (tool)
                           (let ((command
                                  `("go" "build"
                                    ;; This is where the tests expect to find the
                                    ;; executables
                                    "-o" ,(string-append
                                           "src/github.com/mongodb/mongo-tools/bin/"
                                           tool)
                                    "-v"
                                    "-tags=\"ssl sasl\""
                                    "-ldflags"
                                    "-extldflags=-Wl,-z,now,-z,relro"
                                    ,(string-append
                                      "src/github.com/mongodb/mongo-tools/"
                                      tool "/main/" tool ".go"))))
                             (simple-format #t "build: running ~A\n"
                                            (string-join command))
                             (apply invoke command)))
                         all-tools)
               #t))
           (replace 'check
             (lambda _
               (with-directory-excursion "src"
                 (for-each (lambda (tool)
                             (invoke
                              "go" "test" "-v"
                              (string-append "github.com/mongodb/mongo-tools/"
                                             tool)))
                           all-tools))
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (for-each (lambda (tool)
                           (install-file
                            (string-append "src/github.com/mongodb/mongo-tools/bin/"
                                           tool)
                            (string-append (assoc-ref outputs "out")
                                           "/bin")))
                         all-tools)
               #t))))))
    (native-inputs
     `(("go-github.com-howeyc-gopass" ,go-github.com-howeyc-gopass)
       ("go-github.com-jessevdk-go-flags" ,go-github.com-jessevdk-go-flags)
       ("go-golang.org-x-crypto-ssh-terminal" ,go-golang.org-x-crypto-ssh-terminal)
       ("go-gopkg.in-mgo.v2" ,go-gopkg.in-mgo.v2)
       ("go-gopkg.in-tomb.v2" ,go-gopkg.in-tomb.v2)
       ("go-github.com-nsf-termbox-go" ,go-github.com-nsf-termbox-go)
       ("go-github.com-smartystreets-goconvey" ,go-github.com-smartystreets-goconvey)))
    (home-page "https://github.com/mongodb/mongo-tools")
    (synopsis "Various tools for interacting with MongoDB and BSON")
    (description
     "This package includes a collection of tools related to MongoDB.
@table @code
@item bsondump
Display BSON files in a human-readable format
@item mongoimport
Convert data from JSON, TSV or CSV and insert them into a collection
@item mongoexport
Write an existing collection to CSV or JSON format
@item mongodump/mongorestore
Dump MongoDB backups to disk in the BSON format
@item mongorestore
Read MongoDB backups in the BSON format, and restore them to a live database
@item mongostat
Monitor live MongoDB servers, replica sets, or sharded clusters
@item mongofiles
Read, write, delete, or update files in GridFS
@item mongooplog
Replay oplog entries between MongoDB servers
@item mongotop
Monitor read/write activity on a mongo server
@end table")
    (license license:asl2.0)))

(define-public apache-arrow
  (package
    (name "apache-arrow")
    (version "0.7.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/apache/arrow")
               (commit (string-append "apache-arrow-" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1x7sdd8lbs3nfqjql1pcgbkjc19bls56zmgjayshkmablvlc4dy3"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'enter-source-directory
           (lambda _ (chdir "cpp") #t))
         (add-after 'unpack 'set-env
           (lambda _
             (setenv "BOOST_ROOT" (assoc-ref %build-inputs "boost"))
             (setenv "BROTLI_HOME" (assoc-ref %build-inputs "brotli"))
             (setenv "FLATBUFFERS_HOME" (assoc-ref %build-inputs "flatbuffers"))
             (setenv "JEMALLOC_HOME" (assoc-ref %build-inputs "jemalloc"))
             (setenv "RAPIDJSON_HOME" (assoc-ref %build-inputs "rapidjson"))
             #t)))
       #:build-type "Release"
       #:configure-flags
       (list "-DARROW_PYTHON=ON"

             ;; Install to PREFIX/lib (the default is
             ;; PREFIX/lib64).
             (string-append "-DCMAKE_INSTALL_LIBDIR="
                            (assoc-ref %outputs "out")
                            "/lib")

             ;; XXX These Guix package offer static
             ;; libraries that are not position independent,
             ;; and ld fails to link them into the arrow .so
             "-DARROW_WITH_SNAPPY=OFF"
             "-DARROW_WITH_ZLIB=OFF"
             "-DARROW_WITH_ZSTD=OFF"
             "-DARROW_WITH_LZ4=OFF"

             ;; Building the tests forces on all the
             ;; optional features and the use of static
             ;; libraries.
             "-DARROW_BUILD_TESTS=OFF"
             "-DARROW_BUILD_STATIC=OFF")))
    (inputs
     `(("boost" ,boost)
       ("rapidjson" ,rapidjson)
       ("brotli" ,google-brotli)
       ("flatbuffers" ,flatbuffers)
       ;; Arrow is not yet compatible with jemalloc >= 5:
       ;; https://issues.apache.org/jira/browse/ARROW-1141
       ("jemalloc" ,jemalloc-4.5.0)
       ("python-3" ,python)
       ("python-numpy" ,python-numpy)))
    (home-page "https://arrow.apache.org/")
    (synopsis "Columnar in-memory analytics")
    (description "Apache Arrow is a columnar in-memory analytics layer
designed to accelerate big data. It houses a set of canonical in-memory
representations of flat and hierarchical data along with multiple
language-bindings for structure manipulation. It also provides IPC and common
algorithm implementations.")
    (license license:asl2.0)))

(define-public python-pyarrow
  (package
    (name "python-pyarrow")
    (version "0.7.0")
    (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apache/arrow")
             (commit (string-append "apache-arrow-" version))))
       (file-name (git-file-name name version))
       (sha256
         (base32
           "1x7sdd8lbs3nfqjql1pcgbkjc19bls56zmgjayshkmablvlc4dy3"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f ; XXX Test failures related to missing libhdfs, libhdfs3,
                   ; and "Unsupported numpy type 22".
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-source-directory
           (lambda _ (chdir "python") #t))
         (add-after 'unpack 'set-env
           (lambda _
             (setenv "ARROW_HOME" (assoc-ref %build-inputs "apache-arrow"))
             #t)))))
    (propagated-inputs
     `(("apache-arrow" ,apache-arrow)
       ("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-six" ,python-six)))
    (native-inputs
     `(("cmake" ,cmake)
       ("python-cython" ,python-cython)
       ("python-pytest" ,python-pytest)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (home-page "https://arrow.apache.org/docs/python/")
    (synopsis "Python bindings for Apache Arrow")
    (description "This library provides a Pythonic API wrapper for the reference
Arrow C++ implementation, along with tools for interoperability with pandas,
NumPy, and other traditional Python scientific computing packages.")
    (license license:asl2.0)))

(define-public python2-pyarrow
  (package-with-python2 python-pyarrow))
