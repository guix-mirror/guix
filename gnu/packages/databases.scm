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
;;; Copyright © 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2017, 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2016 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017, 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017, 2020 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2017 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2017, 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017, 2018 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017, 2018, 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2015, 2017, 2018, 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Kristofer Buffington <kristoferbuffington@gmail.com>
;;; Copyright © 2018 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
;;; Copyright © 2018, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2019, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020, 2021 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020 Lars-Dominik Braun <ldb@leibniz-psychology.org>
;;; Copyright © 2020 Guy Fleury Iteriteka <gfleury@disroot.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 David Larsson <david.larsson@selfhosted.xyz>
;;; Copyright © 2021 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2021 Bonface Munyoki Kilyungi <me@bonfacemunyoki.com>
;;; Copyright © 2021 Simon Streit <simon@netpanic.org>
;;; Copyright © 2021 Alexandre Hannud Abdo <abdo@member.fsf.org>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 jgart <jgart@dismail.de>
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
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pantheon)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-web)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix bzr-download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system scons)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define-public 4store
  (package
    (name "4store")
    (version "1.1.6")
    (source (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://github.com/4store/4store")
             (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32 "1kzdfmwpzy64cgqlkcz5v4klwx99w0jk7afckyf7yqbqb4rydmpk"))
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
       ("util-linux" ,util-linux "lib")))
    ;; http://www.4store.org has been down for a while now.
    (home-page "https://github.com/4store/4store")
    (synopsis "Clustered RDF storage and query engine")
    (description "4store is a RDF/SPARQL store written in C, supporting
either single machines or networked clusters.")
    (license license:gpl3+)))

(define-public ephemeralpg
  (package
    (name "ephemeralpg")
    (version "3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://eradman.com/ephemeralpg/code/ephemeralpg-"
             version ".tar.gz"))
       (sha256
        (base32 "1ap22ki8yz6agd0qybcjgs4b9izw1rwwcgpxn3jah2ccfyax34s6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/pg_tmp")
                 `("PATH" ":" prefix
                   (,(string-append (assoc-ref inputs "util-linux")
                                    "/bin")
                    ,(string-append (assoc-ref inputs "postgresql")
                                    "/bin")
                    ;; For getsocket.
                    ,(string-append out "/bin")))))
             #t)))
       #:test-target "test"))
    (inputs
     `(("postgresql" ,postgresql)
       ("util-linux" ,util-linux)))
    (native-inputs
     ;; For tests.
     `(("ruby" ,ruby)
       ("which" ,which)))
    (home-page "https://eradman.com/ephemeralpg/")
    (synopsis "Run temporary PostgreSQL databases")
    (description
     "@code{pg_tmp} creates temporary PostgreSQL databases, suitable for tasks
like running software test suites.  Temporary databases created with
@code{pg_tmp} have a limited shared memory footprint and are automatically
garbage-collected after a configurable number of seconds (the default is
60).")
    (license license:isc)))

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

(define-public firebird
  (package
    (name "firebird")
    (version "3.0.7")
    (source
     (let ((revision "33374-0"))
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/FirebirdSQL/"
                             "firebird/releases/download/R"
                             (string-replace-substring version "." "_") "/"
                             "Firebird-" version "." revision ".tar.bz2"))
         (sha256
          (base32 "0xpy1bncz36c6n28y7kllm1dkrdkn4vb4gw2n43f2351mznmrf5c"))
         (modules '((guix build utils)))
         (snippet
          `(begin
             (for-each
              delete-file-recursively
              (list "extern/btyacc/test" ; TODO: package and remove entirely
                    "extern/editline"
                    "extern/icu"
                    "extern/libtommath"
                    "extern/zlib"
                    "src/include/firebird/impl/boost"

                    ;; Missing licence.
                    "builds/install/arch-specific/solaris"
                    "extern/SfIO"
                    "src/msgs/templates.sql"

                    ;; Generated files missing sources.
                    "doc/Firebird-3-QuickStart.pdf"
                    (string-append "doc/Firebird-" ,version
                                   "-ReleaseNotes.pdf")
                    "doc/README.SecureRemotePassword.html"))
             #t)))))
    (build-system gnu-build-system)
    (outputs (list "debug" "out"))
    (arguments
     `(#:configure-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "--with-fbsbin=" out "/sbin")
               (string-append "--with-fbdoc=" out "/share/doc/"
                              ,name "-" ,version)
               (string-append "--with-fbconf=" out "/lib/firebird")
               (string-append "--with-fbintl=" out "/lib/firebird/intl")
               (string-append "--with-fbmisc=" out "/lib/firebird/misc")
               (string-append "--with-fbmsg=" out "/lib/firebird")
               (string-append "--with-fbplugins=" out "/lib/firebird/plugins")
               (string-append "--with-fbudf=" out "/lib/firebird/UDF")
               "--with-fbglock=/run/firebird"
               "--with-fblog=/var/log/firebird"
               "--with-fbhelp=/var/lib/firebird/system"
               "--with-fbsecure-db=/var/lib/firebird/secure"
               "--without-fbsample"
               "--without-fbsample-db"
               "--with-system-editline"))
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             ;; The plugins/ can't find libfbclient otherwise.
             (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib"))
       #:tests? #f                      ; no test suite
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-system-boost
           (lambda _
             (substitute* "src/include/firebird/Message.h"
               (("\"\\./impl/boost/preprocessor/seq/for_each_i\\.hpp\"")
                "<boost/preprocessor/seq/for_each_i.hpp>")
               (("FB_BOOST_") "BOOST_"))
             #t))
         (add-after 'unpack 'patch-installation
           (lambda _
             (substitute*
                 "builds/install/arch-specific/linux/makeInstallImage.sh.in"
               (("/bin/sh") (which "bash"))
               ;; Remove shell script helpers from $PATH.
               (("(addLibs|cp) .*\\.sh .*@FB_SBINDIR@") ":")
               ;; Put files where Guix users expect them.
               (("(License\\.txt.*)@FB_CONFDIR" match)
                (string-append match "@FB_DOCDIR@"))
               (("@FB_CONFDIR@(.*License\\.txt.*)" match)
                (string-append "@FB_DOCDIR@" match))
               (("(cp .*/doc/.*)@FB_CONFDIR@(.*)" _ head tail)
                (string-append head "@FB_DOCDIR@" tail "\n")))
             (substitute*
                 (list "builds/install/posix-common/changeServerMode.sh.in"
                       "builds/install/posix-common/install.sh.in")
               ;; Skip phases that (could) cause problems in Guix.
               (("check(InstallUser|IfServerRunning|Libraries)|addFirebirdUser")
                ":")
               ;; Skip phases that are merely pointless on Guix.
               (("buildUninstallFile|installInitdScript|startFirebird") ":")
               ;; Omit randomly generated password with bonus timestamp.
               (("setDBAPassword") ":"))

             ;; These promote proprietary workflows not relevant on Guix.
             (for-each delete-file-recursively
                       (find-files "doc" "README\\.(build\\.msvc|NT|Win)"))
             #t))
         (add-after 'configure 'delete-init-scripts
           (lambda _
             (delete-file-recursively "gen/install/misc")
             #t))
         (add-before 'build 'set-build-environment-variables
           (lambda _
             ;; ‘isql’ needs to run & find libfbclient.so during the build.
             ;; This doubles as a rudimentary test in lieu of a test suite.
             (setenv "LD_LIBRARY_PATH"
                     (string-append (assoc-ref %build-inputs "icu4c") "/lib"))
             #t))
         (add-before 'install 'keep-embedded-debug-symbols
           (lambda _
             ;; Let the gnu-build-system separate & deal with them later.
             ;; XXX Upstream would use ‘--strip-unneeded’, shaving a whole
             ;; megabyte off Guix's 7.7M libEngine12.so, for example.
             (substitute* "gen/Makefile.install"
               (("readelf") "false"))
             #t))
         (add-after 'install 'prune-undesirable-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion out
                 ;; Remove example binaries.
                 (for-each delete-file-recursively
                           (find-files "." "example"))
                 ;; Delete (now-)empty directories.
                 (for-each rmdir
                           (list "include/firebird/impl"
                                 "lib/firebird/plugins/udr"))
                 #t)))))))
    (inputs
     `(("boost" ,boost)
       ("editline" ,editline)
       ("icu4c" ,icu4c-67)
       ("libtommath" ,libtommath)
       ("ncurses" ,ncurses)
       ("zlib" ,zlib)))
    (home-page "https://www.firebirdsql.org")
    (synopsis "Relational database with many ANSI SQL standard features")
    (description
     "Firebird is an SQL @acronym{RDBMS, relational database management system}
with rich support for ANSI SQL (e.g., @code{INSERT...RETURNING}) including
@acronym{UDFs, user-defined functions} and PSQL stored procedures, cursors, and
triggers.  Transactions provide full ACID-compliant referential integrity.

The database requires very little manual maintenance once set up, making it
ideal for small business or embedded use.

When installed as a traditional local or remote (network) database server,
Firebird can grow to terabyte scale with proper tuning---although PostgreSQL
may be a better choice for such very large environments.

Firebird can also be embedded into stand-alone applications that don't want or
need a full client & server.  Used in this manner, it offers richer SQL support
than SQLite as well as the option to seamlessly migrate to a client/server
database later.")
    (properties
     `((lint-hidden-cve . ("CVE-2017-6369"))))
    (license
     ;; See doc/license/README.license.usage.txt for rationale & details.
     (list license:bsd-3                ; src/common/sha2/
           license:bsd-4                ; src/common/enc.cpp
           license:gpl2+                ; builds/posix/make.defaults
           (license:non-copyleft "file:///builds/install/misc/IPLicense.txt"
                                 "InterBase Public License v1.0")
           (license:non-copyleft "file:///builds/install/misc/IDPLicense.txt"
                                 "Initial Developer's Public License v1.0")
           license:lgpl2.1           ; exception for OSI-compatible licences
           license:mpl1.1            ; examples/interfaces/0{6,8}*.cpp
           license:public-domain)))) ; including files without explicit licence

(define-public leveldb
  (package
    (name "leveldb")
    (version "1.22")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/leveldb")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qrnhiyq7r4wa1a4wi82zgns35smj94mcjsc7kfs1k6ia9ys79z7"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON" "-DLEVELDB_BUILD_TESTS=ON")))
    (inputs
     `(("snappy" ,snappy)))
    (home-page "https://github.com/google/leveldb")
    (synopsis "Fast key-value storage library")
    (description
     "LevelDB is a fast key-value storage library that provides an ordered
mapping from string keys to string values.")
    (license license:bsd-3)))

(define-public memcached
  (package
    (name "memcached")
    (version "1.6.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://memcached.org/files/memcached-" version ".tar.gz"))
       (sha256
        (base32 "0ii3z2mhjrimc6mv5z5x6bwp1s2bbzppja4m3pnmd5zgh9gs74gj"))))
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

(define-public libmemcached
  (package
    (name "libmemcached")
    (version "1.0.18")
    ;; We build from the sources since we want to build the extra HTML
    ;; documentation which is not included with the release.
    (source (origin
              (method bzr-fetch)
              (uri (bzr-reference
                    (url "lp:libmemcached/1.0")
                    (revision (string-append "tag:" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1842s4dxdh21gdr46q4dgxigidcs6dkqnbnqjwb9l8r0bqx5nb10"))
              (patches
               (search-patches "libmemcached-build-with-gcc7.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("memcached" ,memcached)
       ("libtool" ,libtool)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("bison" ,bison)
       ("flex" ,flex)
       ("perl" ,perl)
       ("python-sphinx" ,python-sphinx))) ;to build the HTML doc.
    (inputs
     `(("libevent" ,libevent)
       ("cyrus-sasl" ,cyrus-sasl)))
    (outputs '("out" "doc"))
    (arguments
     '(#:tests? #f                      ;many tests fail and use too much time
       #:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'fix-configure.ac
           ;; Move the AC_CONFIG_AUX_DIR macro use under AC_INIT, otherwise we
           ;; get the error ``configure: error: cannot find install-sh,
           ;; install.sh, or shtool in "." "./.." "./../.."`` (see:
           ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=19539 and
           ;; https://bugs.launchpad.net/libmemcached/+bug/1803922).
           (lambda _
             (delete-file "bootstrap.sh") ;not useful in the context of Guix
             (substitute* "configure.ac"
               (("^AC_CONFIG_AUX_DIR\\(\\[build-aux\\]\\).*") "")
               (("^AC_INIT.*" anchor)
                (string-append anchor "AC_CONFIG_AUX_DIR([build-aux])\n")))))
         (add-before 'build 'build-and-install-html-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((html (string-append (assoc-ref outputs "doc")
                                        "/share/doc/libmemcached/html/")))
               (invoke "make" "install-html")
               ;; Cleanup useless files.
               (for-each delete-file-recursively
                         (map (lambda (x) (string-append html x))
                              '("_sources" ".doctrees" ".buildinfo")))))))))
    (home-page "https://libmemcached.org/libMemcached.html")
    (synopsis "C++ library for memcached")
    (description "libMemcached is a library to use memcached in C/C++
applications.  It comes with a complete reference guide and documentation of
the API, and provides features such as:
@itemize
@item Asynchronous and synchronous transport support
@item Consistent hashing and distribution
@item Tunable hashing algorithm to match keys
@item Access to large object support
@item Local replication
@end itemize")
    (license license:bsd-3)))

(define-public python-pylibmc
  (package
    (name "python-pylibmc")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pylibmc" version))
       (sha256
        (base32 "1sg7d9j0v6g3xg3finf4l1hb72c13vcyyi6rqrc9shbx903d93ca"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'start-memcached-daemon
           ;; The test suite requires a memcached server.
           (lambda _
             (invoke "memcached" "-d"))))))
    (native-inputs
     `(("memcached" ,memcached)
       ("python-nose" ,python-nose)))
    (inputs
     `(("libmemcached" ,libmemcached)
       ("zlib" ,zlib)
       ("cyrus-sasl" ,cyrus-sasl)))
    (home-page "http://sendapatch.se/projects/pylibmc/")
    (synopsis "Python client for memcached")
    (description
     "@code{pylibmc} is a client in Python for memcached.  It is a wrapper
around TangentOrg’s libmemcached library, and can be used as a drop-in
replacement for the code@{python-memcached} library.")
    (license license:bsd-3)))

(define-public python2-pylibmc
  (package-with-python2 python-pylibmc))

(define-public litecli
 (package
  (name "litecli")
  (version "1.6.0")
  (source
   (origin
     (method url-fetch)
     (uri (pypi-uri "litecli" version))
     (sha256
      (base32 "1yb706mgzizzijm1k0fbny98jf58qh5q6f2870rbplxlfla4w9sd"))))
  (build-system python-build-system)
  (propagated-inputs
   `(("python-cli-helpers" ,python-cli-helpers)
     ("python-click" ,python-click)
     ("python-configobj" ,python-configobj)
     ("python-prompt-toolkit" ,python-prompt-toolkit)
     ("python-pygments" ,python-pygments)
     ("python-sqlparse" ,python-sqlparse)))
  (native-inputs
   `(("python-mock" ,python-mock)
     ("python-pytest" ,python-pytest)))
  (home-page "https://litecli.com")
  (synopsis "CLI for SQLite databases")
  (description
   "@code{litecli} is a command-line client for SQLite databases that has
auto-completion and syntax highlighting.")
  (license license:bsd-3)))

(define-public mycli
  (package
    (name "mycli")
    (version "1.24.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "mycli" version))
        (sha256
          (base32 "0rij9nw20zhqr7cqnkm8daw8b1wdc9zb6ny1ji9qz5557nz9i3bl"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ; tests expect a running MySQL
    (propagated-inputs
      `(("python-cli-helpers" ,python-cli-helpers)
        ("python-click" ,python-click)
        ("python-configobj" ,python-configobj)
        ("python-cryptography" ,python-cryptography)
        ("python-prompt-toolkit" ,python-prompt-toolkit)
        ("python-pyaes" ,python-pyaes)
        ("python-pygments" ,python-pygments)
        ("python-pymysql" ,python-pymysql)
        ("python-pyperclip" ,python-pyperclip)
        ("python-sqlparse" ,python-sqlparse)))
    (home-page "https://www.mycli.net")
    (synopsis
      "Terminal Client for MySQL with AutoCompletion and Syntax Highlighting")
    (description
      "MyCLI is a command line interface for MySQL, MariaDB, and Percona with
auto-completion and syntax highlighting.")
    (license license:bsd-3)))

;; XXX When updating, check whether boost-for-mysql is still needed.
;; It might suffice to patch ‘cmake/boost.cmake’ as done in the past.
(define-public mysql
  (package
    (name "mysql")
    (version "5.7.33")
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
               "1bb343mf7n0qg2qz497gxjsqprygrjz1q1pbz76hgqxnsy08sfxd"))))
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
                       #t)))
                  (add-after
                   'install 'wrap-mysql_helpers
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out"))
                            (bin (string-append out "/bin"))
                            (awk (assoc-ref inputs "gawk"))
                            (coreutils (assoc-ref inputs "coreutils"))
                            (grep (assoc-ref inputs "grep"))
                            (ps (assoc-ref inputs "procps"))
                            (sed (assoc-ref inputs "sed")))
                       (wrap-program (string-append bin "/mysql_config")
                         `("PATH" ":" suffix
                           (,(string-append awk "/bin")
                            ,(string-append coreutils "/bin")
                            ,(string-append sed "/bin"))))
                       (wrap-program (string-append bin "/mysqld_safe")
                         `("PATH" ":" suffix
                           (,(string-append awk "/bin")
                            ,(string-append coreutils "/bin")
                            ,(string-append grep "/bin")
                            ,(string-append ps "/bin")
                            ,(string-append sed "/bin"))))
                       #t))))))
    (native-inputs
     `(("bison" ,bison)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("boost" ,boost-for-mysql)
       ("coreutils" ,coreutils)
       ("gawk" ,gawk)
       ("grep" ,grep)
       ("libaio" ,libaio)
       ("libtirpc" ,libtirpc)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("procps" ,procps)
       ("rpcsvc-proto" ,rpcsvc-proto) ; rpcgen
       ("sed" ,sed)
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
    (version "10.5.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://downloads.mariadb.com/MariaDB"
                                  "/mariadb-" version "/source/mariadb-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1s3vfm73911cddjhgpcbkya6nz7ag2zygg56qqzwscn5ybv28j7b"))
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
                            (append (find-files "extra/wolfssl")
                                    (find-files "zlib")))
                  #t))))
    (replacement mariadb/fixed)
    (build-system cmake-build-system)
    (outputs '("out" "lib" "dev"))
    (arguments
     `(#:configure-flags
       (list
         "-DBUILD_CONFIG=mysql_release"
         ;; Linking with libarchive fails, like this:

         ;; ld: /gnu/store/...-libarchive-3.2.2/lib/libarchive.a(archive_entry.o):
         ;; relocation R_X86_64_32 against `.bss' can not be used when
         ;; making a shared object; recompile with -fPIC

         ;; For now, disable the features that that use libarchive (xtrabackup).
         "-DWITH_LIBARCHIVE=OFF"

         ;; Disable the TokuDB engine, because its test suite frequently fails,
         ;; and loading it crashes the server: <https://bugs.gnu.org/35521>.
         "-DTOKUDB_OK=OFF"

         ;; Ensure the system libraries are used.
         "-DWITH_JEMALLOC=yes"
         "-DWITH_PCRE=system"
         "-DWITH_SSL=system"
         "-DWITH_ZLIB=system"

         "-DDEFAULT_CHARSET=utf8"
         "-DDEFAULT_COLLATION=utf8_general_ci"
         "-DMYSQL_DATADIR=/var/lib/mysql"
         "-DMYSQL_UNIX_ADDR=/run/mysqld/mysqld.sock"

         ;; Do not install the benchmark suite.
         "-DINSTALL_SQLBENCHDIR=false"

         (string-append "-DCMAKE_INSTALL_PREFIX=" (assoc-ref %outputs "lib"))
         (string-append "-DCMAKE_INSTALL_RPATH=" (assoc-ref %outputs "lib")
                        "/lib")
         (string-append "-DINSTALL_INFODIR=" (assoc-ref %outputs "out")
                        "/share/mysql/docs")
         (string-append "-DINSTALL_MANDIR=" (assoc-ref %outputs "out")
                        "/share/man")
         (string-append "-DINSTALL_SCRIPTDIR=" (assoc-ref %outputs "out") "/bin")
         (string-append "-DINSTALL_BINDIR=" (assoc-ref %outputs "out") "/bin")
         "-DCMAKE_INSTALL_LIBDIR=lib"
         "-DINSTALL_PLUGINDIR=lib/mysql/plugin"
         (string-append "-DINSTALL_INCLUDEDIR=" (assoc-ref %outputs "dev")
                        "/include/mysql")
         (string-append "-DINSTALL_DOCREADMEDIR=" (assoc-ref %outputs "out")
                        "/share/mysql/docs")
         (string-append "-DINSTALL_DOCDIR=" (assoc-ref %outputs "out")
                        "/share/mysql/docs")
         (string-append "-DINSTALL_SUPPORTFILESDIR=" (assoc-ref %outputs "out")
                        "/share/mysql/support-files")
         "-DINSTALL_MYSQLSHAREDIR=share/mysql"
         "-DINSTALL_SHAREDIR=share")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'adjust-output-references
           (lambda _
             ;; The build system invariably prepends $CMAKE_INSTALL_PREFIX
             ;; to other variables such as $INSTALL_INCLUDEDIR, which does
             ;; not work when the latter uses an absolute file name.
             (substitute* "libmariadb/mariadb_config/mariadb_config.c.in"
               (("%s/@INSTALL_INCLUDEDIR@")
                (string-append "@INSTALL_INCLUDEDIR@"))
               ;; As of 10.5.8, the mariadb_config program tries to be
               ;; clever and computes the installation directory relative
               ;; to /proc/self/exe when running on Linux.  Make it fall
               ;; back to the old behaviour.
               (("defined\\(__linux__\\)")
                "0"))
             (substitute* "libmariadb/mariadb_config/libmariadb.pc.in"
               (("\\$\\{prefix\\}/@INSTALL_INCLUDEDIR@")
                "@INSTALL_INCLUDEDIR@"))
             (substitute* "support-files/mariadb.pc.in"
               (("^(include|bin|script|doc|man)dir=\\$\\{prefix\\}/" _ dir)
                (string-append dir "dir=")))
             (substitute* "include/CMakeLists.txt"
               (("\\\\\\$\\{CMAKE_INSTALL_PREFIX\\}/\\$\\{INSTALL_INCLUDEDIR\\}")
                "${INSTALL_INCLUDEDIR}"))
             (substitute* "cmake/mariadb_connector_c.cmake"
               (("\\\\\\$\\{CMAKE_INSTALL_PREFIX\\}/\\$\\{INSTALL_BINDIR\\}")
                "${INSTALL_BINDIR}"))
             #t))
         (add-after 'unpack 'adjust-tests
           (lambda _
             (let ((disabled-tests
                    '(;; These fail because root@hostname == root@localhost in
                      ;; the build environment, causing a user count mismatch.
                      ;; See <https://jira.mariadb.org/browse/MDEV-7761>.
                      "funcs_1.is_columns_mysql"
                      "main.join_cache"
                      "main.explain_non_select"
                      "main.stat_tables"
                      "main.stat_tables_innodb"
                      "main.upgrade_MDEV-19650"
                      "roles.acl_statistics"

                      ;; FIXME: This test checks various table encodings and
                      ;; fails because Guix defaults to UTF8 instead of the
                      ;; upstream default latin1_swedish_ci.  It's not easily
                      ;; substitutable because several encodings are tested.
                      "main.sp2"

                      ;; XXX: This test occasionally fails on i686-linux:
                      ;; <https://jira.mariadb.org/browse/MDEV-24458>
                      ,@(if (string-prefix? "i686" (%current-system))
                            '("main.myisampack")
                            '())

                      ;; This file contains a time bomb which makes it fail after
                      ;; 2030-12-31.  See <https://bugs.gnu.org/34351> for details.
                      "main.mysqldump"))

                   ;; This file contains a list of known-flaky tests for this
                   ;; release.  Append our own items.
                   (unstable-tests (open-file "mysql-test/unstable-tests" "a")))
               (for-each (lambda (test)
                           (format unstable-tests "~a : ~a\n"
                                   test "Disabled in Guix"))
                         disabled-tests)
               (close-port unstable-tests)

               ;; XXX: These fail because they expect a latin1 charset and
               ;; collation.  See <https://jira.mariadb.org/browse/MDEV-21264>.
               (substitute* '("mysql-test/main/gis_notembedded.result"
                              "mysql-test/main/system_mysql_db.result")
                 (("latin1_swedish_ci") "utf8_general_ci")
                 (("\tlatin1") "\tutf8"))

               (substitute* "mysql-test/suite/binlog/t/binlog_mysqlbinlog_stop_never.test"
                 (("/bin/bash")
                  (which "bash")))

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
                           ;; Skip the replication tests: they are very I/O
                           ;; intensive and frequently causes indeterministic
                           ;; failures even on powerful hardware.
                           "--skip-rpl"
                           "--skip-test-list=unstable-tests"))
                 (format #t "test suite not run~%"))
             #t))
         (add-after
          'install 'post-install
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out     (assoc-ref outputs "out"))
                   (dev     (assoc-ref outputs "dev"))
                   (lib     (assoc-ref outputs "lib"))
                   (openssl (assoc-ref inputs "openssl")))
              (substitute* (list (string-append out "/bin/mariadb-install-db")
                                 (string-append out "/bin/mysql_install_db"))
                (("basedir=\"\"")
                 (string-append "basedir=\"" out "\""))
                (("\\$basedir/share/mysql")
                 (string-append lib "/share/mysql")))

              (with-directory-excursion lib
                ;; Remove tests.
                (delete-file-recursively "mysql-test")
                ;; Remove static libraries.
                (for-each delete-file (find-files "lib" "\\.a$")))

              (with-directory-excursion out
                (delete-file "share/man/man1/mysql-test-run.pl.1")
                ;; Delete huge and unnecessary executables.
                (for-each delete-file (find-files "bin" "test$")))
              (mkdir-p (string-append dev "/share"))
              (mkdir-p (string-append dev "/bin"))
              (rename-file (string-append lib "/bin/mariadbd")
                           (string-append out "/bin/mariadbd"))
              (rename-file (string-append lib "/bin/mysqld")
                           (string-append out "/bin/mysqld"))
              (mkdir-p (string-append dev "/lib"))
              (rename-file (string-append lib "/lib/pkgconfig")
                           (string-append dev "/lib/pkgconfig"))
              (rename-file (string-append lib "/bin/mariadb_config")
                           (string-append dev "/bin/mariadb_config"))
              (rename-file (string-append out "/bin/mysql_config")
                           (string-append dev "/bin/mysql_config"))

              ;; Embed an absolute reference to OpenSSL in mysql_config
              ;; and the pkg-config file to avoid propagation.
              ;; XXX: how to do this for mariadb_config.c.in?
              (substitute* (list (string-append dev "/bin/mysql_config")
                                 (string-append dev "/lib/pkgconfig/mariadb.pc"))
                (("-lssl -lcrypto" all)
                 (string-append "-L" openssl "/lib " all)))

              #t))))))
    (native-inputs
     `(("bison" ,bison)
       ("perl" ,perl)))
    (inputs
     `(("jemalloc" ,jemalloc)
       ("libaio" ,libaio)
       ("libxml2" ,libxml2)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("pam" ,linux-pam)
       ("pcre2" ,pcre2)
       ("xz" ,xz)
       ("zlib" ,zlib)))
    ;; The test suite is very resource intensive and can take more than three
    ;; hours on a x86_64 system.  Give slow and busy machines some leeway.
    (properties '((timeout . 64800)))        ;18 hours
    (home-page "https://mariadb.org/")
    (synopsis "SQL database server")
    (description
     "MariaDB is a multi-user and multi-threaded SQL database server, designed
as a drop-in replacement of MySQL.")
    (license license:gpl2)))

(define mariadb/fixed
  (package
    (inherit mariadb)
    (source (origin
              (inherit (package-source mariadb))
              (patches (search-patches "mariadb-CVE-2021-27928.patch"))))))

(define-public mariadb-connector-c
  (package
    (name "mariadb-connector-c")
    (version "3.1.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://downloads.mariadb.org/f/connector-c-" version
             "/mariadb-connector-c-" version "-src.tar.gz"
             "/from/https%3A//mirrors.ukfast.co.uk/sites/mariadb/?serve"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0xb8fiissblxb319y5ifqqp86zblwis789ipb753pcb4zpnsaw82"))))
    (inputs
     `(("openssl" ,openssl)))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; no tests
    (home-page "https://mariadb.com/kb/en/mariadb-connector-c/")
    (synopsis "Client library to connect to MySQL or MariaDB")
    (description "The MariaDB Connector/C is used to connect applications
developed in C/C++ to MariaDB and MySQL databases.")
    (license license:lgpl2.1+)))

(define-public galera
  (package
    (name "galera")
    (version "26.4.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (commit "bac8171266cb982fe013ce496d78085438c6f23e")
                    (url "https://github.com/codership/galera")
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0h7s670pcasq8wzprhyxqfca2cghi62b8xz2kikb2a86wd453qil"))))
    (build-system cmake-build-system)
    (inputs
     `(("check" ,check)
       ("boost" ,boost)
       ("openssl" ,openssl)))
    (home-page "https://github.com/codership/galera/")
    (synopsis "Extension to the MariaDB database server")
    (description
     "Galera is a wsrep-provider that is used with MariaDB for load-balancing
and high-availability (HA).")
    (license license:gpl2)))                  ;'COPYING' says "version 2" only

;; Don't forget to update the other postgresql packages when upgrading this one.
(define-public postgresql-13
  (package
    (name "postgresql")
    (replacement postgresql-13.3)
    (version "13.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.postgresql.org/pub/source/v"
                                  version "/postgresql-" version ".tar.bz2"))
              (sha256
               (base32
                "07z6zwr58dckaa97yl9ml240z83d1lhgaxw9aq49i8lsp21mqd0j"))
              (patches (search-patches "postgresql-disable-resolve_symlinks.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--with-uuid=e2fs" "--with-openssl"
                           ;; PostgreSQL installs its own Makefile (should it?).
                           ;; Prevent it from retaining needless references to
                           ;; the build tools in order to save size.
                           "MKDIR_P=mkdir -p" "INSTALL_BIN=install -c"
                           "LD=ld" "TAR=tar")
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
       ("libuuid" ,util-linux "lib")
       ("openssl" ,openssl)
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

(define-public postgresql-13.3
  (package
    (inherit postgresql-13)
    (name "postgresql")
    (version "13.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.postgresql.org/pub/source/v"
                                  version "/postgresql-" version ".tar.bz2"))
              (sha256
               (base32
                "18dliq7h2l8irffhyyhdmfwx3si515q6gds3cxdjb9n7m17lbn9w"))
              (patches (search-patches "postgresql-disable-resolve_symlinks.patch"))))))

(define-public postgresql-11
  (package
    (inherit postgresql-13)
    (name "postgresql")
    (version "11.13")
    (source (origin
              (inherit (package-source postgresql-13))
              (uri (string-append "https://ftp.postgresql.org/pub/source/v"
                                  version "/postgresql-" version ".tar.bz2"))
              (sha256
               (base32
                "0j5wnscnxa3sx8d39s55654df8aikmvkihfb0a02hrgmyygnihx0"))))))

(define-public postgresql-10
  (package
    (inherit postgresql-11)
    (version "10.18")
    (source (origin
              (inherit (package-source postgresql-11))
              (uri (string-append "https://ftp.postgresql.org/pub/source/v"
                                  version "/postgresql-" version ".tar.bz2"))
              (sha256
               (base32
                "009qpb02bq0rx0aaw5ck70gk07xwparhfxvlfimgihw2vhp7qisp"))))))

(define-public postgresql-9.6
  (package
    (inherit postgresql-10)
    (version "9.6.23")
    (source (origin
              (inherit (package-source postgresql-10))
              (uri (string-append "https://ftp.postgresql.org/pub/source/v"
                                  version "/postgresql-" version ".tar.bz2"))
              (sha256
               (base32
                "1fa735lrmv2vrfiixg73nh024gxlagcbrssklvgwdf0s82cgfjd8"))))))

(define-public postgresql postgresql-13)

(define-public pgloader
  (package
    (name "pgloader")
    (version "3.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dimitri/pgloader")
             (commit (string-append "v" version))))
       (sha256
        (base32 "06i1jd2za3ih5caj2b4vzlzags5j65vv8dfdbz0ggdrp40wfd5lh"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     ;; NOTE: (Sharlatan-20210119T211511+0000) Tests are disabled due to being
     ;; dependent on Quicklisp, main build target is `pgloader-standalone' which
     ;; does not require Quicklisp workarounds. There is no `install' target
     ;; configured in Makefile.
     `(#:tests? #f
       #:strip-binaries? #f
       #:make-flags
       (list "pgloader-standalone" "BUILDAPP_SBCL=buildapp")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'set-home
           (lambda _
             (setenv "HOME" "/tmp")
             #t))
         (add-after 'unpack 'patch-Makefile
           (lambda _
             (substitute* "Makefile"
               (("--sbcl.*") "--sbcl $(CL) --asdf-path . \\\n"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (install-file "build/bin/pgloader"  bin))
             #t)))))
    (native-inputs
     `(("buildapp" ,buildapp)
       ("sbcl" ,sbcl)))
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-abnf" ,sbcl-cl-abnf)
       ("cl-base64" ,sbcl-cl-base64)
       ("cl-csv" ,sbcl-cl-csv)
       ("cl-fad" ,sbcl-cl-fad)
       ("cl-log" ,sbcl-cl-log)
       ("cl-markdown" ,sbcl-cl-markdown)
       ("cl-mustache" ,sbcl-cl-mustache)
       ("cl-ppcre" ,sbcl-cl-ppcre)
       ("cl-sqlite" ,sbcl-cl-sqlite)
       ("closer-mop" ,sbcl-closer-mop)
       ("command-line-arguments" ,sbcl-command-line-arguments)
       ("db3" ,sbcl-db3)
       ("drakma" ,sbcl-drakma)
       ("esrap" ,sbcl-esrap)
       ("flexi-streams" ,sbcl-flexi-streams)
       ("ixf" ,sbcl-ixf)
       ("local-time" ,sbcl-local-time)
       ("lparallel" ,sbcl-lparallel)
       ("metabang-bind" ,sbcl-metabang-bind)
       ("mssql" ,sbcl-mssql)
       ("postmodern" ,sbcl-postmodern)
       ("py-configparser" ,sbcl-py-configparser)
       ("qmynd" ,sbcl-qmynd)
       ("quri" ,sbcl-quri)
       ("split-sequence" ,sbcl-split-sequence)
       ("trivial-backtrace" ,sbcl-trivial-backtrace)
       ("usocket" ,sbcl-usocket)
       ("uuid" ,sbcl-uuid)
       ("yason" ,sbcl-yason)
       ("zs3" ,sbcl-zs3)))
    (home-page "https://pgloader.io/")
    (synopsis "Tool to migrate data to PostgreSQL")
    (description
     "@code{pgloader} is a program that can load data or migrate databases from
CSV, DB3, iXF, SQLite, MS-SQL or MySQL to PostgreSQL.")
    (license (license:x11-style "file://LICENSE"))))

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
     `(#:configure-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))
       #:make-flags (list "CFLAGS=-fPIC")))
    (home-page "https://fallabs.com/qdbm/")
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

    (native-inputs `(("bc" ,bc)
                     ("bash:include" ,bash "include")
                     ("check" ,check-0.14)
                     ("pkg-config" ,pkg-config)))

    ;; TODO: Add more optional inputs.
    (inputs `(("curl" ,curl)
              ("libgcrypt" ,libgcrypt)
              ("libuuid" ,util-linux "lib")))
    (synopsis "Manipulate plain text files as databases")
    (description
     "GNU Recutils is a set of tools and libraries for creating and
manipulating text-based, human-editable databases.  Despite being text-based,
databases created with Recutils carry all of the expected features such as
unique fields, primary keys, time stamps and more.  Many different field
types are supported, as is encryption.")
    (license license:gpl3+)
    (home-page "https://www.gnu.org/software/recutils/")))

(define-public emacs-recutils
  (package
    (inherit recutils)
    (name "emacs-recutils")
    (build-system emacs-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "etc")
             #t)))))
    (native-inputs '())
    (inputs '())
    (synopsis "Emacs mode for working with recutils database files")
    (description "This package provides an Emacs major mode @code{rec-mode}
for working with GNU Recutils text-based, human-editable databases.  It
supports editing, navigation, and querying of recutils database files
including field and record folding.")))

(define-public rocksdb
  (package
    (name "rocksdb")
    (version "6.11.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/facebook/rocksdb")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0n19p9cd13jg0lnibrzwkxs4xlrhyj3knypkd2ic41arbds0bdnl"))
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
                          "date=1970-01-01" ; build reproducibly
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
         (add-before 'check 'mount-tmp
           ;; Use the provided workspace directory for test files.
           ;; Otherwise, /tmp is used which is a mount namespace on /gnu/store.
           ;; This speeds up the build when the host /tmp is a proper tmpfs or
           ;; other fast filesystem, as opposed to /gnu which may be a HDD.
           (lambda _
             (let ((test-dir (string-append (getcwd) "/../test")))
               (mkdir test-dir)
               (setenv "TEST_TMPDIR" (canonicalize-path test-dir)))))
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
    (home-page "https://rocksdb.org/")
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
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/tialaramex/sparql-query")
                     (commit version)))
              (sha256
               (base32 "0a84a89idpjhj9w2y3fmvzv7ldps1cva1kxvfmh897k02kaniwxk"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (inputs
     `(("curl" ,curl)
       ("glib" ,glib)
       ("libxml2" ,libxml2)
       ("ncurses" ,ncurses)
       ("readline" ,readline)))
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
endpoints over HTTP.  It has been intentionally designed to @code{feel} similar to
tools for interrogating SQL databases.  For example, you can enter a query over
several lines, using a semi-colon at the end of a line to indicate the end of
your query.  It also supports readline so that you can more easily recall and
edit previous queries, even across sessions.  It can be used non-interactively,
for example from a shell script.")
    ;; Some files (like scan-sparql.c) contain a GPLv3+ license header, while
    ;; others (like sparql-query.c) contain a GPLv2+ license header.
    (license (list license:gpl3+))))

(define-public sqitch
  (package
    (name "sqitch")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DW/DWHEELER/App-Sqitch-v"
             version ".tar.gz"))
       (sha256
        (base32 "1ayiwg9kh3w0nbacbcln7h944z94vq5vnnd5diz86033bpbnq57f"))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-check-environment
           (lambda _
             (setenv "TZ" "UTC")
             (setenv "HOME" "/tmp")
             #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (path (getenv "PERL5LIB")))
               (wrap-program (string-append out "/bin/sqitch")
                 `("PERL5LIB" ":" prefix
                   (,(string-append out "/lib/perl5/site_perl"
                                    ":"
                                    path)))))
             #t)))))
    (native-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)
       ("perl-io-pager" ,perl-io-pager)
       ("perl-module-build" ,perl-module-build)
       ("perl-module-runtime" ,perl-module-runtime)
       ("perl-path-class" ,perl-path-class)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-test-dir" ,perl-test-dir)
       ("perl-test-exception" ,perl-test-exception)
       ("perl-test-file" ,perl-test-file)
       ("perl-test-file-contents" ,perl-test-file-contents)
       ("perl-test-mockmodule" ,perl-test-mockmodule)
       ("perl-test-mockobject" ,perl-test-mockobject)
       ("perl-test-nowarnings" ,perl-test-nowarnings)
       ("perl-test-warn" ,perl-test-warn)))
    (inputs
     `(("perl-class-xsaccessor" ,perl-class-xsaccessor)
       ("perl-clone" ,perl-clone)
       ("perl-config-gitlike" ,perl-config-gitlike)
       ("perl-datetime" ,perl-datetime)
       ("perl-datetime-timezone" ,perl-datetime-timezone)
       ("perl-dbd-mysql" ,perl-dbd-mysql)
       ("perl-dbd-pg" ,perl-dbd-pg)
       ("perl-dbd-sqlite" ,perl-dbd-sqlite)
       ("perl-dbi" ,perl-dbi)
       ("perl-devel-stacktrace" ,perl-devel-stacktrace)
       ("perl-encode-locale" ,perl-encode-locale)
       ("perl-hash-merge" ,perl-hash-merge)
       ("perl-ipc-run3" ,perl-ipc-run3)
       ("perl-ipc-system-simple" ,perl-ipc-system-simple)
       ("perl-libintl-perl" ,perl-libintl-perl)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-moo" ,perl-moo)
       ("perl-mysql-config" ,perl-mysql-config)
       ("perl-namespace-autoclean" ,perl-namespace-autoclean)
       ("perl-path-class" ,perl-path-class)
       ("perl-perlio-utf8_strict" ,perl-perlio-utf8_strict)
       ("perl-string-formatter" ,perl-string-formatter)
       ("perl-string-shellquote" ,perl-string-shellquote)
       ("perl-sub-exporter" ,perl-sub-exporter)
       ("perl-template-tiny" ,perl-template-tiny)
       ("perl-template-toolkit" ,perl-template-toolkit)
       ("perl-throwable" ,perl-throwable)
       ("perl-try-tiny" ,perl-try-tiny)
       ("perl-type-tiny" ,perl-type-tiny)
       ("perl-type-tiny-xs" ,perl-type-tiny-xs)
       ("perl-uri" ,perl-uri)
       ("perl-uri-db" ,perl-uri-db)))
    (home-page "https://sqitch.org/")
    (synopsis "Database change management tool")
    (description
     "Sqitch is a standalone change management system for database schemas,
which uses SQL to describe changes.")
    (license license:x11)))

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
                      (url "https://github.com/coffeeandscripts/sqlcrush")
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
    (version "1.4.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.samba.org/ftp/tdb/tdb-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0h8fkblws3d4vf37yhbrbw2nfxg5vk2v3i5mk04hhcbh9y4fvz5w"))))
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
    (version "1.643")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/T/TI/TIMB/DBI-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1yinx39960y241vf2sknxj0dfz82a5m9gvklq5rw78k0nlyrjawa"))))
    (build-system perl-build-system)
    (synopsis "Database independent interface for Perl")
    (description "This package provides an database interface for Perl.")
    (home-page "https://metacpan.org/release/DBI")
    (license license:perl-license)))

(define-public perl-dbix-class
  (package
    (name "perl-dbix-class")
    (version "0.082842")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RI/RIBASUSHI/"
                           "DBIx-Class-" version ".tar.gz"))
       (sha256
        (base32 "1rh7idjjbibc1zmiaaarask434lh0lx7f2xyfwmy37k9fa0xcpmh"))))
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
       ("perl-sql-abstract-classic" ,perl-sql-abstract-classic)
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
    (version "3.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/T/TU/TURNSTEP/"
                           "DBD-Pg-" version ".tar.gz"))
       (sha256
        (base32
         "0kcfqq7g3832wiix0sbyvlc885qghjrp2ah3akn7h2lnb22fjwy9"))))
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
       ("mysql" ,mariadb "lib")
       ("mysql-dev" ,mariadb "dev")))
    (home-page "https://metacpan.org/release/DBD-mysql")
    (synopsis "DBI MySQL interface")
    (description "This package provides a MySQL driver for the Perl5
@dfn{Database Interface} (DBI).")
    (license license:perl-license)))

(define-public perl-dbd-sqlite
  (package
    (name "perl-dbd-sqlite")
    (version "1.66")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/I/IS/ISHIGAKI/DBD-SQLite-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1zljln5nh61gj3k22a1fv2vhx5l83waizmarwkh77hk6kzzmvrw9"))))
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

(define-public perl-mysql-config
  (package
    (name "perl-mysql-config")
    (version "1.04")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DA/DARREN/MySQL-Config-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1svn7ccw2gc4cazvc58j84rxhnc9vs01zpird0l8460598j475qr"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/MySQL-Config")
    (synopsis "Parse and utilize MySQL's /etc/my.cnf and ~/.my.cnf files")
    (description
     "@code{MySQL::Config} emulates the @code{load_defaults} function from
libmysqlclient.  It will fill an array with long options, ready to be parsed by
@code{Getopt::Long}.")
    (license license:perl-license)))

(define-public perl-sql-abstract
  (package
    (name "perl-sql-abstract")
    (version "1.87")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/I/IL/ILMARI/"
                           "SQL-Abstract-" version ".tar.gz"))
       (sha256
        (base32 "0jhw91b23wc9bkfwcgvka4x5ddxk58m9bcp5ay7a3vx77nla09p9"))))
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

(define-public perl-sql-abstract-classic
  (package
    (name "perl-sql-abstract-classic")
    (version "1.91")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RI/RIBASUSHI/"
                           "SQL-Abstract-Classic-" version ".tar.gz"))
       (sha256
        (base32 "0a7g13hs3kdxrjn43sfli09mgsi9d6w0dfw6hlk268av17yisgaf"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-deep" ,perl-test-deep)
       ("perl-test-exception" ,perl-test-exception)
       ("perl-test-warn" ,perl-test-warn)))
    (propagated-inputs
     `(("perl-mro-compat" ,perl-mro-compat)
       ("perl-sql-abstract" ,perl-sql-abstract)))
    (home-page "https://metacpan.org/release/SQL-Abstract-Classic")
    (synopsis "Generate SQL from Perl data structures")
    (description
     "This module is nearly identical to @code{SQL::Abstract} 1.81, and exists
to preserve the ability of users to opt into the new way of doing things in
later versions according to their own schedules.

It is an abstract SQL generation module based on the concepts used by
@code{DBIx::Abstract}, with several important differences, especially when it
comes to @code{WHERE} clauses.  These concepts were modified to make the SQL
easier to generate from Perl data structures.

The underlying idea is for this module to do what you mean, based on the data
structures you provide it.  You shouldn't have to modify your code every time
your data changes, as this module figures it out.")
    (license license:perl-license)))

(define-public perl-sql-splitstatement
  (package
    (name "perl-sql-splitstatement")
    (version "1.00023")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/V/VE/VEESH/"
                           "SQL-SplitStatement-" version ".tar.gz"))
       (sha256
        (base32 "0ppkx46nydzlnsxf9a8pkyb74wggfrdiiwafab143lrarlh88x0s"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-differences" ,perl-test-differences)
       ("perl-test-exception" ,perl-test-exception)
       ("perl-test-script" ,perl-test-script)))
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
   (version "2.3.9")
   (source (origin
            (method url-fetch)
            (uri
             (string-append
              "ftp://ftp.unixodbc.org/pub/unixODBC/unixODBC-"
              version ".tar.gz"))
            (sha256
             (base32 "01xj65d02i3yjy7p9z08y9jakcs5szmz4rask868n7387nn3x0sj"))))
   (build-system gnu-build-system)
   (synopsis "Data source abstraction library")
   (description "Unixodbc is a library providing an API with which to access
data sources.  Data sources include SQL Servers and any software with an ODBC
Driver.")
   (license license:lgpl2.1+)
   ;; COPYING contains copy of lgpl2.1 - but copyright notices just say "LGPL"
   (home-page "http://www.unixodbc.org")))

(define-public nanodbc
  (package
    (name "nanodbc")
    (version "2.13.0")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/nanodbc/nanodbc")
                (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q80p7yv9mcl4hyvnvcjdr70y8nc940ypf368lp97vpqn5yckkgm"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       ;; The tests require ODBC backends to be installed.
       (list "-DNANODBC_DISABLE_TESTS=ON"
             "-DBUILD_SHARED_LIBS=ON")
       #:tests? #false))
    (inputs
     `(("unixodbc" ,unixodbc)))
    (home-page "https://nanodbc.io/")
    (synopsis "C++ wrapper for the native C ODBC API")
    (description "The goal for nanodbc is to make developers happy by providing
a simpler and less verbose API for working with ODBC.  Common tasks should be
easy, requiring concise and simple code.")
    (license license:expat)))

(define-public nanodbc-for-irods
  (package
    (inherit nanodbc)
    (arguments
     `(#:tests? #false
       #:configure-flags
       '("-DBUILD_SHARED_LIBS=ON"
         ;; The tests require ODBC backends to be installed.
         "-DNANODBC_DISABLE_TESTS=ON"
         "-DCMAKE_CXX_COMPILER=clang++"
         "-DCMAKE_CXX_FLAGS=-stdlib=libc++"
         "-DCMAKE_EXE_LINKER_FLAGS=-lc++abi")
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'adjust-CPLUS_INCLUDE_PATH
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs "gcc")))
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join
                        (cons (string-append (assoc-ref inputs "libcxx")
                                             "/include/c++/v1")
                              ;; Hide GCC's C++ headers so that they do not interfere with
                              ;; the Clang headers.
                              (delete (string-append gcc "/include/c++")
                                      (string-split (getenv "CPLUS_INCLUDE_PATH")
                                                    #\:)))
                        ":"))
               (format #true
                       "environment variable `CPLUS_INCLUDE_PATH' changed to ~a~%"
                       (getenv "CPLUS_INCLUDE_PATH"))))))))
    (properties `((hidden? . #true)))
    (inputs
     `(("unixodbc" ,unixodbc)
       ("libcxx" ,libcxx+libcxxabi-6)
       ("libcxxabi" ,libcxxabi-6)
       ("clang" ,clang-6)))))

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
    (home-page "https://www.unqlite.org")
    (synopsis "In-memory key/value and document store")
    (description
     "UnQLite is an in-process software library which implements a
self-contained, serverless, zero-configuration, transactional NoSQL
database engine.  UnQLite is a document store database similar to
Redis, CouchDB, etc., as well as a standard key/value store
similar to BerkeleyDB, LevelDB, etc.")
    (license license:bsd-2)))

(define-public redis
  (package
    (name "redis")
    (version "6.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.redis.io/releases/redis-"
                                  version".tar.gz"))
              (sha256
               (base32
                "1ariw5x33hmmm3d5al0j3307l5kf3vhmn78wpyaz67hia1x8nasv"))
              (modules '((guix build utils)))
              (snippet
               ;; Delete bundled jemalloc, as the package will use the libc one
               '(begin (delete-file-recursively "deps/jemalloc")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("procps" ,procps)               ; for tests
       ("tcl" ,tcl)))                   ; for tests
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'use-correct-tclsh
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "runtest"
               (("^TCLSH=.*")
                (string-append "TCLSH="
                               (assoc-ref inputs "tcl")
                               "/bin/tclsh")))))
         (add-after 'unpack 'adjust-tests
           (lambda _
             ;; Disable failing tests
             (substitute* "tests/test_helper.tcl"
               (("integration/failover") "")
               (("integration/replication-4") "")
               (("integration/replication-psync") "")
               (("integration/replication[^-]") "")))))
       #:make-flags `("CC=gcc"
                      "MALLOC=libc"
                      "LDFLAGS=-ldl"
                      ,(string-append "PREFIX="
                                      (assoc-ref %outputs "out")))))
    (synopsis "Key-value cache and store")
    (description "Redis is an advanced key-value cache and store.  Redis
supports many data structures including strings, hashes, lists, sets, sorted
sets, bitmaps and hyperloglogs.")
    (home-page "https://redis.io/")
    (license license:bsd-3)))

(define-public ruby-redis
  (package
    (name "ruby-redis")
    (version "4.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "redis" version))
        (sha256
         (base32
          "15x2sr6h094rjbvg8pkq6m3lcd5abpyx93aifvfdz3wv6x55xa48"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))    ; Tests require a running redis server.
    (synopsis "Ruby client for Redis' API")
    (description
     "This package provides a Ruby client that tries to match Redis' API
one-to-one, while still providing an idiomatic interface.")
    (home-page "https://github.com/redis/redis-rb")
    (license license:expat)))

(define-public kyotocabinet
  (package
    (name "kyotocabinet")
    (version "1.2.79")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://fallabs.com/kyotocabinet/pkg/"
                                  "kyotocabinet-" version ".tar.gz"))
              (sha256
               (base32
                "079ymsahlrijswgwfr2la9yw5h57l752cprhp5dz31iamsj1vyv7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--disable-opt" ;"-march=native". XXX this also turns off -O0.
        (string-append "LDFLAGS=-Wl,-rpath="
                       (assoc-ref %outputs "out") "/lib"))))
    (inputs `(("zlib" ,zlib)))
    (home-page "https://fallabs.com/kyotocabinet/")
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
  (version "1.856")
  (source
    (origin
      (method url-fetch)
      (uri (string-append "mirror://cpan/authors/id/P/PM/PMQS/DB_File-"
                          version ".tar.gz"))
      (sha256
        (base32 "1ab6rm2b8lz0g3gc8k9y79gkgajyby0zpybkdg9mk4g35y9bmyfd"))))
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
  (synopsis "Perl5 access to Berkeley DB version 1.x")
  (description
    "The DB::File module provides Perl bindings to the Berkeley DB version 1.x.")
  (license license:perl-license)))

(define-public lmdb
  (package
    (name "lmdb")
    (version "0.9.29")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.openldap.org/openldap/openldap.git")
             (commit (string-append "LMDB_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0airps4cd0d91nbgy7hgvifa801snxwxzwxyr6pdv61plsi7h8l3"))))
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
            #t))
         (add-after 'install 'create-pkg-config-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/lib/pkgconfig"))
               (with-output-to-file (string-append out "/lib/pkgconfig/liblmdb.pc")
                 (lambda _
                   (format #t "prefix=~a~@
                           exec_prefix=~a~@
                           libdir=~a/lib~@
                           includedir=~a/include~@
                           ~@
                           Name: liblmdb~@
                           Version: ~a~@
                           Description: Lightning Memory-Mapped Database library~@
                           Libs: -L${libdir} -llmdb~@
                           Cflags: -I${includedir}~%"
                           out out out out ,version)))
                 #t))))))
    (home-page "https://symas.com/lmdb/")
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

(define-public lmdbxx
  (package
    (name "lmdbxx")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hoytech/lmdbxx")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12k5rz74d1l0skcks9apry1svkl96g9lf5dcgylgjmh7v1jm0b7c"))))
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (build-system gnu-build-system)
    (inputs `(("lmdb" ,lmdb)))
    (home-page "https://github.com/hoytech/lmdbxx")
    (synopsis "C++11 wrapper for the LMDB embedded B+ tree database library")
    (description "@code{lmdbxx} is a comprehensive @code{C++} wrapper for the
@code{LMDB} embedded database library, offering both an error-checked
procedural interface and an object-oriented resource interface with RAII
semantics.")
    (license license:unlicense)))

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
    (version "3.14.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "peewee" version))
       (sha256
        (base32 "18jidir2wid0cp8a61m9vf9mf0pdvm6nzspc8bfwdbifghr6ndcy"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ; fails to import test data
    (inputs
     `(("sqlite" ,sqlite)))
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

(define-public python-tortoise-orm
  (package
    (name "python-tortoise-orm")
    (version "0.16.21")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tortoise-orm" version))
       (sha256
        (base32
         "1dallk0q8q4v37klm0v3rppf2w8sjkqmypc1w8r9rraqxg1ylacp"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; Pypi does not have tests and Git snapshot depends on
                   ; poetry.
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'relax-version-requirements
                    (lambda _
                      (substitute* "setup.py"
                        (("pypika>=0\\.44\\.0,<0\\.45\\.0") "pypika")
                        (("aiosqlite>=0.16.0,<0.17.0") "aiosqlite")
                        (("pytz>=2020\\.4,<2021\\.0") "pytz")
                        ;; Not required, since ciso8601 is used.
                        (("'iso8601>=0\\.1\\.13,<0\\.2\\.0',") ""))
                      #t)))))
    (native-inputs
     `(("python-asynctest" ,python-asynctest)
       ("python-nose2" ,python-nose2)))
    (propagated-inputs
     `(("python-aiosqlite" ,python-aiosqlite)
       ("python-pypika" ,python-pypika)
       ("python-ciso8601" ,python-ciso8601)
       ("python-pytz" ,python-pytz)
       ("python-typing-extensions"
        ,python-typing-extensions)))
    (home-page
     "https://github.com/tortoise/tortoise-orm")
    (synopsis
     "Easy async ORM for python, built with relations in mind")
    (description
     "Tortoise ORM is an easy-to-use asyncio ORM (Object Relational Mapper)
inspired by Django.  Tortoise ORM was build with relations in mind and
admiration for the excellent and popular Django ORM.  It’s engraved in its
design that you are working not with just tables, you work with relational
data.")
    (license license:asl2.0)))

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
    (version "4.0.30")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyodbc" version))
       (sha256
        (base32 "0skjpraar6hcwsy82612bpj8nw016ncyvvq88j5syrikxgp5saw5"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system python-build-system)
    (inputs
     `(("unixodbc" ,unixodbc)))
    (arguments
     `(#:tests? #f))                    ; no unit tests exist
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
             (url "https://github.com/brianb/mdbtools")
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
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "lmdb" version))
              (sha256
               (base32
                "1di1gj2agbxwqqwrpk4w58dpfah0kl10ha20s63dlqdd1bgzydj1"))
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
               (setenv "LMDB_PURE" "set") ; don't apply env-copy-txn.patch
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

(define-public python-orator
  (package
    (name "python-orator")
    (version "0.9.9")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "orator" version))
              (sha256
               (base32
                "0mbgybz63ryhr9p1f4glnls5c57jp6il3dw0kf97f3pj80687rvg"))))
    (build-system python-build-system)
    ;; FIXME: Tests are not distributed with PyPI, and the repository
    ;; does not contain setup.py.  How to test?
    (arguments '(#:tests? #f))
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
       ("python-pyyaml" ,python-pyyaml)
       ("python-simplejson" ,python-simplejson)
       ("python-six" ,python-six)
       ("python-wrapt" ,python-wrapt)))
    (home-page "https://orator-orm.com/")
    (synopsis "ActiveRecord ORM for Python")
    (description
     "Orator provides a simple ActiveRecord-like Object Relational Mapping
implementation for Python.")
    (license license:expat)))

(define-public virtuoso-ose
  (package
    (name "virtuoso-ose")
    (version "7.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/virtuoso/virtuoso/" version "/"
                           "virtuoso-opensource-" version ".tar.gz"))
       (sha256
        (base32 "0ly7s7a3w2a2zhhi9rq9k2qlnzapqbbc1rcdqb3zqqpgg81krz9q"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; Tests require a network connection.
       ;; TODO: Removing the libsrc/zlib source directory breaks the build.
       ;; This indicates that the internal zlib code may still be used.
       #:configure-flags '("--without-internal-zlib"
                           "--with-readline"
                           "--enable-static=no")
       #:phases
       (modify-phases %standard-phases
         ;; Even with "--enable-static=no", "libvirtuoso-t.a" is left in
         ;; the build output.  The following phase removes it.
         (add-after 'install 'remove-static-libs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((lib (string-append (assoc-ref outputs "out") "/lib")))
               (for-each (lambda (file)
                           (delete-file (string-append lib "/" file)))
                         '("libvirtuoso-t.a"
                           "libvirtuoso-t.la"))))))))
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
    (version "1.4.27")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "SQLAlchemy" version))
      (sha256
       (base32 "031jbd0svrvwr3n52iibp9mkwsj9wicnck45yd26da5kmsfkas6p"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-cython" ,python-cython) ; for C extensions
       ("python-pytest" ,python-pytest)
       ("python-mock"   ,python-mock))) ; for tests
    (propagated-inputs
     `(("python-greenlet" ,python-greenlet)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "py.test")))))))
    (home-page "https://www.sqlalchemy.org")
    (synopsis "Database abstraction library")
    (description
     "SQLAlchemy is the Python SQL toolkit and Object Relational Mapper that
gives application developers the full power and flexibility of SQL.  It
provides a full suite of well known enterprise-level persistence patterns,
designed for efficient and high-performing database access, adapted into a
simple and Pythonic domain language.")
    (license license:x11)))

(define-public python-sqlalchemy-1.3
  (package
    (inherit python-sqlalchemy)
    (version "1.3.20")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "SQLAlchemy" version))
       (sha256
        (base32 "18b9am7bsqc4nj3d2h5r93i002apczxfvpfpcqbd6f0385zmrwnj"))))
    (propagated-inputs '())))

(define-public python2-sqlalchemy
  (package-with-python2 python-sqlalchemy))

(define-public python-sqlalchemy-stubs
  (package
    (name "python-sqlalchemy-stubs")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sqlalchemy-stubs" version))
       (sha256
        (base32
         "1bppjmv7v7m0q8gwg791pgxbx4ay7mna0zq204pn9vw28kfxcrf6"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-mypy" ,python-mypy)
       ("python-typing-extensions" ,python-typing-extensions)))
    (home-page "https://github.com/dropbox/sqlalchemy-stubs")
    (synopsis "SQLAlchemy stubs and mypy plugin")
    (description "This package contains type stubs and a mypy plugin to
provide more precise static types and type inference for SQLAlchemy
framework.")
    (license license:asl2.0)))

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

(define-public python-alchemy-mock
  (package
    (name "python-alchemy-mock")
    (version "0.4.3")
    (home-page "https://github.com/miki725/alchemy-mock")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "alchemy-mock" version))
              (sha256
               (base32
                "0ylxygl3bcdapzz529n8wgk7vx9gjwb3ism564ypkpd7dbsw653r"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      ;; Create pytest.ini that adds doctest options to
                      ;; prevent test failure.  Taken from tox.ini.
                      (call-with-output-file "pytest.ini"
                        (lambda (port)
                          (format port "[pytest]
doctest_optionflags=IGNORE_EXCEPTION_DETAIL
")))
                      (invoke "pytest" "-vv" "--doctest-modules"
                              "alchemy_mock/"))))))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-sqlalchemy" ,python-sqlalchemy)))
    (synopsis "Mock helpers for SQLAlchemy")
    (description
     "This package provides mock helpers for SQLAlchemy that makes it easy
to mock an SQLAlchemy session while preserving the ability to do asserts.

Normally Normally SQLAlchemy's expressions cannot be easily compared as
comparison on binary expression produces yet another binary expression, but
this library provides functions to facilitate such comparisons.")
    (license license:expat)))

(define-public python-alembic
  (package
    (name "python-alembic")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "alembic" version))
       (sha256
        (base32 "1ys0a44gh544xpbzz6r5xvz3msim74f9qklyfnw0bhn9vk9n9adf"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "pytest" "-vv"))))))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest-cov" ,python-pytest-cov)))
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-importlib-resources" ,python-importlib-resources) ;Python < 3.9
       ("python-sqlalchemy" ,python-sqlalchemy)
       ("python-mako" ,python-mako)
       ("python-editor" ,python-editor)))
    (home-page "https://bitbucket.org/zzzeek/alembic")
    (synopsis "Database migration tool for SQLAlchemy")
    (description
     "Alembic is a lightweight database migration tool for usage with the
SQLAlchemy Database Toolkit for Python.")
    (license license:expat)))

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
    (version "3.31.1-r1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/rogerbinns/apsw/releases"
                            "/download/" version "/apsw-" version ".zip"))
        (sha256
          (base32
           "1gap5lr6c7bp134nzvfwr693i6d0fqyaysg3ms2cayjldv616yfx"))))
    (build-system python-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
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
         (replace 'check
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

(define-public python-aiosqlite
  (package
    (name "python-aiosqlite")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aiosqlite" version))
       (sha256
        (base32
         "0lgfpbkcd730hbgj3zlrbx2y8fzvdns2zj3s4r4l31n49g1arrph"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (invoke "python" "-m" "unittest" "aiosqlite.tests")
                          (format #t "test suite not run~%"))
                      #t)))))
    (propagated-inputs
     `(("python-typing-extensions" ,python-typing-extensions)))
    (native-inputs
     `(("python-aiounittest" ,python-aiounittest)))
    (home-page "https://github.com/jreese/aiosqlite")
    (synopsis
     "Asyncio bridge for sqlite3")
    (description
     "The package aiosqlite replicates the standard sqlite3 module, but with
async versions of all the standard connection and cursor methods, and context
managers for automatically closing connections.")
    (license license:expat)))

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
    (home-page "https://py2neo.org")
    (synopsis "Library and toolkit for working with Neo4j in Python")
    (description "This package provides a client library and toolkit for
working with Neo4j from within Python applications and from the command
line.  The core library has no external dependencies and has been carefully
designed to be easy and intuitive to use.")
    (license license:asl2.0)))

(define-public python-psycopg2
  (package
    (name "python-psycopg2")
    (version "2.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "psycopg2" version))
       (sha256
        (base32 "1smvvs1ngqy0ymlp1d7f85j09j9v0z5dq14f1qky0j0qi7xajkd8"))))
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

(define-public python-sadisplay
  (package
    (name "python-sadisplay")
    (version "0.4.9")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "sadisplay" version))
      (sha256
        (base32
          "15jxwgla3q4xsp6rw8inqaiy1kdzc8l2cixj8amqcf0ji47icrxg"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (begin
                            (setenv "PYTHONPATH"
                                    (string-append ".:" (or (getenv "PYTHONPATH")
                                                           "")))
                            (invoke "pytest" "-vv"))
                          (format #t "test suite not run~%")))))))
    (propagated-inputs
      `(("python-sqlalchemy" ,python-sqlalchemy)))
    (native-inputs
     ;; For tests.
      `(("python-pytest" ,python-pytest)))
    (home-page "https://bitbucket.org/estin/sadisplay")
    (synopsis "SQLAlchemy schema displayer")
    (description "This package provides a program to build Entity
Relationship diagrams from a SQLAlchemy model (or directly from the
database).")
    (license license:bsd-3)))

(define-public python2-sadisplay
  (package-with-python2 python-sadisplay))

(define-public yoyo-migrations
  (package
    (name "yoyo-migrations")
    (version "7.2.0")
    (source
     (origin
       ;; We use the upstream repository, as the tests are not included in the
       ;; PyPI releases.
       (method hg-fetch)
       (uri (hg-reference
             (url "https://hg.sr.ht/~olly/yoyo")
             (changeset (string-append "v" version "-release"))))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32 "0q2z9bgdj3wyix7yvqsayfs21grp5av8ilh411lgmjhigszkvhcq"))))
    (build-system python-build-system)
    (arguments
     ;; XXX: Tests require a connection to some pgsql database and psycopg
     ;; fails to connect to it.
     '(#:tests? #f))
    (propagated-inputs
     `(("python-sqlparse" ,python-sqlparse)
       ("python-tabulate" ,python-tabulate)))
    (home-page "https://ollycope.com/software/yoyo/latest/")
    (synopsis "Database migrations with SQL")
    (description
     "Yoyo is a database schema migration tool.  Migrations are written as SQL
files or Python scripts that define a list of migration steps.")
    (license license:asl2.0)))

(define-public python-mysqlclient
  (package
    (name "python-mysqlclient")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mysqlclient" version))
       (sha256
        (base32
         "1rf5l8hazs3v18hmcrm90z3hi9wxv553ipwd5l6kj8j7l6p7abzv"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))          ;XXX: requires a live database
    (inputs
     `(("mysql-dev" ,mariadb "dev")))
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
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fakeredis" version))
       (sha256
        (base32
         "1s12mn4q4hz7402139khn9fx56kibj7nn0d6w81hn0zs07b90wpc"))))
    (build-system python-build-system)
    (arguments
     ;; no tests
     `(#:tests? #f))
    (propagated-inputs
      `(("python-sortedcontainers" ,python-sortedcontainers)))
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
    (version "3.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "redis" version))
       (sha256
        (base32 "18h5b87g15x3j6pb1h2q27ri37p2qpvc9n2wgn5yl3b6m3y0qzhf"))))
    (build-system python-build-system)
    ;; Tests require a running Redis server.
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
    (version "1.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rq/rq")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ikqmpq0g1qiqwd7ar1286l4hqjb6aj2wr844gihhb8ijzwhp8va"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'check 'start-redis
                    (lambda _
                      (invoke "redis-server" "--daemonize" "yes")))
                  (replace 'check
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        ;; Drop test that needs the SDK for Sentry.io.
                        (delete-file "tests/test_sentry.py")
                        ;; Ensure 'rq' and 'rqworker' ends up on PATH.
                        (setenv "PATH" (string-append out "/bin:"
                                                      (getenv "PATH")))
                        (invoke "pytest" "-vv")))))))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("redis" ,redis)))
    (propagated-inputs
     `(("python-click" ,python-click)
       ("python-redis" ,python-redis)))
    (home-page "https://python-rq.org/")
    (synopsis "Simple job queues for Python")
    (description
     "RQ (Redis Queue) is a simple Python library for queueing jobs and
processing them in the background with workers.  It is backed by Redis and it
is designed to have a low barrier to entry.")
    (license license:bsd-2)))

(define-public python-rq-scheduler
  (package
    (name "python-rq-scheduler")
    (version "0.10.0")
    (home-page "https://github.com/rq/rq-scheduler")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xg6yazqs5kbr2ayvhvljs1h5vgx5k5dds613fmhswln7gglf9hk"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'check 'start-redis
                    (lambda _
                      (invoke "redis-server" "--daemonize" "yes")))
                  (replace 'check
                    (lambda _
                      (substitute* "run_tests.py"
                        (("/usr/bin/env")
                         (which "env")))
                      (invoke "./run_tests.py"))))))
    (native-inputs
     `(("redis" ,redis)
       ("which" ,which)))
    (propagated-inputs
     `(("python-croniter" ,python-croniter)
       ("python-rq" ,python-rq)))
    (synopsis "Job scheduling capabilities for RQ (Redis Queue)")
    (description
     "This package provides job scheduling capabilities to @code{python-rq}
(Redis Queue).")
    (license license:expat)))

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
  trollius.  It is an asynchronous IO (PEP 3156) implementation of the
  Redis protocol.")
    (license license:bsd-2)))

(define-public python2-trollius-redis
  (package-with-python2 python-trollius-redis))

(define-public python-sqlparse
  (package
    (name "python-sqlparse")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sqlparse" version))
              (sha256
               (base32
                "1s2l0jgi1v7rk7smzb99iamasaz22apfkczsphn3ci4wh8pgv48g"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "py.test")))
         ;; XXX: The regular wrap phase ends up storing pytest as a runtime
         ;; dependency.  See <https://bugs.gnu.org/25235>.
         (replace 'wrap
           (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (python (assoc-ref (or native-inputs inputs) "python"))
                    (sitedir (string-append "/lib/python"
                                            (python-version python)
                                            "/site-packages")))
               (wrap-program (string-append out "/bin/sqlformat")
                 `("PYTHONPATH" ":" prefix
                   ,(map (lambda (output)
                           (string-append output sitedir))
                         (list python out))))))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/andialbrecht/sqlparse")
    (synopsis "Non-validating SQL parser")
    (description "Sqlparse is a non-validating SQL parser for Python.  It
provides support for parsing, splitting and formatting SQL statements.")
    (license license:bsd-3)))

(define-public python-sql
  (package
    (name "python-sql")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-sql" version))
       (sha256
        (base32 "0xnimfzlxj1ddrb5xj3s4gaii278a0gpxrvwmdmrdxgjfdi3lq4x"))))
    (build-system python-build-system)
    (home-page "https://python-sql.tryton.org/")
    (synopsis "Library to write SQL queries in a pythonic way")
    (description "@code{python-sql} is a library to write SQL queries, that
transforms idiomatic python function calls to well-formed SQL queries.")
    (license license:bsd-3)))

(define-public python2-sql
  (package-with-python2 python-sql))

(define-public python-pypika
  (package
    (name "python-pypika")
    (version "0.47.6")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/kayak/pypika")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "001pg36sw9a36zdd1kccbymcxndphjcjbbrsy6ri7ng8h4dgz549"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-parameterized" ,python-parameterized)))
    (home-page "https://github.com/kayak/pypika")
    (synopsis "SQL query builder API for Python")
    (description
     "PyPika is a python SQL query builder that exposes the full richness of
the SQL language using a syntax that reflects the resulting query.")
    (license license:asl2.0)))

;; There are many wrappers for this in other languages. When touching, please
;; be sure to ensure all dependencies continue to build.
(define-public apache-arrow
  (package
    (name "apache-arrow")
    (version "6.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/apache/arrow")
             (commit (string-append "apache-arrow-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0mcw361akqw4sxnnpnr9c9v1zk4hphk6gcq763pcb19yzljh88ig"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'enter-source-directory
           (lambda _ (chdir "cpp")))
         (add-after 'unpack 'set-env
           (lambda _
             (substitute* "cpp/cmake_modules/ThirdpartyToolchain.cmake"
               (("set\\(xsimd_SOURCE.*") ""))
             (setenv "BOOST_ROOT" (assoc-ref %build-inputs "boost"))
             (setenv "BROTLI_HOME" (assoc-ref %build-inputs "brotli"))
             (setenv "FLATBUFFERS_HOME" (assoc-ref %build-inputs "flatbuffers"))
             (setenv "RAPIDJSON_HOME" (assoc-ref %build-inputs "rapidjson")))))
       #:build-type "Release"
       #:configure-flags
       (list "-DARROW_PYTHON=ON"
             "-DARROW_GLOG=ON"
             ;; Parquet options
             "-DARROW_PARQUET=ON"
             "-DPARQUET_BUILD_EXECUTABLES=ON"
             ;; The maintainers disallow using system versions of
             ;; jemalloc:
             ;; https://issues.apache.org/jira/browse/ARROW-3507. This
             ;; is unfortunate because jemalloc increases performance:
             ;; https://arrow.apache.org/blog/2018/07/20/jemalloc/.
             "-DARROW_JEMALLOC=OFF"

             ;; The CMake option ARROW_DEPENDENCY_SOURCE is a global
             ;; option that instructs the build system how to resolve
             ;; each dependency. SYSTEM = Finding the dependency in
             ;; system paths using CMake's built-in find_package
             ;; function, or using pkg-config for packages that do not
             ;; have this feature
             "-DARROW_DEPENDENCY_SOURCE=SYSTEM"
             "-Dxsimd_SOURCE=SYSTEM"

             "-DARROW_RUNTIME_SIMD_LEVEL=NONE"
             "-DARROW_SIMD_LEVEL=NONE"
             "-DARROW_PACKAGE_KIND=Guix"

             ;; Split output into its component packages.
             (string-append "-DCMAKE_INSTALL_PREFIX="
                            (assoc-ref %outputs "lib"))
             (string-append "-DCMAKE_INSTALL_RPATH="
                            (assoc-ref %outputs "lib")
                            "/lib")
             (string-append "-DCMAKE_INSTALL_BINDIR="
                            (assoc-ref %outputs "out")
                            "/bin")
             (string-append "-DCMAKE_INSTALL_INCLUDEDIR="
                            (assoc-ref %outputs "include")
                            "/share/include")


             "-DARROW_WITH_SNAPPY=ON"
             "-DARROW_WITH_ZLIB=ON"
             "-DARROW_WITH_ZSTD=ON"
             "-DARROW_WITH_LZ4=ON"
             "-DARROW_COMPUTE=ON"
             "-DARROW_CSV=ON"
             "-DARROW_DATASET=ON"
             "-DARROW_FILESYSTEM=ON"
             "-DARROW_HDFS=ON"
             "-DARROW_JSON=ON"
             ;; Arrow Python C++ integration library (required for
             ;; building pyarrow). This library must be built against
             ;; the same Python version for which you are building
             ;; pyarrow. NumPy must also be installed. Enabling this
             ;; option also enables ARROW_COMPUTE, ARROW_CSV,
             ;; ARROW_DATASET, ARROW_FILESYSTEM, ARROW_HDFS, and
             ;; ARROW_JSON.
             "-DARROW_PYTHON=ON"

             ;; Building the tests forces on all the
             ;; optional features and the use of static
             ;; libraries.
             "-DARROW_BUILD_TESTS=OFF"
             "-DBENCHMARK_ENABLE_GTEST_TESTS=OFF"
             ;;"-DBENCHMARK_ENABLE_TESTING=OFF"
             "-DARROW_BUILD_STATIC=OFF")))
    (inputs
     `(("boost" ,boost)
       ("brotli" ,brotli)
       ("bzip2" ,bzip2)
       ("double-conversion" ,double-conversion)
       ("gflags" ,gflags)
       ("glog" ,glog)
       ("grpc" ,grpc)
       ("protobuf" ,protobuf)
       ("python-3" ,python)
       ("python-numpy" ,python-numpy)
       ("rapidjson" ,rapidjson)
       ("re2" ,re2)
       ("snappy" ,snappy)
       ("xsimd" ,xsimd)))
    ;; These are all listed under Requires.private in arrow.pc
    (propagated-inputs
     `(("apache-thrift" ,apache-thrift "lib")
       ("lz4" ,lz4)
       ("utf8proc" ,utf8proc)
       ("zlib" ,zlib)
       ("zstd" ,zstd "lib")))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (outputs '("out" "lib" "include"))
    (home-page "https://arrow.apache.org/")
    (synopsis "Columnar in-memory analytics")
    (description "Apache Arrow is a columnar in-memory analytics layer
designed to accelerate big data.  It houses a set of canonical in-memory
representations of flat and hierarchical data along with multiple
language-bindings for structure manipulation.  It also provides IPC and common
algorithm implementations.")
    (license license:asl2.0)))

(define-public python-pyarrow
  (package
    (inherit apache-arrow)
    (name "python-pyarrow")
    (build-system python-build-system)
    (arguments
     '(#:tests? #f          ; XXX There are no tests in the "python" directory
       #:phases
       (modify-phases %standard-phases
         (delete 'build) ; XXX the build is performed again during the install phase
         (add-after 'unpack 'enter-source-directory
           (lambda _ (chdir "python")))
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))))
         (add-before 'install 'patch-cmake-variables
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Replace cmake locations with hardcoded guix links for the
             ;; underlying C++ library and headers.  This is a pretty awful
             ;; hack.
             (substitute* "cmake_modules/FindParquet.cmake"
               (("# Licensed to the Apache Software Foundation" m)
                (string-append "set(PARQUET_INCLUDE_DIR \""
                               (assoc-ref inputs "apache-arrow:include")
                               "/share/include\")\n" m))
               (("find_package_handle_standard_args" m)
                (string-append "set(PARQUET_LIB_DIR \""
                               (assoc-ref inputs "apache-arrow:lib")
                               "/lib\")\n" m)))))
         (add-before 'install 'patch-parquet-library
           (lambda _
             (substitute* "CMakeLists.txt"
               (("parquet_shared") "parquet"))))
         (add-before 'install 'set-PYARROW_WITH_PARQUET
           (lambda _
             (setenv "PYARROW_WITH_PARQUET" "1"))))))
    (propagated-inputs
     `(("apache-arrow:lib" ,apache-arrow "lib")
       ("apache-arrow:include" ,apache-arrow "include")
       ("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-six" ,python-six)))
    (native-inputs
     `(("cmake" ,cmake-minimal)
       ("pkg-config" ,pkg-config)
       ("python-cython" ,python-cython)
       ("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (outputs '("out"))
    (home-page "https://arrow.apache.org/docs/python/")
    (synopsis "Python bindings for Apache Arrow")
    (description
     "This library provides a Pythonic API wrapper for the reference Arrow C++
implementation, along with tools for interoperability with pandas, NumPy, and
other traditional Python scientific computing packages.")
    (license license:asl2.0)))

(define-public python-crate
  (package
    (name "python-crate")
    (version "0.23.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "crate" version))
              (sha256
               (base32
                "0ngmlvi320c5gsxab0s7qgq0ck4jdlcwvb6lbjhnfprafdp56vvx"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-urllib3" ,python-urllib3)))
    (home-page "https://github.com/crate/crate-python")
    (synopsis "CrateDB Python client")
    (description
     "This package provides a Python client library for CrateDB.
It implements the Python DB API 2.0 specification and includes support for
SQLAlchemy.")
    (license license:asl2.0)))

(define-public libdbi
  (package
    (name "libdbi")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/libdbi/libdbi/libdbi-"
                                  version "/libdbi-" version ".tar.gz"))
              (sha256
               (base32
                "00s5ra7hdlq25iv23nwf4h1v3kmbiyzx0v9bhggjiii4lpf6ryys"))))
    (build-system gnu-build-system)
    (synopsis "Database independent abstraction layer in C")
    (description
     "This library implements a database independent abstraction layer in C,
similar to the DBI/DBD layer in Perl.  Writing one generic set of code,
programmers can leverage the power of multiple databases and multiple
simultaneous database connections by using this framework.")
    (home-page "http://libdbi.sourceforge.net/")
    (license license:lgpl2.1+)))

(define-public libdbi-drivers
  (package
    (name "libdbi-drivers")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/libdbi-drivers/"
                                  "libdbi-drivers/libdbi-drivers-" version
                                  "/libdbi-drivers-" version ".tar.gz"))
              (sha256
               (base32
                "0m680h8cc4428xin4p733azysamzgzcmv4psjvraykrsaz6ymlj3"))))
    (build-system gnu-build-system)
    (native-inputs
     `(;; For tests.
       ("inetutils" ,inetutils)
       ("glibc-locales" ,glibc-locales)
       ("mariadb" ,mariadb)))
    (inputs
     `(("libdbi" ,libdbi)
       ("mariadb:dev" ,mariadb "dev")
       ("mariadb:lib" ,mariadb "lib")
       ("postgresql" ,postgresql)
       ("sqlite" ,sqlite)))
    (arguments
     `(#:configure-flags
       (let ((libdbi (assoc-ref %build-inputs "libdbi"))
             (mysql:inc (assoc-ref %build-inputs "mariadb:dev"))
             (mysql:lib (assoc-ref %build-inputs "mariadb:lib"))
             (postgresql (assoc-ref %build-inputs "postgresql"))
             (sqlite (assoc-ref %build-inputs "sqlite")))
         (list "--disable-docs"
               (string-append "--with-dbi-incdir=" libdbi "/include")
               (string-append "--with-dbi-libdir=" libdbi "/lib")
               "--with-mysql"
               (string-append "--with-mysql-incdir=" mysql:inc "/include/mysql")
               (string-append "--with-mysql-libdir=" mysql:lib "/lib")
               "--with-pgsql"
               (string-append "--with-pgsql-incdir=" postgresql "/include")
               (string-append "--with-pgsql-libdir=" postgresql "/lib")
               "--with-sqlite3"
               (string-append "--with-sqlite-incdir=" sqlite "/include")
               (string-append "--with-sqlite-libdir=" sqlite "/lib")))
       #:tests? #f  ; FIXME: Find why the tests get stuck forever.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "tests/test_mysql.sh"
               (("^MYMYSQLD=.*")
                (string-append "MYMYSQLD="
                               (assoc-ref inputs "mariadb")
                               "/bin/mysqld")))
             #t))
         (add-after 'install 'remove-empty-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((var (string-append (assoc-ref outputs "out") "/var")))
               (delete-file-recursively var))
             #t)))))
    (synopsis "Database drivers for the libdbi framework")
    (description
     "The @code{libdbi-drivers} library provides the database specific drivers
for the @code{libdbi} framework.

The drivers officially supported by @code{libdbi} are:
@itemize
@item MySQL,
@item PostgreSQL,
@item SQLite.
@end itemize")
    (home-page "http://libdbi-drivers.sourceforge.net/")
    (license license:lgpl2.1+)))

(define-public soci
  (package
    (name "soci")
    (version "4.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/SOCI/soci/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qc6d71lyrfh5zb7pmfihd1hjyazlkgq6p0g06ans77wnnjh0k9l"))))
    (build-system cmake-build-system)
    (propagated-inputs
     ;; Headers of soci has include-references to headers of these inputs.
     `(("firebird" ,firebird)
       ("postgresql" ,postgresql)
       ("sqlite" ,sqlite)
       ("odbc" ,unixodbc)
       ("boost" ,boost)
       ("mariadb:dev" ,mariadb "dev")))
    (arguments
     `(#:configure-flags
       ;; C++11 (-DSOCI_CXX11) is OFF by default.  hyperledger-iroha needs it.
       (list "-DCMAKE_CXX_STANDARD=17"
             "-DSOCI_LIBDIR=lib")
       #:tests? #f))         ; may require running database management systems
    (synopsis "C++ Database Access Library")
    (description
     "SOCI is an abstraction layer for several database backends, including
PostreSQL, SQLite, ODBC and MySQL.")
    (home-page "http://soci.sourceforge.net/")
    (license license:boost1.0)))

(define-public freetds
  (package
    (name "freetds")
    (version "1.2.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.freetds.org/files/stable/"
                           "freetds-" version ".tar.gz"))
       (sha256
        (base32 "11fzwcahc1bc8npxbif0448v9cwyf7k04167i7fcspmfw7a0hj0d"))))
    (build-system gnu-build-system)
    (arguments
     ;; NOTE: (Sharlatan-20210110213908+0000) some tests require DB connection,
     ;; disabled for now.
     `(#:tests? #f))
    (home-page "https://www.freetds.org/")
    (synopsis "Client libraries for MS SQL and Sybase servers")
    (description
     "FreeTDS is an implementation of the Tabular DataStream protocol, used for
connecting to MS SQL and Sybase servers over TCP/IP.")
    (license license:lgpl2.0+)))

(define-public python-tinydb
  (package
    (name "python-tinydb")
    (version "4.5.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "tinydb" version))
              (sha256
               (base32 "1x9c4s42930wwal3ds0plwb57kg5c3gj7kbpy64c29vq478b463x"))))
    (build-system python-build-system)
    ;; PyPi tarball does not contain tests and github repository does not
    ;; have a setup.py file (only pyproject).
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("python-typing-extensions" ,python-typing-extensions)))
    (home-page "https://github.com/msiemens/tinydb")
    (synopsis "TinyDB is a lightweight document oriented database")
    (description
     "TinyDB is a small document oriented database written in pure Python
with no external dependencies.  The targets are small apps that would
be blown away by a SQL-DB or an external database server.")
    (license license:expat)))

(define-public sequeler
  (package
    (name "sequeler")
    (version "0.8.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Alecaddd/sequeler")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0biggmsn8k7j6pdrwk29whl56qlfgvf5d9vjpgz4nyqih56wgh9j"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson_post_install.py"
               (("gtk-update-icon-cache") "true")
               (("update-desktop-database") "true")))))))
    (native-inputs
     `(;("appstream-glib" ,appstream-glib)  ; validation fails for lack of network
       ("gettext-minimal" ,gettext-minimal)
       ("glib:bin" ,glib "bin")             ; for glib-compile-resources
       ("gtk+" ,gtk+ "bin")
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (inputs
     `(("glib" ,glib)
       ("granite" ,granite)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ("gtksourceview-3" ,gtksourceview-3)
       ("libgda" ,libgda)
       ("libgee" ,libgee)
       ("libsecret" ,libsecret)
       ("libssh2" ,libssh2)
       ("libxml2" ,libxml2)))
    (synopsis "Friendly SQL Client")
    (description "Sequeler is a native Linux SQL client built in Vala and
Gtk.  It allows you to connect to your local and remote databases, write SQL in
a handy text editor with language recognition, and visualize SELECT results in
a Gtk.Grid Widget.")
    (home-page "https://github.com/Alecaddd/sequeler")
    (license license:gpl2+)))
