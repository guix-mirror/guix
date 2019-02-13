;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2018, 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu packages finance)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gnuzilla))

(define-public bitcoin-core
  (package
    (name "bitcoin-core")
    (version "0.17.1")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "https://bitcoincore.org/bin/bitcoin-core-"
                             version "/bitcoin-" version ".tar.gz"))
             (sha256
              (base32
               "0am4pnaf2cisv172jqx6jdpzx770agm8777163lkjbw3ryslymiy"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python) ; for the tests
       ("util-linux" ,util-linux)   ; provides the hexdump command for tests
       ("qttools" ,qttools)))
    (inputs
     `(("bdb" ,bdb-5.3) ; with 6.2.23, there is an error: ambiguous overload
       ("boost" ,boost)
       ("libevent" ,libevent)
       ("miniupnpc" ,miniupnpc)
       ("openssl" ,openssl)
       ("protobuf" ,protobuf)
       ("qtbase" ,qtbase)))
    (arguments
     `(#:configure-flags
        (list
          ;; We use a bdb version newer than 4.8.
          "--with-incompatible-bdb"
          ;; Boost is not found unless specified manually.
          (string-append "--with-boost="
                         (assoc-ref %build-inputs "boost"))
          ;; XXX: The configure script looks up Qt paths by
          ;; `pkg-config --variable=host_bins Qt5Core`, which fails to pick
          ;; up executables residing in 'qttools', so we specify them here.
          (string-append "ac_cv_path_LRELEASE="
                         (assoc-ref %build-inputs "qttools")
                         "/bin/lrelease")
          (string-append "ac_cv_path_LUPDATE="
                         (assoc-ref %build-inputs "qttools")
                         "/bin/lupdate"))
       #:phases
        (modify-phases %standard-phases
          (add-before 'configure 'make-qt-deterministic
           (lambda _
            ;; Make Qt deterministic.
            (setenv "QT_RCC_SOURCE_DATE_OVERRIDE" "1")
            #t))
          (add-before 'check 'set-home
           (lambda _
            (setenv "HOME" (getenv "TMPDIR"))  ; Tests write to $HOME.
            #t)))))
    (home-page "https://bitcoin.org/en/")
    (synopsis "Bitcoin peer-to-peer client")
    (description
     "Bitcoin is a digital currency that enables instant payments to anyone
anywhere in the world.  It uses peer-to-peer technology to operate without
central authority: managing transactions and issuing money are carried out
collectively by the network.  Bitcoin Core is the reference implementation
of the bitcoin protocol.  This package provides the Bitcoin Core command
line client and a client based on Qt.")
    (license license:expat)))

(define-public ledger
  (package
    (name "ledger")
    (version "3.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ledger/ledger.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hwnipj2m9p95hhyv6kyq54m27g14r58gnsy2my883kxhpcyb2vc"))
       (patches (search-patches "ledger-fix-uninitialized.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       `("-DBUILD_DOCS:BOOL=ON"
         "-DBUILD_WEB_DOCS:BOOL=ON"
         "-DUSE_PYTHON:BOOL=ON"
         "-DCMAKE_INSTALL_LIBDIR:PATH=lib")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'install-examples
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((examples (string-append (assoc-ref outputs "out")
                                            "/share/doc/ledger/examples")))
               (install-file "test/input/sample.dat" examples)
               (install-file "test/input/demo.ledger" examples))
             #t))
         (add-after 'build 'build-doc
           (lambda _ (invoke "make" "doc")))
         (add-before 'check 'check-setup
           ;; One test fails if it can't set the timezone.
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZDIR"
                     (string-append (assoc-ref inputs "tzdata")
                                    "/share/zoneinfo"))
             ;; Skip failing test BaselineTest_cmd-org.
             ;; This is a known upstream issue. See
             ;; https://github.com/ledger/ledger/issues/550
             (setenv "ARGS" "-E BaselineTest_cmd-org")
             #t)))))
    (inputs
     `(("boost" ,boost)
       ("gmp" ,gmp)
       ("libedit" ,libedit)
       ("mpfr" ,mpfr)
       ("python" ,python-2)
       ("tzdata" ,tzdata)
       ("utfcpp" ,utfcpp)))
    (native-inputs
     `(("groff" ,groff)
       ("texinfo" ,texinfo)))
    (home-page "https://ledger-cli.org/")
    (synopsis "Command-line double-entry accounting program")
    (description
     "Ledger is a powerful, double-entry accounting system that is
accessed from the UNIX command-line.  This may put off some users, since
there is no flashy UI, but for those who want unparalleled reporting
access to their data there are few alternatives.

Ledger uses text files for input.  It reads the files and generates
reports; there is no other database or stored state.  To use Ledger,
you create a file of your account names and transactions, run from the
command line with some options to specify input and requested reports, and
get output.  The output is generally plain text, though you could generate
a graph or html instead.  Ledger is simple in concept, surprisingly rich
in ability, and easy to use.")
    ;; There are some extra licenses in files which do not presently get
    ;; installed when you build this package.  Different versions of the GPL
    ;; are used in the contrib and python subdirectories.  The bundled version
    ;; of utfcpp is under the Boost 1.0 license. Also the file
    ;; `tools/update_copyright_year` has an Expat license.
    (license (list license:bsd-3
                   license:asl2.0     ; src/strptime.cc
                   (license:non-copyleft
                    "file://src/wcwidth.cc"
                    "See src/wcwidth.cc in the distribution.")))))

(define-public geierlein
  (package
    (name "geierlein")
    (version "0.9.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stesie/geierlein.git")
             (commit (string-append "V" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00zpwr3lk2vdmd60fgdwdk0xxs52wvnm19ln2m75yfphydvkglic"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                  ; would require npm, python and a lot more
       #:phases
        (modify-phases %standard-phases
          (delete 'configure)           ; no configure script
          (add-after 'unpack 'override-target-directory-and-tool-paths
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (substitute* "Makefile"
                (("prefix := .*")
                 (string-append "prefix := " (assoc-ref outputs "out") "\n"))
                ;; Required for tests, unused for now:
                ;;(("PYTHON := .*")
                ;; (string-append (which "python") "\n")))
                (("INSTALL := .*")
                 (string-append "INSTALL := " (which "install") "\n")))
              (substitute* "bin/xgeierlein.in"
                ;; Use icecat as XULRUNNER
                (("^for search ")
                 (string-append "XULRUNNER=" (which "icecat") "\n"
                                "for search ")))
              #t)))))
    (inputs
     `(("icecat" ,icecat)))
    (home-page "https://stesie.github.io/geierlein/")
    (synopsis "Free Elster client, for sending Germany VAT declarations")
    (description
     "Geierlein is a free Elster client, i.e. an application that
allows to send VAT declarations to Germany's fiscal authorities.

Currently it is *not* possible to send returns that are due annually
(especially the income tax return) since the fiscal authority doesn't
allow to do that off the ERiC library (which is proprietary however).
It's not clear at the moment whether one day it will be possible to
do so.")
    (license license:agpl3+)))

(define-public electrum
  (package
    (name "electrum")
    (version "3.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.electrum.org/"
                           version "/Electrum-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0z2zfhyawrbzs0w1426a2w0d4wsajl34ymj77qmpm41138g2ysf2"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete the bundled dependencies.
           (delete-file-recursively "packages")
           #t))))
    (build-system python-build-system)
    (inputs
     `(("python-pyaes" ,python-pyaes)
       ("python-pysocks" ,python-pysocks)
       ("python-sip" ,python-sip)
       ("python-pyqt" ,python-pyqt)
       ("python-ecdsa" ,python-ecdsa)
       ("python-pbkdf2" ,python-pbkdf2)
       ("python-requests" ,python-requests)
       ("python-qrcode" ,python-qrcode)
       ("python-protobuf" ,python-protobuf)
       ("python-aiohttp" ,python-aiohttp)
       ("python-aiohttp-socks" ,python-aiohttp-socks)
       ("python-aiorpcx" ,python-aiorpcx)
       ("python-certifi" ,python-certifi)
       ("python-dnspython" ,python-dnspython)
       ("python-jsonrpclib-pelix" ,python-jsonrpclib-pelix)))
    (arguments
     `(#:tests? #f ;; package doesn't have any tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-home
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "setup.py"
               (("~/.local/share")
                (string-append (assoc-ref outputs "out") "/local/share"))))))))
    (home-page "https://electrum.org/")
    (synopsis "Bitcoin wallet")
    (description
     "Electrum is a lightweight Bitcoin client, based on a client-server
protocol.  It supports Simple Payment Verification (SPV) and deterministic key
generation from a seed.  Your secret keys are encrypted and are never sent to
other machines/servers.  Electrum does not download the Bitcoin blockchain.")
    (license license:expat)))

(define-public electron-cash
  (package
    (inherit electrum)
    (name "electron-cash")
    (version "3.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://electroncash.org/downloads/"
                           version
                           "/win-linux/Electron-Cash-"
                           version
                           ".tar.gz"))
       (sha256
        (base32
         "185z3c5j9nvl31ga80hvahx7ghvkgmqgfjrrzw1fbs6p9jxy007w"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete the bundled dependencies.
           (delete-file-recursively "packages")
           #t))))
    (home-page "https://electroncash.org/")
    (synopsis "Bitcoin Cash wallet")
    (description
     "Electroncash is a lightweight Bitcoin Cash client, based on a client-server
protocol.  It supports Simple Payment Verification (SPV) and deterministic key
generation from a seed.  Your secret keys are encrypted and are never sent to
other machines/servers.  Electroncash does not download the Bitcoin Cash blockchain.")
    (license license:expat)))

(define-public monero
  ;; This package bundles easylogging++ and lmdb.
  ;; The bundled easylogging++ is modified, and the changes will not be upstreamed.
  ;; The devs deem the lmdb driver too critical a consenus component, to use
  ;; the system's dynamically linked library.
  (package
    (name "monero")
    (version "0.12.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/monero-project/monero")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (patches (search-patches "monero-use-system-miniupnpc.patch"))
       (sha256
        (base32
         "14db9kgjm2ha93c2x5fjdw01xaqshn756qr3x2cnzyyjh7caz5qd"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("googletest" ,googletest)
       ("graphviz" ,graphviz)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("bind" ,isc-bind)
       ("boost" ,boost)
       ("zeromq" ,zeromq)
       ("cppzmq" ,cppzmq)
       ("expat" ,expat)
       ("libsodium" ,libsodium)
       ("libunwind" ,libunwind)
       ("lmdb" ,lmdb)
       ("miniupnpc" ,monero-miniupnpc)
       ("openssl" ,openssl)
       ("rapidjson" ,rapidjson)
       ("unbound" ,unbound)))
    (arguments
     `(#:out-of-source? #t
       #:build-type "release"
       #:configure-flags '("-DBUILD_TESTS=ON"
                           ,@(if (string=? "aarch64-linux" (%current-system))
                                 '("-DARCH=armv8-a")
                                 '())
                           "-DBUILD_GUI_DEPS=ON")
       #:phases
       (modify-phases %standard-phases
         ;; tests/core_tests need a valid HOME
         (add-before 'configure 'set-home
           (lambda _
             (setenv "HOME" (getcwd))
             #t))
         (add-after 'set-home 'fix-wallet-path-for-unit-tests
           (lambda _
             (substitute* "tests/unit_tests/serialization.cpp"
               (("\\.\\./\\.\\./\\.\\./\\.\\./") "../../"))
             #t))
         (add-after 'fix-wallet-path-for-unit-tests 'change-log-path
           (lambda _
             (substitute* "contrib/epee/src/mlog.cpp"
               (("epee::string_tools::get_current_module_folder\\(\\)")
                "\".bitmonero\""))
             (substitute* "contrib/epee/src/mlog.cpp"
               (("return \\(") "return ((std::string(getenv(\"HOME\"))) / "))
             #t))
         (replace 'check
           (lambda _
             (invoke "make" "ARGS=-E 'unit_tests|libwallet_api_tests'"
                     "test")))
         ;; The excluded unit tests need network access
         (add-after 'check 'unit-tests
           (lambda _
             (let ((excluded-unit-tests
                    (string-join
                     '("AddressFromURL.Success"
                       "AddressFromURL.Failure"
                       "DNSResolver.IPv4Success"
                       "DNSResolver.DNSSECSuccess"
                       "DNSResolver.DNSSECFailure"
                       "DNSResolver.GetTXTRecord")
                     ":")))
               (invoke "tests/unit_tests/unit_tests"
                       (string-append "--gtest_filter=-"
                                      excluded-unit-tests)))))
         (add-after 'install 'install-blockchain-import-export
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "bin/monero-blockchain-import" bin)
               (install-file "bin/monero-blockchain-export" bin)))))))
    (home-page "https://getmonero.org/")
    (synopsis "Command-line interface to the Monero currency")
    (description
     "Monero is a secure, private, untraceable currency.  This package provides the
Monero command line client and daemon.")
    (license license:bsd-3)))

(define-public monero-gui
  (package
    (name "monero-gui")
    (version "0.12.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/monero-project/monero-gui")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1cnrkwh7kp64lnzz1xfmkf1mhsgm5gls292gpqai3jr8jydpkahl"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("graphviz" ,graphviz)
       ("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     `(("boost" ,boost)
       ("libunwind" ,libunwind)
       ("openssl" ,openssl)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtgraphicaleffects" ,qtgraphicaleffects)
       ("qtquickcontrols" ,qtquickcontrols)
       ("readline" ,readline)
       ("unbound" ,unbound)))
    (propagated-inputs
     `(("monero" ,monero)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (add-before 'build 'fix-makefile-vars
           (lambda _
             (substitute* "src/zxcvbn-c/makefile"
               (("\\?=") "="))
             #t))
         (add-after 'fix-makefile-vars 'fix-library-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "monero-wallet-gui.pro"
               (("-L/usr/local/lib") "")
               (("-L/usr/local/opt/openssl/lib")
                (string-append "-L"
                               (assoc-ref inputs "openssl")
                               "/lib"))
               (("-L/usr/local/opt/boost/lib")
                (string-append "-L"
                               (assoc-ref inputs "boost")
                               "/lib")))
             #t))
         (add-after 'fix-library-paths 'fix-monerod-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/daemon/DaemonManager.cpp"
               (("QApplication::applicationDirPath\\(\\) \\+ \"/monerod")
                (string-append "\""(assoc-ref inputs "monero")
                               "/bin/monerod")))
             #t))
         (add-after 'fix-monerod-path 'fix-qt-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((qttools  (assoc-ref inputs "qttools"))
                    (lrelease (string-append qttools "/bin/lrelease"))
                    (lupdate (string-append qttools "/bin/lupdate")))
               (substitute* "monero-wallet-gui.pro"
                 (("\\$\\$\\[QT_INSTALL_BINS\\]/lrelease") lrelease)
                 (("\\$\\$\\[QT_INSTALL_BINS\\]/lupdate") lupdate))
               #t)))
         (replace 'build
           (lambda _
             (invoke "./build.sh")))
         (add-after 'build 'fix-install-path
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "build/Makefile"
               (("/opt/monero-wallet-gui")
                (assoc-ref outputs "out")))
             #t))
         (add-before 'install 'change-dir
           (lambda _
             (chdir "build")))
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/monero-wallet-gui")
                 `("QT_PLUGIN_PATH" ":" prefix
                   ,(map (lambda (label)
                           (string-append (assoc-ref inputs label)
                                          "/lib/qt5/plugins"))
                         '("qtbase" "qtdeclarative")))
                 `("QML2_IMPORT_PATH" ":" prefix
                   ,(map (lambda (label)
                           (string-append (assoc-ref inputs label)
                                          "/lib/qt5/qml"))
                         '("qtdeclarative" "qtgraphicaleffects"
                           "qtquickcontrols"))))
               #t))))))
    (home-page "https://getmonero.org/")
    (synopsis "Graphical user interface for the Monero currency")
    (description
     "Monero is a secure, private, untraceable currency.  This package provides the
Monero GUI client.")
    (license license:bsd-3)))

(define-public monero-core
  (deprecated-package "monero-core" monero-gui))

(define-public python-trezor-agent
  (package
    (name "python-trezor-agent")
    (version "0.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/romanz/trezor-agent.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i4igkxi8fwdlbhg6nx27lhnc9v9nmrw4j5fvpnc202n6yjlc7x7"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-requires-backports-shutil-which
           ;; Remove requires on backport of shutil_which, as python 3.4+ has
           ;; a built-in implementation supported in python-trezor-agent.
           (lambda _
             (substitute* "setup.py"
               (("'backports.shutil_which>=3.5.1',") ""))
             #t))
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key outputs inputs #:allow-other-keys)
             ;; Make installed package available for running the tests.
             (add-installed-pythonpath inputs outputs)
             (invoke "py.test"))))))
    (propagated-inputs
     `(("python-configargparse" ,python-configargparse)
       ("python-daemon" ,python-daemon)
       ("python-docutils" ,python-docutils)
       ("python-ecdsa" ,python-ecdsa)
       ("python-ed25519" ,python-ed25519)
       ("python-mnemonic" ,python-mnemonic)
       ("python-pymsgbox" ,python-pymsgbox)
       ("python-semver" ,python-semver)
       ("python-unidecode" ,python-unidecode)
       ("python-wheel" ,python-wheel)))
    (native-inputs
     `(("gnupg" ,gnupg)
       ("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/romanz/trezor-agent")
    (synopsis "Use hardware wallets as SSH and GPG agent")
    (description
     "@code{libagent} is a library that allows using TREZOR, Keepkey and
Ledger Nano as a hardware SSH/GPG agent.")
    (license license:lgpl3)))

(define-public python2-trezor-agent
  (package-with-python2 python-trezor-agent))

(define-public python-mnemonic
  (package
    (name "python-mnemonic")
    (version "0.18")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "mnemonic" version))
        (sha256
          (base32
            "07bzfa5di6nv5xwwcwbypnflpj50wlfczhh6q6hg8w13g5m319q2"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pbkdf2" ,python-pbkdf2)))
    (home-page "https://github.com/trezor/python-mnemonic")
    (synopsis "Implementation of Bitcoin BIP-0039")
    (description "@code{mnemonic} is a library that provides an implementation
of Bitcoin BIP-0039.")
    (license license:expat)))

(define-public python2-mnemonic
  (package-with-python2 python-mnemonic))

(define-public python-ledgerblue
  (package
    (name "python-ledgerblue")
    (version "0.1.16")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ledgerblue" version))
        (sha256
          (base32
            "010mghaqh1cmz3a0ifc3f40mmyplilwlw7kpha2mzyrrff46p9gb"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-ecpy" ,python-ecpy)
       ("python-future" ,python-future)
       ("python-hidapi" ,python-hidapi)
       ("python-pillow" ,python-pillow)
       ("python-protobuf" ,python-protobuf)
       ("python-pycrypto" ,python-pycrypto)))
    (home-page "https://github.com/LedgerHQ/blue-loader-python")
    (synopsis "Python library to communicate with Ledger Blue/Nano S")
    (description "@code{ledgerblue} is a Python library to communicate with
Ledger Blue/Nano S.")
    (license license:asl2.0)))

(define-public python2-ledgerblue
  (package-with-python2 python-ledgerblue))

(define-public python-trezor
  (package
    (name "python-trezor")
    (version "0.11.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "trezor" version))
        (sha256
          (base32
            "064yds8f4px0c6grkkanpdjx022g4q87ihzhkmdv9qanv0hz6hv0"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          ;; Default tests run device-specific tests which fail, only run specific tests.
          (replace 'check
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; Delete tests that require network access.
              (delete-file "trezorlib/tests/unit_tests/test_tx_api.py")
              (invoke "python" "-m" "pytest" "--pyarg" "trezorlib.tests.unit_tests"))))))
    (propagated-inputs
     `(("python-click" ,python-click)
       ("python-construct" ,python-construct)
       ("python-ecdsa" ,python-ecdsa)
       ("python-libusb1" ,python-libusb1)
       ("python-mnemonic" ,python-mnemonic)
       ("python-pyblake2" ,python-pyblake2)
       ("python-requests" ,python-requests)
       ("python-typing-extensions" ,python-typing-extensions)))
    (native-inputs
     `(("protobuf" ,protobuf) ; Tests
       ("python-black" ,python-black) ; Tests
       ("python-protobuf" ,python-protobuf) ; Tests
       ("python-isort" ,python-isort) ; Tests
       ("python-pyqt" ,python-pyqt) ; Tests
       ("python-pytest" ,python-pytest))) ; Tests
    (home-page "https://github.com/trezor/python-trezor")
    (synopsis "Python library for communicating with TREZOR Hardware Wallet")
    (description "@code{trezor} is a Python library for communicating with
TREZOR Hardware Wallet.")
    (license license:lgpl3)))

(define-public python2-trezor
  (package-with-python2 python-trezor))

(define-public python-keepkey
  (package
    (name "python-keepkey")
    (version "6.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "keepkey" version))
        (sha256
          (base32
            "16j8hnxj9r4b2w6kfncmny09pb1al8ppmn59qxzl3qmh1xhpy45g"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (apply invoke "python" (find-files "tests/unit" "\\.py$")))))))
    (propagated-inputs
     `(("python-ecdsa" ,python-ecdsa)
       ("python-hidapi" ,python-hidapi)
       ("python-libusb1" ,python-libusb1)
       ("python-mnemonic" ,python-mnemonic)
       ("python-protobuf" ,python-protobuf)))
    (home-page "https://github.com/keepkey/python-keepkey")
    (synopsis "Python library for communicating with KeepKey Hardware Wallet")
    (description "@code{keepkey} is a Python library for communicating with
the KeepKey Hardware Wallet.")
    (license license:lgpl3)))

(define-public python2-keepkey
  (package-with-python2 python-keepkey))

(define-public ledger-agent
  (package
    (name "ledger-agent")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ledger_agent" version))
       (sha256
        (base32
         "03zj602m2rln9yvr08dswy56vzkbldp8b074ixwzz525dafblr92"))))
    (build-system python-build-system)
    (inputs
     `(("python-ledgerblue" ,python-ledgerblue)
       ("python-trezor-agent" ,python-trezor-agent)))
    (home-page "http://github.com/romanz/trezor-agent")
    (synopsis "Ledger as hardware SSH/GPG agent")
    (description "This package allows using Ledger as hardware SSH/GPG agent.

Usage for SSH: trezor-agent foo@@example.com --connect
Usage for GPG: Initialize using trezor-gpg init \"Foo <foo@@example.com>\"
Then set the environment variable GNUPGHOME to
\"${HOME}/.gnupg/trezor\".")
    (license license:lgpl3)))

(define-public trezor-agent
  (package
    (name "trezor-agent")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trezor_agent" version))
       (sha256
        (base32
         "144657c7bn0a667dq5fv5r6j7iilxf3h9agj29v1m2qpq40g0az8"))))
    (arguments
     ;; Tests fail with "AttributeError: module 'attr' has no attribute 's'".
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'wrap 'fixup-agent-py
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               ;; overwrite the wrapper with the real thing.
               (install-file "./trezor_agent.py"
                             (string-append out "/bin"))
             #t))))))
    (build-system python-build-system)
    (inputs
     `(("python-trezor" ,python-trezor)
       ("python-trezor-agent" ,python-trezor-agent)))
    (native-inputs
     `(("python-hidapi" ,python-hidapi)))
    (home-page "https://github.com/romanz/trezor-agent")
    (synopsis "Using Trezor as hardware SSH/GPG agent")
    (description "This package allows using Trezor as a hardware SSH/GPG
agent.")
    (license license:lgpl3)))

(define-public keepkey-agent
  (package
    (name "keepkey-agent")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "keepkey_agent" version))
        (sha256
          (base32
            "03779gvlx70i0nnry98i4pl1d92604ix5x6jgdfkrdgzqbh5vj27"))))
    (build-system python-build-system)
    (inputs
     `(("python-keepkey" ,python-keepkey)
       ("python-trezor-agent" ,python-trezor-agent)))
    (home-page "http://github.com/romanz/trezor-agent")
    (synopsis "KeepKey as hardware SSH/GPG agent")
    (description "This package allows using KeepKey as a hardware SSH/GPG
agent.")
    (license license:lgpl3)))

(define-public python-stdnum
  (package
    (name "python-stdnum")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-stdnum" version))
       (sha256
        (base32
         "0hvr47q32xbyiznpmbg4r8rcvxhnf0lwf33hcpnynyik57djy5np"))))
    (build-system python-build-system)
    (home-page
     "https://arthurdejong.org/python-stdnum/")
    (synopsis
     "Python module to handle standardized number and code formats")
    (description
     "This is a Python library that aims to provide functions to handle,
parse and validate standard numbers.
The module supports more than 100 different number formats
amongst which a great number of VAT and other tax numbers,
personal identity and company identification codes,
international standard numbers (ISBN, IBAN, EAN, etc.)
and various other formats.
The module also inclused implementations of the Verhoeff,
Luhn and family of ISO/IEC 7064 check digit algorithms. ")
    (license license:lgpl2.1+)))

(define-public python2-stdnum
  (package-with-python2 python-stdnum))

(define-public python-duniterpy
  (package
    (name "python-duniterpy")
    (version "0.52.0")
    (source
     (origin
       (method git-fetch)
       ;; Pypi's default URI is missing "requirements.txt" file.
       (uri (git-reference
             (url "https://git.duniter.org/clients/python/duniterpy.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "07liba2d21hb8m3n6yccfamq9yq0ryywh18vs9g2sgywfsnv82lh"))))
    (build-system python-build-system)
    (arguments
     ;; Tests fail with "AttributeError: module 'attr' has no attribute 's'".
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-documentation
           (lambda _
             (invoke "make" "docs")))
         (add-after 'build-documentation 'install-documentation
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name)))
               (mkdir-p doc)
               (copy-recursively "docs/_build/html" doc))
             #t)))))
    (native-inputs
     `(("sphinx" ,python-sphinx)
       ("sphinx-rtd-theme" ,python-sphinx-rtd-theme)))
    (propagated-inputs
     `(("aiohttp" ,python-aiohttp)
       ("attr" ,python-attr)
       ("base58" ,python-base58)
       ("jsonschema" ,python-jsonschema)
       ("libnacl" ,python-libnacl)
       ("pyaes" ,python-pyaes)
       ("pylibscrypt" ,python-pylibscrypt)
       ("pypeg2" ,python-pypeg2)))
    (home-page "https://git.duniter.org/clients/python/duniterpy")
    (synopsis "Python implementation of Duniter API")
    (description "@code{duniterpy} is an implementation of
@uref{https://github.com/duniter/duniter/, duniter} API.  Its
main features are:
@itemize
@item Support Duniter's Basic Merkle API and protocol
@item Asynchronous/synchronous without threads
@item Support HTTP, HTTPS and Web Socket transport for Basic Merkle API
@item Support Elasticsearch Duniter4j API
@item Duniter signing key
@item Sign/verify and encrypt/decrypt messages with the Duniter credentials
@end itemize")
    (license license:gpl3+)))

(define-public silkaj
  (package
    (name "silkaj")
    (version "0.6.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.duniter.org/clients/python/silkaj.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1fy509vsmz7rs9m3vah0ky0jvq9mxmfga6b18rkrkl2lbjk872q2"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;no test
    (inputs
     `(("click" ,python-click)
       ("duniterpy" ,python-duniterpy)
       ("ipaddress" ,python-ipaddress)
       ("pynacl" ,python-pynacl)
       ("tabulate" ,python-tabulate)))
    (home-page "https://silkaj.duniter.org/")
    (synopsis "Command line client for Duniter network")
    (description "@code{Silkaj} is a command line client for the
@uref{https://github.com/duniter/duniter/, Duniter} network.

Its features are:
@itemize
@item information about currency,
@item issuers difficulty to generate next block,
@item network view of nodes,
@item list of last issuers,
@item send transactions,
@item get account amount.
@end itemize")
    (license license:agpl3+)))
