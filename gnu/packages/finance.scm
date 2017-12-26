;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017 Vasile Dumitrascu <va511e@yahoo.com>
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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
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
    (version "0.15.1")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "https://bitcoin.org/bin/bitcoin-core-"
                             version "/bitcoin-" version ".tar.gz"))
             (sha256
              (base32
               "1d22fgwdcn343kd95lh389hj417zwbmnhi29cij8n7wc0nz2vpil"))))
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
          (add-before 'check 'set-home
           (lambda _
            (setenv "HOME" (getenv "TMPDIR"))))))) ; Tests write to $HOME.
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
    (version "3.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ledger/ledger/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "12jlv3gsjhrja25q9hrwh73cdacd2l3c2yyn8qnijav9mdhnbw4h"))))
    (build-system cmake-build-system)
    (arguments
     `(#:modules ((guix build cmake-build-system)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules (,@%cmake-build-system-modules
                           (guix build emacs-utils))
       #:configure-flags
       `("-DBUILD_DOCS:BOOL=ON"
         "-DBUILD_WEB_DOCS:BOOL=ON"
         "-DBUILD_EMACSLISP:BOOL=ON"
         "-DUSE_PYTHON:BOOL=ON"
         "-DCMAKE_INSTALL_LIBDIR:PATH=lib"
         ,(string-append "-DUTFCPP_INCLUDE_DIR:PATH="
                         (assoc-ref %build-inputs "utfcpp")
                         "/include"))
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
           (lambda _ (zero? (system* "make" "doc"))))
         (add-before 'check 'check-setup
           ;; One test fails if it can't set the timezone.
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZDIR"
                     (string-append (assoc-ref inputs "tzdata")
                                    "/share/zoneinfo"))
             #t))
         (add-after 'install 'relocate-elisp
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((site-dir (string-append (assoc-ref outputs "out")
                                             "/share/emacs/site-lisp"))
                    (guix-dir (string-append site-dir "/guix.d"))
                    (orig-dir (string-append site-dir "/ledger-mode"))
                    (dest-dir (string-append guix-dir "/ledger-mode")))
               (mkdir-p guix-dir)
               (rename-file orig-dir dest-dir)
               (emacs-generate-autoloads ,name dest-dir))
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
     `(("emacs" ,emacs-minimal)
       ("groff" ,groff)
       ("texinfo" ,texinfo)))
    (home-page "http://ledger-cli.org/")
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
                    "See src/wcwidth.cc in the distribution.")
                   license:gpl2+))))  ; lisp/*

(define-public geierlein
  (package
    (name "geierlein")
    (version "0.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/stesie/geierlein"
                           "/archive/V" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0b11fq8v5w8nxjb20jl4dsfhv76xky6n3sq3k3fbb0m2sq9ikikw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; would require npm, python and a lot more
       #:phases
        (modify-phases %standard-phases
          (delete 'configure)
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
    (home-page "http://stesie.github.io/geierlein/")
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
    (version "3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.electrum.org/"
                           version "/Electrum-"
                           version ".tar.gz"))
       (sha256
        (base32
         "184cmpfqcznnm0wfjiarb6dps2vs0s2aykmy2ji7p77x20fbisfi"))
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

(define-public monero
  ;; This package bundles easylogging++ and lmdb.
  ;; The bundled easylogging++ is modified, and the changes will not be upstreamed.
  ;; The devs deem the lmdb driver too critical a consenus component, to use
  ;; the system's dynamically linked library.
  (package
    (name "monero")
    (version "0.11.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/monero-project/monero/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete bundled dependencies.
           (for-each
            delete-file-recursively
            '("external/miniupnpc" "external/rapidjson"
              "external/unbound"))
           #t))
       (sha256
        (base32
         "16shd834025jyzy68h3gag1sz8vbk875hy4j97hrki8pacz8vd5m"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("googletest" ,googletest)
       ("graphviz" ,graphviz)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("bind" ,isc-bind)
       ("boost" ,boost)
       ("expat" ,expat)
       ("libunwind" ,libunwind)
       ("lmdb" ,lmdb)
       ("miniupnpc" ,miniupnpc)
       ("openssl" ,openssl)
       ("rapidjson" ,rapidjson)
       ("unbound" ,unbound)))
    (arguments
     `(#:out-of-source? #t
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
             (zero?
              (system* "make" "ARGS=-E 'unit_tests|libwallet_api_tests'"
                       "test"))))
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
               (zero?
                (system* "tests/unit_tests/unit_tests"
                         (string-append "--gtest_filter=-"
                                        excluded-unit-tests))))))
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

(define-public monero-core
  (package
    (name "monero-core")
    (version "0.11.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/monero-project/monero-core/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1q7a9kpcjgp74fbplzs2iszdld6gwbfrydyd9in9izhwp100p1rr"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("graphviz" ,graphviz)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("boost" ,boost)
       ("libunwind" ,libunwind)
       ("openssl" ,openssl)
       ("qt" ,qt)
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
         (replace 'build
           (lambda _
             (zero? (system* "./build.sh"))))
         (add-after 'build 'fix-install-path
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "build/Makefile"
               (("/opt/monero-wallet-gui")
                (assoc-ref outputs "out")))
             #t))
         (add-before 'install 'change-dir
           (lambda _
             (chdir "build"))))))
    (home-page "https://getmonero.org/")
    (synopsis "Graphical user interface for the Monero currency")
    (description
     "Monero is a secure, private, untraceable currency.  This package provides the
Monero GUI client.")
    (license license:bsd-3)))

(define-public python-trezor-agent
  (package
    (name "python-trezor-agent")
    (version "0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/romanz/trezor-agent/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0h8jb147vpjk7mqbl4za0xdh7lblhx07n9dfk80kn2plwnvrry1x"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key outputs inputs #:allow-other-keys)
             ;; Make installed package available for running the tests
             (add-installed-pythonpath inputs outputs)
             (invoke "py.test"))))))
    (propagated-inputs
     `(("python-ecdsa" ,python-ecdsa)
       ("python-ed25519" ,python-ed25519)
       ("python-semver" ,python-semver)
       ("python-unidecode" ,python-unidecode)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/romanz/trezor-agent")
    (synopsis "TREZOR SSH and GPG host support")
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
