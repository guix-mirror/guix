;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2018, 2019, 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019, 2020, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2019, 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2019 Sebastian Schott <sschott@mailbox.org>
;;; Copyright © 2020 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2020 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2020 Tom Zander <tomz@freedommail.ch>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020, 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Carlo Holl <carloholl@gmail.com>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2021 ZmnSCPxj jxPCSnmZ <ZmnSCPxj@protonmail.com>
;;; Copyright © 2021 François J <francois-oss@avalenn.eu>
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
  #:use-module (guix build-system copy)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system python)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system go)
  #:use-module (guix build-system qt)
  #:use-module (guix deprecation)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gnuzilla))

(define-public bitcoin-core-0.21
  (package
    (name "bitcoin-core")
    (version "0.21.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://bitcoincore.org/bin/bitcoin-core-"
                              version "/bitcoin-" version ".tar.gz"))
              (sha256
               (base32
                "1q51nqv64lhng5wh1cqb01jar7iswpnyyb1i7xslbkr0j9227zya"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)               ; for the tests
       ("util-linux" ,util-linux)       ; provides the hexdump command for tests
       ("qttools" ,qttools)))
    (inputs
     `(("bdb" ,bdb-4.8)                 ; 4.8 required for compatibility
       ("boost" ,boost)
       ("libevent" ,libevent)
       ("miniupnpc" ,miniupnpc)
       ("openssl" ,openssl)
       ("qtbase" ,qtbase-5)))
    (arguments
     `(#:configure-flags
       (list
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
         (add-before 'build 'set-no-git-flag
           (lambda _
             ;; Make it clear we are not building from within a git repository
             ;; (and thus no information regarding this build is available
             ;; from git).
             (setenv "BITCOIN_GENBUILD_NO_GIT" "1")
             #t))
         (add-before 'check 'set-home
           (lambda _
             (setenv "HOME" (getenv "TMPDIR")) ; tests write to $HOME
             #t))
         (add-after 'check 'check-functional
           (lambda _
             (invoke
              "python3" "./test/functional/test_runner.py"
              (string-append "--jobs=" (number->string (parallel-job-count))))
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

(define-public bitcoin-core-0.20
  (package
    (inherit bitcoin-core-0.21)
    (version "0.20.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://bitcoincore.org/bin/bitcoin-core-"
                              version "/bitcoin-" version ".tar.gz"))
              (sha256
               (base32
                "0y5rad68b398arh0abr2wgiwybdw0i5a4dxz9s3fk9fgdbyn5gab"))))))

;; The support lifetimes for bitcoin-core versions can be found in
;; <https://bitcoincore.org/en/lifecycle/#schedule>.

(define-public bitcoin-core bitcoin-core-0.21)

(define-public hledger
  (package
    (name "hledger")
    (version "1.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hledger/hledger-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1si9zqparkdq77yji87lhcsrf11fr3gisqwsv82cabhrhc36x6l4"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-base-compat-batteries" ,ghc-base-compat-batteries)
       ("ghc-cmdargs" ,ghc-cmdargs)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-decimal" ,ghc-decimal)
       ("ghc-diff" ,ghc-diff)
       ("ghc-easytest" ,ghc-easytest)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-hledger-lib" ,ghc-hledger-lib)
       ("ghc-lucid" ,ghc-lucid)
       ("ghc-math-functions" ,ghc-math-functions)
       ("ghc-megaparsec" ,ghc-megaparsec)
       ("ghc-mtl-compat" ,ghc-mtl-compat)
       ("ghc-old-time" ,ghc-old-time)
       ("ghc-pretty-show" ,ghc-pretty-show)
       ("ghc-regex-tdfa" ,ghc-regex-tdfa)
       ("ghc-safe" ,ghc-safe)
       ("ghc-shakespeare" ,ghc-shakespeare)
       ("ghc-split" ,ghc-split)
       ("ghc-tabular" ,ghc-tabular)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-utility-ht" ,ghc-utility-ht)
       ("ghc-wizards" ,ghc-wizards)))
    (native-inputs
     `(("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)))
    (home-page "https://hledger.org")
    (synopsis "Command-line interface for the hledger accounting system")
    (description
     "The command-line interface for the hledger accounting system.  Its basic
function is to read a plain text file describing financial transactions and
produce useful reports.

hledger is a robust, cross-platform set of tools for tracking money, time, or
any other commodity, using double-entry accounting and a simple, editable file
format, with command-line, terminal and web interfaces.  It is a Haskell
rewrite of Ledger, and one of the leading implementations of Plain Text
Accounting.")
    (license license:gpl3)))

(define-public homebank
  (package
    (name "homebank")
    (version "5.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://homebank.free.fr/public/homebank-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "14qhv79a2waqzmf6l571wklgwq8j1pkmjvzkj5vhh44nia8hfdh7"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("gtk+" ,gtk+)
       ("libofx" ,libofx)
       ("libsoup" ,libsoup)))
    (home-page "http://homebank.free.fr/")
    (synopsis "Graphical personal accounting application")
    (description "HomeBank allows you to manage your personal accounts at
home.  The seeks to be lightweight, simple and easy to use.  It brings
features that allow you to analyze your finances in a detailed way instantly
and dynamically with report tools based on filtering and graphical charts.")
    (license license:gpl2+)))

(define-public ledger
  (package
    (name "ledger")
    (version "3.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ledger/ledger")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x6jxwss3wwzbzlwmnwb8yzjk8f9wfawif4f1b74z2qg6hc4r7f6"))))
    (build-system cmake-build-system)
    (arguments
     `(#:modules (,@%cmake-build-system-modules
                  ((guix build python-build-system) #:select (python-version)))
       #:imported-modules (,@%cmake-build-system-modules
                           (guix build python-build-system))
       #:configure-flags
       `("-DBUILD_DOCS:BOOL=ON"
         "-DBUILD_WEB_DOCS:BOOL=ON"
         "-DUSE_PYTHON:BOOL=ON"
         "-DCMAKE_INSTALL_LIBDIR:PATH=lib")
       #:phases
       (modify-phases (@ (guix build cmake-build-system) %standard-phases)
         (add-after 'unpack 'fix-python-installation-directory
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; By default the package attempts to install its Python bindings
             ;; to the Python store directory, which obviously does not work.
             ;; Passing -DPython_SITEARCH in #:configure-flags has no effect.
             (let ((python-version (python-version (assoc-ref inputs "python")))
                   (out (assoc-ref outputs "out")))
               (substitute* "src/CMakeLists.txt"
                 (("DESTINATION \\$\\{Python_SITEARCH\\}")
                  (string-append "DESTINATION " out "/lib/python"
                                 python-version "/site-packages")))
               #t)))
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
       ("python" ,python)
       ("utfcpp" ,utfcpp)))
    (native-inputs
     `(("groff" ,groff)
       ("texinfo" ,texinfo)
       ("tzdata" ,tzdata-for-tests)))
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

(define-public emacs-ledger-mode
  (package
    (name "emacs-ledger-mode")
    (version "4.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ledger/ledger-mode")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r5rcyxd6d1rqwamzpvqdbkbdf1zbj75aaciqijrklnm59ps244y"))))
    (build-system emacs-build-system)
    (arguments
     `(;; ledger-test.el is needed at runtime (but probably not for a good reason).
       #:exclude '()
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((ledger (assoc-ref inputs "ledger")))
               (make-file-writable "ledger-exec.el")
               (emacs-substitute-variables "ledger-exec.el"
                 ("ledger-binary-path" (string-append ledger "/bin/ledger"))))
             #t))
         (add-after 'build 'build-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((target (string-append (assoc-ref outputs "out")
                                          "/share/info")))
               (mkdir-p target)
               (invoke "makeinfo" "-o" target
                       "../source/doc/ledger-mode.texi"))
             #t))
         (replace 'check
           (lambda _
             (with-directory-excursion "../source/test"
               (invoke "make" "test-batch")))))))
    (inputs
     `(("ledger" ,ledger)))
    (native-inputs
     `(("texinfo" ,texinfo)))
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
in ability, and easy to use.

This package provides the Emacs mode.")
    (license license:gpl2+)))

(define-public geierlein
  (package
    (name "geierlein")
    (version "0.9.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stesie/geierlein")
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
sends VAT declarations to Germany's fiscal authorities.

Currently it is *not* possible to send returns that are due annually
(especially the income tax return) since the fiscal authority doesn't
allow doing that off the ERiC library (which is proprietary however).
It's not clear at the moment whether one day it will be possible to
do so.")
    (license license:agpl3+)))

(define-public electrum
  (package
    (name "electrum")
    (version "4.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.electrum.org/"
                           version "/Electrum-"
                           version ".tar.gz"))
       (sha256
        (base32 "188r4zji985z8pm9b942xhmvv174yndk6jxagxl7ljk03wl2wiwi"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete the bundled dependencies.
           (delete-file-recursively "packages")
           #t))))
    (build-system python-build-system)
    (inputs
     `(("python-pyqt" ,python-pyqt)
       ("python-qrcode" ,python-qrcode)
       ("python-protobuf" ,python-protobuf)
       ("python-aiohttp" ,python-aiohttp)
       ("python-aiohttp-socks" ,python-aiohttp-socks)
       ("python-aiorpcx" ,python-aiorpcx-0.18)
       ("python-certifi" ,python-certifi)
       ("python-bitstring" ,python-bitstring)
       ("python-attrs" ,python-attrs)
       ("python-cryptography" ,python-cryptography)
       ("python-qdarkstyle" ,python-qdarkstyle)
       ("python-dnspython" ,python-dnspython)
       ("python-hidapi" ,python-hidapi)
       ("python-ledgerblue" ,python-ledgerblue)
       ("python-btchip-python" ,python-btchip-python)
       ("libsecp256k1" ,libsecp256k1)))
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-prefix
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; setup.py installs to ~/.local/share if sys.prefix/share isn't
               ;; writable.  sys.prefix points to Python's, not our, --prefix.
               (mkdir-p (string-append out "/share"))
               (substitute* "setup.py"
                 (("sys\\.prefix")
                  (format #f "\"~a\"" out)))
               #t)))
         (add-after 'unpack 'use-libsecp256k1-input
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "electrum/ecc_fast.py"
               (("library_paths = .* 'libsecp256k1.so.0'.")
                (string-append "library_paths = ('"
                               (assoc-ref inputs "libsecp256k1")
                               "/lib/libsecp256k1.so.0'"))))))))
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
    (name "electron-cash")
    (version "4.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Electron-Cash/Electron-Cash")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fj797hbinxsqjwhh4l1vjsx1vzmgzf2apq7fnqqwpv9g0v2pch0"))))
    (build-system python-build-system)
    (inputs
     `(("libevent" ,libevent)
       ("libsecp256k1" ,libsecp256k1-bitcoin-cash)
       ("openssl" ,openssl)
       ("python-cython" ,python-cython)
       ("python-dateutil" ,python-dateutil)
       ("python-dnspython" ,python-dnspython)
       ("python-ecdsa" ,python-ecdsa)
       ("python-hidapi" ,python-hidapi)
       ("python-jsonrpclib-pelix" ,python-jsonrpclib-pelix)
       ("python-keepkey" ,python-keepkey)
       ("python-pathvalidate" ,python-pathvalidate)
       ("python-protobuf" ,python-protobuf)
       ("python-pyaes" ,python-pyaes)
       ("python-pyqt" ,python-pyqt)
       ("python-pysocks" ,python-pysocks)
       ("python-qrcode" ,python-qrcode)
       ("python-requests" ,python-requests)
       ("python-stem" ,python-stem)
       ("python-trezor" ,python-trezor)
       ("qtsvg" ,qtsvg)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f                      ; no tests
       #:modules ((guix build python-build-system)
                  (guix build qt-utils)
                  (guix build utils))
       #:imported-modules (,@%python-build-system-modules
                           (guix build qt-utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'create-output-directories
           (lambda* (#:key outputs #:allow-other-keys)
             ;; setup.py installs to ~/.local/share if this doesn't exist.
             (mkdir-p (string-append (assoc-ref outputs "out") "/share"))))
         (add-after 'unpack 'use-libsecp256k1-input
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "electroncash/secp256k1.py"
               (("library_paths = .* 'libsecp256k1.so.0'.")
                (string-append "library_paths = ('"
                               (assoc-ref inputs "libsecp256k1")
                               "/lib/libsecp256k1.so.0'")))))
         (add-after 'install 'wrap-qt
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-qt-program "electron-cash" #:output out #:inputs inputs))
             #t)))))
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
  ;; The bundled easylogging++ is modified, and the changes will not be
  ;; upstreamed.
  ;; The devs deem the lmdb driver too critical a consenus component, to use
  ;; the system's dynamically linked library.
  (package
    (name "monero")
    (version "0.17.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/monero-project/monero")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (patches (search-patches "monero-use-system-miniupnpc.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete bundled dependencies.
           (for-each
            delete-file-recursively
            '("external/miniupnp" "external/rapidjson"
              "external/unbound"))
           #t))
       (sha256
        (base32 "0nax991fshfh51grhh2ryfrwwws35k16gzl1l3niva28zff2xmq6"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("graphviz" ,graphviz)
       ("pkg-config" ,pkg-config)
       ("protobuf" ,protobuf)
       ("python" ,python)
       ("qttools" ,qttools)))
    (inputs
     `(("boost" ,boost)
       ("cppzmq" ,cppzmq)
       ("expat" ,expat)
       ("hidapi" ,hidapi)
       ("libsodium" ,libsodium)
       ("libunwind" ,libunwind)
       ("libusb" ,libusb)
       ("miniupnpc" ,miniupnpc)
       ("openssl" ,openssl)
       ("protobuf" ,protobuf)
       ("rapidjson" ,rapidjson)
       ("readline" ,readline)
       ("unbound" ,unbound)
       ("xz" ,xz)
       ("zeromq" ,zeromq)))
    (arguments
     `(#:out-of-source? #t
       #:configure-flags
       (list "-DARCH=default"
             "-DBUILD_TESTS=ON"
             (string-append "-DReadline_ROOT_DIR="
                            (assoc-ref %build-inputs "readline")))
       #:phases
       (modify-phases %standard-phases
         ;; tests/core_tests need a valid HOME
         (add-before 'configure 'set-home
           (lambda _
             (setenv "HOME" (getcwd))
             #t))
         (add-after 'set-home 'change-log-path
           (lambda _
             (substitute* "contrib/epee/src/mlog.cpp"
               (("epee::string_tools::get_current_module_folder\\(\\)")
                "\".bitmonero\"")
               (("return \\(")
                "return ((std::string(getenv(\"HOME\"))) / "))
             #t))
         (add-after 'change-log-path 'fix-file-permissions-for-tests
           (lambda _
             (for-each make-file-writable
                       (find-files "tests/data/" "wallet_9svHk1.*"))
             #t))
         ;; Only try tests that don't need access to network or system
         (replace 'check
           (lambda _
             ;; Core tests sometimes fail, at least on i686-linux.
             ;; Let's disable them for now and just try hash tests
             ;; and unit tests.
             ;; (invoke "make" "ARGS=-R 'hash|core_tests' --verbose" "test")))
             (invoke "make" "ARGS=-R 'hash' --verbose" "test")))
         (add-after 'check 'unit-tests
           (lambda _
             (let ((excluded-unit-tests
                    (string-join
                     '("AddressFromURL.Success"
                       "AddressFromURL.Failure"
                       "DNSResolver.IPv4Success"
                       "DNSResolver.DNSSECSuccess"
                       "DNSResolver.DNSSECFailure"
                       "DNSResolver.GetTXTRecord"
                       "is_hdd.linux_os_root")
                     ":")))
               (invoke "tests/unit_tests/unit_tests"
                       (string-append "--gtest_filter=-"
                                      excluded-unit-tests)))))
         (add-after 'install 'delete-unused-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (delete-file-recursively (string-append out "/include")))
             #t)))))
    (home-page "https://web.getmonero.org/")
    (synopsis "Command-line interface to the Monero currency")
    (description
     "Monero is a secure, private, untraceable currency.  This package provides
the Monero command line client and daemon.")
    (license license:bsd-3)))

(define-public monero-gui
  (package
    (name "monero-gui")
    (version "0.17.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/monero-project/monero-gui")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete bundled monero sources, we already have them.
           ;; See the 'extract-monero-sources' phase.
           (delete-file-recursively "monero")
           #t))
       (sha256
        (base32 "0qb746z1sxqrja7q9lqhhbm64v83sn67az4k7gs5q90iaw584qfc"))))
    (build-system qt-build-system)
    (native-inputs
     `(,@(package-native-inputs monero)
       ("monero-source" ,(package-source monero))))
    (inputs
     `(,@(package-inputs monero)
       ("libgcrypt" ,libgcrypt)
       ("monero" ,monero)
       ("qtbase" ,qtbase-5)
       ("qtdeclarative" ,qtdeclarative)
       ("qtgraphicaleffects" ,qtgraphicaleffects)
       ("qtquickcontrols" ,qtquickcontrols)
       ("qtquickcontrols2",qtquickcontrols2)
       ("qtsvg" ,qtsvg)
       ("qtxmlpatterns" ,qtxmlpatterns)))
    (arguments
     `(#:tests? #f ; No tests
       #:configure-flags
       (list "-DARCH=default"
             "-DENABLE_PASS_STRENGTH_METER=ON"
             (string-append "-DReadline_ROOT_DIR="
                            (assoc-ref %build-inputs "readline")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'extract-monero-sources
           ;; Some of the monero package source code is required
           ;; to build the GUI.
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "monero")
             (invoke "tar" "-xv" "--strip-components=1"
                     "-C" "monero"
                     "-f" (assoc-ref inputs "monero-source"))))
         (add-after 'extract-monero-sources 'fix-build
           (lambda _
             (substitute* "src/version.js.in"
               (("@VERSION_TAG_GUI@")
                ,version))
             (substitute* "external/CMakeLists.txt"
               (("add_library\\(quirc" all)
                (string-append
                 "set(CMAKE_C_FLAGS \"${CMAKE_C_FLAGS} -fPIC\")\n"
                 all)))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (install-file "../build/bin/monero-wallet-gui" bin))))
         (add-after 'qt-wrap 'install-monerod-link
           ;; The monerod program must be available so that monero-wallet-gui
           ;; can start a Monero daemon if necessary.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (symlink (string-append (assoc-ref inputs "monero")
                                     "/bin/monerod")
                      (string-append (assoc-ref outputs "out")
                                     "/bin/monerod")))))))
    (home-page "https://web.getmonero.org/")
    (synopsis "Graphical user interface for the Monero currency")
    (description
     "Monero is a secure, private, untraceable currency.  This package provides
the Monero GUI client.")
    (license license:bsd-3)))

(define-public python-trezor-agent
  (package
    (name "python-trezor-agent")
    (version "0.13.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/romanz/trezor-agent")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q99vbfd3h85s8rnjipnmldixabqmmlk5w9karv6f0rhyi54f4zv"))))
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

(define-public python-mnemonic
  (package
    (name "python-mnemonic")
    (version "0.19")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "mnemonic" version))
        (sha256
          (base32 "0cd9prmdj8wzdmc7lxbf9lz0xrlkvak5ignag406mmfbn81fndsf"))))
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

(define-public python-btchip-python
  (package
    (name "python-btchip-python")
    (version "0.1.32")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "btchip-python" version))
        (sha256
          (base32
            "0mcg3gfd0qk8lhral3vy9cfd4pii9kzs42q71pf6b3y0c70y1x9l"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; those require PyQt4
    (propagated-inputs
      `(("python-ecdsa" ,python-ecdsa)
        ("python-hidapi" ,python-hidapi)))
    (home-page "https://github.com/LedgerHQ/btchip-python")
    (synopsis "Python library to communicate with Ledger Nano dongle")
    (description
      "This package provides a Python library to communicate with Ledger
Nano dongle.")
    (license license:asl2.0)))

(define-public python-trezor
  (package
    (name "python-trezor")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "trezor" version))
        (sha256
          (base32 "1w19m9lws55k9sjhras47hpfpqwq1jm5vy135nj65yhkblygqg19"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-click" ,python-click)
       ("python-construct" ,python-construct)
       ("python-ecdsa" ,python-ecdsa)
       ("python-libusb1" ,python-libusb1)
       ("python-mnemonic" ,python-mnemonic)
       ("python-requests" ,python-requests)
       ("python-typing-extensions" ,python-typing-extensions)))
    (native-inputs
     ;; For tests.
     `(("protobuf" ,protobuf)
       ("python-black" ,python-black)
       ("python-protobuf" ,python-protobuf)
       ("python-isort" ,python-isort)
       ("python-pyqt" ,python-pyqt)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/trezor/python-trezor")
    (synopsis "Python library for communicating with TREZOR Hardware Wallet")
    (description "@code{trezor} is a Python library for communicating with
TREZOR Hardware Wallet.")
    (license license:lgpl3)))

(define-public python-keepkey
  (package
    (name "python-keepkey")
    (version "6.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "keepkey" version))
        (sha256
          (base32
            "0z3d0m6364v9dv0njs4cd5m5ai6j6v35xaaxfxl90m9vmyxy81vd"))))
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
    (home-page "https://github.com/romanz/trezor-agent")
    (synopsis "Ledger as hardware SSH/GPG agent")
    (description "This package allows using Ledger as hardware SSH/GPG agent.")
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
    (home-page "https://github.com/romanz/trezor-agent")
    (synopsis "KeepKey as hardware SSH/GPG agent")
    (description "This package allows using KeepKey as a hardware SSH/GPG
agent.")
    (license license:lgpl3)))

(define-public python-stdnum
  (package
    (name "python-stdnum")
    (version "1.14")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-stdnum" version))
       (sha256
        (base32 "070vd9zgvq0jchs4xc4j1h4r1l1lg4wwb8xs3p25k8c2xjw94fpx"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "nosetests"))))))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "https://arthurdejong.org/python-stdnum/")
    (synopsis "Python module to handle standardized number and code formats")
    (description
     "This is a Python library that aims to provide functions to handle,
parse and validate standard numbers.
The module supports more than 100 different number formats
amongst which a great number of VAT and other tax numbers,
personal identity and company identification codes,
international standard numbers (ISBN, IBAN, EAN, etc.)
and various other formats.
The module also includes implementations of the Verhoeff,
Luhn and family of ISO/IEC 7064 check digit algorithms. ")
    (license license:lgpl2.1+)))

(define-public python2-stdnum
  (package-with-python2 python-stdnum))

(define-public python-duniterpy
  (package
    (name "python-duniterpy")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "duniterpy" version))
       (sha256
        (base32 "13kp2ph7fb1cdkx1y6j2h8q33fj2akc104l77ng52cy4v8jic9nz"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Tests fail with: "TypeError: block_uid() missing 1 required
     ;; positional argument: 'value'".
     `(#:tests? #f))
    (propagated-inputs
     `(("aiohttp" ,python-aiohttp)
       ("attrs" ,python-attrs)
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
    (version "0.7.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "silkaj" version))
       (sha256
        (base32 "0hrn0jwg415z7wjkp0myvw85wszlfi18f56j03075xxakr4dmi2j"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;no test
    (inputs
     `(("click" ,python-click)
       ("duniterpy" ,python-duniterpy)
       ("pynacl" ,python-pynacl)
       ("tabulate" ,python-tabulate)
       ("texttable" ,python-texttable)))
    (home-page "https://git.duniter.org/clients/python/silkaj")
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

(define-public grisbi
  (package
    (name "grisbi")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/grisbi/grisbi%20stable/"
             (version-major+minor version) ".x/" version
             "/grisbi-" version ".tar.bz2"))
       (sha256
        (base32
         "1piiyyxjsjbw9gcqydvknzxmmfgh8kdqal12ywrxyxih2afwnvbw"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags (list "--without-ofx")))
    (propagated-inputs
     `(("dconf" ,dconf)))
    (native-inputs
     `(("glib" ,glib "bin")             ; glib-compile-schemas
       ("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("gtk+" ,gtk+)
       ("libgsf" ,libgsf)))
    (synopsis "Personal accounting application")
    (description "Grisbi is a personal accounting application written by
French developers that is designed to follow French accounting rules.
Grisbi can manage multiple accounts, currencies and users.  It manages
third party, expenditure and receipt categories, budgetary lines,
financial years, budget estimates, bankcard management and other
information.")
    (home-page "https://grisbi.org")
    (license license:gpl2+)))

(define-public trezord-udev-rules
  (let ((commit "bff7fdfe436c727982cc553bdfb29a9021b423b0")
        (revision "0"))
      (package
        (name "trezord-udev-rules")
        (version (git-version "0.0.0" revision commit))
        (source
         (origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/trezor/trezor-common")
                 (commit commit)))
           (sha256
            (base32
             "14mrirrn68if7ja6qdk9qlxs1hv0f21vrxy5ncnms0gx9iwakp2l"))
           (file-name (git-file-name name version))))
        (build-system copy-build-system)
        (arguments
         '(#:install-plan
           '(("./udev/51-trezor.rules" "lib/udev/rules.d/"))))
        (home-page "https://github.com/trezor/trezor-common")
        (synopsis "Udev rules for trezord")
        (description
         "This contains the udev rules for trezord.  This will let a user run
trezord as a regular user instead of needing to it run as root.")
        (license license:lgpl3+))))

(define-public trezord
  (package
    (name "trezord")
    (version "2.0.30")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/trezor/trezord-go")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "1hzvk0wfgg7b4wpqjk3738yqxlv3pj5i7zxwm0jady2h97hmrqrr"))
       (file-name (git-file-name name version))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/trezor/trezord-go"))
    (native-inputs
     `(("github.com/gorilla-csrf" ,go-github-com-gorilla-csrf)
       ("github.com/gorilla/handlers" ,go-github-com-gorilla-handlers)
       ("github.com/gorilla/mux" ,go-github-com-gorilla-mux)
       ("gopkg.in/natefinch/lumberjack.v2" ,go-gopkg-in-natefinch-lumberjack.v2)))
    (home-page "https://trezor.io")
    (synopsis "Trezor Communication Daemon aka Trezor Bridge (written in Go)")
    (description "This allows a Trezor hardware wallet to communicate to the
Trezor wallet.")
    (license license:lgpl3+)))

(define-public bitcoin-abc
  (package
    (name "bitcoin-abc")
    (version "0.21.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.bitcoinabc.org/"
                                  version "/src/bitcoin-abc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1amzwy3gpl8ai90dsy7g0z51qq8vxfzbf642wn4bfynb8jmw3kx5"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python)               ; for the tests
       ("util-linux" ,util-linux)       ; provides the hexdump command for tests
       ("qttools" ,qttools)))
    (inputs
     `(("bdb" ,bdb-5.3)
       ("boost" ,boost)
       ("jemalloc" ,jemalloc)
       ("libevent" ,libevent)
       ("miniupnpc" ,miniupnpc)
       ("openssl" ,openssl)
       ("protobuf" ,protobuf)
       ("qrencode" ,qrencode)
       ("qtbase" ,qtbase-5)
       ("zeromq" ,zeromq)
       ("zlib" ,zlib)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'make-qt-deterministic
           (lambda _
             ;; Make Qt deterministic.
             (setenv "QT_RCC_SOURCE_DATE_OVERRIDE" "1")
             #t))
         (add-before 'check 'set-home
           (lambda _
             (setenv "HOME" (getenv "TMPDIR")) ; tests write to $HOME
             #t))
         (add-after 'check 'check-functional
           (lambda _
             (invoke
              "python3" "./test/functional/test_runner.py"
              (string-append "--jobs=" (number->string (parallel-job-count)))
              ;; TODO: find why the abc-miner-fund test fails.
              "--exclude=abc-miner-fund")
             #t)))))
    (home-page "https://www.bitcoinabc.org/")
    (synopsis "Bitcoin ABC peer-to-peer full node for the Bitcoin Cash protocol")
    (description
     "Bitcoin Cash brings sound money to the world, fulfilling the original
promise of Bitcoin as Peer-to-Peer Electronic Cash.  Merchants and users are
empowered with low fees and reliable confirmations is a digital currency that
enables instant payments to anyone anywhere in the world.  It uses
peer-to-peer technology to operate without central authority: managing
transactions and issuing money are carried out collectively by the network.
As a fork it implemented changes lowering the time between blocks and now
offers confimations after less than 5 seconds and have significantly lower
fees that BTC.  Bitcoin ABC is the reference implementation of the Bitcoin
Cash protocol.  This package provides the Bitcoin Cash command line client and
a client based on Qt.  This is a fork of Bitcoin Core.")
    (license license:expat)))

(define-public libofx
  (package
    (name "libofx")
    (version "0.9.15")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libofx/libofx")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jx56ma351p8af8dvavygjwf6ipa7qbgq7bpdsymwj27apdnixfy"))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-build? #f             ;fails with -j64
       #:configure-flags
       (list (string-append "--with-opensp-includes="
                            (assoc-ref %build-inputs "opensp")
                            "/include/OpenSP"))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gengetopt" ,gengetopt)
       ("help2man" ,help2man)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("curl" ,curl)
       ("libxml++-2" ,libxml++-2)
       ("opensp" ,opensp)))
    (home-page "http://libofx.sourceforge.net/")
    (synopsis "Library supporting the Open Financial Exchange format")
    (description
     "The LibOFX library is an API designed to allow applications to very easily
support OFX command responses, usually provided by financial institutions.  The
following three utilities are included with the library:
@enumerate
@item @code{ofxdump}
@item @code{ofx2qif}
@item @code{ofxconnect}
@end enumerate")
    (license license:gpl2+)))

(define-public bitcoin-unlimited
  (package
    (name "bitcoin-unlimited")
    (version "1.9.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/bitcoinunlimited/BCHUnlimited.git/")
             (commit (string-append "BCHunlimited" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cmrvh7azz0g89rsx6i8apd1li6r1lb3jrmbbf8fic1918lwv62m"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("python" ,python) ; for the tests
       ("util-linux" ,util-linux) ; provides the hexdump command for tests
       ("qttools" ,qttools)))
    (inputs
     `(("bdb" ,bdb-4.8)
       ("boost" ,boost)
       ("libevent" ,libevent)
       ("miniupnpc" ,miniupnpc)
       ("openssl" ,openssl)
       ("protobuf" ,protobuf)
       ("qrencode" ,qrencode)
       ("qtbase" ,qtbase-5)
       ("zeromq" ,zeromq)
       ("zlib" ,zlib)))
    (arguments
     `(#:configure-flags
       (list
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
                       "/bin/lupdate")
        "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             ;; Fix data specific test failure
             ;; https://reviews.bitcoinabc.org/rABC67bbd3d0aaee2952ff1cb5da51d1fd0b50c2b63a
             (substitute* "src/test/rpc_tests.cpp"
               (("1607731200") "9907731200"))

             ;; Disable utilprocess_tests because it never ends.
             ;; It looks like it tries to start /bin/sleep and waits until it
             ;; is in the list of running processes, but /bin/sleep doesn't
             ;; exist.
             (substitute* "src/Makefile.test.include"
               (("test/utilprocess_tests.cpp")
                ""))

             ;; Some transaction validation rules have changed (see upstream
             ;; commit f208400825d4641b9310a1fba023d56e0862e3b0), which makes
             ;; a test fail. Disable it for now.
             ;; TODO: Remove this when the next version is released.
             (substitute* "src/Makefile.test.include"
               (("test/txvalidationcache_tests.cpp")
                ""))))
         (add-before 'check 'set-home
           (lambda _
             (setenv "HOME" (getenv "TMPDIR")) ; tests write to $HOME
             #t)))))
    (home-page "https://www.bitcoinunlimited.info/")
    (synopsis "Client for the Bitcoin Cash protocol")
    (description
     "Bitcoin Unlimited is a client for the Bitcoin Cash peer-to-peer
electronic cash system.  This package provides a command line client and
a Qt GUI.")
    (license license:expat)))

(define-public fulcrum
  (package
    (name "fulcrum")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gitlab.com/FloweeTheHub/fulcrum/-/archive/v"
                           version "/fulcrum-v" version ".tar.gz"))
       (sha256
        (base32 "04w5gw02d39caa8a0l6wkn87kc43zzad2prqsyrcq97vlbkdx6x6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Call qmake instead of configure to create a Makefile.
         (replace 'configure
           (lambda _
             (invoke
              "qmake"
              (string-append "PREFIX=" %output)
              "features="))))))
    (native-inputs
     `(("qttools" ,qttools)))
    (inputs
     `(("python" ,python)
       ("qtbase" ,qtbase-5)
       ("rocksdb" ,rocksdb)
       ("zlib" ,zlib)))
    (home-page "https://gitlab.com/FloweeTheHub/fulcrum/")
    (synopsis "Fast and nimble SPV server for Bitcoin Cash")
    (description
     "Flowee Fulcrum is a server that is the back-end for @acronym{SPV,
Simplified Payment Verification} wallets, it provides the full API for those
walets in a fast and small server.  The full data is stored in a full node,
like Flowee the Hub, which Fulcrum connects to over RPC.")
    (license license:gpl3+)))

(define-public flowee
  (package
    (name "flowee")
    (version "2020.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gitlab.com/FloweeTheHub/thehub/-/archive/"
                            version "/thehub-" version ".tar.gz"))
       (sha256
         (base32 "1vwvaxm3b71pfx8l4rrv06wqks6xdf2333w856b36s1bzvj53rhc"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-Dbuild_tests=ON" "-Denable_gui=OFF")
       #:phases
        (modify-phases %standard-phases
          (add-before 'configure 'make-qt-deterministic
            (lambda _
              ;; Make Qt deterministic.
              (setenv "QT_RCC_SOURCE_DATE_OVERRIDE" "1")
             #t))
          (add-before 'configure 'disable-black-box
            ;; the black-box testing runs full hubs and lets them interact.
            ;; this is more fragile and a slow machine, or low memory machine, may
            ;; make the tests timeout and fail.  We just disable them here.
            (lambda _
              (substitute* "testing/CMakeLists.txt"
                (("test_api") ""))
              (substitute* "testing/CMakeLists.txt"
                (("add_subdirectory\\(api\\)") ""))
              #t))
          (add-after 'configure 'set-build-info
            ;; Their genbuild.sh to generate a build.h fails in guix (no .git dir) .
            ;; Its purpose is to write the tag name in the build.h file. We do that
            ;; here instead.
            (lambda _
              (with-output-to-file "include/build.h"
                (lambda _
                  (display
                    (string-append "#define BUILD_DESC " "\"", version "\""))))))
          (add-before 'check 'set-home
            (lambda _
              (setenv "HOME" (getenv "TMPDIR")) ; tests write to $HOME
              #t))
          (replace 'check
            (lambda _
              (invoke "make" "check" "-C" "testing"))))))
    (inputs
     `(("boost" ,boost)
       ("gmp" ,gmp)
       ("libevent" ,libevent)
       ("miniupnpc" ,miniupnpc)
       ("openssl" ,openssl)
       ("qtbase" ,qtbase-5)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)
       ("util-linux" ,util-linux)))       ; provides the hexdump command for tests
    (home-page "https://flowee.org")
    (synopsis "Flowee infrastructure tools and services")
    (description
     "Flowee packages all tier-1 applications and services from the Flowee group.
This includes components like The Hub and Indexer which and various others
that allows you to run services and through them access the Bitcoin Cash networks.")
    (license license:gpl3+)))


(define-public beancount
  (package
    (name "beancount")
    (version "2.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "beancount" version))
       (sha256
        (base32
         "1h465zc7gb0bc5pagm9fsp083sqxrn2mjfbk9l7h162xm7k8rw1b"))
       (patches (search-patches "beancount-disable-googleapis-fonts.patch"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f  ; Says test is missing, not sure why
       #:phases
       (modify-phases %standard-phases
         ;; Not importing the googleapis package for now
         (add-after 'unpack 'ignore-googleapis
           (lambda _
             (substitute* "setup.py"
               (("'google-api-python-client',") ""))
             #t)))))
    (inputs
     `(("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-bottle" ,python-bottle)
       ("python-chardet" ,python-chardet)
       ("python-dateutil" ,python-dateutil)
       ("python-lxml" ,python-lxml)
       ("python-magic" ,python-magic)
       ("python-ply" ,python-ply)
       ("python-requests" ,python-requests)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "http://furius.ca/beancount")
    (synopsis "Command-line double-entry accounting tool")
    (description
     "Beancount is a double-entry bookkeeping computer language that lets you
define financial transaction records in a text file, read them in memory,
generate a variety of reports from them, and provides a web interface.")
    (license license:gpl2)))

;; The beancount source ships with elisp in a subdirectory
(define-public emacs-beancount
  (package
    (inherit beancount)
    (name "emacs-beancount")
    (build-system emacs-build-system)
    (arguments
     `(#:tests? #f ;no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'chdir-emacs
           (lambda _
             (chdir "editors/emacs")
             #t)))))
    (inputs '())
    (native-inputs '())
    (synopsis "Emacs mode for beancount")
    (description
      "Emacs-beancount is an Emacs mode for the Beancount accounting tool.")))

(define-public hledger-web
  (package
    (name "hledger-web")
    (version "1.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "hledger-web/hledger-web-" version ".tar.gz"))
       (sha256
        (base32
         "0w59nr7mj0nx8z44cvhy1rhlj5rmx0wq4p5nfl4dycfmp7jwvsm1"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-decimal" ,ghc-decimal)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-blaze-markup" ,ghc-blaze-markup)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-clientsession" ,ghc-clientsession)
       ("ghc-cmdargs" ,ghc-cmdargs)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-hjsmin" ,ghc-hjsmin)
       ("hledger" ,hledger)
       ("ghc-hledger-lib" ,ghc-hledger-lib)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-http-conduit" ,ghc-http-conduit)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-json" ,ghc-json)
       ("ghc-megaparsec" ,ghc-megaparsec)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-shakespeare" ,ghc-shakespeare)
       ("ghc-wai" ,ghc-wai)
       ("ghc-wai-extra" ,ghc-wai-extra)
       ("ghc-wai-handler-launch" ,ghc-wai-handler-launch)
       ("ghc-warp" ,ghc-warp)
       ("ghc-yaml" ,ghc-yaml)
       ("ghc-yesod" ,ghc-yesod)
       ("ghc-yesod-core" ,ghc-yesod-core)
       ("ghc-yesod-form" ,ghc-yesod-form)
       ("ghc-yesod-static" ,ghc-yesod-static)))
    (home-page "https://hledger.org")
    (synopsis "Web-based user interface for the hledger accounting system")
    (description "This package provides a simple Web-based User
Interface (UI) for the hledger accounting system.  It can be used as a
local, single-user UI, or as a multi-user UI for viewing, adding, and
editing on the Web.")
    (license license:gpl3)))
